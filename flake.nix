{
  description = "Personal Emacs configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    emacs-overlay,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = {system, ...}: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [emacs-overlay.overlay];
          config.allowUnfree = true;
        };
        vcPackagesFor = epkgs:
          import ./nix/vc-packages.nix {
            inherit pkgs;
            p = epkgs;
          };

        tangle = pkgs.callPackage ./nix/tangle.nix {};
        jormungandr-unwrapped = pkgs.emacsWithPackagesFromUsePackage {
          package = pkgs.emacs-unstable-pgtk;
          config = "${tangle}/init.el";
          defaultInitFile = true;
          # Extra packages that aren't discoverable via :ensure or need pinning.
          extraEmacsPackages = epkgs:
            (vcPackagesFor epkgs)
            ++ (with pkgs; [
              epkgs.nerd-icons
            ]);
        };

        cfgWrapper = ''
          export JORMUNGANDR_IMMUTABLE=1
          export JORMUNGANDR_STATE_DIR=${"\${XDG_STATE_HOME:-$HOME/.local/state}"}/jormungandr
        '';

        jormungandr-eln-cache = pkgs.stdenv.mkDerivation {
          pname = "jormungandr-eln-cache";
          version = "0.1";
          dontUnpack = true;
          nativeBuildInputs = [
            jormungandr-unwrapped
            pkgs.coreutils
            pkgs.findutils
          ];
          buildPhase = ''
            runHook preBuild
            export HOME=$TMPDIR
            export XDG_CONFIG_HOME=$TMPDIR/.config
            export XDG_STATE_HOME=$TMPDIR/.local/state
            export JORMUNGANDR_IMMUTABLE=1
            export JORMUNGANDR_STATE_DIR=$XDG_STATE_HOME/jormungandr
            theme_dir="$out/share/emacs/site-lisp/themes"
            native_dir="$out/share/emacs/native-lisp"

            mkdir -p "$theme_dir" "$native_dir"
            cp -r ${tangle}/themes/. "$theme_dir/"

            THEME_DIR="$theme_dir" NATIVE_DIR="$native_dir" ${jormungandr-unwrapped}/bin/emacs --batch <<'EOF'
(require 'comp)
(require 'use-package)
(setq use-package-always-ensure nil)
(let* ((native-compile-target-directory (getenv "NATIVE_DIR"))
       (theme-dir (getenv "THEME_DIR"))
       (files (list "${tangle}/early-init.el"
                    "${tangle}/init.el"
                    "${pkgs.emacs-unstable-pgtk}/share/emacs/site-lisp/site-start.el")))
  (when (and theme-dir (file-directory-p theme-dir))
    (setq files (nconc files (directory-files-recursively theme-dir "\\.el\\'"))))
  (mapc #'native-compile files))
EOF
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            # native-compile wrote directly into $out
            runHook postInstall
          '';
        };

        jormungandr = let
          deps = with pkgs; [emacs-lsp-booster];
        in
          pkgs.symlinkJoin {
            name = "jormungandr";
            paths = [jormungandr-unwrapped jormungandr-eln-cache];
            nativeBuildInputs = [pkgs.makeWrapper];
            postBuild = ''
              wrapProgram $out/bin/emacs \
                --set JORMUNGANDR_DISABLE_PACKAGE_EL 1 \
                --set JORMUNGANDR_ELN_DIR "$out/share/emacs/native-lisp" \
                --set JORMUNGANDR_THEME_DIR "$out/share/emacs/site-lisp/themes" \
                --run ${pkgs.lib.escapeShellArg cfgWrapper} \
                --add-flags "--init-directory ${tangle}" \
                --prefix PATH ":" "${pkgs.lib.makeBinPath deps}"
            '';
          };
      in {
        _module.args.pkgs = pkgs;

        packages = {
          inherit
            tangle
            jormungandr
            jormungandr-unwrapped
            jormungandr-eln-cache
            ;
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.jormungandr}/bin/emacs";
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            self.packages.${system}.jormungandr

            nixd
            nil
            rust-analyzer
            direnv
            just
            statix
            deadnix
            alejandra
          ];
        };

        checks = {
          inherit tangle;
          emacs-smoke =
            pkgs.runCommand "emacs-smoke" {
              buildInputs = [self.packages.${system}.jormungandr];
            } ''
              export HOME=$TMPDIR
              export XDG_CONFIG_HOME=$HOME/.config
              ${self.packages.${system}.jormungandr}/bin/emacs --batch --eval "(message \"ok\")"
              touch $out
            '';
        };
      };
    };
}
