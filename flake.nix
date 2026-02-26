{
  description = "Jormungandr Emacs configuration (flake-parts)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # Optional: use npins to pin non-flake VCS deps in the future.
    # npins.url = "github:andir/npins";
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
        epkgs = pkgs.emacsPackagesFor pkgs.emacs-unstable-pgtk;
        vcPkgs = import ./nix/vc-packages.nix {
          inherit pkgs;
          p = epkgs;
        };
        tangle = pkgs.callPackage ./nix/tangle.nix {};
        jormungandrPackages = with epkgs;
          [
            use-package
            auto-compile
            gcmh
            vertico
            orderless
            marginalia
            nerd-icons
            nerd-icons-completion
            which-key
            general
            embark
            embark-consult
            consult
            evil
            evil-collection
            corfu
            nerd-icons-corfu
            cape
            tempel
            tempel-collection
            smartparens
            treesit-auto
            evil-textobj-tree-sitter
            eglot
            consult-eglot
            direnv
            dape
            transient
            magit
            doom-themes
            solaire-mode
            doom-modeline
            olivetti
            mixed-pitch
            rainbow-mode
            ligature
            org-modern
            nix-mode
            rustic
            autothemer
          ]
          ++ vcPkgs;

        emacs-unwrapped = epkgs.emacsWithPackages (_p: jormungandrPackages);
        bootScript = pkgs.writeShellScript "jormungandr-boot" ''
          set -e
          cfgroot=''${XDG_CONFIG_HOME:-$HOME/.config}/jormungandr
          export XDG_CONFIG_HOME="$cfgroot"
          cfg="$cfgroot/emacs"
          if [ ! -e "$cfg/init.el" ]; then
            mkdir -p "$cfg"
            cp -r ${tangle}/share/jormungandr/* "$cfg"/
          fi
          set -- "--init-directory" "$cfg" \
                 "--load" "$cfg/early-init.el" \
                 "--load" "$cfg/init.el" \
                 "$@"
        '';

        jormungandr = pkgs.symlinkJoin {
          name = "jormungandr";
          paths = [emacs-unwrapped];
          buildInputs = [pkgs.makeWrapper];
          postBuild = ''
            rm $out/bin/emacs
            makeWrapper ${emacs-unwrapped}/bin/emacs $out/bin/emacs \
              --set DYSTHESIS_DISABLE_PACKAGE_EL 1 \
              --run ". ${bootScript}"
          '';
        };
      in {
        _module.args.pkgs = pkgs;

        packages = {
          inherit tangle jormungandr;
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
          tangle = tangle;
          emacs-smoke =
            pkgs.runCommand "emacs-smoke" {
              buildInputs = [self.packages.${system}.jormungandr];
            } ''
              ${self.packages.${system}.jormungandr}/bin/emacs --batch --eval "(message \"ok\")"
              touch $out
            '';
        };
      };
    };
}
