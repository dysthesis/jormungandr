{
  description = "Personal Emacs configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    emacs-overlay,
  }: let
    systems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];

    forAllSystems = nixpkgs.lib.genAttrs systems;

    mkPkgs = system:
      import nixpkgs {
        inherit system;
        overlays = [emacs-overlay.overlay];
      };
  in {
    packages = forAllSystems (system: let
      pkgs = mkPkgs system;

      # emacs-overlay parses the config in Nix, which is brittle with
      # non-ASCII characters and some Emacs reader constructs. Keep the
      # runtime config untouched, but sanitise the text used for
      # dependency inference.
      configForUsePackage = let
        raw = builtins.readFile ./init.el;
        preprocessed = builtins.replaceStrings ["#(" "?─"] ["(" "?-"] raw;
        len = builtins.stringLength preprocessed;
        bytes = builtins.genList (i: builtins.substring i 1 preprocessed) len;
        asciiBytes = builtins.map (b:
          if (builtins.match "[ -~]" b != null) || (b == "\n") || (b == "\t") || (b == "\r")
          then b
          else " ")
        bytes;
      in
        builtins.concatStringsSep "" asciiBytes;

      emacsBase = pkgs.emacs-unstable-pgtk
        or pkgs.emacs-git.pgtk
        or pkgs.emacs;

      emacsWithDeps = pkgs.emacsWithPackagesFromUsePackage {
        config = configForUsePackage;
        package = emacsBase;
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: let
          orgModernIndent = epkgs.trivialBuild {
              pname = "org-modern-indent";
              version = "0.5.1";
              src = pkgs.fetchFromGitHub {
                owner = "jdtsmith";
                repo = "org-modern-indent";
                rev = "v0.5.1";
                hash = "sha256-st3338Jk9kZ5BLEPRJZhjqdncMpLoWNwp60ZwKEObyU=";
              };
              packageRequires = [
                epkgs.compat
                epkgs.org
              ];
              nativeBuildInputs = [
                epkgs.compat
                epkgs.org
              ];
            };

            lean4Mode = epkgs.trivialBuild {
              pname = "lean4-mode";
              version = "1.1.2";
              src = pkgs.fetchFromGitHub {
                owner = "leanprover-community";
                repo = "lean4-mode";
                rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
                hash = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
              };
              packageRequires = [
                epkgs.compat
                epkgs.dash
                epkgs.magit-section
                epkgs.lsp-mode
              ];
              nativeBuildInputs = [
                epkgs.compat
                epkgs.dash
                epkgs.magit-section
                epkgs.lsp-mode
              ];
              installPhase = ''
                runHook preInstall

                LISPDIR=$out/share/emacs/site-lisp
                install -d "$LISPDIR"
                install *.el *.elc "$LISPDIR"
                if [ -d data ]; then
                  cp -r data "$LISPDIR/"
                fi

                runHook postInstall
              '';
            };

            localThemes = pkgs.runCommand "emacs-jormungandr-themes-0.0.0" {} ''
            pkgdir="$out/share/emacs/site-lisp/elpa/jormungandr-themes-0.0.0"
            mkdir -p "$pkgdir"

            cp -v ${pkgs.lib.cleanSource ./themes}/*.el "$pkgdir/"

            cat >"$pkgdir/jormungandr-themes-pkg.el" <<'EOF'
            ;; -*- no-byte-compile: t; lexical-binding: nil -*-
            (define-package "jormungandr-themes" "0.0.0"
              "Local Emacs themes packaged with this flake."
              nil
              :keywords '("themes"))
            EOF

            cat >"$pkgdir/jormungandr-themes-autoloads.el" <<'EOF'
            ;;; jormungandr-themes-autoloads.el --- autoloads  -*- lexical-binding: t; -*-
            ;;; Code:

            (add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))

            (when load-file-name
              (add-to-list 'custom-theme-load-path
                           (file-name-as-directory (file-name-directory load-file-name))))

            (provide 'jormungandr-themes-autoloads)
            ;;; jormungandr-themes-autoloads.el ends here
            EOF
          '';
          in [
            epkgs.use-package
            (epkgs.treesit-grammars.with-grammars (p: with p; [
              tree-sitter-nix
              tree-sitter-rust
              tree-sitter-zig
              tree-sitter-json
              tree-sitter-yaml
              tree-sitter-python
              tree-sitter-markdown
            ]))
            orgModernIndent
            lean4Mode
            localThemes
          ];
        };

      configDir = pkgs.linkFarm "emacs-config" [
        {
          name = "init.el";
          path = ./init.el;
        }
      ];

      extraRuntimePackages = builtins.filter (pkg: pkg != null) [
        (pkgs.emacs-lsp-booster or null)
      ];

      packaged = pkgs.symlinkJoin {
        name = "emacs";
        paths = [emacsWithDeps] ++ extraRuntimePackages;
        postBuild = ''
          rm -f "$out/bin/emacs"
          cat >"$out/bin/emacs" <<-'EOF'
		#!${pkgs.bash}/bin/bash
		set -euo pipefail

		config_home="''${XDG_CONFIG_HOME:-''${HOME}/.config}"
		init_dir="''${EMACS_INIT_DIR:-''${config_home}/emacs-jormungandr}"

		${pkgs.coreutils}/bin/mkdir -p "''${init_dir}"

		for file in init.el early-init.el README.org; do
		  target="''${init_dir}/''${file}"
		  if [ -e "''${target}" ] && [ ! -L "''${target}" ]; then
		    echo "Refusing to overwrite ''${target} because it is not a symlink. Set EMACS_INIT_DIR to a dedicated directory (default: ''${config_home}/emacs-jormungandr)." >&2
		    exit 1
		  fi
		done

		${pkgs.coreutils}/bin/ln -sf "${configDir}/init.el" "''${init_dir}/init.el"
		${pkgs.coreutils}/bin/ln -sf "${configDir}/early-init.el" "''${init_dir}/early-init.el"
		${pkgs.coreutils}/bin/ln -sf "${configDir}/README.org" "''${init_dir}/README.org"

		${pkgs.lib.optionalString (extraRuntimePackages != []) ''
		export PATH="${pkgs.lib.makeBinPath extraRuntimePackages}:''${PATH}"
		''}

		exec "${emacsWithDeps}/bin/emacs" --init-directory "''${init_dir}" "''${@}"
EOF
          chmod +x "$out/bin/emacs"
        '';
      };
    in {
      default = packaged;
      emacs = emacsWithDeps;
      config = configDir;
    });

    apps = forAllSystems (system: let
      pkg = self.packages.${system}.default;
    in {
      default = {
        type = "app";
        program = "${pkg}/bin/emacs";
      };
    });

    devShells = forAllSystems (system: let
      pkgs = mkPkgs system;
    in {
      default = pkgs.mkShell {
        packages = with pkgs; [
          statix
          deadnix
          alejandra
          nixd
        ];
      };
    });
  };
}
