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

      emacsBase =
        if (!pkgs.stdenv.isDarwin) && (builtins.hasAttr "emacs-gtk" pkgs)
        then pkgs."emacs-gtk"
        else pkgs.emacs;

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
        in [
          epkgs.use-package
          orgModernIndent
        ];
      };

      configDir = pkgs.linkFarm "emacs-config" [
        {
          name = "README.org";
          path = ./README.org;
        }
        {
          name = "early-init.el";
          path = ./early-init.el;
        }
        {
          name = "init.el";
          path = ./init.el;
        }
      ];

      packaged = pkgs.symlinkJoin {
        name = "emacs";
        paths = [emacsWithDeps];
        postBuild = ''
          rm -f "$out/bin/emacs"
          cat >"$out/bin/emacs" <<'EOF'
            #!${pkgs.bash}/bin/bash
            set -euo pipefail

            config_home="''${XDG_CONFIG_HOME:-''${HOME}/.config}"
            init_dir="''${EMACS_INIT_DIR:-''${config_home}/emacs}"

            ${pkgs.coreutils}/bin/mkdir -p "''${init_dir}"

            for file in init.el early-init.el README.org; do
              target="''${init_dir}/''${file}"
              if [ -e "''${target}" ] && [ ! -L "''${target}" ]; then
                echo "Refusing to overwrite ''${target} because it is not a symlink. Set EMACS_INIT_DIR to a dedicated directory." >&2
                exit 1
              fi
            done

            ${pkgs.coreutils}/bin/ln -sf "${configDir}/init.el" "''${init_dir}/init.el"
            ${pkgs.coreutils}/bin/ln -sf "${configDir}/early-init.el" "''${init_dir}/early-init.el"
            ${pkgs.coreutils}/bin/ln -sf "${configDir}/README.org" "''${init_dir}/README.org"

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
