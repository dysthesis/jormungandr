{
  description = "Emacs flake";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem = {
        pkgs,
        system,
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.emacs-overlay.overlays.default
          ];
          config = {};
        };
        devShells.default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            nixd
            nixfmt
            alejandra
            statix
            deadnix
          ];
        };

        packages = rec {
          jormungandr = let
            readmeForUsePackage =
              pkgs.runCommand "README-usepackage.org" {
                nativeBuildInputs = [pkgs.coreutils pkgs.gnused];
              } ''
                sed -e 's/:tangle early-init.el/:tangle yes/g' \
                    -e 's/:tangle init.el/:tangle yes/g' \
                    ${./README.org} \
                  | LC_ALL=C tr -cd '\11\12\15\40-\176' > $out
              '';
            baseEmacs = pkgs.emacsWithPackagesFromUsePackage {
              package = pkgs.emacs-unstable-pgtk or pkgs.emacs-git.pgtk or pkgs.emacs;
              config = readmeForUsePackage;
              defaultInitFile = pkgs.writeText "default.el" ''
                (load "${./early-init.el}")
                (load "${./init.el}")
                ;; Nix-managed packages, disable package.el
                (setq package-enable-at-startup nil
                      package-quickstart nil
                      package-archives nil)
                (with-eval-after-load 'package
                  (advice-add 'package-initialize :override #'ignore)
                  (advice-add 'package--ensure-init-file :override #'ignore))
                (setq use-package-always-ensure nil
                      use-package-ensure-function #'ignore)

                ;; Nix-provided codelldb adapter path for dape
                (defconst dysthesis/dape-codelldb-dir-nix
                  "${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/adapter/"
                  "Nix-provided codelldb adapter directory.")
                (setq dysthesis/dape-codelldb-dir dysthesis/dape-codelldb-dir-nix)
                (setenv "CODELLDB_DIR" dysthesis/dape-codelldb-dir-nix)

                (defun dysthesis/dape--force-codelldb-dir (&rest _)
                  "Keep codelldb dir pinned to the Nix path."
                  (setq dysthesis/dape-codelldb-dir dysthesis/dape-codelldb-dir-nix)
                  (when (fboundp 'dysthesis/dape--refresh-codelldb-configs)
                    (dysthesis/dape--refresh-codelldb-configs)))
                (add-hook 'after-init-hook #'dysthesis/dape--force-codelldb-dir)
                (with-eval-after-load 'dape
                  (advice-add 'dysthesis/dape-refresh-adapter-dir
                              :override
                              #'dysthesis/dape--force-codelldb-dir)
                  (dysthesis/dape--force-codelldb-dir))
                ;; Avoid direnv reloading overriding the Nix codelldb path
                (with-eval-after-load 'direnv
                  (advice-remove 'direnv-update-directory-environment
                                 #'dysthesis/dape-refresh-adapter-dir))
              '';
              extraEmacsPackages = _epkgs:
                with pkgs; [
                  emacs-lsp-booster
                  vscode-extensions.vadimcn.vscode-lldb
                ];
            };
          in
            pkgs.symlinkJoin {
              name = "jormungandr";
              paths = [baseEmacs];
              meta = {
                mainProgram = "emacs";
              };
              postBuild = ''
                rm -f $out/bin/emacs
                cat > $out/bin/emacs <<'EOF'
                #!@RUNTIME_SHELL@
                set -euo pipefail
                debug_run=false
                args=()
                for arg in "$@"; do
                  if [ "$arg" = "--debug-run" ]; then
                    debug_run=true
                  else
                    args+=("$arg")
                  fi
                done
                if $debug_run; then
                  exec @EMACS_BIN@ --debug-init "''${args[@]}"
                else
                  exec @EMACS_BIN@ "''${args[@]}"
                fi
                EOF
                sed -i "s|@RUNTIME_SHELL@|${pkgs.runtimeShell}|g" $out/bin/emacs
                sed -i "s|@EMACS_BIN@|${baseEmacs}/bin/emacs|g" $out/bin/emacs
                chmod +x $out/bin/emacs
              '';
            };
          default = jormungandr;
        };
      };
    };
}
