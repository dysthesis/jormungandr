{
  pkgs,
  lib,
  emacs-unstable,
  emacsWithPackagesFromUsePackage,
  runCommand,
  size ? 10,
}: let
  inherit (lib.babel.pkgs) mkWrapper;
  inherit (lib.babel.emacs) orgTangle;
  orgFile = ../../README.org;
  initFile = orgTangle pkgs emacs-unstable orgFile "init.org";

  cfg =
    runCommand "init.el" {
      buildInputs = [emacs-unstable];
      src = "${initFile}/init.el";
    } ''
      mkdir -p $out
      mkdir -p $out/eln-cache

      cat $src >> $out/init.el

      emacs --batch \
        --eval "(setq native-comp-eln-load-path (list \"$out/eln-cache\"))" \
        --eval "(native-compile \"$out/init.el\")"
    '';

  package = emacs-unstable.overrideAttrs (old: {
    postInstall =
      /*
      bash
      */
      ''
        ${old.postInstall or ""}
        shopt -s globstar
        ln -sf $out/share/emacs/native-lisp/*/* $out/lib/emacs/*/native-lisp/*/
        ln -sf ${cfg}/eln-cache/* $out/lib/emacs/*/native-lisp/*/
        shopt -u globstar
      '';
  });

  config = "${cfg}/init.el";
in
  builtins.trace "init.el file is at ${config}"
  emacsWithPackagesFromUsePackage {
    inherit package config;
    alwaysEnsure = true;
    defaultInitFile = true;
    extraEmacsPackages = epkgs:
      with epkgs; [
        use-package
        treesit-grammars.with-all-grammars
        pkgs.unzip
        pkgs.texliveFull
        pkgs.ripgrep
        pkgs.fd
      ];
  }
