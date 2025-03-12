{
  pkgs,
  lib,
  emacs-unstable,
  emacsWithPackagesFromUsePackage,
  runCommand,
}: let
  inherit (lib.babel.pkgs) mkWrapper;
  inherit (lib.babel.emacs) orgTangle;
  orgFile = ../../README.org;
  initFile = orgTangle pkgs emacs-unstable orgFile "init.org";

  cfg =
    runCommand "init.el" {
      buildInputs = [emacs-unstable];
      src = "${initFile}/output.el";
    } ''


      mkdir -p $out
      mkdir -p $out/eln-cache
      # echo "(setq native-comp-eln-load-path (list \"$out/eln-cache\"))" >> $out/init.el
      echo "(add-to-list 'native-comp-eln-load-path \"$out/eln-cache\")" >> $out/init.el
      echo "(add-to-list 'native-comp-eln-load-path \"$emacs/share/emacs/native-lisp\")" >> $out/init.el
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
  emacsWithPackagesFromUsePackage {
    inherit package config;
    alwaysEnsure = true;
    alwaysTangle = true;
    defaultInitFile = true;
  }
