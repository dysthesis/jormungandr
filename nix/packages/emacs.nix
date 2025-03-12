{
  pkgs,
  lib,
  emacs-unstable,
  emacsWithPackagesFromUsePackage,
  runCommand,
}: let
  inherit (lib.babel.emacs) orgTangle;
  package = emacs-unstable;
  orgFile = ../../README.org;
  initFile = orgTangle pkgs package orgFile "init.org";

  cfg =
    runCommand "init.el" {
      buildInputs = [package];
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
  emacs = emacsWithPackagesFromUsePackage {
    config = "${cfg}/init.el";
    inherit package;
    alwaysEnsure = true;
    alwaysTangle = true;
    defaultInitFile = true;
  };
in
  builtins.trace "${cfg}"
  emacs
