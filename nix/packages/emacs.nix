{
  emacs-unstable,
  emacsWithPackagesFromUsePackage,
  runCommand,
}: let
  package = emacs-unstable;
  cfg =
    runCommand "init.el" {
      buildInputs = [package];
      src = ../../README.org;
    } ''
      # Tangle the Org file using Emacs in batch mode.
      cp $src init.org
      emacs --batch \
       -l org init.org \
       -f org-babel-tangle


      mkdir -p $out
      mkdir -p $out/eln-cache
      echo "(setq native-comp-eln-load-path (list \"$out/eln-cache\"))" >> $out/init.el
      echo "(add-to-list 'custom-theme-load-path \"${../../themes}\")" >> $out/init.el
      cat init.el >> $out/init.el


      emacs --batch \
        --eval "(setq native-comp-eln-load-path (list \"$out/eln-cache\"))" \
        --eval "(native-compile \"$out/init.el\")"
    '';
in
  builtins.trace "${cfg}"
  emacsWithPackagesFromUsePackage {
    config = "${cfg}/init.el";
    inherit package;
    alwaysEnsure = true;
    alwaysTangle = true;
    defaultInitFile = true;
  }
