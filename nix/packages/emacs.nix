{
  emacs-unstable,
  emacsWithPackagesFromUsePackage,
  runCommand,
}: let
  emacs = emacs-unstable;
  config =
    runCommand "init.el" {
      buildInputs = [emacs];
      src = ../../README.org;
      resultIsFile = true;
    } ''
         # Tangle the Org file using Emacs in batch mode.
      cp $src init.org
      emacs --batch \
       -l org init.org \
       -f org-babel-tangle


      echo "(add-to-list 'custom-theme-load-path "${../../themes}")" > $out
      cat init.el >> $out
    '';
  # config =
  #   writeText "init.el"
  #   /*
  #   emacs-lisp
  #   */
  #   ''
  #     (add-to-list 'custom-theme-load-path "${../../themes}")
  #     (org-babel-load-file "${../../README.org}")
  #   '';
in
  emacsWithPackagesFromUsePackage {
    inherit config;
    package = emacs;
    alwaysEnsure = true;
  }
