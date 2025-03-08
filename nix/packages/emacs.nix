{
  emacs-unstable,
  writeText,
  emacsWithPackagesFromUsePackage,
}: let
  config =
    writeText "init.el"
    /*
    emacs-lisp
    */
    ''
      (org-babel-load-file "${../../README.org}")
      (add-to-list 'custom-theme-load-path "${../../themes}")
    '';
in
  emacsWithPackagesFromUsePackage {
    inherit config;
    alwaysEnsure = true;
  }
