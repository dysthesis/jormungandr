{
  pkgs,
  emacs-unstable,
  ...
}:
pkgs.runCommand "gruber-darkest"
  {
    nativeBuildInputs = [ emacs-unstable ];
    src = ../../themes/gruber-darkest-theme.el;
  }
  ''
    mkdir -p $out/share/emacs/site-lisp
    cp $src $out/share/emacs/site-lisp/gruber-darkest-theme.el
  ''
