{
  pkgs,
  emacs-unstable,
  ...
}:
pkgs.runCommand "oxocarbon"
{
  nativeBuildInputs = [emacs-unstable];
  src = ../../themes/oxocarbon-theme.el;
}
''
  mkdir -p $out/share/emacs/site-lisp
  cp $src $out/share/emacs/site-lisp/oxocarbon-theme.el
''
