{
  pkgs,
  emacs-unstable,
  ...
}: let
in
  pkgs.runCommand "no-clown-fiesta" {
    nativeBuildInputs = [emacs-unstable];
    src = ../../themes/no-clown-fiesta.el;
  } ''
    mkdir -p $out/share/emacs/site-lisp
    cp $src $out/share/emacs/site-lisp/no-clown-fiesta.el
  ''
