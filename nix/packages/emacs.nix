{
  pkgs,
  lib,
  emacs-unstable,
  emacsWithPackagesFromUsePackage,
}: let
  inherit (lib.babel.pkgs) mkWrapper;
  package = emacs-unstable.overrideAttrs (old: {
    postInstall =
      /*
      bash
      */
      ''
        ${old.postInstall or ""}
        shopt -s globstar
        ln -sf $out/share/emacs/native-lisp/*/* $out/lib/emacs/*/native-lisp/*/
        shopt -u globstar
      '';
  });

  config = ../../README.org;

  wrapped-emacs = emacsWithPackagesFromUsePackage {
    inherit package config;
    alwaysEnsure = true;
    alwaysTangle = true;
    defaultInitFile = true;
  };
in
  wrapped-emacs
# mkWrapper pkgs wrapped-emacs
# /*
# bash
# */
# ''
#   wrapProgram $out/bin/emacs \
#    --prefix "EMACSNATIVELOADPATH" ":" "${package}/share/emacs/native-lisp/"
# ''

