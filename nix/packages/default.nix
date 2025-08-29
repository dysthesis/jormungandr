{
  pkgs,
  lib,
  ...
}: let
  inherit (pkgs) callPackage;
in rec {
  jormungandr = callPackage ./emacs.nix {inherit lib pkgs oxocarbon-theme;};
  oxocarbon-theme = callPackage ./oxocarbon-theme.nix {};
  default = jormungandr;
}
