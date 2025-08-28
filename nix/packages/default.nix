{
  pkgs,
  lib,
  ...
}: let
  inherit (pkgs) callPackage;
in rec {
  jormungandr = callPackage ./emacs.nix {inherit lib pkgs gruber-darkest-theme;};
  gruber-darkest-theme = callPackage ./gruber-darkest-theme.nix {};
  default = jormungandr;
}
