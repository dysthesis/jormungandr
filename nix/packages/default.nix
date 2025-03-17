{
  self,
  pkgs,
  lib,
  ...
}:
let
  inherit (pkgs) callPackage;
in
rec {
  jormungandr = callPackage ./emacs.nix { inherit lib pkgs no-clown-fiesta-theme; };
  no-clown-fiesta-theme = callPackage ./noClownFiesta.nix { };
  default = jormungandr;
}
