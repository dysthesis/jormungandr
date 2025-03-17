{
  self,
  pkgs,
  lib,
  ...
}: let
  inherit (pkgs) callPackage;
  inherit (self.packages.${pkgs.system}) no-clown-fiesta;
in rec {
  jormungandr = callPackage ./emacs.nix {inherit lib pkgs no-clown-fiesta;};
  no-clown-fiesta = callPackage ./noClownFiesta.nix {};
  default = jormungandr;
}
