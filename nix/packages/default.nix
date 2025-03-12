{
  pkgs,
  lib,
  ...
}: rec {
  emacs = pkgs.callPackage ./emacs.nix {inherit lib;};
  default = emacs;
}
