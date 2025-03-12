{
  pkgs,
  lib,
  ...
}: rec {
  emacs = pkgs.callPackage ./emacs.nix {inherit lib pkgs;};
  default = emacs;
}
