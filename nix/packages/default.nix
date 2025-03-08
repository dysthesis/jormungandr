{pkgs, ...}: rec {
  emacs = pkgs.callPackage ./emacs.nix {};
  default = emacs;
}
