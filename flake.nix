{
  description = "Emacs flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Personal library
    babel = {
      url = "github:dysthesis/babel";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      babel,
      nixpkgs,
      treefmt-nix,
      emacs-overlay,
      ...
    }:
    let
      inherit (builtins) mapAttrs;
      inherit (babel) mkLib;
      lib = mkLib nixpkgs;

      # Systems to support
      systems = [
        "aarch64-linux"
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      overlays = [
        emacs-overlay.overlays.default
      ];

      forAllSystems = lib.babel.forAllSystems { inherit systems overlays; };

      treefmt = forAllSystems (pkgs: treefmt-nix.lib.evalModule pkgs ./nix/formatters);
    in
    # Budget flake-parts
    mapAttrs (_: forAllSystems) {
      devShells = pkgs: { default = import ./nix/shell pkgs; };
      # for `nix fmt`
      formatter = pkgs: treefmt.${pkgs.system}.config.build.wrapper;
      # for `nix flake check`
      checks = pkgs: {
        formatting = treefmt.${pkgs.system}.config.build.check self;
      };
      packages =
        pkgs:
        import ./nix/packages {
          inherit
            inputs
            pkgs
            lib
            self
            ;
        };
    };
}
