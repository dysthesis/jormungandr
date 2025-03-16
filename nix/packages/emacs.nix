{
  pkgs,
  lib,
  emacs-unstable,
  emacsWithPackagesFromUsePackage,
  runCommand,
}: let
  inherit (lib.babel.pkgs) mkWrapper;
  inherit (lib.babel.emacs) orgTangle;
  orgFile = ../../README.org;
  initFile = orgTangle pkgs emacs-unstable orgFile "init.org";

  cfg =
    runCommand "init.el" {
      buildInputs = [emacs-unstable];
      src = "${initFile}/init.el";
    } ''
      mkdir -p $out
      mkdir -p $out/eln-cache

      cat $src >> $out/init.el

      emacs --batch \
        --eval "(setq native-comp-eln-load-path (list \"$out/eln-cache\"))" \
        --eval "(native-compile \"$out/init.el\")"
    '';

  package = emacs-unstable.overrideAttrs (old: {
    postInstall =
      /*
      bash
      */
      ''
        ${old.postInstall or ""}
        shopt -s globstar
        ln -sf $out/share/emacs/native-lisp/*/* $out/lib/emacs/*/native-lisp/*/
        ln -sf ${cfg}/eln-cache/* $out/lib/emacs/*/native-lisp/*/
        shopt -u globstar
      '';
  });

  config = "${cfg}/init.el";
  final = emacsWithPackagesFromUsePackage {
    inherit package config;
    alwaysEnsure = true;
    defaultInitFile = true;
    extraEmacsPackages = epkgs: [
      epkgs.use-package
    ];
  };
  deps = with pkgs; [
    fd
    ripgrep
    texliveFull
  ];
in
  builtins.trace "init.el file is at ${config}"
  mkWrapper
  pkgs
  final ''
    for file in $out/bin/emacs $out/bin/emacs-*; do
      wrapProgram $file \
        --prefix PATH ":" "${lib.makeBinPath deps}"
    done
  ''
