{
  no-clown-fiesta-theme,
  pkgs,
  lib,
  emacs-unstable-pgtk,
  emacsWithPackagesFromUsePackage,
  runCommand,
}: let
  baseEmacs = emacs-unstable-pgtk;
  inherit (lib.babel.pkgs) mkWrapper;
  inherit (lib.babel.emacs) orgTangle;
  inherit (lib) mapAttrs;
  orgFile = ../../README.org;
  initFile = orgTangle pkgs baseEmacs orgFile "init.org";

  npins = import ./npins;

  # TODO: make this native compile plugins
  elisp = src: file:
    pkgs.runCommand "${file}.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp -r ${src}/* $out/share/emacs/site-lisp/
    '';

  # Build all packages that are collected with npins
  mkNpins = mapAttrs (name: value: elisp value name);

  builtNpins = mkNpins npins;

  cfg =
    runCommand "init.el"
    {
      buildInputs = [baseEmacs];
      src = "${initFile}/init.el";
    }
    ''
      mkdir -p $out
      mkdir -p $out/eln-cache

      echo "(setq rmh-elfeed-org-files (list \"${../../elfeed.org}\"))" >> $out/init.el
      cat $src >> $out/init.el

      emacs --batch \
        --eval "(setq native-comp-eln-load-path (list \"$out/eln-cache\"))" \
        --eval "(native-compile \"$out/init.el\")"
    '';

  package = baseEmacs.overrideAttrs (old: {
    postInstall =
      # bash
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
    extraEmacsPackages = epkgs:
      with epkgs; [
        use-package
        autothemer
      ];
    override = epkgs: epkgs // builtNpins // {inherit no-clown-fiesta-theme;};
  };
  deps = with pkgs; [
    fd
    ripgrep
    texliveFull
    unzip
    emacs-lsp-booster
  ];
in
  builtins.trace "init.el file is at ${config}" mkWrapper pkgs final ''
    for file in $out/bin/emacs $out/bin/emacs-*; do
      wrapProgram $file \
        --prefix PATH ":" "${lib.makeBinPath deps}"
    done
  ''
