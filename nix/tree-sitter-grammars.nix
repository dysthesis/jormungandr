{ pkgs }:
let
  names = [
    "awk" "bibtex" "blueprint" "commonlisp" "latex" "make" "nu" "org"
    "perl" "proto" "r" "sql" "surface" "toml" "typst" "verilog" "vhdl"
    "vue" "wast" "wat" "wgsl"
  ];
  have = builtins.filter (name: builtins.hasAttr name pkgs.tree-sitter-grammars) names;
in map (name: pkgs.tree-sitter-grammars.${name}) have
