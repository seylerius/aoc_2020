{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell { buildInputs = [ pkgs.leiningen pkgs.clojure-lsp pkgs.clj-kondo ]; }
