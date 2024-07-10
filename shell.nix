{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) lib;
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
in

pkgs.mkShell {
  # Required for correct locales on non-NixOS,
  # see https://nixos.wiki/wiki/Locales
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  nativeBuildInputs = with ocamlPackages; [
    ocaml
    findlib
    dune_3
  ];

  buildInputs = with ocamlPackages; [
    fmt
    qcheck
    core_bench
  ];
}
