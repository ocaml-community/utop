{ pkgs ? import <nixpkgs> {}
, ocamlVersion ? import ./nix/ocamlDefaultVersion.nix
, opam2nix ?
    pkgs.callPackage ./nix/opam2nix.nix {
      inherit pkgs;
      ocamlPackagesOverride = pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}";
} }:

pkgs.callPackage ./nix { inherit ocamlVersion opam2nix; }
