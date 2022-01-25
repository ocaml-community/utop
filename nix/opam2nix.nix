{ source ? builtins.fetchTarball "https://github.com/timbertson/opam2nix/archive/v1.tar.gz"
, pkgs
, ocamlPackagesOverride }:
import source { inherit pkgs ocamlPackagesOverride; }
