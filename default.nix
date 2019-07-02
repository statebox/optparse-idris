{ nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs {};
in
with pkgs; with idrisPackages; build-idris-package  {
  name = "optparse";
  version = "2019-07-02";

  idrisDeps = [ lens ];

  extraBuildInputs = [ pkgconfig SDL2 SDL2_gfx ];

  src = ./.;

  meta = {
    description = "Minimal port of optparse-applicative to idris";
    homepage = https://github.com/HuwCampbell/optparse-idris;
    maintainers = [ lib.maintainers.brainrape ];
  };
}
