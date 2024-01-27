{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    forester.url = "sourcehut:~jonsterling/ocaml-forester";
    forest-server.url = "github:kentookura/forest-server";
  };

  outputs = { self, nixpkgs, utils, forester, forest-server }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = with pkgs;
          mkShell {
            buildInputs = [
              libev
              opam
              ocamlPackages.odoc
              forester.packages.${system}.default
              forest-serer.packages.${system}.default
            ];
            shellHook = "eval $(opam env --switch=5.1.1)";
          };
      });
}
