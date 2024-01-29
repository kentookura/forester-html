{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    opam-nix.url = "github:tweag/opam-nix";
  };

  outputs = { self, nixpkgs, utils, opam-nix }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        legacyPackages = let
          inherit (opam-nix.lib.${system}) buildOpamProject;
          scope = buildOpamProject { } "forester_html" ./. {
            ocaml-base-compiler = "5.1.1";
          };
        in scope;
        defaultPackage = self.legacyPackages.${system}.forester_html;
        devShells.default = with pkgs;
          mkShell {
            buildInputs = [ libev pkg-config openssl opam ];
            shellHook = "eval $(opam env)";
          };
      });
}
