{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    let package = "forester_html";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          ocaml-base-compiler = "5.1.1";
          ocaml-lsp-server = "*";
          ocamlformat = "*";
        };
        query = devPackagesQuery // { };
        scope = on.buildOpamProject' { } ./. query;
        overlay = final: prev: {
          ${package} =
            prev.${package}.overrideAttrs (_: { doNixSupport = false; });
        };
        scope' = scope.overrideScope' overlay;
        main = scope'.${package};
        devPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
        dockerImage = pkgs.dockerTools.buildImage {
          name = "forester-html";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ ./static ];
            pathsToLink = [ "/static" ];
          };
          config = { Cmd = [ "${main}/bin/forester_html" ]; };
        };
      in {
        legacyPackages = scope';

        packages.default = main;
        packages.docker = dockerImage;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ main ];
          buildInputs = with pkgs;
            devPackages ++ [
              libev
              pkg-config
              openssl
              # You can add packages from nixpkgs here
            ];
        };
      });
}
