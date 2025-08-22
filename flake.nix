{
  description = "Geppetto: A G-code manipulation library for Clojure";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };
  outputs = { self, nixpkgs, flake-utils }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        geppetto = pkgs.callPackage ./derivation.nix {};
      in {
        packages = {
          default = geppetto;
          inherit geppetto;
        };
        checks = {
          test = pkgs.runCommandNoCC "geppetto-test" {} ''
            mkdir -p $out
            : ${geppetto}
          '';
        };
    })) // {
      overlays.default = final: prev: {
        geppetto = prev.callPackage ./derivation.nix {};
      };
    };
}
