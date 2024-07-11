{
  description = "TODO: fill me in";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
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
