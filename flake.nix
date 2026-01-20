{
  description = "sentinel";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];
    extra-trusted-public-keys = [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          localSystem = { inherit system; };
        };

        hlib = pkgs.haskell.lib;

        hpkgs = pkgs.haskell.packages."ghc912".override {
          overrides = self: super: {
            servant-client = hlib.overrideCabal super.servant-client (drv: {
              doCheck = false;
            });
          };
        };

        packageName = "sentinel";

        basePackage = hpkgs.callCabal2nix packageName ./backend { };

        finalPackage = hlib.overrideCabal basePackage (old: {
          doCheck = true;
          doHaddock = false;
          enableLibraryProfiling = false;
          enableExecutableProfiling = false;
        });

        wrappedPackage = pkgs.stdenv.mkDerivation {
          name = "${packageName}";

          nativeBuildInputs = [
            pkgs.makeWrapper
          ];

          src = finalPackage;

          installPhase = ''
            mkdir -p $out
            cp -r ${finalPackage}/* $out/
            chmod +w $out/bin || true
          '';
        };
      in
      {
        packages.default = wrappedPackage;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            # Haskell stuff
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.ormolu
            hpkgs.hlint
            hpkgs.hpack
            hpkgs.implicit-hie
            hpkgs.ghcid
            hpkgs.weeder
            pkgs.zlib
            pkgs.pkg-config

            # General stuff
            pkgs.graphviz
            pkgs.just
            pkgs.nixfmt-rfc-style
            pkgs.treefmt
            pkgs.diffutils
            pkgs.tagref
          ];
        };
      }
    );
}
