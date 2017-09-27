{ compiler ? "ghcjsHEAD"
, secure ? false
, debugws ? false
, devel ? false
, debugapi ? false
}:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = new: old: rec {

              ef =
                new.callPackage ./deps/ef/ef.nix { };

              ef-base =
                new.callPackage ./deps/ef-base/ef-base.nix { };

              tlc =
                new.callPackage ./deps/tlc/tlc.nix { };

              trivial =
                new.callPackage ./deps/trivial/trivial.nix { };

              pure =
                new.callPackage ./pure.nix { secure=secure; debugws=debugws; devel=devel; debugapi=debugapi; };

            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { trivial = pkgs.haskell.packages.${compiler}.trivial;
    ef = pkgs.haskell.packages.${compiler}.ef;
    ef-base = pkgs.haskell.packages.${compiler}.ef-base;
    tlc = pkgs.haskell.packages.${compiler}.tlc;
    atomic = pkgs.haskell.packages.${compiler}.pure;
  }

