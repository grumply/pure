{ compiler ? "ghcjsHEAD"
, secure ? false
, debugws ? false
, devel ? false
}:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          extension = new: old: {
            cabal = pkgs.haskell.packages.cabalNoTest;
          };
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

              atomic-base =
                new.callPackage ./deps/atomic-base/atomic-base.nix { };

              atomic-types =
                new.callPackage ./deps/atomic-types/atomic-types.nix { };

              atomic-app =
                new.callPackage ./deps/atomic-app/atomic-app.nix { };

              atomic-attributes =
                new.callPackage ./deps/atomic-attributes/atomic-attributes.nix { };

              atomic-css =
                new.callPackage ./deps/atomic-css/atomic-css.nix { };

              atomic-dom =
                new.callPackage ./deps/atomic-dom/atomic-dom.nix { };

              atomic-html =
                new.callPackage ./deps/atomic-html/atomic-html.nix { };

              atomic-module =
                new.callPackage ./deps/atomic-module/atomic-module.nix { };

              atomic-router =
                new.callPackage ./deps/atomic-router/atomic-router.nix { };

              atomic-server =
                new.callPackage ./deps/atomic-server/atomic-server.nix { };

              atomic-service =
                new.callPackage ./deps/atomic-service/atomic-service.nix { };

              atomic-signals =
                new.callPackage ./deps/atomic-signals/atomic-signals.nix { };

              atomic-svg =
                new.callPackage ./deps/atomic-svg/atomic-svg.nix { };

              atomic-websocket =
                new.callPackage ./deps/atomic-websocket/atomic-websocket.nix { secure=secure; debugws=debugws; devel=devel; };

              atomic =
                new.callPackage ./atomic.nix { };

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
    trivial = pkgs.haskell.packages.${compiler}.trivial;
    atomic-base = pkgs.haskell.packages.${compiler}.atomic-base;
    atomic-types = pkgs.haskell.packages.${compiler}.atomic-types;
    atomic-app = pkgs.haskell.packages.${compiler}.atomic-app;
    atomic-attributes = pkgs.haskell.packages.${compiler}.atomic-attributes;
    atomic-css = pkgs.haskell.packages.${compiler}.atomic-css;
    atomic-dom = pkgs.haskell.packages.${compiler}.atomic-dom;
    atomic-html = pkgs.haskell.packages.${compiler}.atomic-html;
    atomic-module = pkgs.haskell.packages.${compiler}.atomic-module;
    atomic-router = pkgs.haskell.packages.${compiler}.atomic-router;
    atomic-server = pkgs.haskell.packages.${compiler}.atomic-server;
    atomic-service = pkgs.haskell.packages.${compiler}.atomic-service;
    atomic-signals = pkgs.haskell.packages.${compiler}.atomic-signals;
    atomic-svg = pkgs.haskell.packages.${compiler}.atomic-svg;
    atomic-websocket = pkgs.haskell.packages.${compiler}.atomic-websocket;
    atomic = pkgs.haskell.packages.${compiler}.atomic;
  }

