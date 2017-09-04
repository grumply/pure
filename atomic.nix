{ mkDerivation, atomic-app, atomic-attributes, atomic-base
, atomic-css, atomic-dom, atomic-html, atomic-module, atomic-router
, atomic-server, atomic-service, atomic-signals, atomic-svg
, atomic-types, atomic-websocket, base, ef, ef-base, stdenv
, template-haskell
}:
mkDerivation {
  pname = "atomic";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    atomic-app atomic-attributes atomic-base atomic-css atomic-dom
    atomic-html atomic-module atomic-router atomic-server
    atomic-service atomic-signals atomic-svg atomic-types
    atomic-websocket base ef ef-base template-haskell
  ];
  homepage = "github.com/grumply/atomic";
  description = "The atomic web framework";
  license = stdenv.lib.licenses.bsd3;
}
