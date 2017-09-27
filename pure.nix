{ mkDerivation, aeson, base, bytestring, containers, ef, ef-base
, ghc-prim, hashable, io-streams, network, random, stdenv
, template-haskell, text, time, tlc, trivial, unordered-containers
, vector, websockets
}:
mkDerivation {
  pname = "pure";
  version = "0.6.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers ef ef-base ghc-prim hashable
    io-streams network random template-haskell text time tlc trivial
    unordered-containers vector websockets
  ];
  homepage = "github.com/grumply/pure";
  description = "Pure application framework";
  license = stdenv.lib.licenses.bsd3;
}
