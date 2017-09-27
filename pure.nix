{ mkDerivation, aeson, base, bytestring, containers, ef, ef-base
, ghc-prim, hashable, io-streams, network, random, stdenv
, template-haskell, text, time, tlc, trivial, unordered-containers
, vector, websockets
, secure, debugws, debugapi, devel
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
  configureFlags =
    [ (secure ? "-fsecure")
      (debugws ? "-fdebugws")
      (debugapi ? "-fdebugapi")
      (devel ? "-fdevel")
    ];
  homepage = "github.com/grumply/pure";
  description = "Pure application framework";
  license = stdenv.lib.licenses.bsd3;
}
