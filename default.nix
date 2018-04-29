{ mkDerivation, ghc, ghcjs-base, aeson, aeson-pretty, base, bytestring, containers
, ef, ef-base, ghc-prim, hashable, io-streams, network, random
, stdenv, template-haskell, text, time, tlc, trivial
, unordered-containers, vector, websockets, roles
, secure ? false
, debugws ? false
, debugapi ? false
, devel ? false
, useTemplateHaskell ? true
}:
mkDerivation {
  pname = "pure";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers ef ef-base ghc-prim
    hashable text time tlc trivial unordered-containers
    vector websockets roles
  ] ++ (if useTemplateHaskell then [ template-haskell ] else [])
    ++ (if ghc.isGhcjs or false then [ ghcjs-base ] else [
        aeson-pretty io-streams network random websockets
    ]);
  configureFlags =
    [ (secure ? "-fsecure")
      (debugws ? "-fdebugws")
      (debugapi ? "-fdebugapi")
      (devel ? "-fdevel")
    ] ++ (if useTemplateHaskell then [] else [
      "-f-use-template-haskell"
    ]);
  homepage = "github.com/grumply/pure";
  description = "Pure application framework";
  license = stdenv.lib.licenses.bsd3;
}
