{ mkDerivation, ghc, base, pure-core, pure-default, pure-dom
, pure-events, pure-html, pure-lifted, pure-server, pure-styles
, pure-time, pure-websocket, stdenv
, secure ? false
, debugws ? false
, debugapi ? false
, devel ? false
, useTemplateHaskell ? true
}:
mkDerivation {
  pname = "pure";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure-core pure-default pure-dom pure-events pure-html
    pure-lifted pure-styles pure-time
    ] ++ (if ghc.isGhcjs or false then [ ] else [
        pure-server pure-websocket
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
  license = stdenv.lib.licenses.bsd3;
}
