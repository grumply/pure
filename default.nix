{ mkDerivation, ghc, base, pure-core, pure-default, pure-dom
, pure-events, pure-html, pure-lifted, pure-styles
, pure-time, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure-core pure-default pure-dom pure-events pure-html
    pure-lifted pure-styles pure-time pure-txt
    ];
  homepage = "github.com/grumply/pure";
  license = stdenv.lib.licenses.bsd3;
}
