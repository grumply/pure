{ mkDerivation, ghc, base, pure-core, pure-default, pure-dom
, pure-events, pure-html, pure-lifted, pure-theme , pure-time
, pure-txt, pure-styles, stdenv
}:
mkDerivation {
  pname = "pure";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure-core pure-default pure-dom pure-events pure-html
    pure-lifted pure-theme pure-time pure-txt pure-styles
    ];
  homepage = "github.com/grumply/pure";
  license = stdenv.lib.licenses.bsd3;
}
