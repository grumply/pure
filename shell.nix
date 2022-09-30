(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
  
  crossPlatforms = p: [ p.ghcjs ];
}
