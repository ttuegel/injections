{ default ? import ./default.nix {}
}:

let
  inherit (default) shellFor;

  sources = import ./nix/sources.nix;
  haskell-language-server = import sources."haskell-language-server-nix" {
    ghc-versions = [ "8.10.2" ];
  };
in

shellFor {
  buildInputs = [ haskell-language-server ];
}
