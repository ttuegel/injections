{ default ? import ./default.nix {}
}:

let
  inherit (default) shellFor;

  sources = import ./nix/sources.nix;
  nix-haskell-hls = import sources."nix-haskell-hls" { ghcVersion = "ghc8104"; };
  inherit (nix-haskell-hls) hls-renamed;
in

shellFor {
  buildInputs = [ hls-renamed ];
}
