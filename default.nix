{}:

let sources = import ./nix/sources.nix; in
let
  inherit (import sources."haskell.nix" {}) pkgs;
  inherit (pkgs) haskell-nix;
  ttuegel = import sources."nix-lib" { inherit pkgs; };
in

pkgs.haskell-nix.project {
  src = ttuegel.cleanGitSubtree { name = "injection"; src = ./.; };
}
