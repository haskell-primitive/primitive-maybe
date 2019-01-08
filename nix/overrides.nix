{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  primitive-maybe = (
    with rec {
      primitive-maybeSource = pkgs.lib.cleanSource ../.;
      primitive-maybeBasic  = self.callCabal2nix "primitive-maybe" primitive-maybeSource { };
    };
    overrideCabal primitive-maybeBasic (old: {
    })
  );
}
