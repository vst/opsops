{ pkgs, ... }:

## Function that makes a Haskell.
{ haskell
, packages
, overrides
}:
let
  packageFromSpec = self: { name, path }: {
    ${name} = self.callCabal2nix name path { };
  };
in
haskell.override {
  overrides = self: super: builtins.foldl' (a: c: a // packageFromSpec self c) (overrides self super) packages;
}
