_: {
  perSystem =
    { pkgs, ... }:
    {
      _module.args = {
        readYAML = pkgs.callPackage ./function.nix { };
      };
    };
}
