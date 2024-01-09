{ pkgs, ... }:

## Function that makes a Haskell application.
{ drv
, name ? drv.pname
, nativeBuildInputs ? [ ]
, binPaths ? [ ]
}:
let
  ## We need these inputs at buildtime:
  extraNativeBuildInputs = [
    pkgs.git
    pkgs.makeWrapper
    pkgs.ronn
  ] ++ nativeBuildInputs;

  ## We need these inputs at runtime:
  binPath = pkgs.lib.makeBinPath binPaths;

  ## Post-fixup process:
  extraPostFixup = ''
    ## Wrap program:
    wrapProgram $out/bin/${name} --prefix PATH : ${binPath}
  '';
in
pkgs.haskell.lib.justStaticExecutables (
  drv.overrideAttrs (oldAttrs: rec {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
    postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
  })
)
