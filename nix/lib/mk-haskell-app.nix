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
    pkgs.installShellFiles
    pkgs.makeWrapper
    pkgs.ronn
  ] ++ nativeBuildInputs;

  ## We need these inputs at runtime:
  binPath = pkgs.lib.makeBinPath binPaths;

  ## Post-fixup process:
  extraPostFixup = ''
    ## Wrap program:
    wrapProgram $out/bin/${name} --prefix PATH : ${binPath}

    ## Install completion scripts:
    installShellCompletion --bash --name ${name}.bash <($out/bin/${name} --bash-completion-script "$out/bin/${name}")
    installShellCompletion --fish --name ${name}.fish <($out/bin/${name} --fish-completion-script "$out/bin/${name}")
    installShellCompletion --zsh  --name _${name}     <($out/bin/${name} --zsh-completion-script  "$out/bin/${name}")
  '';
in
pkgs.haskell.lib.justStaticExecutables (
  drv.overrideAttrs (oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
    postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
  })
)
