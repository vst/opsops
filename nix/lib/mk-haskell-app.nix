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

    ## Create a temporary directory to store interim artifacts:
    _tmpdir=$(mktemp -d)

    ## Create completion scripts:
    $out/bin/${name} --bash-completion-script "$out/bin/${name}" > ''${_tmpdir}/completion.bash
    $out/bin/${name} --fish-completion-script "$out/bin/${name}" > ''${_tmpdir}/completion.fish
    $out/bin/${name}  --zsh-completion-script "$out/bin/${name}" > ''${_tmpdir}/completion.zsh

    ## Install completion scripts:
    installShellCompletion --bash ''${_tmpdir}/completion.bash
    installShellCompletion --fish ''${_tmpdir}/completion.fish
    installShellCompletion  --zsh ''${_tmpdir}/completion.zsh
  '';
in
pkgs.haskell.lib.justStaticExecutables (
  drv.overrideAttrs (oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
    postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
  })
)
