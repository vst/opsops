{
  lib,
  writeShellApplication,
  bash,
  coreutils,
  moreutils,
  yq-go,
}:

writeShellApplication {
  name = "cabal-verify";

  text = builtins.readFile ./script.sh;

  runtimeInputs = [
    bash
    coreutils
    moreutils
    yq-go
  ];

  meta = with lib; {
    description = "Run project verification checks (format, lint, build, test, docs, etc...)";
    platforms = platforms.all;
  };
}
