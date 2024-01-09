# opsops: SOPS(-Nix) Goodies

> **TODO** Provide minimum viable documentation.

## Development

Provision `direnv`:

```sh
direnv allow
```

Big, long build command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 opsops -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

<!-- REFERENCES -->
