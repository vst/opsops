# Haskell Project Template

This is an opinionated template for creating Haskell projects. It uses
[Nix], [hpack] and [cabal].

> **TODO** Provide minimum viable documentation.

## Quickstart

Create your repository from this template, clone it on your computer
and enter its directory.

Then, run following to configure your project:

```sh
bash ./run-template.sh
```

It will prompt some questions and configure your project according to
your answers.

Once it is configured, provision `direnv`:

```sh
direnv allow
```

And run the big, long build command as given in the next section.

Finally, you can remove the `run-template.sh` script:

```sh
rm run-template.sh
```

## Development

Big, long build command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 haskell-template-hebele -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

<!-- REFERENCES -->

[Nix]: https://nixos.org
[hpack]: https://github.com/sol/hpack
[cabal]: https://www.haskell.org/cabal
