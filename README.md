# opsops: SOPS(-Nix) Goodies

![GitHub Release](https://img.shields.io/github/v/release/vst/opsops)
![GitHub issues](https://img.shields.io/github/issues/vst/opsops)
![GitHub last commit (branch)](https://img.shields.io/github/last-commit/vst/opsops/main)
![GitHub License](https://img.shields.io/github/license/vst/opsops)

`opsops` is a command-line application to generate clear [SOPS]
secrets from a given specification and generate [sops-nix] snippets
for it.

The specification is a YAML/JSON file representing a tree-like
structure where terminal nodes represent how the clear secrets will be
generated, and internal nodes represent the "path" to the clear
secret.

Currently, system processes, scripts and 1password field reference
URIs are supported:

```yaml
secrets:
  zamazingo:
    secret:
      type: "process"
      value:
        command: "zamazingo"
        arguments: ["--hip", "hop"]
  github:
    token:
      type: "script"
      value:
        content: "printf \"%s\" \"$(gh auth token)\""
  example.com:
    password:
      type: "script"
      value:
        interpreter: "python3"
        content: |
          import netrc
          import sys

          _login, _account, password = netrc.netrc().authenticators("example.com")

          sys.stdout.write("password")
  dockerhub:
    password:
      type: "op"
      value:
        account: "PAIT5BAHSH7DAPEING3EEDIE2E"
        vault: "Cloud Accounts"
        item: "yies1Ahl4ahqu1afao4nahshoo"
        field: "password"
  influxdb:
    token:
      type: "op-read"
      value:
        account: "IPAEPH0JI3REE8FICHOOVU4CHA"
        uri: "op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/API Tokens/write-only"
```

<!--toc:start-->
- [opsops: SOPS(-Nix) Goodies](#opsops-sops-nix-goodies)
  - [Installation](#installation)
    - [Using `nix-env`](#using-nix-env)
    - [Using `nix-profile`](#using-nix-profile)
    - [Using `niv`](#using-niv)
  - [Usage](#usage)
    - [Specification](#specification)
    - [See Canonical Specification](#see-canonical-specification)
    - [Render Clear Secrets](#render-clear-secrets)
    - [Create Snippet for `sops-nix`](#create-snippet-for-sops-nix)
  - [Development](#development)
  - [License](#license)
<!--toc:end-->

## Installation

> [!WARNING]
>
> If 1Password is used, 1Password CLI application (`op`) must be on
> `PATH` when running `opsops`.

### Using `nix-env`

```sh
nix-env --install --file https://github.com/vst/opsops/archive/main.tar.gz --attr app
```

### Using `nix-profile`

```sh
nix profile install --file https://github.com/vst/opsops/archive/main.tar.gz app
```

### Using `niv`

```sh
niv add vst/opsops -n opsops
```

... and then:

```sh
sources = import ./nix/sources.nix;
opsops = (import sources.opsops { }).app;
```

... and finally add `opsops` to your system packages, home packages or
Nix shell build inputs.

## Usage

### Specification

A specification is a YAML (or JSON) file. Here is an example:

<details>
  <summary>See Example</summary>

```yaml
## File: opsops.yaml
secrets:
  zamazingo:
    secret:
      type: "process"
      value:
        command: "zamazingo"
        arguments: ["--hip", "hop"]
  github:
    token:
      type: "script"
      value:
        content: "printf \"%s\" \"$(gh auth token)\""
  example.com:
    password:
      type: "script"
      value:
        interpreter: "python3"
        content: |
          import netrc
          import sys

          _login, _account, password = netrc.netrc().authenticators("example.com")

          sys.stdout.write("password")
  dockerhub:
    password:
      type: "op"
      value:
        account: "PAIT5BAHSH7DAPEING3EEDIE2E"
        vault: "Cloud Accounts"
        item: "yies1Ahl4ahqu1afao4nahshoo"
        field: "password"
  influxdb:
    token:
      type: "op-read"
      value:
        account: "IPAEPH0JI3REE8FICHOOVU4CHA"
        uri: "op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/API Tokens/write-only"
```
</details>

### See Canonical Specification

To see canonical/normalized specification:

```sh
opsops normalize --input opsops.yaml
```

<details>
  <summary>See Output</summary>

```yaml
secrets:
  dockerhub:
    password:
      type: op
      value:
        account: PAIT5BAHSH7DAPEING3EEDIE2E
        field: password
        item: yies1Ahl4ahqu1afao4nahshoo
        newline: false
        section: null
        vault: Cloud Accounts
  example.com:
    password:
      type: script
      value:
        arguments: []
        content: |
          import netrc
          import sys

          _login, _account, password = netrc.netrc().authenticators("example.com")

          sys.stdout.write("password")
        interpreter: python3
  github:
    token:
      type: script
      value:
        arguments: []
        content: |
          printf "%s" "$(gh auth token)"
        interpreter: bash
  influxdb:
    token:
      type: op-read
      value:
        account: IPAEPH0JI3REE8FICHOOVU4CHA
        newline: false
        uri: op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/API Tokens/write-only
  zamazingo:
    secret:
      type: process
      value:
        arguments:
        - --hip
        - hop
        command: zamazingo
        environment: {}
```
</details>

### Render Clear Secrets

> [!WARNING]
>
> If 1Password is used, 1Password CLI application (`op`) should be
> authenticated first:
>
> ```sh
> eval $(op signin -f [--account <ACCOUNT>])
> ```

To render clear secrets:

```sh
opsops render --input opsops.yaml
```

<details>
  <summary>See Output</summary>

```yaml
example.com:
  password: password
github:
  token: gho_meecubier5dinohSh3tohphaekuo5Phahpei
zamazingo:
  secret: hebelehubele
dockerhub:
  password: ohbauy5eing8pheSh6iigooweeZee6ch
influxdb:
  token: mu9aephabeadi7zi8goo9peYo8yae7ge
```
</details>

### Create Snippet for `sops-nix`

To create snippet for `sops-nix` that can be copied/pasted inside the
`sops-nix` module configuration:

```sh
opsops snippet sops-nix --input opsops.yaml
```

<details>
  <summary>See Output</summary>

```nix
"dockerhub/password" = {};
"example.com/password" = {};
"github/token" = {};
"influxdb/token" = {};
"zamazingo/secret" = {};
```
</details

... or with some prefix:

```sh
opsops snippet sops-nix --input opsops.yaml --prefix my_namespace
```

<details>
  <summary>See Output</summary>

```nix
"my_namespace/dockerhub/password" = { key = "dockerhub/password"; };
"my_namespace/example.com/password" = { key = "example.com/password"; };
"my_namespace/github/token" = { key = "github/token"; };
"my_namespace/influxdb/token" = { key = "influxdb/token"; };
"my_namespace/zamazingo/secret" = { key = "zamazingo/secret"; };
```
</details>

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

## License

See [LICENSE].

<!-- REFERENCES -->

[LICENSE]: ./LICENSE.md
[SOPS]: https://github.com/getsops/sops
[sops-nix]: https://github.com/Mic92/sops-nix
