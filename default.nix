{ sources ? import ./nix/sources.nix
, compiler ? "default"
, system ? builtins.currentSystem
, ...
}:

let
  ##################
  ## LOAD NIXPKGS ##
  ##################

  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { inherit system; };

  ##################
  ## LOAD HELPERS ##
  ##################

  ## Load the YAML reader:
  readYAML = pkgs.callPackage ./nix/lib/read-yaml.nix { };

  ## Load Haskell package factory:
  mkHaskell = pkgs.callPackage ./nix/lib/mk-haskell.nix { };

  ## Load Haskell application factory:
  mkHaskellApp = pkgs.callPackage ./nix/lib/mk-haskell-app.nix { };

  ## Load Docker image factory for Haskell application:
  mkHaskellDocker = pkgs.callPackage ./nix/lib/mk-haskell-docker.nix { };

  ###########################
  ## ESSENTIAL INFORMATION ##
  ###########################

  ## Get the main Haskell package specification:
  packageSpec = readYAML (thisHaskellPackages.main.path + "/package.yaml");

  #############
  ## HASKELL ##
  #############

  ## Get Haskell packages in the project:
  thisHaskellPackages = {
    main = {
      name = packageSpec.name;
      path = ./.;
    };
    subs = [ ];
  };

  ## Get Haskell packages in the project as a list:
  thisHaskellPackagesAll = [ thisHaskellPackages.main ] ++ thisHaskellPackages.subs;

  ## Get base Haskell package set:
  baseHaskell = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};

  ## Get this Haskell package set:
  thisHaskell = mkHaskell {
    haskell = baseHaskell;
    packages = thisHaskellPackagesAll;
    overrides = self: super: { };
  };

  ###########
  ## SHELL ##
  ###########

  dev-test-build = pkgs.writeShellScriptBin "dev-test-build" ''
    #!/usr/bin/env bash

    ## Fail on any error:
    set -e

    ## Show commands executed:
    set -x

    hpack
    fourmolu --mode check app/ src/ test/
    prettier --check .
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt --check &&
    hlint app/ src/ test/
    cabal build -O0
    cabal run -O0 opsops -- --version
    cabal v1-test
    cabal haddock -O0
  '';

  ## Prepare Nix shell:
  thisShell = thisHaskell.shellFor {
    ## Define packages for the shell:
    packages = p: builtins.map (x: p.${x.name}) thisHaskellPackagesAll;

    ## Enable Hoogle:
    withHoogle = false;

    ## Build inputs for development shell:
    buildInputs = [
      ## Haskell related build inputs:
      thisHaskell.apply-refact
      thisHaskell.cabal-fmt
      thisHaskell.cabal-install
      thisHaskell.cabal2nix
      thisHaskell.fourmolu
      thisHaskell.haskell-language-server
      thisHaskell.hlint
      thisHaskell.hpack

      ## Other build inputs for various development requirements:
      pkgs.docker-client
      pkgs.git
      pkgs.nil
      pkgs.nixpkgs-fmt
      pkgs.nodePackages.prettier

      ## Our custom scripts:
      dev-test-build
    ];
  };

  #################
  ## APPLICATION ##
  #################

  ## Get the installable application (only static executable):
  thisApp = mkHaskellApp {
    drv = thisHaskell.${thisHaskellPackages.main.name};
    binPaths = [
      pkgs.bashInteractive ## Added for bash-based CLI option completions
    ];
  };

  ############
  ## DOCKER ##
  ############

  thisDocker = mkHaskellDocker {
    app = thisApp;
    repository = thisHaskell.${thisHaskellPackages.main.name};
  };
in
{
  shell = thisShell;
  app = thisApp;
  docker = thisDocker;
}
