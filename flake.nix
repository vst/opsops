{
  description = "Haskell Project Template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ## Import nixpkgs:
        pkgs = import nixpkgs { inherit system; };

        ## Load readYAML helper:
        readYAML = pkgs.callPackage ./nix/read-yaml.nix { };

        ## Read package information:
        package = readYAML ./package.yaml;

        ## Get our Haskell:
        thisHaskell = pkgs.haskellPackages.override {
          overrides = self: super: {
            ${package.name} = self.callCabal2nix package.name ./. { };
          };
        };

        ## Prepare dev-test-build script:
        dev-test-build = pkgs.writeShellApplication {
          name = "cabal-dev-test-build";
          text = builtins.readFile ./nix/dev-test-build.sh;
          runtimeInputs = [ pkgs.bash pkgs.bc pkgs.moreutils ];
        };

        ## Prepare Nix shell:
        thisShell = thisHaskell.shellFor {
          ## Define packages for the shell:
          packages = p: [ p.${package.name} ];

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
            thisHaskell.weeder

            ## Our development scripts:
            dev-test-build

            ## Other build inputs for various development requirements:
            pkgs.docker-client
            pkgs.git
            pkgs.nil
            pkgs.nixpkgs-fmt
            pkgs.nodePackages.prettier
            pkgs.upx
          ];
        };

        thisPackage = pkgs.haskell.lib.justStaticExecutables (
          thisHaskell.${package.name}.overrideAttrs (oldAttrs: {
            nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [
              pkgs.git
              pkgs.installShellFiles
              pkgs.makeWrapper
              pkgs.ronn
            ];

            postFixup = (oldAttrs.postFixup or "") + ''
              ## Create output directories:
              mkdir -p $out/{bin}

              ## Wrap program to add PATHs to dependencies:
              wrapProgram $out/bin/${package.name} --prefix PATH : ${pkgs.lib.makeBinPath [
                pkgs.bashInteractive ## Added for bash-based CLI option completions
              ]}

              ## Install completion scripts:
              installShellCompletion --bash --name ${package.name}.bash <($out/bin/${package.name} --bash-completion-script "$out/bin/${package.name}")
              installShellCompletion --fish --name ${package.name}.fish <($out/bin/${package.name} --fish-completion-script "$out/bin/${package.name}")
              installShellCompletion --zsh  --name _${package.name}     <($out/bin/${package.name} --zsh-completion-script  "$out/bin/${package.name}")
            '';
          })
        );

        thisDocker = pkgs.dockerTools.buildImage {
          name = "${package.name}";
          tag = "v${package.version}";
          created = "now";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ pkgs.cacert ];
            pathsToLink = [ "/etc" ];
          };

          runAsRoot = ''
            #!${pkgs.runtimeShell}
            ${pkgs.dockerTools.shadowSetup}
            groupadd -r users
            useradd -r -g users patron
          '';

          config = {
            User = "patron";
            Entrypoint = [ "${thisPackage}/bin/${package.name}" ];
            Cmd = null;
          };
        };
      in
      {
        ## Project packages output:
        packages = {
          "${package.name}" = thisPackage;
          docker = thisDocker;
          default = self.packages.${system}.${package.name};
        };

        ## Project development shell output:
        devShells = {
          default = thisShell;
        };
      });
}
