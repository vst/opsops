{
  description = "opsops - SOPS(-Nix) Goodies";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/flake-modules/read-yaml
      ];

      systems = nixpkgs.lib.systems.flakeExposed;

      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          readYAML,
          ...
        }:
        let
          ## Read package information:
          package = readYAML ./package.yaml;

          ## Get our Haskell:
          thisHaskell = pkgs.haskellPackages.override {
            overrides = self: super: {
              ${package.name} = self.callCabal2nix package.name ./. { };
            };
          };

          ## Common build inputs for both development and CI environments:
          buildInputsCommon = [
            ## Essential Haskell tools:
            thisHaskell.cabal-install
            thisHaskell.fourmolu
            thisHaskell.hlint
            thisHaskell.hpack
            thisHaskell.stan
            thisHaskell.weeder

            ## Other essentials:
            pkgs.git
            pkgs.nixfmt-rfc-style
            pkgs.prettier
            pkgs.shellcheck
            pkgs.shfmt
            pkgs.statix
            pkgs.taplo

            ## Our development scripts:
            (pkgs.callPackage ./nix/cabal-verify { })
          ];

          ## Development-only inputs:
          buildInputsDevOnly = [
            ## Haskell development tools:
            thisHaskell.haskell-language-server
            thisHaskell.cabal-fmt
            thisHaskell.cabal2nix

            ## Other development tools:
            pkgs.docker-client
            pkgs.nil
          ];

          ## Development shell:
          devShell = thisHaskell.shellFor {
            packages = p: [ p.${package.name} ];
            withHoogle = false;
            buildInputs = buildInputsCommon ++ buildInputsDevOnly;
          };

          ## CI shell (minimal, fast):
          ciShell = thisHaskell.shellFor {
            packages = p: [ p.${package.name} ];
            withHoogle = false;
            buildInputs = buildInputsCommon;
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
                wrapProgram $out/bin/${package.name} --prefix PATH : ${
                  pkgs.lib.makeBinPath [
                    pkgs.bashInteractive # Added for bash-based CLI option completions
                  ]
                }

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
            default = thisPackage;
          };

          ## Project development shells:
          devShells = {
            default = devShell;
            ci = ciShell;
          };
        };
    };
}
