{
  description = "Collie-the-bot";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    nixpak = {
      url = "github:max-privatevoid/nixpak";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, flake-utils, haskellNix, pre-commit-hooks, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ haskellNix.overlay ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        mkNixPak = inputs.nixpak.lib.nixpak {
          inherit (pkgs) lib;
          inherit pkgs;
        };
        sandboxed-ghci = mkNixPak {
          config = _: {
            app.package =
              (pkgs.ghc.withPackages (gPkgs: with gPkgs; [
                aeson
                template-haskell
                QuickCheck
                tagsoup
                http-conduit
                megaparsec
                containers
                optics
              ]));
            app.binPath = "bin/ghci";
            dbus.enable = false;
          };
        };
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          settings = {
            ormolu.defaultExtensions = [
              "TypeApplications"
              "PatternSynonyms"
            ];
          };

          hooks = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
            statix.enable = true;
            deadnix.enable = true;
          };
        };
        project = pkgs.haskell-nix.project' {
          inherit (pre-commit-check.shellHook);
          src = ./.;
          compiler-nix-name = "ghc964";
          shell = {
            tools = {
              cabal = { };
              hlint = { };
              haskell-language-server = { };
            };
            nativeBuildInputs = with pkgs; [
              nixpkgs-fmt
              fd
              git
              gnumake
            ];
          };
        };
        flake = project.flake { };
      in
      flake // rec {
        inherit project;
        checks = flake.checks // { formatting-checks = pre-commit-check; };
        packages.ghci = sandboxed-ghci.config.script;
        packages.collie-the-bot = flake.packages."collie-the-bot:exe:collie-the-bot";
        packages.runBot =
          pkgs.writeShellScript "RunCollie" ''
            export COLLIE_GHCI_PATH=${packages.ghci}/bin/ghci
            ${packages.collie-the-bot}/bin/collie-the-bot
          '';
      });
}
