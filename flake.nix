{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [( final: prev: {
            all-cabal-hashes = inputs.all-cabal-hashes;
          })];
          config = {
            allowBroken = true;
            allowUnfree = true;
          };
        };
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc944;
          # packages = {
          #   You can add more than one local package here.
          #   my-package.root = ./.;  # Assumes ./my-package.cabal
          # };
          buildTools = hp: {
            inherit (pkgs)
              chromedriver
              google-chrome
              lambdabot
              neovim
              zlib;
            inherit (hp)
              implicit-hie
              fourmolu;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            ghcid = dontCheck super.ghcid;
          };
          # hlintCheck.enable = true;
          # hlsCheck.enable = true;
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        # packages.default = self'.packages.scanweb;
      };
    };
}
