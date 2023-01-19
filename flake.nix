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
          config.allowUnfree = true;
        };
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc944;
        # packages = {
        #   You can add more than one local package here.
        #   my-package.root = ./.;  # Assumes ./my-package.cabal
        # };
          buildTools = hp: {
            inherit (pkgs)
              lambdabot
              chromedriver
              google-chrome
              zlib;
            inherit (hp)
              implicit-hie_0_1_4_0
              fourmolu;
          };
          # overrides = self: super: { };
          overrides = self: super: rec {
            ghcid = pkgs.haskell.lib.dontCheck super.ghcid;
            hlint = pkgs.haskell.lib.dontCheck (
              self.callHackageDirect
                {
                  pkg = "hlint";
                  ver = "3.5";
                  # sha256 = pkgs.lib.fakeSha256;
                  sha256 = "qQNUlQQnahUGEO92Lm0RwjTGBGr2Yaw0KRuFRMoc5No=";
                }
                { }
            );
          };
          # hlintCheck.enable = true;
          # hlsCheck.enable = true;
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        # packages.default = self'.packages.scanweb;
      };
    };
}
