{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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
          overlays = [( self: super: {
            all-cabal-hashes = inputs.all-cabal-hashes;
            my-neovim = self.callPackage ./vim.nix {};
            my-vscode = super.vscode-with-extensions.override {
              vscodeExtensions = with super.vscode-extensions; [
                asvetliakov.vscode-neovim
                bbenoist.nix
                haskell.haskell
                justusadam.language-haskell
                ms-python.python
              ]
              ++ super.vscode-utils.extensionsFromVscodeMarketplace [
                {
                  name = "groovylambda";
                  publisher = "sheaf";
                  version = "0.1.0";
                  sha256 = "1j2w6y90qwzaima1gg6vb9fij400hxfbxlla1a55hqjls6157zbf";
                }
              ];
            };
          })];
          config = {
            # for script-monad & wedriver-w3c
            allowBroken = true;
            # for google-chrome
            allowUnfree = true;
          };
        };
        haskellProjects.default = {
          # packages.example.root = ./.;  # This value is detected based on .cabal files
          overrides = self: super: with pkgs.haskell.lib; {
            script-monad = dontCheck super.script-monad;
            webdriver-w3c = dontCheck super.webdriver-w3c;
          };
          devShell = {
            enable = true;  # Enabled by default
            tools = hp: {
              inherit (pkgs)
                black
                chromedriver
                google-chrome
                lambdabot
                nodejs
                my-neovim
                my-vscode
                python3Full
                sqlitebrowser;
              inherit (pkgs.nodePackages)
                prettier;
              inherit (hp)
                fourmolu;
              inherit (pkgs.python310Packages)
                colorama
                pip
                pylint
                selenium
                virtualenv
                w3lib;
            };
            #  hlsCheck.enable = true;
          };
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.example;
      };
    };
}
