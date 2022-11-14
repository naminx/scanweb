{
  description = "scanweb";

  nixConfig = {
    bash-prompt = "\n\\[\\033[0;94m\\][\\[\\e]0;\\u@\\h: \\w\\a\\]\\u@\\h:\\w]\$\\[\\033[0m\\] ";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-flake.url = "github:srid/haskell-flake";

    # script-monad-0.0.4: Override -> dontCheck
    script-monad = {
      url = "github:nbloomf/script-monad/5dbe37b62dbadfd831ffd07c79c1838d391a54a5";
      flake = false;
    };
    # webdriver-w3c-0.0.3: Override -> dontCheck
    webdriver-w3c = {
      url = "github:nbloomf/webdriver-w3c/788f024f730ec967e93ac3c5d2754ab846fb6ece";
      flake = false;
    };
    # Updated 7 Apr 2022 to support lens-5.0 but not updated on hackage
    taggy-lens = {
      url = "github:alpmestan/taggy-lens/87235bfb9c3ee8b3d487c1cf48a22f247e59286d";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { pkgs, inputs', self', ... }: {
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc924;
          root = ./.;
          name = "scanweb";
          buildTools = hp: {
            inherit (pkgs)
                lambdabot;
            #   chromedriver
            #   chromium;
            # inherit (hp)
            #   fourmolu;
          };
          source-overrides = {
            inherit (inputs)
              script-monad
              webdriver-w3c
              taggy-lens;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            script-monad = dontCheck (doJailbreak super.script-monad);
            webdriver-w3c = dontCheck (doJailbreak super.webdriver-w3c);
          };
        };
      };
    };
}
