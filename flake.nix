{
  inputs = {
    blueprint = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/blueprint";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };
  outputs =
    inputs:
    inputs.blueprint {
      inherit inputs;
    }
    // {
      inherit (inputs) blueprint;
      formatter = inputs.self.lib.mkFormatter { inherit (inputs) self; };
      lib.mkFormatter =
        { self }:
        inputs.nixpkgs.lib.genAttrs (builtins.attrNames inputs.self.packages) (
          system:
          import ./formatter.nix {
            inherit inputs self;
            flake = self.outPath;
            pkgs = inputs.nixpkgs.legacyPackages.${system};
          }
        );
    };
}
