{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let ventilator-spec-src = ./ventilator-spec.nix;

    copilot-core-repo = fetchFromGitHub {
      owner = "avieth";
      repo = "copilot-core";
      rev = "dc5b32300200bbb5849fc4d27cacd1d66506a995";
      sha256 = "0s952s55b0aji1dm1869mgqn46np6dasg95w06xmxvbx3jkfyffs";
    };
    copilot-language-repo = fetchFromGitHub {
      owner = "avieth";
      repo = "copilot-language";
      rev = "66413fde4eeff7ba28d3bd7c1a144cff5862f633";
      sha256 = "0zk5dnii77dvkn630c1iabdk305jwcxsvfxkry6l71hj0gik8fi3";
    };
    copilot-c99-repo = fetchFromGitHub {
      owner = "avieth";
      repo = "copilot-c99";
      rev = "cb0863fbad1998343e058d52d601aedadf21b0ce";
      sha256 = "0z6rrxljmbjfykldfcvw6m35kq4g5k7wd0kn49h955xayja7hiap";
    };

    copilot-core-src = copilot-core-repo + /copilot-core.nix;
    copilot-language-src = copilot-language-repo + /copilot-language.nix;
    copilot-c99-src = copilot-c99-repo + /copilot-c99.nix;
  
    # Include our ventilator spec in the GHC packages.
    ghcPackageOverrides = self: super: {
      ventilator-spec = self.callPackage ventilator-spec-src {};
      # Some copilot components require patches.
      copilot-core = self.callPackage copilot-core-src {};
      copilot-language = self.callPackage copilot-language-src {};
      copilot-c99 = self.callPackage copilot-c99-src {};
      # Other copilot stuff must be set to proper hackage revisions to meet
      # version bounds.
      copilot = self.callPackage ./copilot.nix {};
      copilot-theorem = self.callPackage ./copilot-theorem.nix {};
      bimap = self.callHackage "bimap" "0.3.3" {};
      copilot-libraries = self.callPackage ./copilot-libraries.nix {};
    };
    ghcWithLocalPackages = haskell.packages.ghc865.extend ghcPackageOverrides;
in  ghcWithLocalPackages.ventilator-spec
