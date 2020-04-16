{ nixpkgs ? import <nixpkgs> {}, port ? "ttyACM0", usb ? "false" }:
with nixpkgs;
let
  specpath = ./spec/default.nix;
  spec = import specpath { inherit nixpkgs; };
  duebuilderpath = fetchFromGitHub {
    owner = "avieth";
    repo = "arduino";
    rev = "67f14ddb06da3ded0f312c9621d991f1389644e2";
    sha256 = "194nh4vp7zlawynh9z0f1ndr0ycyj8ywy4fzrm2w1a2j6a0r9rli";
  };
  duebuilder = import duebuilderpath {
    inherit nixpkgs;
    usb_manufacturer = "Robotique Haply, Inc.";
    usb_product = "Rideau Mk II Ventilator";
  };
  inputs = [ pkgs.coreutils ];
in
  stdenv.mkDerivation {
    name = "os-ventilator";
    version = "0.0.0.1";
    builder = "${bash}/bin/bash";
    args = [ ./build.sh ];
    buildInputs = inputs;
    src = ./arduinobits;
    spec = spec;
    duebuilder = duebuilder;
    port = port;
    usb = usb;
  }
