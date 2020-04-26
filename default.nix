{ nixpkgs ? import <nixpkgs> {}, port ? "ttyACM0", usb ? "false" }:
with nixpkgs;
let
  specpath = ./spec/default.nix;
  spec = import specpath { inherit nixpkgs; };
  duebuilderpath = fetchFromGitHub {
    owner = "avieth";
    repo = "arduino";
    rev = "5bb1b5250c61e56dde62ad418eb40737974d6ebb";
    sha256 = "12sai32d6fh6f7nbaf2a9v53fhxwm74y71hmaynykgi4k0sl5fg6";
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
