{ nixpkgs ? import <nixpkgs> {}, port ? "ttyACM0", usb ? "false" }:
with nixpkgs;
let
  duebuilderpath = fetchFromGitHub {
    owner = "avieth";
    repo = "arduino";
    rev = "0993491796bd201bd7575a7e66f0b231fbbebe50";
    sha256 = "09s7jad0rn393ad9j2kgfxzsgj0smyz2a9y2yrxwnf4hs1haagmg";
  };
  duebuilder = import duebuilderpath { inherit nixpkgs; };
  inputs = [ pkgs.coreutils ];
in
  stdenv.mkDerivation {
    name = "os-ventilator";
    version = "0.0.0.1";
    builder = "${bash}/bin/bash";
    args = [ ./build.sh ];
    buildInputs = inputs;
    src = ./arduinobits;
    duebuilder = duebuilder;
    port = port;
    usb = usb;
  }
