#!/bin/sh

`nix-build ./spec/default.nix`/bin/spec
mv ventilator.c ventilator.h ./arduinobits/
