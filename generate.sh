#!/bin/sh

`nix-build ./spec/default.nix`/bin/generate-c
mv ventilator.c ventilator.h ./arduinobits/
