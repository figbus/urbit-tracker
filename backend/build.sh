#!/bin/sh

nix build
rm -rf dist
cp -r $(readlink result) dist
rm result

