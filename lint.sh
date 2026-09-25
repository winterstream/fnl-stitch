#!/bin/sh
set -eu

cd "$(dirname "$0")"
exec nix develop --command fennel-ls --lint ./*.fnl test/*.fnl examples/*/*.fnl
