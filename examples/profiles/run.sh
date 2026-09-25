#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."
exec nix develop --command fennel examples/profiles/main.fnl
