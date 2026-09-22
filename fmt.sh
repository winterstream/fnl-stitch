#!/bin/sh
set -eu

cd "$(dirname "$0")"
find . -type f -name '*.fnl' -exec nix develop --command fnlfmt --fix {} +
