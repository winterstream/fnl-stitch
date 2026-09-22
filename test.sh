#!/bin/sh
set -eu

cd "$(dirname "$0")"
./lint.sh
exec fennel test/run.fnl
