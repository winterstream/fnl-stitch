#!/bin/sh
set -eu

cd "$(dirname "$0")"
./lint.sh
fennel test/run.fnl
./examples/basic/run.sh
exec ./examples/profiles/run.sh
