#!/usr/bin/env bash

go () { echo -n "> $1 "; pushd $1; stylish-haskell -r -v -i .; popd; }

go blockfrost-freer-client
go chain-watcher
