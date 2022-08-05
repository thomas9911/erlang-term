#! /bin/bash

cargo build --release --example parse
pushd integration_test
mix local.hex --if-missing
mix deps.get
mix test
popd
