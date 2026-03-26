#!/usr/bin/env bash
set -euo pipefail

cargo run -- test --import $(find -L fumola -name "*.fumola" | sort)
