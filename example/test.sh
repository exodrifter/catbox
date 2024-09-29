#!/bin/bash
set -e

cd "$(dirname "$0")/.."

cabal run catbox -- example/example.toml \
  --input example/ \
  --output example/output/ \
  --file input.md
