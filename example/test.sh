#!/bin/bash
set -e

cd "$(dirname "$0")/.."

cabal run catbox -- example/my_site.toml \
  --input example/input/ \
  --output example/output/
