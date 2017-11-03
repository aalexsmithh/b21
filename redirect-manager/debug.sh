#!/bin/bash

set -e

mkdir -p test

export B21_REDIRECTS_PORT=8085
export B21_REDIRECTS_SRC="test/example.redirects"
export B21_REDIRECTS_DST="test/example.conf"

exec stack exec b21-redirect-manager-exe
