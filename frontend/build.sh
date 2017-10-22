#!/bin/bash

set -e

yarn install
./compile-js-deps.sh
stack build
stack exec b21-frontend build || stack exec b21-frontend rebuild
