#!/bin/bash

set -e

./build.sh

(cd _site && python3 -m http.server 8888)
