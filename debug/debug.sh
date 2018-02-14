#!/bin/bash

# builds the frontend
# builds the backend and runs it in the background
# builds the debug server and runs it

set -e

source debug.env

mkdir -p ../frontend/static

(cd .. && ./build-frontend.sh)
(cd ../backend && source test.env && exec ./debug.sh "$@") &

echo "starting debug server on port $B21_DEBUG_PORT ..."

exec stack exec b21-debug-exe
