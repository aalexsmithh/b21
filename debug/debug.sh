#!/bin/bash

# builds the frontend
# builds the backend and runs it in the background
# builds the debug server and runs it

set -e

export B21_DEBUG_PORT=8080
export B21_API_PORT=8082
export B21_FRONTEND_PATH="../frontend/_site"

(cd ../frontend ; ./build.sh "$@")
(cd ../backend ; exec ./debug.sh "$@") &

echo "starting debug server on port $B21_DEBUG_PORT ..."

exec stack exec b21-debug-exe
