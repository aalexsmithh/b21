#!/bin/bash

OFFLINE=0

while (( $# )) ; do
    case "$1" in
        "--offline")
            OFFLINE=1
            ;;
    esac
    shift
done

set -e

if [ $OFFLINE -ne 1 ] ; then
    yarn install
fi
./compile-js-deps.sh
stack build
stack exec b21-frontend build || stack exec b21-frontend rebuild
