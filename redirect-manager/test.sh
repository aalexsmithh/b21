#!/bin/bash

set -e

# Uses curl to send an update request to a test server that's already running.

if test -z "$B21_REDIRECT_SERVER" ; then
    B21_REDIRECT_SERVER="http://localhost:8085"
fi

curl -v -X POST "$B21_REDIRECT_SERVER/update"
