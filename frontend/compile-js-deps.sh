#!/bin/bash

# concatenates all javascript dependencies followed by our custom javascript
# into one file

set -e

mkdir -p js

prefix=node_modules

while read dep ; do
    echo "$prefix/$dep/$(jq -r .main "$prefix/$dep/package.json")"
done <<'EOF' |
jquery
underscore
moment
clndr
EOF
xargs -I {} cat {} js/custom.js > js/dependencies.js
