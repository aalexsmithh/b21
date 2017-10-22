#!/bin/bash

# concatenates all javascript dependencies into one file

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
xargs cat > js/dependencies.js
