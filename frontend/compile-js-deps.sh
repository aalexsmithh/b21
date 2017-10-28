#!/bin/bash

# Compiles JS and CSS dependencies.
# For JS this is somewhat tricky because they're inside node_modules.
# Each module has a JSON file that says what the main file of the module is.
# We loop over our dependencies, extract this string from the JSON file, and
# concatenate all the files identified by this procedure.
# For CSS it's very simple: we just concatenate all the files

# concatenates all javascript dependencies followed by our custom javascript
# into one file

set -e

mkdir -p dist/{js,css}

prefix=node_modules

while read dep ; do
    echo "$prefix/$dep/$(jq -r .main "$prefix/$dep/package.json")"
done <<'EOF' |
jquery
moment
underscore
clndr
EOF
xargs -I {} cat {} js/custom.js > dist/js/dependencies.js

# we do this crazy xargs stuff (instead of `cat css/*`) to make sure we get the
# correct order of files, since declaration order sometimes disambiguates rule
# precedence in CSS.
xargs cat > dist/css/dependencies.css <<'EOF'
css/skeleton.css
css/normalize.css
css/custom.css
EOF
