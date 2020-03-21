# Compiles CSS dependencies.
# It's very simple: we just concatenate all the files

set -e

mkdir -p frontend/css

# we do this crazy xargs stuff (instead of `cat css/*`) to make sure we get the
# correct order of files, since declaration order sometimes disambiguates rule
# precedence in CSS.
xargs cat > frontend/css/dependencies.css <<'EOF'
css/skeleton.css
css/normalize.css
css/custom.css
EOF

