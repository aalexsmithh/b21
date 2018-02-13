#!/bin/bash

set -e

ssh hakyll@building21.ca <<EOF
set -e
cd b21
echo '>>> UPDATING SOURCES'
git fetch
git reset --hard origin/master
echo '>>> BUILDING BACKEND'
(cd backend ; stack install)
echo '>>> BUILDING FRONTEND'
./build-frontend.sh
echo '>>> DEPLOYING FRONTEND'
./deploy.sh
echo '>>> RESTARTING BACKEND'
sudo systemctl restart b21-backend
sudo systemctl status b21-backend
EOF

echo '>>> DEPLOY COMPLETE'
