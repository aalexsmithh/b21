#!/bin/bash

set -e

source .remote-deploy.env

ssh -T -p $REMOTE_DEPLOY_PORT $REMOTE_DEPLOY_USER@$REMOTE_DEPLOY_HOST <<EOF
set -e
cd website
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
# echo '>>> RESTARTING REDIRECT MANAGER'
# sudo systemctl restart b21-redirect-manager
# sudo systemctl status b21-redirect-manager
EOF

echo '>>> DEPLOY COMPLETE'
