#!/bin/bash

set -e

rsync -Pr frontend/ /srv/http/b21/
mkdir -p ~/opt
cp -vf backend/timesheet/casual_timesheet.png ~/opt
