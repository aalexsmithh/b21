#!/bin/bash

set -e

rsync -Pr frontend/ /srv/http/b21/
