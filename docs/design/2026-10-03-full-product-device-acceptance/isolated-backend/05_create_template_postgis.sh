#!/bin/bash
# Init hook (run 05, before 10_init.sh): the image's 10_init.sh expects an
# existing template_postgis database but never creates it. Create it from
# template0 so the stock postgis init/update path completes unchanged.
set -euo pipefail
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname postgres \
  -c 'CREATE DATABASE template_postgis IS_TEMPLATE true;'
