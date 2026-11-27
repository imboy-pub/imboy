#!/bin/sh
# Prepare enabled templates before the official Nginx entrypoint renders them.
set -eu
export NGINX_ENVSUBST_TEMPLATE_DIR=/tmp/imboy-nginx-templates
mkdir -p "$NGINX_ENVSUBST_TEMPLATE_DIR"
cp /etc/nginx/templates/*.template "$NGINX_ENVSUBST_TEMPLATE_DIR/"
case "${UPTRACE_ENABLED:-false}" in
  true|1) cp /etc/imboy/uptrace.conf.template "$NGINX_ENVSUBST_TEMPLATE_DIR/" ;;
esac
if [ "${1:-}" = proxy-protocol ]; then
  sed -i 's/listen 443 ssl;/listen 443 ssl proxy_protocol;/g' "$NGINX_ENVSUBST_TEMPLATE_DIR"/*.template
fi
while :; do sleep 6h & wait "$!"; nginx -s reload; done &
exec /docker-entrypoint.sh nginx -g 'daemon off;'
