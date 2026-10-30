#!/usr/bin/env bash
# Build-time check of the final image: packages, control/SQL and loaded library.
set -euo pipefail
pg_major=${1:?PostgreSQL major required}
package_version=${2:?PostGIS Debian package version required}
sql_version=${package_version%%+*}
package="postgresql-${pg_major}-postgis-3"
for pkg in "$package" "${package}-scripts"; do
  actual=$(dpkg-query -W -f='${Version}' "$pkg")
  echo "$pkg: $actual (expected $package_version)"
  test "$actual" = "$package_version"
done

extension_dir="/usr/share/postgresql/${pg_major}/extension"
for ext in postgis postgis_topology postgis_tiger_geocoder; do
  # -s follows Debian's update-alternatives symlinks and rejects dangling links.
  test -s "$extension_dir/$ext.control"
  test -s "$extension_dir/$ext--$sql_version.sql"
  cat "$extension_dir/$ext.control"
done

pg_bin="/usr/lib/postgresql/${pg_major}/bin"
tmp=$(mktemp -d)
cleanup() {
  gosu postgres "$pg_bin/pg_ctl" -D "$tmp/data" -m immediate -w stop >/dev/null 2>&1 || true
  rm -rf "$tmp"
}
trap cleanup EXIT
trap 'cat "$tmp/server.log" >&2 2>/dev/null || true' ERR
chown postgres:postgres "$tmp"
gosu postgres "$pg_bin/initdb" -D "$tmp/data" -A trust --no-locale >/dev/null
gosu postgres "$pg_bin/pg_ctl" -D "$tmp/data" -l "$tmp/server.log" \
  -o "-c listen_addresses='' -c unix_socket_directories='$tmp'" -w start
psql_check() {
  gosu postgres "$pg_bin/psql" -X -h "$tmp" -d postgres -v ON_ERROR_STOP=1 -At "$@"
}
for ext in postgis postgis_topology postgis_tiger_geocoder; do
  test "$(psql_check -c "SELECT default_version FROM pg_available_extensions WHERE name='$ext'")" = "$sql_version"
  test "$(psql_check -c "SELECT count(*) FROM pg_available_extension_versions WHERE name='$ext' AND version='$sql_version'")" = 1
done
psql_check -c "CREATE EXTENSION fuzzystrmatch;"
for ext in postgis postgis_topology postgis_tiger_geocoder; do
  psql_check -c "CREATE EXTENSION $ext VERSION '$sql_version'; ALTER EXTENSION $ext UPDATE TO '$sql_version';"
  test "$(psql_check -c "SELECT extversion FROM pg_extension WHERE extname='$ext'")" = "$sql_version"
done
test "$(psql_check -c 'SELECT postgis_lib_version()')" = "$sql_version"
psql_check -c 'SELECT postgis_full_version();'
echo 'PostGIS package/control/SQL/library consistency: PASS'
