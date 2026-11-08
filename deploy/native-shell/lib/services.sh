#!/usr/bin/env bash
# Native services and one-host initialization. Loaded by trusted package scripts.

prepare_accounts() {
    local u imboy_user_home
    for u in imboy imboy-pg imboy-garage; do
        case $u in imboy) imboy_user_home=$DATA_DIR/backend;; imboy-pg) imboy_user_home=$DATA_DIR/postgres;; *) imboy_user_home=$DATA_DIR/garage;; esac
        if ! id "$u" >/dev/null 2>&1; then useradd --system --user-group --home-dir "$imboy_user_home" --shell /usr/sbin/nologin "$u"; fi
    done
    install -d -m 755 "$INSTALL_DIR" "$INSTALL_DIR/releases" "$INSTALL_DIR/runtime" "$DATA_DIR" "$CONF_DIR"
    install -d -m 700 -o imboy -g imboy "$DATA_DIR/backend"
    install -d -m 700 -o imboy -g imboy "$DATA_DIR/backend/log" "$DATA_DIR/backend/log/sasl" "$DATA_DIR/backend/mnesia"
    install -d -m 700 -o imboy-pg -g imboy-pg "$DATA_DIR/postgres"
    install -d -m 700 -o imboy-garage -g imboy-garage "$DATA_DIR/garage" "$DATA_DIR/garage/meta" "$DATA_DIR/garage/data"
    install -d -m 750 -o root -g imboy "$CONF_DIR/keys"
    install -d -m 700 "$CONF_DIR/tls" "$BACKUP_DIR"
}

stage_payload() {
    local package=$1 release=$INSTALL_DIR/releases/${META[RELEASE_ID]} temp
    if [[ ! -d $PG_ROOT ]]; then
        temp=$(mktemp -d "$INSTALL_DIR/runtime/.pg-stage.XXXXXX")
        cp -a -- "$package/payload/postgresql/." "$temp/"
        mv -T -- "$temp" "$PG_ROOT"
    fi
    if [[ ! -f $GARAGE_BIN ]]; then install -m 755 "$package/payload/garage/garage" "$GARAGE_BIN"; fi
    if [[ -e $release ]]; then
        [[ -f $release/source.sha && $(cat "$release/source.sha") == "${META[SOURCE_SHA]}" ]] || die 4 RELEASE 'Existing release identity does not match.'
    else
        temp=$(mktemp -d "$INSTALL_DIR/releases/.stage.XXXXXX")
        mkdir "$temp/backend"
        cp -a -- "$package/payload/backend/." "$temp/backend/"
        cp -- "$package/metadata/release.env" "$temp/release.env"
        printf '%s\n' "${META[SOURCE_SHA]}" > "$temp/source.sha"
        chmod 755 "$temp"
        mv -T -- "$temp" "$release"
    fi
    chown -R root:root "$release" "$PG_ROOT" "$GARAGE_BIN"
    # Payload is explicitly secret-free. Runtime secrets/data have separate modes.
    chmod -R a+rX,u+w,go-w "$release" "$PG_ROOT"
    runuser -u imboy -- test -x "$release/backend/bin/imboy"
    runuser -u imboy-pg -- test -x "$PG_ROOT/bin/postgres"
    ln -s "$release/backend" "$INSTALL_DIR/.current-$$"
    mv -Tf "$INSTALL_DIR/.current-$$" "$INSTALL_DIR/current"
    "$PG_ROOT/bin/postgres" --version | grep -q " ${META[PG_VERSION]}" || die 3 PG_VERSION 'PostgreSQL version differs from manifest.'
    "$GARAGE_BIN" --version | grep -q '2.4.1' || die 3 GARAGE_VERSION 'Garage version differs from contract.'
}

generate_rsa() {
    local private=$CONF_DIR/keys/login_rsa_priv.pem public=$CONF_DIR/keys/login_rsa_pub.pem temp
    if [[ -s $private && -s $public ]]; then return; fi
    [[ ! -e $private && ! -e $public ]] || die 4 RSA 'Partial RSA keypair exists. Recover it; do not rotate keys during resume.'
    [[ ! -e $CONF_DIR/rsa.initialized && ! -f $DATA_DIR/postgres/PG_VERSION ]] || die 4 RSA_LOST 'RSA keys missing from an initialized instance. Restore the original keypair.'
    temp=$(mktemp -d "$CONF_DIR/keys/.new.XXXXXX")
    openssl genrsa -traditional -out "$temp/private.pem" 3072 >/dev/null 2>&1
    openssl rsa -in "$temp/private.pem" -pubout -out "$temp/public.pem" >/dev/null 2>&1
    install -m 640 -o root -g imboy "$temp/private.pem" "$private"
    install -m 644 -o root -g imboy "$temp/public.pem" "$public"
    touch "$CONF_DIR/rsa.initialized"
    rm -f "$temp/private.pem" "$temp/public.pem"; rmdir "$temp"
}

copy_tls() {
    local role destination=${1:-$CONF_DIR/tls}
    for role in api s3 files; do
        install -d -m 700 "$destination/$role"
        install -m 644 "$TLS_DIR/$role/fullchain.pem" "$destination/$role/fullchain.pem"
        install -m 600 "$TLS_DIR/$role/privkey.pem" "$destination/$role/privkey.pem"
    done
}

write_garage_config() {
    cat > "$CONF_DIR/garage.toml" <<EOF
metadata_dir = "$DATA_DIR/garage/meta"
data_dir = "$DATA_DIR/garage/data"
db_engine = "lmdb"
replication_factor = 1
rpc_bind_addr = "127.0.0.1:3901"
rpc_public_addr = "127.0.0.1:3901"
rpc_secret = "$GARAGE_RPC_SECRET"
[s3_api]
s3_region = "$GARAGE_REGION"
api_bind_addr = "127.0.0.1:3900"
[s3_web]
bind_addr = "127.0.0.1:3902"
root_domain = ".garage.localhost"
index = "index.html"
[admin]
api_bind_addr = "127.0.0.1:3903"
admin_token = "$GARAGE_ADMIN_TOKEN"
EOF
    chown root:imboy-garage "$CONF_DIR/garage.toml"; chmod 640 "$CONF_DIR/garage.toml"
}

write_units() {
    local unit_dir=${1:-/etc/systemd/system}
    cat > "$unit_dir/imboy-postgresql.service" <<EOF
[Unit]
Description=IMBoy dedicated PostgreSQL 18
After=network.target
[Service]
Type=simple
User=imboy-pg
Group=imboy-pg
RuntimeDirectory=imboy-pg
RuntimeDirectoryMode=0700
ExecStart=$PG_ROOT/bin/postgres -D $DATA_DIR/postgres
ExecReload=/bin/kill -HUP \$MAINPID
KillSignal=SIGINT
TimeoutStopSec=180
Restart=on-failure
RestartSec=5
UMask=0077
NoNewPrivileges=true
PrivateTmp=true
ProtectHome=true
ProtectSystem=strict
ReadWritePaths=$DATA_DIR/postgres /run/imboy-pg
[Install]
WantedBy=multi-user.target
EOF
    cat > "$unit_dir/imboy-garage.service" <<EOF
[Unit]
Description=IMBoy single-node Garage
After=network.target
[Service]
Type=simple
User=imboy-garage
Group=imboy-garage
ExecStart=$GARAGE_BIN -c $CONF_DIR/garage.toml server
Restart=on-failure
RestartSec=5
TimeoutStopSec=180
UMask=0077
NoNewPrivileges=true
PrivateTmp=true
ProtectHome=true
ProtectSystem=strict
ReadWritePaths=$DATA_DIR/garage
[Install]
WantedBy=multi-user.target
EOF
    cat > "$unit_dir/imboy-backend.service" <<EOF
[Unit]
Description=IMBoy backend (native relx)
Requires=imboy-postgresql.service imboy-garage.service
After=network.target imboy-postgresql.service imboy-garage.service
StartLimitIntervalSec=300
StartLimitBurst=3
[Service]
Type=simple
User=imboy
Group=imboy
WorkingDirectory=$INSTALL_DIR/current
EnvironmentFile=$CONF_DIR/backend.env
Environment=RELX_CONFIG_PATH=$CONF_DIR/sys.config
Environment=VMARGS_PATH=$CONF_DIR/vm.args
Environment=RELX_REPLACE_OS_VARS=
Environment=ERL_EPMD_ADDRESS=127.0.0.1
Environment=ERL_CRASH_DUMP=$DATA_DIR/backend/erl_crash.dump
Environment=RUNNER_LOG_DIR=$DATA_DIR/backend/log
ExecStart=$INSTALL_DIR/current/bin/imboy foreground
Restart=on-failure
RestartSec=10
TimeoutStopSec=180
LimitNOFILE=65536
UMask=0077
NoNewPrivileges=true
PrivateTmp=true
ProtectHome=true
ProtectSystem=strict
ReadWritePaths=$DATA_DIR/backend
[Install]
WantedBy=multi-user.target
EOF
    chmod 644 "$unit_dir"/imboy-{postgresql,garage,backend}.service
    systemctl daemon-reload
}

init_postgres() {
    local exists extension
    if [[ ! -f $DATA_DIR/postgres/PG_VERSION ]]; then
        [[ -z $(find "$DATA_DIR/postgres" -mindepth 1 -print -quit) ]] || die 4 PG_PARTIAL 'PostgreSQL data directory is nonempty without PG_VERSION; reconcile partial init, do not delete blindly.'
        runuser -u imboy-pg -- "$PG_ROOT/bin/initdb" -D "$DATA_DIR/postgres" --username=postgres --encoding=UTF8 --locale=C.UTF-8 --auth-local=peer --auth-host=scram-sha-256 >/dev/null
    fi
    [[ $(cat "$DATA_DIR/postgres/PG_VERSION") == 18 ]] || die 4 PG_VERSION 'Only PostgreSQL 18 data directories are supported.'
    cat > "$DATA_DIR/postgres/imboy.conf" <<EOF
listen_addresses = '127.0.0.1'
port = $PG_PORT
unix_socket_directories = '/run/imboy-pg'
unix_socket_permissions = 0700
password_encryption = 'scram-sha-256'
shared_preload_libraries = 'pg_jieba,timescaledb,pg_stat_statements'
timescaledb.telemetry_level = 'off'
timezone = 'UTC'
log_timezone = 'UTC'
max_connections = 160
fsync = on
synchronous_commit = on
log_statement = 'none'
log_min_error_statement = 'panic'
EOF
    grep -qxF "include = 'imboy.conf'" "$DATA_DIR/postgres/postgresql.conf" || printf "\ninclude = 'imboy.conf'\n" >> "$DATA_DIR/postgres/postgresql.conf"
    cat > "$DATA_DIR/postgres/pg_hba.conf" <<EOF
local all postgres peer map=imboy_local
host $PG_DATABASE $PG_USER 127.0.0.1/32 scram-sha-256
host all all 0.0.0.0/0 reject
host all all ::/0 reject
EOF
    printf 'imboy_local imboy-pg postgres\n' > "$DATA_DIR/postgres/pg_ident.conf"
    chown imboy-pg:imboy-pg "$DATA_DIR/postgres/"*.conf
    chmod 600 "$DATA_DIR/postgres/"*.conf
    systemctl enable --now imboy-postgresql.service >/dev/null
    wait_postgres || die 5 PG_START 'PostgreSQL did not become ready.'
    exists=$(psql_admin postgres -Atqc "SELECT count(*) FROM pg_roles WHERE rolname='imboy'")
    if [[ $exists == 0 ]]; then
        # Secret is sent only over stdin, never the process argument list or logs.
        printf "CREATE ROLE imboy LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE PASSWORD '%s';\n" "$PG_PASSWORD" | psql_admin postgres -q >/dev/null
    fi
    exists=$(psql_admin postgres -Atqc "SELECT count(*) FROM pg_database WHERE datname='imboy'")
    if [[ $exists == 0 ]]; then psql_admin postgres -qc "CREATE DATABASE imboy OWNER imboy TEMPLATE template0 ENCODING 'UTF8' LC_COLLATE 'C.UTF-8' LC_CTYPE 'C.UTF-8'" >/dev/null; fi
    for extension in pgcrypto pg_jieba timescaledb vector postgis pg_trgm btree_gin btree_gist citext unaccent intarray pg_stat_statements; do
        psql_admin imboy -qc "CREATE EXTENSION IF NOT EXISTS $extension" >/dev/null
    done
}

init_garage() {
    local n ready=no node version text access secret bucket tmp key
    systemctl enable --now imboy-garage.service >/dev/null
    wait_garage || die 5 GARAGE_START 'Garage did not become ready.'
    node=$(garage_cmd node id | head -1 | cut -d@ -f1)
    [[ $node =~ ^[0-9a-f]{64}$ ]] || die 5 GARAGE_NODE 'Unrecognized Garage node ID format.'
    text=$(garage_cmd layout show)
    version=$(awk '/[Cc]urrent cluster layout version/ {print $NF}' <<< "$text")
    [[ $version =~ ^[0-9]+$ ]] || die 5 GARAGE_LAYOUT 'Unrecognized Garage layout version; inspect locally.'
    if [[ $version == 0 ]]; then
        garage_cmd layout assign -z local -c "$GARAGE_CAPACITY" "$node" >/dev/null
        garage_cmd layout apply --version 1 >/dev/null
    elif [[ $version != 1 ]]; then die 4 GARAGE_LAYOUT 'Unexpected layout version; do not reinitialize an existing cluster.'; fi
    garage_cmd bucket list >/dev/null
    for bucket in "$PRIVATE_BUCKET" "$PUBLIC_BUCKET"; do
        if ! garage_cmd bucket info "$bucket" >/dev/null 2>&1; then garage_cmd bucket create "$bucket" >/dev/null; fi
    done
    if ! text=$(garage_cmd key info imboy-backend --show-secret 2>/dev/null); then
        garage_cmd key list >/dev/null
        text=$(garage_cmd key create imboy-backend)
    fi
    access=$(awk 'tolower($0) ~ /^key id:/ {print $NF}' <<< "$text")
    secret=$(awk 'tolower($0) ~ /^secret key:/ {print $NF}' <<< "$text")
    [[ $access =~ ^GK([0-9a-f]{24}|[0-9a-f]{32})$ && $secret =~ ^[0-9a-f]{64}$ ]] || die 5 GARAGE_KEY 'Unrecognized Garage credential format; no secret was printed.'
    SECRET[GARAGE_ACCESS_KEY]=$access; SECRET[GARAGE_SECRET_KEY]=$secret
    tmp=$(mktemp "$CONF_DIR/.secrets.XXXXXX")
    for key in "${!SECRET[@]}"; do printf '%s=%s\n' "$key" "${SECRET[$key]}" >> "$tmp"; done
    chmod 600 "$tmp"; mv -T "$tmp" "$CONF_DIR/secrets.env"
    load_secrets
    for bucket in "$PRIVATE_BUCKET" "$PUBLIC_BUCKET"; do garage_cmd bucket allow "$bucket" --read --write --owner --key "$GARAGE_ACCESS_KEY" >/dev/null; done
    garage_cmd bucket website --deny "$PRIVATE_BUCKET" >/dev/null
    garage_cmd bucket website --allow "$PUBLIC_BUCKET" >/dev/null
}

s3_request() {
    local method=$1 path=$2 body=${3:-} output=${4:-/dev/null} endpoint=${5:-http://127.0.0.1:3900} cfg hash
    cfg=$(mktemp "$STATE_DIR/.curl.XXXXXX")
    printf 'user = "%s:%s"\naws-sigv4 = "aws:amz:%s:s3"\n' "$GARAGE_ACCESS_KEY" "$GARAGE_SECRET_KEY" "$GARAGE_REGION" > "$cfg"
    local -a args=(--noproxy '*' --config "$cfg" --fail --silent --show-error --connect-timeout 5 --max-time 60 --request "$method" --output "$output")
    if [[ $endpoint == "https://$S3_DOMAIN" ]]; then args+=(--resolve "$S3_DOMAIN:443:127.0.0.1"); fi
    if [[ -n $body ]]; then
        hash=$(sha "$body"); args+=(--data-binary "@$body" --header "x-amz-content-sha256: $hash")
        if [[ $path == *'?cors' ]]; then args+=(--header 'Content-Type: application/xml' --header "Content-MD5: $(openssl dgst -md5 -binary "$body" | openssl base64 -A)"); fi
    fi
    local rc=0
    curl "${args[@]}" "$endpoint/$path" || rc=$?
    rm -f "$cfg"
    return "$rc"
}

configure_s3_cors() {
    local xml origin bucket
    [[ -n $CORS_ORIGINS ]] || return 0
    xml=$(mktemp "$STATE_DIR/.cors.XXXXXX")
    {
        printf '<CORSConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"><CORSRule>'
        local -a origins; IFS=, read -r -a origins <<< "$CORS_ORIGINS"
        for origin in "${origins[@]}"; do printf '<AllowedOrigin>%s</AllowedOrigin>' "$origin"; done
        printf '<AllowedMethod>GET</AllowedMethod><AllowedMethod>HEAD</AllowedMethod><AllowedMethod>PUT</AllowedMethod><AllowedMethod>POST</AllowedMethod><AllowedMethod>DELETE</AllowedMethod><AllowedHeader>*</AllowedHeader><ExposeHeader>ETag</ExposeHeader><ExposeHeader>x-amz-checksum-sha256</ExposeHeader><MaxAgeSeconds>600</MaxAgeSeconds></CORSRule></CORSConfiguration>\n'
    } > "$xml"
    for bucket in "$PRIVATE_BUCKET" "$PUBLIC_BUCKET"; do s3_request PUT "$bucket?cors" "$xml"; done
    rm -f "$xml"
}

write_nginx() {
    local target=${1:-/etc/nginx/conf.d/imboy-native.conf}
    cat > "$target" <<EOF
# IMBoy native installer managed. Signed S3 URI/Host must remain unchanged.
# Do not log query strings: they can contain presigned credentials.
log_format imboy_native '\$remote_addr [\$time_local] \$request_method \$uri \$status';
map \$http_upgrade \$imboy_connection { default upgrade; '' close; }
server {
    listen 80;
    server_name $API_DOMAIN $S3_DOMAIN $FILES_DOMAIN;
    return 301 https://\$host\$request_uri;
}
server {
    listen 443 ssl;
    server_name $API_DOMAIN;
    ssl_certificate $CONF_DIR/tls/api/fullchain.pem;
    ssl_certificate_key $CONF_DIR/tls/api/privkey.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    access_log /var/log/nginx/imboy-api.access.log imboy_native;
    error_log /var/log/nginx/imboy-api.error.log warn;
    client_max_body_size 500m;
    location = /metrics { return 403; }
    location ^~ /api/adm/setup/ { return 403; }
    location / {
        proxy_pass http://127.0.0.1:$HTTP_PORT;
        proxy_http_version 1.1;
        proxy_set_header Host \$http_host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$remote_addr;
        proxy_set_header X-Forwarded-Proto https;
        proxy_set_header Upgrade \$http_upgrade;
        proxy_set_header Connection \$imboy_connection;
        proxy_read_timeout 3600s;
        proxy_send_timeout 300s;
    }
}
server {
    listen 443 ssl;
    server_name $S3_DOMAIN;
    ssl_certificate $CONF_DIR/tls/s3/fullchain.pem;
    ssl_certificate_key $CONF_DIR/tls/s3/privkey.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    access_log /var/log/nginx/imboy-s3.access.log imboy_native;
    error_log /var/log/nginx/imboy-s3.error.log warn;
    client_max_body_size 500m;
    location / {
        proxy_pass http://127.0.0.1:3900;
        proxy_set_header Host \$http_host;
        proxy_set_header X-Forwarded-Proto https;
        proxy_http_version 1.1;
        proxy_request_buffering off;
        proxy_buffering off;
        proxy_read_timeout 300s;
        proxy_send_timeout 300s;
    }
}
server {
    listen 443 ssl;
    server_name $FILES_DOMAIN;
    ssl_certificate $CONF_DIR/tls/files/fullchain.pem;
    ssl_certificate_key $CONF_DIR/tls/files/privkey.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    access_log /var/log/nginx/imboy-files.access.log imboy_native;
    error_log /var/log/nginx/imboy-files.error.log warn;
    location / {
        limit_except GET HEAD { deny all; }
        proxy_pass http://127.0.0.1:3902;
        proxy_set_header Host $PUBLIC_BUCKET.garage.localhost;
        proxy_set_header X-Forwarded-Proto https;
    }
}
EOF
    chmod 644 "$target"
    nginx -t
}

start_backend_migrate() {
    wait_postgres || die 5 PG_READY 'Database readiness is required before migrations.'
    wait_garage || die 5 GARAGE_READY 'Storage readiness is required before backend startup.'
    psql_admin postgres -qc 'ALTER ROLE imboy SUPERUSER' >/dev/null
    MIGRATION_PRIVILEGE=yes
    systemctl reset-failed imboy-backend.service >/dev/null 2>&1 || true
    systemctl enable --now imboy-backend.service >/dev/null
    wait_health
    psql_admin postgres -qc 'ALTER ROLE imboy NOSUPERUSER NOCREATEDB NOCREATEROLE' >/dev/null
    MIGRATION_PRIVILEGE=no
}
