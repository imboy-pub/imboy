#!/usr/bin/env bash
# Disposable Garage only: no shared bucket, config, credentials or ports.
set -euo pipefail
cd "$(dirname "$0")/../.."
RUN_DIR=$(mktemp -d /tmp/imboy-asset-garage.XXXXXX)
CONTAINER="imboy-asset-garage-${RUN_DIR##*.}"
IMAGE=${IMBOY_TEST_GARAGE_IMAGE:-dxflrs/garage:v2.3.0}
cleanup() { docker rm -f -v "$CONTAINER" >/dev/null 2>&1 || true; }
trap cleanup EXIT
umask 077
mkdir "$RUN_DIR/beams"
RPC_SECRET=$(openssl rand -hex 32)
export IMBOY_TEST_ACCESS="GK$(openssl rand -hex 12)"
export IMBOY_TEST_SECRET="$(openssl rand -hex 32)"
cat > "$RUN_DIR/garage.toml" <<CONFIG
metadata_dir = "/tmp/meta"
data_dir = "/tmp/data"
db_engine = "lmdb"
replication_factor = 1
rpc_bind_addr = "0.0.0.0:3901"
rpc_secret = "$RPC_SECRET"
[s3_api]
s3_region = "garage"
api_bind_addr = "0.0.0.0:3900"
CONFIG
docker image inspect "$IMAGE" --format '{{.Id}}' > "$RUN_DIR/image-id.txt"
docker run -d --name "$CONTAINER" -p 127.0.0.1::3900 \
  -v "$RUN_DIR/garage.toml:/etc/garage.toml:ro" \
  --entrypoint /garage "$IMAGE" server > "$RUN_DIR/container-id.txt"
garage() { docker exec "$CONTAINER" /garage "$@"; }
ready=0
for ((attempt=0; attempt<30; attempt++)); do
  if garage node id > "$RUN_DIR/node-id.txt" 2>/dev/null; then ready=1; break; fi
  sleep 1
done
[[ "$ready" == 1 ]] || { echo 'Garage startup failed' >&2; exit 1; }
read -r NODE_ID _ < "$RUN_DIR/node-id.txt"
garage layout assign -z test -c 1G "$NODE_ID" > "$RUN_DIR/layout.log" 2>&1
garage layout apply --version 1 >> "$RUN_DIR/layout.log" 2>&1
garage bucket create synthetic-enterprise-assets > "$RUN_DIR/bucket.log" 2>&1
garage key import --yes "$IMBOY_TEST_ACCESS" "$IMBOY_TEST_SECRET" > "$RUN_DIR/key-private.log" 2>&1
garage bucket allow synthetic-enterprise-assets --read --write --key "$IMBOY_TEST_ACCESS" > "$RUN_DIR/grant.log" 2>&1
export IMBOY_TEST_ENDPOINT="http://$(docker port "$CONTAINER" 3900/tcp)"
erlc -I include -o "$RUN_DIR/beams" src/lib/elib_s3_sign.erl src/lib/elib_oss.erl \
  src/features/enterprise_business/infrastructure/eb_asset_object_garage.erl
cat > "$RUN_DIR/check.erl" <<'CHECK'
-module(check).
-export([run/0, verify/0]).
setup() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    Config = #{endpoint => list_to_binary(os:getenv("IMBOY_TEST_ENDPOINT")),
        bucket => <<"synthetic-enterprise-assets">>, region => <<"garage">>,
        access_key => list_to_binary(os:getenv("IMBOY_TEST_ACCESS")),
        secret_key => list_to_binary(os:getenv("IMBOY_TEST_SECRET"))},
    application:set_env(imboy, garage, Config),
    Key = <<"enterprise/101/202/synthetic.txt">>,
    Prefix = <<"enterprise/101/202/">>,
    Bytes = <<"synthetic private enterprise attachment", 0, 255, 10>>,
    {Key, Prefix, Bytes, Config}.
run() ->
    {Key, _Prefix, Bytes, _Config} = setup(),
    ok = eb_asset_object_garage:put(Key, Bytes, #{mime => <<"application/octet-stream">>}),
    halt(0).
verify() ->
    {Key, Prefix, Bytes, Config} = setup(),
    {ok, #{bytes := Bytes, size := Size}} = eb_asset_object_garage:get(Key, Prefix),
    Size = byte_size(Bytes),
    AnonymousUrl = binary_to_list(<<(maps:get(endpoint, Config))/binary,
        "/synthetic-enterprise-assets/", Key/binary>>),
    {ok, {{_, 403, _}, _, _}} = httpc:request(get, {AnonymousUrl, []},
        [{timeout, 10000}, {autoredirect, false}], []),
    {error, out_of_scope} = eb_asset_object_garage:get(Key, <<"enterprise/999/202/">>),
    {error, out_of_scope} = eb_asset_object_garage:delete(Key, <<"enterprise/999/202/">>),
    application:set_env(imboy, garage, Config#{secret_key => <<"wrong-synthetic-key">>}),
    {error, {http_status, 403}} = eb_asset_object_garage:get(Key, Prefix),
    application:set_env(imboy, garage, Config),
    ok = eb_asset_object_garage:delete(Key, Prefix),
    {error, not_found} = eb_asset_object_garage:get(Key, Prefix),
    application:unset_env(imboy, garage),
    {error, storage_not_configured} = eb_asset_object_garage:put(Key, Bytes, #{}),
    io:format("PASS: real Garage bytes survive server and client restart; anonymous denial, tenant prefix, invalid signature, delete and missing config~n"),
    halt(0).
CHECK
erlc -o "$RUN_DIR/beams" "$RUN_DIR/check.erl"
ERL_CRASH_DUMP="$RUN_DIR/erl_crash.dump" erl -noshell -pa "$RUN_DIR/beams" -s check run > "$RUN_DIR/result.log" 2>&1
docker restart "$CONTAINER" > "$RUN_DIR/restart.log"
export IMBOY_TEST_ENDPOINT="http://$(docker port "$CONTAINER" 3900/tcp)"
ready=0
for ((attempt=0; attempt<30; attempt++)); do
  if [[ "$(curl -s -o /dev/null -w '%{http_code}' "$IMBOY_TEST_ENDPOINT/" || true)" == 403 ]]; then ready=1; break; fi
  sleep 1
done
[[ "$ready" == 1 ]] || { echo 'Garage restart failed' >&2; exit 1; }
ERL_CRASH_DUMP="$RUN_DIR/erl_crash.dump" erl -noshell -pa "$RUN_DIR/beams" -s check verify >> "$RUN_DIR/result.log" 2>&1
cat "$RUN_DIR/result.log"
printf 'Evidence: %s\n' "$RUN_DIR"
