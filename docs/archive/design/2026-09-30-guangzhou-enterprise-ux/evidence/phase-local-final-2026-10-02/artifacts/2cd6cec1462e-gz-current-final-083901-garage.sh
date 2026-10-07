#!/usr/bin/env bash
# Disposable Garage only: no shared bucket, config, credentials or ports.
set -euo pipefail
cd "/Users/leeyi/project/imboy.pub/.Codex/worktrees/gz-enterprise-webhook-transport"
RUN_DIR=$(mktemp -d /tmp/imboy-asset-garage.XXXXXX)
CONTAINER="imboy-asset-garage-${RUN_DIR##*.}"
IMAGE=${IMBOY_TEST_GARAGE_IMAGE:-dxflrs/garage:v2.3.0}
cleanup() { docker rm -f -v "$CONTAINER" >/dev/null 2>&1 || true; }
trap cleanup EXIT
trap 'exit 143' TERM
trap 'exit 130' INT
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
docker run -d --label "imboy.gate.owner=${IMBOY_GATE_OWNER:?}" --name "$CONTAINER" -p 127.0.0.1::3900 \
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
export IMBOY_CS_BROWSER_ATTACHMENTS=1
unset IMBOY_CS_BROWSER_RUNNER IMBOY_ASSET_GARAGE_PG_CHECK IMBOY_ADMIN_GOVERNANCE_PG_CHECK
export IMBOY_DEPS_ROOT=/var/folders/8m/wbjj0qmn4ml0mgn56wm56j2m0000gn/T/gz-frozen-native-deps-fe0qq17k
bash /tmp/gz-current-final-083901-backend.sh
printf "Garage evidence: %s\n" "$RUN_DIR"
