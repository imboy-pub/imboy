#!/usr/bin/env bash
# Controlled community LOGIN bootstrap: no host psql/Python or password argv/SQL.
# 仅 --create 创建缺失 LOGIN；已有账号只验证，不改密码、权限或迁移。
set +x
set +v
set -euo pipefail
umask 077
cd "$(dirname -- "${BASH_SOURCE[0]}")"
fail() { printf '[ERROR] cleanup-db: %s\n' "$*" >&2; exit 1; }
create=false
case "${1:-}" in
  '') ;;
  --create) create=true; shift ;;
  -h|--help)
    printf '%s\n' 'Usage: bash init-cleanup-db.sh [--create]' \
      'Default: verify the configured LOGIN after migrations, without changing it.' \
      '--create: use the configured POSTGRES_USER to create a missing LOGIN only.' \
      'Requires CREATEROLE, ADMIN OPTION on v5_worker and CONNECT grant authority (or an existing DB administrator).' \
      'Only the bundled community database topology is supported. No automatic privilege elevation.'
    exit 0 ;;
  *) fail '仅支持 --create / --help；密码不得通过命令行传入' ;;
esac
[[ $# -eq 0 ]] || fail '多余参数'
for tool in docker jq openssl; do
  command -v "$tool" >/dev/null 2>&1 || fail "缺少工具: $tool"
done
for sql in cleanup-db/bootstrap.sql cleanup-db/verify.sql; do
  [[ -r "$sql" ]] || fail "交付文件缺失: $sql"
done
read -r -a files <<< "${COMPOSE_FILES:-docker-compose.community.yml}"
[[ ${files[0]:-} == docker-compose.community.yml ]] \
  || fail '自动步骤仅支持社区版；外部/商务数据库由 DBA 按手册初始化'
compose_args=()
for file in "${files[@]}"; do compose_args+=(-f "$file"); done
compose() { docker compose "${compose_args[@]}" "$@"; }

# Use Compose effective values, including overrides; never source/eval .env.
resolved=$(compose config --format json 2>/dev/null) || fail 'Compose 配置解析失败'
if ! jq -es '
  def text: type == "string" and length > 0 and (test("[\u0000\r\n]") | not);
  if length != 1 then false else
    .[0].services.imboy_backend.environment as $b |
    .[0].services.imboy_pg18.environment as $p |
    ($b.IMBOY_PG_HOST == "imboy_pg18") and ($b.IMBOY_PG_PORT == "5432")
    and all([$b.IMBOY_PG_USER, $b.IMBOY_PG_DB, $b.IMBOY_PG_PASSWORD,
             $b.IMBOY_DIRECT_CLEANUP_PG_USER, $b.IMBOY_DIRECT_CLEANUP_PG_PASSWORD][]; text)
    and ($b.IMBOY_PG_USER == $p.POSTGRES_USER)
    and ($b.IMBOY_PG_DB == $p.POSTGRES_DB)
    and ($b.IMBOY_PG_PASSWORD == $p.POSTGRES_PASSWORD)
    and ($b.IMBOY_DIRECT_CLEANUP_PG_USER | test("^[a-z_][a-z0-9_]{0,62}$"))
    and ($b.IMBOY_DIRECT_CLEANUP_PG_USER | startswith("pg_") | not)
    and ($b.IMBOY_DIRECT_CLEANUP_PG_USER != $b.IMBOY_PG_USER)
    and ($b.IMBOY_DIRECT_CLEANUP_PG_USER != "v5_worker")
    and ($b.IMBOY_DIRECT_CLEANUP_PG_PASSWORD != $b.IMBOY_PG_PASSWORD)
    and ($b.IMBOY_DIRECT_CLEANUP_PG_PASSWORD | length >= 32)
    and ($b.IMBOY_DIRECT_CLEANUP_PG_PASSWORD | contains("CHANGE_ME") | not)
  end
' >/dev/null 2>&1 <<< "$resolved"; then
  fail '配置不符合内置社区 PG 拓扑或独立清理账号规则；未执行 SQL'
fi
value() { jq -er ".services.imboy_backend.environment.$1" <<< "$resolved"; }
PGUSER=$(value IMBOY_PG_USER)
PGPASSWORD=$(value IMBOY_PG_PASSWORD)
PGDATABASE=$(value IMBOY_PG_DB)
CLEANUP_LOGIN=$(value IMBOY_DIRECT_CLEANUP_PG_USER)
CLEANUP_PASSWORD=$(value IMBOY_DIRECT_CLEANUP_PG_PASSWORD)
CLEANUP_CREATE=$create
export PGUSER PGPASSWORD PGDATABASE CLEANUP_LOGIN CLEANUP_PASSWORD CLEANUP_CREATE
# The bundled Erlang pg_conf has ssl => false. Do not let libpq prefer SSL and
# accidentally verify a different HBA rule; custom TLS configurations use DBA flow.
export PGHOST=imboy_pg18 PGPORT=5432 PGSSLMODE=disable PGGSSENCMODE=disable PGCONNECT_TIMEOUT=10 LC_ALL=C

pg_id=$(compose ps -q imboy_pg18 2>/dev/null) || fail '无法读取 PG 容器'
backend_id=$(compose ps -q imboy_backend 2>/dev/null) || fail '无法读取后端容器'
[[ "$pg_id" =~ ^[a-f0-9]{12,64}$ && "$backend_id" =~ ^[a-f0-9]{12,64}$ ]] \
  || fail 'PG 与后端必须各有一个正在运行的容器；先完成后端迁移'
running_env=$(docker inspect --format '{{json .Config.Env}}' "$backend_id" 2>/dev/null) \
  || fail '无法读取运行中后端配置；未执行 SQL'
# Compare via stdin, never jq --arg/argv: credentials must match the running worker,
# including higher-priority aliases and the worker password-file exclusion rule.
if ! printf '%s\n%s\n' "$resolved" "$running_env" | jq -es '
  def compatible:
    (has("IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE") | not)
    and all([[.IMBOY_PG_USERNAME, .IMBOY_PG_USER],
             [.IMBOY_PG_DATABASE, .IMBOY_PG_DB]][];
            .[0] == null or .[0] == "" or .[0] == .[1]);
  if length != 2 or (.[1] | type) != "array" then false else
    .[0].services.imboy_backend.environment as $b |
    (.[1] | map(index("=") as $n |
      if $n == null or $n == 0 then error("invalid environment")
      else {key: .[0:$n], value: .[($n+1):]} end)) as $pairs |
    ($pairs | from_entries) as $r |
    ($pairs | length) == ($r | length)
    and ($b | compatible) and ($r | compatible)
    and all(["IMBOY_PG_HOST", "IMBOY_PG_PORT", "IMBOY_PG_USER", "IMBOY_PG_DB",
             "IMBOY_PG_PASSWORD", "IMBOY_DIRECT_CLEANUP_PG_USER",
             "IMBOY_DIRECT_CLEANUP_PG_PASSWORD"][]; $r[.] == $b[.])
  end
' >/dev/null 2>&1; then
  fail 'Compose 与运行中后端凭据不一致或存在冲突别名/密码文件；先同步部署配置，未执行 SQL'
fi
unset resolved running_env
pg_image=$(docker inspect --format '{{.Image}}' "$pg_id" 2>/dev/null) || fail '无法读取 PG 镜像'
[[ "$pg_image" =~ ^sha256:[a-f0-9]{64}$ ]] || fail '无法确定已运行 PG 的本地镜像'
# Read the actual release oracle, also when auto_migrate was disabled.
versions=$(docker exec "$backend_id" sh -c '
  for f in /opt/imboy/lib/imboy-*/priv/migrations/*.up.sql; do
    test -f "$f" || exit 1
    f=${f##*/}; printf "%s\n" "${f%%_*}"
  done
' 2>/dev/null) || fail '无法读取运行中后端的迁移清单；自定义 release 路径须由 DBA 验收'
CLEANUP_VERSIONS=$(jq -Rers '
  split("\n") | map(select(length > 0)) |
  if length > 0 and all(.[]; test("^[0-9]{8}$")) then
    map(tonumber) | sort |
    if length == (unique | length) then join(",") else error("duplicate versions") end
  else error("invalid migration names") end
' <<< "$versions" 2>/dev/null) || fail '后端迁移清单为空、重号或格式不符'
export CLEANUP_VERSIONS
unset versions
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

# No customer volume is attached. HBA sees the worker source address, not loopback.
client() {
  docker run --rm -i --pull never --network "container:$backend_id" \
    --entrypoint /bin/sh -e PGHOST -e PGPORT -e PGSSLMODE -e PGGSSENCMODE -e PGUSER -e PGPASSWORD \
    -e PGDATABASE -e PGCONNECT_TIMEOUT -e LC_ALL -e CLEANUP_VERSIONS \
    -e CLEANUP_LOGIN -e CLEANUP_PASSWORD -e CLEANUP_CREATE "$pg_image" -c "$1"
}
if [[ $create == true ]]; then
  printf '[INFO] cleanup-db: 创建缺失 LOGIN 并验证；使用已有数据库权限，不自动提权\n'
else
  printf '[INFO] cleanup-db: 只验证已有 LOGIN，不更改账号或授权\n'
fi
if ! client '
  set -eu
  umask 077
  sql=$(mktemp)
  trap '\''rm -f "$sql"'\'' EXIT
  cat > "$sql"
  # SQL comes from -f; stdin supplies only the two hidden \password prompts.
  printf "%s\n%s\n" "$CLEANUP_PASSWORD" "$CLEANUP_PASSWORD" |
    psql -X --no-password -q -v ON_ERROR_STOP=1 -f "$sql"
' < cleanup-db/bootstrap.sql >"$work/bootstrap.log" 2>&1; then
  stage=$(sed -n 's/^CLEANUP_STAGE=\([a-z_]*\)$/\1/p' "$work/bootstrap.log" | tail -1)
  fail "初始化/前置验证失败（阶段: ${stage:-connection}）；检查迁移、管理权限和现有角色。事务未提交时新建 LOGIN 自动回滚；不要重建数据目录"
fi
if grep -qx CLEANUP_CREATED "$work/bootstrap.log"; then
  printf '[INFO] cleanup-db: 新 LOGIN 已原子创建；继续验证实际认证与权限\n'
else
  printf '[INFO] cleanup-db: 已有 LOGIN 保持原样；继续验证实际认证与权限\n'
fi

PGUSER=$CLEANUP_LOGIN
# A connection failure alone is not authentication evidence. Require password
# rejection, then independently require a successful correct-password session.
PGPASSWORD="invalid-$(openssl rand -hex 32)"
if client 'exec psql -X --no-password -qAt -v ON_ERROR_STOP=1 -c "SELECT 1"' \
    </dev/null >"$work/negative.log" 2>&1; then
  fail '错误密码也能登录；请 DBA 修复后端连接路径的 HBA，禁止以 trust 代替密码认证'
fi
grep -q 'password authentication failed' "$work/negative.log" \
  || fail '错误密码检查未得到明确认证拒绝；先排查网络、HBA、镜像或客户端，不能判定通过'
PGPASSWORD=$CLEANUP_PASSWORD
unset CLEANUP_PASSWORD
if ! client 'exec psql -X --no-password -qAt -v ON_ERROR_STOP=1' \
    < cleanup-db/verify.sql >"$work/verify.log" 2>&1; then
  fail '正确密码登录或最小权限检查失败；已有账号不会自动改密/补权，请 DBA 核对后重跑'
fi
grep -qx CLEANUP_VERIFIED "$work/verify.log" || fail '缺少数据库验证完成标记'
printf '[PASS] cleanup-db: 错误密码拒绝、正确密码登录、独立身份及 SET ROLE/函数授权均通过\n'
