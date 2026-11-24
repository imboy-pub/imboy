#!/usr/bin/env bash
set -euo pipefail
project_dir=$1 version=$2 source_head=$3 prod_env=$4 db_container=$5 db_name=$6 db_user=$7 local_mode=$8
shift 8
candidate=${IMBOY_OFFLINE_CANDIDATE:-}
phase=${IMBOY_OFFLINE_PHASE:-migrate}

fail() { printf '%s\n' "$*" >&2; exit 1; }

cd "$project_dir"
project_dir=$(pwd -P)
marker="$project_dir/.imboy-offline-upgrade"
if [[ -z "$candidate" ]]; then
  exec 9>.imboy-deploy.lock
  flock -n 9 || fail "无法获取部署锁，稍后重试"
  [[ ! -e "$marker" ]] || fail "维护升级尚未完成，请继续原 api --offline 发布"
else
  exec 8>.imboy-offline-upgrade.lock
  flock -n 8 || fail "上一次离线迁移尚未退出"
fi

shopt -s nullglob
if [[ "$local_mode" -eq 0 ]]; then
  if [[ -n "$candidate" ]]; then
    release=$candidate
    [[ -d "$release" && ! -L "$release" && "$(cat "$release/etc/source-head")" == "$source_head" ]] \
      || fail "已安装候选包与本次源码不一致"
  else
    releases=()
    for release in /usr/local/imboy-"$version"-*; do
      [[ -d "$release" && ! -L "$release" && -r "$release/etc/source-head" ]] || continue
      [[ "$(<"$release/etc/source-head")" != "$source_head" ]] || releases+=("$release")
    done
    [[ ${#releases[@]} -eq 1 ]] || fail "须有唯一已安装候选包（版本 $version，源码 $source_head），找到 ${#releases[@]} 个"
    release=${releases[0]}
  fi
  runtime=("$release"/erts-*/bin/erl)
  release_config="$release/releases/$version"
  migrations="$release/lib/imboy-$version/priv/migrations"
  [[ ${#runtime[@]} -eq 1 && -x "${runtime[0]}" && -r "$release_config/sys.config" \
    && -r "$release_config/no_dot_erlang.boot" ]] || fail "候选发布包运行时不完整"
  vm_args=(-boot "$release_config/no_dot_erlang" -config "$release_config/sys" -pa "$release"/lib/*/ebin)
  migration_source=$release
else
  stage=$(mktemp -d)
  trap 'rm -rf -- "$stage"' EXIT
  gzip -dc | tar -xf - -C "$stage" || fail "本地迁移包上传或解包失败"
  migrations="$stage/priv/migrations"
  migration_source="本地 Git $source_head"
fi
target_version=0
for migration in "$migrations"/*.up.sql; do
  [[ "${migration##*/}" =~ ^([0-9]{8})_ ]] || fail "迁移文件名非法：${migration##*/}"
  migration_version=$((10#${BASH_REMATCH[1]}))
  if (( migration_version > target_version )); then target_version=$migration_version; fi
done
[[ "$target_version" -gt 0 ]] || fail "迁移包缺少迁移文件"

[[ -f "$prod_env" && -r "$prod_env" ]] || fail "生产环境文件不可读：$prod_env"
check_stopped() {
  for port in "$@"; do
    [[ -n "$port" ]] || continue
    listeners=$(ss -tlnH "sport = :$port")
    [[ -z "$listeners" ]] || fail "端口 $port 仍监听，须先停止 IMBoy 节点"
  done
  for process in heart beam.smp; do
    process_ids=$(pgrep -x "$process") || [[ $? -eq 1 ]] || fail "无法检查 $process 进程"
    for pid in $process_ids; do
      if ! executable=$(readlink "/proc/$pid/exe"); then
        [[ ! -d "/proc/$pid" ]] || fail "无法确认进程 $pid 已停写"
        continue
      fi
      case "$executable" in
        /usr/local/imboy-*|"$project_dir"/_rel/imboy/*)
          fail "IMBoy $process 进程 $pid 尚在运行：$executable"
          ;;
      esac
    done
  done
}

stop_local_writers() {
  local port listeners pids pid executable root
  local -A releases_to_stop=()
  for port in "$@"; do
    [[ -n "$port" ]] || continue
    listeners=$(ss -tlnH "sport = :$port")
    [[ -n "$listeners" ]] || continue
    pids=$(lsof -t -a -iTCP:"$port" -sTCP:LISTEN) || fail "无法确认端口 $port 的监听进程"
    [[ -n "$pids" ]] || fail "无法确认端口 $port 的监听进程"
    for pid in $pids; do
      [[ "$pid" =~ ^[0-9]+$ ]] || fail "端口 $port 的监听 PID 非法"
      executable=$(readlink "/proc/$pid/exe") || fail "无法确认监听进程 $pid 的归属"
      root=${executable%/erts-*/bin/beam.smp}
      if [[ "$root" == "$executable" ]] ||
         [[ ! "$root" =~ ^/usr/local/imboy-[a-zA-Z0-9._-]+-[a-zA-Z0-9_-]+$ && "$root" != "$project_dir/_rel/imboy" ]]; then
        fail "端口 $port 仍监听，进程 $pid 不是本次部署可停止的 IMBoy 节点"
      fi
      releases_to_stop["$root"]=1
    done
  done
  pids=$(pgrep -x beam.smp) || [[ $? -eq 1 ]] || fail "无法检查 legacy IMBoy 进程"
  for pid in $pids; do
    if executable=$(readlink "/proc/$pid/exe") && [[ "$executable" == "$project_dir"/_rel/imboy/erts-*/bin/beam.smp ]]; then
      releases_to_stop["$project_dir/_rel/imboy"]=1
    fi
  done
  for root in "${!releases_to_stop[@]}"; do
    [[ -x "$root/bin/imboy" ]] || fail "缺少节点停止入口：$root/bin/imboy"
  done
  for root in "${!releases_to_stop[@]}"; do
    printf '停止 IMBoy 节点 %s\n' "$root"
    IMBOYENV=pro timeout 20s "$root/bin/imboy" stop || fail "节点停止失败：$root"
  done
}

[[ "$local_mode" -eq 1 || -n "$candidate" ]] || check_stopped "$@"

set -a
. "$prod_env"
set +a
unset ERL_AFLAGS ERL_FLAGS ERL_ZFLAGS ERL_LIBS
if [[ "$local_mode" -eq 1 ]]; then
  command -v erl >/dev/null && command -v erlc >/dev/null || fail "服务器需安装 Erlang/OTP 28+（erl 和 erlc）"
  [[ -n "${IMBOY_PG_PASSWORD:-}" ]] || fail "生产环境文件缺少 IMBOY_PG_PASSWORD"
  export IMBOY_PG_HOST="${IMBOY_PG_HOST:-127.0.0.1}"
  export IMBOY_PG_PORT="${IMBOY_PG_PORT:-5432}"
  export IMBOY_PG_USERNAME="${IMBOY_PG_USERNAME:-${IMBOY_PG_USER:-$db_user}}"
  export IMBOY_PG_DATABASE="${IMBOY_PG_DATABASE:-${IMBOY_PG_DB:-$db_name}}"
fi

db_query() {
  docker exec "$db_container" psql -XAtqw -v ON_ERROR_STOP=1 -U "$db_user" -d "$db_name" -c "$1"
}
identity_sql="SELECT current_database() || ':' || oid || ':' || extract(epoch FROM pg_postmaster_start_time()) FROM pg_database WHERE datname = current_database()"
ledger_sql='SELECT CASE WHEN count(*) = 1 AND bool_and(dirty IS FALSE) THEN max(version) END FROM public.schema_migrations'
sessions_sql="SELECT count(*) FROM pg_stat_activity WHERE datname = current_database() AND pid <> pg_backend_pid() AND (backend_type IS NULL OR backend_type = 'client backend')"
db_identity=$(db_query "$identity_sql")
[[ -n "$db_identity" ]] || fail "无法确认目标数据库身份"
current_version=$(db_query "$ledger_sql")
[[ "$current_version" =~ ^[0-9]{1,8}$ && "$current_version" -le "$target_version" ]] \
  || fail "迁移台账缺失、非单一干净版本，或已超过候选版本 $target_version"
if [[ -n "$candidate" && -e "$marker" ]]; then
  [[ "$(<"$marker")" == "$IMBOY_OFFLINE_KEY" ]] || fail "维护状态与本次发布不匹配"
fi

if [[ "$local_mode" -eq 1 ]]; then
  cd "$stage"
  mkdir ebin
  erlc -I epgsql/include -o ebin epgsql/src/*.erl erlang_migrate/src/erlang_migrate_driver.erl
  erlc -pa ebin -I epgsql/include -I epgsql/src -o ebin epgsql/src/*/*.erl
  erlc -pa ebin -o ebin erlang_migrate/src/*.erl imboy_migrate.erl
  cp epgsql/src/epgsql.app.src ebin/epgsql.app
  runtime=(erl)
  vm_args=(-pa "$stage/ebin")
else
  cd "$release"
fi
run_migration_vm() {
  IMBOYENV=pro ERL_CRASH_DUMP_SECONDS=0 "${runtime[0]}" -noshell -noinput \
  "${vm_args[@]}" \
  -eval '
    [IdentitySql, ExpectedDb, SessionsSql, MigrationDir, LocalMode, Phase, RequireVerifiedConn] = init:get_plain_arguments(),
    Result = try
        Config = case LocalMode of
            "1" -> #{host => os:getenv("IMBOY_PG_HOST"),
                     port => list_to_integer(os:getenv("IMBOY_PG_PORT")),
                     username => os:getenv("IMBOY_PG_USERNAME"),
                     password => os:getenv("IMBOY_PG_PASSWORD"),
                     database => os:getenv("IMBOY_PG_DATABASE"), timeout => 4000};
            _ -> ok = application:load(imboy),
                   ok = imboy_env:override_from_env(),
                   {ok, Account} = application:get_env(imboy, super_account),
                   Account
        end,
        {ok, _} = application:ensure_all_started(epgsql),
        {module, imboy_migrate} = code:ensure_loaded(imboy_migrate),
        UseExistingConnection = erlang:function_exported(imboy_migrate, migrate, 2),
        (UseExistingConnection orelse RequireVerifiedConn =:= "0") orelse error(migration_entrypoint_missing),
        Conn = case epgsql:connect(Config) of
            {ok, Connection} -> Connection;
            {error, ConnectReason} when is_atom(ConnectReason) -> error({database_connect_failed, ConnectReason});
            {error, _} -> error(database_connect_failed)
        end,
        try
            {ok, _, [{DbIdentity}]} = epgsql:squery(Conn, IdentitySql),
            binary_to_list(DbIdentity) =:= ExpectedDb orelse error(database_mismatch),
            case Phase of
                "probe" -> ok;
                "migrate" ->
                    {ok, _, [{Sessions}]} = epgsql:squery(Conn, SessionsSql),
                    Sessions =:= <<"0">> orelse error(database_sessions_present),
                    case UseExistingConnection of
                        true -> ok = imboy_migrate:migrate(Conn, MigrationDir);
                        false -> ok
                    end
            end
        after
            epgsql:close(Conn)
        end,
        case {UseExistingConnection, Phase} of
            {false, "migrate"} -> ok = imboy_migrate:migrate(MigrationDir);
            _ -> ok
        end,
        0
    catch
        error:{database_connect_failed, NetworkReason} ->
            io:format(standard_error, "无法连接目标数据库 (~p)，请检查生产环境的 PostgreSQL 地址与端口~n", [NetworkReason]), 1;
        error:database_connect_failed ->
            io:format(standard_error, "数据库连接失败，请检查生产环境的 PostgreSQL 用户、密码与库名~n", []), 1;
        error:database_mismatch ->
            io:format(standard_error, "迁移连接与部署数据库不一致~n", []), 1;
        error:database_sessions_present ->
            io:format(standard_error, "目标数据库仍有其他客户端连接，请先停止写入方并断开连接~n", []), 1;
        error:migration_entrypoint_missing ->
            io:format(standard_error, "候选缺少离线迁移入口 imboy_migrate:migrate/2，请使用包含维护升级实现的已提交候选~n", []), 1;
        Class:Reason:Stack ->
            SafeReason = case Reason of
                {migration_failed, {Tag, _}} when is_atom(Tag) -> Tag;
                {Tag, _} when is_atom(Tag) -> Tag;
                Tag when is_atom(Tag) -> Tag;
                _ -> unknown
            end,
            Location = [{Module, Function} || {Module, Function, _, _} <- lists:sublist(Stack, 3)],
            io:format(standard_error, "离线迁移失败 (~p:~p) ~p~n", [Class, SafeReason, Location]), 1
    end,
    halt(Result).' -extra "$identity_sql" "$db_identity" "$sessions_sql" "$migrations" "$local_mode" "$1" \
    "$require_verified_conn"
}

require_verified_conn=$local_mode
if [[ -n "$candidate" ]]; then
  require_verified_conn=1
  run_migration_vm probe
  case "$phase" in
    probe) exit 0 ;;
    verify)
      [[ "$current_version" == "$target_version" ]] || fail "活动候选的数据库尚未迁移到 $target_version"
      exit 0 ;;
    migrate) ;;
    *) fail "未知维护升级阶段" ;;
  esac
  for port in "$@"; do
    [[ -n "$port" ]] || continue
    listeners=$(ss -tlnH "sport = :$port")
    [[ -z "$listeners" ]] || fail "生产槽 $port 仍监听，拒绝离线迁移"
  done
  [[ "$(db_query "$sessions_sql")" == 0 ]] || fail "目标库仍有客户端，请暂停写入及自动拉起机制后重试"
  if [[ "$current_version" != "$target_version" ]]; then
    umask 077
    mkdir -p /var/backups/imboy
    backup=$(mktemp "/var/backups/imboy/$db_name-$current_version-XXXXXX.dump")
    if ! docker exec "$db_container" pg_dump -w -Fc --lock-wait-timeout=5s -U "$db_user" -d "$db_name" >"$backup" \
       || ! docker exec -i "$db_container" pg_restore --list <"$backup" >/dev/null; then
      rm -f -- "$backup"
      fail "迁移前数据库备份失败"
    fi
    printf '迁移前备份 %s\n' "$backup"
  fi
  [[ "$(db_query "$identity_sql")" == "$db_identity" && "$(db_query "$ledger_sql")" == "$current_version" ]] \
    || fail "备份期间数据库身份或迁移台账发生变化"
  if [[ ! -e "$marker" ]]; then
    marker_tmp=$(mktemp "$marker.XXXXXX")
    printf '%s\n' "$IMBOY_OFFLINE_KEY" >"$marker_tmp"
    mv -T -- "$marker_tmp" "$marker"
    sync -f "$project_dir"
  fi
fi

if [[ "$local_mode" -eq 1 ]]; then
  run_migration_vm probe
  stop_local_writers "$@"
  check_stopped "$@"
fi
printf '离线迁移 %s（%s → %s）\n' "$migration_source" "$current_version" "$target_version"
run_migration_vm migrate

[[ "$(db_query "$identity_sql")" == "$db_identity" && "$(db_query "$ledger_sql")" == "$target_version" ]] \
  || fail "迁移后未能核验目标数据库与版本 $target_version"
printf '离线迁移完成，数据库版本 %s\n' "$target_version"
