# 独立清理数据库 LOGIN 开通与验收 / Cleanup database account

本节命令仅在客户服务器首次安装、迁移完成后，由获授权的数据库管理员执行。**制包与离线自测不执行这些数据库操作，也不预填通过结果。** `install.sh --check` 不执行本节命令。

## 为什么还需要这一步

上游迁移只创建 `v5_worker` 这样的 `NOLOGIN` 权限角色。原安装器填写 `IMBOY_DIRECT_CLEANUP_PG_USER/PASSWORD`，但不创建 LOGIN 或授予权限。后台清理 worker 延迟执行，账号不可用时记录告警，可能仍然有 `/healthz=ok`；因此“能打开系统”不能证明清理功能已就绪。

清理 LOGIN 必须独立于 `POSTGRES_USER`，使用 `.env` 中的匹配密码，仅加入 `v5_worker`。不可是 superuser、replication、bypassrls，不应拥有 public 表或加入应用/proof 权限角色。已有角色不会通过下面命令自动重置密码或追加权限。

## 1. 检查配置和迁移

从包根进入 `deploy/`。字段沿用本包无引号的 `KEY=value` 格式，不打印密码。

```bash
cd deploy
docker compose --env-file .env -f docker-compose.community.yml ps imboy_pg18 imboy_backend
```

先按 [现场验收命令](ACCEPTANCE_COMMANDS.md) 确认迁移状态为 clean，且版本与实际镜像对应。不能在业务迁移失败时临时创建一个同名 `v5_worker` 来绕过故障。

## 2. 首次创建；已存在则不修改

下面整块在 Bash 子 shell 内执行。使用 PostgreSQL 容器的 `psql`，宿主机无需数据库客户端。`\getenv` 从容器环境取值，密码不进入宿主机命令行参数；请勿开启 `set -x` 或把展开后的环境写入日志。

```bash
(
  set -euo pipefail
  test -f .env
  env_value() { awk -F= -v key="$1" '$1 == key {sub(/^[^=]*=/, ""); print; exit}' .env; }
  IMBOY_DIRECT_CLEANUP_PG_USER=$(env_value IMBOY_DIRECT_CLEANUP_PG_USER)
  IMBOY_DIRECT_CLEANUP_PG_PASSWORD=$(env_value IMBOY_DIRECT_CLEANUP_PG_PASSWORD)
  app_user=$(env_value POSTGRES_USER)
  [[ "$IMBOY_DIRECT_CLEANUP_PG_USER" =~ ^[a-z_][a-z0-9_]{0,62}$ ]]
  test "$IMBOY_DIRECT_CLEANUP_PG_USER" != "$app_user"
  test "${#IMBOY_DIRECT_CLEANUP_PG_PASSWORD}" -ge 32
  case "$IMBOY_DIRECT_CLEANUP_PG_PASSWORD" in *CHANGE_ME*) exit 1;; esac
  export IMBOY_DIRECT_CLEANUP_PG_USER IMBOY_DIRECT_CLEANUP_PG_PASSWORD
  docker compose --env-file .env -f docker-compose.community.yml exec -T \
    -e IMBOY_DIRECT_CLEANUP_PG_USER -e IMBOY_DIRECT_CLEANUP_PG_PASSWORD \
    imboy_pg18 sh -c 'exec psql -X -v ON_ERROR_STOP=1 -U "$POSTGRES_USER" -d "$POSTGRES_DB"' <<'SQL'
\getenv cleanup_login IMBOY_DIRECT_CLEANUP_PG_USER
\getenv cleanup_password IMBOY_DIRECT_CLEANUP_PG_PASSWORD
SELECT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = :'cleanup_login') AS role_exists \gset
\if :role_exists
  \echo 'LOGIN already exists; password and memberships were not changed. Verify below.'
\else
  BEGIN;
  CREATE ROLE :"cleanup_login" LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS PASSWORD :'cleanup_password';
  GRANT v5_worker TO :"cleanup_login";
  COMMIT;
\endif
SQL
)
```

`v5_worker` 不存在、密码不符合规则或 SQL 失败时停止。该事务中的新 LOGIN 不应被当作已完成交付，必须继续验证实际密码和角色隔离。不要给它业务表所有权、DBA 身份或应用角色来消除错误。

## 3. 独立客户端验证密码认证与权限边界

仍在 `deploy/`，执行。不能假设容器内 `127.0.0.1` 一定验证密码，它可能命中 HBA 的 trust 规则。这里使用同一网络中的临时客户端容器，不挂客户数据卷，复用运行中 PG 的本地镜像；先确认错误密码被拒绝，再以正确密码执行只读检查。

```bash
(
  set -euo pipefail
  env_value() { awk -F= -v key="$1" '$1 == key {sub(/^[^=]*=/, ""); print; exit}' .env; }
  CLEANUP_LOGIN=$(env_value IMBOY_DIRECT_CLEANUP_PG_USER)
  CLEANUP_DATABASE=$(env_value POSTGRES_DB)
  PGPASSWORD=$(env_value IMBOY_DIRECT_CLEANUP_PG_PASSWORD)
  export PGPASSWORD
  pg_image_id=$(docker inspect --format '{{.Image}}' imboy_pg18)
  test -n "$pg_image_id"
  negative_log=$(mktemp)
  trap 'rm -f "$negative_log"' EXIT
  if PGPASSWORD="invalid-$(openssl rand -hex 16)" \
    docker run --rm --pull never --network imboy-network --entrypoint psql \
      -e PGPASSWORD -e LC_ALL=C "$pg_image_id" -X -v ON_ERROR_STOP=1 --no-password \
      -h imboy_pg18 -U "$CLEANUP_LOGIN" -d "$CLEANUP_DATABASE" \
      -c 'SELECT 1' >/dev/null 2>"$negative_log"; then
    printf '%s\n' 'FAIL: 错误密码也可登录；请 DBA 核查 HBA 认证方式。' >&2
    exit 1
  fi
  if ! grep -qi 'password authentication failed' "$negative_log"; then
    printf '%s\n' 'BLOCKED: 未得到明确密码认证拒绝；先排查网络/镜像/客户端错误。' >&2
    exit 1
  fi
  docker run --rm -i --pull never --network imboy-network --entrypoint psql \
    -e PGPASSWORD "$pg_image_id" -X -v ON_ERROR_STOP=1 --no-password \
    -h imboy_pg18 -U "$CLEANUP_LOGIN" -d "$CLEANUP_DATABASE" <<'SQL'
SELECT current_user, rolsuper, rolcreatedb, rolcreaterole, rolreplication,
       rolbypassrls, rolinherit
FROM pg_roles WHERE rolname = current_user;
SELECT parent.rolname AS granted_role
FROM pg_auth_members membership
JOIN pg_roles parent ON parent.oid = membership.roleid
JOIN pg_roles member ON member.oid = membership.member
WHERE member.rolname = current_user ORDER BY 1;
SELECT count(*) AS public_relations_owned
FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
WHERE n.nspname = 'public' AND c.relowner = current_user::regrole;
SET ROLE v5_worker;
SELECT current_user AS effective_role;
RESET ROLE;
SQL
)
```

通过条件：错误密码被拒绝且正确密码连接成功（只有前者失败不能算通过）；六个角色属性均为 `f`；直接成员仅 `v5_worker`；public 对象所有权计数为 0；`SET ROLE` 后身份为 `v5_worker`。还需确认该角色没有间接高权限成员关系，并在真实附件生命周期验收中观察清理 worker 无认证/权限告警。此处不主动调用删除函数，也不宣称真实垃圾回收已验。

本地 socket 或 loopback 的 trust 登录不能替代上述正反认证验证。自定义网络/HBA 场景须按实际配置调整并记录。已有 LOGIN 的密码或授权与配置不一致时，交给数据库管理员按变更流程处理；不要重新生成 `.env` 或重建数据目录。

## 源码依据

以下是源码定位入口；审阅时切换至包内 `SOURCE_COMMIT` 对应提交：

- [部署配置说明](https://github.com/imboy-pub/imboy/blob/main/deploy/README.md)
- [清理 worker 的初始化、连接和角色检查](https://github.com/imboy-pub/imboy/blob/main/src/ds/attachment_direct_cleanup_worker.erl)
