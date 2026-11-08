# 独立清理数据库 LOGIN 初始化与验收 / Cleanup database account

社区版安装器现在把清理数据库身份作为交付门禁：**默认只验证；首次安装显式传入 `--init-cleanup-db`，才以现有数据库管理权限创建缺失的 LOGIN。** 已有账号始终只验证，不自动改密、补权或降权。宿主机只使用 Shell、Docker、jq、OpenSSL 和常规 Unix 工具，`psql` 来自 PostgreSQL 客户端容器，不需要宿主机 Python 或 PostgreSQL 客户端。

本节数据库操作只在获授权的目标实例执行。制包、离线自测和包根 `install.sh --check` 不执行真实数据库初始化，也不预填验收结果。商务版、外部数据库和权限不足的实例使用第 3 节 DBA 路径。

## 1. 初始化边界与迁移顺序

清理 LOGIN 必须独立于业务账号 `POSTGRES_USER`，密码至少 32 字符、不同于业务密码，并与后端实际使用的 `IMBOY_DIRECT_CLEANUP_PG_PASSWORD` 一致。它是 `LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS`，只加入 `v5_worker`，成员关系为 `ADMIN FALSE, INHERIT FALSE, SET TRUE`；另有目标数据库的 `CONNECT` 权限。初始化不授予表写权限、数据库或 schema 的 `CREATE` 权限，也不复用超级用户作为工作账号。

| 阶段 | 依据与门禁 |
|---|---|
| 正常数据库迁移 | 179 创建 `v5_worker NOLOGIN NOINHERIT`；267 引入 direct tombstone API；270 加入跨域删除保护；271 引入 cross rescan API；274 收紧 proof worker 边界 |
| 完整迁移核验 | 从**运行中的 backend 镜像**读取迁移文件集合，与 `schema_migrations_history` 逐版本精确比较；`schema_migrations` 必须只有一行、head 匹配且 `dirty=false`；镜像集合须包含 179/267/270/271/274 |
| 受控创建 | 只有显式授权且 LOGIN 不存在时，才在一个事务内创建、设置 SCRAM 密码、授予 `v5_worker` 和目标库 `CONNECT`；使用事务锁串行化本工具在同一数据库内对同名账号的创建 |
| 实际验证 | 先比对 Compose 与运行中 backend 的数据库环境变量，再从 backend 的网络命名空间连接真实 PostgreSQL；要求错误密码明确被拒绝、正确密码登录成功，再验证账号属性、成员关系、所有权、权限和 `SET ROLE v5_worker` |

`imboy_app` 默认在启动监听器前运行迁移，但可配置关闭自动迁移；清理 worker 延迟执行，失败可能只有告警。因此 `pg_isready`、角色已存在或 `/healthz=ok` 均不能替代上述门禁。迁移缺失或 dirty 时先修复迁移，不手工创建同名 `v5_worker`、修改迁移记录或跳过校验。

`--init-cleanup-db` 使用 Compose 实际配置中的 `POSTGRES_USER` 连接同一个数据库。该身份需要已有的 `CREATEROLE`、`v5_worker` 的直接 `ADMIN OPTION` 和数据库 `CONNECT` 授权权，或原本就是具备这些能力的数据库管理员；还须能读取 `public.schema_migrations` 与 `public.schema_migrations_history`，供前置迁移核验使用。脚本只检查并使用现有权限；不足时停止，**不自动给任何账号提权**。标准 PG 镜像首次初始化使用的管理身份与清理工作身份是两个账号。

权限检查覆盖这个清理身份的部署契约，不代表整个数据库 ACL 已审计。迁移 190 合法授予 `v5_worker` 对五张生命周期表的 `SELECT`；`NOINHERIT` 不能屏蔽 `PUBLIC` 权限。脚本检查 LOGIN 与有效 worker 对 public 表的写权限等危险授权，但不全局 `REVOKE PUBLIC`，也不声称清理账号只能执行四个函数。共享 ACL 的额外授权须由 DBA 评估其业务影响后处理。

**PG 镜像前置条件：**本变更移除了 `docker/pg-initdb-imboy.sh` 和 `docker/pg-update-imboy.sh` 的 `set -x`，避免首次初始化时追踪输出 `POSTGRES_PASSWORD`。宿主机脚本更新不能修改已发布镜像内部的旧脚本；全新部署须由交付方重建或选用包含该修复的 PG 镜像，并记录实际 digest。不得仅凭当前源码或版本标签推定运行镜像已修复。历史日志如已包含凭据，按客户的凭据轮换和日志访问流程处理，不删除数据卷重做初始化。

## 2. 标准社区版：显式创建或只验证

先按 [INSTALL.md](INSTALL.md) 填好配置、确认镜像和网络条件。对全新实例，从独立包根目录执行：

```bash
bash install.sh --init-cleanup-db
```

从源码仓库的 `deploy/` 执行时，入口为：

```bash
bash install.sh --edition community --init-cleanup-db
```

安装器依次启动服务、完成 TLS、等待后端健康，再运行清理身份门禁，最后才进入 sanity 和完成输出。不传 `--init-cleanup-db` 时仍强制验证；缺少 LOGIN 会停止，不会把生成密码或配置预检通过当成数据库身份已就绪。

服务和迁移已就绪时，可从 `deploy/` 单独重跑该步骤：

```bash
bash init-cleanup-db.sh           # 只验证，不改变已有账号
bash init-cleanup-db.sh --create  # 经授权才执行；仅创建缺失账号
```

启用 overlay 时，传入与实际部署一致的 `COMPOSE_FILES`，例如：

```bash
COMPOSE_FILES='docker-compose.community.yml docker-compose.livekit-turn.yml' \
  bash init-cleanup-db.sh
```

脚本只支持内置社区拓扑：backend 与 PG 指向相同业务库，host 为 `imboy_pg18`、port 为 5432，沿用交付配置的 `pg_conf` 中 `ssl => false`。客户端显式设置 `PGSSLMODE=disable`、`PGGSSENCMODE=disable` 与之匹配；已自定义 TLS 的实例须走第 3 节，不为通过检查关闭原有 TLS。它复用运行中 PG 的本地镜像作为临时客户端，以 `--network container:<backend容器ID>` 共用后端网络命名空间，不挂客户数据卷、不另拉镜像。这样验证使用后端的源地址与连接方式，不会把容器内 socket、loopback 或其他 TLS 路径的 `trust` 登录当成密码认证。

脚本还检查实际 backend 容器的数据库环境与当前 Compose 一致，拒绝冲突的 `IMBOY_PG_USERNAME` / `IMBOY_PG_DATABASE` 高优先级别名，以及与明文环境变量同时设置的 `IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE`。只改 `.env` 尚未重建 backend 时，不会先创建或验证另一组凭据；应按正常部署流程更新容器、等待健康后重跑。运行中另行修改 Erlang `pg_conf` 或自定义入口脚本属于 DBA 验收范围。

密码通过环境和标准输入传入客户端，SQL 文件不包含明文密码；`psql \password` 在客户端生成 verifier，避免明文进入 PostgreSQL statement/error 日志。不要开启 Shell xtrace/verbose、打印完整 Compose 配置或容器环境。只有门禁输出 `cleanup-db` 的最终 `[PASS]` 且退出码为 0，才表示本次账号检查通过；这不是附件真实垃圾回收验收结果。

## 3. DBA 路径：商务版、外部数据库或管理权限不足

商务安装不自动运行此社区门禁；`--init-cleanup-db` 对商务版直接拒绝。外部/自定义拓扑也不自动降级为本机 PG。DBA 先确认后端实际的 host、port、database、业务 LOGIN、清理 LOGIN、认证方式和运行镜像；按 [ACCEPTANCE_COMMANDS.md](ACCEPTANCE_COMMANDS.md) 核对真实迁移来源。下面复用包内 `deploy/cleanup-db/bootstrap.sql` 与 `deploy/cleanup-db/verify.sql`，不复制另一套授权 SQL。

### 3.1 准备同路径客户端与迁移清单

在 `deploy/` 的受控 Bash 会话中执行。输入的是连接元数据，不是密码。`client_image` 应是已在本机的 PostgreSQL 18 客户端镜像 ID/digest；内置 PG 可用 `docker inspect --format '{{.Image}}' <PG容器ID>` 获取。`backend_id` 使用实际 backend 容器 ID，不猜测其他实例。所有连接参数必须与 worker 一致。

```bash
set +x
set +v
set -euo pipefail
umask 077
unset PGPASSWORD
read -r -p 'Backend container ID: ' backend_id
read -r -p 'Local PostgreSQL 18 client image ID/digest: ' client_image
read -r -p 'Worker PostgreSQL host: ' PGHOST
read -r -p 'Worker PostgreSQL port: ' PGPORT
read -r -p 'Worker-matching libpq SSL mode (disable for bundled ssl=false): ' PGSSLMODE
case "$PGSSLMODE" in disable|require|verify-ca|verify-full) ;; *) exit 1 ;; esac
read -r -p 'Worker database: ' PGDATABASE
read -r -p 'Business database LOGIN: ' app_login
read -r -p 'Authorized DBA LOGIN: ' PGUSER
dba_user=$PGUSER
read -r -p 'Dedicated cleanup LOGIN: ' CLEANUP_LOGIN
[[ "$CLEANUP_LOGIN" =~ ^[a-z_][a-z0-9_]{0,62}$ ]]
[[ "$CLEANUP_LOGIN" != pg_* && "$CLEANUP_LOGIN" != v5_worker ]]
[[ "$CLEANUP_LOGIN" != "$app_login" && "$CLEANUP_LOGIN" != "$dba_user" ]]

CLEANUP_VERSIONS=$(docker exec "$backend_id" sh -c '
  for f in /opt/imboy/lib/imboy-*/priv/migrations/*.up.sql; do
    test -f "$f" || exit 1
    f=${f##*/}; printf "%s\n" "${f%%_*}"
  done
' | jq -Rers '
  split("\n") | map(select(length > 0)) |
  if length > 0 and all(.[]; test("^[0-9]{8}$")) then
    map(tonumber) | sort |
    if length == (unique | length) then join(",") else error("duplicate versions") end
  else error("invalid migration names") end
')
CLEANUP_CREATE=true
PGCONNECT_TIMEOUT=10
PGGSSENCMODE=disable
LC_ALL=C
export PGHOST PGPORT PGSSLMODE PGGSSENCMODE PGDATABASE PGUSER PGCONNECT_TIMEOUT LC_ALL
export CLEANUP_LOGIN CLEANUP_CREATE CLEANUP_VERSIONS

db_client() {
  docker run --rm -it --pull never --network "container:$backend_id" \
    --mount "type=bind,src=$PWD/cleanup-db,dst=/cleanup-db,readonly" \
    --entrypoint psql -e PGHOST -e PGPORT -e PGSSLMODE -e PGGSSENCMODE -e PGDATABASE -e PGUSER \
    -e PGCONNECT_TIMEOUT -e LC_ALL -e CLEANUP_LOGIN -e CLEANUP_CREATE \
    -e CLEANUP_VERSIONS "$client_image" -X -v ON_ERROR_STOP=1 "$@"
}
```

自定义 release 或 `scripts_path` 时，把清单读取位置改成**本次运行镜像实际使用的迁移目录**，不能手填几个最低版本冒充完整清单。外部数据库如要求 TLS/CA/客户端证书，须在上面的客户端和下面的负密码客户端中加入相同的 `PGSSLMODE`、`PGSSLROOTCERT` 等参数与只读证书挂载，沿用部署规定，不关闭校验。若 DBA 只允许从独立管理网连接，创建步骤使用获准管理客户端；后面的认证和权限验证仍从 worker 的实际网络路径执行，不为迁就脚本放宽 HBA。

### 3.2 管理员显式创建；已有账号不改

```bash
db_client -W -f /cleanup-db/bootstrap.sql
```

`-W` 首先交互提示 DBA 的连接密码。若清理 LOGIN 不存在，事务中的 `\password` 再以隐藏输入提示两次清理密码：输入后端实际配置中的强密码，不能输入 DBA/业务密码。已有 LOGIN 不会出现改密提示。密码不写在命令行、SQL 文件或 Shell 历史中。保留退出码和非敏感阶段信息；失败就停止，不手工跳过脚本中的权限或迁移校验。

该 SQL 只使用管理员已有权限。DBA 若没有创建/授权能力，须由已有权限的负责人执行；不要授予清理账号管理权限，也不要为安装器自动扩权。对已有账号，密码、成员关系或属性不符时由 DBA 在独立变更中修正；本脚本不代替密码轮换或账号用途审查。若无需创建，先设置并导出 `CLEANUP_CREATE=false` 再执行同一命令，即只做前置与存在性检查。

### 3.3 错误密码必须拒绝，正确密码必须通过

继续使用 3.1 的连接参数与客户端镜像。负例使用临时随机错误密码；网络失败不能当成认证拒绝：

```bash
PGUSER=$CLEANUP_LOGIN
export PGPASSWORD="invalid-$(openssl rand -hex 32)"
negative_log=$(mktemp)
trap 'rm -f "$negative_log"' EXIT
if docker run --rm --pull never --network "container:$backend_id" \
  --entrypoint psql -e PGHOST -e PGPORT -e PGSSLMODE -e PGGSSENCMODE -e PGDATABASE -e PGUSER \
  -e PGPASSWORD -e PGCONNECT_TIMEOUT -e LC_ALL "$client_image" \
  -X --no-password -qAt -v ON_ERROR_STOP=1 -c 'SELECT 1' \
  >"$negative_log" 2>&1; then
  printf '%s\n' 'FAIL: wrong password accepted; DBA must inspect HBA.' >&2
  exit 1
fi
if ! grep -q 'password authentication failed' "$negative_log"; then
  printf '%s\n' 'BLOCKED: no explicit password rejection; inspect network/HBA/client.' >&2
  exit 1
fi
unset PGPASSWORD
rm -f "$negative_log"
trap - EXIT
db_client -W -f /cleanup-db/verify.sql
```

最后一个 `-W` 提示输入**后端实际配置的清理密码**。必须连接成功、输出 `CLEANUP_VERIFIED` 且退出 0。`verify.sql` 在只读事务中检查 LOGIN 属性、唯一角色成员关系、所有权、CREATE/表写权限，再 `SET LOCAL ROLE v5_worker` 检查 schema 使用权和四个 cleanup 函数的 EXECUTE；不调用 claim/ack，不读写业务队列。仅负例失败或仅看到角色属性输出都不算通过。

## 4. 重跑、失败恢复与安全退役

| 情况 | 恢复操作 |
|---|---|
| 账号不存在，默认验证停止 | 确认管理权限后显式 `--init-cleanup-db` / `--create`，或由 DBA 执行第 3 节 |
| Compose 与运行中 backend 配置不一致 | 使用同一份配置按正常流程更新 backend 并等待健康；不要仅修改 `.env` 后把另一组凭据的验证当成 worker 就绪 |
| 创建事务内的授权、密码设置或连接失败 | 未提交时新角色与授权由 PostgreSQL 一起回滚；排除原因，保留原 `.env` 后重跑 |
| 提交结果不明，或提交后正反密码/权限验证失败 | 保留角色、原密码和 `.env`；先由 DBA 核对实际状态，重跑只验证已有账号，不自动删除或再生成密码 |
| 已有账号密码错误、额外角色/高权限或授权缺失 | DBA 核对账号用途并执行明确的凭据/权限变更；脚本不自动改密、补权或降权 |
| 迁移 dirty/缺版本、`v5_worker` 不安全 | 修复迁移或已有权限污染后再跑；不修改 history、伪造角色或全局撤销 PUBLIC 来绕过门禁 |

移除自动化补丁本身不需要回退数据库迁移。若要退役**本次新建、确认为清理专用**的 LOGIN，先暂停相关 backend/worker，避免继续建立连接，再由 DBA 从正确数据库执行下列 `psql` 命令。可重新准备第 3.1 节，设 `PGUSER=$dba_user` 后用 `db_client -W` 进入管理员会话：

```sql
\getenv cleanup_login CLEANUP_LOGIN
SELECT current_database() AS cleanup_database \gset
BEGIN;
ALTER ROLE :"cleanup_login" NOLOGIN;
REVOKE v5_worker FROM :"cleanup_login";
REVOKE CONNECT ON DATABASE :"cleanup_database" FROM :"cleanup_login";
COMMIT;
SELECT pid, datname, state FROM pg_stat_activity WHERE usename = :'cleanup_login';
SELECT dbid, classid::regclass, objid, deptype FROM pg_shdepend
 WHERE refclassid = 'pg_authid'::regclass AND refobjid = :'cleanup_login'::regrole;
```

`NOLOGIN` 防止新连接，但不会终止已有会话；`REVOKE CONNECT` 也不会撤销从 PUBLIC 获得的 CONNECT。先按维护流程处理已有会话，检查所有数据库的所有权和依赖。只有确认不再使用、无依赖后，才另行执行 `DROP ROLE :"cleanup_login";`；依赖导致拒绝时停止核查，不使用 `CASCADE`、`DROP OWNED` 或改迁移记录来强行删除。对初始化前就存在的账号不做自动回滚删除。

## 5. 验证范围与记录

| 验证环境 | 必测内容 |
|---|---|
| 现有安装器 Docker 桩 | 默认仅验与显式创建的参数、迁移/健康后及 sanity 前的调用顺序、失败中止、已有 `.env`/密钥保留、输出不含测试密码；桩不能证明真实账号或权限 |
| 隔离的真实 PostgreSQL 18 | 账号不存在与首次创建、正反密码、SET ROLE/函数授权、重复执行不变更、已有错误密码、缺成员/额外成员/危险属性、管理权限不足、CREATE 后授权失败回滚、提交后验证失败再跑、脏迁移/清单不符、PUBLIC 或直接表写权限、HBA trust 负例、客户端和服务端日志不含测试明文密码 |
| 目标部署现场 | 与运行镜像一致的迁移、实际 worker 网络/HBA/凭据、权限门禁；另在真实附件生命周期验收中观察清理 worker，无认证/权限告警且业务行为符合预期 |

回归入口为 `scripts/test/install_one_click_test.sh`；源码仓库中的真实数据库专项为 `scripts/test/cleanup_db_postgres_test.sh`，只在隔离测试实例运行。该专项使用合成迁移台账和函数验证身份/事务契约，不证明全量业务迁移或真实附件删除；独立交付包不包含这个开发测试。测试通过不能代替目标服务器验收；没有 Docker/PG、DBA、镜像或现场条件的项目记录 `BLOCKED` / `NOT_RUN`，不要预填 `PASS`。

## 源码依据

- 包内 `deploy/init-cleanup-db.sh`、`deploy/cleanup-db/bootstrap.sql`、`deploy/cleanup-db/verify.sql`；安装入口 `deploy/install.sh`。
- [清理 worker 的连接和角色检查](https://github.com/imboy-pub/imboy/blob/main/src/ds/attachment_direct_cleanup_worker.erl)、[cross rescan 调用](https://github.com/imboy-pub/imboy/blob/main/src/ds/attachment_cross_rescan.erl)、[迁移启动器](https://github.com/imboy-pub/imboy/blob/main/src/lib/imboy_migrate.erl)。审阅源码时切换至包内 `SOURCE_COMMIT` 对应提交。
