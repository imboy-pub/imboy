# 升级、重复安装与恢复 / Upgrade and recovery

本手册用于已经完成首次验收的社区单机实例。默认 `DATA_DIR=./data`、`BACKEND_PORT=9800`。升级和恢复需要客户维护窗口；**制包和离线回归不执行真实升级、迁移或恢复，相关结果须单独演练记录**。

升级前必须取得目标版本的发布说明、镜像身份和数据库兼容性说明。没有这些材料时记录 `BLOCKED`，不要只把 `.env` 的版本号改大后尝试启动。

## 1. 重跑安装器的含义

在原目录、原 `.env` 和原数据上重跑，会继续使用已有非占位内部密钥、RSA 和证书，并重新检查 / 确保服务启动。它不是恢复备份、重置数据库、重新开通清理 LOGIN 或升级 PostgreSQL 的命令。

出现下列情况先停止，恢复原始材料后再判断下一步：

- 数据还在，`.env` 却丢失，或其中关键密钥为空 / 变回 `CHANGE_ME`。
- RSA 私钥、Garage RPC secret、Garage access key / secret、密码盐与原实例不一致。
- 解压到新目录后 `DATA_DIR=./data` 指向了一套空数据。
- 已有数据库账号被改名、密码仅在 `.env` 中更改而数据库端未同步。

根入口会检查重复安装保护条件并持有并发锁；不要绕过入口直接运行内部脚本来跳过这些检查。并发锁只保护使用本包入口的安装操作，不能约束另一个终端直接调用 Docker。维护期间安排单一操作者。

## 2. 使用与当前实例相同的 Compose 组合

以下命令在同一 Bash 终端中执行，先定义 `dc`。标准路径不开启 Uptrace 或 enterprise-direct；TURN 已启用时追加同一 overlay：

```bash
cd /opt/imboy-community/deploy
IMBOY_MAINTENANCE_ARGS=(--env-file .env -f docker-compose.community.yml)
case "$(awk -F= '$1 == "LIVEKIT_TURN_ENABLED" {print $2; exit}' .env)" in
  true|1) IMBOY_MAINTENANCE_ARGS+=(-f docker-compose.livekit-turn.yml) ;;
  false|0|'') ;;
  *) printf '%s\n' '请核对 LIVEKIT_TURN_ENABLED，保持模板的字面布尔值。' >&2; exit 1 ;;
esac
dc() { docker compose "${IMBOY_MAINTENANCE_ARGS[@]}" "$@"; }
dc config -q
dc ps
```

只使用 `config -q` 或 `config --images`。完整 `config` 和未过滤的 `docker inspect` 可能展开密钥，不应贴到工单或验收记录。

## 3. 升级前记录身份与固定 PG 镜像

记录当前四类 IMBoy 镜像以及其他依赖的实际 digest，方法见 [现场验收命令](ACCEPTANCE_COMMANDS.md)。保存当前 `.env` 和包文件清单。

`IMBOY_VERSION` 同时影响 backend、admin、widget 和 pg18 的默认镜像引用。应用升级前，在 `.env` 中显式填写当前已验证的 `PG_IMAGE=<当前 PostgreSQL 镜像的仓库@sha256 摘要>`，防止随后整栈操作误换 PostgreSQL 镜像。此处只能使用当前运行镜像真实返回的摘要，不能照抄一个示例值。

若 `BACKEND_IMAGE`、`ADMIN_IMAGE`、`IMBOY_WIDGET_IMAGE` 已显式设置，它们会覆盖 `IMBOY_VERSION` 的默认引用，升级时须按发布清单一起更新。edge 复用 backend 镜像，不能漏掉。

PostgreSQL 主版本或扩展版本升级需要独立计划与恢复演练，不属于下面的应用升级命令。

## 4. 在维护窗口制作可恢复检查点

先阻止客户业务写入并通知测试人员停止操作。下面是本包默认单机路径的备份命令，需使用有权读取数据目录、写入备份盘和访问 Docker 的账号。把备份目录改为客户实际备份盘；不要放进将被替换的交付目录中。

```bash
IMBOY_BACKUP_DIR="/var/backups/imboy/$(date +%Y%m%d-%H%M%S)"
(
  set -euo pipefail
  umask 077
  mkdir -p "$IMBOY_BACKUP_DIR"
  chmod 700 "$IMBOY_BACKUP_DIR"

  # 暂停入口和业务写入；PG / Garage 此时保留，供逻辑备份使用。
  dc stop imboy_nginx imboy_livekit imboy_backend imboy_edge \
    imboy_admin imboy_widget imboy_certbot

  pg_user=$(awk -F= '$1 == "POSTGRES_USER" {print $2; exit}' .env)
  pg_db=$(awk -F= '$1 == "POSTGRES_DB" {print $2; exit}' .env)
  test -n "$pg_user"
  test -n "$pg_db"
  dc exec -T imboy_pg18 pg_dump -U "$pg_user" -d "$pg_db" -Fc \
    > "$IMBOY_BACKUP_DIR/database.dump"
  dc exec -T imboy_pg18 pg_dumpall -U "$pg_user" --globals-only \
    > "$IMBOY_BACKUP_DIR/database-globals.sql"

  # Garage 的 metadata 与对象数据必须一起备份，复制时保持停止。
  dc stop garage
  tar --numeric-owner -cpf "$IMBOY_BACKUP_DIR/garage.tar" data/garage
  tar --numeric-owner -cpf "$IMBOY_BACKUP_DIR/config-and-certificates.tar" \
    .env data/backend_priv data/certbot/conf

  # 这里只证明备份格式可读取，不等于完成恢复演练。
  dc exec -T imboy_pg18 pg_restore -l \
    < "$IMBOY_BACKUP_DIR/database.dump" \
    > "$IMBOY_BACKUP_DIR/database.contents.txt"
  tar -tf "$IMBOY_BACKUP_DIR/garage.tar" > "$IMBOY_BACKUP_DIR/garage.contents.txt"
  tar -tf "$IMBOY_BACKUP_DIR/config-and-certificates.tar" \
    > "$IMBOY_BACKUP_DIR/config.contents.txt"
  (
    cd "$IMBOY_BACKUP_DIR"
    sha256sum database.dump database-globals.sql garage.tar config-and-certificates.tar \
      > SHA256SUMS
  )
  printf '备份目录：%s\n' "$IMBOY_BACKUP_DIR"
)
```

这一步结束时业务服务和 Garage 仍处于停止状态，便于继续升级。若中途失败，保持维护窗口，核对当前服务状态和备份完整性，不能假定已自动恢复。

数据库全局角色文件可能含密码哈希；配置和证书归档包含真实密钥。限制访问、使用客户认可的加密存储和传输方式，不把这些备份放回交付 ZIP。若本次启用了监控，监控命名卷的备份与保留要求另行记录；上述业务备份命令没有备份监控历史。

要证明备份能恢复，必须在独立环境完成数据库、角色、Garage、密钥与应用的一致恢复，再做登录、消息、附件验收。仅检查文件存在、文件大小或能列目录，不可填写“恢复验证通过”。

## 5. 更新配置并定向升级应用

在原安装目录编辑 `.env`，保留所有客户密钥、数据路径和当前 PG 摘要，只按已确认发布清单更新目标应用镜像。不要重新复制 `.env.example`。

```bash
nano .env
dc config -q
dc config --images
```

比对显示的镜像清单与发布清单；输出不一致时停止。随后执行：

```bash
(
  set -euo pipefail
  dc pull imboy_backend imboy_admin imboy_widget imboy_edge
  # 新镜像以 UID/GID 10001:10001 运行；保留既有私钥所有权并准备最小 ACL。
  bash prepare-backend-runtime.sh "${IMBOY_MAINTENANCE_ARGS[@]}"
  dc start garage imboy_certbot imboy_livekit
  # --no-deps 防止本次定向应用升级顺带重建 PostgreSQL 等依赖。
  dc up -d --no-deps imboy_backend imboy_admin imboy_widget imboy_edge
  dc start imboy_nginx
  dc ps
)
```

这段命令适用于本次只更换应用镜像、Compose / Nginx 等静态配置未改变的升级。若新交付包修改了这些静态配置，应先在新包暂存目录执行完整性检查和差异评审，再按该版本专门的升级说明替换静态文件 / 重建受影响服务。不能直接在新的解压目录启动第二套同名实例，也不能覆盖原 `.env`、`data/` 或验收记录。

目标 backend 在启动阶段执行数据库迁移。`up -d` 返回并不代表迁移完成，必须执行下一节检查。

新后端镜像必须配套新版 `prepare-backend-runtime.sh`。它保留私钥内容、宿主所有权
和只读挂载，为 UID 10001 授予读取私钥及写入既有日志所需的 ACL；失败时不能继续
`up`。自定义挂载、符号链接、非标准 UID 或不支持 POSIX ACL 的文件系统须单独迁移。
The new non-root image requires the matching runtime-permission helper. Run it with
the same Compose options after pulling and before starting containers. It preserves
secret contents and ownership while granting the runtime UID narrowly scoped access;
do not continue after a failure or assume custom mounts have been validated.

## 6. 升级后验收

按 [ACCEPTANCE_COMMANDS.md](ACCEPTANCE_COMMANDS.md) 重新验证：

- 全部核心服务、适用容器健康、API `/healthz`、Nginx 实际配置。
- backend / admin / widget / edge 实际镜像身份对齐，PG 仍为升级前固定摘要。
- `schema_migrations` 为单行，`dirty=false`，head 等于运行镜像的迁移最高版本，history 与运行镜像内迁移文件集合一致；不以行数大于 0 代替迁移完整性。
- 独立清理 LOGIN 和所需成员资格仍有效。
- 原管理员和普通账号可以登录，旧消息 / 联系关系可用，新消息双向可达，旧附件和新附件均可读取。
- 若交付包含 Widget、RTC / TURN、SMTP / 短信、监控，复验对应路径。

完成后再解除维护窗口，更新验收记录。发布说明没有列明的未验证功能保留为 `NOT_RUN`。

## 7. 回退与恢复边界

| 当前情况 | 处理原则 |
|---|---|
| 新镜像尚未运行，未执行迁移 | 恢复原应用镜像引用，核对原配置与数据，再启动旧版本并复验 |
| 新镜像已经运行，迁移已成功 | 只有发布说明明确旧 binary 与新 schema 兼容时，才可以只回退镜像 |
| 迁移失败或 `dirty=true` | 停止业务写入，保留日志与数据库现场，由 DBA 按该迁移的恢复方案处理 |
| schema 与旧 binary 不兼容 | 使用经过验证的一致恢复检查点，或针对当前版本修复；不能单独降低镜像版本 |
| 原 `.env` / RSA / Garage 密钥丢失 | 先从受控备份恢复匹配材料；随机生成新值不等于恢复 |

数据库回退必须同时考虑业务数据和对象存储的一致时间点。恢复旧数据库但保留较新对象数据、或恢复 Garage 却使用另一套密钥，不能被视为已恢复。

本节普通 `tar --numeric-owner -cpf` 备份不保存 POSIX ACL。从归档恢复 `backend_priv`
后，先确认目标镜像与恢复的数据、配置兼容。启动目标非 root 镜像的任何 `up` 之前，
用第 2 节相同的 Compose 参数拉取镜像并重新准备权限：

```bash
(
  set -euo pipefail
  dc pull imboy_backend
  bash prepare-backend-runtime.sh "${IMBOY_MAINTENANCE_ARGS[@]}"
)
```

权限准备失败时保持业务停止，先处理路径或 ACL 问题；保留恢复的密钥字节与所有者。
The plain `tar --numeric-owner -cpf` backup above does not preserve POSIX ACLs.
After restoring `backend_priv`, confirm the target image is compatible with the
restored data and configuration. Before any `up` of a non-root image, use the
same Compose arguments from section 2 to pull that image and rerun the helper,
as shown above. Keep services stopped if preparation fails, preserving restored
secret contents and ownership.

保留失败现场，在独立数据目录 / 实例中验证备份恢复后，再按客户批准的切换方案恢复服务。恢复演练需要记录可接受的数据丢失窗口、实际耗时和客户端复验结果。本包不提供清空数据库、删除数据目录或强行执行历史 `down.sql` 的通用命令。

仓库部署文档记录了破坏性迁移的 binary / DB 兼容禁区；该风险不能由“有备份”或“只改版本号”消除。依据见 [部署文档的升级章节](https://github.com/imboy-pub/imboy/blob/main/deploy/README.md#升级)，并核对本次目标版本的发布说明及 `SOURCE_COMMIT` 对应内容。
