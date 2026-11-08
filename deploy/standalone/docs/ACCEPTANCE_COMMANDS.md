# 真实服务器验收命令与业务步骤 / Site acceptance

以下供客户在获准服务器上执行。**制包与离线检查不会执行这些 Docker、TLS、数据库或业务操作，现场记录的初始状态全部为 `NOT_RUN`。** 离线检查的证据范围另见 [验证说明](VALIDATION_RESULTS.md)。

基础检查尽量只读；Garage 开桶、续期演练、重复安装及客户端操作会产生现场变更，已分别说明。非零退出是待处理结果，不能追加 `|| true` 消除。

## 1. 在同一 Bash 终端准备命令环境

以下基于 `DATA_DIR=./data`、9800 端口，且不启用 Uptrace / enterprise-direct。`.env` 沿用模板单行无引号 `KEY=value` 格式。`env_value` 只读取字面值，不执行配置内容。

```bash
cd /opt/imboy-community/deploy
set -o pipefail
env_value() { awk -F= -v key="$1" '$1 == key {sub(/^[^=]*=/, ""); print; exit}' .env; }
IMBOY_ACCEPTANCE_ARGS=(--env-file .env -f docker-compose.community.yml)
case "$(env_value LIVEKIT_TURN_ENABLED)" in
  true|1) IMBOY_ACCEPTANCE_ARGS+=(-f docker-compose.livekit-turn.yml) ;;
  false|0|'') ;;
  *) printf '%s\n' '请核对 LIVEKIT_TURN_ENABLED，保持模板的字面布尔值。' >&2; exit 1 ;;
esac
dc() { docker compose "${IMBOY_ACCEPTANCE_ARGS[@]}" "$@"; }
IMBOY_ACCEPTANCE_PG_USER=$(env_value POSTGRES_USER)
IMBOY_ACCEPTANCE_PG_DB=$(env_value POSTGRES_DB)
IMBOY_ACCEPTANCE_DIR="../records/acceptance-$(date +%Y%m%d-%H%M%S)"
umask 077
mkdir -p "$IMBOY_ACCEPTANCE_DIR"
chmod 700 "$IMBOY_ACCEPTANCE_DIR"
```

每段执行后记录时间、退出码和证据位置。原始记录由客户保管，外发前脱敏；不记录 `.env`、私钥、完整配置展开或容器环境。`pipefail` 用于防止 `tee` 掩盖左侧失败。

## 2. 工具、实际 Compose 展开与全部核心服务

```bash
(
  set -euo pipefail
  docker version
  docker compose version
  bash --version | sed -n '1p'
  jq --version
  openssl version
  dc config -q
  dc ps -a
)
```

检查 9 个核心服务，补查原 sanity 未明确覆盖的 `imboy_widget` 和 `imboy_edge`：

```bash
(
  set -euo pipefail
  running=$(dc ps --status running --services)
  for service in imboy_pg18 garage imboy_backend imboy_admin imboy_widget \
    imboy_edge imboy_nginx imboy_certbot imboy_livekit; do
    if ! printf '%s\n' "$running" | grep -qx "$service"; then
      printf 'FAIL: %s 未运行\n' "$service" >&2
      exit 1
    fi
    cid=$(dc ps -q "$service")
    test -n "$cid"
    state=$(docker inspect --format \
      '{{.State.Status}}/{{if .State.Health}}{{.State.Health.Status}}{{else}}none{{end}}' "$cid")
    printf '%s %s\n' "$service" "$state"
    case "$state" in running/healthy|running/none) ;; *) exit 1 ;; esac
  done
) | tee "$IMBOY_ACCEPTANCE_DIR/core-services.txt"
```

`running/none` 表示该服务没有容器健康探针，不能填成“健康探针通过”。服务运行只是前置条件，继续检查业务路径。

## 3. 实际镜像身份

只读取镜像引用和摘要，不打印容器环境变量：

```bash
(
  set -euo pipefail
  for service in imboy_pg18 garage imboy_backend imboy_admin imboy_widget \
    imboy_edge imboy_nginx imboy_certbot imboy_livekit; do
    cid=$(dc ps -q "$service")
    test -n "$cid"
    image_id=$(docker inspect --format '{{.Image}}' "$cid")
    printf 'SERVICE=%s\n' "$service"
    docker inspect --format 'IMAGE_REF={{.Config.Image}} IMAGE_ID={{.Image}}' "$cid"
    docker image inspect --format 'REPO_DIGESTS={{json .RepoDigests}}' "$image_id"
  done
) | tee "$IMBOY_ACCEPTANCE_DIR/image-identity.txt"
```

与交付方发布清单逐一比对。RepoDigests 为空或镜像来源 / 构建身份不明时记录缺口，不把固定源码 SHA 直接当作运行镜像构建 SHA。

## 4. RSA 挂载、Nginx 配置与健康

```bash
(
  set -euo pipefail
  test "$(stat -c %a .env)" = 600
  test "$(stat -c %a data/backend_priv/keys/login_rsa_priv.pem)" = 600
  openssl pkey -in data/backend_priv/keys/login_rsa_priv.pem \
    -pubout -outform DER -out "$IMBOY_ACCEPTANCE_DIR/rsa-derived-public.der"
  openssl pkey -pubin -in data/backend_priv/keys/login_rsa_pub.pem \
    -outform DER -out "$IMBOY_ACCEPTANCE_DIR/rsa-declared-public.der"
  cmp "$IMBOY_ACCEPTANCE_DIR/rsa-derived-public.der" \
    "$IMBOY_ACCEPTANCE_DIR/rsa-declared-public.der"
  dc exec -T imboy_backend sh -c \
    'test -r "$IMBOY_LOGIN_RSA_PRIV_KEY_FILE" && test -r "$IMBOY_LOGIN_RSA_PUB_KEY_FILE"'
  dc exec -T imboy_nginx nginx -t
  curl -fsS --connect-timeout 5 --max-time 15 http://127.0.0.1:9800/healthz \
    | jq -e '.status == "ok"'
)
```

两个 `.der` 文件仅为公钥；私钥没有输出。通过要求：权限正确、公私钥匹配、容器能读取、Nginx 配置合法、健康返回 `status=ok`。

运行原 sanity，保留 WARN 明细。TURN 启用时加入同一 overlay：

```bash
case "$(env_value LIVEKIT_TURN_ENABLED)" in
  true|1)
    bash ../scripts/sanity_check.sh \
      -f docker-compose.community.yml -f docker-compose.livekit-turn.yml ;;
  *) bash ../scripts/sanity_check.sh -f docker-compose.community.yml ;;
esac
```

不传 `--skip-tls`。原 sanity 的 Nginx 探测忽略证书校验，迁移检查只看状态表是否有记录，必须继续下文检查。

## 5. 数据库与完整迁移检查

先验证 PG 就绪、扩展列表和迁移状态：

```bash
dc exec -T imboy_pg18 pg_isready \
  -U "$IMBOY_ACCEPTANCE_PG_USER" -d "$IMBOY_ACCEPTANCE_PG_DB"
dc exec -T imboy_pg18 psql -X -v ON_ERROR_STOP=1 \
  -U "$IMBOY_ACCEPTANCE_PG_USER" -d "$IMBOY_ACCEPTANCE_PG_DB" \
  -c 'SELECT extname, extversion FROM pg_extension ORDER BY extname;' \
  -c 'SELECT version, dirty, applied_at FROM public.schema_migrations;'
```

按发布镜像的扩展要求比对结果。`schema_migrations` **只有一行状态**，`schema_migrations_history` 才逐版本记录。不得把状态表行数与迁移文件数比较。

下面只读枚举运行镜像内迁移文件，并与 history / head 逐一比较，不需要完整源码：

```bash
(
  set -euo pipefail
  dc exec -T imboy_backend sh -c '
    set -eu
    found=0
    for f in /opt/imboy/lib/imboy-*/priv/migrations/*.up.sql; do
      [ -f "$f" ] || continue
      n=${f##*/}
      v=${n%%_*}
      case "$v" in ""|*[!0-9]*) exit 1 ;; esac
      printf "%s\n" "$v"
      found=1
    done
    [ "$found" = 1 ]
  ' | awk '{printf "%.0f\n", $0+0}' | sort -n \
    > "$IMBOY_ACCEPTANCE_DIR/migrations-image.txt"

  dc exec -T imboy_pg18 psql -X -v ON_ERROR_STOP=1 -At \
    -U "$IMBOY_ACCEPTANCE_PG_USER" -d "$IMBOY_ACCEPTANCE_PG_DB" \
    -c 'SELECT version FROM public.schema_migrations_history ORDER BY version;' \
    > "$IMBOY_ACCEPTANCE_DIR/migrations-applied.txt"

  test -s "$IMBOY_ACCEPTANCE_DIR/migrations-image.txt"
  cmp "$IMBOY_ACCEPTANCE_DIR/migrations-image.txt" \
    "$IMBOY_ACCEPTANCE_DIR/migrations-applied.txt"
  expected_head=$(tail -1 "$IMBOY_ACCEPTANCE_DIR/migrations-image.txt")
  actual_state=$(dc exec -T imboy_pg18 psql -X -v ON_ERROR_STOP=1 -At \
    -U "$IMBOY_ACCEPTANCE_PG_USER" -d "$IMBOY_ACCEPTANCE_PG_DB" \
    -c 'SELECT count(*) = 1 AND bool_and(NOT dirty), min(version)
        FROM public.schema_migrations;')
  test "$actual_state" = "t|$expected_head"
  printf 'migration history 与镜像一致；head=%s；dirty=false\n' "$expected_head"
)
```

自定义镜像若改变 release / `scripts_path`，须使用真实迁移目录并记录。迁移编号允许有空缺，不可按 `1..最高编号` 补齐或推断已执行。

额外复核启动代码要求的三个 C2C 排序守卫触发器，查询结果必须是 `3`：

```bash
dc exec -T imboy_pg18 psql -X -v ON_ERROR_STOP=1 -At \
  -U "$IMBOY_ACCEPTANCE_PG_USER" -d "$IMBOY_ACCEPTANCE_PG_DB" <<'SQL'
SELECT count(*) FROM pg_trigger
WHERE NOT tgisinternal
  AND tgname='trg_00_c2c_order_guard'
  AND tgenabled IN ('O','A')
  AND tgrelid IN ('public.msg_store_staging'::regclass,
                 'public.msg_c2c'::regclass,
                 'public.msg_store'::regclass)
  AND tgfoid=to_regprocedure('public.fn_c2c_order_write_guard()');
SQL
```

发现 dirty、漏版本或触发器缺失时保留现场，按版本恢复方案处理；不要手改 dirty、删除迁移记录或执行历史 down 来让检查通过。独立清理 LOGIN 的开通、最小授权及真实密码认证正反验收见 [DB_CLEANUP_ACCOUNT.md](DB_CLEANUP_ACCOUNT.md)。

## 6. Garage 桶与附件前置

先只读核对：

```bash
(
  set -euo pipefail
  private_bucket=$(env_value IMBOY_GARAGE_BUCKET)
  public_bucket=$(env_value IMBOY_GARAGE_PUBLIC_BUCKET)
  test -n "$private_bucket"
  test -n "$public_bucket"
  test "$private_bucket" != "$public_bucket"
  dc exec -T garage /garage status
  dc exec -T garage /garage bucket list
  dc exec -T garage /garage bucket info "$private_bucket"
  dc exec -T garage /garage bucket info "$public_bucket"
)
```

基础 Compose 的 `--default-bucket` 只初始化私有默认桶。若只读列表确认公开桶尚不存在，在获准安装窗口创建；`bucket info` 因连接、权限等其他原因失败时先处理错误，不把任意失败解释为缺桶：

```bash
# 仅在确认目标公开桶缺失时执行此块。
(
  set -euo pipefail
  private_bucket=$(env_value IMBOY_GARAGE_BUCKET)
  public_bucket=$(env_value IMBOY_GARAGE_PUBLIC_BUCKET)
  test -n "$private_bucket"
  test -n "$public_bucket"
  test "$private_bucket" != "$public_bucket"
  dc exec -T garage /garage bucket create "$public_bucket"
)
```

目标桶存在后，用现有服务 key 授权，不创建新 key、不改变存储密钥：

```bash
(
  set -euo pipefail
  private_bucket=$(env_value IMBOY_GARAGE_BUCKET)
  public_bucket=$(env_value IMBOY_GARAGE_PUBLIC_BUCKET)
  access_key=$(env_value IMBOY_GARAGE_ACCESS_KEY)
  test -n "$private_bucket"
  test -n "$public_bucket"
  test -n "$access_key"
  test "$private_bucket" != "$public_bucket"
  dc exec -T garage /garage bucket allow "$public_bucket" \
    --read --write --owner --key "$access_key"
  dc exec -T garage /garage bucket info "$public_bucket"
)
```

本包已透传公开桶变量，但开桶和授权不等于公开资源业务验收完成。社区 Compose 没有单独的 S3 Website 服务，不盲目照搬其他部署脚本的 `bucket website --allow`。实际头像 / 公开图片由产品现有路径验收，私有附件另测。

## 7. 证书与公网入口

服务器上检查五域证书存在、未过期、主机名匹配，且没有临时自签标记：

```bash
(
  set -euo pipefail
  for key in API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN RTC_DOMAIN TURN_DOMAIN; do
    domain=$(env_value "$key")
    cert_dir="data/certbot/conf/live/$domain"
    test -n "$domain"
    test -s "$cert_dir/fullchain.pem"
    test -s "$cert_dir/privkey.pem"
    test ! -e "$cert_dir/.imboy-selfsigned"
    openssl x509 -in "$cert_dir/fullchain.pem" -noout -checkend 0
    openssl x509 -in "$cert_dir/fullchain.pem" -noout -checkhost "$domain" \
      | grep -Fx "Hostname $domain does match certificate"
    openssl x509 -in "$cert_dir/fullchain.pem" -noout -issuer -subject -dates
  done
)
```

这没有读取私钥内容，但本地文件检查不证明公网信任。下面先在服务器执行，再从**客户实际网络**的验收终端使用相同域名复测。外部终端只填非敏感域名，不复制 `.env`，并具备相应 curl / jq / OpenSSL 工具：

```bash
(
  set -euo pipefail
  api_domain=$(env_value API_DOMAIN)
  admin_domain=$(env_value ADMIN_DOMAIN)
  widget_domain=$(env_value CS_WIDGET_DOMAIN)
  rtc_domain=$(env_value RTC_DOMAIN)
  curl -fsS --connect-timeout 5 --max-time 15 "https://$api_domain/healthz" \
    | jq -e '.status == "ok"'
  curl -fsS --connect-timeout 5 --max-time 15 -o /dev/null "https://$admin_domain/"
  curl -fsS --connect-timeout 5 --max-time 15 -o /dev/null "https://$widget_domain/v1/loader.js"
  for domain in "$api_domain" "$admin_domain" "$widget_domain" "$rtc_domain"; do
    timeout 15 openssl s_client -connect "$domain:443" -servername "$domain" \
      -verify_hostname "$domain" -verify_return_error -brief < /dev/null
  done
)
```

不得加入 `-k` 或关闭主机名校验。TURN 默认关闭时不检查 TURN 域 443；启用后从客户网络验证 5349：

```bash
turn_domain=$(env_value TURN_DOMAIN)
timeout 15 openssl s_client -connect "$turn_domain:5349" -servername "$turn_domain" \
  -verify_hostname "$turn_domain" -verify_return_error -brief < /dev/null
```

TLS 握手成功仍不证明 LiveKit 信令鉴权、媒体直连或 TURN relay，继续双端业务验收。

获准后可单独执行 ACME 续期 dry-run：

```bash
dc run --rm --entrypoint certbot imboy_certbot renew --dry-run
```

它会连接真实 ACME 测试服务，并可能写入 certbot 日志。此步骤验证续期请求路径，不代表真实证书换发后的 Nginx / TURN 加载已演练完成；实际换证加载另留验收项。

## 8. 管理员、登录、WebSocket 消息与附件

这些步骤依赖匹配的真实客户端、两个验收账号和客户网络。Shell 基础设施命令不能代替端到端业务。缺少条件时记录 `BLOCKED`，不使用开发验证码、直接 RPC 造 token 或 `scripts/smoke_8step.sh`。

| ID | 操作 | 通过要求 |
|---|---|---|
| B01 | 浏览器进入 Admin `/setup`，按首次向导建管理员，再实际登录 | 成功进入后台，记录账号代号、时间和客户端版本；密码不进入证据 |
| B02 | 已初始化后再次访问 `/setup` | 不能再次执行首次管理员初始化 |
| B03 | 账号 A、B 分别在两个客户端按正式流程登录 | 地址正确、证书受信任、正常鉴权；若开户依赖短信 / SMTP，真实投递另测 |
| B04–05 | A 向 B 发唯一标记文本，B 回复另一条文本 | 双向收发与回执符合产品行为，记录两端时间和消息标记 |
| B06 | B 断网，A 发消息，B 恢复网络 | B 正常重连并收到应收消息，无重复展示或不可恢复错误 |
| B07 | A 发送测试文件，B 下载 | 原件与下载件内容一致，成功不只看上传 HTTP 状态 |
| B08 | 发送图片并在 B 显示 / 下载，按范围更新头像或群图 | 图片内容实际可用，公开资源与私有附件分别记录 |
| B10–12 | 如交付 Widget / 一对一 RTC / TURN，执行访客对话、双向音视频、受限网络中继 | 记录真实业务与实际中继路径，不用容器运行或端口可达替代 |

在发送端用 Shell 生成不含业务信息的小文件：

```bash
printf 'IMBoy customer delivery round-trip\n' > imboy-delivery-check.txt
sha256sum imboy-delivery-check.txt
```

经客户端传输后，下载端执行 `sha256sum` 比对摘要。若两文件已放在同一验收目录，可执行：

```bash
cmp original/imboy-delivery-check.txt downloaded/imboy-delivery-check.txt
```

返回 0 才表示逐字节相同。macOS 可用 `shasum -a 256` 取摘要。另选客户允许的无敏感图片测试显示与公开资源，不附带真实客户文件。

## 9. 重复安装保护现场复核

在已完成基础验收、已备份的**独立演练实例**执行。标准演练保持 TURN 关闭，避免把自身 TURN 端口占用混入此项。以下会再次执行安装流程：

```bash
(
  set -euo pipefail
  sha256sum .env data/backend_priv/keys/login_rsa_priv.pem \
    data/backend_priv/keys/login_rsa_pub.pem > "$IMBOY_ACCEPTANCE_DIR/reinstall-before.sha256"
  printf 'delivery-data-preservation\n' > data/delivery-protection-check.txt
  cp data/delivery-protection-check.txt "$IMBOY_ACCEPTANCE_DIR/data-marker.txt"
  bash ../install.sh
  sha256sum .env data/backend_priv/keys/login_rsa_priv.pem \
    data/backend_priv/keys/login_rsa_pub.pem > "$IMBOY_ACCEPTANCE_DIR/reinstall-after.sha256"
  cmp "$IMBOY_ACCEPTANCE_DIR/reinstall-before.sha256" \
    "$IMBOY_ACCEPTANCE_DIR/reinstall-after.sha256"
  cmp data/delivery-protection-check.txt "$IMBOY_ACCEPTANCE_DIR/data-marker.txt"
)
```

再验证原账号登录、已有消息和旧附件，才能确认业务数据仍可用。配置丢失但数据存在、并发入口拒绝等负向场景只在独立副本测试，不从客户运行实例移走 `.env` 或私钥。

## 10. 填写结果与来源

将实际命令、退出码、环境、时间和证据位置写入 [ACCEPTANCE_TEMPLATE.md](ACCEPTANCE_TEMPLATE.md)。只读查询结果也须与预期对照才可判定 PASS。未执行项保持 `NOT_RUN`；缺条件则写 `BLOCKED`、原因及负责人。

源码定位入口如下；审阅时切换至包内 `SOURCE_COMMIT` 对应提交，运行镜像按实际 digest 另核对：

- [社区 Compose](https://github.com/imboy-pub/imboy/blob/main/deploy/docker-compose.community.yml)、[原 sanity](https://github.com/imboy-pub/imboy/blob/main/scripts/sanity_check.sh)。
- [迁移启动与守卫](https://github.com/imboy-pub/imboy/blob/main/src/lib/imboy_migrate.erl)、[迁移状态 / 历史表合同](https://github.com/imboy-pub/imboy/blob/main/scripts/customer_service_migration_gate.sh)。
- [Garage 既有开桶与授权步骤](https://github.com/imboy-pub/imboy/blob/main/scripts/garage-install.sh)。
