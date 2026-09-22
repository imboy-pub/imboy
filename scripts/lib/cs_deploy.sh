#!/usr/bin/env bash
# =============================================================================
# cs_deploy.sh — 托管客服 Widget 一键部署 helper（CSD-CLI-01）
#
# 被 scripts/imboy-deploy.sh source 后提供 deploy_cs；也可被单元测试单独
# source（纯函数与副作用命令通过可覆盖的函数名变量解耦，见下方 wrapper）。
#
# 固定步骤顺序（冻结合同 hosted-widget-contract-v1 §S7）：
#   PRECHECK → BUILD_AND_VERIFY_WIDGET → STAGE_WIDGET_RELEASE
#   → VALIDATE_CS_VHOST_AND_TLS → DEPLOY_BACKEND_BLUE_GREEN
#   → ATOMIC_ACTIVATE_WIDGET_AND_VHOST → REAL_SMOKE
#   → FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY
#
# 不变量：
#   I1 Backend 先成功再激活 Widget（复用 deploy_api，零第二套蓝绿）。
#   I2 升级失败只恢复旧 symlink/vhost，不动已成功 Backend。
#   I3 首次安装失败清除未激活 vhost/symlink/暂存文件，无半配置。
#   I4 smoke 失败恢复旧 symlink/vhost；新 release 目录保留。
#   I5 远端写入前时间戳备份 + checksum + 恢复记录；不触碰未知文件。
#   I6 全部输入在 SSH 前 allowlist 校验；远端 realpath 须位于批准根
#      /www/wwwroot/ 内且含 .imboy-cs-root marker。
#   I7 verbose 输出零 secret/token/证书私钥/完整敏感配置/联系方式。
#
# 远端命令自描述约定：每条副作用远端命令以 `: imboy-cs-<tag>` 开头
# （shell no-op 携带标签），便于审计日志与离线 fake harness 识别。
# =============================================================================

# ---------- 可注入副作用 wrapper（测试以同名变量注入 fake） ----------
: "${CS_SSH_EXEC_FN:=ssh_exec}"       # 执行远端命令
: "${CS_SSH_CAP_FN:=ssh_cap}"         # 执行远端命令并捕获 stdout
: "${CS_SCP_FN:=scp}"                 # 上传单个文件
: "${CS_RSYNC_FN:=rsync}"             # 上传 release 目录
: "${CS_BUN_FN:=bun}"                 # 本地构建 widget
: "${CS_DEPLOY_API_FN:=deploy_api}"   # 复用既有蓝绿实现（禁止第二套）

cs_ssh_exec()    { "$CS_SSH_EXEC_FN" "$@"; }
cs_ssh_cap()     { "$CS_SSH_CAP_FN" "$@"; }
cs_scp()         { "$CS_SCP_FN" "$@"; }
cs_rsync()       { "$CS_RSYNC_FN" "$@"; }
cs_bun()         { "$CS_BUN_FN" "$@"; }
cs_call_deploy_api() { "$CS_DEPLOY_API_FN"; }

# 独立 source（单元测试）时的最小日志兜底；主脚本随后的同名定义会覆盖它们。
if ! declare -F log >/dev/null 2>&1; then log() { printf '%s\n' "$*"; }; fi
if ! declare -F ok >/dev/null 2>&1; then ok() { printf 'OK %s\n' "$*"; }; fi
if ! declare -F warn >/dev/null 2>&1; then warn() { printf 'WARN %s\n' "$*" >&2; }; fi
if ! declare -F fail >/dev/null 2>&1; then fail() { printf 'FAIL %s\n' "$*" >&2; exit 1; }; fi

# =============================================================================
# 纯函数：输入 allowlist（I6）与脱敏（I7）
# =============================================================================

# FQDN（RFC 风格：字母数字与连字符的标签，至少两段，TLD 字母开头）
cs_valid_domain() {
  local d="$1"
  [[ -n "$d" && ${#d} -le 253 ]] || return 1
  [[ "$d" =~ ^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)+$ ]] || return 1
  return 0
}

# origin（scheme+host[:port]，无 path/userinfo/query；scheme 仅 http/https）
cs_valid_origin() {
  local o="$1"
  [[ "$o" =~ ^https?://[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)+(:[0-9]{1,5})?$ ]] || return 1
  return 0
}

# 安全绝对路径：受限字符集、非根、无 .. 段（拒绝穿越/空白/命令替换/分号等）
cs_safe_abs_path() {
  local p="$1"
  [[ "$p" =~ ^/[a-zA-Z0-9._/-]+$ ]] || return 1
  [[ "$p" != "/" && "$p" != *..* ]] || return 1
  return 0
}

# CS 远端根的批准根成员校验（marker 本身由 PRECHECK 在远端验证）
cs_remote_root_in_approved_scope() {
  local p="$1"
  cs_safe_abs_path "$p" || return 1
  [[ "$p" == /www/wwwroot/* && "$p" != "/www/wwwroot" && "$p" != "/www/wwwroot/" ]] || return 1
  return 0
}

# 安全版本号（与主脚本 DEPLOY_VSN allowlist 同式）
cs_valid_version() {
  [[ "$1" =~ ^[a-zA-Z0-9._-]+$ ]]
}

# 脱敏：只暴露前两字符 + 占位符，空值显式标注（I7；测试断言密值不出现）
cs_redact() {
  local v="${1:-}"
  if [[ -z "$v" ]]; then
    printf '(unset)'
    return 0
  fi
  if [[ ${#v} -le 4 ]]; then
    printf '***'
  else
    printf '%s***' "${v:0:2}"
  fi
}

# 合同 §S7 的固定步骤序列（每行一个，供编排自检与测试比对）
cs_expected_steps() {
  printf '%s\n' \
    PRECHECK \
    BUILD_AND_VERIFY_WIDGET \
    STAGE_WIDGET_RELEASE \
    VALIDATE_CS_VHOST_AND_TLS \
    DEPLOY_BACKEND_BLUE_GREEN \
    ATOMIC_ACTIVATE_WIDGET_AND_VHOST \
    REAL_SMOKE \
    FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY
}

# =============================================================================
# 纯函数：远端命令构造（全部输入均已过 allowlist；单引号包裹防拼接）
# =============================================================================

# PRECHECK：realpath/批准根/marker/current symlink/vhost 存在性（只读）
cs_remote_precheck_script() {
  cat <<EOF
: imboy-cs-check-precheck
set -eu
REAL=\$(realpath -e '$CS_REMOTE_ROOT') || exit 2
case "\$REAL" in /www/wwwroot/*) ;; *) exit 3 ;; esac
[ -f "\$REAL/.imboy-cs-root" ] || exit 4
printf 'ROOT=%s\n' "\$REAL"
C=\$(readlink '$CS_REMOTE_ROOT/current' 2>/dev/null || true)
printf 'CURRENT=%s\n' "\$C"
if [ -f '$CS_NGINX_CONF' ]; then printf 'VHOST_EXISTS=1\n'; else printf 'VHOST_EXISTS=0\n'; fi
EOF
}

# 创建远端不可变 release 目录
cs_remote_prepare_release_cmd() {
  printf ": imboy-cs-op-prepare-release\nmkdir -p '%s' || exit 1\n" "$CS_NEW_RELEASE_DIR"
}

# release 完整性：manifest.sha256 ↔ manifest.json 校验和比对 + 关键产物存在
cs_remote_verify_release_script() {
  cat <<EOF
: imboy-cs-op-verify-release
set -eu
cd '$CS_NEW_RELEASE_DIR' || exit 5
[ -s manifest.json ] || exit 6
[ -s manifest.sha256 ] || exit 7
printf '%s  manifest.json\n' "\$(tr -d '[:space:]' < manifest.sha256)" | sha256sum -c - >/dev/null || exit 8
[ -s loader.js ] || exit 9
[ -s widget/index.html ] || exit 10
[ -n "\$(ls -A assets 2>/dev/null)" ] || exit 11
EOF
}

# 证书存在/非空/配对 + staged vhost 独立 nginx -t（只读，不触碰线上配置）
# $1 = 远端 staged vhost 路径
cs_remote_tls_validate_script() {
  local staged="$1"
  cat <<EOF
: imboy-cs-check-tls-vhost
set -eu
[ -s '$CS_CERT_FULLCHAIN' ] || exit 10
[ -s '$CS_CERT_KEY' ] || exit 11
if command -v openssl >/dev/null 2>&1; then
  A=\$(openssl x509 -in '$CS_CERT_FULLCHAIN' -noout -pubkey 2>/dev/null | openssl sha256) || exit 12
  B=\$(openssl pkey -in '$CS_CERT_KEY' -pubout 2>/dev/null | openssl sha256) || exit 13
  [ "\$A" = "\$B" ] || exit 14
fi
TEST_MAIN=\$(mktemp /tmp/imboy-cs-nginx.XXXXXX.conf)
trap 'rm -f "\$TEST_MAIN"' EXIT
printf 'events {}\nhttp {\n    include %s;\n}\n' '$staged' > "\$TEST_MAIN"
nginx -t -c "\$TEST_MAIN" >/dev/null 2>&1 || exit 15
EOF
}

# 从 NGINX_CONF 发现当前蓝绿活动 upstream 端口；无唯一活动端口时输出空
cs_remote_discover_upstream_script() {
  cat <<EOF
: imboy-cs-op-discover-upstream
B=\$(awk '/^[[:space:]]*server[[:space:]]+127\\.0\\.0\\.1:$DEPLOY_BLUE_PORT;/{n++} END{print n+0}' '$NGINX_CONF' 2>/dev/null || echo 0)
G=\$(awk '/^[[:space:]]*server[[:space:]]+127\\.0\\.0\\.1:$DEPLOY_GREEN_PORT;/{n++} END{print n+0}' '$NGINX_CONF' 2>/dev/null || echo 0)
if [ "\$B" = 1 ] && [ "\$G" != 1 ]; then printf 'UPSTREAM=%s\n' '$DEPLOY_BLUE_PORT'
elif [ "\$G" = 1 ] && [ "\$B" != 1 ]; then printf 'UPSTREAM=%s\n' '$DEPLOY_GREEN_PORT'
else printf 'UPSTREAM=\n'
fi
EOF
}

# 旧 vhost 时间戳备份 + checksum 记录（I5）；输出 BAK=<路径>（无旧文件则空）
cs_remote_backup_vhost_script() {
  cat <<EOF
: imboy-cs-op-backup-vhost
CONF='$CS_NGINX_CONF'
BAK="\$CONF.cs-bak-$CS_RELEASE_STAMP"
if [ -f "\$CONF" ]; then
  cp -p "\$CONF" "\$BAK" || exit 12
  sha256sum "\$CONF" > "\$BAK.sha256" || exit 13
  printf 'BAK=%s\n' "\$BAK"
else
  printf 'BAK=\n'
fi
EOF
}

# 记录旧 symlink 指向（I5 恢复命令记录）
cs_remote_record_prev_cmd() {
  printf ": imboy-cs-op-record-prev\nprintf '%%s\\n' '%s' > '%s/.imboy-cs-prev-%s' || exit 14\n" \
    "$CS_OLD_SYMLINK" "$CS_ROOT_REAL" "$CS_RELEASE_STAMP"
}

# 原子 symlink 切换：临时名 + mv -T（$1=新 release 目录 $2=current 链接路径）
cs_remote_swap_symlink_cmd() {
  local target="$1" link="$2"
  printf ": imboy-cs-op-swap-symlink\nln -sfn '%s' '%s.cs-new' || exit 16\nmv -T '%s.cs-new' '%s' || exit 17\n" \
    "$target" "$link" "$link" "$link"
}

# 原子 vhost 替换（$1=远端暂存 final 路径 $2=线上 vhost 路径）
cs_remote_swap_vhost_cmd() {
  printf ": imboy-cs-op-swap-vhost\nmv -T '%s' '%s' || exit 18\n" "$1" "$2"
}

# nginx -t 与 reload 分离，失败码可区分
cs_remote_nginx_reload_cmd() {
  printf ": imboy-cs-op-nginx-reload\nnginx -t || exit 6\nnginx -s reload || exit 19\n"
}

# REAL_SMOKE：宿主页 / loader / 未知 /w/ id（统一 404，合同 S3）/ manifest / health
cs_remote_smoke_script() {
  cat <<EOF
: imboy-cs-check-smoke
SHOP='$CS_SMOKE_SHOP_ORIGIN'
CS_ORIGIN='https://$CS_WIDGET_DOMAIN'
curl -fsS -o /dev/null --max-time 10 "\$SHOP/" || exit 30
curl -fsS -o /dev/null --max-time 10 "\$CS_ORIGIN/v1/loader.js" || exit 31
W=\$(curl -sS -o /dev/null -w '%{http_code}' --max-time 10 "\$CS_ORIGIN/w/0" || true)
[ "\$W" = "404" ] || exit 32
curl -fsS -o /dev/null --max-time 10 "\$CS_ORIGIN/manifest.json" || exit 33
curl -fsS -o /dev/null --max-time 10 "\$CS_ORIGIN/health.txt" || exit 34
printf 'SMOKE=OK\n'
EOF
}

# 成功收尾：activations.log 仅记录版本/目录（无敏感值），并清理暂存 vhost
cs_remote_finalize_cmd() {
  printf ": imboy-cs-op-finalize\necho 'stamp=%s vsn=%s release=%s' >> '%s/activations.log' || exit 1\nrm -f '%s'.cs-staged-* '%s'.cs-final-* 2>/dev/null || true\n" \
    "$CS_RELEASE_STAMP" "$DEPLOY_VSN" "${CS_NEW_RELEASE_DIR##*/}" "$CS_ROOT_REAL" \
    "$CS_NGINX_CONF" "$CS_NGINX_CONF"
}

# 回滚：恢复旧 symlink（旧值空 = 首次安装，直接移除，I3）
cs_remote_restore_symlink_cmd() {
  printf ": imboy-cs-op-restore-symlink\nif [ -n '%s' ]; then\n  ln -sfn '%s' '%s.cs-restore' && mv -T '%s.cs-restore' '%s' || exit 20\nelse\n  rm -f '%s' || exit 20\nfi\n" \
    "$1" "$1" "$2" "$2" "$2" "$2"
}

# 回滚：恢复旧 vhost（备份空 = 首次安装，直接移除，I3）
cs_remote_restore_vhost_cmd() {
  printf ": imboy-cs-op-restore-vhost\nif [ -n '%s' ]; then\n  cp -p '%s' '%s.cs-restore' && mv -T '%s.cs-restore' '%s' || exit 21\nelse\n  rm -f '%s' || exit 21\nfi\n" \
    "$1" "$1" "$2" "$2" "$2" "$2"
}

# 回滚：清理未激活暂存 vhost 与未完成的 symlink 临时名（I3 无半配置残留）
cs_remote_clean_staged_cmd() {
  printf ": imboy-cs-op-clean-staged\nrm -f '%s'.cs-staged-* '%s'.cs-final-* '%s/current.cs-new' 2>/dev/null || true\nexit 0\n" \
    "$CS_NGINX_CONF" "$CS_NGINX_CONF" "$CS_ROOT_REAL"
}

# =============================================================================
# 本地构建产物（CS_BUILD_DIR / CS_BUILD_PATH / CS_BUILD_REPO）
# =============================================================================

# 生成本地 vhost 渲染文件（$1=目标路径 $2=upstream 端口）
cs_render_vhost() {
  local out="$1" port="$2"
  cat >"$out" <<EOF
# Managed by imboy-deploy.sh cs (CSD-CLI-01), contract hosted-widget-contract-v1 S7.
# 本文件由部署工具 staged → nginx -t → 原子替换管理，请勿手工编辑。

server {
    listen 80;
    server_name $CS_WIDGET_DOMAIN;

    location /.well-known/acme-challenge/ { root /var/www/certbot; }
    location / { return 301 https://\$host\$request_uri; }
}

server {
    listen 443 ssl http2;
    server_name $CS_WIDGET_DOMAIN;

    ssl_certificate     $CS_CERT_FULLCHAIN;
    ssl_certificate_key $CS_CERT_KEY;
    ssl_protocols TLSv1.2 TLSv1.3;
    client_max_body_size 50m;

    # S5/S6: Widget 会话 SSE —— 关闭缓冲，读超时对齐 API vhost（3600s）
    location ~ ^/api/v1/cs/widget/sessions/[0-9A-Za-z_-]+/events\$ {
        proxy_pass http://127.0.0.1:$port;
        proxy_http_version 1.1;
        proxy_set_header Host \$http_host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
        proxy_buffering off;
        proxy_cache off;
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
    }

    # S4: 动态 frame /w/:public_widget_id → backend（no-store 由 backend 下发）
    location ^~ /w/ {
        proxy_pass http://127.0.0.1:$port;
        proxy_http_version 1.1;
        proxy_set_header Host \$http_host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
        proxy_read_timeout 300s;
        proxy_send_timeout 300s;
    }

    # S5: 同源 Widget API（含旧 frame 兼容路径与上传代理端点）
    location /api/v1/cs/widget/ {
        proxy_pass http://127.0.0.1:$port;
        proxy_http_version 1.1;
        proxy_set_header Host \$http_host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
        proxy_read_timeout 300s;
        proxy_send_timeout 300s;
    }

    # S6: 静态产物（直连 nginx 形态下由本 vhost 按冻结缓存表下发）
    location = /v1/loader.js {
        root $CS_ROOT_REAL;
        try_files /loader.js =404;
        add_header Cache-Control "no-cache" always;
    }
    location ^~ /assets/ {
        root $CS_ROOT_REAL;
        add_header Cache-Control "public, max-age=31536000, immutable" always;
    }
    location ^~ /widget/ {
        root $CS_ROOT_REAL;
        add_header Cache-Control "no-store" always;
    }
    location = /manifest.json { root $CS_ROOT_REAL; add_header Cache-Control "no-store" always; }
    location = /health.txt   { root $CS_ROOT_REAL; add_header Cache-Control "no-store" always; }
    location / { return 404; }

    # 安全头最小集（server 级唯一来源；XFO/CSP 由 backend 逐路径下发，网关零注入）
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
    add_header X-Content-Type-Options nosniff always;
    add_header Referrer-Policy strict-origin-when-cross-origin always;
}
EOF
}

# 渲染产物的本地不变量断言（占位符零残留 + 关键路由存在）
cs_assert_vhost_render() {
  local f="$1" port="$2"
  [[ -s "$f" ]] || return 1
  if grep -q '@[A-Z_]*@' "$f"; then return 1; fi
  grep -q "server_name $CS_WIDGET_DOMAIN;" "$f" || return 1
  grep -q "listen 443 ssl" "$f" || return 1
  grep -q "ssl_certificate     $CS_CERT_FULLCHAIN;" "$f" || return 1
  grep -q "proxy_pass http://127.0.0.1:$port;" "$f" || return 1
  grep -q 'proxy_buffering off;' "$f" || return 1
  grep -q 'location = /v1/loader.js' "$f" || return 1
  grep -q 'location \^~ /w/' "$f" || return 1
  grep -q 'location /api/v1/cs/widget/' "$f" || return 1
  grep -q 'max-age=31536000, immutable' "$f" || return 1
  return 0
}

# 本地 Widget 产物存在性/完整性校验（CSD-IMG-01 布局）
cs_verify_artifact_dir() {
  local d="$1"
  [[ -d "$d" ]] || fail "Widget 产物目录不存在: $d"
  local f
  for f in loader.js manifest.json manifest.sha256 health.txt widget/index.html; do
    [[ -s "$d/$f" ]] || fail "Widget 产物缺失或为空: $f"
  done
  [[ -d "$d/assets" ]] || fail "Widget 产物缺失: assets/"
  [[ -n "$(ls -A "$d/assets" 2>/dev/null)" ]] || fail "Widget 产物 assets/ 为空"
}

# 预 SSH 的 CS_BUILD_DIR 解析：固定从源码仓构建，禁止发布陈旧 ignored 产物。
cs_resolve_build_paths() {
  local raw="$CS_BUILD_DIR" base parent_raw
  [[ "$raw" =~ ^[a-zA-Z0-9._/-]+$ ]] || fail "CS_BUILD_DIR 含非法字符，拒绝部署"
  base="${raw##*/}"
  [[ "$base" != "." && "$base" != ".." && -n "$base" ]] || fail "CS_BUILD_DIR 必须指向具体产物目录"
  # 源码仓 = 去掉最后一段（产物目录名）；首次构建时产物目录尚不存在，
  # 不能借道 ".." 穿越，必须先剥掉 basename 再解析。
  parent_raw="${raw%/*}"
  [[ "$parent_raw" == "$raw" ]] && parent_raw="."
  CS_BUILD_REPO="$(cd "$SCRIPT_DIR/$parent_raw" 2>/dev/null && pwd -P)" \
    || fail "CS_BUILD_DIR 的源码仓目录不可访问: $raw"
  [[ -f "$CS_BUILD_REPO/package.json" ]] || fail "CS_BUILD_DIR 上级缺少 package.json（需 imboyadmin 源码仓）"
  grep -q '"build:widget"' "$CS_BUILD_REPO/package.json" \
    || fail "源码仓 package.json 缺少 build:widget 脚本"
  CS_BUILD_PATH="$CS_BUILD_REPO/$base"
  [[ "$CS_BUILD_PATH" != "/" ]] || fail "CS_BUILD_DIR 解析结果非法"
  CS_SOURCE_HEAD="$(git -C "$CS_BUILD_REPO" rev-parse HEAD 2>/dev/null)" \
    || fail "Widget 源码不是可读取的 Git 工作树"
  [[ "$CS_SOURCE_HEAD" =~ ^[0-9a-f]{40}$ ]] || fail "Widget 源码 Git HEAD 非法"
  if [[ -n "$(git -C "$CS_BUILD_REPO" status --porcelain --untracked-files=normal)" ]]; then
    warn "Widget 源码存在未提交或未跟踪改动；按当前工作树继续发布（source_head 仅记录 Git HEAD 基线）" >&2
  fi
}

cs_verify_source_head() {
  local actual
  actual="$(sed -n 's/.*"source_head"[[:space:]]*:[[:space:]]*"\([0-9a-f]*\)".*/\1/p' \
    "$CS_BUILD_PATH/manifest.json" | head -1)"
  [[ "$actual" == "$CS_SOURCE_HEAD" ]] \
    || fail "Widget manifest source HEAD 不匹配: got=${actual:-missing} expect=$CS_SOURCE_HEAD"
}

# =============================================================================
# CS 专属配置校验（主脚本 allowlist 阶段、建立 SSH 之前调用，I6）
# =============================================================================
cs_validate_config() {
  local var
  for var in CS_WIDGET_DOMAIN CS_BUILD_DIR CS_REMOTE_ROOT CS_NGINX_CONF \
             CS_CERT_FULLCHAIN CS_CERT_KEY CS_SMOKE_SHOP_ORIGIN; do
    [[ -n "${!var:-}" ]] || fail ".env.deploy 缺少 CS 必填项: $var"
  done
  cs_valid_domain "$CS_WIDGET_DOMAIN" || fail "CS_WIDGET_DOMAIN 非法（须为合法 FQDN）"
  cs_remote_root_in_approved_scope "$CS_REMOTE_ROOT" \
    || fail "CS_REMOTE_ROOT 必须位于 /www/wwwroot/ 批准根内且不含 .."
  cs_safe_abs_path "$CS_NGINX_CONF" || fail "CS_NGINX_CONF 必须是无 .. 的安全绝对路径"
  cs_safe_abs_path "$CS_CERT_FULLCHAIN" || fail "CS_CERT_FULLCHAIN 必须是无 .. 的安全绝对路径"
  cs_safe_abs_path "$CS_CERT_KEY" || fail "CS_CERT_KEY 必须是无 .. 的安全绝对路径"
  [[ "$CS_CERT_FULLCHAIN" != "$CS_CERT_KEY" ]] || fail "CS_CERT_FULLCHAIN 与 CS_CERT_KEY 不得相同"
  cs_valid_origin "$CS_SMOKE_SHOP_ORIGIN" || fail "CS_SMOKE_SHOP_ORIGIN 非法（须为 http(s)://host[:port]）"
  cs_valid_version "${DEPLOY_VSN:-}" || fail "DEPLOY_VSN 非法，拒绝部署 CS 组件"
  return 0
}

# =============================================================================
# 步骤记账 + 回滚（I2/I3/I4/I5）
# =============================================================================

cs_step() {
  local name="$1" note="${2:-}"
  CS_STEPS_DONE="$CS_STEPS_DONE $name"
  if [[ -n "${CS_EVENT_LOG:-}" ]]; then
    printf 'STEP %s\n' "$name" >>"$CS_EVENT_LOG"
  fi
  log "▶ [CS/$name] $note"
}

cs_rollback() {
  local reason="$1" restored=0
  warn "CS 回滚开始: $reason"

  # 暂存/未激活 vhost 一律清理（I3）
  if ! cs_ssh_exec "$(cs_remote_clean_staged_cmd)" 2>/dev/null; then
    warn "CS 回滚: 暂存 vhost 清理命令失败（继续恢复主配置）"
  fi

  # 逆序恢复：先 vhost 后 symlink（与激活顺序相反）
  if [[ "${CS_ACTIVATED_VHOST:-0}" == 1 ]]; then
    if cs_ssh_exec "$(cs_remote_restore_vhost_cmd "${CS_VHOST_BAK:-}" "$CS_NGINX_CONF")"; then
      warn "CS 回滚: 已恢复 vhost ← ${CS_VHOST_BAK:-<首次安装，已移除>}"
    else
      warn "CS 回滚: vhost 恢复失败，需人工介入（备份: ${CS_VHOST_BAK:-无}）"
    fi
    CS_ACTIVATED_VHOST=0
    restored=1
  fi
  if [[ "${CS_ACTIVATED_SYMLINK:-0}" == 1 ]]; then
    if cs_ssh_exec "$(cs_remote_restore_symlink_cmd "${CS_OLD_SYMLINK:-}" "$CS_ROOT_REAL/current")"; then
      warn "CS 回滚: 已恢复 current symlink ← ${CS_OLD_SYMLINK:-<首次安装，已移除>}"
    else
      warn "CS 回滚: symlink 恢复失败，需人工介入（旧指向: ${CS_OLD_SYMLINK:-无}）"
    fi
    CS_ACTIVATED_SYMLINK=0
    restored=1
  fi

  if [[ "$restored" == 1 ]]; then
    if ! cs_ssh_exec "$(cs_remote_nginx_reload_cmd)" 2>/dev/null; then
      warn "CS 回滚: nginx reload 失败，需人工执行 nginx -t && nginx -s reload"
    fi
  fi

  # I4: 新 release 目录保留待人工排查，不删除任何 release
  if [[ -n "${CS_NEW_RELEASE_DIR:-}" ]]; then
    warn "CS 回滚: 新 release 目录保留待排查: $CS_NEW_RELEASE_DIR"
  fi
  warn "CS 回滚完成"
}

# =============================================================================
# deploy_cs — 一键事务主流程（S7）
# 用法: deploy_cs [with-backend|skip-backend]
#   with-backend  独立 `cs` 组件：内部复用 deploy_api 完成蓝绿后端
#   skip-backend  `all` 编排：Backend 已由 deploy_api 完成过一次，这里只做 CS 面
# =============================================================================
deploy_cs() {
  local backend_mode="${1:-with-backend}"
  local state

  CS_STEPS_DONE=""
  CS_ACTIVATED_SYMLINK=0
  CS_ACTIVATED_VHOST=0
  CS_OLD_SYMLINK=""
  CS_VHOST_BAK=""
  CS_NEW_RELEASE_DIR=""
  CS_EXPECTED_UPSTREAM_PORT=""
  CS_ACTUAL_UPSTREAM_PORT=""
  CS_STAGED_LOCAL_DIR="$(mktemp -d /tmp/imboy-cs-stage.XXXXXX)"

  parse_upstream() {
    case "$1" in
      UPSTREAM=*) printf '%s' "${1#UPSTREAM=}"; return 0 ;;
      *) return 1 ;;
    esac
  }

  # ---------- 1. PRECHECK ----------
  cs_step PRECHECK "校验远端批准根 / marker / 现状"
  local precheck_rc=0
  state="$(cs_ssh_cap "$(cs_remote_precheck_script)")" || precheck_rc=$?
  if [[ "$precheck_rc" -ne 0 ]]; then
    case "$precheck_rc" in
      2) fail "CS 远端根不存在: $CS_REMOTE_ROOT" ;;
      3) fail "CS 远端 realpath 越出批准根 /www/wwwroot/，拒绝部署" ;;
      4) fail "CS 远端根缺少 .imboy-cs-root 标记，拒绝部署: $CS_REMOTE_ROOT" ;;
      *) fail "CS PRECHECK 失败（远端状态探测异常, exit=${precheck_rc}）" ;;
    esac
  fi
  local line root_real="" cur="" vhost_exists=""
  while IFS= read -r line; do
    case "$line" in
      ROOT=*) root_real="${line#ROOT=}" ;;
      CURRENT=*) cur="${line#CURRENT=}" ;;
      VHOST_EXISTS=*) vhost_exists="${line#VHOST_EXISTS=}" ;;
    esac
  done <<STATE
$state
STATE
  cs_remote_root_in_approved_scope "$root_real" \
    || fail "CS 远端 realpath 不在批准根内: $root_real"
  [[ "$vhost_exists" == 0 || "$vhost_exists" == 1 ]] || fail "CS PRECHECK 输出异常"
  CS_ROOT_REAL="$root_real"
  CS_OLD_SYMLINK="$cur"
  # 保留部署前 vhost 存在性快照（审计/外部测试可见；回滚以 CS_VHOST_BAK 为准）
  # shellcheck disable=SC2034
  CS_VHOST_EXISTS="$vhost_exists"
  if [[ -n "$CS_OLD_SYMLINK" ]]; then
    log "  现状: current → $CS_OLD_SYMLINK"
  else
    log "  现状: 无 current symlink（首次安装）"
  fi
  if [[ "$vhost_exists" == 1 ]]; then
    log "  现状: vhost 已存在（升级）: $CS_NGINX_CONF"
  else
    log "  现状: vhost 不存在（首次安装）: $CS_NGINX_CONF"
  fi

  # ---------- 2. BUILD_AND_VERIFY_WIDGET ----------
  cs_step BUILD_AND_VERIFY_WIDGET "本地构建并校验 Widget 产物"
  log "  构建: (cd $CS_BUILD_REPO && bun run build:widget)"
  if ! (cd "$CS_BUILD_REPO" && cs_bun run build:widget); then
    cs_rollback "Widget 本地构建失败"
    fail "Widget 构建失败（build:widget）"
  fi
  if ! cs_verify_artifact_dir "$CS_BUILD_PATH"; then
    cs_rollback "Widget 产物完整性校验失败"
    fail "Widget 产物完整性校验失败: $CS_BUILD_PATH"
  fi
  cs_verify_source_head
  ok "  Widget 产物校验通过: $CS_BUILD_PATH"

  # ---------- 3. STAGE_WIDGET_RELEASE ----------
  cs_step STAGE_WIDGET_RELEASE "上传至远端不可变 release 目录"
  CS_RELEASE_STAMP="$(date +%Y%m%d%H%M%S)"
  CS_NEW_RELEASE_DIR="$CS_ROOT_REAL/releases/$CS_RELEASE_STAMP"
  if ! cs_ssh_exec "$(cs_remote_prepare_release_cmd)"; then
    cs_rollback "创建远端 release 目录失败"
    fail "创建远端 release 目录失败: $CS_NEW_RELEASE_DIR"
  fi
  log "  上传产物 → $SERVER_USER@$SERVER_HOST:$CS_NEW_RELEASE_DIR"
  if ! cs_rsync -az --exclude='.imboy-cs-root' \
      -e "ssh -p $SERVER_PORT -o ControlPath=$SSH_CTRL" \
      "$CS_BUILD_PATH/" \
      "$SERVER_USER@$SERVER_HOST:$CS_NEW_RELEASE_DIR/"; then
    cs_rollback "Widget release 上传失败"
    fail "Widget release 上传失败"
  fi
  if ! cs_ssh_exec "$(cs_remote_verify_release_script)"; then
    cs_rollback "release manifest 校验和不匹配"
    fail "release manifest.sha256 校验失败: $CS_NEW_RELEASE_DIR"
  fi
  ok "  release 已暂存并通过 manifest 校验: $CS_NEW_RELEASE_DIR"

  # ---------- 4. VALIDATE_CS_VHOST_AND_TLS ----------
  cs_step VALIDATE_CS_VHOST_AND_TLS "staged vhost + 证书配对校验"
  if ! state="$(cs_ssh_cap "$(cs_remote_discover_upstream_script)")"; then
    fail "蓝绿 upstream 发现失败，无法渲染 staged vhost"
  fi
  CS_EXPECTED_UPSTREAM_PORT="$(parse_upstream "${state%%$'\n'*}")" \
    || fail "upstream 发现输出异常"
  if [[ -z "$CS_EXPECTED_UPSTREAM_PORT" ]]; then
    CS_EXPECTED_UPSTREAM_PORT="$DEPLOY_BLUE_PORT"
    log "  当前无唯一活动 upstream（首次安装），staged vhost 以 BLUE $DEPLOY_BLUE_PORT 预渲染"
  fi
  local staged_local="$CS_STAGED_LOCAL_DIR/cs-vhost-staged.conf"
  cs_render_vhost "$staged_local" "$CS_EXPECTED_UPSTREAM_PORT"
  if ! cs_assert_vhost_render "$staged_local" "$CS_EXPECTED_UPSTREAM_PORT"; then
    fail "staged vhost 本地不变量校验失败"
  fi
  local staged_remote="$CS_NGINX_CONF.cs-staged-$CS_RELEASE_STAMP"
  if ! cs_scp -P "$SERVER_PORT" -o "ControlPath=$SSH_CTRL" \
      "$staged_local" "$SERVER_USER@$SERVER_HOST:$staged_remote"; then
    cs_rollback "staged vhost 上传失败"
    fail "staged vhost 上传失败"
  fi
  if ! cs_ssh_exec "$(cs_remote_tls_validate_script "$staged_remote")"; then
    cs_rollback "staged vhost nginx -t 或证书校验失败"
    fail "staged vhost nginx -t / 证书存在性配对校验失败"
  fi
  ok "  staged vhost nginx -t 通过，证书存在且配对"

  # ---------- 5. DEPLOY_BACKEND_BLUE_GREEN ----------
  if [[ "$backend_mode" == "skip-backend" ]]; then
    cs_step DEPLOY_BACKEND_BLUE_GREEN "由 all 编排已完成（复用 deploy_api，零第二套蓝绿）"
  else
    cs_step DEPLOY_BACKEND_BLUE_GREEN "复用 deploy_api 蓝绿部署后端"
    # 条件上下文（if !）会抑制被调链路的 errexit，deploy_api 内部失败会被吞掉；
    # 必须在非条件上下文调用并显式捕获返回码（配合 deploy_api 的显式 return 1）。
    local api_rc=0
    set +e
    cs_call_deploy_api
    api_rc=$?
    set -e
    if [[ "$api_rc" -ne 0 ]]; then
      cs_rollback "Backend 蓝绿部署失败（未激活任何 Widget 变更，I1/I2）"
      fail "Backend 蓝绿部署失败 (exit=$api_rc)"
    fi
  fi

  # ---------- 6. ATOMIC_ACTIVATE_WIDGET_AND_VHOST ----------
  cs_step ATOMIC_ACTIVATE_WIDGET_AND_VHOST "原子 symlink + vhost 替换"
  if ! state="$(cs_ssh_cap "$(cs_remote_discover_upstream_script)")"; then
    cs_rollback "激活前 upstream 复核失败"
    fail "激活前蓝绿 upstream 复核失败"
  fi
  CS_ACTUAL_UPSTREAM_PORT="$(parse_upstream "${state%%$'\n'*}")" \
    || fail "upstream 发现输出异常"
  [[ -n "$CS_ACTUAL_UPSTREAM_PORT" ]] || {
    cs_rollback "Backend 部署后无唯一活动 upstream"
    fail "Backend 部署后 NGINX_CONF 无唯一活动 upstream，拒绝激活"
  }
  local final_local="$CS_STAGED_LOCAL_DIR/cs-vhost-final.conf"
  cs_render_vhost "$final_local" "$CS_ACTUAL_UPSTREAM_PORT"
  if ! cs_assert_vhost_render "$final_local" "$CS_ACTUAL_UPSTREAM_PORT"; then
    cs_rollback "final vhost 本地不变量校验失败"
    fail "final vhost 本地不变量校验失败"
  fi
  local final_remote="$CS_NGINX_CONF.cs-final-$CS_RELEASE_STAMP"
  if ! cs_scp -P "$SERVER_PORT" -o "ControlPath=$SSH_CTRL" \
      "$final_local" "$SERVER_USER@$SERVER_HOST:$final_remote"; then
    cs_rollback "final vhost 上传失败"
    fail "final vhost 上传失败"
  fi
  if ! cs_ssh_exec "$(cs_remote_tls_validate_script "$final_remote")"; then
    cs_rollback "final vhost nginx -t 失败"
    fail "final vhost nginx -t 失败"
  fi
  if ! state="$(cs_ssh_cap "$(cs_remote_backup_vhost_script)")"; then
    cs_rollback "vhost 备份失败"
    fail "vhost 时间戳备份失败，拒绝原地替换"
  fi
  CS_VHOST_BAK="$(printf '%s\n' "$state" | sed -n 's/^BAK=//p' | head -1)"
  if ! cs_ssh_exec "$(cs_remote_record_prev_cmd)"; then
    warn "CS: 旧 symlink 指向记录失败（回滚仍可依据 PRECHECK 记忆值）"
  fi
  if cs_ssh_exec "$(cs_remote_swap_symlink_cmd "$CS_NEW_RELEASE_DIR" "$CS_ROOT_REAL/current")"; then
    CS_ACTIVATED_SYMLINK=1
  else
    cs_rollback "原子 symlink 切换失败"
    fail "原子 symlink 切换失败"
  fi
  if cs_ssh_exec "$(cs_remote_swap_vhost_cmd "$final_remote" "$CS_NGINX_CONF")"; then
    CS_ACTIVATED_VHOST=1
  else
    cs_rollback "原子 vhost 替换失败"
    fail "原子 vhost 替换失败"
  fi
  if ! cs_ssh_exec "$(cs_remote_nginx_reload_cmd)"; then
    cs_rollback "nginx reload 失败"
    fail "nginx -t/reload 失败，已恢复旧 symlink/vhost"
  fi
  ok "  current → ${CS_NEW_RELEASE_DIR}；vhost 已原子替换并 reload"

  # ---------- 7. REAL_SMOKE ----------
  cs_step REAL_SMOKE "宿主页 / loader / frame 路由 / 健康探针"
  local smoke_rc=0
  state="$(cs_ssh_cap "$(cs_remote_smoke_script)")" || smoke_rc=$?
  if [[ "$smoke_rc" -ne 0 ]]; then
    cs_step FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY "smoke 失败 → 回滚"
    cs_rollback "REAL_SMOKE 失败 (exit=$smoke_rc)"
    fail "REAL_SMOKE 失败 (exit=$smoke_rc)，已恢复旧 symlink/vhost"
  fi
  ok "  smoke 全部通过: $state"

  # ---------- 8. FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY ----------
  cs_step FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY "记录激活台账"
  if ! cs_ssh_exec "$(cs_remote_finalize_cmd)"; then
    warn "CS: activations.log 记录失败（部署本身已成功）"
  fi
  rm -rf "$CS_STAGED_LOCAL_DIR"
  ok "▶ CS Widget 部署完成: https://${CS_WIDGET_DOMAIN}（release ${CS_NEW_RELEASE_DIR##*/}）"

  # 编排自检：步骤序列必须逐字等于合同 S7（防御性，A03）
  local expected done_steps
  expected="$(cs_expected_steps | tr '\n' ' ' | sed -e 's/^ *//' -e 's/ *$//')"
  done_steps="$(printf '%s' "$CS_STEPS_DONE" | sed -e 's/^ *//' -e 's/ *$//')"
  if [[ "$done_steps" != "$expected" ]]; then
    fail "CS 步骤序列偏离合同 S7: [$done_steps] != [$expected]"
  fi
}
