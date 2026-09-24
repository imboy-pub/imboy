#!/usr/bin/env bash
# LiveKit TURN 证书分发 + 安全重启 —— certbot deploy hook（LK-DEP-01 / LK-01 V4）
#
# 安装位置（宿主机 certbot 形态，宝塔/生产机）：
#   cp deploy/nginx/livekit-turn-cert-deploy-hook.sh \
#      /etc/letsencrypt/renewal-hooks/deploy/livekit-turn-cert.sh && chmod +x ...
# certbot 每次续期成功后对本机**所有**域名的 lineage 逐个调用本 hook；本脚本只
# 处理 TURN_DOMAIN 的 lineage，其余域名直接跳过（不会误动 nginx 或其他服务，
# A03：hook 只 reload 目标 —— 本域唯一目标就是 LiveKit 容器，不经 nginx）。
#
# 行为：
#   1. 域名过滤：RENEWED_LINEAGE 目录名（或手工调用时的 $1）≠ TURN_DOMAIN 即退出 0；
#   2. 校验源证书可解析且 SAN 含 TURN_DOMAIN，任何校验失败立即退出非零，
#      目标目录保持旧证书不动（失败不留半配置，下一次续期自然重试）；
#   3. 原子分发：目标目录与 Certbot lineage 不同时，先 install 到目标目录内的
#      暂存文件并复验，再 mv 覆盖；两者相同时直接使用 lineage，绝不覆盖 Certbot
#      管理的符号链接；
#   4. 安全重启：先 docker inspect 确认容器在跑，再 restart。⚠️ LiveKit 重启 =
#      **全部进行中通话立即中断**（room 状态在进程内存，LK-01 §5.2），客户端
#      SDK 自动 RESUME 恢复；本 hook 只应在低峰续期窗口被触发。容器不在跑/不存在
#      时跳过重启（退出 0，不动 certbot 状态），下次启动自然加载新证书。
#
# 手工演练（不触发真实续期）：
#   TURN_DOMAIN=turn.imboy.pub LIVEKIT_TURN_CERT_TARGET=/tmp/lk-certs \
#     bash livekit-turn-cert-deploy-hook.sh /etc/letsencrypt/live/turn.imboy.pub
#
# 可调环境变量：TURN_DOMAIN / LIVEKIT_TURN_CERT_TARGET（兼容旧名，优先）/
# LIVEKIT_TURN_CERT_DIR（安装器使用的名称；默认 /etc/imboy/livekit-certs）/
# LIVEKIT_CONTAINER_NAME（默认 imboy_livekit）。

set -euo pipefail

# The L4 SNI installer stores only non-secret deployment paths/domains here.
# Source it only when root owns it and neither group nor world can write it.
HOOK_CONFIG="${LIVEKIT_TURN_HOOK_CONFIG:-/etc/imboy/livekit-l4-sni.env}"
if [ -r "$HOOK_CONFIG" ]; then
  [ "$(stat -c %u "$HOOK_CONFIG")" = 0 ] \
    || { printf 'livekit-turn hook: unsafe config owner: %s\n' "$HOOK_CONFIG" >&2; exit 1; }
  if find "$HOOK_CONFIG" -prune -perm /022 -print -quit | grep -q .; then
    printf 'livekit-turn hook: writable config rejected: %s\n' "$HOOK_CONFIG" >&2
    exit 1
  fi
  # shellcheck source=/dev/null
  source "$HOOK_CONFIG"
fi

TURN_DOMAIN="${TURN_DOMAIN:-turn.imboy.pub}"
CERT_TARGET_DIR="${LIVEKIT_TURN_CERT_TARGET:-${LIVEKIT_TURN_CERT_DIR:-/etc/imboy/livekit-certs}}"
CONTAINER="${LIVEKIT_CONTAINER_NAME:-imboy_livekit}"
STAGE_FULLCHAIN="$CERT_TARGET_DIR/.fullchain.pem.staged"
STAGE_PRIVKEY="$CERT_TARGET_DIR/.privkey.pem.staged"

die() { printf 'livekit-turn hook: ❌ %s\n' "$*" >&2; exit 1; }
log() { printf 'livekit-turn hook: %s\n' "$*"; }
canonical_dir() { (cd -P -- "$1" 2>/dev/null && pwd -P); }

# 半配置防线：任何退出路径都清掉暂存文件（已 mv 的最终文件不受影响）
cleanup_stage() { rm -f -- "$STAGE_FULLCHAIN" "$STAGE_PRIVKEY"; }
trap cleanup_stage EXIT

# ── 1) 域名过滤：非 TURN 域的续期一律跳过（exit 0，不惊动 certbot）──────────
LINEAGE="${RENEWED_LINEAGE:-${1:-}}"
[ -n "$LINEAGE" ] || { log "无 RENEWED_LINEAGE 且未传 lineage 参数，跳过"; exit 0; }
LINEAGE_DOMAIN="$(basename "$LINEAGE")"
if [ "$LINEAGE_DOMAIN" != "$TURN_DOMAIN" ]; then
  log "非 TURN 域（${LINEAGE_DOMAIN} ≠ ${TURN_DOMAIN}），跳过"
  exit 0
fi
SRC_FULLCHAIN="$LINEAGE/fullchain.pem"
SRC_PRIVKEY="$LINEAGE/privkey.pem"
[ -s "$SRC_FULLCHAIN" ] && [ -s "$SRC_PRIVKEY" ] \
  || die "lineage 证书文件缺失：$LINEAGE"

# ── 2) 源证书校验：可解析 + SAN/CM 含 TURN_DOMAIN + 公私钥配对 ───────────────
command -v openssl >/dev/null 2>&1 || die "缺少 openssl，无法校验证书"
openssl x509 -noout -in "$SRC_FULLCHAIN" 2>/dev/null \
  || die "fullchain.pem 无法解析为 X.509 证书"
openssl x509 -noout -subject -ext subjectAltName -in "$SRC_FULLCHAIN" 2>/dev/null \
  | grep -q "$TURN_DOMAIN" \
  || die "证书不覆盖 ${TURN_DOMAIN}（subject/SAN 均不含），拒绝分发"
CERT_PUB="$(openssl x509 -noout -pubkey -in "$SRC_FULLCHAIN" 2>/dev/null || true)"
KEY_PUB="$(openssl pkey -pubout -in "$SRC_PRIVKEY" 2>/dev/null || true)"
[ -n "$CERT_PUB" ] && [ "$CERT_PUB" = "$KEY_PUB" ] \
  || die "fullchain 与 privkey 公钥不配对，拒绝分发"

# ── 3) 原子分发：暂存 → 复验 → rename 覆盖 ──────────────────────────────────
SAME_AS_LINEAGE=0
if [ -d "$CERT_TARGET_DIR" ]; then
  TARGET_CANONICAL="$(canonical_dir "$CERT_TARGET_DIR")" \
    || die "无法解析证书目标目录：$CERT_TARGET_DIR"
  LINEAGE_CANONICAL="$(canonical_dir "$LINEAGE")" \
    || die "无法解析 Certbot lineage：$LINEAGE"
  [ "$TARGET_CANONICAL" = "$LINEAGE_CANONICAL" ] && SAME_AS_LINEAGE=1
fi
if [ "$SAME_AS_LINEAGE" = 1 ]; then
  log "目标目录就是 Certbot lineage，保留 Certbot 符号链接并跳过证书复制"
else
  mkdir -p "$CERT_TARGET_DIR"
  install -m 644 "$SRC_FULLCHAIN" "$STAGE_FULLCHAIN"
  install -m 600 "$SRC_PRIVKEY" "$STAGE_PRIVKEY"
  # 暂存副本复验（防 install 中途写坏/磁盘错误把坏证书换上去）
  openssl x509 -noout -in "$STAGE_FULLCHAIN" 2>/dev/null \
    || die "暂存证书复验失败，目标目录未改动"
  openssl pkey -noout -in "$STAGE_PRIVKEY" 2>/dev/null \
    || die "暂存私钥复验失败，目标目录未改动"
  mv -f -- "$STAGE_FULLCHAIN" "$CERT_TARGET_DIR/fullchain.pem"
  mv -f -- "$STAGE_PRIVKEY" "$CERT_TARGET_DIR/privkey.pem"
  log "证书已分发到 ${CERT_TARGET_DIR}（fullchain.pem + privkey.pem）"
fi

# ── 4) 安全重启：先确认容器状态，再 restart ─────────────────────────────────
# ⚠️ restart = 进行中通话全部中断（见文件头）；容器未运行/不存在时跳过。
command -v docker >/dev/null 2>&1 || { log "宿主机无 docker 命令，跳过重启（证书已更新）"; exit 0; }
STATE="$(docker inspect -f '{{.State.Status}}' "$CONTAINER" 2>/dev/null || true)"
case "$STATE" in
  running)
    # LiveKit 进程不 watch 证书文件，TURN/TLS 证书只有 restart 才生效。
    if docker restart "$CONTAINER"; then
      log "已重启 ${CONTAINER}（TURN/TLS 新证书生效；进行中通话已被中断，SDK 自动 RESUME）"
    else
      # 证书已就位，仅重启失败：退出非零让 certbot 日志留痕，人工重试 restart 即可
      die "docker restart $CONTAINER 失败 —— 证书已分发，请人工排查后重试重启"
    fi
    ;;
  "")
    log "容器 $CONTAINER 不存在，跳过重启（exit 0；部署容器后自然加载新证书）"
    ;;
  *)
    log "容器 ${CONTAINER} 状态为 ${STATE}（非 running），跳过重启；下次启动加载新证书"
    ;;
esac
