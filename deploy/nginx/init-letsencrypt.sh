#!/usr/bin/env bash
# IMBoy 首次 TLS 证书签发（仅首次部署执行一次）
#
# 解决鸡生蛋问题：nginx 的 HTTPS server 引用证书文件，但证书需 nginx 先能响应
# ACME challenge 才能签发。本脚本：① 下发临时自签证书让 nginx 起来 →
# ② certbot 经 webroot 申请正式证书 → ③ reload nginx 加载正式证书。
#
# 之后续期全自动：imboy_certbot 容器每 12h 跑 certbot renew，imboy_nginx 每 6h reload。
#
# 用法：cd deploy && bash nginx/init-letsencrypt.sh
# 前提：.env 已填域名 / CERTBOT_EMAIL，且域名 A/AAAA 记录已指向本机。

set -euo pipefail
cd "$(dirname "$0")/.."

[ -f .env ] || { echo "缺少 .env，请先 cp .env.example .env 并填写"; exit 1; }
command -v openssl >/dev/null 2>&1 || { echo "缺少 openssl，无法生成首次启动所需的临时证书"; exit 1; }
set -a; . ./.env; set +a

: "${API_DOMAIN:?在 .env 中设置 API_DOMAIN}"
: "${ADMIN_DOMAIN:?在 .env 中设置 ADMIN_DOMAIN}"
# CSD-DEP-01：客服 Widget 第三域同为必填（fail-closed），证书与 API/Admin 各自独立签发
: "${CS_WIDGET_DOMAIN:?在 .env 中设置 CS_WIDGET_DOMAIN（客服 Widget 域名）}"
# LK-DEP-01：LiveKit 两域必签 —— RTC 域供 nginx 443 WSS 反代使用；TURN 域供
# embedded TURN TLS 5349（overlay 只读挂载该 live 目录）。两域均经 webroot 签发
#（未匹配 server_name 的 ACME 请求由默认 80 server 的 webroot location 兜底应答）。
: "${RTC_DOMAIN:?在 .env 中设置 RTC_DOMAIN（LiveKit 信令域）}"
: "${TURN_DOMAIN:?在 .env 中设置 TURN_DOMAIN（LiveKit TURN 域）}"
: "${CERTBOT_EMAIL:?在 .env 中设置 CERTBOT_EMAIL（证书到期通知邮箱）}"
DATA_DIR="${DATA_DIR:-./data}"
# install.sh 传入空格分隔的完整 Compose 文件集合；单独运行仍兼容 COMPOSE_FILE。
COMPOSE_FILES="${COMPOSE_FILES:-${COMPOSE_FILE:-docker-compose.prod.yml}}"
COMPOSE_ARGS=()
for compose_file in $COMPOSE_FILES; do
  [ -f "$compose_file" ] || { echo "缺少 compose 文件：$compose_file" >&2; exit 1; }
  COMPOSE_ARGS+=(-f "$compose_file")
done
compose() { docker compose "${COMPOSE_ARGS[@]}" "$@"; }

DOMAINS=("$API_DOMAIN" "$ADMIN_DOMAIN" "$CS_WIDGET_DOMAIN" "$RTC_DOMAIN" "$TURN_DOMAIN")
case "$(printf '%s' "${UPTRACE_ENABLED:-false}" | tr '[:upper:]' '[:lower:]')" in
  true|1)
    : "${UPTRACE_DOMAIN:?UPTRACE_ENABLED=true 时必须设置 UPTRACE_DOMAIN}"
    DOMAINS+=("$UPTRACE_DOMAIN")
    ;;
esac

CONF="$DATA_DIR/certbot/conf"
WWW="$DATA_DIR/certbot/www"
mkdir -p "$WWW"
# 临时自签证书标记文件：certbot 中途失败时，尚未轮到的域名会留着 1 天期的
# 自签证书。无标记的话重跑会被「证书已存在，跳过」误判，该域永远停在自签
# 证书上（A04 故障恢复路径）。带标记 = 必须重签。
SELF_SIGNED_MARK=".imboy-selfsigned"
is_real_cert() {
  [ -s "$1/fullchain.pem" ] && [ -s "$1/privkey.pem" ] && [ ! -e "$1/$SELF_SIGNED_MARK" ]
}
# 只处理缺失证书的域名，绝不覆盖已签发证书。
MISSING_DOMAINS=()
for d in "${DOMAINS[@]}"; do
  if is_real_cert "$CONF/live/$d"; then
    echo "==> 正式证书已存在，跳过 $d"
  else
    [ ! -e "$CONF/archive/$d" ] && [ ! -e "$CONF/renewal/$d.conf" ] \
      || { echo "证书状态不完整：$d 存在 archive/renewal 但 live 文件缺失，请先人工修复" >&2; exit 1; }
    MISSING_DOMAINS+=("$d")
  fi
done

if [ "${#MISSING_DOMAINS[@]}" -eq 0 ]; then
  compose up -d imboy_nginx imboy_certbot
  echo "✅ TLS 证书均已存在：${DOMAINS[*]}"
  exit 0
fi

# ① 为缺证书的域名下发临时自签证书，使 nginx HTTPS server 能启动
for d in "${MISSING_DOMAINS[@]}"; do
  live="$CONF/live/$d"
  mkdir -p "$live"
  echo "==> 临时自签证书 $d"
  openssl req -x509 -nodes -newkey rsa:2048 -days 1 \
      -keyout "$live/privkey.pem" \
      -out    "$live/fullchain.pem" \
      -subj "/CN=$d"
  chmod 600 "$live/privkey.pem"
  touch "$live/$SELF_SIGNED_MARK"
done

# ② 启动 nginx（已能加载临时证书并响应 ACME challenge）
echo "==> 启动 nginx"
compose up -d imboy_nginx

# ③ 删除临时证书并经 webroot 申请正式证书（每个域名独立 live 目录）
for d in "${MISSING_DOMAINS[@]}"; do
  echo "==> 申请正式证书 $d"
  rm -rf -- "$CONF/live/$d"
  compose run --rm --entrypoint certbot imboy_certbot \
    certonly --webroot -w /var/www/certbot \
      -d "$d" --email "$CERTBOT_EMAIL" \
      --agree-tos --no-eff-email --non-interactive
  [ -s "$CONF/live/$d/fullchain.pem" ] && [ -s "$CONF/live/$d/privkey.pem" ] \
    || { echo "certbot 返回成功但证书文件缺失：$d" >&2; exit 1; }
done

# ④ reload nginx 加载正式证书
echo "==> reload nginx"
compose exec imboy_nginx nginx -s reload
echo "✅ TLS 证书签发完成：${DOMAINS[*]}"
