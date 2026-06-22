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
# 前提：.env 已填 API_DOMAIN / ADMIN_DOMAIN / CERTBOT_EMAIL，且两域名 A 记录已指向本机。

set -euo pipefail
cd "$(dirname "$0")/.."

[ -f .env ] || { echo "缺少 .env，请先 cp .env.example .env 并填写"; exit 1; }
set -a; . ./.env; set +a

: "${API_DOMAIN:?在 .env 中设置 API_DOMAIN}"
: "${ADMIN_DOMAIN:?在 .env 中设置 ADMIN_DOMAIN}"
: "${CERTBOT_EMAIL:?在 .env 中设置 CERTBOT_EMAIL（证书到期通知邮箱）}"
DATA_DIR="${DATA_DIR:-./data}"
COMPOSE="docker compose -f docker-compose.prod.yml"
DOMAINS="$API_DOMAIN $ADMIN_DOMAIN"

CONF="$DATA_DIR/certbot/conf"
WWW="$DATA_DIR/certbot/www"
mkdir -p "$WWW"

# ① 为每个域名下发临时自签证书，使 nginx HTTPS server 能启动
for d in $DOMAINS; do
  live="$CONF/live/$d"
  mkdir -p "$live"
  echo "==> 临时自签证书 $d"
  docker run --rm -v "$(pwd)/$CONF:/etc/letsencrypt" certbot/certbot \
    openssl req -x509 -nodes -newkey rsa:2048 -days 1 \
      -keyout "/etc/letsencrypt/live/$d/privkey.pem" \
      -out    "/etc/letsencrypt/live/$d/fullchain.pem" \
      -subj "/CN=$d"
done

# ② 启动 nginx（已能加载临时证书并响应 ACME challenge）
echo "==> 启动 nginx"
$COMPOSE up -d imboy_nginx

# ③ 删除临时证书并经 webroot 申请正式证书（每个域名独立 live 目录）
for d in $DOMAINS; do
  echo "==> 申请正式证书 $d"
  rm -rf "$CONF/live/$d" "$CONF/archive/$d" "$CONF/renewal/$d.conf"
  $COMPOSE run --rm --entrypoint certbot imboy_certbot \
    certonly --webroot -w /var/www/certbot \
      -d "$d" --email "$CERTBOT_EMAIL" \
      --agree-tos --no-eff-email --non-interactive
done

# ④ reload nginx 加载正式证书
echo "==> reload nginx"
$COMPOSE exec imboy_nginx nginx -s reload
echo "✅ TLS 证书签发完成：$DOMAINS"
