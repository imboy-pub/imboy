#!/usr/bin/env bash
# 本地开发环境一键初始化 / One-shot local dev environment setup
#
# 步骤 / Steps:
#   1. 检查依赖（docker / erlang / make）
#   2. 准备 .env（缺 IMBOY_PG_PASSWORD 时自动生成）
#   3. 启动 PostgreSQL 容器（docker compose --profile dev）并等待就绪
#   4. 准备 config/sys.local.config（从 example 复制）
#   5. （可选 --with-garage）启动本地 Garage S3
#
# 用法 / Usage:
#   bash scripts/dev_setup.sh [--with-garage]
#
# 之后 / Afterwards:
#   IMBOYENV=local make run     # 数据库迁移在应用启动时自动执行
#   bash scripts/seed_demo.sh   # （可选）填充演示数据
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

WITH_GARAGE=0
[ "${1:-}" = "--with-garage" ] && WITH_GARAGE=1

step() { printf '\n==> %s\n' "$1"; }
die()  { printf '✗ %s\n' "$1" >&2; exit 1; }

# ============ 1. 依赖检查 / Prerequisites ============
step "检查依赖 / Checking prerequisites"
command -v docker >/dev/null || die "缺少 docker，请先安装 / docker not found"
command -v make   >/dev/null || die "缺少 make / make not found"
command -v erl    >/dev/null || die "缺少 erlang（需要 OTP 28+）/ erlang not found (OTP 28+ required)"
OTP_VER="$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().')"
if [ "${OTP_VER%%.*}" -lt 28 ]; then
  echo "⚠️  当前 OTP ${OTP_VER}，项目要求 28+，编译可能失败 / OTP 28+ required"
fi
echo "✓ docker / make / erlang(OTP ${OTP_VER})"

# ============ 2. .env / Environment file ============
step "准备 .env"
if [ ! -f .env ]; then
  cp .env.example .env
  echo "✓ 已从 .env.example 创建 .env"
fi
# IMBOY_PG_PASSWORD 是 compose 的必填项；缺失或为占位值时自动生成
# IMBOY_PG_PASSWORD is required by compose; auto-generate when missing/placeholder
if ! grep -qE '^IMBOY_PG_PASSWORD=.+' .env || grep -qE '^IMBOY_PG_PASSWORD=(changeme|CHANGE_ME|your_password)?$' .env; then
  PW="$(openssl rand -hex 16)"
  if grep -qE '^IMBOY_PG_PASSWORD=' .env; then
    # 兼容 GNU/BSD sed：写临时文件再覆盖 / Portable across GNU & BSD sed
    sed "s|^IMBOY_PG_PASSWORD=.*|IMBOY_PG_PASSWORD=${PW}|" .env > .env.tmp && mv .env.tmp .env
  else
    printf '\nIMBOY_PG_PASSWORD=%s\n' "${PW}" >> .env
  fi
  echo "✓ 已生成随机 IMBOY_PG_PASSWORD（见 .env，请妥善保管）"
fi

# ============ 3. PostgreSQL 容器 / PG container ============
step "启动 PostgreSQL（imboy_pg18，host 端口 \${IMBOY_PG_HOST_PORT:-4323}）"
docker compose --profile dev up -d imboy_pg18

echo -n "等待 PG 就绪 / Waiting for PG"
PG_OK=0
for _ in $(seq 1 30); do
  if docker exec imboy_pg18 pg_isready -U "${IMBOY_PG_USERNAME:-imboy_user}" >/dev/null 2>&1; then
    echo " ✓"; PG_OK=1; break
  fi
  echo -n "."; sleep 2
done
[ "$PG_OK" -eq 1 ] || die "PG 60 秒未就绪，查看：docker logs imboy_pg18"

# ============ 4. 本地配置 / Local config ============
step "准备 config/sys.local.config"
if [ ! -f config/sys.local.config ]; then
  cp config/sys.local.config.example config/sys.local.config
  echo "✓ 已从 example 创建；请按需修改数据库密码等项（与 .env 保持一致）"
else
  echo "✓ 已存在，跳过（不覆盖）"
fi

# ============ 5. 可选 Garage / Optional Garage S3 ============
if [ "$WITH_GARAGE" -eq 1 ]; then
  step "启动本地 Garage S3"
  bash scripts/garage-local-setup.sh
fi

# ============ 完成 / Done ============
step "完成！下一步 / Done! Next steps:"
cat <<'NEXT'
  1. 核对 config/sys.local.config 中的数据库连接（密码须与 .env 一致）
  2. IMBOYENV=local make run        # 启动后端（迁移自动执行）
  3. bash scripts/seed_demo.sh      # （可选）演示数据
  4. make eunit                     # 单元测试
NEXT
