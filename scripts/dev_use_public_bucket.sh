#!/usr/bin/env bash
# dev_use_public_bucket.sh — 本地开发把对象存储切到公网真桶（s3.imboy.pub）。
#
# 为什么需要：AI 回课时 presign 出去的 URL 是给**第三方多模态模型**抓的，
# 本地 garage 只有内网地址，模型抓不到 ⇒ provider_error / 超时，且日志无痕。
# 切到公网桶后本地与生产同构，不再依赖 cloudflared 隧道。
#
# 隔离方式：不新建桶，而是在 imboy 桶里用 key_prefix = moya/ 做命名空间隔离，
# 避免本地测试对象污染生产数据（代码侧见 elib_oss:key_prefix/0）。
# 密钥**不写进配置文件**，走 {env, VAR} 从 .env.local 读取。
#
# 用法：
#   bash scripts/dev_use_public_bucket.sh status    # 体检（不改动任何东西）
#   bash scripts/dev_use_public_bucket.sh apply     # 切换到公网桶
#   bash scripts/dev_use_public_bucket.sh revert    # 恢复本地 garage
#
# ⚠ 前置：本机若开着 Clash 一类代理，s3.imboy.pub 会被 fake-ip 劫持成
#   198.18.x.x，Erlang 侧同样受害（inet:getaddr 验证过）。apply 会先拦下，
#   并按下面的命令修 /etc/hosts 后再来。

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG="$ROOT/config/sys.local.config"
ENVLOCAL="$ROOT/.env.local"
REAL_IP="106.53.76.53"
HOST="s3.imboy.pub"
PREFIX="moya/"

PY="$(command -v python3 || true)"
[ -n "$PY" ] || PY=/usr/bin/python3

ok()   { printf "  \033[32m✓\033[0m %s\n" "$1"; }
bad()  { printf "  \033[31m✗\033[0m %s\n" "$1"; }
warn() { printf "  \033[33m⚠\033[0m %s\n" "$1"; }
info() { printf "     %s\n" "$1"; }

# ---------------------------------------------------------------- DNS
check_dns() {
  local ip
  ip="$("$PY" -c "import socket,sys
try:
    print(socket.gethostbyname('$HOST'))
except Exception:
    sys.exit(0)" 2>/dev/null)"

  if [ -z "$ip" ]; then
    warn "解析不到 ${HOST}（离线？）"
    return 1
  fi

  case "$ip" in
    198.18.*|198.19.*|198.51.100.*)
      bad "${HOST} 解析成 ${ip} —— 是本地代理的 fake-ip 劫持，后端同样会拿到这个假地址"
      info "修：echo '${REAL_IP} ${HOST}' | sudo tee -a /etc/hosts"
      info "（去掉：sudo sed -i '' '/${HOST}/d' /etc/hosts）"
      return 1
      ;;
    "$REAL_IP")
      ok "${HOST} → ${ip}（真实地址）"
      return 0
      ;;
    *)
      warn "${HOST} → ${ip}（与记录的 ${REAL_IP} 不同；若桶迁移过请更新脚本里的 REAL_IP）"
      return 0
      ;;
  esac
}

# ---------------------------------------------------------------- 密钥
ensure_keys() {
  local ak sk
  ak="$(grep -E '^GARAGE_ACCESS_KEY=' "$ENVLOCAL" 2>/dev/null | head -1 | cut -d= -f2-)"
  sk="$(grep -E '^GARAGE_SECRET_KEY=' "$ENVLOCAL" 2>/dev/null | head -1 | cut -d= -f2-)"

  if [ -n "$ak" ] && [ -n "$sk" ]; then
    ok ".env.local 已有 GARAGE_ACCESS_KEY / GARAGE_SECRET_KEY"
    return 0
  fi

  # 从 sys.pro.config 现有配置迁移过来（那对凭据本就明文在库里，
  # 挪到 gitignored 的 .env.local 是**改善**，不是扩散）
  local pak psk
  pak="$(awk '/access_key/{print $3}' "$ROOT/config/sys.pro.config" 2>/dev/null | head -1 | tr -d '<>"')"
  psk="$(awk '/secret_key/{print $3}' "$ROOT/config/sys.pro.config" 2>/dev/null | head -1 | tr -d '<>"')"

  if [ -z "$pak" ] || [ -z "$psk" ]; then
    bad "取不到凭据：请手工写入 ${ENVLOCAL}"
    info "GARAGE_ACCESS_KEY=<access key>"
    info "GARAGE_SECRET_KEY=<secret key>"
    return 1
  fi

  [ -f "$ENVLOCAL" ] || : > "$ENVLOCAL"
  [ -n "$ak" ] || echo "GARAGE_ACCESS_KEY=${pak}" >> "$ENVLOCAL"
  [ -n "$sk" ] || echo "GARAGE_SECRET_KEY=${psk}" >> "$ENVLOCAL"
  ok "已从 sys.pro.config 把凭据写入 .env.local（gitignored）"
  warn "这对密钥**明文在 git 历史里**，建议尽快轮换"
  return 0
}

# ---------------------------------------------------------------- 当前态
current_mode() {
  # 只看非注释行：配置注释里会提到公网地址
  if grep -vE '^[[:space:]]*%' "$CONFIG" 2>/dev/null |
     grep -qE "public_endpoint[[:space:]]*=>[[:space:]]*<<\"https://${HOST}\">>"; then
    echo "public"
  else
    echo "local"
  fi
}

# ---------------------------------------------------------------- 动作
do_status() {
  echo "== 对象存储现状 =="
  local mode
  mode="$(current_mode)"
  if [ "$mode" = "public" ]; then
    ok "当前：公网桶 ${HOST}（前缀 ${PREFIX}）"
  else
    warn "当前：本地 garage（内网，模型抓不到 → AI 回课必然失败）"
  fi
  check_dns
  if [ "$mode" = "public" ]; then
    ensure_keys
  fi
  echo ""
  echo "== 运行时实际生效的值（来自运行节点，若节点未起则跳过）=="
  return 0
}

do_apply() {
  echo "== 切换到公网桶 =="
  if [ "$(current_mode)" = "public" ]; then
    ok "已经是公网桶模式，无需重复切换"
    return 0
  fi

  check_dns || return 1
  ensure_keys || return 1

  cp "$CONFIG" "$CONFIG.moyabak"
  if ! "$PY" - "$CONFIG" "$PREFIX" "$HOST" <<'PY'
import io, re, sys

path, prefix, host = sys.argv[1], sys.argv[2], sys.argv[3]
s = io.open(path, encoding="utf-8").read()

m = re.search(r"\{garage,\s*#\{(.*?)\}\},", s, re.S)
if not m:
    sys.exit("没找到 garage 配置块，未做任何改动")

# 逐项替换，保留块内的注释与未提及的键（public_bucket / public_base_url 等）
mapping = {
    "endpoint":         '<<"https://%s">>' % host,
    "public_endpoint":  '<<"https://%s">>' % host,
    "access_key":       '{env, <<"GARAGE_ACCESS_KEY">>}',
    "secret_key":       '{env, <<"GARAGE_SECRET_KEY">>}',
    "key_prefix":       '<<"%s">>' % prefix,
}

lines = m.group(1).split("\n")
out, seen = [], set()
for ln in lines:
    st = ln.strip()
    if st.startswith("%"):          # 注释行原样保留
        out.append(ln)
        continue
    mm = re.match(r"(\w+)\s*=>\s*(.+?),?\s*$", st)
    if mm and mm.group(1) in mapping:
        k = mm.group(1)
        indent = ln[: len(ln) - len(ln.lstrip())]
        out.append("%s%s => %s," % (indent, k, mapping[k]))
        seen.add(k)
    else:
        out.append(ln)

for k, v in mapping.items():        # 原本没有的键补到块尾
    if k not in seen:
        out.append("            %s => %s," % (k, v))

# Erlang 的 map 不允许 trailing comma，而块尾紧接 `}}` ⇒ 去掉最后一项的逗号
while out and not out[-1].strip():
    out.pop()
if out and out[-1].rstrip().endswith(","):
    out[-1] = out[-1].rstrip()[:-1]

s = s[: m.start(1)] + "\n".join(out) + s[m.end(1) :]
io.open(path, "w", encoding="utf-8").write(s)
print("    已改写 garage 配置块（保留未提及的键）")
PY
  then
    bad "改写配置失败，已回滚"
    mv "$CONFIG.moyabak" "$CONFIG"
    return 1
  fi

  # 改写是文本替换，必须回读验证：语法（trailing comma 会让节点起不来）
  # + 语义（键名写错 / 前缀没进去 / 密钥忘了走 env，纯语法校验都抓不到）
  local vout
  vout="$(escript "$ROOT/scripts/dev_verify_oss_config.escript" "$CONFIG" "$PREFIX" 2>&1)"
  if ! printf '%s' "$vout" | grep -q "OSS_CONFIG_OK"; then
    bad "改写后校验不通过，已回滚："
    printf '%s\n' "$vout" | sed 's/^/       /'
    mv "$CONFIG.moyabak" "$CONFIG"
    return 1
  fi
  ok "改写后校验通过（语法 + 语义）"

  ok "已切换（备份在 $(basename "$CONFIG").moyabak）"
  info "重启节点后生效；临时验证可跑 scripts/dev_hot_reload_oss.sh（若存在）"
  warn "历史素材仍在本地桶 → 见 scripts/dev_migrate_media.sh（若存在）"
  return 0
}

do_revert() {
  echo "== 恢复本地 garage =="
  if [ ! -f "$CONFIG.moyabak" ]; then
    warn "没有备份，无需恢复"
    return 0
  fi
  mv "$CONFIG.moyabak" "$CONFIG"
  ok "已恢复（原备份内容）"
  info "重启节点后生效"
  return 0
}

case "${1:-status}" in
  status) do_status ;;
  apply)  do_apply ;;
  revert) do_revert ;;
  *)
    echo "用法: $0 [status|apply|revert]" >&2
    exit 2
    ;;
esac
