#!/usr/bin/env bash
# gen_login_rsa_keys.sh — 新环境登录 RSA 密钥初始化
#
# 用途：为 imboy 登录密码加密（管理后台/移动端 passport）生成 RSA-2048 密钥对。
# 前端契约：WebCrypto RSA-OAEP-SHA256，公钥必须为 SPKI 格式（BEGIN PUBLIC KEY），
#           PKCS#1（BEGIN RSA PUBLIC KEY）不被接受。
#
# 用法：
#   ./gen_login_rsa_keys.sh [keys_dir]                  # 生成密钥对（默认 ./imboy_keys）
#   POSTGRE_AES_KEY=xxx ./gen_login_rsa_keys.sh --sql [keys_dir]
#                                                       # 额外输出旧版 config 表入库 SQL
# 环境变量：
#   POSTGRE_AES_KEY — 部署 sys.config 里的 postgre_aes_key（16/24/32 字节），
#                     仅 --sql 模式需要；不要把真实密钥写进脚本或提交到仓库。
set -euo pipefail

KEYS_DIR="imboy_keys"
EMIT_SQL=0
for arg in "$@"; do
  case "$arg" in
    --sql) EMIT_SQL=1 ;;
    *) KEYS_DIR="$arg" ;;
  esac
done

PUB="$KEYS_DIR/login_rsa_pub.pem"
PRIV="$KEYS_DIR/login_rsa_priv.pem"

mkdir -p "$KEYS_DIR"
chmod 700 "$KEYS_DIR"

if [ -s "$PRIV" ]; then
  echo "==> 私钥已存在，跳过生成: $PRIV"
else
  echo "==> 生成 RSA-2048 私钥..."
  openssl genrsa -out "$PRIV" 2048 2>/dev/null
fi

if [ -s "$PUB" ]; then
  echo "==> 公钥已存在，跳过导出: $PUB"
else
  echo "==> 导出 SPKI 公钥（BEGIN PUBLIC KEY，WebCrypto 兼容）..."
  openssl rsa -in "$PRIV" -pubout -out "$PUB" 2>/dev/null
fi

chmod 600 "$PRIV"
chmod 644 "$PUB"

# --- 自检 ---------------------------------------------------------------
grep -q "BEGIN PUBLIC KEY" "$PUB" || { echo "!! 公钥不是 SPKI 格式（缺少 BEGIN PUBLIC KEY 头）"; exit 1; }
if grep -q "BEGIN RSA PUBLIC KEY" "$PUB"; then
  echo "!! 公钥是 PKCS#1 格式，前端 WebCrypto 不接受"
  exit 1
fi

PUB_BITS=$(openssl rsa -pubin -in "$PUB" -text -noout 2>/dev/null | head -1 | grep -o '[0-9]\+' | head -1)
[ "$PUB_BITS" = "2048" ] || { echo "!! 公钥位数异常: $PUB_BITS（期望 2048）"; exit 1; }

PRIV_MOD=$(openssl rsa  -in "$PRIV" -noout -modulus 2>/dev/null | cut -d= -f2)
PUB_MOD=$(openssl rsa -pubin -in "$PUB" -noout -modulus 2>/dev/null | cut -d= -f2)
[ "$PRIV_MOD" = "$PUB_MOD" ] || { echo "!! 公私钥不匹配（modulus 不一致）"; exit 1; }

echo "==> 自检通过：RSA-2048 / SPKI 公钥 / 公私钥配对"
echo "    公钥指纹: $(openssl rsa -pubin -in "$PUB" -outform DER 2>/dev/null | openssl sha256 | awk '{print $NF}')"

# --- 接线指引 ------------------------------------------------------------
cat <<EOF

== 接线方式（二选一，取决于后端版本）==

[A] 当前版本（>= 2026-04-16 提交 0803ef5d4：密钥已从数据库迁移到文件路径配置）
    用环境变量指向这两个文件后重启 beam（无需改 sys.config）：
      export IMBOY_LOGIN_RSA_PUB_KEY_FILE=$(cd "$KEYS_DIR" && pwd)/login_rsa_pub.pem
      export IMBOY_LOGIN_RSA_PRIV_KEY_FILE=$(cd "$KEYS_DIR" && pwd)/login_rsa_priv.pem
    docker-compose 部署则写入 deploy/.env 并确认文件已挂载进容器。
    重启后验证（public_key 非空即正常）：
      curl -s http://127.0.0.1:8080/api/adm/passport/meta | jq '.data.public_key'

[B] 旧版本（< 0803ef5d4：密钥读数据库 config 表，提交注释明言"新部署需手动 config_ds:set"）
    首选：进运行节点 Erlang 远程 shell（bin/imboy remote 或 make ctl ARGS="remote"）执行：
      {ok, P} = file:read_file("$(cd "$KEYS_DIR" && pwd)/login_rsa_pub.pem").
      ok = config_ds:set(<<"login_rsa_pub_key">>, P).
      {ok, S} = file:read_file("$(cd "$KEYS_DIR" && pwd)/login_rsa_priv.pem").
      ok = config_ds:set(<<"login_rsa_priv_key">>, S).
    然后**重启 beam**（config 有长约 10 天的本地缓存，读空过一次就必须重启刷新）。
EOF

# --- 旧版纯 SQL 入库（可选，需 POSTGRE_AES_KEY） --------------------------
if [ "$EMIT_SQL" = "1" ]; then
  AES_KEY="${POSTGRE_AES_KEY:-}"
  if [ -z "$AES_KEY" ]; then
    echo "!! --sql 需要 POSTGRE_AES_KEY 环境变量（sys.config 的 postgre_aes_key）" >&2
    exit 1
  fi
  KEY_LEN=${#AES_KEY}
  case "$KEY_LEN" in 16|24|32) ;; *) echo "!! POSTGRE_AES_KEY 长度 $KEY_LEN 非 16/24/32 字节，AES-CBC 无法使用" >&2; exit 1;; esac

  SQL_FILE="$KEYS_DIR/seed_login_rsa.sql"
  command -v python3 >/dev/null || { echo "!! --sql 模式需要 python3（用于 JSON 编码）" >&2; exit 1; }

  # 密文在本地用 openssl 计算，SQL 里只落单行 aes_cbc_<b64> 字符串（无引号陷阱）。
  # 与 config_ds 写读格式逐层对齐（四层，缺一不可）：
  #   app 写：PEM → jsone:encode（JSON 文本）→ base64 文本 → pgcrypto encrypt(·,key,'aes-cbc/pad:pkcs')
  #           → base64 → 'aes_cbc_' 前缀
  #   app 读：逆序，最外 base64 解码后还有一层 base64 解码还原 JSON，再 jsone:decode 还原 PEM
  #   此处：python json.dumps → base64 → openssl AES-CBC（零 IV——config_ds 审计注释明言
  #         pgcrypto encrypt/3 固定全零 IV；PKCS7 padding = 'pad:pkcs'；密钥长 16/24/32 → aes-128/192/256）
  json_encode_b64() { python3 -c 'import json,sys,base64; sys.stdout.write(base64.b64encode(json.dumps(sys.stdin.read()).encode()).decode())'; }
  # $(cat) 会吞尾部换行；app 存的是含尾 LF 的文件原文，须保真
  PUB_RAW=$(cat "$PUB"; printf x); PUB_RAW="${PUB_RAW%x}"
  PRIV_RAW=$(cat "$PRIV"; printf x); PRIV_RAW="${PRIV_RAW%x}"
  encrypt_value() { # $1=AES_KEY(ASCII) $2=JSON 文本 → base64 密文
    local kh cipher
    kh=$(printf '%s' "$1" | od -An -tx1 | tr -d ' \n')
    cipher="aes-$(( ${#1} * 8 ))-cbc"
    printf '%s' "$2" | openssl enc -"$cipher" -K "$kh" -iv 00000000000000000000000000000000 -base64 -A -nosalt
  }

  PUB_CIPHER="aes_cbc_$(encrypt_value "$AES_KEY" "$(printf '%s' "$PUB_RAW" | json_encode_b64)")"
  PRIV_CIPHER="aes_cbc_$(encrypt_value "$AES_KEY" "$(printf '%s' "$PRIV_RAW" | json_encode_b64)")"

  {
    echo "-- 旧版 config 表密钥入库（密文已在本地按 config_ds 写读格式算好，本文件不含 AES 密钥）"
    echo "-- 明文链：PEM → JSON 字符串（应用层 config_ds:get 会 jsone:decode 解包）→ AES-CBC(零 IV) → base64"
    echo "-- AES 密钥须与本部署 postgre_aes_key 完全一致，否则 config_ds:get 解密失败"
    for KV in "login_rsa_pub_key|$PUB_CIPHER|登录RSA公钥" \
              "login_rsa_priv_key|$PRIV_CIPHER|登录RSA私钥"; do
      KEY="${KV%%|*}"; REST="${KV#*|}"; CIPHER="${REST%%|*}"; TITLE="${REST#*|}"
      echo ""
      echo "UPDATE public.config SET value = '$CIPHER', updated_at = now() WHERE key = '$KEY';"
      echo "INSERT INTO public.config (tab, key, value, title, sort, remark, system, status)"
      echo "SELECT 'sys', '$KEY', '$CIPHER', '$TITLE', 20, '', true, 1"
      echo "WHERE NOT EXISTS (SELECT 1 FROM public.config WHERE key = '$KEY');"
    done
    echo ""
    echo "-- 验证（psql 连接后把 <AES_KEY> 换成本部署 postgre_aes_key 再执行，应输出 PEM 头）："
    echo "-- SELECT left(decode(encode(decrypt(decode(replace(value,'aes_cbc_',''),'base64'), convert_to('<AES_KEY>','UTF8'), 'aes-cbc/pad:pkcs'), 'escape'), 'base64')::text, 40) FROM public.config WHERE key='login_rsa_pub_key';"
  } > "$SQL_FILE"

  chmod 600 "$SQL_FILE"
  echo ""
  echo "==> 已生成旧版入库 SQL: $SQL_FILE（含密钥密文，用完即删）"
  echo "    执行: psql -h <pg_host> -U <user> -d <db> -f $SQL_FILE ，随后重启 beam。"
fi
