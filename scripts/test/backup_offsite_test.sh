#!/usr/bin/env bash
# ============================================================
# 备份加密与异地副本测试 / Backup encryption + off-site tests
# ------------------------------------------------------------
# 覆盖 B-29。用假的 rclone/gpg 桩，**不连网、不需要真实远端**。
# 用法: bash scripts/test/backup_offsite_test.sh
# ============================================================
set -uo pipefail
cd "$(dirname "$0")/../.."

PASS=0; FAIL=0
RED='\033[0;31m'; GREEN='\033[0;32m'; NC='\033[0m'
ok()  { echo -e "${GREEN}  PASS${NC} $1"; PASS=$((PASS+1)); }
bad() { echo -e "${RED}  FAIL${NC} $1"; echo "    ${2:-<空>}"; FAIL=$((FAIL+1)); }

STUB="$(mktemp -d)"
WORK="$(mktemp -d)"
trap 'rm -rf "$STUB" "$WORK"' EXIT

# 假 rclone：把"上传"记录到文件，便于断言上传的是哪个文件
# 假 rclone：把全部位置参数记下来。
# ⚠️ 别只记"最后一个非 - 参数" —— 那是**目的地**不是源文件，第一版这么写导致
#    断言恒不命中（测试自身的 bug，不是被测代码的）。
cat > "$STUB/rclone" <<'EOS'
#!/usr/bin/env bash
for a in "$@"; do case "$a" in -*) ;; *) echo "$a" >> "${RCLONE_LOG}";; esac; done
exit 0
EOS
# 假 age：产出一个非空"密文"
cat > "$STUB/age" <<'EOS'
#!/usr/bin/env bash
OUT=""; SRC=""
while [ $# -gt 0 ]; do
  case "$1" in
    -r) shift 2 ;;
    -o) OUT="$2"; shift 2 ;;
    *) SRC="$1"; shift ;;
  esac
done
printf 'ENCRYPTED:' > "$OUT"; cat "$SRC" >> "$OUT"
EOS
chmod +x "$STUB/rclone" "$STUB/age"
export PATH="$STUB:$PATH"
export RCLONE_LOG="$WORK/rclone.log"

run_push() {  # run_push <recipient> <dest>
  : > "$RCLONE_LOG"
  PLAIN="$WORK/imboy_pro_20260101T000000Z.dump"
  printf 'PLAINTEXT-DUMP' > "$PLAIN"
  ( set +u
    BACKUP_ENCRYPT_RECIPIENT="$1" BACKUP_OFFSITE_DEST="$2"
    export BACKUP_ENCRYPT_RECIPIENT BACKUP_OFFSITE_DEST
    . scripts/lib/backup_offsite.sh
    push_offsite "$PLAIN" ) 2>"$WORK/err.log"
}

echo "== B-29 加密 + 异地副本 =="

# 1) 配了收件人 → 上传的必须是**密文**，且内容确实被加密工具处理过
run_push "age1testrecipient" "s3remote:imboy-backup/pg"
if grep -q '\.age$' "$RCLONE_LOG" 2>/dev/null; then
  ok "配置收件人后上传的是密文（$(grep '\.age$' "$RCLONE_LOG" | head -1 | xargs basename)）"
else
  bad "上传的不是密文" "$(cat "$RCLONE_LOG") / $(cat "$WORK/err.log")"
fi
# 反向：明文**不得**被上传
if grep -qE '\.dump$' "$RCLONE_LOG" 2>/dev/null; then
  bad "明文也被上传了" "$(cat "$RCLONE_LOG")"
else
  ok "明文未被上传"
fi

# 2) 本地明文必须**保留** —— 私钥不在服务器上，本地那份要留着给恢复演练用
if [ -f "$WORK/imboy_pro_20260101T000000Z.dump" ]; then
  ok "本地明文保留（恢复演练还能用）"
else
  bad "本地明文被删了（恢复演练将无备份可验）" ""
fi

# 3) 临时密文用完必须删掉，不能在备份目录里越积越多
if [ ! -f "$WORK/imboy_pro_20260101T000000Z.dump.age" ]; then
  ok "临时密文上传后已清理"
else
  bad "临时密文残留在备份目录" "$(ls "$WORK")"
fi

# 4) 未配异地目标 → 跳过但**必须警告**，不能静默
run_push "age1testrecipient" ""
if grep -q "没有异地副本" "$WORK/err.log"; then
  ok "未配异地目标时大声警告"
else
  bad "未配异地目标时没有警告（会被当成一切正常）" "$(cat "$WORK/err.log")"
fi
if [ ! -s "$RCLONE_LOG" ]; then
  ok "未配异地目标时不调用 rclone"
else
  bad "未配异地目标却仍上传" "$(cat "$RCLONE_LOG")"
fi

# 5) 未配收件人 → 明文上传，但必须警告
run_push "" "s3remote:imboy-backup/pg"
if grep -q "将以\*\*明文\*\*推送" "$WORK/err.log"; then
  ok "未配收件人时明文上传有警告"
else
  bad "未配收件人时明文上传无警告" "$(cat "$WORK/err.log")"
fi

# 6) 加密工具产出空密文 → 必须失败，不得把空文件当备份推上去
cat > "$STUB/age" <<'EOS'
#!/usr/bin/env bash
OUT=""
while [ $# -gt 0 ]; do case "$1" in -o) OUT="$2"; shift 2 ;; -r) shift 2 ;; *) shift ;; esac; done
: > "$OUT"
EOS
chmod +x "$STUB/age"
if run_push "age1testrecipient" "s3remote:imboy-backup/pg"; then
  bad "密文为空时仍返回成功" "$(cat "$WORK/err.log")"
else
  ok "密文为空时失败退出"
fi
if [ ! -s "$RCLONE_LOG" ]; then
  ok "密文为空时不上传"
else
  bad "密文为空却仍上传" "$(cat "$RCLONE_LOG")"
fi

echo
echo "总计: PASS=${PASS} FAIL=${FAIL}"
[ "$FAIL" -eq 0 ]
