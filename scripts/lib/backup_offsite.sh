#!/usr/bin/env bash
# ============================================================
# 备份加密与异地副本 / Backup encryption + off-site replication
# ------------------------------------------------------------
# 被 scripts/backup_pg.sh 引用（source）。
#
# 为什么需要（B-29）：此前备份只落在**被备份的那台机器**上，且是明文。
#   - 机器没了 = 备份也没了，备份等于没做
#   - 谁能读那个目录，谁就拿到全库用户数据（含私信密文与元数据）
#
# 两件事都**默认关闭**，必须显式配置才生效：
#   BACKUP_ENCRYPT_RECIPIENT  加密收件人（age 公钥 或 gpg key id/邮箱）
#   BACKUP_OFFSITE_DEST       rclone 目标，如 s3remote:imboy-backup/pg
# 未配置时脚本会**大声警告但不失败** —— 本地开发不该被这两项卡住，
# 但生产上"没有异地副本"必须是刺眼的，不能悄悄跳过。
#
# ⚠️ 加密用**非对称收件人**而非口令：私钥留在离线处，服务器只有公钥，
#   因此服务器被攻破也解不开自己的历史备份。用口令的话口令一定就在这台机器上。
#
# ⚠️ **只加密离开本机的那一份，本地保留明文**。原因不是图省事：
#   私钥按设计不在服务器上 → 服务器解不开自己加密的备份 → 每日恢复演练
#   （scripts/restore_smoke.sh，B-21/B-22）就做不成了。
#   而"备份能恢复"这件事必须每天被验证，优先级高于"本地那份也加密"——
#   何况 PG 的数据目录本来就在同一台机器上，本地明文的边际暴露很小。
#   代价是本地副本仍需靠文件系统权限保护。
# ============================================================

BACKUP_ENCRYPT_RECIPIENT="${BACKUP_ENCRYPT_RECIPIENT:-}"
BACKUP_OFFSITE_DEST="${BACKUP_OFFSITE_DEST:-}"
BACKUP_OFFSITE_TIMEOUT="${BACKUP_OFFSITE_TIMEOUT:-1800}"

# 选择可用的加密工具：age 优先（收件人就是一个字符串，无需 keyring），
# 否则回落 gpg（服务器上更常见）。两者都没有则报错——不静默降级成明文。
_pick_encryptor() {
  if command -v age >/dev/null 2>&1; then echo age
  elif command -v gpg >/dev/null 2>&1; then echo gpg
  else echo ""
  fi
}

# 把明文加密成一个**临时密文**（不动原文件），回显密文路径。
# 未配置收件人则回显空串，调用方据此决定是否明文上传。
# 参数：<plaintext_path>
_encrypt_to_temp() {
  local src="$1"
  local tool; tool="$(_pick_encryptor)"
  local enc

  case "$tool" in
    age)
      enc="${src}.age"
      age -r "$BACKUP_ENCRYPT_RECIPIENT" -o "$enc" "$src" || return 1
      ;;
    gpg)
      enc="${src}.gpg"
      # --trust-model always：备份场景下收件人由运维显式指定，不走 web-of-trust
      gpg --batch --yes --trust-model always \
          --recipient "$BACKUP_ENCRYPT_RECIPIENT" \
          --output "$enc" --encrypt "$src" || return 1
      ;;
    *)
      echo "[backup_offsite] ERROR: 已配置 BACKUP_ENCRYPT_RECIPIENT 但 age/gpg 都不可用" >&2
      return 1
      ;;
  esac

  # 密文非空才算成功 —— 加密工具静默产出空文件时不能当成功往外推
  [ -s "$enc" ] || { echo "[backup_offsite] ERROR: 密文为空" >&2; rm -f "$enc"; return 1; }
  printf '%s' "$enc"
}

# 推送到异地：**先加密再上传，上传完删掉临时密文**，本地保留明文供恢复演练。
# 未配置 BACKUP_OFFSITE_DEST 则警告后跳过（返回 0）。
# 参数：<plaintext_file_path>
push_offsite() {
  local f="$1" upload="$1" tmp_enc=""

  if [ -z "$BACKUP_OFFSITE_DEST" ]; then
    echo "[backup_offsite] ⚠️  BACKUP_OFFSITE_DEST 未设置：**没有异地副本**，本机故障即全部丢失" >&2
    return 0
  fi
  if ! command -v rclone >/dev/null 2>&1; then
    echo "[backup_offsite] ERROR: 已配置异地目标但 rclone 未安装" >&2
    return 1
  fi

  if [ -n "$BACKUP_ENCRYPT_RECIPIENT" ]; then
    tmp_enc="$(_encrypt_to_temp "$f")" || return 1
    upload="$tmp_enc"
    echo "[backup_offsite] 已加密待上传：$(basename "$upload")" >&2
  else
    echo "[backup_offsite] ⚠️  BACKUP_ENCRYPT_RECIPIENT 未设置：将以**明文**推送到异地" >&2
  fi

  local rc=0
  # timeout 是 GNU coreutils 的东西，macOS 默认没有（测试里实测踩到）。
  # 有就用、没有就直接跑 —— 为了一个超时保护而让整个异地推送在某些平台上
  # 必然失败，是把小问题换成大问题。
  local -a TIMEOUT_CMD=()
  if command -v timeout >/dev/null 2>&1; then
    TIMEOUT_CMD=(timeout "$BACKUP_OFFSITE_TIMEOUT")
  elif command -v gtimeout >/dev/null 2>&1; then
    TIMEOUT_CMD=(gtimeout "$BACKUP_OFFSITE_TIMEOUT")
  else
    echo "[backup_offsite] 提示：无 timeout 命令，本次推送不设超时" >&2
  fi

  # --immutable 让目标端已存在的同名文件不被覆盖：异地副本是最后一道防线，
  # 不能因为本地一次坏备份就把远端的好副本盖掉。
  if "${TIMEOUT_CMD[@]+"${TIMEOUT_CMD[@]}"}" \
       rclone copy --immutable "$upload" "$BACKUP_OFFSITE_DEST" >&2; then
    echo "[backup_offsite] 已推送异地：${BACKUP_OFFSITE_DEST}/$(basename "$upload")" >&2
  else
    echo "[backup_offsite] ERROR: 异地推送失败 dest=${BACKUP_OFFSITE_DEST}" >&2
    rc=1
  fi

  [ -n "$tmp_enc" ] && rm -f "$tmp_enc"
  return $rc
}
