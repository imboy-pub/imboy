#!/usr/bin/env bash
# release_gate_check.sh — w2-release-runbook.md §一 前置清单机检
#
# 只读本地：不访问生产、不 push、不打印任何密钥值（密钥只报 SET/UNSET）。
# 输出每项 PASS/FAIL/WARN/MANUAL + 发布日前「剩余人工项」汇总。
#
# 用法：
#   bash scripts/release_gate_check.sh [快照目录]
#   快照目录缺省时自动扫 /tmp/sd_*（H3 项降级为 WARN，不阻断清单展示）。
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WS="$(cd "$ROOT/.." && pwd)"
SNAP_DIR="${1:-}"
PASS=0; FAIL=0; WARN=0; MANUAL=0

ok()    { printf '  \033[0;32mPASS\033[0m   %s\n' "$1"; PASS=$((PASS+1)); }
fail()  { printf '  \033[0;31mFAIL\033[0m   %s\n' "$1"; FAIL=$((FAIL+1)); }
warn()  { printf '  \033[0;33mWARN\033[0m   %s\n' "$1"; WARN=$((WARN+1)); }
manual(){ printf '  \033[0;36m人工\033[0m   %s\n' "$1"; MANUAL=$((MANUAL+1)); }
title() { printf '\n== %s ==\n' "$1"; }

title "1) H4 push 状态（三仓，只读检查）"
for pair in "imboy:普通推" "imboyapp:普通推" "imboyadmin:须先 fetch 再 --force-with-lease（历史已重写）"; do
    repo="${pair%%:*}"; mode="${pair#*:}"
    d="$WS/$repo"
    if [ ! -d "$d/.git" ]; then fail "$repo 仓不存在"; continue; fi
    dirty=$(git -C "$d" status --porcelain | wc -l | tr -d ' ')
    ahead=$(git -C "$d" rev-list --count origin/main..HEAD 2>/dev/null || echo '?')
    if [ "$dirty" != "0" ]; then
        warn "${repo}：工作区有 $dirty 个未提交文件（先确认归属再处理）"
    fi
    if [ "$ahead" = "0" ]; then
        ok "${repo}：已与 origin/main 同步"
    else
        manual "${repo}：领先 origin/main $ahead 笔未推（${mode}）——按 w2-h4-commit-execution-plan.md §五 由用户指令执行"
        signed=$(git -C "$d" log "origin/main..HEAD" --format='%(trailers:key=Signed-off-by,valueonly,only)' | grep -vc '^$')
        if [ "${signed}" = "$ahead" ]; then
            ok "${repo}：DCO 签名 $ahead/$ahead 全覆盖"
        else
            fail "${repo}：DCO 签名 ${signed}/${ahead}——补签须用户确认 committer 身份后重写历史（tag 需同步重打）"
        fi
    fi
done

title "2) 版本物（alpha.71 定版材料）"
vsn=$(cat "$ROOT/VERSION" 2>/dev/null || echo "")
if [ -z "$vsn" ]; then fail "VERSION 文件缺失"; fi
if grep -q '^## \[Unreleased\]' "$ROOT/CHANGELOG.md"; then
    manual "VERSION=${vsn}，CHANGELOG 有 [Unreleased] 待转正——定版时同步改 VERSION + CHANGELOG + 三仓 tag"
else
    warn "CHANGELOG 无 [Unreleased] 节（已定版或缺失，请人工确认 VERSION=$vsn 是否为目标版本）"
fi

title "3) 备份工具（生产备份用，恢复路径已被 H3 预演实证）"
for s in scripts/backup_pg.sh scripts/backup_imboy_db.sh; do
    if [ -x "$ROOT/$s" ]; then ok "$s 存在且可执行"; else fail "$s 缺失或不可执行"; fi
done
manual "生产库备份的实际执行（服务器侧跑 backup_pg.sh + backup_imboy_db.sh）"

title "4) H3 脱敏快照（本地产物校验；生产快照为发布日动作）"
if [ -z "$SNAP_DIR" ]; then
    SNAP_DIR=$(ls -dt /tmp/sd_* 2>/dev/null | head -1 || true)
fi
if [ -z "$SNAP_DIR" ] || [ ! -d "$SNAP_DIR" ]; then
    warn "未找到快照目录（可传参：release_gate_check.sh <快照目录>）——生产跑 sanitized_snapshot.sh 属发布日人工项"
else
    dump="$SNAP_DIR/sanitized.dump"; mf="$SNAP_DIR/manifest.txt"
    if [ -f "$dump" ] && [ -f "$mf" ]; then
        want=$(grep -o '^sha256=.*' "$mf" | head -1 | cut -d= -f2)
        got=$(shasum -a 256 "$dump" 2>/dev/null | cut -d' ' -f1)
        age_days=$(( ( $(date +%s) - $(stat -f %m "$dump") ) / 86400 ))
        if [ "$want" = "$got" ]; then
            if [ "$age_days" -le 7 ]; then
                ok "快照 ${SNAP_DIR}：sha256 校验一致，$age_days 天前生成"
            else
                warn "快照 ${SNAP_DIR}：sha256 一致但已 $age_days 天，发布日建议用生产库重新生成"
            fi
        else
            fail "快照 ${SNAP_DIR}：sha256 不匹配（manifest=$want 实际=$got）"
        fi
    else
        fail "快照目录 $SNAP_DIR 缺 sanitized.dump / manifest.txt"
    fi
fi
manual "生产库跑 scripts/sanitized_snapshot.sh（维护窗口）+ h3_rehearsal.sh 演练复核，或用户明示豁免"

title "5) 生产五密钥（IMBOY_* env 注入，只报 SET/UNSET）"
KEYS="IMBOY_JWT_KEY IMBOY_POSTGRE_AES_KEY IMBOY_ADM_COOKIE_SECRET IMBOY_SOLIDIFIED_KEY IMBOY_SOLIDIFIED_KEY_IV IMBOY_PASSWORD_SALT"
unset_n=0
for k in $KEYS; do
    if [ -n "${!k:-}" ]; then ok "$k=SET"; else warn "$k=UNSET（发布日经 env 注入，不落文件）"; unset_n=$((unset_n+1)); fi
done
[ "$unset_n" = "0" ] || manual "密钥清单中 ${unset_n} 项未注入——生产发布命令行须携带（value 由用户掌握，勿写入任何文件）"

title "6) H2 残余（不可机检，人工声明收齐或豁免）"
manual "第二台真机：Push 端到端（FCM/APNs 凭据亦未配）+ 音视频对端"
manual "FCM/APNs 凭据：Firebase Console 取 project id + service account OAuth；APNs key（iOS 推送）"
manual "3 人 30 秒理解测试"
manual "以上任一未收齐 → 本次发布定名「alpha 发布」，公告不得声称正式 Release"

total=$((PASS+FAIL+WARN+MANUAL))
printf '\n== 汇总 ==\n'
printf 'PASS=%d FAIL=%d WARN=%d 人工项=%d / 共 %d 项\n' "$PASS" "$FAIL" "$WARN" "$MANUAL" "$total"
printf '结论：本脚本只核本地可机检面；上面列出的「人工」项即发布日前剩余人工门。\n'
[ "$FAIL" = "0" ] || exit 1
