#!/usr/bin/env bash
# ============================================================
# PG18 镜像目录换壳升级（Pro01 实战版，2026-11-25 prod 实证全绿）
# ------------------------------------------------------------
# 思路：停旧容器留壳 → 数据目录整份 cp -a → 同名同端口挂副本起新镜像
#       → 立即 ALTER EXTENSION → restart 清连接风暴残留 → 时间窗自检
# 依据：PG 18.x 小版本二进制兼容；数据目录自带全部角色/密码（全程无账号参数）；
#       beam 无需停止——同名同端口接管后应用重连自愈（vm.args -heart 同理）。
#
# 用法:
#   bash scripts/pg18_directory_swap_upgrade.sh                 # 升级（停机约 2 分钟）
#   bash scripts/pg18_directory_swap_upgrade.sh cleanup          # 清升级过程中间残留（随时可跑）
#   bash scripts/pg18_directory_swap_upgrade.sh cleanup-anchors  # 清 7 天回滚锚（满观察期才放行）
#
# 复用：下次升级只改 IMG 与 NEW_DIR 两个词。
# 前提：新镜像已 docker pull 且 digest 与审计一致；bind-mount 数据目录；
#       半瘫窗口（起新容器→ALTER 完成）刷错属正常，ALTER 前全库查询必报错。
# ============================================================
set -u
IMG="${IMG:-imboy/pg18:3.7.0-1}"                  # ← 下次升级改这里
C="${C:-prod_imboy_pg18}"                          # 线上固定容器名，不改
BAK="${BAK:-$C.bak}"                               # 旧容器壳回滚锚
DIR="${DIR:-/data/docker/prod_imboy_pg18}"         # 现役数据目录（宿主机）
NEW_DIR="${NEW_DIR:-${DIR}_v370}"                  # ← 下次升级改这里
U="${U:-imboy_user}"; D="${D:-imboy_pro}"
PGDATA_PIN="${PGDATA_PIN:-/var/lib/postgresql/18/docker}"
PORT="${PORT:-5182}"
HEALTHZ="${HEALTHZ:-http://127.0.0.1:9800/healthz}"
STATE="${STATE:-/data/docker/pg18_upgrade.state}"
OLD_IMG="${OLD_IMG:-imboy/pg18:3.6.1-2}"           # cleanup-anchors 顺带清理的旧镜像

cmd="${1:-upgrade}"
say(){ printf '[pg18 %s] %s\n' "$(date +%H:%M:%S)" "$*"; }

do_cleanup(){
  say "清理升级过程中间残留（不动回滚锚与自有 cron 备份）"
  rm -f /tmp/pg18_errs /tmp/mig.dump /tmp/d.dump /tmp/f.dump /tmp/dump_*.dump
  rm -f /root/pg18_upgrade_370.sh /root/switch_pg370.sh
  rm -f /root/pg18_baseline_*.txt /root/pg18_upgrade_*.log
  rm -f /data/docker/pg_backup/mig_*.dump
  say "完成。回滚锚（$BAK 容器 + $DIR 目录 + 旧镜像）满 7 天观察期后用 cleanup-anchors 清"
}

do_cleanup_anchors(){
  live="$(docker inspect "$C" --format '{{range .Mounts}}{{.Source}}{{end}}' 2>/dev/null || true)"
  [ -d "$live" ] || { say "❌ 读不到当前容器挂载目录（容器在跑吗），拒绝清理"; exit 1; }
  [ "$live" != "$DIR" ] || { say "❌ 活数据目录=待删原件目录，拒绝清理"; exit 1; }
  [ "$NEW_DIR" != "$DIR" ] || { say "❌ NEW_DIR 与原件目录相同，拒绝清理"; exit 1; }
  ts="$(grep -E '^TS_EPOCH=' "$STATE" 2>/dev/null | cut -d= -f2 || true)"
  if [ -z "$ts" ]; then
    say "❌ 无状态文件 $STATE（不知升级时间，如本次 2026-11-25 手工升级）"; [ "${FORCE:-0}" = 1 ] || exit 1
  elif [ "${FORCE:-0}" != 1 ]; then
    age=$(( ( $(date +%s) - ts ) / 86400 ))
    [ "$age" -ge 7 ] || { say "❌ 升级仅 ${age} 天，未满 7 天观察期（FORCE=1 强制）"; exit 1; }
  fi
  say "清理旧容器壳 $BAK"; docker rm "$BAK" >/dev/null 2>&1 || say "  不存在，跳过"
  say "清理原件目录 $DIR（回滚物理锚，确认活数据在 $NEW_DIR 才执行到这里）"; rm -rf "$DIR"
  say "清理旧镜像 $OLD_IMG"; docker rmi "$OLD_IMG" >/dev/null 2>&1 || true
  say "清理完成"
}

[ "$cmd" = "cleanup" ] && { do_cleanup; exit 0; }
[ "$cmd" = "cleanup-anchors" ] && { do_cleanup_anchors; exit 0; }
[ "$cmd" = "upgrade" ] || { say "用法: upgrade|cleanup|cleanup-anchors"; exit 2; }

echo "1/6 停旧容器并留壳"
docker stop "$C" >/dev/null && docker rename "$C" "$BAK" \
  || { say "❌ 停/改名失败"; exit 1; }

echo "2/6 数据目录整份拷贝（原件 $DIR 即回滚锚）"
[ -e "$NEW_DIR" ] \
  && { say "❌ $NEW_DIR 已存在（上次升级残留或活数据）。确认后手动处理或改 NEW_DIR，防 cp 嵌套出假库"; exit 1; }
cp -a "$DIR" "$NEW_DIR" \
  || { say "❌ 拷贝失败。恢复: docker rename $BAK $C && docker start $C"; exit 1; }

echo "3/6 原名原端口挂新目录起新镜像（无账号密码参数，角色随目录走）"
docker run -d --name "$C" -p "$PORT:5432" -v "$NEW_DIR:/var/lib/postgresql" \
  -e "PGDATA=$PGDATA_PIN" -e TZ=Asia/Shanghai \
  --restart unless-stopped "$IMG" postgres \
  || { say "❌ 起容器失败。恢复: docker rm -f $C && docker rename $BAK $C && docker start $C"; exit 1; }
for _ in $(seq 1 60); do docker exec "$C" pg_isready -U "$U" -d "$D" >/dev/null 2>&1 && break; sleep 1; done
docker exec "$C" pg_isready -U "$U" -d "$D" >/dev/null || { say "❌ 新容器未就绪"; exit 1; }

echo "4/6 立即升级扩展（事务性秒级；此刻前日志刷错=正常半瘫窗口）"
docker exec "$C" psql -X -U "$U" -d "$D" -c "ALTER EXTENSION timescaledb UPDATE" \
  || say "  ⚠️ timescaledb ALTER 未确认（可能 too many clients；若真失败，步骤5重启后半瘫错误墙会持续并被自检兜住）"
docker exec "$C" psql -X -U "$U" -d "$D" -c "ALTER EXTENSION postgis UPDATE" \
  || say "  postgis 未确认（无新版或连接满；无新版属正常）"

echo "5/6 重启容器清重连风暴残留连接并自检"
docker restart "$C" >/dev/null
for _ in $(seq 1 60); do docker exec "$C" pg_isready -U "$U" -d "$D" >/dev/null 2>&1 && break; sleep 1; done
docker exec "$C" psql -X -U "$U" -d "$D" -Atc "select extname||':'||extversion from pg_extension order by 1;"
sleep 8
docker logs --since 1m "$C" 2>&1 | grep -iE "could not access" > /tmp/pg18_errs || true
if [ -s /tmp/pg18_errs ]; then say "❌ 仍有扩展加载错误:"; cat /tmp/pg18_errs; exit 1; fi

echo "6/6 完成（beam 自动重连，无需重启节点）"
printf 'TS_EPOCH=%s\nIMG=%s\nNEW_DIR=%s\n' "$(date +%s)" "$IMG" "$NEW_DIR" > "$STATE"
say "healthz: $(curl -fsS --max-time 5 "$HEALTHZ" 2>/dev/null || echo '应用重连中，稍后自查')"
say "回滚(丢切换后新写入): docker stop $C && docker rm $C && docker rename $BAK $C && docker start $C"
say "过程残留清理: bash $0 cleanup ；满7天清锚: bash $0 cleanup-anchors"
