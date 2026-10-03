# L4 SNI 手动模式恢复合同 / Manual recovery contract

状态：候选步骤，未创建本轮生产备份，未执行生产恢复演练。
本合同保持手动模式，不用安装器重复 `--apply` 接管。

## 当前配置来源 / Current source

2026-10-03 只读复采确认：

- nginx master 使用 `/www/server/nginx/sbin/nginx -c /www/server/nginx/conf/nginx.conf`。
- LiveKit 实际 Compose 文件为 `/etc/imboy/livekit-compose.yml`，工作目录为 `/etc/imboy`。
- 主机没有 `docker compose` 插件；实际命令为 `/usr/bin/docker-compose`，版本 v2.23.3。
- 用 `--env-file /etc/imboy/livekit.env` 渲染后，`LIVEKIT_CONFIG` 和 `TZ`
  与现役容器的对应环境值完全一致；比较在服务器内存中完成，未导出密钥。
- `config --services` 确认 service 名为 `imboy_livekit`。
- nginx TLS 路径除了 `/etc/letsencrypt`，还包括
  `/www/server/panel/vhost/cert/prodadm.imboy.pub/`；备份必须覆盖两者。
- 镜像引用为 `livekit/livekit-server@sha256:5d3dcc475d064536d9948ebe4eeab8e3b24d6f07a46f6d71a3415a2901bbdc52`。
- `/var/lib/imboy-livekit-l4-sni` 不存在，不能声称安装器 `--rollback` 可用。

以上事实属于采样时点。执行备份或恢复前须再次核对；不能用历史 overlay
组合替代当前单文件 Compose。变更现场组合时必须重新冻结此合同。

## 备份候选 / Backup candidate

精确写入对象：`/root/imboy-l4-sni-manual-backups/<唯一RUN_ID>/`。
仅新增该目录及其快照，不改现役配置，不重建容器、不重载服务。
须取得针对该对象的服务器写入确认后执行；目录存在即停止，禁止覆盖。

```bash
# 在生产主机的 root bash 中运行；RUN_ID 由本轮执行记录固定。
set -euo pipefail
umask 077
: "${RUN_ID:?必须设置唯一且已确认的RUN_ID}"
case "$RUN_ID" in *[!a-zA-Z0-9_-]*|'') exit 2;; esac
SNAPSHOT="/root/imboy-l4-sni-manual-backups/$RUN_ID"
test ! -e "$SNAPSHOT"
mkdir -p "$SNAPSHOT"
chmod 0700 "$SNAPSHOT"
NGINX=/www/server/nginx/sbin/nginx
CONF=/www/server/nginx/conf/nginx.conf
"$NGINX" -c "$CONF" -T > "$SNAPSHOT/nginx-effective.txt" 2> "$SNAPSHOT/nginx-test.log"
sed -n 's/^# configuration file \(.*\):$/\1/p' \
  "$SNAPSHOT/nginx-effective.txt" > "$SNAPSHOT/files.list"
test -s "$SNAPSHOT/files.list"
while IFS= read -r file; do
  test -r "$file"
  readlink -f -- "$file"
done < "$SNAPSHOT/files.list" > "$SNAPSHOT/resolved.list"
cat "$SNAPSHOT/resolved.list" >> "$SNAPSHOT/files.list"
python3 - "$SNAPSHOT/nginx-effective.txt" > "$SNAPSHOT/tls-files.list" <<'PY'
import pathlib, re, shlex, sys
text = pathlib.Path(sys.argv[1]).read_text()
paths = set()
for value in re.findall(r'^\s*ssl_certificate(?:_key)?\s+([^;]+);', text, re.M):
    parts = shlex.split(value, comments=True)
    if len(parts) != 1 or '$' in parts[0] or not parts[0].startswith('/'):
        raise SystemExit('BLOCKED_EVIDENCE: dynamic/relative TLS path')
    path = pathlib.Path(parts[0])
    paths.update([str(path), str(path.resolve(strict=True))])
if not paths:
    raise SystemExit('BLOCKED_EVIDENCE: TLS file list empty')
print('\n'.join(sorted(paths)))
PY
cat "$SNAPSHOT/tls-files.list" >> "$SNAPSHOT/files.list"
printf '%s\n' /etc/imboy/livekit-compose.yml /etc/imboy/livekit.env \
  /etc/imboy/livekit-l4-sni.env /etc/haproxy/haproxy.cfg /etc/letsencrypt \
  >> "$SNAPSHOT/files.list"
sort -u "$SNAPSHOT/files.list" -o "$SNAPSHOT/files.list"
tar -Pczf "$SNAPSHOT/configs.tar.gz" -T "$SNAPSHOT/files.list"
/usr/bin/docker-compose --project-directory /etc/imboy \
  --env-file /etc/imboy/livekit.env -f /etc/imboy/livekit-compose.yml \
  config > "$SNAPSHOT/compose-effective.yml"
systemctl show -p ActiveState -p UnitFileState haproxy eturnal cron \
  > "$SNAPSHOT/service-state.txt"
ps -C nginx -o pid=,args= > "$SNAPSHOT/nginx-processes.txt"
docker inspect --format '{{.Image}} {{.State.Status}}' imboy_livekit \
  > "$SNAPSHOT/livekit-state.txt"
cd "$SNAPSHOT"
tar -tzf configs.tar.gz > archive-members.txt
sha256sum configs.tar.gz compose-effective.yml nginx-effective.txt \
  service-state.txt nginx-processes.txt livekit-state.txt files.list resolved.list \
  tls-files.list nginx-test.log archive-members.txt \
  > SHA256SUMS
sha256sum -c SHA256SUMS
chmod 0600 ./*
```

快照包含配置、密钥和证书私钥，只能留在服务器的受保护目录。
本地证据仅登记目录、校验结果、文件数量与摘要；不得下载快照内容。
这是手动恢复快照，不能作为安装器 format=2 的 `--backup` 参数。
中途失败须保留失败目录用于调查，不得将其标记为有效或自动覆盖重试。

## 恢复前置与命令 / Restore prerequisites and commands

恢复为独立生产操作，须确认具体快照、维护窗口、责任人与服务影响。
先校验 SHA256SUMS，确认归档路径与快照清单一致，并审阅现役配置相对快照
的差异；存在本任务外的后续变更时停止，禁止整包覆盖共享配置。
先在隔离目录演练解包、文件模式和 Compose 渲染，证明环境值及镜像相同。

授权后的恢复顺序为：精确恢复已审阅文件 → 指定实例 `nginx -t` →
HAProxy 配置验证 → 使用上述相同 env/Compose 组合重建 `imboy_livekit` →
受控重载原 nginx 实例与 HAProxy → 巡检、端口 owner、HTTPS 和 LiveKit 验证。
恢复文件的清单和重载命令须由当次 diff 固定；本文不提供无条件整包解压入口。

LiveKit 重建命令仅限：

```bash
/usr/bin/docker-compose --project-directory /etc/imboy \
  --env-file /etc/imboy/livekit.env -f /etc/imboy/livekit-compose.yml \
  up -d --no-deps --force-recreate imboy_livekit
```

它会中断该实例会话，必须单独批准；不执行 `down`，不删除旧 TURN。
`haproxy -c -f /etc/haproxy/haproxy.cfg` 和指定 nginx `-t` 只证明配置可解析，
不能替代恢复后的实际健康与媒体验证。未完成恢复行为证明前，
`L4-T8-RECOVERY=BLOCKED_EVIDENCE`，生产恢复就绪仍未证明。
