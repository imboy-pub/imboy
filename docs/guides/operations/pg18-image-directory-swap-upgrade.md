# PG18 镜像目录换壳升级 Runbook（实战版）

> 适用：Pro01 形态——裸 `docker run` 的 `prod_imboy_pg18`（非 compose）、bind mount
> `/data/docker/prod_imboy_pg18`、backend 为宿主机原生 beam 节点。
> 脚本：`scripts/pg18_directory_swap_upgrade.sh`。
> **2026-11-25 已在 prod 实证全绿**（imboy/pg18:3.6.1-2 → 3.7.0-1，停机约 2 分钟）。
> 取代已删除的蓝绿物理同步方案（`pg18_bluegreen_upgrade.sh`，git 历史 `30e462862` 可查）。

## 原理

PG 18.x 小版本二进制兼容 → 数据目录整份 `cp -a` 副本直接给新镜像用，无需 dump/restore。
角色/密码/全部库随目录原样保留，重建容器**不传任何 `POSTGRES_*` 参数**
（容器 env 里的密码是 initdb 旧值，改过密码不回写——这是本方案不用账号参数的原因之一）。
beam 无需停止：同名同端口接管后应用重连自愈（vm.args `-heart` 存活时同理）。

## 前置

1. 新镜像已 `docker pull`，digest 与审计版本一致
2. `docker inspect prod_imboy_pg18 --format '{{range .Mounts}}{{.Source}}{{end}}'`
   确认 bind mount 与脚本 `DIR` 一致
3. 重建容器必须显式 `-e PGDATA=/var/lib/postgresql/18/docker`（钉死——新镜像默认
   PGDATA 若不同，空挂载目录会触发 initdb 造出假库）
4. 单写方假设：冻结期内应用写入会失败返回（PG 事务原子，无脏数据）

## 六步流程（脚本 `upgrade` 自动执行）

1. 停旧容器并改名留壳（`prod_imboy_pg18.bak`，回滚锚，不删）
2. `cp -a` 数据目录 → `NEW_DIR`（原件即物理回滚锚）
3. 同名同端口挂 `NEW_DIR` 起新镜像容器，等 `pg_isready`
4. **立即** `ALTER EXTENSION timescaledb / postgis UPDATE`（事务性秒级；提示
   does not exist = 未安装，正常跳过）
5. `docker restart` 清重连风暴残留连接 → extversion 清单自检（重启同时实证
   ALTER 持久化）
6. 时间窗日志自检（`--since 1m`）→ healthz

## 实战教训（2026-11-25 prod 实证）

- 新容器起到 ALTER 完成之间是**半瘫窗口**（timescaledb catalog 错位，全库查询报
  `could not access file "timescaledb-2.23.1"`）。应用重连风暴 + launcher 重试会打满
  max_connections（`too many clients` 连 superuser 都拒）→ 步骤 5 restart 即清。
  **窗口内错误墙属预期正常，不是事故**。
- 验日志必须 `docker logs --since <时刻>`；grep 全历史会把半瘫窗口旧错误当新错误。
  管道末端 `tail` 空输出退出 0，`|| echo ✅` 不触发——**空输出即无错误**。
- ALTER 撞 `too many clients` 后重跑失败无碍（首跑已应用，UPDATE 幂等）。
- bloom sparse indexes WARN 与 dev 同款，核查不适用本 schema，不阻塞。
- prod 实装 8 扩展（timescaledb/postgis/pg_jieba/pgcrypto/btree_gist/pg_trgm/
  uuid-ossp/plpgsql），仅 timescaledb/postgis 需要 UPDATE，其余随目录无缝过渡。

## 回滚（放弃切换后的新写入；切换前失败恢复见脚本各步注释）

```bash
docker stop prod_imboy_pg18 && docker rm prod_imboy_pg18 \
  && docker rename prod_imboy_pg18.bak prod_imboy_pg18 && docker start prod_imboy_pg18
# beam 自动重连；若需手动重启节点，先确认 release bin 真实路径（勿猜）
```

## 清理

```bash
bash scripts/pg18_directory_swap_upgrade.sh cleanup           # 过程残留（随时可跑）
bash scripts/pg18_directory_swap_upgrade.sh cleanup-anchors   # 回滚锚：稳定 ≥7 天后
```

- `cleanup`：/tmp 导出 dump、/root 历史脚本与基线/日志、`/data/docker/pg_backup`
  下本方案的 `mig_*.dump`。**不动**自有 cron 备份 `/data/backup/imboy_db/`。
- `cleanup-anchors`：守卫=活数据挂载目录必须存在且**不等于**待删原件目录、状态文件
  记录的升级时间满 7 天（`FORCE=1` 强制）；动作=`docker rm` 旧容器壳 + `rm -rf`
  原件目录 + `docker rmi` 旧镜像（约释放 1GB，该盘 84% 使用率）。
- 2026-11-25 手工升级留下的锚用脚本清时需 `BAK=prod_imboy_pg18_bak361` 且
  `FORCE=1`（当日未写状态文件）：

```bash
BAK=prod_imboy_pg18_bak361 FORCE=1 \
  bash scripts/pg18_directory_swap_upgrade.sh cleanup-anchors
```
