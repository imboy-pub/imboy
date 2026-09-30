# IMBoy 金丝雀/灰度发布 Runbook（Canary Release）

> 适用版本：imboy v1.0.0-rc.1+（蓝绿双节点架构：蓝 `:9800` / 绿 `:9801`）
> 依赖脚本：`scripts/imboy-deploy.sh`（蓝绿部署）、`scripts/lib/blue_green_deploy.sh`
> 关联文档：[deploy-script.md](./deploy-script.md)（蓝绿部署原理与紧急回滚）、
> [upgrade-runbook.md](../upgrade-runbook.md)（版本升级流程）、
> [monitoring.md](./monitoring.md)（告警处置）

---

## 运维处置卡片（owner / 止损 / 回滚）

| 要素 | 值 |
|---|---|
| **Owner** | IMBoy Ops（待用户指名，指名后替换本行） |
| **Escalation** | ① IMBoy Ops 值班（灰度窗口内必须在线）→ ② 待指名平台负责人（IM/电话占位）→ ③ 客户端不兼容时联动 App 端负责人（灰度期内客户端反馈渠道保持畅通） |

### 止损线（量化，任一触发即执行回滚，不在灰度态排障）

每个批次观察窗内，**新色侧**出现以下任一条件即回滚：

1. HTTP 5xx 错误率 > 1%（5 分钟窗口）或出现灰度前不存在的错误码/报错模式；
2. 消息投递 p99 > 500ms 持续 5 分钟（灰度期用 SLO 目标值而非 2s 临界值——
   灰度的意义就是更早发现劣化）；
3. 新色 WS 连接建立失败率上升（客户端报告掉线重连失败 / 绿色端口连接数不升）；
4. 数据兼容性错误（新色日志出现 schema/迁移相关 ERROR）——立即回滚并冻结
   该版本，走升级手册 §6 评估。

**止血动作**：触发即删灰度配置回到旧色 100%（见回滚），通知灰度批次用户
重新连接；回滚后指标回绿、根因定位前**禁止重试灰度**。

### 回滚（灰度期专用，< 5 分钟）

灰度配置全部是**增量插入**的临时段，回滚 = 删除/还原，旧色槽位全程未动：

```bash
# 1. 删除 API vhost 里的 split_clients 灰度段（见附录 A），恢复标准 proxy_pass
ssh -p $SERVER_PORT $SERVER_USER@$SERVER_HOST \
  "sed -i '/# CANARY-BEGIN/,/# CANARY-END/d' /path/to/api.vhost.conf && nginx -t && nginx -s reload"

# 2. admin vhost 若已切新色，切回旧色端口（例：绿 9801 → 蓝 9800）
ssh -p $SERVER_PORT $SERVER_USER@$SERVER_HOST \
  "sed -i 's/127.0.0.1:9801/127.0.0.1:9800/' /path/to/prodadm.vhost.conf && nginx -s reload"

# 3. 确认回滚生效：全部流量回到旧色
curl -s https://$API_DOMAIN/healthz    # 版本号应为旧版本
```

> 灰度期不使用 `imboy-deploy.sh rollback`——那是全量切槽位回滚；灰度回滚只需
> 摘掉灰度段，不动 upstream 槽位（旧色一直是主槽位）。

---

## 1. 前提与不变量

1. 蓝绿双节点已按 [deploy-script.md](./deploy-script.md) 完成**新色启动**：
   `DEPLOY_STOP_OLD=false bash scripts/imboy-deploy.sh api`，新色（如绿
   `:9801`）已起、healthz 通过，但**尚未执行全量切流**；
2. **主 upstream 单槽位不变量保持不动**：nginx upstream 与 admin/CS vhost
   全程指向旧色，直到灰度毕业才走标准切流。这保证：
   - `blue_green_deploy.sh` 的 fail-closed 门禁（"不是唯一蓝/绿 upstream 拒绝
     猜测"）在灰度态依然成立；
   - 任何时刻摘掉灰度段即回到"旧色 100%"。
3. 灰度配置一律用 `# CANARY-BEGIN` / `# CANARY-END` 包裹（回滚按标记整段删除）；
4. 灰度窗口内冻结其他生产变更（部署/迁移/扩容）。

## 2. 灰度批次（每批一个观察窗，逐批放行）

| 批次 | 对象 | 方式 | 观察窗 |
|---|---|---|---|
| 0 | 内测冒烟 | 直接压绿色端口：`curl http://127.0.0.1:9801/healthz` + 内测客户端指向新色验证收发消息/E2EE/附件 | 15 分钟 |
| 1 | 内部员工 | admin vhost 切新色（管理后台/客服是天然金丝雀人群） | 30 分钟 |
| 2 | 10% App 用户 | API vhost 插入 `split_clients` 灰度段（附录 A） | 60 分钟，须覆盖一个业务高峰 |
| 3 | 50% App 用户 | 调整 `split_clients` 百分比 | 60 分钟 |

**批次门槛**：上一批观察窗内止损线 0 触发、核心指标（5xx/p99/WS 掉线/
消息速率）与旧色基线持平，才进入下一批。任一批触发止损线 → 回滚 → 灰度终止。

## 3. 观测窗口与看什么

- Prometheus 面板对比新旧色：`up{job="imboy_backend"}`（两实例）、
  `imboy_http_requests_total` 按 instance 拆分的 5xx 率、
  `imboy_msg_deliver_duration_seconds_bucket` p99、`imboy_ws_connections_total`；
- 新色日志尾随：`docker logs` / `tail -f /var/log/imboy/*.log`，重点 grep
  `ERROR`、`crash`、`migrat`；
- 已知灰度特性（非故障）：长连 WS 在重连前仍走旧色，批次 2/3 的连接占比
  迁移有分钟级滞后，属预期。

## 4. 毕业流程（灰度 → 全量）

```bash
# 1. 删除灰度段（附录 A 标记整段删除），恢复标准配置
# 2. 走标准蓝绿切流（脚本完成 upstream/admin/CS 三 vhost 同槽切换 + readiness 门禁）
bash scripts/imboy-deploy.sh api     # 检测到新色已起，执行切流与验证
# 3. 切流后按 deploy-script.md 的人工止损线观察 10 分钟，再停旧色
```

---

## 附录 A：split_clients 灰度段（批次 2/3，插入 API vhost 的 location 前）

```nginx
# CANARY-BEGIN （灰度临时段：按登录态哈希分桶；毕业/止损时整段删除）
# 首批 10%；批次 3 改 50%。key 用 Authorization+来源 IP，保证同一用户稳定落桶。
split_clients "${http_authorization}${remote_addr}" $canary_backend {
    10%      127.0.0.1:9801;      # 新色（绿）
    *        127.0.0.1:9800;      # 旧色（蓝，主槽位不动）
}
# CANARY-END
```

`location` 内 `proxy_pass http://$canary_backend;`（WS location 同理）。
注意：`$canary_backend` 含变量的 proxy_pass 不解析 upstream 块，属直接端口
转发，不影响 upstream 单槽位不变量与脚本门禁。

## 附录 B：批次 1 的 admin vhost 切换

```bash
# admin vhost 直接 proxy_pass 到应用端口（不走 upstream），单独切新色
ssh -p $SERVER_PORT $SERVER_USER@$SERVER_HOST \
  "sed -i 's/127.0.0.1:9800/127.0.0.1:9801/' /path/to/prodadm.vhost.conf && nginx -s reload"
```

> 灰度期 admin vhost 与主 upstream 不同槽是**有意为之**的中间态；
> `deploy_readiness` 的一致性检查在此期间会（正确地）拒绝通过，毕业切流后
> 自动恢复一致。
