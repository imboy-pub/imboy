# Runbook — Agent Hub 本地 Golden Flow 复现（E2E-01）

> 目标：从零 scratch 环境复现 Agent Hub 全链（incoming webhook → 内建 Agent 对话
> → MCP Agent task/HITL → 人工审批 → Bot mention/delivery/reply）并归档证据。
> 前置阅读：PDT-01 三契约、ADR 信任边界、EVID-00 证据协议、
> [bot-webhook-aead-key](../runbooks/bot-webhook-aead-key.md)（AEAD 主密钥生命周期）。
>
> 当前自动化状态：脚本使用 marker scratch DB 跑迁移和 15 套模块/真库测试；
> 最后一套通过真实 MCP task logic、审批逻辑和 Bot outbox 写出同一 correlation
> 的持久记录，再由参数化 SQL 投影为链形状并交给冻结 verifier。随后脚本启动真实
> loopback 后端，以独立标准库 HTTP 客户端完成 MCP create/approve/grant/task/revoke
> 11 项检查；频道 incoming webhook 同时完成有效 token 发布、无效/停用 token 同形
> 404 共 4 项检查，并核对仅有效请求落一条 system-bot 消息。内建 Agent
> 子链在运行节点用 `ai_agent_ds:create/1` 建号，经真实 WebSocket C2G `mentions`
> 帧触发本地 `chat/3` fake provider，并同时核对人类消息和 Agent 定稿已写入
> `msg_c2g` 与 `msg_store`。fake provider 仅从临时 ebin 加载，不发起任何网络请求。
> 脚本还会真实 stop/start 后端，比较 task/approval/delivery/correlation
> 快照并从重启进程回读终态。
> 该投影中的 request/execution/outcome 仍非运行时审计记录，Bot mention/reply
> 也尚未组成完整 HTTP 正负链，
> 所以 A01/A02 继续失败，整体必须输出 verifier 接受的 `PARTIAL`（退出码 1）。

## 1. 环境准备（从空 scratch 开始）

1. PostgreSQL 实例（本地 4323 或自备），创建 scratch 库：
   `createdb imboy_agenthub_e2e`
2. 安装扩展（12 个，见 imboy-clean-deploy 文档：pg_jieba/postgis/timescaledb/…）。
3. 后端配置：`IMBOYENV=local`、`sys.local.config` 指向 scratch 库；
   `postgre_aes_key` 必须配置（WH-01 AEAD 主密钥，缺失时投递 fail-closed）。
4. 前端（可选）：管理台 `bun run dev`（8082）。
5. harness 默认占用 loopback `19862`；如被占用，用
   `IMBOY_AGENT_HUB_HTTP_PORT=<未占用私有端口>` 覆盖。非 loopback 地址会拒绝。

## 2. 启动与种子

```bash
IMBOYENV=local make run            # 启动即自动迁移 1→93+（imboy_migrate strict）
# fake LLM provider：harness 编译 test fixture 到临时 ebin（无真实外呼）
# fake Bot webhook receiver：tests 提供 gen_tcp fixture（或 nc -l 本地端口）
# 最小 MCP client：tests 提供 JSON-RPC 脚本（initialize → tools/list → tools/call）
```

种子（管理台或 API）：两个用户、一个非 E2EE 群、一个 AI Agent、一个 Bot
（webhook_url 指向 fixture receiver）、一个 MCP Client（记录一次性 secret）。

## 3. Golden Flow（目标正例，尚未全部自动化）

| # | 步骤 | 验证点 |
|---|---|---|
| 1 | 频道 incoming webhook 发消息（token） | 200；消息落频道；correlation_id 生成 |
| 2 | 内建 Agent 纯对话 | 已自动化；真 WS `mentions` 输入，fake LLM 回复，两条消息皆进正式表与归档表 |
| 3 | MCP client：create_agent_task | task=submitted；同 client+idem 重放返回原任务 |
| 4 | update(start/progress) | working；attempt 事件累计 |
| 5 | request_task_approval | 群卡片 awaiting_approval；MCP poll 可读 |
| 6 | 群内人工 approve | approved → 系统调度 resume → 执行 → completed |
| 7 | 群内 @Bot mention | delivery 入 outbox → fixture 收到签名请求（200）|
| 8 | Bot 携 reply_context 回复原群 | 验签/归属/到期/一次性全过；消息落原群 |

## 4. 负例（全部预期拒绝）

- MCP：revoked / expired / disabled credential → 401；未授权 tool → deny；
- 重复 idempotency key → 返回原结果，不产生第二任务；
- Webhook：fixture 5xx → retry（5/30/300s）→ dead；404 → 直接 dead；重放 → delivery_id 不变；
- 跨群 Bot reply（context 归属不符）→ 拒绝；过期/已用 context → 拒绝；
- E2EE 群的 agent task → 服务端不产生卡片帧；
- 服务重启（kill 后端再起）：pending/approval/correlation 全部从 DB 恢复。

## 5. 证据与清理

```bash
bash scripts/agent_hub_golden_flow.sh --profile local-fixture \
  --evidence-dir "$IMBOY_EVIDENCE_ROOT/E2E-01"
python3 scripts/verify_agent_hub_task_evidence.py --task "$IMBOY_EVIDENCE_ROOT/E2E-01/evidence.json"
shasum -a 256 "$IMBOY_EVIDENCE_ROOT"/E2E-01/*
```

在受信入口审计、完整 HTTP 编排和最终 Base 重跑全部补齐前，上述前两条命令预期
退出码均为 `1`，证据结论为 `PARTIAL`。A03 的 stop/start 可以独立为 `PASS`；A02
即使 MCP HTTP 11/11、频道 incoming webhook 4/4、内建 Agent 群对话通过，也仍因
Bot 正负链和负例缺失保持 `FAIL`。trace
verifier 的 `OK` 只证明本地 fixture 投影满足链的结构约束，不证明受信入口已经产生
真实审计链。

清理只删除本 harness 创建且带 marker 的临时资源（scratch 库 drop、fixture
端口关闭、临时 secret 文件删除）。同一路径已有证据会先移动到带
`.superseded.<timestamp>.<pid>` 后缀的同级目录，避免旧文件混入新 manifest。

## 6. WH-01 AEAD 主密钥生命周期演练

见 [bot-webhook-aead-key](../runbooks/bot-webhook-aead-key.md)：双密钥窗轮换五步、
密钥丢失 fail-closed 演练（停 worker → 影响面 → 凭证重置/SLA 通知）、
发布前演练清单。要点：

- 主密钥缺失/解密失败 → 投递 dead（`no_key`），无明文降级路径；
- 轮换后 `token_migrated=false` 行数必须归零；
- 旧密钥退役在全部节点滚动完成后执行。
