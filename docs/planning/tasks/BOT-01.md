# BOT-01 — 开发者 Bot C2G Mention 与原会话回复

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 BOT-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/BOT-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Backend-Bot-Group
- Dependencies：WH-01、TRACE-00
- Repo：`imboy`
- Files：Bot C2G 分派、`bot_logic/handler`、Webhook event contract tests。
- Actions：
  1. 仅在非 E2EE 群、Bot 为有效群成员、消息明确 mention Bot 且订阅 `message.c2g_mention` 时写 delivery。
  2. event 携带不可伪造的 reply context：group_id、trigger_msg_id、bot_id、correlation_id 和到期时间；不把任意 from_uid 当可信身份。
  3. 扩展 Bot send API 支持回复原 C2G context；校验 Bot 群成员关系、context 归属、到期和一次性/幂等语义。
  4. C2C 现有行为保持；events 白名单只开放 `message.c2c` 与 `message.c2g_mention`，未知 event 拒绝注册。
  5. Bot 自己发出的消息不得再次触发自身或 Bot-to-Bot 环。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
make app
make eunit-local t=bot_logic_tests
make eunit-local t=bot_webhook_logic_tests
make eunit-local t=bot_group_mention_tests
make eunit-local t=msg_c2g_logic_tests
make eunit-local t=bot_e2e_tests
git diff --check
```

- Acceptance IDs：`BOT-01-A01` mention 产生一次 delivery 且普通群消息零 delivery；`BOT-01-A02` Bot 可回复原群但不能改目标群且保留 correlation；`BOT-01-A03` 跨群、过期、停用、非成员、E2EE、自触发全拒绝。
