# EXT-01 — 真机、真实 MCP 客户端与客户隔离环境技术门

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 EXT-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/EXT-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked_external`
- Owner：人工指定
- Dependencies：GATE-01=PASS、用户明确授权设备/账号/外部动作
- Required evidence：
  - Android/iOS 真机至少一端完整 task progress/HITL/terminal 回读；若产品声明双端支持，则两端都需验。
  - 一个真实但非生产敏感的 MCP 客户端完成连接、grant、task 和 revoke。
  - 客户隔离环境安装/升级/备份回滚验证。
- Acceptance IDs：`EXT-01-A01` 声称支持的真机端完整通过；`EXT-01-A02` 真实 MCP client 的 grant/task/revoke 通过；`EXT-01-A03` 客户隔离环境安装/升级/备份回滚通过；`EXT-01-A04` 外部证据脱敏且经 verifier 判定 PASS。
- Boundary：没有以上证据时，最多声称 `LOCAL_AGENT_HUB_GATE=PASS`，不得写“生产可用”或“正式发布”。任何设备安装、客户环境、真实账号或第三方联调必须先获得用户明确授权。

---

## 本地侧准备记录（2026-09-08，授权前完成，不含任何外部动作）

- 作业书：`docs/runbooks/agent-hub-ext01-external-verification.md`（§0 凭证获取/授权边界、§1 A01 真机清单、§2 A02 脚本用法、§3 A03 安装/升级/备份回滚演练、§4 证据协议、§5 停止条件）。
- A02 真实 MCP client：`scripts/agent_hub_ext01_mcp_client_smoke.py`（python3 标准库，11 检查点；内建守卫——非 loopback 目标必须 `EXT01_AUTHORIZED=yes`；`--self-test` 对内嵌 stub 自检 11/11 PASS）。
- 阻塞面修复（A02 前置，MCP-01/ADM-01 缺口）：`/api/adm/mcp/clients/create` 与 `/api/adm/mcp/clients/grants/set` 此前**无路由**（前者前端已调用会 404，后者导致 V1 空授权下任何工具无法经 API 启用）——已补路由 + handler `set_grant` action + 2 回归用例（`adm_mcp_handler_tests` 5/5 绿）。该缺口因 bun mock 测试与 golden flow 内部桥接不经过 HTTP 路由而未被既有门发现。
- 证据模板已按 EVID-00 schema v1 校准并用 /tmp 假样本过 verifier（exit 0）；真实证据仍须来自授权隔离环境的实际执行。

### 本地实测记录（2026-09-08，对真实后端的全链 QA，非正式 EXT-01 证据）

在本地 imboy_v1（迁移 93）+ 私有快照 ebin 起的 dev 节点（HTTP 9820，无分布式、不触碰共享 9800/9801 节点）上，A02 smoke 脚本对真实 HTTP 链路跑通 **11/11 全绿**（create/approve/grant/grants 复核/initialize/tools-list/create_task/get_task/错凭证 401/revoke/撤销后失效）。实测抓出并修复了三个只有真实链路才暴露的缺口：

1. **`mcp_client_repo:digest_hex/1` 未导出**：`mcp_governance_logic:authenticate_secret/1`（HTTP 认证路径）跨层调用它 → 运行时 undef 500。已导出（digest 唯一真源）+ 导出存在性/同构回归测试。
2. **`agent_task_logic:lookup/1` 混同「无决定行」与「任务不存在」**：submitted（建任务后常态）无决定行 → 返回 undefined → `get_agent_task` 对一切新建任务误报「任务不存在」，A01 progress 回读路径全断。已增加 `{live_status, StatusBin}` 返回分支 + `get_agent_task` 对应子句 + 回归测试。相关套件 15/15 绿。
3. （口径澄清）`sign_admin_cookie` 返回 elib_hasher 的 **base64**（88 字符 ASCII），cookie 值安全；此前「raw HMAC 会崩 setcookie」的怀疑不成立（elib_hasher:hmac_sha256 本身就 base64 编码）。runbook §0.2 已按实测口径改写。

方法学沉淀：**mock 层 eunit 与内部桥接 harness 都测不到「HTTP 路由/导出/跨模块契约」层**——本卡执行前先用 smoke 脚本对真实节点跑一遍应成为 EXT-01 的固定前置；共享工作区跑后端须用私有 ebin 快照（`imboy/ebin` 目录形态 + priv 链接），否则并行构建会覆盖 beam 造成同源假象。
