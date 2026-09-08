# EXT-01 外部技术验证 Runbook — 真机 / 真实 MCP 客户端 / 客户隔离环境

> 所属：[IMBoy Agent Hub 产品收敛执行计划](../planning/ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 执行卡：[tasks/EXT-01.md](../planning/tasks/EXT-01.md) ｜ Status：`blocked_external`
> 前置门：`LOCAL_AGENT_HUB_GATE=PASS`（已达，2026-09-08）+ 用户明确授权设备/账号/外部动作。
> 本 runbook 是授权后执行的完整作业书；授权前**不得**执行任何 §1-§3 的外部动作。

---

## §0 前置条件与凭证获取

### 0.1 授权边界（先于一切）

| 项 | 要求 |
|---|---|
| 执行环境 | 用户授权的**隔离环境**（专用 VM/机器），禁止生产环境 |
| 设备 | 用户明确授权的真机（Android/iOS），禁止模拟器 |
| 账号 | 隔离环境内新造的测试账号，禁止真实用户/生产账号 |
| 对外动作 | 禁止触达任何第三方（webhook 端点必须指向隔离环境内 receiver） |
| 证据 | 全部脱敏后才可离开隔离环境（见 §4.3） |

### 0.2 admin 签名 cookie 对（ADM_UID / ADM_SIG）

admin API 鉴权为 cookie 对：`adm_user_id` + `adm_user_sig`。签名值是
**elib_hasher:hmac_sha256 的 base64 输出（88 字符 ASCII，含 +/=）**，可直接经
环境变量传递。两种获取方式：

1. **浏览器 DevTools**：用管理员账号登录 admin 控制台 → DevTools → Application →
   Cookies → 复制 `adm_user_id` 与 `adm_user_sig` 的值。
2. **服务端铸造**（在目标环境节点同版本代码上执行；必须与运行节点用同一份
   ebin，否则签名口径可能不一致）：
   ```bash
   erl -noshell -boot no_dot_erlang -config config/sys.<env> \
     -pa ebin $(for d in deps/*/ebin; do echo -n "-pa $d "; done) \
     -eval 'application:load(imboy),
            io:format("~s~n", [adm_auth_middleware:sign_admin_cookie(<<"<管理员uid>">>)]),
            halt(0).'
   ```

脚本注入：`export ADM_UID=<uid> ADM_SIG=<签名>`（或 `ADM_SIG_HEX` 传 hex 形态的
任意字节值）；凭证只经环境变量，不落盘。

### 0.3 本地预检（授权前可在开发机做，无外部动作）

```bash
# 脚本客户端逻辑自检（内嵌 loopback stub，非产品证据）
python3 scripts/agent_hub_ext01_mcp_client_smoke.py --self-test
# 期望：11 PASS / 0 FAIL
```

---

## §1 EXT-01-A01 真机 task progress/HITL/terminal 回读

**目标**：在至少一端真机（若产品声明双端支持则双端）完整走通 Agent task 的
进度回读、人工审批（HITL）、终态回读。

### 前置

- 隔离环境后端已部署（含迁移 90-93，见 §3.2）且 Agent Hub feature 开启
- 真机已安装匹配的 app 构建（禁止模拟器；Flutter 调试必须真机）
- 隔离环境内造好：两个测试用户、一个非 E2EE 群、一个内建 Agent
- 一个 MCP client（§2 步骤 1-4 已建好并授权 `create_agent_task`）

### 步骤

1. **触发任务**：在真机账号 A 的群内 @Agent 触发多轮对话，或由 §2 的 MCP client
   `tools/call create_agent_task` 创建任务（group_id=测试群）。记录 task_id。
2. **progress 回读**：真机进入 Agent task 卡片（APP-01 交付），确认能看到任务状态从
   `submitted → working` 的进度变化（或经 `get_agent_task` 轮询路径）。
3. **HITL**：MCP client 调 `request_task_approval`（reason=审批请求说明）→
   真机收到审批卡片（awaiting_approval）→ 账号 A 在真机上**批准**。
4. **terminal 回读**：MCP client 再调 `get_agent_task`，确认状态经审批后到达终态
   （executed/completed 分支按任务语义）；真机卡片同步显示终态。
5. **负例（真机侧）**：账号 B（非任务群成员）打开同一任务卡片 → 应不可见或无操作入口。

### 证据采集

- 每步真机截图（脱敏：遮蔽手机号/头像真名）
- `adb logcat` / iOS Console 摘录（过滤本任务 task_id 相关行）
- 服务端 `agent_task` / `agent_event` 行导出（correlation_id 链完整）
- MCP client 侧 `--out` 结果 JSON

---

## §2 EXT-01-A02 真实 MCP 客户端 connect/grant/task/revoke

**目标**：一个真实但非生产敏感的 MCP 客户端（本 smoke 脚本，python3 标准库实现）
在隔离环境完整走通凭证生命周期。

### 步骤

```bash
export IMBOY_BASE_URL=https://<隔离环境地址>   # 非 loopback → 需下一步授权位
export EXT01_AUTHORIZED=yes                    # 用户授权后才设
export ADM_UID=1 ADM_SIG=<签名cookie>
python3 scripts/agent_hub_ext01_mcp_client_smoke.py --out /tmp/ext01-a02-result.json
```

### 脚本自动覆盖的检查点（11 项）

1. admin `create`（secret 一次返回，输出脱敏只显 prefix）
2. `approve`
3. `grants/set` 显式授权 `create_agent_task`（V1 approve 默认空授权，必须显式授）
4. `grants` 复核 enabled
5. MCP `initialize`（session 建立）
6. `tools/list` 含目标 tool
7. `tools/call create_agent_task`（幂等键派生，返回 task_id/correlation_id）
8. `tools/call get_agent_task`（状态回读）
9. 负例：错误凭证 401 fail-closed
10. `revoke`
11. 负例：撤销后原凭证立即失效（401/403）

### 证据

- `--out` JSON（含 11 项 check 明细）
- admin 审计页 `GET /api/adm/mcp/audit?client_id=<id>` 导出（create/approve/grant/revoke 全链）

---

## §3 EXT-01-A03 客户隔离环境安装/升级/备份回滚

**目标**：证明客户可在隔离环境完成「干净安装 → 升级 → 备份 → 回滚验证」全循环，
全程不需要工程师改 DB/代码/隐藏配置。

### 3.1 干净安装

按 `deploy/README.md`：`cp .env.example .env` → 填最小 env → `bash preflight.sh` →
`docker compose -f docker-compose.prod.yml up -d`。前置注意：12 个 PG 扩展必须先于迁移
（见 `docs/operations/agent-hub-local-golden-flow.md`）。验收：健康检查全绿，admin 可登录。

### 3.2 升级（本计划新增迁移 90-93）

在旧版本数据之上应用新版本：

```bash
# 迁移随启动自动应用（imboy_migrate）；验收：
psql ... -c "SELECT version FROM schema_migrations"        # >= 93
curl -s .../api/health || docker compose ps                 # 服务健康
```

验收点：存量数据可读（旧消息/联系人不受影响）；`mcp_client` 存量行已回填 `client_key`；
`bot.token_migrated=false` 行数=0（或存在明确迁移说明）。

### 3.3 备份

```bash
bash scripts/backup_pg.sh          # PG 逻辑备份
bash scripts/backup_garage.sh      # 附件 S3（Garage）备份
```

验收：备份产物存在且 `pg_restore --list` 可读；记录备份耗时与体积（写入证据）。

### 3.4 回滚验证

1. 升级后人为制造一条新数据（如新 MCP client 一条）。
2. 用 §3.3 备份执行 `bash scripts/restore_pg.sh` 恢复到升级后时点。
3. 验收：恢复后服务可启动、登录正常；步骤 1 的数据按恢复点语义存在/消失且与备份时点一致；
   `agent_task`/`bot_delivery` 在恢复后状态与备份时点完全一致（correlation 无孤儿）。

> 回滚到升级**前**版本需降级迁移（down 脚本按版本倒序执行）；本卡验收口径以
> 「恢复到备份时点」为准，降级演练可作为附加项记录。

---

## §4 证据协议

### 4.1 证据位置

全部写入 `$IMBOY_EVIDENCE_ROOT/EXT-01/`（与既有证据同根；该位置为受控目录，
未经脱敏的原始产物不得进入）。建议布局：

```
EXT-01/
  evidence.json            # 主证据（EVID-00 schema）
  a01-device/              # §1 截图/日志/DB 导出（脱敏后）
  a02-mcp-smoke.json       # §2 --out 结果
  a03-drill/               # §3 安装/升级/备份/回滚各步日志
  manifest.sha256
```

### 4.2 evidence.json 骨架（EVID-00 schema v1；`<>` 为执行时填充位）

```json
{
  "schema_version": 1,
  "task_id": "EXT-01",
  "status": "PASS",
  "verification_status": "verified",
  "base_sha": {
    "imboy": "<40位sha或na>",
    "imboyapp": "<40位sha或na>",
    "imboyadmin": "<40位sha或na>"
  },
  "commit": "not-created-no-identity-approval",
  "final_diff": ["imboy:scripts/agent_hub_ext01_mcp_client_smoke.py"],
  "commands": [
    {"id": "cmd-01", "command": "python3 scripts/agent_hub_ext01_mcp_client_smoke.py --out <证据目录>/a02-mcp-smoke.json", "exit_code": 0, "note": "A02 十一检查点"},
    {"id": "cmd-02", "command": "bash scripts/backup_pg.sh", "exit_code": 0, "note": "A03 备份"},
    {"id": "cmd-03", "command": "bash scripts/restore_pg.sh", "exit_code": 0, "note": "A03 回滚恢复"}
  ],
  "tests": {"passed": 11, "failed": 0, "skipped": 0},
  "acceptance": [
    {"acceptance_id": "EXT-01-A01", "status": "PASS", "command_ids": ["cmd-01"],
     "artifact_ids": ["artifact-a01"],
     "assertions": ["真机端 task progress/HITL/terminal 回读完整通过（型号与两端口径见 note）"]},
    {"acceptance_id": "EXT-01-A02", "status": "PASS", "command_ids": ["cmd-01"],
     "artifact_ids": ["artifact-a02"], "assertions": ["真实 MCP client connect/grant/task/revoke 全链通过"]},
    {"acceptance_id": "EXT-01-A03", "status": "PASS", "command_ids": ["cmd-02", "cmd-03"],
     "artifact_ids": ["artifact-a03"], "assertions": ["隔离环境安装/升级/备份回滚通过"]},
    {"acceptance_id": "EXT-01-A04", "status": "PASS", "command_ids": [],
     "artifact_ids": ["artifact-manifest"], "assertions": ["外部证据脱敏复核完成"]}
  ],
  "artifacts": [
    {"id": "artifact-a01", "path": "<绝对路径>/a01-device.tar.gz", "sha256": "<实测填充>"},
    {"id": "artifact-a02", "path": "<绝对路径>/a02-mcp-smoke.json", "sha256": "<实测填充>"},
    {"id": "artifact-a03", "path": "<绝对路径>/a03-drill.tar.gz", "sha256": "<实测填充>"},
    {"id": "artifact-manifest", "path": "<绝对路径>/manifest.sha256", "sha256": "<实测填充>"}
  ],
  "decisions": [
    {"decision": "<执行中的口径决策>", "rationale": "<理由>"}
  ],
  "residual_risks": [
    "<如实记录未覆盖面>"
  ]
}
```

判定（与全计划同协议，exit 0=PASS）：

```bash
python3 scripts/verify_agent_hub_task_evidence.py --task "$IMBOY_EVIDENCE_ROOT/EXT-01/evidence.json"
```

> 模板可用假样本自检（放 /tmp，勿入证据根）：
> `mkdir -p /tmp/ext01-check/EXT-01` → 填充模板（artifacts 用 /tmp 实文件算 sha256）→
> 跑上面 verifier，期望 exit 0。

### 4.3 脱敏红线（A04）

证据离场前逐文件检查，出现即整改：

- 手机号 / 真实姓名 / 头像原片 / 精确地理位置
- 任何 secret 明文：MCP secret、bot verify_token、AEAD 主密钥、admin cookie 签名
- webhook receiver 的真实公网地址（以 `<isolated-endpoint>` 占位）
- logcat/console 摘录需先 `grep` 过滤上述模式后再入档

---

## §5 停止条件

- 未获用户对设备/账号/外部动作的明确授权 → 全卡保持 `blocked_external`，§1-§3 不得执行。
- 隔离环境不可用或被迫使用生产环境 → 停止。
- 任何一步需要「改 DB/改代码/隐藏配置」才能继续 → 记录为安装缺陷（A03 不通过），不得现场修补后声称通过。
- 证据无法满足 §4.3 脱敏 → 该项证据作废重采，不得脱敏失败后照常提交。
