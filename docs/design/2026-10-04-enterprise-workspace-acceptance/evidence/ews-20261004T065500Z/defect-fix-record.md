# Defect Fix Record — EWS-BE-D1 / EWS-BE-D2 (已修复并合并 main)

> 本文件为修复后新增记录。不改 final/manifest.sha256（152 文件清单）内任何文件；
> defect-trust-audit-tsid.md 与 defect-oa-sso-dual-clock.md 两张缺陷卡保持定稿原样，
> 其修复状态以本文件为准。
> Executed: 2026-10-04, 双 agent 并行修复（独立 worktree/分支/relay 端口），串行集成。

## EWS-BE-D1 — trust_audit TSID 生成器未注册（P2）

- **机理**（唯一运行时调用点）：`src/logic/olm_identity_logic.erl:350`
  `emit_rotation_event/6` → `elib_tsid:generate(trust_audit)`，调用链
  `olm_handler → report_identity/7 → rotate_identity/7` 可达；无惰性注册守卫，
  未注册时 `elib_tsid:require_runtime/1`（src/lib/elib_tsid.erl:494-512）抛
  `elib_tsid_generator_not_registered`，且调用点无 try/catch —— 换钥请求在身份
  upsert 落库后整体崩溃，违背该函数"事件失败不阻断"的设计意图。
- **判定遗漏非设计**：catalog 已登记（elib_tsid_catalog.erl:283）；守护测试
  `elib_tsid_registration_guard_tests` 以 `imboy_app:tsid_generator_names()` 为
  唯一权威且头注记载同类缺陷两度进生产（a93aa998、bot_ds）；imboy_app 内 3 处
  同类补登先例注释；无 feature-flag/按需启动机制涉及 olm 域。
- **修复**：`e352e881` `fix(tsid): register trust_audit generator in app boot`
  （src/imboy_app.erl +4，对齐相邻补登条目写法）。
- **验证（agent 定向，9 项全绿）**：registration_guard_tests（修复前失败
  `{expected,[]},{value,[trust_audit]}` → 转绿）、elib_tsid_tests 66P、
  olm_identity_logic_tests 43P、e2ee_trust_logic_tests 24P、elib_tsid_guard_tests
  26P、trust_audit_repo_integration_tests 4P、imboy_env_tsid_tests 15P、
  elib_tsid_catalog_tests 10P（catalog digest 不受影响）、make compile。

## EWS-BE-D2 — OA/SSO 一次性授权码双时钟 CHECK 竞态（P1）

- **方案**：单一时钟源根治 —— `issue_tx` 的 INSERT 省略 `created_at` 列，由 DDL
  既有 `DEFAULT CURRENT_TIMESTAMP`（事务开始时刻，事务内固定）在 DB 端生成。
  CURRENT_TIMESTAMP 数学上必然 ≤ 后续任何 clock_timestamp()，CHECK 恒可满足，
  不依赖时钟单调性承诺。9 参 → 8 参，迁移 DDL 无需动。
- **否决项**：b) `GREATEST(clock_timestamp(), created_at)` —— 不除根因、VM 领先时
  回拨审计时间戳、会静默掩盖未来回归；a') INSERT 内联 clock_timestamp() —— PG 不
  对其跨语句单调性做文档承诺。
- **修复**：`4c973499` `fix(enterprise-oa-sso): single clock source for code
  issue/consume`（repo 改 INSERT + 测试补 D2 回归断言：created_at 为 DB 端
  RFC3339 binary、created_at =< consumed_at；原 INT-14 错误合同断言全保留）。
- **验证**：
  - psql 竞态对照（保留库 imboy_ews_20261004T0655Z，事务内 fixture+ROLLBACK）：
    旧形态 created_at=clock_timestamp()+20ms → UPDATE 23514 违反（与缺陷卡硬证据
    逐字吻合）；新形态同事务零间隔签发→立即消费 → UPDATE 1，无违反。
  - 定向 eunit：enterprise_internal_foundation_pg_tests 9P（含缺陷卡点名
    `sso_code_issue_consume_single_use` 原现场用例）、enterprise_oa_sso_tests 53P
    （INT-14 面）、enterprise_oa_context_binding_tests 1P；调用方
    （logic/handler）检查无 created_at 隐性消费。

## 集成与复验

- 合并：两分支均基于 e9b2e83a，与 main 漂移（40689721，.githooks 包装器）文件
  交集为零；串行 --no-ff 合并 → main `4838635e`（D1）、`27e85f76`（D2）。
  合并消息过 commit-msg 钩子的教训：**subject 含 em dash（—）会被 conventional
  检查拒绝**，纯 ASCII 消息通过（首次尝试 `fix(tsid): ... — ...` 被拒）。
- 合并后复验（worktree ff 至 27e85f76）：make compile 绿；
  elib_tsid_registration_guard_tests `Test passed.`；
  enterprise_internal_foundation_pg_tests `All 9 tests passed.`
- 清理：两个修复 worktree 与 run/d1-*、run/d2-* 分支已删（safe -d）；
  worktree 内 gitignored 的 config/sys.local.config 为运行物化文件，随 worktree
  移除。未 push（禁令保持）。

## 附带发现（只报告，未修，建议另开技术债）

`src/repo/` 下仍有 10+ 模块用 `elib_dt:now()`（VM 时钟）写时间列：
billing_plan_repo、moment_comment_repo、user_setting_repo、conversation_pin_repo、
bot_repo、group_notice_repo、project_member_repo、group_task_assignment_repo、
app_upgrade_log_repo、group_member_repo 等 —— 与 D2 同构的潜在跨时钟比较面，
是否构成 CHECK 竞态需逐一核对其表约束。

## 保留物状态更新

- `imboy_ews_20261004T0655Z` 库：D2 已修复，复验已完成；库可由用户决定保留或删除。
- App 侧三卡（ContactPage 企业入口 / auto_test 企业旅程 spec / macOS OA 空入口）
  与客户 OA/SSO/Webhook 外部联调（BLOCKED_EXTERNAL）仍待对应 owner / 授权。
