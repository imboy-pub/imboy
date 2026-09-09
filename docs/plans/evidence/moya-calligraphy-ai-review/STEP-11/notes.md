# STEP-11 证据 — AI 视频回课 Worker 专项测试（R6 续轮）

Owner: Agent B（教学域后端） | 日期: 2026-09-09 | 环境: scratch PG `moya_mig_test@127.0.0.1:4323`（docker 实例，00000001→00000099 全量态）

> R6 前状态：teaching_ai_provider/teaching_ai_worker 代码骨架编译全绿但**无专项测试**（Step 11 = PARTIAL）。
> 本轮补齐 AI-01/AI-03 验收定义的专项测试两套，全部通过。真实 vision provider 仍 BLOCKED_EXTERNAL。

## 1. 用例清单（文件/用例名 ↔ 覆盖的 AI-0x 路径）

### test/logic/teaching_ai_provider_tests.erl（纯 logic，meck，零网络零真实密钥）

| 用例 | 覆盖路径 |
|---|---|
| `ai03_provider_name_unconfigured_test_` | AI-03 形一：provider 名未配置 → `{error, provider_unavailable}`；registry lookup **0 次**调用（未触注册表） |
| `ai03_registry_miss_test_` | AI-03 形二：registry 未命中 → 降级；chat **0 次**（零外呼断言） |
| `ai03_vision_false_test_` | AI-03 形三：`capabilities().vision = false` → 降级；chat 0 次 |
| `ai03_empty_api_key_test_` | AI-03 形四：`api_key = <<>>` → 降级；chat 0 次 |
| `ai01_success_whitelist_test_` | AI-01 成功：白名单重建恰 7 键（含 confidence）；`reasoning`/`raw_transcript` 思维链键结构性丢弃（AI-02 交叉） |
| `ai01_timeout_test_` | AI-01 超时：`{error, timeout}`（Worker 侧可重试瞬时类） |
| `ai01_provider_error_test_` | AI-01 provider 错误：任意 `{error, _}` 收敛 `provider_error` |
| `ai01_provider_crash_test_` | AI-01 provider 崩溃（throw）：catch 收敛 `provider_error` 不扩散 |
| `ai01_bad_json_test_` | AI-01 非法 JSON：content 非 JSON → `bad_output`（不可重试） |
| `ai01_valid_json_bad_schema_test_` | AI-01 合法 JSON 但缺必填键 → `bad_output` |
| `ai01_empty_content_test_` | AI-01 空 content / 无 content 无 result → `bad_output` |
| `validate_ok_whitelist_test` 等 7 例 | validate_result 直测：moments 空/超5/负值/非数值、outline 超3项/超200字节、文本缺/空/超300字节、needs_human_check 非布尔、confidence 越界丢弃（其余保留）、非 map 拒绝 |

### test/repo/teaching_ai_worker_tests.erl（真库 4323，每用例 BEGIN..ROLLBACK 不留数据；ID 段 97 前缀与 flow-98/bind-99 错开）

| 用例 | 覆盖路径 |
|---|---|
| `ai01_success_path_test_` | **AI-01 路径1 成功**：queued→running→succeeded；result_json 白名单 jsonb、model_profile=provider 名、completed_at 非空、error_code NULL；chat 恰 1 次 |
| `ai01_timeout_retry_then_cap_test_` | **AI-01 路径2 超时 + 重试上限**：第 1 次 attempt=1 < max=2 → requeued（DB：status=queued、ai_task_id='run:2'、error_code/completed_at 清空）；第 2 次 attempt=2 = max → 终态 failed(error_code='timeout', completed_at 非空)；chat 共 2 次（上限=2 显式体现） |
| `ai01_provider_unavailable_no_retry_test_` | **AI-01 路径3 provider 错误（显式例外）**：provider_unavailable **不重试**直接 failed；无 `run:%` 残留；chat 0 次 |
| `ai01_bad_json_failed_test_` | **AI-01 路径4 非法 JSON**：bad_output 不重试 → failed(bad_schema)；chat 1 次 |
| `ai01_attachment_deleted_test_` | **AI-01 路径5 附件删除**：practice_video 绑定关系移除 → failed(attachment_missing)；资源门先于 provider，chat 0 次 |
| `ai01_submission_withdrawn_test_` | 资源门变体：真实 withdraw_tx 后 → failed(submission_withdrawn)；chat 0 次 |
| `ai01_media_too_large_test_` | 媒体复核：attachment.size=150MB > 100MB 上限 → failed(media_too_large)；chat 0 次 |
| `ai03_degrade_human_loop_still_works_test_` | **AI-03 核心闭环**：registry 未命中 → failed(provider_unavailable) 后，**同一事务**内老师 upsert_draft + lock-first publish 成功、家长可读已发布回评（人工回课闭环不经 AI 仍通过） |
| `claim_fifo_test_` | 队列 FIFO：两 queued 只处理 created_at 最老一条（同事务时间戳恒定，显式错开保证确定性） |
| `queue_empty_done_test_` | 空队列 → `{ok, done}` |

## 2. 运行命令与输出摘要

```bash
cd /Users/leeyi/project/imboy.pub/imboy
erlc -I include -o test/ test/logic/teaching_ai_provider_tests.erl test/repo/teaching_ai_worker_tests.erl
# （输出无任何警告）

erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval 'R = eunit:test([teaching_ai_provider_tests, teaching_ai_worker_tests], [no_tty]), io:format("RESULT: ~p~n", [R]), case R of ok -> halt(0); _ -> halt(1) end.'
# RESULT: ok
# verbose 模式计数：provider 25 passed / worker 10 passed，0 failed 0 skipped

# 回归（与 Step 16 一起，见 STEP-16/notes.md「B 接线完成」末尾命令）
# teaching_flow_integration_tests + teaching_acl_tests + teaching_auth_logic_tests
#   + teaching_learner_bind_integration_tests + 本步两套 + handler 测试 → RESULT: ok
```

## 3. 实现要点与发现（后续维护者必读）

1. **fake provider 必须用真实存在的模块**：meck 无法创建磁盘上不存在的模块（meck_proc 报 undefined_module）。两套测试统一 meck `imboy_llm_qianfan`（全量替换 `chat/3` 与 `capabilities/0`，零真实网络；api_key 一律占位值 `test-key-placeholder`）。
2. **Worker 全局池查询的直连路由**：`load_context`（teaching_context_repo:submission_scope）与 `load_attachment` 走 `elib_pg:query/2`（全局连接池），直连测试不可达——worker 测试 meck `query/2` 转发到测试连接 C（同事务可见种子、受 ROLLBACK 隔离）；**带 Conn 的 `_tx` 函数（claim/finish/requeue 等）不经 mock，走真实实现**（query/3 在 passthrough 下直通）。
3. **dispatch_failure 返回的 `attempt` 字段 = 下一次尝试号**（`Attempt + 1`），不是本次尝试号。首次超时 requeue 返回 `attempt => 2`（回队为 run:2）。已用调试脚本对照 DB 行确认。
4. **同一用例内同一模块不可重复 setup_mock**（后者会卸掉前者的 mock）——`ai01_provider_crash` 曾因此误报 provider_unavailable。
5. 哨兵值教训：mock 用进程字典注入可变返回值时，`undefined` 不能同时当"未设置默认"与"显式置空"（用 `miss`/`unconfigured` 等显式哨兵原子）。
6. 附件删除路径按源码实际行为定：`load_attachment` 查 `submission_asset(kind='practice_video') JOIN attachment(status>=0)`——测试用 **DELETE 绑定关系** 模拟"附件已删除/未绑定"（worker 注释明确的等价形态）；未编造源码没有的行为。

## 4. 遗留缺口（不阻塞 AI-01/AI-03 验收，登记跟踪）

- **真实 vision provider 接入**：BLOCKED_EXTERNAL（imboy_llm 现有 provider vision 全 false）。AI-03 主降级路径 `provider_unavailable` 的骨架语义已由测试锁定，真实 provider 接入后无需改动 Worker。
- **stuck-row 回收器**：`run_once/0` 生产入口 claim 与 finish 分事务，worker 进程崩溃会遗留 `running` 行。本轮全部走 `run_once_tx/1` 直连入口（claim+process 同事务），生产入口的池连接行为未测。
- **multi-worker 并发互斥**：`FOR UPDATE SKIP LOCKED` 的多连接竞态未覆盖（单测试事务内不可构造，需双连接 fixture——可后续补）。

## 5. 缺口收口（R7）

Owner: Agent B | 日期: 2026-09-09 | 承接 §4 登记的三个缺口，全部收口。

### 5.1 缺口一：run_once/0 生产池入口（已收口）

`teaching_ai_worker_tests.erl` 新增 4 用例（夹具扩展：elib_pg `with_tx/1、with_tx/2` mock 转发为直接执行 `Tx(测试连接)`——外层 BEGIN..ROLLBACK 兜底，不开嵌套事务；`fake_with_tx` 进程字典哨兵注入失败形态）：

| 用例 | 断言 |
|---|---|
| `run_once_pool_success_test_` | `{ok, {processed, #{outcome => succeeded}}}`，DB 终态 succeeded |
| `run_once_pool_empty_test_` | 空队列 `{ok, done}` |
| `run_once_pool_claim_fail_closed_test_` | with_tx 注入 `{fail, conn_lost}` → `{error, claim_error}`，行仍 queued（fail-closed） |
| `run_once_pool_process_fail_closed_test_` | `{fail_second, _}`（claim 成功后 process 事务失败）→ `{error, worker_error}` |

### 5.2 缺口二：SKIP LOCKED 双连接并发互斥（已收口）

新增独立双连接夹具（`setup_pair`/`cleanup_pair`，无 meck）+ 2 用例：

- `claim_skip_locked_no_double_claim_test_`：两连接各自 BEGIN 并发 `claim_next_queued_tx`，两 queued 行各被一连接认领（A 得最老、B 得次老），**零双重认领、零丢行**；双视角断言（各自事务内自己的行 running、对方未提交行仍 queued）。
- `claim_skip_locked_single_row_test_`：A 认领唯一行后 B claim 得 `undefined`（不阻塞不重复）；**A ROLLBACK 释放锁后 B 可再认领同一行**——这正是 5.3 回收器能自愈卡死行的语义基础。

**关键教训（R7 首版失败根因）**：种子若放在 A 的未提交事务里，B 连接（READ COMMITTED）**根本看不见队列行**——并发用例的种子必须先提交（autocommit），只有 claim 留在未提交事务；已提交种子由 `cleanup_seed/1` 物理清理（逆 FK 序，setup 预清 + cleanup 兜底），不靠 ROLLBACK。

### 5.3 缺口三：stuck-row 回收器（已收口）

新增 `teaching_ai_worker:reclaim_stuck/0`（池入口，ecron/手动）与 `reclaim_stuck_tx/2`（事务内）：

- 判定：`status='running' AND created_at < now() - 阈值`（running 行无 completed_at 可依；worker 秒级处理，阈值即卡死证据）。动作：回 `queued`，清 error_code/completed_at。
- **保守模式（cleanup_unbound 同款）**：阈值下限 **300s 强制钳制在 reclaim_stuck_tx 内**（对一切调用方生效，不止池入口）；默认 900s，经 `{teaching_ai_stuck_age_seconds, N}` 可配。
- **防毒行永动**：ai_task_id（run:N 计数）保留不重置——被回收的坏行重跑即达上限 failed（`reclaim_stuck_retry_budget_preserved_test_` 实证：timeout 毒行回收后重跑直接 failed，chat 总调用恰 2 次）。
- 测试 4 例全绿：`reclaim_stuck_basic`（超龄回收/阈值内不动）、`reclaim_stuck_min_age_clamp`（传 1s 按 300s 执行，10 秒在处理行不被误回收）、`reclaim_stuck_retry_budget_preserved`、`reclaim_stuck_pool_entry`。

### 5.4 【跨任务】ecron `{jobs,[]}` 键从未被消费——修复 + 影响面登记

**（本条为 boot 冒烟发现、Coordinator 指派 B 落地的 pre-existing 真 bug，非本任务引入）**

- 根因：ecron pin v1.1.0 的 `ecron_sup` 只读 `global_jobs`/`local_jobs`（deps/ecron/src/ecron_sup.erl:18/44/45），**无任何代码消费 `{jobs, ...}`**——sys.config.example:526 起整个 jobs 块（attachment_orphan_cleanup / attachment_pending_cleanup / teaching_unbound_cleanup / teaching_ai_worker / **B-06 payment_reconcile** / red_packet_expire_refund / ops_weekly_report）从未被调度。
- **影响面登记（给 Coordinator/运维）**：用户双节点（9800/9801，加载 sys.runtime.config）上 **B-06 支付对账、附件孤儿/pending 清理、教学未绑定附件回收在历史上从未生效**；用户重启节点前不会自愈。建议协调者在 final-report 单列风险条目并安排重启窗口。
- 修复：`{jobs, [` → **`{local_jobs, [`**。选型理由：① 块内全部作业幂等/有守卫（AI Worker 认领=原子 UPDATE...SKIP LOCKED，多 worker 天然安全——5.2 双连接测试实证；清理类 age+NOT EXISTS 守卫；支付对账本身幂等补单），双节点重复执行安全；② 不依赖 global 注册/quorum，网络抖动/注册异常不会静默停摆全部作业；③ Coordinator 冒烟正在验证 local_jobs 形态可用（已被 5.5 独立 mini-boot 复证）。
- 新增 ecron 条目（置于修复后的正确键下）：`{teaching_ai_stuck_reclaim, "*/10 * * * *", {teaching_ai_worker, reclaim_stuck, []}}`。
- 验证：① `file:consult` OK，local_jobs=8 项、无残留 `jobs` 键；② 独立 mini-boot（`erl -pa deps/ecron/ebin -pa deps/telemetry/ebin` + set_env 后 `application:ensure_all_started(ecron)`，未触碰 9800/9801/9811 节点）：`ecron:statistic()` 返回 **8 个作业全部 activate**。

### 5.5 R7 验证记录

```bash
erlc -I include -o ebin src/logic/teaching_ai_worker.erl   # 零警告
erlc -I include -o test/ test/repo/teaching_ai_worker_tests.erl   # 零警告

# 定向（22 用例：R6 10 + R7 12）
erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval 'R = eunit:test([teaching_ai_worker_tests], [no_tty]), ...'
# RESULT: ok

# 回归（R6 七模块清单原样重跑）
# flow + acl + auth_logic + learner_bind_integration + ai_provider + ai_worker + bind_handler
# REGRESSION: ok（EXIT=0）

make compile   # EXIT=0，零源码警告
```

### 5.6 R7 遗留

- 无新缺口。§4 三缺口全部关闭；真实 vision provider 仍 BLOCKED_EXTERNAL（与 R6 相同，非本轮范围）。
- 双节点 `local_jobs` 重复执行的负载特征（每节点每分钟一次 worker 空轮询）可接受；若未来作业量增大可再评估 global_jobs + quorum。
