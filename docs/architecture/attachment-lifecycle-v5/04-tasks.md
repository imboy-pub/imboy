# W0—W6 原子任务 V5.5

本文件与execution-contract.json构成同一任务合同；20任务/七Gate全部NOT_STARTED。新runner/gate均由W0-03交付，命令目前是规划接口。本次只修订计划，不实施、不commit。

## 公共执行边界

后端及gate命令cwd=imboy；flutter/dart命令cwd=imboyapp；bun命令cwd=imboyadmin。所有路径前缀imboyapp:/imboyadmin:表示独立仓，其余归imboy。先将new/动态迁移占位解析成唯一owned清单并检查foreign/staged冲突，未完成不得写入。当前App ios两个外部WIP属于禁止修改区；其他foreign见baseline。moment_ds已提交成为基线，仍需写前重采样。

准入必须是新可信用户授权receipt，绑定本manifest和baseline；implementation与local_commit分别授权。本轮DESIGN_ONLY/false保持。获得包含任务提交的授权后，每task是完整可评审功能提交；独立取证/文档可提交必要脱敏摘要，不造空commit。跨仓逐仓提交并形成完整向量；无权commit不得伪报已提交/PASS，不push/部署。Git身份仅命令作用域leeyi <leeyisoft@qq.com>。

公共恢复R：保留原始输出及故障分类；基础设施重试≤2、代码修复≤2轮、任务总尝试≤5。每次启动前持久attempt+1，崩溃照计；task默认28800s（W1-02为57600s、W6-01为172800s、W6-02为43200s）、wave默认86400s（W6为345600s）、run604800s从首次启动跨重启保留。外部缺失BLOCKED_EXTERNAL不轮询；安全断言失败先独立分类：既定范围内实现错误可在修复预算内重试；设计冲突CONTRACT_CONFLICT；unknown COMMIT先只读调和。超预算NO_GO，不换run_id重置预算。条件改变只能在剩余预算内恢复。
公共回滚B：停本task worker/物理GC，只撤销本task拥有变更或在授权下反向提交，不reset/clean/stash、不动foreign；DDL按03的Down阻断/forward-fix；已物理删除需已验证备份，否则IRREVERSIBLE。

每task证据E：三仓输入HEAD/owned tree digest/plan和schema与fixture hash、授权receipt及资源白名单、命令/cwd/脱敏环境、开始结束时间和exit、唯一叶的executed/pass/fail/skip与pre/action/post oracle、原始输出hash、主执行复核、完整commit向量。真实存储另需精确I、前后字节hash/独立DB可见性/请求与lease来源；真机与浏览器另需真实设备/进程来源。历史日志/mock/HTTP200/0执行/required skip不得代验收。每case精确scenario/driver/phase集合见JSON，数量不加分。

现有make suite选择必须转换为 `make eunit-local t=<module> EUNIT_CONFIG=<run-owned绝对路径无后缀>`，最终全量去掉t；临时配置只连scratch合成库，禁推送/定时GC/自动迁移，不加载sys.local默认业务库。gate是后端cwd命令，即使前一个命令在App/Admin也必须切回；所有--case带--task，四种schema-phase仅base/expand-queue/guards-contract/integrated。case/phase缺失非零退出，不skip。

## W0-01 — 冻结三仓与工具链

- TASK_ID：`W0-01`；依赖：无；除W0外前一Wave Gate须PASS。
- 代码证据：F18；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`docs/architecture/attachment-lifecycle-v5/evidence/w0-01-input-summary.json (new; sanitized task output, outside frozen input manifest)`。
- 只读取证文件：`imboyapp:lib/service/message_retry.dart`；`scripts/check_migrations.sh`。
- 实现边界：复核HEAD、dirty文件哈希、迁移/依赖/运行时版本；后续编辑必须独占task路径。未跟踪冻结输入仅按JSON frozen_input_commit_paths核验原字节并与本任务摘要精确提交，不编辑或宽泛stage目录。
- SQL影响：只读catalog和scratch库；schema-phase=base。
- 测试命令：`bash scripts/check_migrations.sh`。
- 断言：三仓与manifest匹配；foreign diff未变；迁移数非零且最新版本一致。
- 必验Case：`W0-01/baseline`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`docs(attachments): w0-01 freeze lifecycle v5 evidence`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W0-02 — 重建Schema与引用覆盖矩阵

- TASK_ID：`W0-02`；依赖：W0-01；除W0外前一Wave Gate须PASS。
- 代码证据：F02, F13, F14；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`docs/architecture/attachment-lifecycle-v5/evidence/w0-02-input-summary.json (new; sanitized task output, outside frozen input manifest)`。
- 只读取证文件：`imboyadmin:src/services/api/client.ts`；`priv/migrations/`；`src/repo/attachment_repo.erl`；`src/repo/user_collect_repo.erl`；`src/features/enterprise_business/infrastructure/eb_pg_store_sql.erl`。
- 实现边界：仅scratch原Up重放与引用/位置静态矩阵；base不查询未创建V5字段；不要求未来strict runner；无授权历史副本时该分支BLOCKED且历史保持保护。
- SQL影响：catalog、索引、trigger、FK与历史合成fixture；schema-phase=base。
- 测试命令：`docker exec -i "$V5_PG_CONTAINER" psql -X -U "$V5_PG_USER" -d "$V5_SCRATCH_DB" -v ON_ERROR_STOP=1 < docs/architecture/attachment-lifecycle-v5/evidence/catalog-query.sql`。
- 断言：原Up/catalog完整；静态unknown保护矩阵完整；strict ledger由W0-03实测；运行时GC资格由W3/W6验证。
- 必验Case：`W0-02/schema`、`W0-02/history-unknown`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`docs(attachments): w0-02 freeze lifecycle v5 evidence`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W0-03 — 建立最小持久执行与证据验证器

- TASK_ID：`W0-03`；依赖：W0-02；除W0外前一Wave Gate须PASS。
- 代码证据：F01, F10；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`scripts/attachment_lifecycle_v5_runner.py (new)`；`scripts/test/attachment_lifecycle_v5_gate.sh (new)`。
- 只读取证文件：`deps/erlang_migrate/src/erlang_migrate.erl`；`deps/erlang_migrate/src/erlang_migrate_pg.erl`。
- 实现边界：主执行器与独立Reviewer均模型无关；只实现本计划的单writer日志、限次恢复、准入及证据验证，不造通用Agent平台；strict ledger在本任务生成runner后实测并追验W0-01/02；交付06的规划CLI、双W5任务调度/单writer合并验收、34变异与4项并行故障自测；不把runner当自动生成业务代码的通用平台。
- SQL影响：不连接业务DB；runner状态在工作区.Codex/runs中；schema-phase=base。
- 测试命令：`python3 scripts/attachment_lifecycle_v5_runner.py self-test`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W0-03`。
- 断言：杀runner续跑不重做已提交任务；错hash/空测试/缺AC/伪exit/循环重试负例全部拒绝；活holder SIGSTOP后取锁≤2秒失败，权威state/events不变且无子动作。
- 必验Case：`W0-03/runner-selftest`、`W0-03/strict-ledger-bootstrap`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`test(attachments): add bounded lifecycle runner and evidence gates`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W1-01 — 冻结状态机、锁序与存储接口

- TASK_ID：`W1-01`；依赖：W0-03；除W0外前一Wave Gate须PASS。
- 代码证据：F03, F08, F18；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`docs/adr/0008-pluggable-attachment-storage.md`；`docs/architecture/attachment-lifecycle-v5/evidence/w1-contract-output.json (new; task output, outside frozen input manifest)`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：将Proposed存储方向与V5安全边界对齐；不能继续引用缺失旧计划；不实施存储搬迁; base catalog plus static lifecycle contract only, no dependency on W1-02 SQL。
- SQL影响：无DDL；冻结锁序/决策矩阵；schema-phase=base。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W1-01 --case contract`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W1-01`。
- 断言：八AC无空项；每个删除/引用入口映射；key终身不复用；Hold线性化明确。
- 必验Case：`W1-01/contract`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`docs(attachments): w1-01 freeze lifecycle v5 evidence`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W1-02 — 实现并验证Expand/Queue/Guards迁移

- TASK_ID：`W1-02`；依赖：W1-01；除W0外前一Wave Gate须PASS。
- 代码证据：F02, F06, F10；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`priv/migrations/<next>_attachment_lifecycle_v5_expand.{up,down}.sql (new)`；`priv/migrations/<next>_attachment_lifecycle_v5_queue.{up,down}.sql (new)`；`docs/architecture/attachment-lifecycle-v5/guards-contract.sql (new; deferred activation W4-04)`；`test/repo/attachment_lifecycle_v5_pg_tests.erl (new)`；`src/repo/attachment_gc_repo.erl`；`src/ds/attachment_ds.erl`；`docs/architecture/attachment-lifecycle-v5/evidence/w1-function-inventory.json (new; task output, outside frozen manifest)`。
- 只读取证文件：`priv/migrations/00000168_workspace_attachment_retention.up.sql`。
- 实现边界：按03 SQL合同完成，不改既有迁移；迁移号动态冻结；未知行保护；按migration_function_install_contract将前置调用依赖全部放M2，禁止W4调用尚未安装的M3函数。
- SQL影响：M1字段；M2两queue及全部协议函数/受限EXECUTE；M3仅独立scratch守卫/遗留权限收紧，正式M3仍W4-04进入priv；enterprise_recovery_tombstone合同必需，禁止另建session表。；schema-phase=expand-queue。
- 测试命令：`bash scripts/check_migrations.sh`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W1-02`。
- 断言：直接DML篡改位置/归属失败；合法首绑成功；NULL绕CHECK失败；旧数据无误标；非空Down阻断; preserve both existing queue PKs, attempts and tombstones; no third queue; confirmed FK compatible with metadata tombstone；两个队列最小权限与intent-authority真实角色DML负例；合法审计actor匿名化；expand已安装全部调用函数；M3不补缺函数；两个快照受约束且普通confirmed GC保留关联。
- 必验Case：`W1-02/migration`、`W1-02/immutable`、`W1-02/down-block`、`W1-02/session-link-integrity`、`W1-02/intent-authority`、`W1-02/audit-actor-anonymization`、`W1-02/registration-integrity`、`W1-02/recovery-tombstone-integrity`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w1-02 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W2-01 — Pending失败关闭与冻结身份

- TASK_ID：`W2-01`；依赖：W1-02；除W0外前一Wave Gate须PASS。
- 代码证据：F01, F02, F03；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/logic/attach_logic.erl`；`src/repo/attach_pending_repo.erl`；`src/logic/enterprise_asset_logic.erl`；`src/repo/enterprise_asset_repo.erl`；`src/ds/group_file_ds.erl`；`src/logic/attachment_upload_logic.erl`；`src/api/attach_handler.erl`；`src/api/enterprise_asset_handler.erl`；`src/api/enterprise_internal_idempotency.erl`；`src/features/enterprise_business/interfaces/eb_tenant_handler.erl`；`src/features/enterprise_business/application/asset/eb_asset_app.erl`；`src/features/enterprise_business/infrastructure/eb_pg_asset_meta.erl`；`src/features/enterprise_business/infrastructure/eb_asset_store.erl`；`src/features/customer_service/interfaces/cs_widget_handler.erl`；`src/features/customer_service/application/widget/cs_widget_session_app.erl`；`src/imboy_router.erl`；`api/openapi-api.yaml`；`api/openapi-internal.yaml`；`src/features/enterprise_business/interfaces/eb_enterprise_actions.erl`；`src/features/enterprise_business/interfaces/eb_enterprise_http.erl`；`src/features/customer_service/interfaces/cs_actions.erl`；`src/features/customer_service/interfaces/cs_http.erl`；`src/features/customer_service/application/widget/cs_widget_support.erl`；`test/features/enterprise_business/application/asset/eb_asset_tests.erl`；`test/features/customer_service/interfaces/cs_widget_handler_tests.erl`；`src/api/attachment_storage_gateway_handler.erl (new)`；`src/lib/imboy_jwt.erl`；`src/lib/elib_oss.erl`；`src/lib/elib_s3_sign.erl`；`test/lib/elib_s3_sign_tests.erl`；`test/api/attachment_storage_gateway_handler_tests.erl (new)`；`src/features/enterprise_business/infrastructure/eb_asset_object_garage.erl`。
- 只读取证文件：`src/features/enterprise_business/enterprise_business_facade.erl`；`src/features/customer_service/customer_service_facade.erl`。
- 实现边界：实现upload_recovery_api的register/status/renew；外层handler幂等事务只保存稳定登记结果，COMMIT后签名；普通/内部应用/EB坐席/widget全部入口保持旧字段；EB presign先登记，PUT复用CAS；动作参数投影按operation条件校验register声明/status查找/confirm二选一；新字段不得被action表丢弃，status响应不得误补upload.url；先交付Garage受控裸PUT gateway、upload_registration快照及upload_content首次pin，同字节重试；保留旧响应字段但不再给新会话可变内容直传能力；企业适配器同W2交付冻结签名Context端到端传递；恢复同时查live/queue终态，复用已有Widget epoch/Seat abort保护。
- SQL影响：复用M1 client_upload_id/session deadline/last capability expiry；登记与稳定幂等结果同事务；未知COMMIT只读调和；不存签名URL；M1两个不可变JSON快照、M2 register/pin等函数已安装；网关pin COMMIT之后才发送已校验字节；enterprise_recovery_tombstone合同必需，禁止另建session表。；schema-phase=expand-queue。
- 测试命令：`make eunit t=attachment_upload_logic_tests`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W2-01`。
- 断言：登记失败无URL；DB异常0签名；scope mismatch拒绝；事务提交未知先查询恢复；不同字节409/provider PUT=0；pin未知COMMIT不PUT；旧客户端裸PUT成功且不能绕过网关。
- 必验Case：`W2-01/pending-failclosed`、`W2-01/pending-multipart`、`W2-01/session-recovery-api`、`W2-01/upload-content-pin`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w2-01 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W2-02 — Confirm幂等与兼容回读

- TASK_ID：`W2-02`；依赖：W2-01；除W0外前一Wave Gate须PASS。
- 代码证据：F04, F05；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/logic/attach_logic.erl`；`src/ds/attachment_ds.erl`；`src/repo/attachment_repo.erl`；`src/logic/enterprise_asset_logic.erl`；`src/repo/enterprise_asset_repo.erl`；`src/api/attach_handler.erl`；`src/api/enterprise_asset_handler.erl`；`src/features/enterprise_business/application/asset/eb_asset_app.erl`；`src/features/enterprise_business/interfaces/eb_tenant_handler.erl`；`src/features/customer_service/interfaces/cs_widget_handler.erl`；`src/features/enterprise_business/interfaces/eb_enterprise_actions.erl`；`src/features/enterprise_business/interfaces/eb_enterprise_http.erl`；`src/features/customer_service/interfaces/cs_actions.erl`；`src/features/customer_service/interfaces/cs_http.erl`；`src/features/customer_service/application/widget/cs_widget_support.erl`；`test/features/enterprise_business/application/asset/eb_asset_tests.erl`；`test/features/customer_service/interfaces/cs_widget_handler_tests.erl`；`src/lib/elib_oss.erl`；`test/api/attachment_storage_gateway_handler_tests.erl (new)`。
- 只读取证文件：`src/features/enterprise_business/enterprise_business_facade.erl`；`src/features/customer_service/customer_service_facade.erl`。
- 实现边界：metadata insert-if-absent与真实引用新增拆开；同pending session原子confirm；首审计一次；保留md5输入回退；EB按授权asset_id恢复confirm不要求持久化upload_ref；动作参数投影按operation条件校验register声明/status查找/confirm二选一；新字段不得被action表丢弃，status响应不得误补upload.url；精确对象真实读流SHA256/size与durable content一致后确认，HEAD不代替字节校验。
- SQL影响：session confirmed+asset同事务；EAR同事务注册；schema-phase=expand-queue。
- 测试命令：`make eunit t=attach_logic_tests`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W2-02`。
- 断言：100次同请求row=1、引用不增；conflicting scope/cipher拒绝；响应丢失回读同ID；真实对象hash/size匹配后确认；旧能力不能把已确认A改为B。
- 必验Case：`W2-02/confirm-idempotent`、`W2-02/confirm-old-client`、`W2-02/confirmed-content-immutable`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w2-02 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W2-03 — Confirm/GC状态协议屏障

- TASK_ID：`W2-03`；依赖：W2-02；除W0外前一Wave Gate须PASS。
- 代码证据：F05, F09；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/repo/attach_pending_repo.erl`；`src/ds/attachment_ds.erl`；`src/features/enterprise_business/infrastructure/eb_pg_asset_meta.erl`；`test/repo/attachment_lifecycle_v5_pg_tests.erl (new)`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：真实双连接同步屏障，不用sleep猜并发；先使用存储调用记录器证明无未提交删除，W6再接真实字节；不宣称旧GC入口已修复。
- SQL影响：对象锁/session行锁/CAS；schema-phase=expand-queue。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W2-03 --case confirm-gc-race`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W2-03`。
- 断言：生产Repo状态协议两个提交顺序唯一胜出；真实GC入口与字节保护W4/W6验收。
- 必验Case：`W2-03/confirm-gc-race`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w2-03 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W3-01 — 业务引用与注销保护

- TASK_ID：`W3-01`；依赖：W2-03；除W0外前一Wave Gate须PASS。
- 代码证据：F06, F12, F13, F14；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/ds/group_file_ds.erl`；`src/ds/moment_ds.erl`；`src/logic/user_collect_logic.erl`；`src/logic/channel_logic_message.erl`；`src/lib/user_deletion_executor.erl`；`src/repo/moya_submission_repo.erl`；`src/repo/attachment_repo.erl`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：已有关系首绑/释放加入同锁；普通scope未知或收藏/密文映射不全则保留；不新建引用总账。
- SQL影响：关系原子写；gc_verified只有覆盖证明后true；schema-phase=expand-queue。
- 测试命令：`make eunit t=group_file_atomic_pg_tests`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W3-01`。
- 断言：收藏后原帖删除/上传者注销仍可读；历史/密文未知0删除；新增引用对墓碑失败。
- 必验Case：`W3-01/reference-gc-race`、`W3-01/shared-owner`、`W3-01/history-unknown`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w3-01 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W3-02 — Retention/Hold统一资格

- TASK_ID：`W3-02`；依赖：W3-01；除W0外前一Wave Gate须PASS。
- 代码证据：F07, F11；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/logic/enterprise_asset_retention_logic.erl`；`src/repo/enterprise_attachment_retention_repo.erl`；`src/features/enterprise_business/infrastructure/eb_pg_purge.erl`；`src/features/enterprise_business/infrastructure/eb_pg_store.erl`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：两域统一资格和Hold竞争协议；按12入口映射测试协议，真实入口改造及整矩阵复验属W4-01，不以前向未实施入口作为G3条件。
- SQL影响：expand-queue中的M2资格/治理协议；direct DML触发器另在guards-contract证明，正式激活属W4-04；schema-phase=expand-queue。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W3-02 --case hold-all-entrypoints`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W3-02`。
- 断言：协议层48 Hold组合+24 Retention组合拒绝意图；intent先提交Hold冲突；不声称所有生产入口已安全。
- 必验Case：`W3-02/hold-all-entrypoints`、`W3-02/hold-gc-race`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w3-02 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W3-03 — 企业/客服原子绑定回归

- TASK_ID：`W3-03`；依赖：W3-02；除W0外前一Wave Gate须PASS。
- 代码证据：F08, F15；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/features/enterprise_business/infrastructure/eb_pg_store.erl`；`src/features/enterprise_business/infrastructure/eb_pg_store_sql.erl`；`src/features/customer_service/application/session/cs_session_app.erl`；`src/features/customer_service/application/widget/cs_widget_session_app.erl`；`src/features/enterprise_business/infrastructure/eb_pg_canonical_tx.erl`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：复用canonical append及asset_ids、组织/Workspace两键；修复只限锁顺序/资格，不碰Seat WIP；新引用只接受经过content校验的confirmed资产；旧历史继续原读兼容与未知保护。
- SQL影响：绑定资产+消息+审计同事务；schema-phase=expand-queue。
- 测试命令：`make eunit t=eb_message_assets_projection_tests`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W3-03`。
- 断言：重复client_msg_id无重复引用；多asset部分失败全回滚；无权跨组织业务归属访问拒绝。
- 必验Case：`W3-03/enterprise-bind`、`W3-03/cs-bind`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w3-03 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W4-01 — 收口全部物理删除入口

- TASK_ID：`W4-01`；依赖：W3-03；除W0外前一Wave Gate须PASS。
- 代码证据：F06, F07, F11, F12；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/ds/attachment_ds.erl`；`src/logic/attach_cleanup_logic.erl`；`src/logic/attach_logic.erl`；`src/logic/enterprise_asset_logic.erl`；`src/logic/enterprise_asset_retention_logic.erl`；`src/logic/moya_attach_logic.erl`；`src/logic/user_deletion_logic.erl`；`src/lib/user_deletion_executor.erl`；`src/features/enterprise_business/infrastructure/eb_asset_store.erl`；`src/repo/attachment_gc_repo.erl`。
- 只读取证文件：`priv/migrations/00000168_workspace_attachment_retention.up.sql`。
- 实现边界：普通GC/拒绝上传/教学/注销/企业显式删除按所属域写对应既有队列意图，共享删除安全协议；不在DB事务中网络DELETE。
- SQL影响：资格+metadata/job/audit同事务；失败rollback；schema-phase=expand-queue。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-01 --case intent-rollback`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-01`。
- 断言：扫描所有直接调用只允许driver/worker；audit/commit失败存储DELETE=0；未知返回不作ok；生产入口重跑三类竞态。
- 必验Case：`W4-01/delete-entrypoints`、`W4-01/intent-rollback`、`W4-01/confirm-gc-race`、`W4-01/reference-gc-race`、`W4-01/hold-gc-race`、`W4-01/hold-entrypoints`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w4-01 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W4-02 — 可靠Worker与旧任务接管

- TASK_ID：`W4-02`；依赖：W4-01；除W0外前一Wave Gate须PASS。
- 代码证据：F08, F09, F10；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/features/enterprise_business/infrastructure/eb_pg_asset_delete_queue.erl`；`src/features/enterprise_business/infrastructure/eb_pg_asset_meta.erl`；`src/features/enterprise_business/infrastructure/eb_pg_purge.erl`；`src/features/enterprise_business/application/retention/eb_retention_purge_worker.erl`；`src/repo/attachment_gc_repo.erl`；`src/ds/attachment_ds.erl`。
- 只读取证文件：`priv/migrations/00000168_workspace_attachment_retention.up.sql`。
- 实现边界：Extend both existing queues; preserve ordinary tombstones and legacy attempts; CAS lease/backoff/reaper; enterprise pending flag joins enterprise queue; no queue consolidation；实际Purge先复制永久恢复UUID/快照再删metadata，register与Purge同UUID锁；与迁移172访客阅读级联兼容。
- SQL影响：M2字段；SKIP LOCKED；旧flag→queue CAS；enterprise_recovery_tombstone合同必需，禁止另建session表。；schema-phase=expand-queue。
- 测试命令：`make eunit t=eb_retention_purge_worker_tests`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-02`。
- 断言：两个worker无丢任务；旧token0更新；8次终止；毒任务不饿死后续；无对象身份复用。
- 必验Case：`W4-02/worker-crash`、`W4-02/worker-lease`、`W4-02/worker-db-fault`、`W4-02/queue-adoption`、`W4-02/last-attempt-crash`、`W4-02/old-attempts-preserved`、`W4-02/cross-queue-identity`、`W4-02/ordinary-queue-recovery`、`W4-02/claim-capacity-and-liveness`、`W4-02/purged-session-recovery`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w4-02 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W4-03 — Storage Boundary与Garage/Disk闭环

- TASK_ID：`W4-03`；依赖：W4-02；除W0外前一Wave Gate须PASS。
- 代码证据：F03, F10, F18；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/lib/elib_oss.erl`；`src/lib/attachment_storage/attachment_storage.erl (new)`；`src/lib/attachment_storage/attachment_storage_driver.erl (new)`；`src/lib/attachment_storage/attachment_storage_garage.erl (new)`；`src/lib/attachment_storage/attachment_storage_disk.erl (new)`；`src/api/attachment_storage_gateway_handler.erl (created W2-01)`；`src/imboy_router.erl`；`config/sys.config`；`src/features/enterprise_business/infrastructure/eb_asset_object_garage.erl`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：按完整I dispatch；复制现有Garage行为到driver后逐调用替换，政策函数留业务侧；Disk流式网关/原子写/路径与签名校验；无COS/OSS占位；不配置外部域名; after storage-disk PASS rerun deferred predecessor Disk protocol scenarios before G4；gateway已由W2-01提供Garage路径，本任务迁移到统一driver并实现Disk同I OS锁；不依赖Garage条件写/版本，所有写路径必须经pin。
- SQL影响：位置只影响新上传；无桶搬迁；schema-phase=expand-queue。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-03 --case storage-switch`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-03`。
- 断言：真实两驱动PUT/HEAD/GET/Range/DELETE；改默认driver不改旧对象；符号链接/..拒绝；Garage迟到PUT未证则pending回收blocked；两driver同key内容不可变；Disk迟到PUT不得在DELETE后重建；Garage未知drain禁止GC；既有限额资源与兼容通过。
- 必验Case：`W4-03/storage-garage`、`W4-03/storage-disk`、`W4-03/storage-switch`、`W4-03/late-put`、`W4-03/namespace-alias-integrity`、`W4-03/disk-predecessor-protocols`、`W4-03/driver-content-immutable`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w4-03 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W4-04 — 调和与可观测性

- TASK_ID：`W4-04`；依赖：W4-03；除W0外前一Wave Gate须PASS。
- 代码证据：F05, F10；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`src/logic/attach_cleanup_logic.erl`；`src/features/enterprise_business/infrastructure/eb_pg_asset_delete_queue.erl`；`src/adm/adm_attach_handler.erl`；`priv/migrations/<next>_attachment_lifecycle_v5_guards.{up,down}.sql (new)`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：只调和已登记精确对象；失败分类/age/attempt/blocked计数；日志不含签名URL或PII；最后激活M3迁移并验证所有已适配写入口。
- SQL影响：bounded keyset扫描；不做全桶删除；schema-phase=integrated。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-04 --case reconcile`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-04`。
- 断言：DELETE后落账丢失能重试not_found；未知对象永不被删；队列积压可解释；M3激活后所有写入口通过直接DML/业务回归。
- 必验Case：`W4-04/reconcile`、`W4-04/no-infinite-loop`、`W4-04/guards-activation`、`W4-04/all-writer-compat`、`W4-04/activation-commit-unknown`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w4-04 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W5-01 — Flutter恢复同一上传与消息

- TASK_ID：`W5-01`；依赖：W4-04；除W0外前一Wave Gate须PASS。
- 代码证据：F16；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`imboyapp:lib/store/api/attachment_api.dart`；`imboyapp:lib/page/chat/chat/attachment_handler.dart`；`imboyapp:lib/service/e2ee/attachment_descriptor.dart`；`imboyapp:test/unit_test/store/attachment_upload_presign_test.dart`；`imboyapp:lib/service/migrations/manifest_all.dart`；`imboyapp:lib/service/migrations/upgrade_edges_v33.dart (new; recheck version)`；`imboyapp:lib/service/migrations/downgrade_edges_all.dart`；`imboyapp:lib/component/upload/batch_upload_controller.dart`；`imboyapp:lib/service/assets.dart`；`imboyapp:lib/config/init.dart`；`imboyapp:lib/store/repository/user_repo_local.dart`；`imboyapp:lib/store/repository/attachment_upload_task_repo_sqlite.dart (new)`；`imboyapp:test/unit_test/store/attachment_upload_task_recovery_test.dart (new)`；`imboyapp:lib/service/e2ee/attachment_encryptor.dart`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：扩展既有上传编排，保存key/cipher descriptor/阶段/业务幂等键；不持久化URL；敏感密钥只用既有安全存储；新key必须重新seal，不能复用绑定旧key密文；持久保存/安全引用同一contentKey、baseNonce、descriptor与密文文件，进程重启不得新随机seal到旧key；无法恢复同密文则显式新任务/新key；服务端只接收密文hash。
- SQL影响：SQLite migration manifest additive edge; generated SQL only; Down protects pending sessions；schema-phase=integrated。
- 测试命令：`flutter test test/unit_test/store/attachment_upload_presign_test.dart`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W5-01`。
- 断言：杀进程后PUT成功confirm未知可回读重试，无新key/重复发送；logout隔离用户；旧密文可读。
- 必验Case：`W5-01/flutter-resume`、`W5-01/flutter-old-client`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w5-01 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：client-app；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W5-02 — Admin治理与状态同步

- TASK_ID：`W5-02`；依赖：W4-04；除W0外前一Wave Gate须PASS。
- 代码证据：F17, F07；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`imboyadmin:src/pages/storage/StorageOverviewPage.tsx`；`imboyadmin:src/services/api/storage.ts`；`imboyadmin:src/widget/customer_service/app/attachmentMachine.ts`；`src/adm/adm_attach_handler.erl`；`imboyapp:lib/modules/enterprise_app/presentation/asset_retention_panel.dart`；`imboyadmin:src/widget/customer_service/app/uploader.ts`；`imboyadmin:src/modules/customer_service/seat/workbench/seatSend.ts`；`imboyadmin:src/widget/customer_service/app/uploader.test.ts`；`imboyadmin:src/modules/customer_service/seat/workbench/attachmentUpload.ts`；`imboyadmin:src/modules/customer_service/seat/workbench/workbenchApi.ts`；`imboyadmin:src/modules/customer_service/seat/workbench/workbenchApi.test.ts`；`imboyadmin:src/modules/customer_service/seat/workbench/seatSend.scopeCommit.test.tsx`；`imboyadmin:src/modules/customer_service/seat/workbench/seatSend.attachmentRecovery.test.tsx`；`imboyadmin:src/modules/customer_service/seat/workbench/seatSend.revocation.test.tsx`；`imboyadmin:src/widget/customer_service/app/widgetApi.ts`；`imboyadmin:src/widget/customer_service/app/widgetApi.test.ts`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：现有界面追加逻辑已删/物理待删/阻断原因，不建新控制台；保留EntityId与分页；与Seat WIP冲突则停该任务；attachmentUpload.ts在每个远程阶段边界await持久化回调；API adapters支持status/renew，不让seatSend只在confirm后收到结果；旧账号迟到回调禁止写入新账号journal。
- SQL影响：查询状态接口；无客户端绕过hold的force按钮；schema-phase=integrated。
- 测试命令：`bun run test src/pages/storage/StorageOverviewPage.test.tsx src/widget/customer_service/app/attachmentMachine.test.ts src/widget/customer_service/app/uploader.test.ts src/widget/customer_service/app/widgetApi.test.ts src/modules/customer_service/seat/workbench/workbenchApi.test.ts src/modules/customer_service/seat/workbench/seatSend.scopeCommit.test.tsx src/modules/customer_service/seat/workbench/seatSend.attachmentRecovery.test.tsx src/modules/customer_service/seat/workbench/seatSend.revocation.test.tsx`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W5-02`。
- 断言：purged不误报字节已删；超时显示未知可恢复；无权访问其他组织/Workspace时看不到job；linked才成功。
- 必验Case：`W5-02/admin-governance`、`W5-02/status-sync`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w5-02 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：client-admin；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W6-01 — 真实PG/存储故障矩阵

- TASK_ID：`W6-01`；依赖：W5-01,W5-02；除W0外前一Wave Gate须PASS。
- 代码证据：F01, F05, F07, F10；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`test/repo/attachment_lifecycle_v5_pg_tests.erl (new)`；`scripts/test/attachment_lifecycle_v5_gate.sh (new)`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：一套合成固定种子数据；真实PG两连接/真实Garage专用bucket/专用Disk目录；注入仅影响本run子进程。
- SQL影响：scratch升级/故障恢复；schema-phase=integrated。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W6-01 --case real-pg-garage-disk`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W6-01`。
- 断言：AC01-07每项实际执行；HTTP和SQL和字节hash三方对应；无skip无stub。
- 必验Case：`W6-01/real-pg-garage-disk`、`W6-01/all-ac01-ac07`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w6-01 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W6-02 — 三端E2E与历史兼容

- TASK_ID：`W6-02`；依赖：W6-01；除W0外前一Wave Gate须PASS。
- 代码证据：F13, F14, F15, F16, F17；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`imboyapp:integration_test/attachment_lifecycle_v5_test.dart (new)`；`imboyadmin:tests/e2e/attachment-lifecycle-v5.spec.ts (new)`；`scripts/test/attachment_lifecycle_v5_gate.sh (new)`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：真机Android/macOS设备必须由W0核验可用，不改ios/macos保留区；Admin真实浏览器；真实后端/对象存储，禁止route.fulfill替换核心链路。
- SQL影响：仅合成数据；schema-phase=integrated。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W6-02 --case e2e-compat`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W6-02`。
- 断言：旧请求fixture和历史合成附件、E2EE、多业务、断网/重启全链路；UAT仍由用户签署。
- 必验Case：`W6-02/e2e-compat`、`W6-02/ac08`、`W6-02/flutter-recovery-real`、`W6-02/admin-recovery-real`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w6-02 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## W6-03 — 冻结候选全量回归及独立验收

- TASK_ID：`W6-03`；依赖：W6-02；除W0外前一Wave Gate须PASS。
- 代码证据：F01, F18；01中的实际路径/函数及F19～F27、current-baseline。
- 实际/新增文件：`docs/architecture/attachment-lifecycle-v5/evidence/final-candidate-summary.json (new; task output, outside frozen input manifest)`；`scripts/attachment_lifecycle_v5_runner.py (new)`。
- 只读取证文件：以FACT/source baseline为准。
- 实现边界：独立Reviewer复核每AC、SHA、exit、执行数与证据hash；每候选全量EUnit一次；不得以测试总数替代矩阵。
- SQL影响：不触生产；全量也必须指向scratch库；schema-phase=integrated。
- 测试命令：`make eunit-local`；`bash scripts/test/attachment_lifecycle_v5_gate.sh --task W6-03`。
- 断言：W0..W6-02与G0..G5通过，当前W6-03全部叶通过再提交并标PASS/G6；完整候选全量与真实证据齐；零required skip；UAT/生产UNVERIFIED。
- 必验Case：`W6-03/final-regression`、`W6-03/ledger-verify`。
- 失败恢复/回滚：R / B；迁移见03；外部WIP冲突先阻断。
- Commit：`feat(attachments): w6-03 lifecycle v5`；仅未来明确授权后，主集成者精确路径/块暂存和复核。
- 执行责任：main-integrator；独立只读Reviewer；主集成者唯一提交人。
- 验收证据：E + JSON每个required leaf；当前NOT_STARTED。

## 七Gate

| Gate | 准入证据 | 未满足行为 |
|---|---|---|
| G0 | W0全部；原Up/catalog；活holder挂起2秒有限拒绝；W0-03另库strict台账、追验01/02、runner变异及崩溃接管自测 | 不进W1 |
| G1 | W1静态接口与完整新SQL分库验收，M2全部函数可调用且M3不补缺函数、快照权限/DML/升级/Down/负载边界 | 不进W2、不启GC |
| G2 | W2登记失败关闭、恢复API、Garage受控裸PUT/pin、真实hash确认及旧能力异字节拒绝 | 不进W3；不冒称入口/字节总体验收 |
| G3 | W3引用与Hold协议双连接、48 Hold+24 Retention映射、业务归属/匿名化 | 不进W4；真实删除入口仍待G4 |
| G4 | W4实际12入口整矩阵；两queue故障、真实Garage/Disk字节不可变及expiry/OS锁、writer兼容后M3、调和 | 不进W5；Garage drain不可证明的pending保持blocked并明示 |
| G5 | W5持久任务、逐段checkpoint、状态回查、账号/Workspace隔离与迟到回调 | 不进W6；不能用仅内存重试通过 |
| G6 | W6真实存储/真机/浏览器、同冻结候选全量、完整证据与独立验收、提交向量 | 仅LOCAL_CANDIDATE_PASS；UAT/发布/生产另授权与验收 |

## 引导、阶段顺序及输入冻结

1. 先核验授权和资源白名单。W0-01手动主执行单writer检查三仓HEAD/status、owned/foreign hash与manifest；运行迁移命名门。W0-01/02按JSON格式写仓外checkpoint，尚无runner并不免候选绑定。
2. W0-02显式创建run_id专用本地scratch库，记录创建回执；每条psql/docker命令显式指定该库/地址，拒绝环境默认。按历史原Schema日志扩展清单初始化，排序原Up后逐文件 `psql -X -v ON_ERROR_STOP=1 --single-transaction -f "$migration"`；catalog/probes必须非零执行并ROLLBACK，三OBSERVED及remaining_synthetic_rows=0仅证明基线缺口。此处不证明strict台账，不查询base尚无的gc_verified。unknown只做入口/关系/保护决策静态矩阵；运行时字段和字节断言在W3/W6。
3. W0-03先实现runner，再按安装的erlang_migrate真实API，用 `erlang_migrate:up(#{conn=>RunOwnedConn,dir=>FrozenOriginalMigrationDir,strict=>true,strict_bootstrap=>fail})`向**另一个base库**重放；检查dirty=false及history版本集合精确相等，文件hash由runner在执行前后核对（现有ledger不存checksum）；禁止force/backfill。禁止用隐式连接的imboy_migrate:migrate。API不符、库缺扩展则阻断而非退回raw SQL冒充strict；追验W0-01/02完整receipt后才能G0。
4. W1-01只base/static并更新ADR/产生任务输出，不需要未来M3；W1-02先expand-queue，再独立库guards-contract，保留各库catalog及精确SQL摘要。M1/M2进priv；M2已定义全部协议及守卫函数，输出signature/definition hash/owner/ACL。M3仅挂最终守卫/收紧旧权限，先只docs/guards-contract.sql。W2/W3/W4适配writer和测试，W4-04确认全部兼容后M3正式进priv并重建integrated。启动应用前证明旧worker/ecron未注册；不以跑测试为由自动启动旧drain。
5. 冻结manifest仅列输入文件。W0摘要、ADR、guards SQL、未来证据是task output，在checkpoint记录hash，不增写已授权manifest。修改安全规则/任务范围/必验集合必须新版本、重新独立审查及可信授权绑定；不让执行器自批准。初始HEAD在首次准入核对，后继任务以已接受commit向量推进，禁止机械要求回到原HEAD。
6. SQLite当前manifest v32，W5重核后占用下一版本，运行既有 `dart run tool/generate_sqlite_migrations.dart --check` 和manifest契约测试；CLI若不匹配，先查源码修订命令，不跳过。不改ios/macOS保留区或新建旁路数据库。

## 执行器恢复与提交

OS advisory lock保护单writer，CAS校验run_id/revision/owner_token；lease120秒、heartbeat30秒。先append完整next-state事件并fsync，再同目录temp/fsync/rename/fsync parent写state。事件含seq/前hash/状态digest与完整payload；event领先snapshot可重放，snapshot领先/内部损坏则CONTRACT_CONFLICT。只允许持锁保存损坏尾段后截去不完整尾event，不隐瞒中间损坏。启动外部动作前必须有durable launch intent和attempt。

接管先以LOCK_NB尝试OS锁，总等待≤2秒；失败exit75并仅stdout/独占observation记录BLOCKED_EXTERNAL，不写state/events、不轮询、不计未启动attempt。成功后核对boot/PID/start-time/进程组。旧runner仍持锁，即使lease过期也不能双执行。旧子组TERM等≤10秒、KILL确认≤5秒；无法证实均已停止或含foreign进程则BLOCKED_EXTERNAL，不杀不明进程。记录每个进程的命令摘要与资源归属；不只检查父PID。VERIFYING、COMMITTING均允许BLOCKED_EXTERNAL；后者保存resume_state=COMMITTING。

候选变化不把历史PASS搬成当前PASS：计划内后继task修改共享文件时，由当前task记录affected_prior_case_ids并补齐附加回归叶，消耗当前task预算，不重新启动早期实施时钟。实际返修原task则保留该逻辑任务累计attempt/修复数/deadline，超预算NO_GO。最终W6按冻结候选重跑规定源oracle；仅提交前后全owned tree等价可重绑定。附加回归与基础required集合分开核验，不能重复叶充数，详见JSON candidate_progression_contract。NOT_STARTED/READY允许明确阻断终态。

COMMIT_UNKNOWN先逐仓查确切parent/tree/owned路径：匹配原commit则补记，不重复提交；确证未提交才重验再提交；冲突CONTRACT_CONFLICT。部分向量保持COMMITTING（缺外部条件可阻断），不撤销已提交仓。W6-03验证时要求此前任务/G0～G5通过与当前VERIFYING叶完整，不要求自己的未来PASS；随后完整提交向量→任务PASS→G6，消除自依赖。

每个case命令上限1800秒；task默认28800秒、W1-02为57600秒、W6-01为172800秒、W6-02为43200秒；wave默认86400秒、W6为345600秒；run604800秒；--task可拆为多条case/scenario命令，不能把全部集成叶塞一条无界命令。准入先计算资源槽位、setup/测试/退避/终止预算的关键路径；真实lease测试预留300秒，不能加速120秒租约。不能满足现有限额则CONTRACT_CONFLICT先修订预算，不能跳叶、后台继续或重启重计时。G4后W5两任务可并行；独立fixture测试最多4槽，必须资源互斥且预算可行，单writer与提交串行。

## 测试职责与最终证据

W3-02按12入口fixture映射测试资格协议；W4-01/hold-entrypoints调用每个实际生产入口重跑四级Hold×12=48及两类Retention×12=24；不能仅重跑hold-gc竞态替代入口矩阵。不能映射的业务明确unknown/blocked且DELETE=0，不伪造Hold成功或N/A。

W4-01/W4-02前置只用Garage或driver无关DB；既有66个Disk协议叶仍在W4-03/storage-disk完成后由disk-predecessor-protocols接管，G4/W6必验。新增生产Hold/intent权限等前置DB场景由W6双driver扩展，不能被66旧数遮蔽。direct DML用实际受限角色和独立guards-contract库，所有receipt带schema输入hash。

长退避仅加速run-owned fixture：先测实际DB next_retry_at相对失败时间的区间/抖动，再停worker，以测试proof角色推进该合成job时间并留前后receipt；不清attempt、不改I、不伪造done/解除治理。G4及W6各保留至少一次真实120秒lease和30秒reaper观察。第8次claim崩溃需在DB健康与持续调度条件下≤30*ceil(expired_rows/100)+5秒blocked，不能第9次claim。

W5-01使用已有按用户加密SQLite；认证后init回读，loginAfter/quitLogin切断旧账号任务。W5-02使用按部署/主体/Workspace/访客session隔离的最小journal，attachmentUpload.ts必须暴露presign/PUT/confirm阶段checkpoint，widget/seat API adapter支持status/renew；签名能力仅内存。重启缺本地字节要求重选匹配声明文件；检查点落盘失败先停。W6-02在真实设备与tests/e2e配置中的浏览器重跑每个恢复场景，包括响应丢失、PUT前重启、能力续签、换号/换Workspace迟到回调、撤权和持久化失败；资源不可获得则BLOCKED_EXTERNAL，不能降为mock。

W6-03由主集成者在冻结候选执行：后端隔离配置make eunit-local；App flutter analyze、flutter test；Admin bun run test、bun run build、bun run lint。生命周期/企业/客服相关默认排除suite必须专项执行并列入required，无关排除说明理由。全量每候选一次是默认，修复/候选变化导致证据失效必须必要重验。纯全量与台账检查按候选各场景一次，真实driver覆盖由W6-01/02负责。独立Reviewer核对逐叶provenance，不只看绿灯数；本修订不代签独立验收。


V5.5总69 cases/2272唯一叶；W6-01真实集成case 1271叶；34验证器变异。注册身份场景按真实domain映射，widget/seat专属负例不虚设到普通domain。上述数量仅集合一致性，不是已运行或加分。
W5必须保留同key同密文恢复，Admin重选不同字节即使filename/MIME相同也拒绝；全局验收仍包含当前兼容调用方。新增网关不会改变W4-03才验Disk的顺序。Gate所需预算不得因叶数扩大而默认可行，W0仍须按实际槽位计算关键路径，无法在原预算终止就CONTRACT_CONFLICT。

当前TSID初始化依赖OTP global_name_server，W0隔离VM保留该服务，不能加-kernel start_distribution false；仍显式隔离数据库、资源及业务worker。外部最新App登录dispose/真机fixture变更由W5/W6在当前候选验证，不能继承外部测试自述。

## V5.5并行与验收补充

W5-01、W5-02均依赖W4-04/G4，可同时RUNNING；W6-01依赖两者/G5。写入前主集成者验证两files集合不相交。workers不得写中央state/events或Git index，不提交；共享编译/fixture独占。两个任务都停止修改后，在合并候选重新跑两任务required测试，主集成者按01→02提交；任一失败不放行G5。详细规则及入口命令见06，新增runner四项并行自测和mutation29～34不能省略。

G1补验recovery-tombstone-integrity；G2四真实入口及W2企业adapter固定expiry；G4补验purged-session-recovery和迁移172级联；G5补验合并候选与部分提交恢复。W4-02实际Purge以锁定metadata原字段复制恢复墓碑，不接受客户端提供快照；done后重复UUID不产生新资产/能力。
