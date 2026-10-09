# W0—W6 原子任务 / Task cards

以下为未来实施任务，当前均NOT_STARTED。本轮取证只作输入，不自动完成W0。

公共执行环境：后端命令cwd=imboy；flutter命令cwd=imboyapp；bun命令cwd=imboyadmin。所有DB相关测试由gate显式注入scratch PG变量，并拒绝非loopback、非run-owned数据库；禁止采用测试脚本默认业务库。新gate/runner由W0-03创建，在该任务通过前下述新命令是规划接口，不可假装现在可执行。
W0-01和W0-02是引导任务，仅执行各自现有命令及本文引导步骤，由主执行按同一JSON格式持久化检查点；不依赖尚未创建的gate。W0-03创建runner后必须回验两个引导任务证据，失败使G0失败。其余每个task先运行列出的既有针对性测试（若有），再执行 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task TASK_ID`；该task展开下面case集合。单项case不存在、0断言或skip必须失败。全量仅W6-03，不能阶段性反复全量。

公共恢复R：保留原始失败输出/状态，明确infrastructure或code failure；基础设施同指令最多重试2次，代码修复最多2轮，累计任务运行最多5次，之后NO_GO；外部条件缺失BLOCKED_EXTERNAL无轮询。恢复重核HEAD/hash/lease。
公共回滚B：停止本task worker与物理GC；只撤销本task拥有变更或创建反向本地提交，不reset/stash/clean、不碰foreign staged；DDL采用03的forward-fix/Down阻断；真实DELETE不能撤销，只能已验证备份恢复。
每task独立本地提交（跨仓则每仓一个并作为commit向量，串行完成）；Git身份命令作用域leeyi <leeyisoft@qq.com>，禁止改global配置、push、部署。证据目录忽略规则已知，提交时明确包含本task拥有的脱敏证据文件。

## W0-01 — 冻结三仓与工具链

- TASK_ID：`W0-01`；依赖：无；代码证据：F18（01文档含路径/函数/行号）。
- 实际/新增文件：`docs/architecture/attachment-lifecycle-v5/`；`scripts/check_migrations.sh`。
- 实现边界：复核HEAD、dirty文件哈希、迁移/依赖/运行时版本；后续编辑必须独占task路径。
- SQL影响：只读catalog和scratch库。
- 测试命令：`bash scripts/check_migrations.sh`；引导时不调用新gate；W0-03回验 `--task W0-01`。
- 断言：三仓与manifest匹配；foreign diff未变；迁移数非零且最新版本一致。Case集合：`baseline`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`docs(attachments): w0-01 freeze lifecycle v5 evidence`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W0-02 — 重建Schema与引用覆盖矩阵

- TASK_ID：`W0-02`；依赖：W0-01；代码证据：F02, F13, F14（01文档含路径/函数/行号）。
- 实际/新增文件：`priv/migrations/`；`src/repo/attachment_repo.erl`；`src/repo/user_collect_repo.erl`；`src/features/enterprise_business/infrastructure/eb_pg_store_sql.erl`。
- 实现边界：仅scratch重放原Up；历史只读聚合须有本地授权副本，否则历史回填分支BLOCKED，unknown默认保留不阻断安全改造。
- SQL影响：catalog、索引、trigger、FK与历史合成fixture。
- 测试命令：`docker exec -i "$V5_PG_CONTAINER" psql -X -U "$V5_PG_USER" -d "$V5_SCRATCH_DB" -v ON_ERROR_STOP=1 < docs/architecture/attachment-lifecycle-v5/evidence/catalog-query.sql`；引导时不调用新gate；W0-03回验 `--task W0-02`。
- 断言：164基线Up按当前重核；无法证明的历史GC=0；关系矩阵覆盖所有引用写入/解除入口。Case集合：`schema`, `history-unknown`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`docs(attachments): w0-02 freeze lifecycle v5 evidence`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W0-03 — 建立最小持久执行与证据验证器

- TASK_ID：`W0-03`；依赖：W0-02；代码证据：F01, F10（01文档含路径/函数/行号）。
- 实际/新增文件：`scripts/attachment_lifecycle_v5_runner.py (new)`；`scripts/test/attachment_lifecycle_v5_gate.sh (new)`。
- 实现边界：GLM主执行驱动本合同，不造通用Agent平台；Flash只读取证/验证；任务日志持久CAS、限次重试、审批阻断。
- SQL影响：不连接业务DB；runner状态在工作区.Codex/runs中。
- 测试命令：`python3 scripts/attachment_lifecycle_v5_runner.py self-test`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W0-03`。
- 断言：杀runner续跑不重做已提交任务；错hash/空测试/缺AC/伪exit/循环重试负例全部拒绝。Case集合：`runner-selftest`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`docs(attachments): w0-03 freeze lifecycle v5 evidence`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W1-01 — 冻结状态机、锁序与存储接口

- TASK_ID：`W1-01`；依赖：W0-03；代码证据：F03, F08, F18（01文档含路径/函数/行号）。
- 实际/新增文件：`docs/adr/0008-pluggable-attachment-storage.md`；`docs/architecture/attachment-lifecycle-v5/`。
- 实现边界：将Proposed存储方向与V5安全边界对齐；不能继续引用缺失旧计划；不实施存储搬迁。
- SQL影响：无DDL；冻结锁序/决策矩阵。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --case contract`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W1-01`。
- 断言：八AC无空项；每个删除/引用入口映射；key终身不复用；Hold线性化明确。Case集合：`contract`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`docs(attachments): w1-01 freeze lifecycle v5 evidence`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W1-02 — 实现并验证Expand/Queue/Guards迁移

- TASK_ID：`W1-02`；依赖：W1-01；代码证据：F02, F06, F10（01文档含路径/函数/行号）。
- 实际/新增文件：`priv/migrations/<next>_attachment_lifecycle_v5_expand.{up,down}.sql (new)`；`priv/migrations/<next>_attachment_lifecycle_v5_queue.{up,down}.sql (new)`；`docs/architecture/attachment-lifecycle-v5/guards-contract.sql (new; W1仅设计/独立scratch验证，W4-04才落迁移)`；`test/repo/attachment_lifecycle_v5_pg_tests.erl (new)`。
- 实现边界：按03 SQL合同完成，不改既有迁移；迁移号动态冻结；未知行保护。
- SQL影响：M1/M2；M3仅独立scratch验证与docs合同；W4-04再落可自动加载迁移；旧worker必须停。
- 测试命令：`bash scripts/check_migrations.sh`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W1-02`。
- 断言：直接DML篡改位置/归属失败；合法首绑成功；NULL绕CHECK失败；旧数据无误标；非空Down阻断。Case集合：`migration`, `immutable`, `down-block`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w1-02 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W2-01 — Pending失败关闭与冻结身份

- TASK_ID：`W2-01`；依赖：W1-02；代码证据：F01, F02, F03（01文档含路径/函数/行号）。
- 实际/新增文件：`src/logic/attach_logic.erl`；`src/repo/attach_pending_repo.erl`；`src/logic/enterprise_asset_logic.erl`；`src/repo/enterprise_asset_repo.erl`；`src/ds/group_file_ds.erl`；`src/logic/attachment_upload_logic.erl`。
- 实现边界：所有签发入口落库commit后签名；multipart同归属/位置守卫；旧API字段保留。
- SQL影响：新session字段；不再冲突覆盖owner/location。
- 测试命令：`make eunit t=attachment_upload_logic_tests`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W2-01`。
- 断言：登记失败无URL；DB异常0签名；scope mismatch拒绝；事务提交未知先查询恢复。Case集合：`pending-failclosed`, `pending-multipart`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w2-01 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W2-02 — Confirm幂等与兼容回读

- TASK_ID：`W2-02`；依赖：W2-01；代码证据：F04, F05（01文档含路径/函数/行号）。
- 实际/新增文件：`src/logic/attach_logic.erl`；`src/ds/attachment_ds.erl`；`src/repo/attachment_repo.erl`；`src/logic/enterprise_asset_logic.erl`；`src/repo/enterprise_asset_repo.erl`。
- 实现边界：metadata insert-if-absent与真实引用新增拆开；同pending session原子confirm；首审计一次；保留md5输入回退。
- SQL影响：session confirmed+asset同事务；EAR同事务注册。
- 测试命令：`make eunit t=attach_logic_tests`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W2-02`。
- 断言：100次同请求row=1、引用不增；conflicting scope/cipher拒绝；响应丢失回读同ID。Case集合：`confirm-idempotent`, `confirm-old-client`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w2-02 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W2-03 — Confirm/GC状态协议屏障

- TASK_ID：`W2-03`；依赖：W2-02；代码证据：F05, F09（01文档含路径/函数/行号）。
- 实际/新增文件：`src/repo/attach_pending_repo.erl`；`src/ds/attachment_ds.erl`；`src/features/enterprise_business/infrastructure/eb_pg_asset_meta.erl`；`test/repo/attachment_lifecycle_v5_pg_tests.erl (new)`。
- 实现边界：真实双连接同步屏障，不用sleep猜并发；先使用存储调用记录器证明无未提交删除，W6再接真实字节。
- SQL影响：对象锁/session行锁/CAS。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --case confirm-gc-race`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W2-03`。
- 断言：生产Repo状态协议两个提交顺序唯一胜出；被claim后confirm稳定拒绝；真实GC入口和字节保护留W4/W6，不在本任务声称完成。Case集合：`confirm-gc-race`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w2-03 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W3-01 — 业务引用与注销保护

- TASK_ID：`W3-01`；依赖：W2-03；代码证据：F06, F12, F13, F14（01文档含路径/函数/行号）。
- 实际/新增文件：`src/ds/group_file_ds.erl`；`src/ds/moment_ds.erl`；`src/logic/user_collect_logic.erl`；`src/logic/channel_logic_message.erl`；`src/lib/user_deletion_executor.erl`；`src/repo/moya_submission_repo.erl`；`src/repo/attachment_repo.erl`。
- 实现边界：已有关系首绑/释放加入同锁；普通scope未知或收藏/密文映射不全则保留；不新建引用总账。
- SQL影响：关系原子写；gc_verified只有覆盖证明后true。
- 测试命令：`make eunit t=group_file_atomic_pg_tests`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W3-01`。
- 断言：收藏后原帖删除/上传者注销仍可读；历史/密文未知0删除；新增引用对墓碑失败。Case集合：`reference-gc-race`, `shared-owner`, `history-unknown`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w3-01 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W3-02 — Retention/Hold统一资格

- TASK_ID：`W3-02`；依赖：W3-01；代码证据：F07, F11（01文档含路径/函数/行号）。
- 实际/新增文件：`src/logic/enterprise_asset_retention_logic.erl`；`src/repo/enterprise_attachment_retention_repo.erl`；`src/features/enterprise_business/infrastructure/eb_pg_purge.erl`；`src/features/enterprise_business/infrastructure/eb_pg_store.erl`。
- 实现边界：保留两域规则；同锁判据、阻止删除意图后新Hold假成功；策略错误failclosed。
- SQL影响：M3触发器接线；EAR purge为逻辑提交。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --case hold-all-entrypoints`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W3-02`。
- 断言：所有入口未来Retention/各scope Hold阻止意图；job先提交后Hold明确冲突。Case集合：`hold-all-entrypoints`, `hold-gc-race`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w3-02 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W3-03 — 企业/客服原子绑定回归

- TASK_ID：`W3-03`；依赖：W3-02；代码证据：F08, F15（01文档含路径/函数/行号）。
- 实际/新增文件：`src/features/enterprise_business/infrastructure/eb_pg_store.erl`；`src/features/enterprise_business/infrastructure/eb_pg_store_sql.erl`；`src/features/customer_service/application/session/cs_session_app.erl`；`src/features/customer_service/application/widget/cs_widget_session_app.erl`。
- 实现边界：复用canonical append及asset_ids、租户两键；修复只限锁顺序/资格，不碰Seat WIP。
- SQL影响：绑定资产+消息+审计同事务。
- 测试命令：`make eunit t=eb_message_assets_projection_tests`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W3-03`。
- 断言：重复client_msg_id无重复引用；多asset部分失败全回滚；跨租户拒绝。Case集合：`enterprise-bind`, `cs-bind`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w3-03 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W4-01 — 收口全部物理删除入口

- TASK_ID：`W4-01`；依赖：W3-03；代码证据：F06, F07, F11, F12（01文档含路径/函数/行号）。
- 实际/新增文件：`src/ds/attachment_ds.erl`；`src/logic/attach_cleanup_logic.erl`；`src/logic/attach_logic.erl`；`src/logic/enterprise_asset_logic.erl`；`src/logic/enterprise_asset_retention_logic.erl`；`src/logic/moya_attach_logic.erl`；`src/logic/user_deletion_logic.erl`；`src/lib/user_deletion_executor.erl`；`src/features/enterprise_business/infrastructure/eb_asset_store.erl`。
- 实现边界：普通GC/拒绝上传/教学/注销/企业显式删除仅形成同一队列意图；不在DB事务中网络DELETE。
- SQL影响：资格+metadata/job/audit同事务；失败rollback。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --case intent-rollback`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-01`。
- 断言：扫描所有直接调用只允许driver/worker；audit/commit失败存储DELETE=0；未知返回不作ok。Case集合：`delete-entrypoints`, `intent-rollback`, `confirm-gc-race`, `reference-gc-race`, `hold-gc-race`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w4-01 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W4-02 — 可靠Worker与旧任务接管

- TASK_ID：`W4-02`；依赖：W4-01；代码证据：F08, F09, F10（01文档含路径/函数/行号）。
- 实际/新增文件：`src/features/enterprise_business/infrastructure/eb_pg_asset_delete_queue.erl`；`src/features/enterprise_business/infrastructure/eb_pg_asset_meta.erl`；`src/features/enterprise_business/infrastructure/eb_pg_purge.erl`；`src/features/enterprise_business/application/retention/eb_retention_purge_worker.erl`。
- 实现边界：扩展已有queue领取/lease/reaper/backoff；pending_object_delete事务接管；保留done防复用。
- SQL影响：M2字段；SKIP LOCKED；旧flag→queue CAS。
- 测试命令：`make eunit t=eb_retention_purge_worker_tests`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-02`。
- 断言：两个worker无丢任务；旧token0更新；8次终止；毒任务不饿死后续；无对象身份复用。Case集合：`worker-crash`, `worker-lease`, `worker-db-fault`, `queue-adoption`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w4-02 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W4-03 — Storage Boundary与Garage/Disk闭环

- TASK_ID：`W4-03`；依赖：W4-02；代码证据：F03, F10, F18（01文档含路径/函数/行号）。
- 实际/新增文件：`src/lib/elib_oss.erl`；`src/lib/attachment_storage/attachment_storage.erl (new)`；`src/lib/attachment_storage/attachment_storage_driver.erl (new)`；`src/lib/attachment_storage/attachment_storage_garage.erl (new)`；`src/lib/attachment_storage/attachment_storage_disk.erl (new)`；`src/api/attachment_storage_gateway_handler.erl (new)`；`src/imboy_router.erl`；`config/sys.config`；`src/features/enterprise_business/infrastructure/eb_asset_object_garage.erl`。
- 实现边界：按完整I dispatch；复制现有Garage行为到driver后逐调用替换，政策函数留业务侧；Disk流式网关/原子写/路径与签名校验；无COS/OSS占位；不配置外部域名。
- SQL影响：位置只影响新上传；无桶搬迁。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --case storage-switch`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-03`。
- 断言：真实两驱动PUT/HEAD/GET/Range/DELETE；改默认driver不改旧对象；符号链接/..拒绝；Garage迟到PUT未证则pending回收blocked。Case集合：`storage-garage`, `storage-disk`, `storage-switch`, `late-put`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w4-03 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W4-04 — 调和与可观测性

- TASK_ID：`W4-04`；依赖：W4-03；代码证据：F05, F10（01文档含路径/函数/行号）。
- 实际/新增文件：`src/logic/attach_cleanup_logic.erl`；`src/features/enterprise_business/infrastructure/eb_pg_asset_delete_queue.erl`；`src/adm/adm_attach_handler.erl`；`priv/migrations/<next>_attachment_lifecycle_v5_guards.{up,down}.sql (new)`。
- 实现边界：只调和已登记精确对象；失败分类/age/attempt/blocked计数；日志不含签名URL或PII；将已验证M3合同落入新迁移并做所有写入口回归。
- SQL影响：bounded keyset扫描；不做全桶删除；新增M3正式guards迁移并在integrated scratch启用。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --case reconcile`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W4-04`。
- 断言：DELETE后落账丢失能重试not_found；未知对象永不被删；队列积压可解释。Case集合：`reconcile`, `no-infinite-loop`, `guards-activation`, `all-writer-compat`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w4-04 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W5-01 — Flutter恢复同一上传与消息

- TASK_ID：`W5-01`；依赖：W4-04；代码证据：F16（01文档含路径/函数/行号）。
- 实际/新增文件：`imboyapp:lib/store/api/attachment_api.dart`；`imboyapp:lib/page/chat/chat/attachment_handler.dart`；`imboyapp:lib/service/e2ee/attachment_descriptor.dart`；`imboyapp:lib/service/migrations/upgrade_edges_v33.dart (new; 版本实施时重核)`；`imboyapp:lib/service/migrations/manifest_all.dart`；`imboyapp:lib/service/migrations/downgrade_edges_all.dart`；`imboyapp:assets/migrations/upgrade.sql (generated)`；`imboyapp:test/unit_test/store/attachment_upload_presign_test.dart`。
- 实现边界：扩展既有上传编排，保存key/cipher descriptor/阶段/业务幂等键；不持久化URL；敏感密钥只用既有安全存储；新key必须重新seal，不能复用绑定旧key密文。
- SQL影响：SQLite additive按实际manifest登记；升级不可删除未确认记录。
- 测试命令：`flutter test test/unit_test/store/attachment_upload_presign_test.dart`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W5-01`。
- 断言：杀进程后PUT成功confirm未知可回读重试，无新key/重复发送；logout隔离用户；旧密文可读。Case集合：`flutter-resume`, `flutter-old-client`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w5-01 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W5-02 — Admin治理与状态同步

- TASK_ID：`W5-02`；依赖：W5-01；代码证据：F17, F07（01文档含路径/函数/行号）。
- 实际/新增文件：`imboyadmin:src/pages/storage/StorageOverviewPage.tsx`；`imboyadmin:src/services/api/storage.ts`；`imboyadmin:src/widget/customer_service/app/attachmentMachine.ts`；`src/adm/adm_attach_handler.erl`；`imboyapp:lib/modules/enterprise_app/presentation/asset_retention_panel.dart`。
- 实现边界：现有界面追加逻辑已删/物理待删/阻断原因，不建新控制台；保留EntityId与分页；与Seat WIP冲突则停该任务。
- SQL影响：查询状态接口；无客户端绕过hold的force按钮。
- 测试命令：`bun run test src/pages/storage/StorageOverviewPage.test.tsx src/widget/customer_service/app/attachmentMachine.test.ts`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W5-02`。
- 断言：purged不误报字节已删；超时显示未知可恢复；跨租户看不到job；linked才成功。Case集合：`admin-governance`, `status-sync`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w5-02 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W6-01 — 真实PG/存储故障矩阵

- TASK_ID：`W6-01`；依赖：W5-02；代码证据：F01, F05, F07, F10（01文档含路径/函数/行号）。
- 实际/新增文件：`test/repo/attachment_lifecycle_v5_pg_tests.erl (new)`；`scripts/test/attachment_lifecycle_v5_gate.sh (new)`。
- 实现边界：一套合成固定种子数据；真实PG两连接/真实Garage专用bucket/专用Disk目录；注入仅影响本run子进程。
- SQL影响：scratch升级/故障恢复。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --case real-pg-garage-disk`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W6-01`。
- 断言：AC01-07每项实际执行；HTTP和SQL和字节hash三方对应；无skip无stub。Case集合：`real-pg-garage-disk`, `all-ac01-ac07`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w6-01 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W6-02 — 三端E2E与历史兼容

- TASK_ID：`W6-02`；依赖：W6-01；代码证据：F13, F14, F15, F16, F17（01文档含路径/函数/行号）。
- 实际/新增文件：`imboyapp:integration_test/attachment_lifecycle_v5_test.dart (new)`；`imboyadmin:e2e/attachment-lifecycle-v5.spec.ts (new)`；`scripts/test/attachment_lifecycle_v5_gate.sh (new)`。
- 实现边界：真机Android/macOS已有可用设备，不改ios/macos保留区；Admin真实浏览器；真实后端/对象存储，禁止route.fulfill替换核心链路。
- SQL影响：仅合成数据。
- 测试命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --case e2e-compat`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W6-02`。
- 断言：旧请求fixture和历史合成附件、E2EE、多业务、断网/重启全链路；UAT仍由用户签署。Case集合：`e2e-compat`, `ac08`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w6-02 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## W6-03 — 冻结候选全量回归及独立验收

- TASK_ID：`W6-03`；依赖：W6-02；代码证据：F01, F18（01文档含路径/函数/行号）。
- 实际/新增文件：`docs/architecture/attachment-lifecycle-v5/`；`scripts/attachment_lifecycle_v5_runner.py (new)`。
- 实现边界：独立Reviewer复核每AC、SHA、exit、执行数与证据hash；每候选全量EUnit一次；不得以测试总数替代矩阵。
- SQL影响：不触生产；全量也必须指向scratch库。
- 测试命令：`make eunit-local`；统一门 `bash scripts/test/attachment_lifecycle_v5_gate.sh --task W6-03`。
- 断言：所有任务Gate通过；全量EUnit+Flutter+Admin+真实环境证据齐；零未批准skip；报告UAT/生产UNVERIFIED。Case集合：`final-regression`, `ledger-verify`。
- 失败恢复：R；回滚：B，涉及数据使用03中明确阻断/恢复条件。
- Commit：`feat(attachments): w6-03 lifecycle v5`；只提交本任务拥有文件；跨仓证据需所有commit齐。
- 验收证据：candidate三仓SHA + owned diff SHA + command/cwd/env + exit + executed/pass/fail/skip + 每case oracle + raw SHA256 + commit vector。真存储case额外保存删除前后精确I、合成字节hash、DB事务/租约事件。

## Wave Gate（不得合并或跳过）

| Gate | 必须满足 | 失败后停止边界 |
|---|---|---|
| G0 | W0全部通过，runner负例自测，真实schema、覆盖矩阵、foreign保护 | 不进入W1 |
| G1 | W1全部通过，完整新SQL scratch验收，锁序审查、Down阻断 | 不进入W2，不启GC |
| G2 | W2全部通过，AC01/03、AC02元数据侧和AC04状态协议真实PG；AC04整体仍PENDING | 不进入W3 |
| G3 | W3全部通过，引用/hold状态协议双连接屏障与所有权矩阵；真实删除入口待G4 | 不进入W4 |
| G4 | W4全部通过，全部删除入口收口、队列故障与真实Garage/Disk、调和 | 不进入W5；未证明Garage在途上界的pending自动清理必须保持blocked并在范围表明示 |
| G5 | W5全部通过，客户端恢复/治理与后端状态一致 | 不进入W6 |
| G6 | W6全部通过，八AC严格覆盖、全量与独立验收、candidate绑定 | 只可LOCAL_CANDIDATE_PASS；UAT/发布/生产另判 |

W4存储提取可能涉及01快照之外的调用者：实施时由`rg -n "elib_oss" src test`生成完整task拥有路径清单，先检查foreign冲突，再纳入W4-03文件白名单；不得扫描后直接改遍仓。Gate要求零业务层provider直调，不以遗漏调用者缩范围。测试文件迁移可保留功能测试但改模块名；不因抽象重构删除安全用例。

W6-03精确全量命令：scratch环境下 `make eunit-local`；App `flutter analyze` 与 `flutter test`；Admin `bun run test`、`bun run build`、`bun run lint`；然后复核W6-01/02在同一候选上的真实环境证据。make默认排除的专项套件需分类：相关生命周期/企业/客服专项必须单跑并列入required集合；无关排除需明确理由，不能宣称全部测试零排除。

## 引导与安全测试环境（W0-01/02无需未来gate）

1. W0-01执行三仓 `git rev-parse HEAD`、`git status --porcelain=v1`、owned/foreign文件SHA256和迁移文件SHA256；执行 `bash scripts/check_migrations.sh` 并保存exit。读取本合同检查TASK_ID/依赖唯一，计划hash不符即停。
2. W0-02仅创建带run_id的本地scratch库；确认create成功且名称记录到run-owned资源manifest。按evidence/schema-rebuild.log的扩展集合初始化，按升序逐个执行 `psql -X -v ON_ERROR_STOP=1 --single-transaction -f "$migration"`；任一失败停止，不加 `|| true`。执行catalog-query.sql与baseline-probes.sql，校验输出中三个OBSERVED标记和remaining_synthetic_rows=0。原始基线证明不是安全PASS。
3. W0-03追溯检查前两项证据，形成G0；此后由runner唯一写状态，不能因为引导没有runner而跳过HEAD/hash。
4. Make测试统一使用生成的run-owned临时配置 `make eunit-local t=<module> EUNIT_CONFIG=<absolute_run_config_without_suffix>`；该配置仅包含合成scratch连接、HTTP_PORT=0与独立测试secret，禁真实推送/通知/定时清理/自动迁移。本文卡片中的 `make eunit t=...` 是suite选择指令，实际必须按本条封装，不能裸跑。本轮裸跑失败的证据保留。W6全量也使用同一隔离配置，不能加载sys.local真实业务库。
5. SQLite真源为migration manifest（当前32），禁止创建旁路SQL手工迁移。W5-01修改manifest edge并运行 `dart run tool/generate_sqlite_migrations.dart --check` 与实际migration manifest契约测试；如生成器CLI与当前源码不符先校对，不静默跳过。
