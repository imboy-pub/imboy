# IMBoy 附件生命周期 V5.5：真实现状与差距 / Source audit

日期：2026-10-14（Asia/Shanghai）。本轮只设计、取证、评审；没有实施业务代码、创建正式迁移或提交。
当前静态基线为 `evidence/current-baseline.json`；带 `20261013` 的数据库日志是此前取证的历史输入，本次修订未重新运行。不能因日期相同把历史 PASS 当作当前运行结果。

## FACT：当前基线

| 仓库 | 分支 | HEAD |
|---|---|---|
| imboy | main | 35f13d7316420eddda9872e29d8843a13f4ffa66 |
| imboyapp | main | bc32be472cb00ecd73cd73cfd065a0dfe717e0b2 |
| imboyadmin | main | 817b6801b22e04885b7aa832630eaa4b3bdf10c1 |

当前 Backend tracked 工作树干净，剩余两个外部未跟踪文档；App 有 ios/Podfile.lock 与 ios/Runner.xcodeproj/project.pbxproj 外部修改，属于禁止改动区；Admin 保留外部未跟踪续跑提示。moment_ds 已随其他会话提交，不再是当前外部WIP；其现有逻辑成为输入基线。逐路径状态与哈希以 current-baseline 顶层为准；历史观察不得当当前状态。
最新后端迁移为00000172，171组Up/Down（41缺号）；本轮只运行迁移命名门，不重放Schema。Flutter SQLite manifest仍为v32，W5重核下一版本。新迁移172是访客阅读元数据，对消息使用CASCADE，W4-02须证明消息Purge及恢复墓碑不受影响。

此前取证曾在 `imboy_pg18` 容器创建专用空库 `imboy_v5_audit_20261013_01`，PG18.1，初始化 pg_jieba/postgis/timescaledb/pgcrypto，170 个原始 Up 按升序逐个单事务执行到 171，未出现 ERROR。catalog 与三个旧合成观察探针 exit=0，ROLLBACK 后合成附件 0。该结果证明原 SQL 重建，不证明 erlang_migrate strict 台账、升级/Down、修复后的并发、真实历史数据或对象存储安全。默认 Homebrew PG18.4 缺扩展且既有 imboy_v1 台账 dirty=true；未修复或写该库。

## FACT → GAP → DECISION

| ID | 当前代码证据（相对 imboy） | 当前结论 | Task |
|---|---|---|---|
| F01 | src/logic/attach_logic.erl:63–105 | pending 登记失败/异常仍返回 URL，P0；改为先提交登记再签发，提交未知不返回能力 | W2-01 |
| F02 | src/repo/attach_pending_repo.erl:add/4；迁移54；baseline-probes | 身份仍可冲突更新，探针证实；冻结 key/owner/scope/location | W1-02,W2-01 |
| F03 | src/logic/attach_logic.erl:124；elib_oss | HEAD 按请求 scope/current bucket，不是冻结位置；需签发时身份回读 | W2-02,W4-03 |
| F04 | src/repo/attachment_repo.erl:121；attachment_ds.erl:56 | Confirm 仍调用冲突计数+1；首条审计幂等已存在，引用幂等未完成；探针得到2 | W2-02 |
| F05 | src/repo/attach_pending_repo.erl:claim_expired_tx/3；attachment_ds.erl:246；迁移168 | 已有 hash(key,168)锁、重验 NOT EXISTS、持久删除意图与 path 墓碑，关闭“完全无锁/无队列”的旧源码描述；真实竞态仍 UNVERIFIED | W2-03,W4-01 |
| F06 | src/ds/attachment_ds.erl:140,163,194；attachment_repo.erl:350,389 | 孤儿先删元数据并入队已修复；worker 在另一个数据库事务内 DELETE 外部对象，位置仍取当前配置，引用检查仍靠计数/两类retention；不能宣布安全完成 | W3-01,W4-01,W4-02 |
| F07 | src/logic/enterprise_asset_retention_logic.erl:257–292 | 单附件治理仍在事务内删对象，后续SQL/COMMIT失败不可恢复；当前源码已移除未知返回视为ok的分支，保留该修复并改为intent | W3-02,W4-01 |
| F08 | src/features/enterprise_business/infrastructure/eb_pg_purge.erl:153,173,374 | Workspace锁、intent/metadata/audit同事务，成功后drain，源码结构已修复；实际提交及字节验收待W6 | W4-02 |
| F09 | src/features/enterprise_business/infrastructure/eb_pg_asset_meta.erl:209 | 已有pending CAS/pending_object_delete，复用并接入既有队列 | W4-02 |
| F10 | eb_pg_asset_delete_queue.erl:24–55；迁移163 | 企业queue普通SELECT、无lease/backoff、成功DELETE行；位置不完整；扩展该队列，不引入第三队列 | W1-02,W4-02 |
| F11 | attach_logic.erl:155,160,171；enterprise_asset_logic；eb_asset_store.erl:137 | 内容拒绝和私有删除直达存储，必须收口全部入口；不能只改定时GC | W4-01 |
| F12 | user_deletion_executor.erl:41；user_deletion_logic.erl:297；moya_attach_logic.erl:170 | 上传者不等于唯一所有者，注销/教学同样重验引用与Hold | W3-01,W4-01 |
| F13 | group_file_ds.erl:319；迁移108/157 | 复用group_file_id锚点/索引，所有写入和解除同锁；列非空不是全量关系证明 | W3-01 |
| F14 | moment_ds bind_moment_scope_ref；user_collect_logic.erl:255；channel payload | JSON/密文/hash不能证明零引用；不可证明的对象禁自动GC | W0-02,W3-01 |
| F15 | 企业canonical append、enterprise_asset.message_id、eb_pg_purge.erl:57–90 | 两套治理主体保留；hold FK/Workspace锁是有效保护结构，不重复判为全无锁；双连接竞态未验证 | W3-03 |
| F16 | imboyapp/lib/store/api/attachment_api.dart:205,257,281；batch_upload_controller.dart:173 | 现有PUT退避和内存重试，不等于进程恢复；复用持久存储/状态回读 | W5-01 |
| F17 | imboyadmin/src/widget/customer_service/app/attachmentMachine.ts:99；seatSend.ts:113；StorageOverviewPage.tsx:450 | 已有幂等键及消息结果未知回查；confirm回读和物理状态仍缺；unit/mock不是真实E2E | W5-02 |
| F18 | 三仓当前SHA；170原Up重放；catalog-extra；src/repo/attachment_gc_repo.erl | 迁移168新增普通队列 attachment_object_deletion 与不可删 workspace_attachment_retention；旧唯一企业队列方案已过时，必须保留两个既有队列并共享安全协议 | W0,W1,W4 |

## CONTRACT：Schema 与引用边界

历史原Schema catalog支持（本次未重跑）：attachment path UNIQUE、status CHECK；attach_pending key PK/created_at索引；enterprise_asset有企业复合FK及retention/purge触发器；企业queue有Workspace RESTRICT FK。迁移168新增普通queue path PK、attempts>=0、未完成重试索引、attachment path防复用触发器（hashtextextended(path,168)）；workspace retention附件FK RESTRICT、治理记录禁止UPDATE/DELETE、scope首绑登记与回填。完整定义见当前 catalog，两项168表额外查询见 catalog-extra。
普通孤儿谓词已同时排除 workspace_attachment_retention 和 enterprise_attachment_retention。永久Workspace保留不能被TTL或账号注销降级；计划不新增解除保留入口。
引用地图至少覆盖 group_file、群/私聊消息历史、channel_message、moment_post.media、user_collect.info/attach_file_hash256、教学绑定、客服canonical append、企业应用EAR。枚举出列/调用不等于证明历史引用完整；W0-02须形成每种“创建/转发/收藏/解绑/注销/归档”的入口矩阵。历史位置及引用无法证明一律gc_verified=false。

## TEST → EVIDENCE

此前取证执行170原Up、catalog、3个观察探针；本次计划修订只静态核对源文件/合同/manifest及迁移命名，不重跑数据库。历史观察不是业务修复PASS。EUnit、Flutter、Admin测试本轮未运行，旧beam和历史日志不继承。真实Garage/Disk、真机、浏览器、故障注入及迁移升级/Down均UNVERIFIED。
当前候选无完整V5 runner/gate，相关命令是未来W0-03交付接口。历史探针证明基线缺口，不冒充修复用例。本次不读取历史真实数据、不建库、不启动服务；未来仅经实施授权的run-owned合成库/对象可用于Gate。
本次没有修改本地memory；以本地源码、原迁移和合同为证据，没有发送项目代码到外部服务。

调用链：attach_handler→attach_logic→attachment_ds→attachment_repo/attach_pending_repo；attach_cleanup_logic→attachment_ds→attachment_gc_repo→elib_oss。企业治理→retention_logic→EAR/attachment→elib_oss；企业purge→metadata+audit+enterprise queue→commit后drain→eb_asset_store。群文件/动态/收藏复用DS；注销/教学须纳入删除入口审计。

交付路径核验：docs-site/content/architecture被站点.gitignore忽略，是站点副本而非Git真源。本修订回到docs/architecture/attachment-lifecycle-v5；只将当前证据纳入交付，旧站点文件保留，不继承历史验收。

2026-10-13二次冻结：后端HEAD变为0a5ade3cd8e8c005a1cc22991633dedd81af0445（其他会话提交既有cache改动）；生命周期关键源码与170原Up逐文件hash仍一致，原Schema取证按相同输入hash复用，不能称作在新HEAD重跑了Schema。计划质量与运行成熟度分开评分。

最终冻结又观察到三端统一交付合并：后端38/App17/Admin3个提交间差异文件，原370项内容hash不变；已记录58个差异文件hash并审阅7处相关diff。新增ACK delivered状态修复与Admin二进制流透传属于当前兼容基线，W5/W6须覆盖；其他合并差异未全面重审及未跑回归，UNVERIFIED，不能继承外部合并测试PASS。

## V5.3 反向审查补证

| FACT | GAP | DECISION / TASK | 证据边界 |
|---|---|---|---|
| F19：enterprise_asset_handler:with_idempotency/6 在事务回调保存响应；attach_handler:presign 错误分支有限 | Logic内部调整不足以保证提交后签发 | W2-01拥有两个handler和幂等辅助模块；事务保存稳定身份，提交分支签名，503不泄露能力 | 当前源码直接证实；修复未实现 |
| F20：eb_asset_app:presign_mint/4 先mint token；Admin attachmentUpload:runSeatAttachmentUpload 仅confirm后返回assetId | 登记、响应丢失及PUT前检查点没有完整持久恢复链 | W1/M1复用既有session字段；W2四入口status/renew；W5辅助函数逐阶段await checkpoint | 当前源码直接证实；新API为设计要求 |
| F21：迁移118 uploaded_by_user_id是审计字段，FK ON DELETE SET NULL | 全部上传人绝对不可变会阻断合法注销 | 业务归属不可变；审计actor允许受约束FK匿名化；W1/W3验证 | 原迁移直接证实；触发器兼容UNVERIFIED |
| F22：Admin已有seatSend.scopeCommit、attachmentRecovery、revocation三组测试；Playwright testDir为tests/e2e | 原seatSend.test.ts及e2e根路径不匹配 | W5/W6路径已纠正，新增账号/Workspace迟到回调必验叶 | 文件存在由静态核对；测试未执行 |

分类规则：当前源码直接证实 / 本次实际运行的静态检查 / 历史证据支持未重验 / 仅设计要求尚未实现 / UNVERIFIED / 与源码或合同冲突，不能相互替代。V5.2的十项冲突及V5.3处理见05；修订作者不代独立Reviewer关闭问题。


## V5.4 本轮静态补证

| FACT | GAP / DECISION | TASK / TEST / EVIDENCE |
|---|---|---|
| F23：attach_pending_repo:add/4及迁移54未保存原filename/MIME；迁移118企业审计actor可为空 | 仅client_upload_id不能恢复原声明及widget session身份；增加不可变upload_registration | W1-02/registration-integrity、W2四入口；源码已读，新SQL/恢复未实现 |
| F24：elib_s3_sign:presign_url/6仅host签名、UNSIGNED-PAYLOAD；elib_oss:put_object/4内部重新生成相对TTL URL | 已发PUT可在Confirm后覆写同key；受控网关固定实际字节、绝对内部到期、真实hash确认 | W2-01/02、W4-03两driver；新协议仅设计，真实Garage能力未验 |
| F25：attachment_api.dart先取key再seal；attachment_encryptor.dart生成加密材料 | 不可要求presign提供明文hash；同key重启重试必须保留同一密文 | W5-01、W6-02真机；旧客户端兼容与资源预算待Gate |
| F26：V5.3 M3首次定义意图函数，W4-01/02却只加载M1/M2 | 前向依赖；函数定义归M2，M3挂守卫/收权限 | W1两个独立phase清单、W4-04正式激活；未执行DDL |
| F27：V5.3运行锁先于接管，但活holder可无限持锁 | LOCK_NB≤2秒；旁路只写非权威独占observation，绝不双writer | W0-03挂起进程负例/G0；runner仍未实施 |

历史V5.4的moment_ds绑定从cc08dfa95d956969b0974f589fdd12dabc8b344fcfdff7555c5e889a6d65e030变为145ee97581f874a58a0b3e73e1ff4359762b7c10804c1ff3726f84989d51f30d。当前HEAD与WIP diff涉及ACL缓存after_commit/错误失败关闭；附件首绑块未见改动。未持有旧hash对应完整源码，不能声称重建了冻结版本到当前的逐行diff。此次只重绑定事实输入，W3-01/G3及W6相关运行证据仍须当前候选重验；当时foreign拥有权冲突保持。该段仅为历史，当前状态见顶部；当时其他会话继续写cache，最终状态以baseline采样时点为准。

V5.4最终采样HEAD：Backend 27bfb1a1deb903f0fe044c3de107ac425c0780e7；App 48bb090e49f807ac17b6ffef39551ccdde3c574c；Admin 2d6b9053082df7670014fc2d27a0cceb46fda857。本轮外部合并MC测试fixture及App登录dispose/导航重连测试，差异文件hash见v54_final_rebaseline。此前TSID初始化已要求global_name_server，Makefile不再禁用它；W0隔离启动不得沿用禁用该服务的旧参数。W5/W6登录/真实设备/全量回归必须按当前候选执行。源码hash匹配只证明静态绑定，不证明这些合并运行正确。

## V5.5 当前差异与任务影响

| FACT | 决策与任务 | 本轮证据边界 |
|---|---|---|
| F28：eb_pg_purge:delete_and_audit先入queue再物理删enterprise_asset；恢复UUID/声明仅设计保存在原行 | 既有enterprise_asset_delete_queue加两可空恢复字段；M2原子复制/唯一性，W2恢复查墓碑，W4实际Purge | 现行调用链及迁移163已读；修复只在合同 |
| F29：eb_asset_store:write_reserved→eb_asset_object_garage:put→elib_oss:put_object/4会重新计时 | 企业adapter固定Context传递前移W2-01，W4统一Driver；四入口expiry测试 | 调用链已读，尚未实施 |
| F30：Admin widget已有isCurrent/credentialEpoch、seat上传已传AbortSignal；App已有auth-transition监听 | W5复用并接持久journal，禁止重复建第二账号epoch；保留新正文校验/阅读API | 三仓相关diff已审；其新功能运行正确性未重验 |
| F31：新增迁移172访客阅读记录，enterprise_message外键CASCADE | W0以当前171 Up重建；W4-02实际消息清理验证cascade且墓碑保留 | SQL已读，历史170 Up日志不覆盖172 |

V5.4复评指出R54-01/02/03，V5.5处理见05；不存在本次9.9独立签核。原源码hash有11项、跨仓hash有12项变化，精确清单在baseline.v55_rebaseline。最后采样时另有仅hird报告的后端提交推进，相关生命周期文件hash未变；不继承任何外部运行PASS。
