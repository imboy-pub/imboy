# IMBoy 附件生命周期 V5 — 主工程合同 / Engineering contract

版本 V5.5，2026-10-14。状态：DELIVERABLE_WITH_ADMISSION_GATES。V5.4复评8.90/NEEDS_REVISION的三项缺口已落实修订，当前评分待独立判断，9.9是目标。业务实施NOT_STARTED；UAT/生产UNVERIFIED。执行入口为06-zcode-handoff.md。

文档顺序：`01-facts-and-gaps.md`（FACT/GAP）→ 本文（DECISION/CONTRACT）→ `03-migration-contract.md` → `04-tasks.md`（TASK/TEST）→ `execution-contract.json` → `05-independent-review.md` → `06-zcode-handoff.md` → `evidence/`。
冻结对象是以上文件和 manifest 的字节哈希，不以“V5”标题代替计划绑定。后续实施必须先机械核验 manifest、三仓HEAD和任务拥有文件；计划变化产生新版本，旧验收不得搬用。

## 1. 决策及最小范围

1. 继续使用 attachment、attach_pending、enterprise_asset、企业两类治理表、既有业务关系。保留迁移168的 attachment_object_deletion 和迁移163的 enterprise_asset_delete_queue，分别增量扩展，不新建第二个GC框架、扫描桶删除器或通用引用总账。
2. metadata.status 是读授权/业务可见性，不能兼任物理删除状态；referer_time保留兼容展示，不作为真实引用计数或唯一GC条件。
3. Confirm只登记资产；创建业务关系才是引用。重试只回读同一资产；不能借 confirm/收藏/转发冲突更新改变原所有权、scope、位置、cipher 或计数。
4. 默认 conservative：历史位置或引用完整性不可证，`gc_verified=false`；仍按旧读ACL提供兼容，禁止自动清理。保护不是声称“历史已经验收”。
5. Storage Boundary是实际缺失依赖：Garage兼容、Disk实际读写删除都须完成。优先小范围迁移现有I/O到统一边界，禁止顺便重写消息系统；COS/OSS不属于V5验收。ADR0008是Proposed，本V5明确替代其“按当前scope全量回填garage”的建议，未来W1同步ADR并生成任务输出摘要；不得改写本冻结输入；当前不改ADR。
6. 不新增上传SDK；旧 presign/confirm/view_url字段保留，允许新增状态字段。普通旧客户端无需发送新的session ID，object_key可作会话lookup键。已有 group anchor要求不得放松。
7. 不承诺所有历史字节都能回收：V5安全验收要求“可证明的对象可回收，无法证明的对象被阻止且可解释”。人工审批也不能代替技术归属证明；生产历史数据另需授权。

## 2. 五类对象与状态

| 对象 | 现有持久主体 | V5状态与转移 | 唯一职责 |
|---|---|---|---|
| Upload Session | attach_pending；企业消息域复用 enterprise_asset.pending_confirm | registered→confirmed；registered→rejected/expired→delete_intent→deleted；终态不回退 | 已签发或可能签发的写权限、上传截止、冻结身份 |
| Asset Metadata | attachment / enterprise_asset | live→delete_intent→deleted；历史默认live但gc_verified=false | 身份/读ACL/元数据；不承诺存在业务引用 |
| Business Reference | 群文件、moment、channel、收藏、消息/客服已有关系 | absent→bound→released；同业务ID重放不重复插入 | 谁仍然需要该对象；删除消息不等于立即物理删除 |
| Retention/Authorization | EAR、EB policy/hold、scope ACL | retention仅延长，hold none↔held；purged不可回退 | 决定是否允许形成删除意图，独立于上传者意愿 |
| Physical Delete Job | attachment_object_deletion / enterprise_asset_delete_queue | ready→leased→done；leased→retry_wait/blocked；过期leased→重新claim | 提交后的外部副作用及可恢复确认 |

registered写库提交后才调用签名并返回URL。签名成功但响应丢失：保留registered，视为“可能已签发”，不得立即补偿删除。签名失败也留下可回收登记，不删除登记后尝试重签。
confirmed session保留轻量结果/对象身份，支持幂等回读；不无限保存签名URL。pending_remove语义改为确认标记，不在过渡期擦除唯一身份。
业务status=0/-1不会直接置delete_intent。purged表示**逻辑删除已提交**，physical_delete_state单独表达尚未/已经删除字节；旧成功码仍对应逻辑操作完成，不冒充物理已完成。

## 3. 对象身份和不可变性

物理身份 `I=(storage_driver, storage_namespace, storage_container, object_key)`；持久storage_locator_sha256绑定namespace的物理解析，别名归canonical后使用同一storage_root_id锁域。
namespace是部署内不可复用的位置ID，指向固定存储实例/磁盘根；不是可变“当前driver”，也不是URL或凭据。更换根目录/bucket/实例必须分配新namespace；仅凭据轮换不改变身份。
保留attachment.path、pending.object_key、EB.object_key，不另造key字段。container在Garage为bucket，Disk为固定逻辑容器（例如default），namespace固定绝对root映射。
身份、组织业务归属/应用、初始scope、cipher、群锚点声明不可变；普通creator仍按身份合同冻结。企业uploaded_by_user_id是可匿名化审计actor，不是业务所有者：迁移118的ON DELETE SET NULL保留，仅旧父用户同事务已删除且其他受保护字段不变时允许非NULL→NULL，父用户仍存在时任意置NULL/换人均拒绝；moment scope_ref允许且仅允许未绑定→同事务首个确定ID，群anchor_conv_seq同样单调首绑，不能把合法首绑误当违规。
数据库触发器和应用检查双层执行。位置未知行只能由专用证明回填流程填一次，审计记录manifest hash；不提供任意UPDATE“迁移位置”的API。
一个key永久不分配给另一会话/对象；已删除的元数据或job终态保留防复用标记。相同字节hash不是同一个对象。
上传能力尚可使用时不完成物理GC：记录upload_expires_at；需存储/网关最大在途上传时间上界。Disk以同I的PUT/DELETE稳定OS锁防迟到重建；Garage网关内部PUT及历史直传未证明上界时该pending标记blocked_upload_drain，仅可列出，不自动DELETE。不得用“过24h大概够了”证明所有在途写已消失。W4-03必须提供真实超时/迟到PUT证据，否则Garage该类pending回收保持关闭。

## 4. 事务锁顺序与线性化点

所有新增引用、Confirm、Hold、Retention、GC资格、账号注销按相同顺序：

1. 既有组织/成员资格锁 → Workspace行（涉及EB Hold和Purge使用FOR UPDATE；不允许拿资产锁再升级Workspace）。个人域跳过无关父锁。
2. 企业带client_upload_id的register/intent/Purge先获取相同登记UUID事务锁（多UUID按字节排序），在父级/Workspace之后且I/任何资产行锁之前；持锁查enterprise_asset与既有queue联合身份。跨Workspace碰到既有UUID直接拒绝，不反向锁原Workspace。然后，对完整且已证明I，获取跨两个domain一致的完整I advisory lock；所有签发、确认、引用、enqueue和proof入口都参与，持锁后查另一queue及metadata以拒绝同物理I共享。然后对象事务级 advisory lock：普通沿用迁移168的 hashtextextended(object_key,168)，企业采用domain+object_key稳定键；在锁内核对完整I，禁止配置漂移和跨domain物理身份共享。不得在同一普通对象上另选身份哈希而与旧触发器分裂锁域；多队列跨domain身份无法证明唯一时禁GC。键用规范化对象键摘要；普通缺位置旧行仍用hashtextextended(path,168)，仅允许保护/读取，不参与GC。多对象按完整identity字节排序。哈希冲突只增加串行，不放松检查。
3. Session行 → Asset行（按ID升序，FOR UPDATE）→ 治理行（EAR等）→ 既有业务关系行（稳定业务键排序）→ Job行。

企业消息canonical append现有FOR UPDATE资产锁须先进入同一父锁/对象锁顺序；新建Hold必须先锁Workspace，再查覆盖范围。EAR初次创建也先锁对应资产，避免“治理行不存在”幻读。普通不存在资产行的首次Confirm必须锁真实pending行，不能靠SELECT空结果锁住不存在的行。
事务隔离默认READ COMMITTED；取锁之后重查所有资格。锁住前的候选查询、HEAD结果、列表快照不构成授权。死锁/序列化失败整事务限次重试，不能只重试其中SQL。

| 动作 | 线性化点及失败语义 |
|---|---|
| Pending登记 | INSERT事务COMMIT；失败或提交结果未知不返回可用URL，查登记再恢复 |
| Confirm | 持锁后合法registered→confirmed、metadata和必要审计同事务COMMIT；真实对象hash/size校验在事务外，但必须持锁后重查会话版本/状态/位置及upload_content；变动则拒绝或重新验证，HEAD本身不充分 |
| 新引用 | 同一对象锁内检查asset live +授权，再与业务关系同事务COMMIT；job存在/delete_intent/deleted禁止新增 |
| 删除意图 | 父锁、对象锁、行锁内重验GC资格，将metadata设delete_intent并写不可变job快照/审计，COMMIT；这是引用和Hold竞争的裁决点 |
| Worker | 短事务CAS领取租约并COMMIT后才DELETE；成功后独立短事务按队列主键+lease_token完成 |

Hold先提交：GC必须拒绝。删除意图先提交：该对象不再允许新增Hold/延长留存/新增引用；返回明确already_deleting，绝不能返回hold成功却继续删。Workspace级后续Hold可覆盖其余live对象，回执列明已逻辑删除对象不在覆盖范围。这是逻辑删除边界，非物理删除前可撤销承诺。
DELETE过程中不持数据库事务；DB断线不影响已提交意图安全，但暂停领取新job。拿到job后数据库故障导致无法再次验证时不发起新的DELETE；若请求已经在途，完成未知→等待数据库恢复后同job重试。

## 5. GC资格和引用合同

统一判据：完整且已证明位置、session写权限已耗尽、metadata处于合法可清理状态、引用检查完整且零有效引用、Retention到期、无Hold、所有权/组织业务归属匹配、无进行中的首绑/Confirm、策略允许、审计能同事务提交。任一查询失败或未知为不具备资格。
普通新确认但未绑定对象保持当前保守宽限（至少7天，默认30天）；未确认默认24h且不少于2h，但这只是候选年龄，不能替代写权限/在途请求耗尽证明。企业域保持既有业务retention，不把普通TTL强套过去。

| 业务 | 复用关系 | V5处理与自动GC资格 |
|---|---|---|
| group_file | attachment.group_file_id + group_file +群资格 | 首绑/解除同事务对象锁；FK新增前盘点孤儿。能证明唯一绑定和禁止跨scope共享的新对象才可gc_verified |
| C2G | anchor_msg_id/anchor_conv_seq + staging/archive | ACK/离线表清理不等于引用释放；消息归档开关不能改变附件保留。密文历史引用不完整→gc_verified=false |
| C2C/E2EE | 现有消息与客户端descriptor | 不解密、不上传明文hash；无可证明引用释放机制时确认后的对象不自动GC |
| moment | scope_ref首绑 + moment_post.media/状态 | bind已存在，补同锁和解除行为；确证独占scope的新对象可证明，历史JSON/URL歧义冻结 |
| channel | channel_message payload + scope_ref频道 | scope_ref只证明频道归属，不证明零消息引用；本V5默认禁自动GC已确认channel对象，除非W3显式完成完整关系证明并列出所有写入口 |
| 收藏/转发 | user_collect.info/attach_file_hash256等 | hash不可作删除主键。无法解析密文/证明对象映射的来源资产持续保护；普通新资产一旦可能进入该链，gc_verified=false，不能仅凭group_file/moment已删除清理 |
| 教学 | moya_submission_repo既有绑定 | 未绑定扫描也需同对象锁；有批改/作业引用的对象不得清理；旧未知永久保护 |
| 客服/企业消息 | enterprise_asset.message_id + canonical append asset_ids | 复用组织/Workspace两键/状态CAS/消息保留期；禁止绕过企业Hold |
| 企业应用附件 | attachment scope=enterprise + EAR | 与普通GC共享删除安全协议，但保留应用域治理真源，不混成enterprise_asset |
| 注销/后台软删 | 既有账号/管理流程 | 只释放个人引用或撤销访问；不能按creator一把删掉共享/企业所有的资产；未知引用保留隔离记录 |

引用安全负面用例必须包括：收藏后删除原帖、转发后原消息过期、群文件上传者注销、Hold后后台删除、Purge与Hold相撞、多资产跨scope引用。不能由“referer_time没有减到0”关闭泄漏或误删风险。

## 6. 持久删除队列与调和

保留两个已存在队列及主键：普通/app/pending走attachment_object_deletion(path)，企业消息走enterprise_asset_delete_queue(org,ws,asset_id)。两者共享完整I、状态、attempts、退避、租约CAS与永久tombstone协议；复用普通attempts/next_retry_at/completed_at。企业旧pending_object_delete接入企业队列。无需变更企业PK或为个人伪造workspace。旧worker在切换窗口停止；严禁同时运行旧删除入口。
领取：按next_retry_at,稳定队列主键选可执行任务，FOR UPDATE SKIP LOCKED；UPDATE设置随机lease_token、lease_until、attempts+1；提交后调用driver。每批≤100，每个DELETE请求≤30s，lease=120s，物理租约固定120s不续租；DELETE请求30s硬超时；主执行器检查点心跳独立为30s；同job过期可再领。旧lease完成不得改新lease状态。
外部DELETE保证at-least-once，不宣称exactly-once；租约不能阻止已在途旧HTTP请求，所以安全根基是身份永不复用、删除意图不可逆且禁止新增引用。只有明确ok或该精确位置的not_found视为成功，403/超时/未知返回值均失败。
任务最多8次持久claim尝试（含领取后崩溃，不能当成8次实际DELETE）；退避30s、2m、10m、30m、2h、6h、12h（加≤20%抖动），第8次失败blocked；权限/位置不明立即blocked。DB故障不能把任务删掉或视为done。V5不提供已尝试blocked任务自动或人工清零重放；此类blocked是本版本终止状态。唯一例外是迁移生成的legacy_location_unverified、attempts=0且从未领取的旧job：专用proof角色同事务补齐位置、重验当前全部资格并写审计后可首次blocked→ready；这不是重试，也不能清零attempts。后续如要重放，必须另行设计持久attempt历史/epoch、单独审批并更新合同，不能运行时重置attempts。
调和仅比较“已提交job/墓碑/已登记session”与对应精确对象，禁止全桶扫到陌生对象即删。成功DELETE后DB落账失败→原job可重试；元数据提交失败→无job→禁止DELETE；旧pending_object_delete=true通过同事务入queue再清标记接管，done tombstone保留，历史旧deleted且flag=false绝不回填删除。

## 7. 迁移/发布兼容与回退

详见迁移合同。Expand→安全代码切换→回填证明→逐scope启用GC，不能混合旧删除worker与新队列。开关默认关闭物理GC；读取与上传可继续，危险删除入口需统一开关。停机部署、生产操作不在授权内，本文只规定未来顺序。
Down在存在位置回填、V5 session/job或delete_intent时明确异常阻断；不要自动DROP治理和墓碑。停worker/停新上传→保留数据库新增列→回退到仍能读新列和拒绝删除的安全兼容版本；不能直接回滚到本基线危险删除代码。已经物理删除无法靠SQL恢复，恢复需事先验证的备份/对象版本，没备份标记IRREVERSIBLE，不假装有回滚。

## 8. 八项不可放宽的验收

- AC01：pending INSERT失败/抛异常/commit结果未知，HTTP不含可用put_url；签名器调用0次（明确提交失败），不创建无登记对象。
- AC02：旧key所有者/scope/namespace/container变更请求被拒；配置切换后confirm/read/delete仍指原位置，unknown历史对象自动清理0。
- AC03：同key相同confirm重放100次，asset数=1、业务引用数不增加、首确认审计=1；冲突metadata不能覆盖原行。
- AC04：双真实PG连接屏障交错Confirm/GC、新增引用/GC各两个顺序，胜者唯一；确认/引用成功的对象字节仍存在，GC胜出后新引用被拒。
- AC05：每个删除入口与workspace/conversation/message/attachment级Hold、未来Retention组合，DELETE调用0且字节不变；删除意图后hold不得假成功。
- AC06：注入intent INSERT、audit、COMMIT失败，存储DELETE=0；不能只在mock内检验调用顺序，需独立连接观察提交可见性与真实对象。
- AC07：两个worker、进程SIGKILL、超时结果未知、DELETE后DB失败、租约过期/旧worker回写、毒任务，均达到done或有限blocked；合法引用对象不丢失，无永久busy loop。
- AC08：旧客户端请求/返回字段、旧附件读ACL、E2EE descriptor和加密字节、群history、视频Range保持；未知历史数据保留，Flutter真机恢复同一次上传/消息，不重复发附件。

每个AC绑定任务和真实证据，0 executed、skip、stub、只有HTTP200不能PASS。最终全量EUnit只在冻结候选执行一次；若失败修复造成候选变化，新候选需新的全量运行记录，不能沿用旧PASS。用户UAT由用户本人完成，自动化不代签。

## 9. Storage接口冻结与分阶段验证

W1冻结下列Erlang回调，不从已缺失历史计划推定签名：`presign_put(Location, Context)`、`put_file(Location, Path, Context)`、`stat(Location, Context)`、`private_url(Location, Context)`、`public_url(Location, Context)`、`delete(Location, Context)`。Location为完整I；Context只含操作需要的mime、已固定内容receipt、绝对issued_at/expires_at、request_id、范围/超时参数，禁止client传driver。返回统一`{ok, Value}`/`ok`/`{error, not_found | timeout | unauthorized | invalid_location | invalid_key | unsupported_driver | term()}`；只有stat明确404可映射not_found，DELETE未知结果不能ok。
W2/W3在已知Garage固定namespace与显式bucket下验证session/状态协议，W2-01先交付受控Garage裸PUT与内容pin，复用elib_oss的带bucket操作及绝对签名到期；不接受配置切换，不宣称已有Disk或跨driver验收。W4-03才迁移到完整统一边界并执行跨namespace配置切换/两个真实驱动；最终Gate不允许留下业务层绕过边界。
W2-03与W3引用竞态只证明生产Repo状态协议，不代表旧清理器已修复。W4-01必须接线所有真实业务入口并重跑同一竞态集合；AC04只有W4生产入口和W6真实字节验证后才能PASS。

## 2026-10-13 补充约束

迁移168的永久Workspace保留与path tombstone必须保留；禁止为收敛队列而迁移/删除它们。元数据已被旧GC删除且未保存namespace的任务默认blocked，不得从当前bucket猜位置。新增引用与GC应先advisory再行锁；当前delete_orphan_tx先DELETE取得行锁再enqueue锁，可能与INSERT触发器锁序相反，W2/W4必须用双连接屏障证明无死锁及误删，不能直接沿用。普通Confirm的asset/pending消失仍凭永久path墓碑拒绝复用。两队列不能处理同一物理I；新上传分离key空间并冻结domain，历史混用保留不清理。

持久session FK与metadata物理删除存在冲突：V5默认保留普通metadata tombstone，confirmed_attachment_id FK RESTRICT不改为CASCADE；旧已删metadata的pending仍按未知历史保护。W4收口普通删除由DELETE行改为live→delete_intent→deleted，企业已存在purge可删metadata但队列永久保留精确身份。

## 10. 计划质量与实施成熟度分离

目标9.9/10评价的是任务合同完整性，不要求设计轮提前完成未来实施。每个未知必须明确位于一个有责任文件、依赖、命令、测量oracle、失败终态的任务中；未知本身不扣计划质量，未纳入任务或缺乏可判定oracle才扣分。执行成熟度仍是NOT_STARTED/UNVERIFIED，不能被高计划评分升级。
精确案例、任务前后置、schema phase、证据字段与验证器变异负例以execution-contract.json为机器真源。case不存在/0执行/skip不是条件通过。每Gate精确比较期望leaf集合与实际leaf集合，聚合成功数不能替代case身份。W6全量/台账验证属于候选级检查，各场景执行一次，不按driver/domain虚增。
每个case的required_scenarios逐项命名故障点或并发顺序，禁止用normal/fault_injected泛标签合并不同故障。W6真实集成从具体source_case_ref/source_scenario展开；普通、企业、跨domain的阶段性DB用例在最终候选分别使用Garage/Disk合成对象重跑，纯SQL/catalog场景保留driver=none。all-ac01-ac07仅验证精确集合，属于PLAN叶，不能替代任何真实AC证据。场景拆分不增加产品功能；测试以同一fixture的有序子用例复用准备成本，每个叶仍单独记录观察结果。
普通confirmed metadata恢复查tombstone返回原结果且不重新stat未知当前存储；旧未确认session缺失namespace的请求不得猜新配置确认，只允许证据证明后的兼容确认或明确稳定retry/reupload错误，原对象保持保护。属于安全兼容限制，AC08必须覆盖且不能把该对象计入自动GC成功。

## 11. Worker活性与激活结果未知

每queue至少1个可执行槽，配置并发数c≥1；只有空闲槽才claim1行，不能先领取100行再串行排队。poll每5s，idle无忙等；reaper每30s独立扫描expired leased，最多100行/批并keyset推进，不能被ready或毒任务挤掉。DB健康时每slot一次服务预算≤45s（claim≤5s、DELETE≤30s、完成≤5s、调度余量≤5s）。固定有限backlog n、持续调度且DB/driver请求有界时，首次服务上界≤5+ceil(n/c)*45秒；旧lease reaper转移上界≤30*ceil(expired_rows/100)+5秒。重试的next_retry_at另计；第8次过期只block不第9次claim。持续DB故障不承诺成功时长，达到任务deadline记BLOCKED_EXTERNAL。

M3激活仅在isolated集成候选中：FROZEN→WRITERS_QUIESCED→DDL_VERIFIED→GUARDS_VERIFIED→NEW_WORKER_ENABLED。每transition保存candidate、schema/catalog/hash、角色权限、producer兼容集合与实际进程清单；DDL事务提交后若checkpoint未知，恢复先读取strict迁移ledger+catalog与精确输入digest。同一完整目标已经存在则补记DDL_VERIFIED并仍执行guard负例；未存在且确证rollback才重跑；部分或矛盾状态CONTRACT_CONFLICT，不自动CREATE OR REPLACE或启worker。只在GUARDS_VERIFIED后启动新worker。所有可写工具命令经过同一admission检查，未授权实施/commit分别拒绝；本轮仍DESIGN_ONLY，不构成服务启动或部署许可。

## 12. 上传恢复边界（W1冻结，W2实现，W5/W6消费）

复用四个现有presign入口，复用IMBoy上传handler及既有session表：普通GET `/api/v1/attachment/presign`；内部应用POST `/api/internal/v1/files/presign`；企业坐席POST `/api/v1/enterprise/organizations/:org_id/assets/presign`；访客POST `/api/v1/cs/widget/sessions/:id/assets/presign`。各自保留认证、原返回envelope与旧字段，新增 `operation=register|status|renew`，缺省register。status只读且不返回能力；每次响应Cache-Control:no-store。

新客户端先持久化随机UUID `client_upload_id`，首次登记携原声明，同ID重试返回同key/asset。服务端在既有attach_pending/enterprise_asset加可空唯一ID和upload_registration快照；重复时逐字段核验快照中的原主体/组织/Workspace/访客session和完整声明，不用客户端声明授权。跨主体404、不一致409；旧客户端无ID保持旧调用。内部Idempotency-Key保留digest冲突检查，但事务只保存稳定登记结果；外层确认COMMIT后才签名，不能继续保存完整signed URL再提交。EB在presign事务预留pending_confirm行，PUT仅CAS填充同一资产。

status/renew用client_upload_id或既有key/asset_id查找，重新鉴权；无权404或已知主体撤权403，不泄露对象。响应增量字段为client_upload_id、session_state、session_deadline、upload_expires_at、can_resume、retry_action。registered可renew；confirmed回原asset不再发PUT能力；rejected/delete_intent/deleted为409终态；数据库未知503/retry_same_id且无能力；历史身份不明409/recovery_unverified并保护字节。EB的pending_confirm映射registered，不改变既有业务status；EB不向客户端暴露物理key/位置。

新会话绝对deadline=登记DB时间+2小时；单次能力到期=min(DB时间+既有服务端TTL,deadline)：普通/内部应用默认3600秒，EB/widget默认900秒、上限3600秒，保留旧客户端时长，不擅自缩短。renew取同一父锁/I/session锁，检查授权、状态及无intent/job，将历次最晚upload_expires_at持久提交后签名；不延长绝对deadline。能力过期而session未过期可同key重签；超过deadline返回410/session_expired/reupload_required，由用户明确重传，新key重新seal，禁止静默换key。签名使用已提交的绝对到期值，已经过期不签；GC必须等最晚能力到期及真实在途drain证明。续签与GC共锁，不能出现GC已胜仍续签成功。

既有confirm继续接受key/upload_ref；EB新增asset_id作为查找参数，必须完整重验原actor/conversation/声明，不让asset_id变成授权凭证。已确认回原结果，避免重启后因不保存upload_ref而不能confirm。status不是HEAD存在即确认；确认核对真实对象hash/size与不可变upload_content、适用原声明，不凭HEAD通过。

客户端持久检查点依次为：网络前意图 → presign后登记身份 → PUT后uploaded → confirm后资产ID → 发消息前稳定client_msg_id。任一落盘失败停止下一次远程动作。Admin的attachmentUpload.ts逐阶段await checkpoint，不能只让seatSend在confirm后收到结果。journal不保存token/upload_ref/签名URL/明文消息/文件字节；丢本地文件要求重选匹配声明文件，不能自动上传另一份。账号/Workspace/访客session切换使旧回调失效，服务端授权仍是最终边界。

## 13. 数据库删除意图权限与无人值守准入

应用协议正确不等于直接DML安全。两个既有队列各有一个受限SECURITY DEFINER意图函数；业务角色无队列DML与受保护life/gc_verified/location列写权限，Worker只可claim/ack/reaper。函数由NOLOGIN角色拥有、固定search_path并完全限定对象名，撤销PUBLIC执行，业务角色不可SET ROLE为owner/proof。函数不接收“资格通过”布尔值或任意状态；按对象ID与有限reason在同事务锁后重查完整资格，写metadata/session intent、ready job、审计，任一失败全回滚。真实DELETE仍在该事务提交及claim提交后。详细权限、反例及历史proof规则以03和JSON为准；不声称能防恶意超级用户，运行凭据不得是表owner/superuser。

DESIGN_ONLY及implementation_authorized=false是此冻结计划的真实状态。未来明确实施授权由仓外run receipt绑定manifest SHA256、当前baseline摘要、三仓起点、foreign清单、任务范围、资源白名单、到期和独立commit权限；必须核对可信用户消息来源，执行器自写receipt不产生授权。本次“完善计划”不授权启动W0。有效receipt作为运行overlay，不改签名计划的mode来启用任务，避免hash与权限循环。

W0-01/02可按引导协议在runner产生前执行；W0-02仅原Up/catalog与静态未知保护矩阵，strict台账移到W0-03生成runner后另建专库验证并追验前两任务，G0之后才进入W1。W1-01只base/static；W1-02分库expand-queue→guards-contract；M3正式文件在W4-04所有writer适配后才进入priv。W3只证明Hold资格协议，W4-01必须通过同一48 Hold+24 Retention生产入口矩阵，W6两个真实driver复验；G3不提前宣称删除入口安全。

运行中只有单一OS锁持有者可写检查点；先fsync完整状态事件再写snapshot，恢复校验hash链并重放已持久事件。lease过期不允许越过活着的锁持有者。崩溃接管先确认旧进程与子进程身份，对本run进程组TERM最多10秒、KILL后最多5秒确认消失；无法确认或涉及外部进程则BLOCKED_EXTERNAL。attempt在启动前持久增加，task/wave期限从首次启动跨重启保留。COMMIT_UNKNOWN先逐仓比对parent/tree/owned路径，部分向量不得PASS或重复提交。


## 14. V5.4 固定上传内容与有限锁等待

**原声明真源**：attach_pending、enterprise_asset各新增可空upload_registration jsonb；固定version/surface、认证派生的subject(kind/principal/application/org/workspace/conversation/visitor_session)、declaration(filename/mime/size/hash/scope/scope_ref/message/business_identity/retain_until)。精确字段及规范化以JSON upload_registration_contract为准：缺省为null，ID/数值为规范十进制字符串，禁止重复键和未知字段；同ID重新register比较整个服务端规范化jsonb。status/renew只传稳定查找键，不重发或覆盖声明。该主体快照仅绑定恢复授权，不能代替业务归属或使已注销/撤权主体恢复权限；企业审计actor的合法SET NULL保持。旧NULL不可由当前访问者补造，返回recovery_unverified并保护对象。

**写字节边界**：所有新V5会话返回的put_url指向IMBoy受控裸PUT入口，保持响应字段、旧客户端无JWT/Cookie的PUT及CORS；EB/widget和multipart复用已有代理入口。W2-01先实现原计划中的attachment_storage_gateway_handler Garage路径，W4-03再统一driver并扩展Disk，避免前向依赖。网关能力使用专用upload audience、session/key/原主体摘要/操作/绝对到期/key-version；不能作为读或confirm授权。客户端不再取得可任意覆写的Garage URL。

网关在既有限额内将实际上传内容写入独占0600临时文件，计算SHA256/size，校验原声明；取父/I/session锁将upload_content从NULL一次固定为{version,sha256,size_bytes,mime}并COMMIT，再发送同一已校验文件的内容。未知COMMIT先调和，无已知持久pin不得PUT。同key不同字节/size/mime返回409且provider PUT=0；并发/重试只可写同一receipt。不能hash一个路径后重新打开可替换的文件；失败只清本请求临时文件，不补偿删除对象。普通E2EE拿到key后seal，pin记录密文hash，绝不索取明文hash。客户端须持久化同一密文/加密材料；材料丢失须新任务新key，不能同key重新随机加密。

Confirm真实读取精确I的对象、计算hash/size并比对receipt，然后同锁重验才提交。时间线PUT A→Confirm→旧能力PUT B：B在网关被拒；Confirm之前已在途的A重放只写相同内容，不能变成B。confirmed后新PUT不再写存储，仅回读相同结果或already_confirmed。旧直传能力不因新增状态字段而撤销；历史无receipt对象维持保护，需证明旧写能力耗尽再专用proof，否则稳定要求重传。

Garage内部签名固定已提交的issued_at/expires_at，不能暂停恢复后重新延长；本地30秒HTTP超时不是远端drain证明。无法证明在途结束上界，该类GC继续blocked。Disk PUT和DELETE共用完整I稳定OS锁，等待最多5秒，锁文件永久保留且不随对象unlink；先OS锁再短DB复核，所有DB事务结束后文件I/O，不允许持DB锁等OS锁。PUT原子发布与DELETE在同锁中，暂停旧PUT会阻断DELETE；不能在删除完成后重建对象。上述OS等待计入driver的30秒总请求预算。

这会增加普通上传的后端转发及Confirm完整对象读取成本。W2/W4必须验证既有最大文件、内存/临时磁盘/并发/超时、旧客户端进度/CORS；不能为达标退回可变内容直传。[Garage官方兼容表](https://github.com/deuxfleurs-org/garage/blob/main-v2/doc/book/reference-manual/s3-compatibility.md)只作设计参考，本合同不依赖未经当前实例验证的条件写或版本功能。没有新增SDK、生命周期表或第三队列；旁路存储writer必须在G4消除。

**有限启动**：runner仅LOCK_NB取锁，总等待≤2秒。活holder挂起且lease过期时仍不接管：本次调用exit75/BLOCKED_EXTERNAL，stdout及独占创建的observations/<unique>.json记录owner_unresponsive；未持锁不得写state/events，不计未启动任务attempt、不伪造权威任务状态。不得无限轮询或自动杀活holder。可信人工恢复可按已有10秒TERM/5秒KILL协议停止已确认本run进程，再取锁调和；身份不明保持阻断。此合同保证有限安全退出，不承诺活进程挂起后无需外部介入。

**阶段闭合**：M2安装register/confirm/pin、两队列intent/claim/ack/reaper/proof以及M3所需触发器函数定义、固定owner与EXECUTE权限；W2～W4-03可在expand-queue独立调用。M3只挂接最终守卫、约束并撤销旧表/列权限，不首次创建必需函数。M2兼容期GC关闭，旧宽权限不能称direct-DML防护完成；M3仍须W4-04所有writer适配后才进入priv。每阶段比对函数signature/owner/definition hash/ACL，禁止暗中手动加载M3补缺函数。

普通session的confirmed是终态，永远保留confirmed_attachment_id；已确认资产GC只转metadata life并写job，不能把其session改成delete_intent/deleted而违反confirmed iff linked约束。从未确认的session才走rejected/expired→delete_intent→deleted。恢复回原asset ID并明确metadata物理状态，不能把原confirmed结果当作仍可读写。W1 session-link-integrity及W6双driver验证该全程。

## 15. V5.5 企业恢复终态与入口覆盖

企业Purge继续按既有外键顺序删asset再删message。删除前，同一意图事务在enterprise_asset_delete_queue复制client_upload_id和完整规范化upload_registration；不新建session/幂等表。两字段永久不可变，不含能力/token/字节；同UUID在live与queue联合域只代表原asset。M2的register/intent函数共同取`hashtextextended('v5-eb-upload:'||UUID::text,169)`事务锁，顺序见§4；M3收紧直接DML。表内UNIQUE不能替代跨表查重。快照仅用于重新鉴权，不授予已撤销权限。

同UUID重试先查live及queue；queue优先。当前授权有效则409终态并返回原asset_id、can_resume=false、retry_action=none，无PUT能力；异主体404、已撤权403。原conversation/session已删除而无法证明权限仍拒绝，绝不按“原行不存在”创建新资产。同主体异声明409。复制、metadata删除、审计一起提交；COMMIT未知先调和，DB不可用503无能力。旧NULL身份不补造。墓碑不新增指向可清理message/contact/user的FK，既有Workspace RESTRICT不变。W1的SQL负例和W4两个真实企业入口的Purge后恢复均为required。

W2-01同时拥有eb_asset_object_garage:put/3最小适配：冻结issued_at/expires_at/location/content从eb_asset_store传到elib_oss显式Context入口及elib_s3_sign。不能在底层重新计时；旧四参数接口只保留给不属于V5的调用者，G4必须扫描所有V5 writer零旁路。W4-03再统一Garage/Disk边界。

内容pin、Confirm防覆写与Driver测试按ordinary、enterprise_internal、enterprise_seat、cs_widget分别出真实入口receipt。bare gateway/multipart只属于前两项；ciphertext-only只属于ordinary；具体适用集合以JSON叶为准，不创建虚假N/A。共享底层SQL测试不机械乘四。漏入口或拿坐席receipt代widget必须被验证器拒绝。

## 16. 并行开发与自动验收

20任务不扩成第二套流程。W0～W4保持有因果依赖的顺序；G4后W5-01与W5-02文件互斥，可两个worker同时开发，独立Reviewer只读；W6-01显式依赖两者。主集成者独占state/events及Git index/提交，worker只写受领文件与独占receipt。G5前两worker冻结写入，主集成在合并候选重跑两任务required定向测试，再逐任务串行提交，部分向量不能PASS。详细候选/崩溃规则见JSON parallel_execution_contract及06。

测试可在最多4个独立fixture槽并发；不得共享可变数据库、目录、key或编译输出。状态机仍单writer，资源容量不足可安全串行，但必须重新证明预算可行。默认task8h，W1-02 16h、W6-01 48h、W6-02 12h；wave24h，W6 96h；run7天，单命令30min。总尝试5、基础设施重试2、修复轮2不变，含重启/等待不重计。W0及每wave计算全部required与重验余量的排程；不足则阻断，不能减叶。
