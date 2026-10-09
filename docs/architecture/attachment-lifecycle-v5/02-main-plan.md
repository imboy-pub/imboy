# IMBoy 附件生命周期 V5 — 主工程合同 / Engineering contract

版本 V5.0，2026-10-09。状态：DESIGN_REVIEW_PASS_WITH_EXECUTION_GATES（最终review若未收口，以review阻断项为准）。**本轮只设计；业务实施 NOT_STARTED；生产与用户UAT UNVERIFIED。**

文档顺序：`01-facts-and-gaps.md`（FACT/GAP）→ 本文（DECISION/CONTRACT）→ `03-migration-contract.md` → `04-tasks.md`（TASK/TEST）→ `execution-contract.json` → `05-independent-review.md` → `evidence/`。
冻结对象是以上文件和 manifest 的字节哈希，不以“V5”标题代替计划绑定。后续实施必须先机械核验 manifest、三仓HEAD和任务拥有文件；计划变化产生新版本，旧验收不得搬用。

## 1. 决策及最小范围

1. 继续使用 attachment、attach_pending、enterprise_asset、企业两类治理表、既有业务关系。只扩展已有 enterprise_asset_delete_queue 以承载所有物理删除，不新建第二个GC框架、扫描桶删除器或通用引用总账。
2. metadata.status 是读授权/业务可见性，不能兼任物理删除状态；referer_time保留兼容展示，不作为真实引用计数或唯一GC条件。
3. Confirm只登记资产；创建业务关系才是引用。重试只回读同一资产；不能借 confirm/收藏/转发冲突更新改变原所有权、scope、位置、cipher 或计数。
4. 默认 conservative：历史位置或引用完整性不可证，`gc_verified=false`；仍按旧读ACL提供兼容，禁止自动清理。保护不是声称“历史已经验收”。
5. Storage Boundary是实际缺失依赖：Garage兼容、Disk实际读写删除都须完成。优先小范围迁移现有I/O到统一边界，禁止顺便重写消息系统；COS/OSS不属于V5验收。ADR0008是Proposed，本V5明确替代其“按当前scope全量回填garage”的建议，未来W1同步ADR；当前不改ADR。
6. 不新增上传SDK；旧 presign/confirm/view_url字段保留，允许新增状态字段。普通旧客户端无需发送新的session ID，object_key可作会话lookup键。已有 group anchor要求不得放松。
7. 不承诺所有历史字节都能回收：V5安全验收要求“可证明的对象可回收，无法证明的对象被阻止且可解释”。人工审批也不能代替技术归属证明；生产历史数据另需授权。

## 2. 五类对象与状态

| 对象 | 现有持久主体 | V5状态与转移 | 唯一职责 |
|---|---|---|---|
| Upload Session | attach_pending；企业消息域复用 enterprise_asset.pending_confirm | registered→confirmed；registered→rejected/expired→delete_intent→deleted；终态不回退 | 已签发或可能签发的写权限、上传截止、冻结身份 |
| Asset Metadata | attachment / enterprise_asset | live→delete_intent→deleted；历史默认live但gc_verified=false | 身份/读ACL/元数据；不承诺存在业务引用 |
| Business Reference | 群文件、moment、channel、收藏、消息/客服已有关系 | absent→bound→released；同业务ID重放不重复插入 | 谁仍然需要该对象；删除消息不等于立即物理删除 |
| Retention/Authorization | EAR、EB policy/hold、scope ACL | retention仅延长，hold none↔held；purged不可回退 | 决定是否允许形成删除意图，独立于上传者意愿 |
| Physical Delete Job | 扩展enterprise_asset_delete_queue | ready→leased→done；leased→retry_wait/blocked；过期leased→重新claim | 提交后的外部副作用及可恢复确认 |

registered写库提交后才调用签名并返回URL。签名成功但响应丢失：保留registered，视为“可能已签发”，不得立即补偿删除。签名失败也留下可回收登记，不删除登记后尝试重签。
confirmed session保留轻量结果/对象身份，支持幂等回读；不无限保存签名URL。pending_remove语义改为确认标记，不在过渡期擦除唯一身份。
业务status=0/-1不会直接置delete_intent。purged表示**逻辑删除已提交**，physical_delete_state单独表达尚未/已经删除字节；旧成功码仍对应逻辑操作完成，不冒充物理已完成。

## 3. 对象身份和不可变性

物理身份 `I=(storage_driver, storage_namespace, storage_container, object_key)`。
namespace是部署内不可复用的位置ID，指向固定存储实例/磁盘根；不是可变“当前driver”，也不是URL或凭据。更换根目录/bucket/实例必须分配新namespace；仅凭据轮换不改变身份。
保留attachment.path、pending.object_key、EB.object_key，不另造key字段。container在Garage为bucket，Disk为固定逻辑容器（例如default），namespace固定绝对root映射。
身份、上传者、租户/应用、初始scope、cipher、群锚点声明不可变；moment scope_ref允许且仅允许未绑定→同事务首个确定ID，群anchor_conv_seq同样单调首绑，不能把合法首绑误当违规。
数据库触发器和应用检查双层执行。位置未知行只能由专用证明回填流程填一次，审计记录manifest hash；不提供任意UPDATE“迁移位置”的API。
一个key永久不分配给另一会话/对象；已删除的元数据或job终态保留防复用标记。相同字节hash不是同一个对象。
上传能力尚可使用时不完成物理GC：记录upload_expires_at；需存储/网关最大在途上传时间上界。Disk网关强制超时和状态检查；Garage直传未证明上界时该pending标记blocked_upload_drain，仅可列出，不自动DELETE。不得用“过24h大概够了”证明所有在途写已消失。W4-03必须提供真实超时/迟到PUT证据，否则Garage该类pending回收保持关闭。

## 4. 事务锁顺序与线性化点

所有新增引用、Confirm、Hold、Retention、GC资格、账号注销按相同顺序：

1. 既有组织/成员资格锁 → Workspace行（涉及EB Hold和Purge使用FOR UPDATE；不允许拿资产锁再升级Workspace）。个人域跳过无关父锁。
2. 对象事务级 advisory lock，键用规范化完整身份摘要；普通缺位置旧行用稳定domain+asset ID，仅允许保护/读取。多对象按完整identity字节排序。哈希冲突只增加串行，不放松检查。
3. Session行 → Asset行（按ID升序，FOR UPDATE）→ 治理行（EAR等）→ 既有业务关系行（稳定业务键排序）→ Job行。

企业消息canonical append现有FOR UPDATE资产锁须先进入同一父锁/对象锁顺序；新建Hold必须先锁Workspace，再查覆盖范围。EAR初次创建也先锁对应资产，避免“治理行不存在”幻读。普通不存在资产行的首次Confirm必须锁真实pending行，不能靠SELECT空结果锁住不存在的行。
事务隔离默认READ COMMITTED；取锁之后重查所有资格。锁住前的候选查询、HEAD结果、列表快照不构成授权。死锁/序列化失败整事务限次重试，不能只重试其中SQL。

| 动作 | 线性化点及失败语义 |
|---|---|
| Pending登记 | INSERT事务COMMIT；失败或提交结果未知不返回可用URL，查登记再恢复 |
| Confirm | 持锁后合法registered→confirmed、metadata和必要审计同事务COMMIT；HEAD可在事务外，但必须持锁后重查会话版本/状态/位置；变动则拒绝或重新stat |
| 新引用 | 同一对象锁内检查asset live +授权，再与业务关系同事务COMMIT；job存在/delete_intent/deleted禁止新增 |
| 删除意图 | 父锁、对象锁、行锁内重验GC资格，将metadata设delete_intent并写不可变job快照/审计，COMMIT；这是引用和Hold竞争的裁决点 |
| Worker | 短事务CAS领取租约并COMMIT后才DELETE；成功后独立短事务按job_id+lease_token完成 |

Hold先提交：GC必须拒绝。删除意图先提交：该对象不再允许新增Hold/延长留存/新增引用；返回明确already_deleting，绝不能返回hold成功却继续删。Workspace级后续Hold可覆盖其余live对象，回执列明已逻辑删除对象不在覆盖范围。这是逻辑删除边界，非物理删除前可撤销承诺。
DELETE过程中不持数据库事务；DB断线不影响已提交意图安全，但暂停领取新job。拿到job后数据库故障导致无法再次验证时不发起新的DELETE；若请求已经在途，完成未知→等待数据库恢复后同job重试。

## 5. GC资格和引用合同

统一判据：完整且已证明位置、session写权限已耗尽、metadata处于合法可清理状态、引用检查完整且零有效引用、Retention到期、无Hold、所有权/租户匹配、无进行中的首绑/Confirm、策略允许、审计能同事务提交。任一查询失败或未知为不具备资格。
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
| 客服/企业消息 | enterprise_asset.message_id + canonical append asset_ids | 复用租户两键/状态CAS/消息保留期；禁止绕过企业Hold |
| 企业应用附件 | attachment scope=enterprise + EAR | 与普通GC共享物理队列，但保留应用域治理真源，不混成enterprise_asset |
| 注销/后台软删 | 既有账号/管理流程 | 只释放个人引用或撤销访问；不能按creator一把删掉共享/企业所有的资产；未知引用保留隔离记录 |

引用安全负面用例必须包括：收藏后删除原帖、转发后原消息过期、群文件上传者注销、Hold后后台删除、Purge与Hold相撞、多资产跨scope引用。不能由“referer_time没有减到0”关闭泄漏或误删风险。

## 6. 持久删除队列与调和

仍使用enterprise_asset_delete_queue。新增subject_kind/source_key、job_id、完整I、状态、attempts、next_attempt_at、lease_token/lease_until、last_error_code、completed_at；保留旧org/ws/asset唯一约束供旧enqueue语句兼容。个人/app任务不伪造workspace。旧worker在迁移窗口必须停止；新worker才能处理通用subject。
领取：按next_attempt_at,job_id选可执行任务，FOR UPDATE SKIP LOCKED；UPDATE设置随机lease_token、lease_until、attempts+1；提交后调用driver。每批≤100，每个DELETE请求≤30s，lease=120s，支持心跳但硬上限5min；同job过期可再领。旧lease完成不得改新lease状态。
外部DELETE保证at-least-once，不宣称exactly-once；租约不能阻止已在途旧HTTP请求，所以安全根基是身份永不复用、删除意图不可逆且禁止新增引用。只有明确ok或该精确位置的not_found视为成功，403/超时/未知返回值均失败。
任务最多8次物理尝试；退避30s、2m、10m、30m、2h、6h、12h（加≤20%抖动），第8次失败blocked；权限/位置不明立即blocked。DB故障不能把任务删掉或视为done。V5不提供已尝试blocked任务自动或人工清零重放；此类blocked是本版本终止状态。唯一例外是迁移生成的legacy_location_unverified、attempts=0且从未领取的旧job：专用proof角色同事务补齐位置、重验当前全部资格并写审计后可首次blocked→ready；这不是重试，也不能清零attempts。后续如要重放，必须另行设计持久attempt历史/epoch、单独审批并更新合同，不能运行时重置attempts。
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

W1冻结下列Erlang回调，不从已缺失历史计划推定签名：`presign_put(Location, Context)`、`put_file(Location, Path, Context)`、`stat(Location, Context)`、`private_url(Location, Context)`、`public_url(Location, Context)`、`delete(Location, Context)`。Location为完整I；Context只含操作需要的mime、expiry、request_id、范围/超时参数，禁止client传driver。返回统一`{ok, Value}`/`ok`/`{error, not_found | timeout | unauthorized | invalid_location | invalid_key | unsupported_driver | term()}`；只有stat明确404可映射not_found，DELETE未知结果不能ok。
W2/W3在已知Garage固定namespace与显式bucket下验证session/状态协议，复用elib_oss的带bucket操作；不接受配置切换，不宣称已有Disk或跨driver验收。W4-03才迁移到完整统一边界并执行跨namespace配置切换/两个真实驱动；最终Gate不允许留下业务层绕过边界。
W2-03与W3引用竞态只证明生产Repo状态协议，不代表旧清理器已修复。W4-01必须接线所有真实业务入口并重跑同一竞态集合；AC04只有W4生产入口和W6真实字节验证后才能PASS。
