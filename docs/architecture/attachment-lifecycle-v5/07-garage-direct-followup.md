# 07. Garage Direct 后续工程计划 — V5.5 收口后的独立增量

版本：1.0；日期：2026-10-27。状态：`PLANNED_NOT_ADMITTED`；产品测试：`NOT_RUN`。

本文件把 Garage Direct V2.3 的直传协议、最小数据模型、下载及性能验收接到附件生命周期 V5.5 后面。它是新的后续阶段，不属于正在执行的 W0～W6 / G0～G6；不修改 01～06、execution-contract.json、原 manifest、当前授权、任务状态、required 集合或既有验收结论。V5.5 的成功仍只按其原合同裁决，不能因新增本文件追溯判失败，也不能用原 G6 PASS 代替本阶段验收。

本轮授权只用于完善计划。ZCODE 不得因为看到本文件就在当前 run 中追加任务或扩大写集；须在原任务安全收口后，以真实用户消息授权本阶段实施、独立绑定本文件及当前候选。commit、push、合并 main、部署、生产迁移、数据操作及外部权限创建分别遵守授权；本文不授予这些权限，不修改 memory。

## 1. 与 V5.5 的衔接及准入

1. V5.5 主合同 §14 / W2-01 / W4-03 的受控网关、pin-before-PUT、完整对象确认语义继续服务旧协议会话，包括其 Garage/Disk 与历史兼容。新协议固定为 `direct-v1`，在 session 首次创建时持久化；不能让一次已创建的会话在两种协议间回退。新协议是对未来选定 Garage 新会话的数据面替换，需单独证明安全等价，不将它描述成原 V5.5 网关实现已满足的新要求。
2. 只读准备可以先做；本阶段产品代码实施要求原 G6 的当前有效本地候选、三仓提交/内容向量和未决事项可核验，且 V5 写协调器已释放相关写集。若 G6 未通过或仍有在途写入，只完成计划与只读盘点，不占用当前任务状态、测试资源、迁移号或 worker。若未来决定提前实施，必须先单独修订和授权跨计划依赖，不能由执行器自行跳过本准入。
3. 首次执行重新核验来源三仓最新 HEAD 与已收口 V5 候选的差异。它们不一致时，按文件/调用方/SQL/测试分析并形成明确集成基线，不能把另一分支 PASS 搬到新 HEAD。已有本计划族状态时始终恢复原 worktree；main 前移不能触发 reset/rebase 或新建一套进度。
4. 原 V5 task/Gate/validator 只识别原合同。本阶段使用独立计划族 `imboy-garage-direct-v1`、原 Direct 的 24 个 `step-*` ID 与 A0/A1/B/C/D/E；不得把它们写入原 V5 `state.json` 或交给只接受 W0～W6 的 runner。已有 Direct 状态、实现或迁移必须先核验复用；不能重新建设第二套 direct-v1。
5. 沿用 V5 的五类对象、既有业务真源、受限 SQL 函数、引用/Hold/Retention/intent、两个删除队列及永久墓碑。新增 attempt 只记多次外部写副作用，不是第二套附件主表、引用总账或第三个 GC 队列。

### 来源与冻结边界

本文件内含所需范围、24 卡依赖、验收编号与恢复规则，执行不依赖继续读取仓外旧包。来源为 `IMBoy_Garage_Direct_V2_3/TASKS.md` 及其 tasks/acceptance 登记表；历史源码定位只用于现场复核，不证明当前缺陷仍存在。本文形成时已静态确认 main 存在 `attach_direct_logic`、`attach_direct_attempt_repo` 及相关测试，不能把“需核验”自动变为“重新实现”。

| 输入 | SHA256 |
|---|---|
| 原 V5 manifest（保持原字节） | `c2d436ebfa793fd4b9c9eeec3ca9fc1e148f525dbca22ac799c381034e52fc3a` |
| 来源 TASKS.md | `af5ab7befbf25b47d92ffa1ca5ef5466114707a3029d32dcbc71b51d1e4a3055` |
| 来源 tasks.json | `461e32328a7ecd47a76c389253507ea282f28e953a648e342877e9ca9f95b182` |
| 来源 acceptance.json | `8a880ac63d0c878b9b271bd3c8ab60ef6c656dfc2dd9d2545ac143c98c430ceb` |

本阶段单独使用 `07-garage-direct-followup.sha256` 校验本文；不把它加入原 V5 manifest。执行 step-0 时另冻结当前实际授权、资源、三仓候选、适用规则与本阶段测试登记表；运行证据放仓外。旧包如仍存在可对照以上哈希；旧状态兼容需要的工具/包快照必须核验来源与完整性，不能覆盖已有 state。

本文版本1.0是补充文档修订号，不把已有 Direct 计划族重置为一个新 run。接管旧状态时逐条比对本补充的 V5 准入、队列身份、验收归属及回归要求，保留 task ID、WIP、全部历史尝试和副作用账；受影响旧 PASS 标为 INVALIDATED 并重验，不删除进度或自动继承原包 PASS。step-0 复用或补齐最小控制工具与机器任务/验收登记表，逐项核对本文精确集合与 DAG；不能另造通用执行平台。

## 2. 产品范围及明确排除

必需：Backend/App/Admin 三仓选定 Garage 单 PUT 链；普通私聊/群聊/频道/朋友圈、群文件/群相册、Flutter E2EE、Admin AI 头像、Internal Application、EB Member/Platform/Seat/Visitor、Widget。涵盖上传、最终确认、业务绑定、内容规则/派生、下载、删除和历史兼容。R1 为选定普通 scope 的小范围候选；完整交付仍需 R2 的群文件/相册/Admin、企业/客服与下载收敛，不能在 R1 后宣称本阶段完成。

不新增 MPU、新 Disk 数据面、新 provider 或 Moya 第四仓改造；保留 step-16～19 的范围外登记，不给它们 PASS。离线历史迁移/通用导入不属本轮；如果所选在线流程实际依赖它，先登记必要增量与授权，不能隐瞒正文旁路。历史 Disk/FastDFS 若存在，按合法原位 reader/intent 兼容；不强制全库 Disk=0，不删除既有模块和未知对象。

所选新 direct 正文退出 BEAM，NGINX/边缘承担正文；Erlang 只做鉴权和小型控制信息。旧 V5/Moya 等范围外兼容流量必须分开计量，不能把保留兼容要求改成全实例正文为零。

## 3. 新协议、最终字节与存储权限

唯一默认顺序：

`register（无写能力）→ 准备稳定最终字节/密文 → expected+reservation COMMIT → 内容绑定 grant → 私有 staging PUT → 真 HEAD 早拒绝 → 单次 dispatch Copy 到唯一 candidate → target_write_closed → 最终 full-hash/size proof → 必要内容 Worker 同字节规则 → 当前授权/配额/状态复核 → winner+mapping+actual pin+confirm 同事务 → bind → 可选显式发布`。

- register 不签 URL；expected 与 actual 分开。grant 的声明/配额已确认 COMMIT 才签能力；COMMIT 结果未知按稳定 ID 只读调和，不生成重复会话/配额/能力。scope/主体/hash/size/MIME 改变不得复用旧 grant。
- grant 固定真实 SHA256 checksum 与 Content-Type 签名头；摘要为原始 32 字节的 Base64。客户端 hash、可写 metadata、ETag 或 composite checksum 均不能单独作为最终 full proof。实际 Garage profile 不支持所需校验或可信 proof 时，拒绝启用，不退到旧 BEAM 路径或放宽 proof。
- staging/sealed 是两个私有 bucket；prefix 不能冒充 IAM 边界。上传 signer 无 sealed 权限；sealer、reader、janitor 明确角色和实际可授予的权限粒度。Worker 优先获取精确对象短期读能力，不声称 bucket 级 key 具备 object 级权限。配置/镜像目标 Garage v2.4.1，运行版本/digest/反代/profile 必须实测；不能混用旧 v2.3.0 证据。
- 前置 HEAD 只用于早拒绝，不是锁，不固定最终 actual pin。Copy 每个 attempt 仅一次持久 CAS dispatch；源/目标/声明/profile 在发包前记录。禁止 HTTP 客户端或代理透明重试/重定向产生第二次未知写；完整解析有界 Copy 响应，HTTP200 内 XML 错误不是成功。
- 只有固定版本/操作语义及关联唯一 dispatch 的明确成功终态才能支持 `target_write_closed`；随后对稳定 candidate 建最终 proof。Copy 响应丢失、超时、dispatch 后发包前 kill 均为 `UNKNOWN_QUARANTINED`。本阶段 unknown 没有晋升边；HEAD 匹配、hash 相同、lease 到期、等待若干秒均不能晋升。新的写操作必须新 attempt、新 physical target。
- 旧 PUT 重放最多影响 staging，不能改 winner。每个可能被写的物理 key 永不复用；DB fence/租约不能停止已发外部请求。陈旧 owner 不可提交 winner，不可回写新 lease 状态。
- 控制接口实现真 HEAD/Copy，修复 elib_oss GET+Range 和 driver 完整 read 两条伪 HEAD；direct 不调用 `verify_and_save_v5 → v5_object_bytes → read_snapshot` 获取正文，也不通过其他名字隐藏完整读取。headers/XML/错误体等在完整缓冲前执行硬上限。

### 状态与事务

| 对象 | 合法推进 | 失败/恢复裁决 |
|---|---|---|
| session | RESERVED → DECLARED → GRANTED → FINALIZING → CONFIRMED；或 EXPIRED/CANCELLED | 固定主体/声明/协议/profile/总 deadline；terminal 不回退，未知提交先调和 |
| attempt | CREATED → DISPATCHED → COPY_COMPLETED → PROOF_VERIFIED → WINNER | Copy 结果未知进 UNKNOWN_QUARANTINED；无晋升边；规则/证据失败 REJECTED |
| 未发布 candidate | PROOF_VERIFIED → WINNER 或 GC_CLAIMED | winner CAS 与 GC claim 同一固定锁序；GC 胜后永不发布，winner 胜后 candidate GC 拒绝 |
| 正式资产 | confirmed → bound/released → 原生命周期资格与 intent | Confirm 不增业务引用；Hold/Retention/引用检查和 direct DML 守卫继续有效 |

step-3 在现有枚举旁显式映射新状态，冻结 canonical JSON、ID 精度/时间单位、每认证面 `(HTTP status,envelope code,state,retry_action)`；不能原地改义旧字段、破坏 HTTP200 业务 envelope 或留安全相关未决分支。

## 4. 最小数据模型、锁序与迁移

1. attachment/attach_pending/enterprise_asset 继续是业务真源；在必要现有字段与最小 attempt 子表上表达 expected、reservation、dispatch、closed/proof、unknown、winner。已有迁移/表/函数先取证复用，不预占写死迁移号，不建立第二 session/附件真源。
2. logical object_key 保持 `u<Uid>/...`、消息 descriptor、旧 API 身份不变；新 candidate 使用不同内部 physical_key。完整映射至少含 driver/namespace/container/physical_key/locator_digest/winner/proof 版本。新 direct 必须有合法映射；旧行明确走 legacy resolver，缺位置 proof 不按默认 bucket/scope 猜测。
3. 声明、grant、attempt、winner 及资产按固定锁序推进；沿用 V5 父级/对象 advisory 锁和 restricted 函数，并明确 logical/physical 锁标识与稳定排序。step-3 输出 SQL 级锁序和每条 writer 清单；step-7a 两真实 PG 连接屏障证明与 Confirm、首绑、GC、Hold、Purge 无反序、误发布或误删。网络 I/O 不置于长持锁事务。
4. winner+location mapping+actual pin+confirmed 原子提交；直接 DML 不能伪造完成、替换 winner/位置、重新授予写权或向 tombstone 绑引用。不能因实际 mapping 和 logical key 不同而放松 V5 物理身份唯一性。
5. reader/view_url/content/收藏转发/hash JOIN/群文件相册/企业 purge/普通 orphan 与 pending 清理全部消费解析后的精确身份。元数据删除事务必须先把完整 physical winner 捕获进持久 intent；主行删除后不能再查 mapping 猜删除目标。正式删除仍只由提交后的既有 worker 执行。
6. 同一 logical session 可有多个失败 candidate，不能把它们覆盖进按 logical path 唯一的一条正式 job。step-4 必须设计并证明：沿用所属普通/企业既有队列的独立不可变 job identity，保存各 candidate 精确物理身份，禁止覆盖正式 winner 的 intent/墓碑；多 candidate 可分别记账且两个队列不能指向同一物理对象。若既有键/守卫不支持，先冻结最小受审查扩列/索引/函数修订，不通过新增第三队列或直接 DELETE 绕开。
7. candidate 账本只裁决是否可 enqueue，不能成为独立 DELETE worker。winner、candidate GC、正式 GC、Hold/Purge 的交错必须有明确安全结果；Hold 在正式 intent 之后建立不得假成功，之前建立必须阻止相应正式删除。unknown 无法证明写已终止时保留隔离与容量账；404 不证明未来不会迟到写。
8. Expand 和新旧兼容 reader/intent 先行，全部 writer 适配后才挂最终 guards，最后按 scope 启用 direct。schema-phase 仅可用 `base / expand-queue / guards-contract / integrated`，不得杜撰第五种。迁移预约取现场最新集合；空库与历史 fixture 升级、逐 DDL 失败、中断回填/续跑、部分 NULL、权限/函数 ACL、索引锁等待和滚动读写均需真实验证。大表索引是否用 CONCURRENTLY、事务边界、分批 keyset/水位、lock_timeout/statement_timeout 的数值在 step-3 冻结后才实施。
9. 新 session/mapping/attempt/job 已存在时禁止破坏性 Down。回滚优先停新 grant，保留兼容 reader、mapping、intent、restricted 函数、quota/GC 账和 reconciler；不能回到不认识新定位的旧二进制。已删字节不承诺 SQL 可恢复。

## 5. 下载、客户端、内容规则与配额

- 显式区分 `business_gateway / s3_put / edge_content`。既有同源 Bearer/upload_ref/capability 规则保留；s3_put 仅带 profile 允许的 required_headers，不转发业务 Authorization/Cookie/Host。不能按 URL 外观猜类型。浏览器不要求手工设置 Content-Length；native fixed-length 与 HTTP2/chunked 分别实测，超大正文硬限额由边缘/provider 落实。
- group/channel/moment/teaching、企业/Seat/Visitor 新 direct 默认逐请求 edge_content，复用当前 ACL。private/c2c 只有在 step-3 明确接受有限授权窗口后可短 GET，否则保持严格路径。原 GET URL 必须记录上界和排空；auth_request 仅保证下一请求资格，不能承诺撤回已下载字节或中断长连接。
- GET/HEAD/Range/206/416/304、401/403/404、缓存 key/账号隔离、origin 不可绕过、SSRF allowlist、内部签名不外泄、HTTP200 业务错误不能误当鉴权成功，均需真实边缘测试。AssetsService/现有 URL resolver 规则保持，受控 URL 缓存不得跨主体复用。
- Flutter 首传、重传、恢复、接收使用文件/可重开流；大文件不 readAsBytes。密文只生成一次并持久化可安全恢复材料和文件；固定 logical key，不改变 chunk/AAD/nonce/descriptor 算法。固定输入比较新旧完整密文字节/hash；解密到隔离文件，全部 tag/size/full hash 通过才原子暴露。丢失材料必须新任务新 key，不能重新随机加密到旧 key。实际验证新 sender→旧 receiver。
- 沿用现有账号 epoch、AbortSignal、SQLite/Admin journal 和阶段性持久检查点；网络前意图、grant 后身份、PUT 后、confirm 后及发消息前都有恢复点。新 parser 能表示服务端封存中，不把上传100%当作 confirmed，不重复发消息。普通 journal 不存 JWT/签名 URL/密钥/明文或正文。
- 独立受限 OS 进程/容器执行现有 EB 魔数/文本尾部和必要图片规则，并在同次稳定读中计算完整 hash；receipt 绑定 candidate/attempt/hash/size/ruleset/授权。过期、伪造、跨 attempt、超时失败关闭；E2EE 不交明文 Worker。变换输出另有 locator/hash/lineage/proof。公开复制与业务发布是独立步骤，未许可前 staging/candidate/派生不得匿名可读，发布 DB/S3 非原子失败可调和。
- 普通上限 104857600 bytes，EB 26214400 bytes；E2EE 明文上限扣除 tag 等密文开销。Web hash 阶段也要预算，不以 File PUT 流式推断 Blob.arrayBuffer 无内存问题；小对象 bytes 便捷接口有明确大小/并发界限。
- reservation、confirm 转占用、cancel 和 bind 独立记账；confirmed 未 bind 无空窗，bind 不再次收费。逻辑配额与 staging/candidate/unknown/Worker 临时盘物理容量分账。主体/全局并发、总 deadline、每 session 最大 dispatch 与 unknown 数/字节/龄期阈值在新 grant 前冻结，按持久累计值计数；达到阈值停相应新 grant，不无限创建 target。

## 6. 24 个稳定任务及最小交付

保持原 Direct 的 ID 与 DAG，便于已有执行恢复，不把它们混入 V5 台账。以下卡的 implementation 与 verification 分开；代码卡只需相关 LOCAL_TEST，真实 A0/A1/B/C/D 不可由 mock 代替。基线中已完成的能力须给当前文件/函数/测试证据再裁决复用，不能重做。

验收归属修正：原包 step-20b 泛称执行全部适用 T01～T50，但 T49/T50 由后继性能卡 step-20c 实际交付；若把它们当作 step-20b 的退出前置，会产生隐性循环。本文规定 step-20b 收口 T01～T48 的适用项及 E01～E12；T49/T50 仅由 step-20c 收口，step-20 再汇总57个 required。保留显式 DAG，不降低最终集合。

| TASK_ID | 任务 | 直接依赖 | 最低证据 | 实现边界/退出要求 |
|---|---|---|---|---|
| `step-0` | 控制目录、恢复合同与工具自检 | 无 | `LOCAL_TEST` | 建立唯一 plan_family 控制目录；验证本包校验值；建立任务、仓库绑定和证据状态。首次与恢复调用使用同一 RESUME_PROMPT.md。；首次初始化不写用户三仓；重复 init 保留现有进度；损坏状态不能重置；同一 worktree 绑定不得悄悄替换。 |
| `step-1` | 三仓基线、规则、全部正文入口与消费者盘点 | step-0 | `STATIC_REVIEW` | 只读核验启动时真实 Backend/App/Admin HEAD、Git 身份、AGENTS/CLAUDE、工作区和并行计划；对源码来源 R01–R10/G01–G20 重新定位，并生成全部启用附件入口/正文 source-sink/映射/下载/删除消费者清单。历史来源 SHA 仅为比较基线，不要求回退；Moya 不实施。；三仓基线与外部WIP摘要完整；每个入口有owner、正文消费者、测试入口和证据状态；缺资源仅阻塞对应门禁。 |
| `step-2` | A0：Garage 原生能力与测试资源登记 | step-1 | `PROVIDER_NATIVE` | 在已授权隔离资源验证固定provider的逐方法合同，区分A0原生能力与A1应用集成。；完成本包对应交付、测试与证据；缺资源不得伪 PASS。 |
| `step-3` | direct-v1 协议、状态机、权限与兼容合同冻结 | step-1 | `STATIC_REVIEW` | 制定独立direct-v1附件传输协议：旧会话保持旧含义，register不发能力，grant提交后签发，finalize有明确处理中状态；逐认证面冻结HTTP/envelope/时间单位、required_headers、逻辑key与candidate映射、下载撤权窗口、SQL锁序及恢复期限。；无影响安全或兼容的未决协议分支；真实三端parser可表达处理中和失败；provider未知项明确阻塞启用。 |
| `step-4` | 逻辑/物理位置映射、最小 Schema 与兼容删除 | step-3 | `LOCAL_TEST` | 在已冻结协议和已预约迁移号下，扩展现有pending与最小seal attempt模型，交付logical key到唯一physical winner的约束映射、兼容reader和删除intent捕获；SQL沿现有restricted函数与短事务锁序实现，不创建第二附件主表。；新旧位置可正确解析且部分NULL失败关闭；winner和映射原子可见；元数据删除前持久化完整物理身份。 |
| `step-5` | 签名、双伪 HEAD 修复与有界控制客户端 | step-3 | `LOCAL_TEST` | 在冻结provider合同下实现带checksum与Content-Type的signed-headers presign，贯通真实storage driver的HEAD、Copy及可信proof；同时处理elib_oss的GET+Range和garage driver整文件read伪HEAD，约束错误正文、headers、XML、重试与重定向。；单元/控制流负向通过；两条伪 HEAD 都有新控制路径；direct 无全文 GET fallback。真实 A1 单列 step-5a，不让缺实桶阻塞独立编码。 |
| `step-5a` | A1：真实应用签名器与 Garage/反代合同 | step-2、step-5 | `APP_STORAGE_INTEGRATION` | 以已交付应用 signer/driver 运行真实 Garage 和同类反代合同；A0 原生 SDK 探测不能冒充 A1。；checksum/头/方法/路径/权限负向逐项验证；上传 signer 无 sealed 权限；无整文件 GET fallback；运行版本/profile/候选绑定。 |
| `step-6` | direct 会话、单次封存、最终 proof 与恢复 | step-4、step-5 | `LOCAL_TEST` | 在数据模型与有界控制接口已具备后实现direct register/grant/finalize/reconcile；每次可能写入使用新candidate与持久化dispatch CAS，未知Copy只探测原target，稳定最终字节proof后短事务复核授权并pin/winner/confirm；新协议绕开旧v5_object_bytes。；local/fake 状态机与 SQL 约束通过；每 attempt 一次写授权；unknown 仅隔离、不能由 HEAD 相等晋升；真实 B 单列 step-7a。 |
| `step-7` | 配额、candidate GC 与正式生命周期互斥 | step-4、step-6 | `LOCAL_TEST` | 基于现有intent和GC框架补齐direct多节点reservation、confirmed未bind占用、candidate/unknown物理容量与取消对账；candidate GC claim和winner同数据库互斥，正式purge在元数据删除事务内捕获physical winner，保留hold及引用守卫。；并发不超卖或重复释放；GC先claim则不能发布且winner先提交则不能删；迟到对象有tombstone和预算，未知历史不猜位置。 |
| `step-7a` | B-core：真实 PG/Garage 并发及未知写隔离 | step-5a、step-6、step-7 | `REAL_PG_RACE` | 以 barrier/failpoint 确定性注入 HEAD 后换源、Copy 响应丢失、dispatch 后 kill、旧 owner 迟到、winner/GC 与取消竞争。；正确请求可确认；错误或 unknown candidate 零发布；只能新 target 重试；完整 HTTP/DB/对象断言，不以成功计数代替一致性。 |
| `step-8` | Flutter 首传/恢复/取消的文件传输全链 | step-3、step-4、step-5 | `LOCAL_TEST` | 将Flutter附件source、AttachmentApi、resumable uploader、网络PUT及重启恢复改为文件/可重开流，传输新descriptor全部允许headers并处理封存中状态；大文件恢复不readAsBytes，保留账号epoch与取消、普通旧协议兼容及文件所有权清理。；首传、恢复、取消与 required_headers 定向测试通过；无大文件 readAsBytes；真实设备证明属于 step-20b，未过 A1/B 不启用。 |
| `step-9` | E2EE 文件化 seal/open 与新旧字节等价 | step-8 | `LOCAL_TEST` | 在Flutter文件source已具备后，将现有附件seal/open及真实调用方改为文件化分块处理；保留chunk、AAD、nonce和descriptor密码合同及稳定logical key，同session复用同一密文文件；解密先写隔离临时文件，全部认证后才对预览和业务可见。；固定输入的新旧密文字节/hash完全等价；重启与重试不重新生成密文；末块或full hash失败时无可见明文，密文上限含tag开销。 |
| `step-10` | 普通附件映射、边缘下载、scope 撤权与缓存 | step-3、step-4、step-5 | `LOCAL_TEST` | 按冻结scope读政策改造普通view/download、逻辑到physical winner解析、边缘鉴权与App URL缓存；要求逐请求权限的group/channel/moment等走受控content，明确private/c2c有限授权窗口和旧S3 GET到期排空，保留历史消息可读。；普通读 resolver 与边缘授权适配的代码/测试齐全；scope 策略固定；真实撤权/Range/缓存在 step-20b；缺现场证据不能称强撤权已通过。 |
| `step-11` | 群文件/相册从正文上传改为确认引用绑定 | step-4、step-6、step-7 | `LOCAL_TEST` | 在direct确认和生命周期已具备后，将群文件multipart与相册multipart/base64及批量正文入口改为confirmed逻辑引用绑定，贯通Handler到Logic及真实调用方；保留消息锚点、当前成员权限、逐项结果与批量上限，过渡旧入口明确限额和退役条件。；未确认或跨主体附件不能绑定；重复调用不重复引用且部分失败逐项明确；旧图片检查依赖未满足时不能启用新入口。 |
| `step-12` | 最小内容 Worker、认证 receipt 与图片策略 | step-3、step-4、step-5 | `LOCAL_TEST` | 交付独立受限内容Worker及认证receipt接口，读取稳定最终candidate，在同次受控读中计算完整hash并执行现有EB魔数/文本尾部规则；图片变换独立输出locator/hash/lineage，冻结CPU、内存、解压、临时盘及超时预算，不处理E2EE明文。 不开发 Disk 上传 Worker；复用本任务唯一最小 Worker，不新增独立业务权限系统。；现有规则样本逐项等价；伪造/跨attempt/过期ruleset与超时失败关闭；输入输出proof绑定精确字节且未经许可不公开。 |
| `step-13` | Admin 头像直传、处理与发布 | step-6、step-7、step-12 | `LOCAL_TEST` | 在direct核心、图片Worker和发布合同可用后，改造Admin身份上传会话、AI头像业务绑定与Web File/Blob传输，替代现有FormData正文入口；消费明确descriptor并隔离业务认证，派生图片和公开复制失败可恢复，不借用Human凭据。；真实浏览器CORS和required_headers正确；S3无Admin Cookie/JWT；验证与业务发布许可前匿名不可读，失败无错误头像引用。 本卡以代码和定向测试为退出条件；真实业务、浏览器与设备验收归 step-20b，不以本卡 PASS 替代。 |
| `step-14` | Internal/EB/Seat/Visitor 三端企业上传 | step-6、step-7、step-8、step-12 | `LOCAL_TEST` | 将Internal Application、EB Member/Platform/Seat/Visitor及App企业上传逐面接入direct descriptor和内容Worker，保留各自认证、upload_ref与业务绑定合同；明确business_gateway和s3_put分派，保留同源业务Bearer分支而不把认证头发送存储。；每个认证面有真实HTTP证据；25MiB与魔数/文本规则不退化；登录切换和取消不跨主体写回，封存中不误发消息。 本卡以代码和定向测试为退出条件；真实业务、浏览器与设备验收归 step-20b，不以本卡 PASS 替代。 |
| `step-15` | 企业 content 边缘下载与独立认证面 | step-4、step-10、step-14 | `LOCAL_TEST` | 在位置映射和统一边缘基础可用后，为Internal Application及EB/Seat/Visitor原content路由交付逐请求授权适配与私有origin正文传输，保留实例凭证scope及各认证面；冻结HEAD/Range、缓存隔离、内部错误转换与传输审计。；撤权后下一请求拒绝；GET/HEAD/206/416/304及无权限隐藏符合合同；客户端不能绕origin或接触内部签名，正文不进BEAM。 本卡以代码和定向测试为退出条件；真实业务、浏览器与设备验收归 step-20b，不以本卡 PASS 替代。 |
| `step-20a` | 本地最终集成、适用全量检查与恢复协议演练 | step-4、step-5、step-6、step-7、step-8、step-9、step-10、step-11、step-12、step-13、step-14、step-15 | `LOCAL_TEST` | 集成本任务所有必需代码，核对三仓 worktree 与测试注册表，执行适用全量/定向检查及同提示词恢复协议测试。；所有本地适用测试有真实输出；生产与真实设备不可用另记阻塞；不能拿本地通过改写真实门禁。 |
| `step-20b` | 真实 HTTP/浏览器/设备/同字节规则及兼容门 | step-7a、step-20a | `RUNTIME_ACCEPTANCE` | 在同一完整候选运行三端真实业务 HTTP、桌面浏览器、获准 Android/iOS、企业 Worker 和下载边缘测试；覆盖旧发送/接收兼容。所选全部认证面及 scope 有证据；T01～T48 中适用用例及 E01～E12 执行；T49/T50 交后继 step-20c，不作为本卡退出前置；缺设备/旧版本客户端明确 BLOCKED 对应行。 |
| `step-20c` | C：完整链路流量、内存、消息延迟与容量验收 | step-20b | `LOAD_TEST` | 冻结预算与 SLO 后比较旧 spool+confirm GET 完整链路和新准备/PUT/Copy/proof/bind/下载链路，采集应用与数据面独立指标。；上传、confirm、下载、内容规则与派生所有所选正文路径经 BEAM 为零；同文件/并发/校验/网络条件；尾延迟有样本数，无伪造节省率。 |
| `step-20` | A0/A1/B/C/D/E 同一最终候选证据汇总 | step-2、step-5a、step-7a、step-20a、step-20b、step-20c | `STATIC_REVIEW` | 将各门禁和所有适用 T/E case 汇总到相同三仓候选与 profile 指纹；审查证据缺失、失效、数量及例外，不执行不存在的斜杠命令。；必需用例全部真实通过、证据对当前候选有效；本轮排除 MPU/Disk/Moya 的专属新增能力，不可排除失败核心 case。 |
| `step-21` | 最终 diff、授权、锁序及证据复审 | step-20 | `INDEPENDENT_REVIEW` | 审查最终候选diff、SQL权限锁序、完整性、认证域、日志及门禁证据，指出未解决项。；单独 review 记录覆盖代码、产品安全及证据；不能声称存在未实际调用的独立审查者；高风险发现必须修复重验；无源码型空 PASS。 |
| `step-22` | Garage-only 交付、停签发/排空/回滚方案 | step-21 | `STATIC_REVIEW` | 为已通过所选入口A1/B/C/D和最终审查的候选制定可评审灰度清单，冻结scope、客户端版本、停止新grant、旧URL/会话/在途操作排空、reader/reconciler保留和回滚步骤；只生成计划，任何实际发布和第三方动作另按用户明确确认。；Garage-only 发布清单、停止条件、历史读取/删除、回滚和资源交接完备；本卡仅产出发布方案，不部署、不 push；历史 Disk 不强制为零。 |

### 每卡强制完成合同

每卡实施前把以下内容冻结到仓外 harness registry：TASK_ID/owner、实际文件与函数/全部调用者、依赖与写集、SQL/迁移/索引/权限影响、精确命令数组/cwd/显式 fixture 环境、关联 T/E 与子叶、行为 oracle、超时/修复预算、失败恢复/回滚、候选及证据路径。没有这些不能从 READY 进入产品写入；文档中的历史路径或概括不是可执行命令。

每卡完成要有可评审的完整 diff 和必要测试/迁移/文档；独立审查按实际能力安排，不能自称已经调用 Reviewer。有 commit 授权才按完整逻辑功能精确 stage 并提交；不按表格行机械拆碎提交、不混 foreign WIP。主协调器独占 index、state 和提交；commit 结果未知先调和 HEAD/tree/预期父提交/提交内容，不重复 commit。跨仓部分成功保持 PARTIAL，不标完整 PASS。

### 现场路径与定向验证入口

| 责任面/任务 | 必须覆盖的实际路径（相对所属仓） | 已存在的定位入口或待交付验证 |
|---|---|---|
| Backend，step-3/4/6/7 | `src/api/attach_handler.erl`、`attach_direct_handler.erl`；`src/logic/attach_logic.erl`、`attach_direct_logic.erl`；`src/repo/attach_pending_repo.erl`、`attach_direct_attempt_repo.erl`；`priv/migrations/` | 现有 `test/logic/attach_direct_{confirm,logic_pg,quota_gc_pg,bind}_tests.erl` 与 `test/api/attach_direct_handler_tests.erl`；逐模块注册真实命令，先核查 fixture 隔离 |
| Storage，step-2/5/5a | `src/lib/elib_s3_sign.erl`、`elib_oss.erl`、`attachment_storage/`、`eb_asset_object_garage.erl` | `test/lib/attachment_storage_garage_direct_tests.erl`；A0 原生方法与 A1 signer/driver/反代分开交付；已有 gate 先读参数/副作用 |
| GC，step-4/7/7a | `attach_cleanup_logic`、`attachment_gc_repo`、`attachment_object_deletion`、`eb_pg_purge`、`eb_pg_asset_delete_queue` 及现有 worker/受限 SQL | 双真实 PG 连接+Garage+故障代理，关联 T13～T22/T40/T41/T46/T48；禁止按 plan state 代替业务 DB |
| Flutter，step-8/9/10/11/14 | `lib/store/api/attachment_api.dart`、`lib/component/upload/resumable_attachment_uploader.dart`、`lib/store/repository/attachment_upload_task_repo_sqlite.dart`、E2EE codec/encryptor、URL resolver/cache、企业 transport | `flutter test test/unit_test/store/attachment_upload_presign_test.dart`；同目录 recovery 测试与 `test/unit_test/service/e2ee/attachment_chunk_codec_test.dart`；真实恢复和内存证明另列 |
| Backend/App 群业务，step-11 | `src/api/group_file_handler.erl`、`group_album_handler.erl` 及全部 Logic/客户端 multipart/base64 调用者 | 实际 HTTP+DB：消息锚点、逐项失败、成员权限、未确认拒绑、重复调用幂等 |
| Admin，step-13/14 | `src/modules/ai_agent/api/public.ts`、Seat API/attachmentUpload/seatSend、Widget uploader、upload journal；对应 Backend 头像/企业 handler | `bun run test src/widget/customer_service/app/uploader.test.ts src/modules/customer_service/seat/workbench/seatSend.attachmentRecovery.test.tsx`；AI 头像相关定向及真实浏览器另列 |
| 下载/企业，step-10/15 | 普通 sign_view/content、`AssetsService` 与缓存、`enterprise_asset_handler/logic`、`cs_widget_handler`、`eb_asset_content`；实际边缘配置 | 各认证面真实 GET/HEAD/Range/撤权/缓存/origin 绕过；先定位现有部署配置，不假设 NGINX 已存在 |
| Worker，step-12/13/14 | 现场核验已有进程/目录后复用或新增最小 Worker 与 receipt adapter；图片输出/发布调用方 | 规则等价样本、同字节 proof、恶意图片/资源预算与 receipt 负向；缺 harness 由本卡交付 |

Backend 定向命令只在注册后的隔离 cwd、显式 scratch DB/provider 下执行，例如 `IMBOYENV=local make eunit t=attach_direct_confirm_tests`；它可能启动应用/DB，不能裸跑默认业务配置。Flutter/Admin 命令也需核对 SDK、cwd、依赖、资源占用；现有测试文件存在不代表已通过。

新 A0/A1/B/C/D harness 先在所属卡实现并记录真实命令；不发明已存在的 `--gate` CLI，也不把 V5 runner 当 Direct runner。step-0 的控制工具自测不依赖尚未完成的产品 A1/B；step-5a / step-7a 才承担应用/并发真实验证，避免循环。

## 7. 验收集合与 Gate

共有 62 个登记编号：T01～T50、E01～E12；本轮 57 个 required。仅 T37～T39（新 MPU）、T42/T43（新 Disk）范围外，不标 PASS。T34 只验 Garage/边缘，T41/T48 保留历史 Disk reader/intent 兼容要求。每编号按认证面/scope/故障位置展开精确唯一子叶，step-3 冻结集合，数量只用于一致性检查。

| ID | 是否必需 | 行为 oracle |
|---|---|---|
| `T01` | 是 | direct register无URL；grant回滚/COMMIT响应丢失/恢复查询失败均不越权签发，无重复reservation |
| `T02` | 是 | 同client_upload_id同声明同session，确认/恢复不重复记账 |
| `T03` | 是 | 换hash/size/MIME/scope冲突，不复用旧grant |
| `T04` | 是 | 跨实例/主体/应用凭据scope操作status/renew/finalize不可成功且不泄露 |
| `T05` | 是 | 错SHA/换正文，存储拒绝或最终不可确认 |
| `T06` | 是 | 删除/替换checksum头失败，32字节Base64编码正确 |
| `T07` | 是 | copy-source/ACL/额外x-amz头/额外method不扩大能力 |
| `T08` | 是 | 普通104857600+1、EB26214400+1、E2EE边界与tag开销，硬限额拒绝点/残留明确 |
| `T09` | 是 | 空文件、字段/part/batch限制，无无界累积 |
| `T10` | 是 | 中文/空格/+/%、显式端口、路径前缀签名与请求一致 |
| `T11` | 是 | 浏览器禁设Content-Length、HTTP2/chunked，不绕过数据面上限 |
| `T12` | 是 | metadata、无来源SHA、COMPOSITE均不伪作full proof；Worker替代需独立通过 |
| `T13` | 是 | HEAD后/Copy中/预扫描后换源，最终字节与Worker receipt决定结果，无错winner |
| `T14` | 是 | confirmed后重放旧PUT只能影响staging，不改winner |
| `T15` | 是 | GC/Delete后迟到PUT/Copy重现不恢复逻辑资格，tombstone与容量继续核对 |
| `T16` | 是 | Copy HTTP200含XML错误、超大错误体均不确认，控制内存有界 |
| `T17` | 是 | Copy响应丢失/dispatch后发包前kill/确认前kill/HEAD可见但unknown，不重写原target |
| `T18` | 是 | 旧lease写/旧fence提交/新winner后迟到操作不覆盖，未解决对象有账 |
| `T19` | 是 | kill/重启/网络分区恢复session/额度/任务，不无限pending |
| `T20` | 是 | cancel/revoke/hold/purge与winner/GC claim互斥，无已删对象成为winner |
| `T21` | 是 | URL/session到期但已开始上传，迟到写受控，不能只凭时间清账 |
| `T22` | 是 | 消息锚点、收藏/转发/群文件等重复绑定不重复引用 |
| `T23` | 是 | 退群/归档/撤权后重放已有GET/HEAD/Range地址，按冻结scope合同拒绝或保留明确窗口 |
| `T24` | 是 | 固定key/nonce/input新旧字节等价；末块/tag/full hash失败前无可见明文 |
| `T25` | 是 | 首传/断线/重启复用同密文文件；App恢复无整文件readAsBytes |
| `T26` | 是 | attempt变化不改logical key/descriptor；新发→旧收；读/删命中正确physical winner |
| `T27` | 是 | 相册批量逐项结果、上限、部分失败与重复调用，未confirm不可绑 |
| `T28` | 是 | Admin身份建会话/绑定头像，S3无Admin/Human凭据；处理/发布未过不匿名可读 |
| `T29` | 是 | Seat/Widget/App登录切换/AbortSignal/epoch隔离；业务Bearer仅同源允许路由，S3绝无 |
| `T30` | 是 | MIME/魔数/尾部控制字节/扫描后换对象；与现有规则样本等价且绑定winner字节 |
| `T31` | 是 | Worker超时/kill/伪造回执/跨attempt同size/过期ruleset全部fail-closed |
| `T32` | 是 | 企业撤权后下一请求拒绝，缓存不跨身份 |
| `T33` | 是 | 私有origin/internal鉴权/重定向/SSRF不可绕过，错误不泄漏存在性 |
| `T34` | 是 | GET/HEAD/Range/206/416/304/取消/鉴权状态转换/缓存隔离正确 |
| `T35` | 是 | staging/candidate未发布无匿名读；公开创建与业务许可顺序正确 |
| `T36` | 是 | 变换/公开复制响应丢失和DB失败可核对，不冒充跨DB/S3原子 |
| `T37` | 否：首轮不新增 MPU | MPU乱序/重复/缺失/跨session part、complete重试，服务端决定精确组合 |
| `T38` | 否：首轮不新增 MPU | composite独立字段；最终完整SHA由可信proof形成 |
| `T39` | 否：首轮不新增 MPU | abort/complete/迟到part、恢复及容量/孤儿收敛 |
| `T40` | 是 | driver/profile/凭据轮换过程中旧会话冻结身份，不改用默认bucket |
| `T41` | 是 | 未知历史/无GC proof/缺映射不猜bucket、不误删 |
| `T42` | 否：用户取消新 Disk 数据面 | Disk空间不足、路径穿越、符号链接/同名竞争失败关闭 |
| `T43` | 否：用户取消新 Disk 数据面 | Disk写入/hash/fsync/rename间kill与备份恢复，proof/receipt同字节 |
| `T44` | 是 | HEAD/Copy/XML/headers/ListParts错误有硬上限，拒绝时不先完整收体 |
| `T45` | 是 | 应用/边缘/Worker/客户端日志无signed URL、JWT、Cookie、key |
| `T46` | 是 | 多节点reservation/confirm未bind/cancel/unknown容量，不超卖/重复释放 |
| `T47` | 是 | profile能力缺失明确拒绝新grant，不静默退BEAM |
| `T48` | 是 | 新会话后停签发/回滚保留reader/reconciler/映射/intent，旧引用可读可删 |
| `T49` | 是 | 固定样本×并发，吞吐/尾延迟/普通消息延迟/各组件内存/GC恢复达事前阈值 |
| `T50` | 是 | 所选入口上传、confirm、Worker、下载、派生正文经BEAM=0，全部例外列明 |
| `E01` | 是 | 同一提示词首次运行两次：不创建第二控制目录、第二 run 或重复 worktree；已有状态不重置。 |
| `E02` | 是 | 任意卡修改文件后会话中断、尚未 commit：新会话在原 worktree 续作，完整保留 staged/unstaged/untracked。 |
| `E03` | 是 | 两个执行者同时更新同一状态 revision：只允许一方写入，另一方报冲突；不能相互覆盖进度。 |
| `E04` | 是 | 来源 main 前移但任务 worktree 未变：记录差异；不 reset/rebase/切分支，不默认全部历史结果仍有效。 |
| `E05` | 是 | 已 PASS 输入文件/锁文件/SQL/profile/权限变化：相关证据 INVALIDATED，依赖/最终门禁按影响失效。 |
| `E06` | 是 | 控制目录存在但 state 损坏/丢失、manifest 多份或 worktree 不存在：禁止初始化空进度与新建替代目录，保留并报告真实恢复阻塞。 |
| `E07` | 是 | Copy/外部进程在中断前已 dispatch：恢复先查资源账与外部状态，不重复未知写；HEAD 相同不授权晋升。 |
| `E08` | 是 | Garage/真机/浏览器占用或无授权：阻塞对应真实验收，继续无依赖的安全编码；不得挪用其他任务资源。 |
| `E09` | 是 | 最终候选与全部证据未变且已完成：同提示词再次运行只复核索引并返回，不制造重复修改或重跑全套。 |
| `E10` | 是 | credentials epoch/注销期间断点恢复：旧任务不能向新账号写回，不把 secret/cipher key 放普通 checkpoint。 |
| `E11` | 是 | 从较早计划迁移恢复：同 plan_family、原工作树与外部资源不变；ID映射保留；被取消的 Disk 卡不改为 PASS。 |
| `E12` | 是 | 停止点留有允许的后台测试进程：PID/启动时间/命令摘要/端口/资源 owner 可核实；不得只因 lease 时间到便抢锁或 kill。 |

### 门禁责任和安全时间线

| Gate | 验证责任/产出 | 失败裁决 |
|---|---|---|
| A0 | step-2：实际固定 Garage 版本/profile 原生 checksum、HEAD、Copy、权限与语义探测 | 无能力或无授权则阻塞真实合同/启用，不把它改成 mock PASS |
| A1 | step-5a：实际 IMBoy signer/driver/代理/客户端合同；T05～T12/T16/T44 等 | A0 SDK 通过不能替代；缺关键字节/权限证明不启用 direct |
| B | step-7a 加 step-20b：同字节、winner/GC、配额、Hold/引用、DML、外部写故障 | 两真实连接屏障+实对象；unknown 零晋升，错误对象零发布、合法对象不误删 |
| C | step-20c：完整链路 BEAM 正文为零及事前冻结的资源/SLO | 性能 FAIL 或不可测不能以“功能通过”豁免 |
| D | step-20b：全部选定 scope/认证面、真机/浏览器、新旧客户端、读删/回滚 | 缺设备、旧版本、服务或权限，相关 required 阻塞，不能降级 mock |
| E | step-0 和 step-20a/20b：E01～E12 的真实恢复演练 | 状态工具单测不能代替 worktree/外部 Copy/设备/活进程恢复 |

以下必须保存确定性时间线和 HTTP/SQL/对象三方 oracle：

- grant COMMIT 响应丢失：无已知提交就不签，按稳定身份调和后仅返回原结果；恢复查询失败不继续发能力。
- HEAD 后换源/Copy 中换源：只有最终 candidate 字节匹配 expected、closed 及所需 Worker receipt 才能确认。故障管理员注入高于普通 grant 的能力需明示，不声称普通客户端能绕 checksum。
- Copy 成功但响应丢失，或 dispatch 后发包前 kill：原 attempt 进入 unknown；即便 HEAD 匹配也零晋升，只可新 target 再试且旧对象继续记容量。
- GC 与 winner/新增引用竞争：相同锁序唯一胜者；GC claim/正式 intent 先提交则后续发布/引用拒绝，winner/引用先提交则相关清理不成立。
- Hold 与正式 intent 前后竞争：Hold 先建立则 intent 失败；intent 先提交则新 Hold 不假成功。不得以 direct candidate 路径绕过既有正式资产的 Hold。
- lease 过期后旧 worker 仍发 DELETE/Copy：物理身份不复用，旧 token 不 ACK 新租约、不替换 winner；迟到对象保留 tombstone/容量并有限调和，不因一次404销账。
- DELETE 成功 ACK 失败：既有 job 按精确身份幂等恢复；intent COMMIT 不明时先证明提交，不能先删；双队列同物理身份被约束拒绝。
- config/凭据/profile 变化：旧会话和任务解析其冻结 registry，不读当前默认 bucket；无法解析即阻塞。新 upload 不复用旧物理 key，延迟旧 job 不可能删新 winner。
- 直接 DML/旁路 writer：业务角色不能伪造 mapping/proof/winner、向已删除身份绑引用或清空预算；SQL 约束/触发器/ACL 负例与全部 writer 扫描共同证明。

### 受影响的 V5 回归

新模型、reader、绑定、队列函数或权限改变后，原 G6 只证明旧候选。本阶段按 `source_case_ref/source_scenario` 登记并在新候选重跑受影响的 V5 场景：登记/确认幂等、引用/GC 两个顺序、48个 Hold 与24个 Retention 入口组合、两个队列与跨队列身份、DELETE 后 ACK 失败/未知 COMMIT、真实120秒 lease+reaper、配置切换及 DML 负例。不能以 Direct 的单个 T20 通过替代整个受影响矩阵。

本阶段不新增 Disk 数据面；凡共享 driver/registry/SQL/权限变更影响既有 Disk 读写删除和 V5 安全协议，仍须重验对应 Disk 场景。证据归本阶段新候选与回归集合，保留原 V5 旧候选/旧证据，不回填或篡改在途/已封存 V5 state。只有证明无影响的旧叶可引用已有结果，需记录输入相等的依据。

## 8. 性能、流量和资源验收

样本固定为 10000/100000/1000000/25000000/100000000 bytes，补普通/EB MiB 边界与 E2EE tag 开销；并发 1/4，达到现场授权后才可 16。准备、hash、加密、PUT、Copy、proof、规则、bind、下载分别计时；旧对照必须含 spool+confirm 全文 GET，不能只取旧路径一半。

step-1/3 建 resource_manifest：机器/运行时/镜像/digest/配置/网络、字节和费用上限、并发、正常消息 p95/p99 允许退化、吞吐/错误率/恢复 SLO、BEAM/客户端/Worker/边缘/Garage RSS及 binary/ETS、临时盘/unknown 容量、采样周期和统计最小样本数。阈值采用实际业务与容量依据，在压测前冻结；没有数值或观测方法则 C 不启动，不在测试后按结果改线。

T50 要求所选新 direct 上传、confirm、下载、内容规则和派生的大对象正文经 BEAM 为零；source-sink 静态路径与按 request_id/字节数关联的真实流量观测同时证明。小型控制响应单列预算，范围外旧流量单列，不以 grep=0 或 HTTP200 代替。采集各组件独立资源与正常消息尾延迟，不能把压力转到同宿主机 Worker/Garage 后宣称系统资源为零；不预填95%节省或64KiB峰值。

## 9. 持久执行、并行、恢复及验收证据

- 同计划族仅一位代码写协调器；原 V5 run 与本阶段写集互斥。优先复用现有 Direct 控制根 `.task-state/imboy-garage-direct-v1`，先检查身份/状态/历史 worktree；损坏、丢失或多份状态记 BLOCKED_RECOVERY，禁止新建空进度。确无历史执行才在明确授权下初始化隔离 worktree，不复制用户未提交 WIP。
- state 原子替换、revision CAS、单 writer、历史/事件和 checkpoint；运行锁包含 host、PID、启动时间、命令摘要、写集和资源，先验证实际 holder。lease 到期不代表进程已停，不凭 PID 或超时抢锁/kill 未知进程。产品 session/attempt 真源仍是业务 DB，计划 state 不可生成业务 proof。
- 默认产品代码串行；只读盘点与真正独占的测试可并行。step-3 协议/接口冻结后，可先并行只读审查；若实施授权允许多 worker，主协调器先登记精确互斥文件/fixture，不能把同仓不同卡自动当作无冲突。SQL/router/signer/协议/lockfile/最终集成始终单 writer。共享构建/BEAM/DB/端口/SDK/真机资源不未经证明并行。
- 每卡持久保存执行总尝试、修复轮次、基础设施重试和命令/task/run deadline；同一故障签名默认最多3次有新观察/修改的修复重验。开跑前计次，崩溃不豁免，新会话/run_id 不清零；有新证据后按授权增量预算。外部 Copy dispatch/session 写尝试预算独立持久化；既有物理 DELETE job 的8次 claim 合同继续执行，三者不得混用。
- step-0/1 在调度前根据实际叶集合、槽冲突与准备成本冻结每命令超时、task/run 上限及必要重验余量；不得使用无限轮询/无限等待。blocked 只有解锁条件真实变化才重查。缺资源继续无依赖且获准的 READY，但最终 required 不能跳过。
- 每叶记录：case/子叶、真实命令数组/cwd/start/end/raw exit、故障点、seed、HTTP/DB/对象 oracle、三仓 HEAD/tree/owned/untracked 内容摘要、计划/SQL/profile/权限/ruleset/工具与设备 provenance、原始输出摘要、资源 owner 和清理结果。禁止秘密/真实用户数据入库或日志；状态工具自测不是产品验收。
- 证据验证器必须拒绝缺叶/重复叶/0执行/required skip、假日志、错误候选、错设备/服务 provenance、未收敛资源或跨仓部分成功；使用变异自测验证拒绝路径。没有真实 harness 的叶是 MISSING_HARNESS，由所属卡交付后执行，不能算 PASS。
- 输入、writer、SQL/ACL、锁文件、profile、内容规则或候选变化，按明确影响集合 INVALIDATED 并传播依赖；影响不明保守重验。保留历史证据而不冒充当前 PASS。最终由主集成者在统一冻结三仓候选执行 Backend 适用全量/专门门、Flutter test/analyze/SQLite、Admin test/lint/build/Widget/Seat 与本阶段 A0/A1/B/C/D/E；修复后必要重验，没有“一次全量后永不重跑”的豁免。
- 停止前记录活跃 task、WIP/暂存/未跟踪、依赖 cursor、累计预算、未知外部操作、活进程与恢复点；固定提示词只保证下次从同状态恢复，不保证宿主会话结束后仍有进程继续工作。

## 10. 完成、清理与交接

只有 24 核心卡及57 required 编号展开的全部精确叶，在相同有效候选下达到各自证据要求，且 A0/A1/B/C/D/E、最终独立审查和资源核对均通过，才可报告 `DIRECT_LOCAL_CANDIDATE_PASS`。本地代码已完成但真实环境缺失只能 `LOCAL_CODE_VALIDATED / BLOCKED`；不借 V5 G6、历史日志或绿灯数量升级结论。

发布方案必须包含 scope/版本、停新 grant、旧 URL/session 与在途操作排空、持续 reader/reconciler、回滚保留数据、容量阈值和停止条件。step-22 只产出方案；UAT/发布/生产仍分别验收授权。9.9 是质量目标，本文未获独立评分，不承诺分数。

测试清理只针对本 run 资源账中创建且获准删除的精确合成对象/库/目录。unknown 写未证明终止时不全桶 purge、不用“清理完成”隐藏迟到对象，不删除必要墓碑/证据。合并、删分支或移除 worktree 需另行明确授权且先证实提交已完整集成；外部 WIP 始终保护。

给 ZCODE 的后续使用入口（须由用户另行授予实施范围，文件本身不是实施 receipt）：

> 原 V5.5 完成后，读取本文件与独立 checksum，先核验原 G6 当前候选、在途写者和现有 Direct 状态/源码，按准入条款建立唯一后续基线及资源授权。沿24个 step-* 任务恢复或推进，先复用现场已存在能力，不改原 V5 冻结合同或台账。只按真实授权实施/提交；缺服务/设备/权限时诚实有限阻断，任何 required 不降级。新 direct 的全部 A0/A1/B/C/D/E 证据绑定最终候选，收口上限为 DIRECT_LOCAL_CANDIDATE_PASS。
