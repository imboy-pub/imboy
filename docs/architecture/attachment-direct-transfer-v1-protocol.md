# direct-v1 附件直传协议合同（冻结版）

状态：FROZEN_FOR_IMPLEMENTATION（step-3 交付）。计划族 `imboy-garage-direct-v1`。
基线：backend a2fe60cd / flutter c61cd8d46 / admin 74af973。
本文把 TASKS.md（V2.3 standalone-1）A 部分安全设计映射到三仓真实 Schema、API、锁序与错误码。
任何实现与本文冲突处，先修订本文（ADR）再改码；不得默默放宽。

## 0. 范围与并存规则

- Garage 是唯一新增存储目标；Disk 只保留历史 reader/intent 兼容。
- `direct-v1` 是**新会话协议**：客户端在创建会话时显式声明 `protocol=direct-v1`；未声明者保持 V5 语义不变（旧字段、旧枚举、旧 URL 含义均不原地改义）。
- 已创建 direct 会话后不静默回退 BEAM 上传路径（I14）；失败只能重试 direct 或显式取消后另建会话。
- 协议版本固定到会话：`attach_pending.upload_registration->'protocol' = "direct-v1"`，会话存续期内不可变。
- 本轮不启用：MPU（T37–T39 范围外）、新 Disk 数据面（T42/T43 用户取消）。墨芽已迁出独立产品，旧教学 scope 不再提供上传或读取接口；历史媒体保持保护，迁移边界见 REST API 目录 D2。

## 1. 会话状态机（映射真实 Schema）

### 1.1 载体

- 主载体仍是 `attach_pending`（object_key 主键）；新增可空列 `direct_state text`（仅 direct 会话非 NULL），旧 `session_state` 枚举（registered/confirmed/rejected/expired/delete_intent/deleted，迁移 178 ck_v5_pending_state）继续作为兼容读与既有 intent 函数的输入，不改变其现有转换语义。
- `direct_state` 与 `session_state` 的联动由 CHECK 约束保证（见 §1.3），二者在 direct 会话上由同一受限函数族推进。
- 新增最小子表 `attach_direct_attempt`（每 attempt 一行）承载多次外部副作用；业务真源仍在 attachment/attach_pending，attempt 表只做副作用账本，不进只能删除的队列。

### 1.2 direct_state 枚举与转换

| direct_state | 允许后继 | 持久化条件（进入该态前原子完成） | session_state 同步 |
|---|---|---|---|
| RESERVED | DECLARED / EXPIRED / CANCELLED | register 仅登记身份（subject/object_key/scope/protocol）与预算，**无任何写能力、无 put_url、无 capability** | registered |
| DECLARED | GRANTED / EXPIRED / CANCELLED | expected（sha256/size/mime）版本号原子冻结 + reservation 记账 COMMIT | registered |
| GRANTED | FINALIZING / EXPIRED / CANCELLED | grant 签发内容绑定 staging URL（先有 COMMIT 后签名，I02）；renew 可续 grant 但不延长会话总 deadline（7200s，沿 V5 常量） | registered |
| FINALIZING | CONFIRMED / FAILED / CANCELLED | 至少一个 attempt 到达 PROOF_VERIFIED 后的原子复核事务入口 | registered |
| CONFIRMED | （终态，进入业务生命周期） | winner+mapping+actual pin+confirm 同事务可见（I03：expected 与 actual proof 分开） | confirmed |
| FAILED / EXPIRED / CANCELLED | delete_intent（走既有 fn_v5_intent_pending_delete 模式 reject/sweep） | 失败原因与 attempts 账本保留 | rejected/expired |

### 1.3 attempt 状态机（attach_direct_attempt）

| attempt_state | 允许后继 | 关键约束 |
|---|---|---|
| CREATED | DISPATCHED / ABANDONED | 精确 source/target key、attempt_no、profile 指纹**在发包前**持久化；单 CAS 写授权（UPDATE ... WHERE attempt_state='CREATED'） |
| DISPATCHED | COPY_COMPLETED / UNKNOWN_QUARANTINED | 仅当收到完整、有界、profile 语义明确的成功响应才置 COPY_COMPLETED；超时/响应丢失/kill 一律 UNKNOWN_QUARANTINED |
| COPY_COMPLETED | PROOF_VERIFIED / REJECTED | candidate full hash/size（真 HEAD 控制接口）；规则需要时附 Worker 同字节 receipt |
| UNKNOWN_QUARANTINED | （只读探测 / 隔离 GC） | **本轮无晋升边**：HEAD 可见、hash 相同、重复读一致、lease 过期均不得转 PROOF_VERIFIED；需要新写 = 新 attempt + 新 target |
| PROOF_VERIFIED | WINNER / GC_CLAIMED / REJECTED | winner 前短事务复核：当前授权/session/expected 版本/deadline/quota/hold/purge（§4 锁序） |
| WINNER / GC_CLAIMED / REJECTED / ABANDONED | 终态 | winner CAS 与 GC claim 同锁序互斥 |

预算冻结（实现时写入 resources.json 并在 grant 前校验）：每 session 最大写 attempt 数 = 3；每 subject 并发 direct 会话上限 = 4；UNKNOWN_QUARANTINED 累计字节与龄期阈值按 profile 登记，达阈值暂停该 subject 新 grant 并告警（不无限创建对象换成功）。

### 1.4 与旧 V5 分派隔离

direct 会话的 confirm/复核**禁止**进入 `verify_and_save_v5 → v5_object_bytes → read_snapshot`（全量 GET 链，attach_logic.erl:336/351）；新分派在 attach_logic 入口按 `upload_registration->protocol` 判别。V5 旧会话与旧路由原样保留至排空。

## 2. API 面（HTTP 冻结）

时间单位：全部秒；时间戳 RFC3339 UTC。JSON 中 64-bit ID 沿用现有 integer 传输（客户端 `safeParseBigIntJson` 约定不变，TSID/EntityId 规则不变）。

信封沿用现有 HTTP200 业务信封（`{code, ...}`）；**禁止把现有 200 端点改成 202**。处理中状态用信封内 `state` 表达，三端 parser 必须识别：

| 端点 | 方法 | 语义 | 成功响应（信封） |
|---|---|---|---|
| /api/v1/attachment/direct/register | POST | 无写能力登记：subject、scope、object_key、protocol、预算声明 | code=0，`{session_key, state:"RESERVED", expires_at}`（无 put_url/capability 字段） |
| /api/v1/attachment/direct/declare | POST | 冻结 expected（sha256/size/mime/version） | code=0，`{state:"DECLARED", expected_version}`；重复声明同值幂等（T02），异值 40903 拒绝（T03） |
| /api/v1/attachment/direct/grant | POST | COMMIT 后返回 staging s3_put 描述符 | code=0，`{state:"GRANTED", grant:{url, method:"PUT", required_headers, expires_at, attempt_id}}`；不可续总会话 deadline |
| /api/v1/attachment/direct/status | GET | 会话/attempt 查询（恢复入口） | code=0，`{state, attempts:[{attempt_id,state,target_digest}], next_action}`；跨主体/实例查询 403 信封不泄露存在性 |
| /api/v1/attachment/direct/renew | POST | 仅 GRANTED 态续 grant（同一声明） | code=0，新 expires_at ≤ 会话 deadline |
| /api/v1/attachment/direct/finalize | POST | 触发单次 Copy dispatch + 复核 | code=0，`{state:"FINALIZING", attempt_id}`；重复调用同 attempt 幂等（每 attempt 恰一次 dispatch） |
| （现有）/api/v1/attachment/confirm | POST | direct 会话经此进入 CONFIRMED（复用入口，内部分派） | 与 V5 相同形状 + `state:"CONFIRMED"` |
| （现有）/api/v1/attachment/presign?operation=status/renew | GET | V5 旧语义不变 | 不变 |

错误信封（全认证面统一形状，具体码见 §5 各面冻结表）：
`{code, state, retry_action}`，retry_action ∈ `retry_session | retry_attempt | poll | abort`。
FINALIZING 期间消息不可发送：App/Admin/Seat/Widget journal 显示"服务端封存中"，不得把未 CONFIRMED 附件发出（5.1）。

## 3. 传输类型与 required_headers（不可猜测）

传输描述符三型（descriptor 字段显式声明，禁止按 URL 外观判定）：

1. `business_gateway`：同源业务端点（如 EB `/api/v1/enterprise/organizations/:org/assets/presign`、Seat `/api/v1/seat/enterprise/...`）——**保留现有同源 Bearer/upload_ref 合同**（seatApiClient.ts:266-274 与 cs_asset_put_transport putMember:46 的现状即本型基准），凭据仅送本实例允许路由（R05 现状锁定，不放宽也不误删）。
2. `s3_put`：存储 staging 直传——**仅**发送 profile 允许集合内的 required_headers（Content-Type、x-amz-checksum-sha256）；绝不携带 IMBoy/Seat/Admin JWT、Cookie、业务 Authorization。
3. `edge_content`：内容下载——使用原业务读凭据（§7）。

s3_put presign 合同（step-5 实现，此处冻结语义）：
- 签名 headers 至少绑定 `host;content-type;x-amz-checksum-sha256`（修复 G01 现状 host-only）。
- `x-amz-checksum-sha256` = 原始 32 字节摘要的 **Base64**（非 hex，I04）；PUT 缺头、换头、换正文均被存储端拒绝或最终不可确认（T05/T06）。
- 不允许客户端在 required_headers 之外追加 `x-amz-*`、copy-source、ACL 类头（T07）；presign 不支持的方法不签。
- grant 的 URL 绑定唯一 attempt 的 staging target；新 attempt 必然新 target key（I05），客户端无 sealed 写/删能力。

## 4. SQL 锁序、错误码与迁移

- 新 SQL 沿用现有受限函数族（fn_v5_* SECURITY DEFINER）+ 短事务；网络 I/O 绝不在持锁事务内（I10）。
- 锁序冻结（与迁移 196 一致，不得 reorder）：workspace 行 → `fn_v5_lock_identity(driver,ns,container,key)` → `pg_advisory_xact_lock(hashtextextended(key,168))` → attach_pending FOR UPDATE → attach_direct_attempt FOR UPDATE（按 attempt_no）→ attachment FOR UPDATE（按 id）。
- winner CAS 与 candidate GC claim 使用同锁序互斥：GC claim 先提交则该 candidate 永不发布；winner 先提交则 GC claim 拒绝。
- 错误码沿现有族：23510 非法状态转换 / 23505 tombstone 已存在 / P0002 未知会话 / 40001 资格竞态丢失 / 23514 未知模式；新增信封码：40903 expected 冲突、40904 attempt 预算耗尽、42302 会话 deadline 已过（映射 retry_action=abort/new session）。
- 迁移号预约：**00000200 起**（197/198 已占；199 留空缓冲）。新增列/表见 step-4 卡（本文只冻结语义，不落 DDL）。

## 5. 逐认证面合同冻结

| 认证面 | 传输型 | 上限 | 处理中/失败表达 | 撤权语义 |
|---|---|---|---|---|
| ordinary（App 用户 Bearer） | s3_put + edge_content | 104857600 B | FINALIZING 信封 state；journal"封存中" | scope 政策 §7 |
| group file/album（成员） | s3_put + 确认后引用绑定 | 同 ordinary（step-11 收敛 multipart） | 同上 | 逐请求成员资格复核（复用现有 ACL） |
| EB Member/Platform（business_gateway presign + upload_ref） | business_gateway 字节端点（现状保留）→ direct 化后 s3_put | 26214400 B | 同上 | 实例凭证 scope（现状） |
| Seat（console 同源 Bearer） | business_gateway（仅同源白名单路径） | 25MiB | 同上 | 同 EB |
| Visitor/Widget（upload_ref 无登录态） | business_gateway | 25MiB + 魔数/文本尾部规则（Worker） | durable journal 状态机 | upload_ref 单次性（现状） |
| Admin AI 头像（Admin 身份域） | s3_put（新，替代 FormData 正文） | 图片策略预算 | 处理中不可引用 | 发布前匿名不可读（许可后公开复制） |
| internal application | edge_content（原 content 路由保留 + 逐请求授权适配） | — | — | 撤权后下一请求拒绝 |

所有面共通：业务 Bearer 仅送本实例允许路由；`Content-Length` 不可由浏览器脚本自定义（T11）；native fixed-length streaming 验证归 step-8/20b。

## 6. 逻辑/物理位置与 candidate 映射（语义冻结，DDL 归 step-4）

- logical object_key 保持 `u<Uid>/...` 与 E2EE descriptor 格式（I09/5.2），加密前固定，attempt 变化不改。
- candidate 物理 key：`direct-c/<session_key>/<attempt_no>/<rand>` 内部前缀，**每 attempt 唯一**；最终 winner 位置 = 位置三元组（driver/namespace/container）+ physical_key + locator_digest（winner 版本）。
- 映射行确认时与 winner 原子产生；所有 direct 新行**缺映射失败关闭**（解析不到 winner = 拒绝读取，不猜默认 driver/bucket，T41）。
- 删除：元数据删除事务必须先捕获完整物理身份到 intent（attachment_object_deletion 现有 tombstone 机制扩展 physical winner 列）；主行删除后不得再查映射决定删哪个对象。

## 7. 下载与撤权窗口（scope 政策冻结）

- group/channel/moment + 企业/Seat/Visitor content：**逐请求 edge_content**（业务 ACL 复核，正文不进 BEAM）。
- private/c2c：**有限授权窗口**短 GET（与签名寿命一致，600s；客户端缓存 540s 现状即为此设计），窗口内不追溯撤销（如实描述，不强称强撤权）。
- 历史已发 S3 GET URL：到期自然排空（最长 600s+余量），不宣称可主动召回；已下载字节不可召回。
- staging/candidate 在 CONFIRMED 前**无任何面向用户的读取或匿名面**（I13）。
- edge 的 auth_request 只保证请求前检查，不中断在途传输；HEAD/GET/Range/206/416/304 映射与 SSRF allowlist 归 step-10/15 实现验证。

## 8. 恢复、兼容与回滚

- 客户端恢复：status 查询稳定身份（session_key/object_key）→ 幂等重建本地任务；禁止凭缓存 URL 重放已失效 grant（T14：重放只可能命中 staging，不改 winner）。
- 服务端恢复：进程重启后 UNKNOWN_QUARANTINED 保持隔离；FINALIZING 会话由 reconcile 任务续走（复核 attempts 账本），不重复 dispatch。
- 回滚顺序：停新 grant（新 register 返回 unsupported）→ 记录最大已发期限/活跃会话/在途 attempt → 排空 → 保留 reader/mapping/restricted 函数/quota/GC 账/reconciler（有新数据不破坏性 down）。
- 兼容 Schema/reader 先行，后启用新会话（10）；新客户端连旧服务端：创建会话前协商失败即走 V5，不混用。

## 9. 未决项（阻塞启用，不阻塞编码）

| 项 | 阻塞的门 | 归属 |
|---|---|---|
| Garage v2.4.1 真 HEAD/Copy/checksum-PUT/错误体语义实测 | step-2 A0 → step-5a A1 → 启用 | 资源未获准（resources.json REQUESTED_NOT_GRANTED） |
| 反代（nginx/caddy）对真 HEAD/PUT checksum 头的透传行为 | step-5a | 同上 |
| Worker 与 Garage 之间的最短时能力签发方式（bucket 级 key 的实际粒度） | step-12 | A0 后定 |
| 16 并发压测授权与预算数值 | step-20c | 用户冻结 resource_manifest |

以上未决项在 A0 落地前：相关代码可写、定向测试可用 fake driver；**不得启用**任何 direct 会话（T47）。

## 10. 验收映射

本文条款 ↔ C 部分：§1.2/1.3 ↔ T01/T02/T03/T17/T18/T19；§3 ↔ T05/T06/T07/T11/T29；§4 ↔ T20/T46；§6 ↔ T26/T41/T48；§7 ↔ T23/T32/T33/T34；§5 ↔ T04/T27/T28/T30。实现卡（step-4…15）逐条引用本文小节作为输入指纹。
