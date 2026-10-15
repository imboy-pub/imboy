# C06 收口 — W1 标记 Registry 尾巴项逐项核查（registry-w1-closeout）

> task_id: CACHE_V221_BACKEND_CLOSEOUT_V1 / C06 尾巴核查
> 生成: 2026-10-12（Asia/Shanghai）
> 核查基线: imboy main `675b0744`（工作树含未提交的 sync 系列修改，本文件引用行号以核查时实际读取的工作树为准；引用的 `imboy_cache_sync.erl` 行号来自当前工作树版本）

## 当前实现增量核验（2026-10-13，候选尚未冻结）

以下原审计源码及结论保留为历史记录，不能继承为当前候选 PASS。

- group：当前 `join/leave` 已改为 invalidate-on-change，不再用旧缓存读改写；
  `group_member_ds:join_group/5` 已登记 `elib_pg:after_commit`，动作须在实际
  COMMIT CommandComplete 确认、连接归还后运行，ROLLBACK 不执行。
  原“BLOCKED”描述的实现缺口已修复；真 PG 事务与跨节点业务验收仍须绑定最终候选。
- tag：fill/flush 键已对齐，但再次追踪发现 `user_tag_ds:delete/change_name`
  原来忽略事务返回，回滚仍失效；logic 的事务外预删除还会先自动提交关系行。
  当前删除/改名在同一事务登记提交后副标题失效，直接返回事务错误；去除重复
  预删除，Repo 更新错误透传并 abort，API 删除/改名不再把回滚当 success。
  `user_tag_commit_tests` 使用真实 elib_pg 提交动作队列与 mocked PG driver，
  验证写后 deferred、COMMIT/归还连接后失效、silent ROLLBACK 不失效和更新失败
  abort；这不是实际 PG COMMIT/跨节点证据。定向 DS/logic/API 共 46 测试、
  Repo 更新三场景共 3 测试通过（exit 0，Failed/Skipped/取消均 0）。
- Registry `auth_session_epoch`：`bump/bump_in_tx` 已统一登记提交后 evict，
  实际 PG 定向三场景通过：COMMIT 后归还连接再失效、显式 abort 回滚不失效、
  SQL 异常使真实 COMMIT 返回 ROLLBACK 时拒绝动作；epoch 回滚后保持 2。
  实际表为 public.user_auth_epoch，唯一合成大 UID fixture 删除后查询残留为 0。
  数据库驱动/事务为真实 PG；失效端是观察替身，不证明跨节点读。
  本次可执行入口为仓外 /tmp/cache_c06_pg_probe.erl，exit 0 / cases=3 /
  Failed=0 / Skipped=0；最终候选冻结后仍须正式证据绑定及跨节点业务验证。
- `moment_acl` 的唯一写入口是 create_post 的事务内 `replace_for_post`；未发现
  既有 ACL 编辑/吊销入口。但是 `list_uids_by_post` 把 DB 错误变成空列表，
  visibility=4 的 deny-list 判定因此可能放行。当前增加 `list_for_post/1` 一次
  SQL 完整 ACL 读取，DB/畸形行错误不转空集合，loader 使用零 TTL error，
  visibility 3/4 不确定即拒绝；constructor 登记实际 COMMIT 后失效。
  定向验证使用真实 depcache、真实提交动作队列与 mock PG driver：完整空 ACL
  与失败可区分，DB 恢复后重新读取，错误不驻留；此处不是实际 PG/分区验收。
- config5 已改 memo loader：启用 fenced 时保留原 TTL 864000；未启用时
  loader 返回零 TTL 值，使用 `{config5, Key, uncached_v1}` 合并等待但不保留
  派生值，避免旧实现遗留 raw 值继续命中。写入先以同一 pgcrypto 算法生成
  最终密文，再一次实际 insert/update；写/加密失败不广播、不落明文中间态，
  set 以安全异常终止忽略返回值的调用方，save/aes 错误透传。
  受控旧 loader 横跨 writer 提交（fenced 与未 fenced）、遗留 raw ghost、
  authority 写失败、加密失败、主要直接调用方异常传播和入口单次采样 Fenced
  共 7 场景通过（exit 0）。末项只证明入口已选非 fenced loader 仍零 TTL；
  C12 冻结后不得改变开关，不代表任意生产热切换已验收。
  实际 PG 专用 public.config 合成 key 的新增/更新密文往返、约束失败原密文
  不变、NULL key 加密失败不写库共 4 场景通过（exit 0），DELETE 后残留 0。
  直接调用方定向 80 测试 exit 0（config、onboarding、知识库、角色、版本及
  agent 角色缓存路径）；首个广 scope run 将 agent 无关真实 TX 六项拉入，
  因隔离 runner 缺 pg_conf 取消，原 exit 1 保留，不能以定向 80 替代该整套。
  一次写入只采样一次加密密钥；运行时主密钥轮换及生产性能不是该证据范围。
- billing_meter：全部生产订阅 mutation 引用落在 billing_subscription_ds 的
  create/update/renew（Repo 无 Conn 单语句自动提交）；成功且 Id/Count>0 后
  失效当前部署级 `{billing_meter,active_sub,0}`，error/Count0 保留返回且不失效。
- moderation_sensitive_words：全部增删词 mutation 引用落在 moderation_ds，
  created/delete Count>0 的单语句自动提交成功后失效；error/重复 skipped/Count0
  不触发。八个新增 hook 断言及直接 meter/policy/DS 回归通过。
  hooks、既有直接回归与安全 fixture 环境恢复的组合共 29 场景 exit 0；其中
  权威 Repo 使用 mock，不能替代最终实际 PG 跨节点业务验收。

- CSRF：原 raw binary Key 不能映射 Registry local，已将生产/消费/失效三处
  统一为 `{local_cache,csrf,Token}`；前端 token 字符串不变。真实 depcache/
  facade 验证启用 dsync 后不调用广播，完整 handler 与安全队列共 30 场景
  exit 0。旧页面 token 需刷新取得新本地键；不兼容回读旧 raw Key。
- adm_session_epoch：bump 事务内登记实际 COMMIT 后动作，不再只凭业务体 ok
  判断提交；真实队列/mock driver 的 silent ROLLBACK 负例与 handler 回归通过。
  当前无 bump_in_tx 生产入口，未新增不需要的 API；实际 PG admin fixture 门禁
  仍待最终候选执行，不能继承用户 epoch 的 PG 证据。
- 好友 confirm/pending insert/delete 与 DND save/delete 的 Repo PG 错误不再
  伪装 ok；denylist add/remove 和会话 restore 的错误在业务失效之前安全异常
  终止，避免忽略 tuple 的直接调用方继续返回成功/发送通知。八个失败爆炸
  哨兵负例先实际失败再通过，加成功写入/直接恢复调用方及 DND/denylist 旧
  定向共 33 场景 exit 0；不签发多步好友写入的事务原子性。
  首个含 friend_ds 全套的广 scope run 因隔离 runner 无 pg_conf 取消，原 exit 1
  保留；定向不代替最终真实 PG 及完整套件。
- Tag 追加：find_tag_id 已改 checked pluck，查询失败直接传播，delete 不再把
  DB 错误当标签不存在/成功；关联对象标签 UPDATE 增加参数化 owner 限定，
  收藏以 user_id、好友以 from_user_id 为范围，不改其他用户同名标签。
  新失败 oracle 修复前两项 FAIL，修后真实队列/mock driver 共 9 PASS。
- DND 追加：getter 移入 memo loader；fenced 值用原 TTL3600，空结果或
  未启用 fenced 时零 TTL；非 fenced 新 key 绕旧 raw ghost。保持 Repo 既有
  map 合同，不把空偏好解释为权限拒绝。旧 loader 横跨失效、空结果恢复及
  非 fenced ghost 三项真实 depcache 回归 PASS；Tag/DND 与直接 API/logic
  旧测试 51 PASS，均 exit 0。上述定向未执行实际 PG，不是最终候选门禁。
- 权限冷读：denylist Repo 使用 checked pluck，DB/畸形结果仅以明确的
  denylist_read_unavailable 安全异常传播；logic 仅捕获该类型并零 TTL 拒绝，
  moment/WebRTC 拒绝。relationship SQL 非完整布尔行或失败返回零 TTL
  {false,true}，pending、转发、C2C 消息及回执直接调用方不放行。
  冷读、恢复及直接调用方共 59 场景 exit 0；不证明旧允许热缓存的分区吊销，
  该 oracle 依赖核心健康门控及最终三节点候选。
- user_device_active/name：删除 Repo PG 错误以安全异常在 Olm 清理、失效、
  kick 之前中止；改名检查实际 UPDATE 结果，失败不失效或假成功，成功返回
  后失效。删除/改名直接调用方共 4 场景 exit 0；Count0 保持原幂等成功语义。
  此处驱动为 mock，没有签发设备操作实际 PG 或跨节点吊销。
- channel_subs：subscribe/unsubscribe/remove subscriber 均在最后业务 SQL 后
  登记提交动作，实际 COMMIT 确认后失效，silent ROLLBACK 不触发。
  subscriber_uids 权威读取进入 memo loader，错误零 TTL 空列表，旧 loader
  横跨失效返回不得驻留；新 getter 两场景 exit 0。真实 elib_pg 队列/mock
  driver 与原 DS/remove 调用方组合 30 场景 exit 0（其中包括设备删除两项）；
  不替代实际 PG channel 事务或三节点受众内容验收。
- 47 个原 Registry owner 路径均存在；account_list 已转专用 allocator，
  tag_subtitle_3 已退役改为 2。运行时 hrl 实际为 48 个 map 项，包含
  local_cache 别名及额外 user_tag_relation 条目；原 47 项是逻辑投影而非
  活跃生产键数量，不能把 map 项数当原计划逻辑条数；CSRF 无独立 hrl 项，由
  实际 local_cache 键提供，原冻结快照不改写。当前源码/属性需由 C12 统一绑定。

实际 PG 业务 fixture 补验证（仓外 `/tmp/cache_c06_business_pg_probe.erl`）：
两条独立真实连接，合成 UID/id/pair，动作及外部通知观察替身；未改变 schema。
首轮 DND smallint 越界在客户端 codec 中断专用连接，exit 1，未签发 PASS。
第二轮使用服务器 NOT NULL 约束负例，先回收旧 fixture 并查询残留 0；Device
改名/失败不失效/删除、denylist 增删、DND 保存/失败不失效/删除、Tag 改名/
真实 SQL 异常 silent ROLLBACK/删除/两个合成 owner 同名标签不串写，共 12
场景执行至断言成功。随后 channel subscribe 返回 error，整轮 exit 1；未以
前段定向代替整轮通过。finally 通过健康读取连接逐项回收八表，查询残留 0。
Channel 独立诊断确认 SQLSTATE23503：现有 fk_channel_subscription_user
要求 public."user" fixture，前 probe 未创建该行；这是 fixture 前置缺失，
不是提交钩子的产品失败。诊断 exit 1，连接归还、清理查询残留 0 有明确日志。
后续独立 Channel probe 已补仅合成 disabled user（status0、email/mobile NULL）
及其纯 DB fts_user 触发器依赖；首个订正轮错误期待软删除后总行0，exit1，
保留原日志。按真实 Repo 合同验证 active0、总行1/status0后，四场景实际
PG exit0 / Failed0 / Skipped0：subscribe COMMIT 后失效、SQL1/0使真实
COMMIT→ROLLBACK 不失效、unsubscribe、remove_subscriber。缓存/通知为观察
替身；连接归还后动作、subscriber_count0均有断言，最终物理清理订阅/
channel/user，user/fts及全部fixture残留0。原综合probe仍exit1，不改写为
整轮 PASS；该独立定向不证明跨节点投递或最终候选绑定。日志及失败轮次
保留，源码未因 PG 准备改变。

敏感读取追加收口：auth/admin current_epoch、user_device is_active、billing
active_sub_id、group e2ee_mode 原来在 memo/set 之前已完成权威读取，旧值
可于提交后失效后写进新代次。现五条权威读取均移到实际 memo loader 内；
epoch DB error、设备 false/error、E2EE error/畸形和订阅未知/无订阅不驻留。
缺 epoch 行=1、缓存故障时 authority fallback 与 soft billing 行为保留。
Group E2EE 成功缓存仍是原整数0/1，外层返回{ok,Mode}，避免已有 raw热值
返回裸整数违反 API。原 member_uids_legacy 在当前 policy 下只可能
`dsync=false` 且未显式 fenced；dsync=true 敏感 invalidate 自动 fenced。
B2 join/leave 产品债未在本增量扩大。

受控 loader 先捕获旧 epoch/活跃设备/SubId/Mode0，再失效、恢复，第一读与
后续读必须为新值；五个 race 和五个 error不驻留/恢复场景、旧 E2EE raw
热值合同共 11 PASS、exit0。auth revoked、admin cookie verify、device logic
及 billing do_meter 直接路径包含在 oracle；旧直接回归组合78 PASS、exit0
（最后 Group 返回形状小改后由 native11重验）。这不是最终 core候选绑定。
初始 billing测试误调私有API的undef、Group旧raw形状负例及 broad group
suite隔离fixture缺pool/meta ETS等失败日志均保留；最后广scope实际
104 PASS/13 FAIL、exit1，定向不能替代最终整轮门禁。

本增量核验不能签发 C06 PASS。W0 Registry 是原计划冻结快照，不在此轮重写；
最终台账必须结合当前实现逐项绑定，不能直接继承旧 TRACED_COMMIT_AFTER_OK。

## 0. 尾巴项定位与方法

- 台账面（ledger-v1.json / review-ledger-v1.md）：W1-01..W1-07 无独立尾巴 ID；
  W1-03/W1-05 为 REVERIFY（closeout_task C12/C14）。W1 尾巴项的权威定义在
  W0 产物 `docs/architecture/cache-v2.2.1/w0/key-registry.json` 的
  `mutation_hook` 字段：`W1_KNOWN_DEFECT` / `SECRET_SUBITEM_REVIEW_W1` /
  `LOCAL_SELF_MANAGED_BROADCAST_W1` / `TRACED_IN_TX_W1` 四类标记，
  即 C06 重扫时逐 namespace 冻结的遗留处置点。
- 本文件对应用户指定清单 5 项：tag_subtitle_2/3 键失配、config5 双重失效 7.1
  竞态、apns_jwt/fcm 广播 vs local、group join/leave 读改写、login_attempt_ip_limit。
- 方法：只读 src/ 与 test/，逐项比对源码现状、W1-W5 执行报告结论与 Registry
  标记，grep 确认测试存在性。三选一分类：FIXED-with-test / Documented-Exception /
  BLOCKED。
- 红线遵守：未修改任何 src//test/ 文件；未运行 make eunit/eunit-local/dcheck；
  未启动任何 Erlang 节点；未占用端口；未 git add/commit；
  `scripts/cache_v21_dcheck.erl`、`scripts/cache_v21_mixed_dcheck.erl`、
  `src/lib/imboy_cache_sync*.erl` 仅只读引用、未做任何写操作。

## 汇总结论

| # | 尾巴项 | 分类 | 一句话结论 |
|---|---|---|---|
| 1 | tag_subtitle_2/3 fill/flush 键失配 | **FIXED-with-test（2026-10-13）** | fill/flush 均指向 tag_subtitle_2；真实缓存回归验证整数与二进制 TagId 的失效后回填 |
| 2 | config5 双重失效 7.1 竞态 | **实现已修复，最终集成待绑定** | 现已 fenced memo / 未 fenced 零 TTL；仅成功密文写后失效；下文为原审计历史 |
| 3 | apns_jwt / fcm / qianfan 广播 vs local | **FIXED-with-test（2026-10-13）** | 一元组/裸原子形态已按 local 分类；真实 syn 对照验证正常 set 不发布凭据，旧 SET 接收也不能覆盖本地凭据 |
| 4 | group join/leave 读改写竞态 | **实现已修复，最终真 PG 验收待绑定** | 现已 invalidate-on-change + 提交动作队列；下文 BLOCKED 为原审计历史 |
| 5 | login_attempt_ip_limit | **FIXED-with-test（本地定向；待最终绑定）** | 2026-10-13 复用既有 login_attempt_ds actor 串行原本地读改写；40 路无丢计数及阈值拒绝通过，保持成功滑动续期 3600 秒、拒绝不续期 |

---

## 1. tag_subtitle_2/3 fill/flush 键失配

**Registry 标记**: `tag_subtitle_2` → `W1_KNOWN_DEFECT`；
`tag_subtitle_3` → `LEGACY_TAG_NO_WRITER`（key-registry.json）

### 原审计源码位置（2026-10-12）

- 填充键: `src/repo/user_tag_relation_repo.erl:262` —
  `Key = elib_cnv:implode("_", ["tag_subtitle_2", TagId])`，
  `:296` `imboy_cache:memo(Fun, Key, 86400)`（TTL 1 天）
- 失效键: `src/repo/user_tag_relation_repo.erl:301-302` —
  `Key = elib_cnv:implode("_", ["tag_subtitle_3", TagId])`，
  `imboy_cache:invalidate(Key)`
- 失效调用链: `src/ds/user_tag_ds.erl:88,143,239-240`、
  `src/logic/user_tag_relation_logic.erl:99,202,430,454` → 全部经
  `flush_subtitle/1` → 上述 tag_subtitle_3 键
- Policy 注册: `src/lib/imboy_cache_policy.erl:93-94`
  （tag_subtitle_2/3 → invalidate）

### 原审计分析（2026-10-12，修复前）

1. 填充键（`tag_subtitle_2_<TagId>`）与失效键（`tag_subtitle_3_<TagId>`）自
   0.7.3 架构重构（`83c15f7a`，2026-01-01）以来持续失配。全仓 grep 确认：
   `tag_subtitle_3_*` 键无任何填充方（纯删除标签）；
   `tag_subtitle_2_*` 键无任何失效方。
2. W5-01（`4b05dd03`）仅将 `flush_subtitle` 的 `imboy_cache:flush/1` 迁移为
   `invalidate/1`（调用形态迁移），键字面量未改，失配原样保留。
3. Registry（key-registry.json）已正确登记根因：
   "失效时序本身正确（user_tag_ds:88/143 均在 with_tx 闭包结束后调
   flush_subtitle）；缺陷在填充键…与失效键…失配，副标题变更后最长 1 天内读旧值"。
4. Policy 层面：binary 键 `namespace_of/1`（policy.erl:152-158）仅支持元组
   形态 → 二进制拼接键归 `legacy` 兜底类别，不参与 V2 复制/fenced；
   invalidate/1 对其走 legacy `{flush, Key}` 扇出（无值泄漏，仅键名指令），
   对其他节点为无害 no-op。
5. 测试面：`test/ds/user_tag_ds_tests.erl:522` `flush_subtitle_success_test_`
   等 mock 测试仅验证转发语义，不校验 fill/flush 键对齐；
   全 test/ 无键失配回归测试。

### 当前分类: FIXED-with-test（2026-10-13）

`flush_subtitle/1` 已对齐 scene2 填充键 `tag_subtitle_2`，不再失效无填充方的
`tag_subtitle_3`。`subtitle_invalidation_refills_actual_cached_key_test_` 使用真实
不连接 PG 的 depcache/imboy_cache/sync，替换 PG 查询为可变受控数据源：先确认
Alice 被缓存，权威改为 Bob 后整数 TagId 失效立即回填 Bob；再次改为 Carol 后
二进制 TagId 失效回填 Carol，查询次数分别为 1/2/3。
该证据只覆盖键匹配和本地回填，不证明真实 PG COMMIT/ROLLBACK、跨节点通知或
并发 loader 栅栏；其余 Registry 钩子仍需单独验证。原 W0 Registry 保留原审计快照。

### 历史登记例外（已修复）

**原保留现状的技术理由**:

- 缺陷已由 W0 Registry 完整登记（双条目 + 根因 + 影响上界），不构成静默缺漏
  （19.4 合同满足）；
- 影响面为好友标签副标题（scene 2 展示数据），非安全关键键；TTL 86400s 提供
  陈旧上界兜底；
- binary 键属 legacy 类别，与 V2 复制/fenced 完全隔离，无跨节点一致性放大；
- 修复动作明确且极小（`flush_subtitle` 键字面量 tag_subtitle_3 → tag_subtitle_2
  对齐 + 键对齐回归测试），修复时 tag_subtitle_3 本无填充、tag_subtitle_2 旧值
  靠 TTL 自然过期，无需过渡清理，适合在下一实施轮以独立原子提交完成。

**历史风险描述（已修复）**: 标签关系/好友备注变更后，标签副标题缓存最长 1 天返回旧值；
每次变更还会对集群广播一次必然 no-op 的 `{flush, tag_subtitle_3_*}` 消息
（轻微无效流量）。

---

## 2. config5 双重失效的 7.1 竞态

**Registry 标记**: `config5` → `SECRET_SUBITEM_REVIEW_W1`（key-registry.json）

### 源码位置

- 写前失效: `src/ds/config_ds.erl:135` — `imboy_cache:invalidate(cache_key(Key2))`
- DB 写: `src/ds/config_ds.erl:154-166` — 裸 `elib_pg:insert/update`
  （无显式事务包裹，单语句自动提交）
- 写后失效: `src/ds/config_ds.erl:167` — `imboy_cache:invalidate(cache_key(Key))`
- 回填路径: `src/ds/config_ds.erl:109-124` — `get/2` 手写
  `get → pluck_decrypted_value → imboy_cache:set(CacheKey, Result, 864000)`
  两步回填，不走 `memo/4`
- W5-01 迁移: `4b05dd03` 将两处 `flush` → `invalidate`（双重结构本就存在）

### 现状分析

1. 双重失效（写前 + 写后）的真实语义：写前失效清当前副本并拦截"写前回填"；
   写后失效关闭"读取开始于写前、回填于 DB 写之后"的窗口。
2. 残余 7.1 窗口（计划 7.1 原文明确承认"如果只在事务提交后广播失效，仍然
   不能完全防止上述竞态"）：
   `reader get miss → pluck 读到旧值（writer 提交前）→ [writer: 写前失效 →
   DB 提交 → 写后失效] → reader set(旧值)` → 旧配置值驻留缓存，TTL 864000s
   （10 天）放大。
3. 19.3 栅栏现状：`cache_fenced_namespaces` 默认空（policy.erl:112-114），
   config5 未启用；且即使启用，`config_ds:get/2` 是手写 get/set 回填、不走
   `memo/4`，栅栏路径（imboy_cache.erl:117-124）对它不生效——彻底闭合需要
   回填代码改造，不只是配置。
4. 敏感广播半边已闭合：`{config5, K}` 为二元组 → policy invalidate
   （policy.erl:81）→ `should_broadcast`（imboy_cache.erl:246-250，仅 legacy
   广播）= false → 解密后配置值不再经 set/5 全节点广播（W1-05 策略化修复，
   SECRET_SUBITEM_REVIEW_W1 的广播半边已解决，剩 G-04 敏感子项审查属 C06
   Registry 属性面，不在本尾巴范围）。
5. 测试面：`test/ds/config_ds_tests.erl` 全部为占位断言（无 DB、无失效时序
   断言）；19.3 栅栏测试 `test/lib/imboy_cache_v2_tests.erl:109`
   `fenced_stale_loader_must_not_return_old_value_test_` 仅覆盖 fenced 合成键
   的 memo 路径，不覆盖 config5 业务回填路径。

### 分类: Documented-Exception

**保留现状的技术理由**:

- 计划 7.1/7.5 自身承认"提交后失效"不能彻底消除该竞态，设计对策是 7.2 栅栏
  （19.3）；而 fenced 逐 namespace 启用按 W5-01 报告与计划门控属"性能 gate
  之后阶段"，当前保留 legacy 行为是计划内的阶段性状态，不是未授权偏离；
- 双重失效已覆盖绝大多数交错（仅"读旧值→回填"横跨整个写路径的极窄窗口
  残余）；
- 写入方为管理后台运维级低频操作；权威源为 PG config 表，节点冷启动重建；
- 彻底闭合路径明确：config5 入 fenced 名单 + `get/2` 回填改走 `memo/4`
  （或手写栅栏），属 fenced 启用轮的实施项。

**风险描述**: 极端交错下，某节点在运维变更配置后最长 864000s（10 天）继续
提供旧配置值（TTL 放大是本例外最大代价）；缓解手段：配置变更后手动
`invalidate` 或滚动重启节点。

---

## 3. apns_jwt / fcm 命名空间：广播 vs 本地策略

**Registry 标记**: 两者均 → `LOCAL_SELF_MANAGED_BROADCAST_W1`
（key-registry.json："签名 JWT / OAuth token 经 imboy_cache:set 在
dsync_enabled=true 时全节点广播；legacy 行为与 local 策略矩阵不一致
（G-04/W1-05）"）

### 原审计源码位置（2026-10-12）

- apns_jwt 写入: `src/ds/push_notification_ds.erl:491-505` —
  `get_apns_jwt/2`，`imboy_cache:set({apns_jwt, KeyId}, Jwt, 3000)`
- fcm 写入: `src/push_provider_fcm_token.erl:39`
  `-define(CACHE_KEY, {fcm_access_token_dyn}).`（**一元组**）；
  `:243-247` `cache_on_success/1` → `imboy_cache:set(?CACHE_KEY, Token, 3000)`
- Policy 注册: `src/lib/imboy_cache_policy.erl:53,56`
  （apns_jwt / fcm_access_token_dyn → local）
- 广播判定: `src/lib/imboy_cache.erl:246-250` —
  `should_broadcast(Key) = dsync_enabled ∧ ¬local_cache_key ∧ policy(Key) =:= legacy`
- namespace 提取: `src/lib/imboy_cache_policy.erl:152-158` —
  `namespace_of/1` 仅匹配尺寸 2–5 且首元素为 atom 的元组（及
  `{local_cache, Sub, _}`），**无一元组子句**

### 原审计分析（2026-10-12，修复前）

1. **apns_jwt（已修复）**: `{apns_jwt, KeyId}` 为二元组 → namespace =
   apns_jwt → policy = local（policy.erl:53）→ `should_broadcast` = false
   → W1-05 的 set/5 广播策略化对该键生效，JWT 不再广播。
2. **fcm（缺陷仍在）**: `{fcm_access_token_dyn}` 为**一元组** →
   `namespace_of` 无匹配子句 → undefined → policy = legacy（未分类兜底）→
   `dsync_enabled=true` 时 `set/5` 仍执行
   `broadcast({set, {fcm_access_token_dyn}, Token, 3000, []})` —
   **OAuth access_token 全值广播复制到全组**。policy.erl:56 的 local 注册
   对该键形态永不生效。测试侧的对应缺口：
   `test/lib/imboy_cache_policy_tests.erl:39` `namespace_of_variants_test_`
   断言了 `{1,2}`（非 atom 二元组）→ undefined，但无一元组形态断言。
3. **同族失配（关联发现）**: `src/lib/qianfan_api.erl:67` 键为裸原子
   `qianfan_access_token` → namespace 同样 undefined → legacy → 同样广播
   （Registry 亦标 local，同样不生效）。修复 fcm 时必须一并处理，否则只修一半。
4. 触发条件限定：`dsync_enabled` 生产默认 false（imboy_cache.erl:248），
   当前单节点/默认部署不暴露；缺陷在多节点且 dsync_enabled=true 时实际发生。

### 当前分类: FIXED-with-test（2026-10-13）

`namespace_of/1` 当前支持裸原子和非空 atom 首项元组，一元组 FCM、裸原子
qianfan 和 APNs 元组均按 Registry local 策略处理。已有不广播单元回归加上
`credential_policy_is_enforced_on_real_publish_and_receive_test_` 的真实 syn/depcache/
sync 对照：启用 dsync 后三类合成凭据的标准 set 不发布 SET，未分类 legacy 键
仍真实发布；旧格式 SET 注入后，本地凭据值不被覆盖。

旧 SET 接收仅接受未分类且无复制索引/版本行的键。另有真实 DELETE-only →
撤销 Registry → 旧 SET 回归，验证墓碑保留窗口内不写 raw 值；公开无版本 set
拒绝，预先存在的 raw ghost 不可读。此证据不等于真实令牌提供商、跨主机或
保留窗口之外的超窗恢复验收；C10 当前候选混合版本矩阵仍需重跑。

### 历史分类与证据（原 BLOCKED 已由上述实现消解）

**原 apns_jwt 证据（2026-10-12）**:

- `test/lib/imboy_cache_policy_tests.erl:11-28`
  `registry_policy_classification_test_` — `:14`
  `?assertEqual(local, imboy_cache_policy:policy({apns_jwt, 1}))`：
  广播判定的决定性输入（policy = local ≠ legacy）被测试锁定；
- `test/lib/imboy_cache_tests.erl:74-95`
  `set_local_cache_key_skips_broadcast_test_` — local 类键 set/5 不触发
  `imboy_cache_sync:broadcast/1` 的机制级行为测试；
- `test/lib/imboy_cache_v2_tests.erl:31`
  `put_local_writes_without_broadcast_test_` — put/4 local 不广播。
- 如实注明：`set({apns_jwt,1})` 这一具体形态的"不广播"直接行为断言不存在，
  闭合链 = 分类断言（有测试）+ should_broadcast 纯函数（imboy_cache.erl:
  246-250 实现）。建议后续轮补一条该形态的直接断言。

**原 fcm 阻塞原因（已消解，2026-10-12）**:

- 修复需要 src/ 写变更，且方案影响面超出单点：
  (a) `namespace_of/1` 增加一元组子句 `{NS} when is_atom(NS) -> NS`（policy
  契约扩展，影响全部 namespace 分类矩阵与 registry_hash——C01 已冻结
  registry_hash，变更须走冻约流程）；或
  (b) `push_provider_fcm_token.erl` 的 `?CACHE_KEY` 改为二元组
  `{fcm_access_token_dyn, primary}`（键形变更 + 兼容旧值过期）；
  两条路径均须同步补 policy/行为测试，并将同族的 `qianfan_access_token`
  裸原子键一并处置，避免修一半。
- 本轮 C06 尾巴核查红线只读 src/，无法执行上述任一路径。

**历史风险描述（已修复标准写入及接收边界）**: `dsync_enabled=true` 的多节点部署下，FCM OAuth access_token
（敏感凭据，Registry sensitive=true）经 legacy `{set, ...}` 消息全组复制，
违反计划 19.5"JWT/OAuth…不得全值复制"红线；qianfan token 同理。当前默认
配置（dsync_enabled=false）下为潜伏缺陷，开关翻转即暴露。

---

## 4. group join/leave 读-改-写竞态（含 W1-03 事务内缓存变更漏扫）

**Registry 标记**: `group` → `TRACED_IN_TX_W1`（key-registry.json：
"group_member_ds 事务内调 group_ds:join → imboy_cache:set 位于事务内
（W1-03）；且 join/leave（:264-296）为成员列表缓存读改写，与 account_list
同类非原子缺陷"）

### 源码位置

- join 读改写: `src/ds/group_ds.erl:261-274` —
  `member_uids(Gid)` 读 → `imboy_cache:set(CacheKey, [Uid | Li], ?HOUR)` 写
- leave 读改写: `src/ds/group_ds.erl:284-292` — 读 → `set(CacheKey, lists:delete(...))`
- 缓存键: `src/ds/group_ds.erl:44` `-define(GROUP_CACHE_KEY(Gid), {group, Gid}).`（TTL 3600）
- 回填: `src/ds/group_ds.erl:151-174` `member_uids_cached/1`
- **事务内缓存变更**: `src/ds/group_member_ds.erl:146` —
  `join_group/5` 在 `elib_pg:with_tx` 事务体内（经 Conn 执行 upsert 之后）
  调用 `group_ds:join(Uid, Gid)` → `imboy_cache:set` 位于事务内；
  git 溯源 `636442da`（2026-09-07）即存在
- join_group 事务调用方（4 处）: `src/logic/group_member_logic.erl:84-95`
  （join_group 为闭包尾部）、`src/ds/group_member_ds.erl:60`（add_member）、
  `src/ds/group_ds.erl:534`（face2face_join，闭包尾部）、
  `src/ds/workspace_ds.erl:223`（**join 之后事务内还有 Channel 创建等后续
  步骤，后续 throw → 整体回滚而缓存已写**）
- leave 侧对照（合规）: `src/logic/group_member_logic.erl:141-168`
  `leave_execute` 在 `with_tx` 返回 `{ok,_,_}` 后才调 `group_ds:leave`
  （7.5 模式正确）
- 下游消费: `src/ds/group_ds.erl:144-149` `member_uids_strict` →
  `src/repo/micro_community_access_repo.erl:156-162` `filter_uids/2`
  （仅按 micro-community/channel 资格过滤，**不校验群成员表**——脏缓存
  列表不会被下游 DB 过滤兜底）

### 现状分析

1. **读改写竞态（未修）**: join/join、join/leave 并发交错时后写覆盖前写，
   缓存成员列表丢成员/带已离开成员。丢成员方向直接命中
   `src/ds/group_ds.erl:140-143` 代码注释自认的最严重后果——"空收件人
   …消息落库了、发送方看到成功…**收件人为空，DB 恢复后也永久不可投递**…
   静默丢消息在监控上表现为一切正常"。同类缺陷（account_list 读改写）已在
   C06 重扫后由 `e001fb8e` 修复（`src/ds/imboy_account_allocator.erl` 专用
   allocator 进程），group 侧未跟进。
2. **事务内缓存变更（W1-03 历史结论漏扫，未修）**: 历史 W1-03 复核结论
   "全部 13 处 `elib_pg:with_tx` 事务体经扫描 0 缓存调用"只扫描了事务体内
   **直接**的 `imboy_cache:*` 调用，未追踪 `join_group → group_ds:join →
   imboy_cache:set` 的间接调用链；key-registry.json（W0 产物）正确记录了
   该点（TRACED_IN_TX_W1），但 W1-03 执行结论未处理。现状：
   workspace 路径（workspace_ds:223）join 后事务内仍有可失败步骤，
   throw → 回滚后 `{group, Gid}` 缓存已含 OwnerUid 而 DB 无该成员
   （脏缓存 ≤ TTL 3600s），C2G 投递按缓存列表执行且 `filter_uids` 不做
   成员资格兜底。
3. **测试面**: `test/ds/group_ds_tests.erl:194-258` join/leave 仅有单线程
   顺序 mock 断言（`join_adds_uid_to_cached_member_list_test_`、
   `join_is_idempotent_when_uid_already_exists_test_` 系列）；
   `test/ds/group_member_ds_tests.erl:417` 附近的 abort_tx 测试仅覆盖
   **前置校验失败**（upsert 之前 throw，缓存未动）路径；
   无并发读改写测试、无"缓存写后事务回滚"测试。

### 分类: BLOCKED

**精确阻塞原因**:

- 修复需要两个 src/ 写变更组合，均超出本轮 C06 尾巴核查的只读红线：
  1. join 缓存变更移出事务：`group_member_ds:join_group/5` 去除 `:146` 的
     `group_ds:join/2` 调用，由 4 个事务调用方（group_member_logic / add_member /
     face2face / workspace_ds）在 `with_tx` 成功分支后统一执行——参照
     `leave_execute`（group_member_logic.erl:141-168）的既有正确模式做事务
     边界重构；
  2. 成员列表缓存读改写原子化：改为 invalidate-on-change + `member_uids_cached`
     全量回填（invalidate 类别键的语义本位），或仿照 `e001fb8e` 的
     imboy_account_allocator 模式串行化——涉及 `{group, Gid}` 语义变更与
     C2G 投递链回归。
- 两项均需配套并发回归测试（对照 account 侧
  `test/ds/account_ds_tests.erl:49` `concurrent_allocate_ids_are_unique_test_`
  40 路并发模式），须在允许写 src/ 的实施轮完成。

**风险描述**: 并发入群交错下 C2G 消息静默漏投成员（不可恢复、监控不可见）；
workspace 建群路径部分失败回滚后，缓存脏成员最长 1 小时内向无成员资格者
投递群消息。保留期内实际暴露面受写入并发度与 workspace 路径失败率约束。

---

## 5. login_attempt_ip_limit 相关项

### 当前修正（2026-10-13）

历史的 Documented-Exception 不再代表当前代码。此前若其他例外台账已写
RESOLVED，也不能倒推旧 get→set 实现无竞态；本次先以 40 路并发复现
2 FAIL/1 PASS（exit 1），再修复既有 actor 的串行入口。

`check_ip_rate_limit/1` 现调用本模块已监督的 gen_server，原缓存读改写在
一个 handle_call 内完成；没有新增 actor、依赖或缓存键。每次成功仍使用
原 `{login_attempt_ip_limit,Ip}` local 键、set TTL 3600；Count>=阈值时不写
或续期。既有账号 record_failure 同一 actor 无回调本入口，不产生自调用。

`login_ip_atomic_tests` 使用真实 depcache 和受控 get 延迟，40 路结果为
1..40 唯一序号；阈值 10 只允许 10、拒绝 30，拒绝时不追加写；真实
login_security_logic 直接失败入口达到 IP 阈值后不记录账号失败。三场景
exit 0，旧 DS/logic 定向另 17 PASS、exit 0。此处不涉及 PG，不签发生产
吞吐或跨节点统一计数；计数按原合同保持本地。

以下为修复前历史审计，保留其证据与风险描述，不作为当前未修结论。

**Registry 标记**: `login_attempt_ip_limit` → `W1_KNOWN_DEFECT`
（key-registry.json："get→set 读改写非原子，且每次失败登录都经 set 全节点
广播（W1-05）"）；关联条目 `login_attempt` → `LOCAL_SELF_MANAGED`
（"depcache 侧仅 login_security_logic:160 读取，无同源写入"）

### 源码位置

- 键宏: `src/ds/login_attempt_ds.erl:57`
  `-define(IP_RATE_LIMIT_CACHE_KEY(Ip), {login_attempt_ip_limit, Ip}).`
- 读改写: `src/ds/login_attempt_ds.erl:173-193` `check_ip_rate_limit/1` —
  `imboy_cache:get(Key)` → `imboy_cache:set(Key, Count+1, 3600)` 非原子
- 同模块原子化对照: `src/ds/login_attempt_ds.erl:377-444`
  `record_failure` 走 gen_server + ETS 原子路径（模块头注释 :23-25 宣称的
  原子性仅覆盖此路径与 `{login_attempt, Identifier, Ip}` 键，不覆盖 IP 限流键）
- 消费点: `src/logic/login_security_logic.erl:74` → `check_ip_rate_limit`
- Policy 注册: `src/lib/imboy_cache_policy.erl:59`（local）
- 关联缺陷（读错存储）: `src/logic/login_security_logic.erl:160`
  `imboy_cache:get(login_attempt_ds:cache_key(Identifier, Ip))` 读 depcache
  的 `{login_attempt, Identifier, Ip}`——该键的写方 `record_failure` 写的是
  自有 ETS（`?LOGIN_ATTEMPT_ETS`），从不写 depcache → 此 get 恒 miss →
  `build_lock_info` 的 `locked_until` 字段永不填充（key-registry login_attempt
  条目已记录"读 depcache 无同源写入"）

### 现状分析

1. **广播半边（已修复）**: `{login_attempt_ip_limit, Ip}` 为二元组 →
   policy = local（policy.erl:59）→ `should_broadcast` = false →
   W1-05 策略化后失败登录的 set 不再全节点广播。
2. **读改写半边（未修）**: 并发 N 个请求同时 get 到相同 Count → 各自 set
   Count+1 → 增量丢失、计数偏低 → IP 限流阈值（默认 100 次/小时）达成时刻
   被推迟（fail-open 方向）。
3. **纵深防御现状**: 账号级锁定（`record_failure` 原子路径，默认 5 次/30
   分钟锁定）是第一防线且不受此缺陷影响；IP 限流是第二层。
4. **测试面**: `test/ds/login_attempt_ds_tests.erl:69-79`
   `check_ip_rate_limit_success_test_` 为单线程顺序 mock（get undefined →
   set 1），验证基本语义但不验证原子性；全 test/ 无该键并发测试；
   `imboy_cache_policy_tests` 的分类抽样断言未含 `login_attempt_ip_limit`
   （广播闭合依赖机制级测试 + policy 注册，无该键直接断言）。

### 分类: Documented-Exception（读改写半边；广播半边已闭合，证据为机制级）

**保留现状的技术理由**:

- 账号级原子锁定防线完好，IP 限流计数丢失仅延迟第二层阈值达成时刻；
- 默认阈值 100 次/小时 + 3600s 窗口有界，持续速率型爆破下计数仍会推进
  （仅完全并发交错的突发丢失增量）；
- local 类别键，无跨节点复制面（W1-05 后 set 不广播），单节点内存计数；
- 原子化模式在同模块现成（record_failure 的 gen_server+ETS），后续实施轮
  可低成本迁移，属增强而非设计豁免。

**风险描述**: 攻击者以完全并发突发打登录接口时，IP 限流形同虚设的风险真实
存在（每次并发读到同值 → 计数不增）；建议后续轮按 record_failure 模式原子化
并补 40 路并发对照测试。附带一并修复 `login_security_logic.erl:160` 的
depcache 读错存储（`locked_until` 展示缺失，fail-safe 方向，无安全放大）。

---

## 关联发现（同族问题，不在本轮 5 项清单，建议登记）

1. **一元组/裸原子键 policy 失配族**: `{fcm_access_token_dyn}`（一元组）与
   `qianfan_access_token`（裸原子）在 `namespace_of/1` 下均归 legacy，
   Registry 的 local 注册不生效——与第 3 项 fcm 同根因，修复须成组处理；
   `bind_mail`（三元组）等其他 local 键形态正常。
2. **W1-03 历史结论修正建议**: "13 处 with_tx 事务体 0 缓存调用"的扫描
   未覆盖间接调用链（join_group → group_ds:join → imboy_cache:set），
   group join 是现存实例（第 4 项）；C12/C14 复算该条目时建议按间接链口径
   重扫，避免 REVERIFY 直接继承旧结论。
3. **policy 分类测试抽样补全建议**: `registry_policy_classification_test_`
   现有抽样未覆盖 `login_attempt_ip_limit`、`fcm_access_token_dyn`（一元组）、
   `qianfan_access_token`（裸原子）三个已知敏感/失配键，建议在允许写 test/
   的轮次补充，作为本文件第 3/5 项 FIXED 判定的行为级加固。

## 证据文件索引

- 台账: `docs/architecture/cache-v2.2.1/closeout-v1/ledger-v1.json`、
  `review-ledger-v1.md`（W1-01..07 条目）
- W1 尾巴权威标记: `docs/architecture/cache-v2.2.1/w0/key-registry.json`
  （mutation_hook 字段四类 *_W1 标记）、`w0/key-registry.md`
- 历史执行: `docs/architecture/cache-v2.2.1/w1-w5-report-20261011.md`
  （W1-03/W1-05 结论）、`w1-w5-addendum-20261011.md`
- 计划: `docs/architecture/cache-v2.2.1/w0/plan-v2.2.1.md`（7.1/7.5/19.3/19.4/19.5 节）、
  `backend-closeout-plan-v1.md`（C06 任务卡）
- 引用提交: `83c15f7a`（0.7.3 失配源头）、`4b05dd03`（W5-01 迁移）、
  `636442da`（join 事务内缓存调用引入）、`e001fb8e`（account 同类缺陷
  C06 修复对照）
