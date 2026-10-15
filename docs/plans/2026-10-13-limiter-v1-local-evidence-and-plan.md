# IMBoy Limiter V1：本地证据与最小治理计划

> 日期：2026-10-13（Asia/Shanghai）。范围：当前磁盘版本的静态取证、报告与计划。  
> 结论：**发现已确认的源码缺陷；尚未动态复现；L1 合同未冻结；不可声明 Limiter 已就绪。**  
> 本文件不授权实现、提交、部署或改变其他任务。本轮只精修本报告；旧报告与共享 WIP 保持原样。

## 1. 结论摘要

1. **P1：真实密码/验证码登录的失败计数链断开。** handler 已取得真实 IP，但调用逻辑层的五参数接口不传 IP；逻辑层使用空 IP，`record_login_failure/2` 因 `invalid_ip` 提前返回，调用方丢弃结果。验证码锁定门又把三元组错误漏到默认放行分支。HTTP IP 桶仍存在，所以不能说“所有登录防护归零”；但账户失败锁定不能按设计成立。
2. **P1：状态来源与操作边界不一致。** 计数在自有 ETS，`locked_until` 却读取无对应生产写入的 Cache；失败写在 gen_server 中，reset/管理员解锁直接删 ETS，能形成旧值写回。成功认证也不总与整条登录成功等价。
3. **P1：IP Cache 读改写非原子，窗口、容量和清理都不适合作为严格额度权威。** 成功增加会续期 3600 秒；Cache 内存压力可淘汰有效额度，过期清理的无版本 flush 有删除新状态的交错。
4. **P1/P2：OIDC 与 HTTP 入口 IP 信任规则不同；HTTP 依赖还存在初始化、窗口和资源治理缺口。** OIDC authorize/callback 都在处理前扣同一 IP 桶；HTTP 另扣请求桶。throttle 的单次 ETS 递增原子，不代表定时清零、重复 setup、配置更新和容量整体正确。
5. **推荐先整理现有领域模块。** 保留“认证结果后记录失败”和“处理前原子准入”两个操作，复用少量私有状态机制；不预设新建 `elib_limiter`、分片、Generation 或基础设施。默认部署材料支持单后端；节点本地额度是候选建议，不是已冻结的集群业务要求。

本轮精修合同：容量在途漏记触发账户域保守降级并拒绝后续准入（AC-10/12/13）；两HTTP IP scope局部迁移但保留scope周期（AC-08/14）；旧基线前移L0、候选混合负载先过L2（AC-16）；reset确认后才签发凭据（AC-17）；关闭/排空/停节点/批准空态切换，无已验证回退候选则保持关闭（AC-18）。D1～D7业务批准与测量均独立列出，新增19项AC不等于已实现/已通过。

优先级：P1 为实施前必须解决的防护/一致性问题；P2 为协议、资源、运维合同缺口，必须在 L4 前关闭。没有生产运行证据，因此不定性为已发生事故。所有并发时序均为**源码推导**，不是本轮“已复现”。

范围仅含登录失败与相关 IP 请求/失败计数。验证码消费只核实直接调用边界，不改造其一次性消费实现；QR、消息限频、Agent 预算、ACK/Async/Retry 的独立治理均排除。

## 2. 本地基线、覆盖与证据限制

### 2.1 仓库与协作边界

- 实际Git根：`/Users/leeyi/project/imboy.pub/imboy`；分支`main`；本轮HEAD：`0a5ade3cd8e8c005a1cc22991633dedd81af0445`；暂存区为空。
- 明确以本文件（包含F1～F8、D1～D7、L0～L4）为修订对象；定向检索同主题仅本文件与2026-10-10旧稿，没有明确接续更新稿，不以mtime选择。修订前本报告SHA-256：`a8ca6d2b9caba5df659469e900d79704ba464849266aae0fe900dde3c61c4566`。
- 旧报告取证HEAD为`8ee8f899911eddd9cea9a6342bf431bf9449e281`；本轮不把它当当前版本。先逐文件核对71个原证据，再补读凭据/设备/Registry路径，当前共78文件见附录A。
- 本轮开始已有工作树状态如下（只记录路径，不输出内容/秘密）；全部作为外部WIP保留，本任务不暂存、不提交：

```text
 M docs/architecture/cache-v2.2.1/closeout-v1/c10-legacy-build.md
 M docs/architecture/cache-v2.2.1/closeout-v1/protocol-budget-frozen.md
 M docs/architecture/cache-v2.2.1/closeout-v1/registry-w1-closeout.md
 M docs/architecture/imboy-distributed-cache-v2.md
 M scripts/cache_v21_mixed_dcheck.erl
 M scripts/check_cache_closeout_v1.py
 M src/api/user_tag_handler.erl
 M src/ds/auth_session_ds.erl
 M src/ds/billing_subscription_ds.erl
 M src/ds/config_ds.erl
 M src/ds/moderation_ds.erl
 M src/ds/moment_ds.erl
 M src/ds/user_tag_ds.erl
 M src/lib/imboy_cache.erl
 M src/lib/imboy_cache_policy.erl
 M src/lib/imboy_cache_sync.erl
 M src/lib/imboy_cache_sync_apply.erl
 M src/lib/imboy_cache_sync_recover.erl
 M src/lib/imboy_cache_sync_snapshot.erl
 M src/logic/user_tag_logic.erl
 M src/repo/moment_post_acl_repo.erl
 M src/repo/user_tag_relation_repo.erl
 M test/api/user_tag_handler_tests.erl
 M test/ds/moment_atomic_binding_tests.erl
 M test/ds/user_tag_ds_tests.erl
 M test/lib/imboy_cache_metadata_tests.erl
 M test/lib/imboy_cache_snapshot_coverage_tests.erl
 M test/logic/user_tag_logic_tests.erl
 M test/repo/user_tag_relation_repo_tests.erl
?? docs/architecture/attachment-lifecycle-v5/
?? docs/architecture/cache-v2.2.1/closeout-v1/evidence-schema-v2.md
?? docs/business/imboy-open-source-productization-strategy-v1.md
?? docs/design/2026-10-08-micro-community/plan-v1.md.sha256
?? src/lib/imboy_cache_registry.hrl
?? test/ds/cache_mutation_hooks_tests.erl
?? test/ds/cache_security_hooks_tests.erl
?? test/ds/config_cache_closeout_tests.erl
?? test/ds/user_tag_commit_tests.erl
?? test/lib/imboy_cache_capability_tests.erl
?? test/lib/imboy_cache_manifest_merge_tests.erl
?? test/scripts/cache_closeout_verifier_selftest.py
```

- 已读取用户根规则及当前后端AGENTS.md；文档无更深规则变更证据。用户要求简体中文及本轮只修文档优先；未执行不存在的/mem命令，未修改记忆。
- 报告在写前核对原指纹，若并发变化则不覆盖，应交唯一候选并标明替代关系；本轮写前指纹未变化。

### 2.2 取证方法与版本绑定

覆盖磁盘源码、当前相关 Git diff、相关未跟踪测试、本地 deps、被忽略的本地配置中的相关键。先搜索所有模块调用、宏键、ETS 表名、错误返回，再沿 handler→logic→DS→Cache/依赖及 supervisor 追踪。仓内检索未找到另一个生产 `login_attempt_ets` 写方或 `admin_unlock` 的实际 HTTP 调用；无法据静态检索排除外部节点 RPC、热代码或运维 shell 的临时调用。

关键文件 SHA-256、读取/复核时间及 HEAD/暂存区归属列在附录。旧稿核心首轮读取为2026-10-13 10:21:30 +08:00；本轮刷新读取UTC区间见附录A/B。这些是逐文件读取，不是原子仓库快照。每条跨文件推导依赖附录所列文件版本，尤其 Cache 四文件不得随意替换成旧 HEAD 后仍沿用结论。

依赖取自磁盘 `deps/throttle`：其 Git HEAD 为 `4f5fb17c9d4a86ba016e7011648ae5dfe539ac01`，app 声明版本 0.3.0；依赖子仓自身有 Makefile/app.src 修改及未跟踪构建文件。实际 `throttle_ets` 源码已读取，**未证实已运行节点加载的 BEAM 与磁盘一致**。

证据分级：

- **S（源码直接证明）**：调用参数、返回匹配、Key、实际 ETS/Cache 操作、默认配置与启动顺序。
- **I（由源码推导）**：并发丢更新、迟到消息删除、重启后额度恢复、可利用条件。
- **R（实际运行）**：本轮仅执行只读文件/Git 查询与报告静态检查；**无业务动态验证**。

没有编译、EUnit、make dry-run、应用启动、数据库连接、依赖安装、Git 状态变更或生产查询。Makefile:29 本身有解析期复制配置行为，因此连 `make -n` 也未执行。本地配置不是在线运行配置；未读取节点环境或提供真实连接数据。

## 3. 调用矩阵与状态来源矩阵

### 3.1 实际入口与执行阶段

源码简称仅供表格：P=`src/logic/passport_logic.erl`，H=`src/api/passport_handler.erl`，D=`src/ds/login_attempt_ds.erl`，S=`src/logic/login_security_logic.erl`，T=`src/api/throttle_middleware.erl`。

| 入口/可达性 | 调用链及阶段 | 计数/重置与实际结果 | 对外行为/现有测试 |
|---|---|---|---|
| POST /api/v1/passport/login 密码，已路由 | router:81 → HTTP T:46–63 → H:168–202 → P:390–424 → P:528–591 → S → D | HTTP 先占请求额度；密码校验前账户 check；校验 error 后 failure，因空 IP 不写；成功后 reset | 业务 error 经 elib_response:error/2；HTTP 桶超限为 429。handler/logic 多为 mock，REST 有单次错误口令 |
| 同一路由 code 非空，已路由 | H:185–192 → P:440–466 → verification_code_ds:consume/2 → P:474–522 | check 的三元组锁定未匹配；错误验证码后 failure 被空 IP 截断；验证码通过后、检查用户 status 前 reset | 保留错误消息及 envelope；未检出该真实锁定闭环测试 |
| 内部兼容 login/3、do_login/3 | P:353–384 包装到 do_login/5；仓内 login/3 主要见 CT 调用 | PostVals 虽有 ip，未传到校验层；同样空 IP；verify_user/3 是导出的较低层接口，不独立调用 limiter | 不是另一条已证实 HTTP 入口；迁移必须覆盖兼容导出和测试调用 |
| GET /api/v1/auth/oidc/authorize，路由存在，provider 有条件启用 | router:116 → T 通用 IP → auth_oidc_handler:70–85 → D:174–193 → authorize | **前置请求准入并增加**；与 callback 共用 login_attempt_ip_limit；provider 关闭也先扣额度 | 成功 302；超限/错误用业务 400 code（通常 HTTP 200），不是 HTTP 429 |
| GET /api/v1/auth/oidc/callback，同上 | router:117 → T → handler:99–111 → auth_oidc_logic:100–169 | **处理前**扣 IP；失败出口 :623–626 另写 {login_attempt,<<"oidc">>,Ip}；没有对应 is_locked 前置/成功 reset | 一致失败消息；license quota 错误分支不经 fail；OIDC 测试 mock D |
| POST /api/v1/auth/oidc/exchange | router:118 → T → handler:114–121 → exchange | 通用 HTTP IP 桶；不扣上述 Cache 桶，不写登录失败 ETS | 一次性码交换；state/otc 消费安全独立于本报告 |
| quick_login / alipay_login | router:78–79 → T passport → H → P:53–164 的第三方认证链 | 有 HTTP IP 请求额度，未调用 D/S 的账户失败记录/成功 reset | 服务配置决定功能可用；第三方错误不能未经决策混入本地口令失败 |
| Admin POST /api/adm/passport/login 或 do_login | router:1123–1125 → adm_auth_middleware:26–32 放行 → T 通用 IP → adm_passport_handler:130–193 → adm_passport_logic:29–72 | captcha/CSRF/密码验证；没有 D/S 失败记录；实际属于 api_per_ip，不是 passport_per_ip | 业务 error 或管理员 cookie 成功；是否纳入账户锁定需 L1 明确，不能声称已覆盖 |
| 其他登录凭据入口：Moya 微信、企业 OA exchange、QR | 已检索认证 handler 与 D/S 全量调用点 | 未发现它们使用本轮登录失败桶；按路由经现有 HTTP 桶，另有自身凭据合同 | 本轮只保持相关 HTTP 桶兼容，不扩展凭据消费/预算算法 |
| 管理员解锁 / get_lock_info / S:check_ip_rate_limit 包装 | S:103、110、118 → D | 导出可调用；未检出生产 HTTP 解锁/get_lock_info 调用；lock info 的私有构造则在真实 check 中调用 | 属残留/内部能力，不能假装管理员 UI 已接线 |
| 到期清理 | D:init:373 → D:handle_info:453 | 每 60 秒全表 fold；不经 HTTP | 与失败写在同一 server，不能把这条与失败写误报为两个并发写线程 |

HTTP 中间件在 auth/feature gate 后、handler 前（`imboy_app:94–107`）。前置认证或功能门拒绝的请求可能尚未进入计数；并非每个到达端口的报文都占 HTTP 额度。路由真值是 `/api/v1/passport/login`，不能仅按 API 文档文件名中的 auth/login 判定路由。

### 3.2 状态来源与原子边界

| 状态 | Key/权威存储、作用域 | 读/写/清除边界 | 时间、重置、容量/故障 |
|---|---|---|---|
| 账户失败 | {login_attempt,Identifier,Ip}；节点本地 public ETS login_attempt_ets | failure 的 lookup→insert 在单 gen_server 内；is_locked/get_attempts 直接 lookup；reset、解锁直接 delete | 起点 first_fail_at；窗口默认 30 分钟，从首次失败而非达到阈值起算；成功删除；未设条目/Key/队列上限；owner 死亡丢表 |
| 锁定展示 | attempts/is_locked/remaining 来自上述 ETS；locked_until 来自同名 Cache key | 三次读可能交错，另读 config 和 Cache；**不是一致快照** | 无正常 Cache 写方，locked_until 通常缺失；get_attempts 不逻辑过期，可能显示旧次数 |
| 登录/OIDC IP | {login_attempt_ip_limit,Ip}；imboy_cache→depcache | 调用进程 get→比较→set；每次单 set 串行，整个准入非原子 | 初次 1；每次通过续期 3600s；达到上限拒绝不加、不续；没有业务失败退款/成功 reset；共享 Cache 可淘汰/flush |
| OIDC 失败观察 | {login_attempt,<<"oidc">>,Ip}，同账户 ETS | fail 直接 D:record_failure；没有读取这只锁来准入的调用 | 可累计超过 F；看起来有“account_locked”日志不证明请求被锁；成功不清 |
| HTTP passport IP | throttle scope passport_per_ip + Ip；默认 throttle_ets | update_counter 单次原子、并发建 Key 使用默认行；scope reset 删整表与更新元信息分两步 | 默认 10/min；首次 N 次通过，第 N+1 次写到饱和值 N+1 并拒绝，后续拒绝仍调用 update；不退款；全 scope 定时清零 |
| HTTP 通用 IP | api_per_ip + Ip；同上 | 匿名普通路由含 OIDC、Admin 登录；已认证请求走 UID 桶（本轮不改） | 默认 60/min；同上。两只 IP 表无容量/Key 长度上限 |
| Cache 复制/删除 | policy 默认 login_attempt 与 login_attempt_ip_limit 均 local | set:should_broadcast=false；sync legacy SET 接收也拒绝 local；flush/flush(Key) 仍会传播 | **local 不是“不会被远端 flush 清掉”**；全局 flush 可清空 IP 额度；不能作为集群额度 |

完整链条证据：D:55–57、87–193、338–374、377–472；S:57–96、144–169；`imboy_cache.erl:273–298,370–404,457–485,541–542`；`imboy_cache_policy.erl:77,103–120`及`src/lib/imboy_cache_registry.hrl:11–12`；`imboy_cache_sync.erl:75–94,550–571`；`deps/throttle/src/drivers/throttle_ets.erl:18–39`。

### 3.3 配置、时间与部署的当前事实

| 配置/来源 | 当前磁盘可证明的值/生效点 | 限制 |
|---|---|---|
| imboy.login_max_attempts | D:491–495/S:127–130；正整数，否则 5；每次读 | 不是统一规则快照；变更立即影响已有状态判断 |
| imboy.login_lock_duration_minutes | D:500–504/S:135–139；正整数，否则 30；每次重算 expiry | 变更会改变已有记录有效期；已清掉的记录无法恢复，尚在表中记录可能重新变有效 |
| imboy.login_max_ip_attempts_per_hour | D:509–513；正整数，否则 100 | 只改阈值，不改硬编码 3600 TTL；未发现这三键的专门 IMBOY_* override，不能套用“全部环境变量优先”的泛化结论 |
| throttle.rates / driver | driver 默认 throttle_ets；rates 来自 application env，app init 再显式 setup 两只 IP scope | sys.config api_per_ip=60；sys.local.config 为 3000、passport_per_ip=120；无配置 passport 则 app 默认 10。注释中的 5 不是真默认 |
| 环境覆盖 | imboy_env:142–146,326–337：IMBOY_THROTTLE_API_PER_IP、IMBOY_THROTTLE_PASSPORT_PER_IP | 启动 env 覆盖并重建 rates；只 set application env 不自动更新已经创建的 scope。没有读取实际节点 env |
| 代理配置 | elib_req:205–264，config_ds:env；trusted_proxy_ips 默认回环地址，hops 默认 1；IMBOY_TRUSTED_PROXY_IPS 专门覆盖 | hop 支持二进制转整数，但专门 hops 环境注入路径未证实；直连可信才取 XFF 从右第 Hops 段；返回段未解析/规范化为统一 IP 表示 |
| Cache | supervisor 默认 depcache_memory_max=100（依赖解释为 MB）；dsync_enabled、cache_registry_overrides、受管复制元信息可影响路径 | 配置预算不是严格条目硬限；100 MB 也不是进程+索引+队列+日志的总预算 |
| 时间 | elib_dt:now/0 返回基于系统时间的 RFC3339 二进制；add 保持微秒精度；D 比较二进制；throttle 用 system_time(ms) | 不是单调时钟；时钟跳变要验证。throttle_time:25–29 为 now+period，不是自然整点分钟窗口 |

部署材料：`deploy/docker-compose.prod.yml:120–122` 为单后端服务；Helm values 的 backend.replicaCount=1，Helm README:3–11 明确实验性、多副本未验证。只能据此确认**仓内默认部署合同与模板**，不能确认线上实际节点数、代理拓扑、入口负载均衡或已加载 driver。代码允许多 Erlang 节点，不等于共享额度。Mnesia driver 虽随依赖存在，当前配置搜索未见选择它；不将其当成已生效或已证明线性一致的替代方案。

## 4. 九条历史线索逐项核实

| # | 判定 | 本地证据与纠正 |
|---|---|---|
| 1 空 IP、失败提前返回、忽略返回 | **仍存在** | H:176 取 IP，但 :190–192 不传；P:441/540 空 IP；S:74–80 先 IP 检查；P:465/588 丢弃错误 |
| 2 验证码二元/三元匹配 | **仍存在** | S:62 返回 {error,locked,Map}，P:443 只匹配 {error,LockMsg}，:445 默认继续 |
| 3 计数、锁定、剩余、until 同源快照 | **仍存在** | S:145–164 分散读，最后跨到 Cache；D 没有写对应 Cache |
| 4 record server / reset 直接 ETS | **仍存在** | D:94、144、384–420、338–355；清理与 record 同 server，这部分风险应区别 |
| 5 IP get→set 竞争 | **仍存在** | D:179–187；Cache 单次 set 并未合并整个操作 |
| 6 每次续期与“每小时”语义 | **仍存在** | set(...,3600) 每次通过执行；depcache:1097–1106 更新 expiry；是闲置续期累计桶，非固定一小时/精确滑动窗口 |
| 7 共享 IP 桶/前后阶段混合/HTTP 双扣 | **仍存在，但旧描述需纠正** | S failure 在认证结果后；OIDC authorize 和 callback 都在处理前。同一 OIDC 流至少两次占 Cache 桶，HTTP 再扣 api_per_ip；无法从源码认定叠加是已批准策略 |
| 8 IP 与账户归一 | **部分已修复，仍存在其他缺口** | HTTP 与 passport helper 已统一可信 XFF；OIDC 仍无 peer 信任检查，IPv6 文本不规范化；验证码 raw Account 与成功 canonical account 不一致 |
| 9 测试只 mock 核心 | **部分属实，不能一概而论** | DS 有真实 ETS 顺序测试；安全层、OIDC、passport/handler 多 mock 核心。缺真实端到端累计与并发语义证明 |

额外推翻两条旧断言：

- 2026-10-10 旧报告“验证码失败已计数/锁定门已修复”只证明调用点存在，**不证明真实路径生效**；“统一空 IP 已修复”反而与 D 输入要求冲突。
- 旧 Cache W0 文档中的广播行为已不适用于本次默认策略。当前 closeout 文档“账户级保护为有效第一防线”（registry-w1-closeout:421–433）的前提也被本轮真实调用链否定。本轮不改其他任务文档，仅标明依赖前提失效。

可以确认的局部修复：HTTP/passport 的可信代理共用 helper；login_attempt 命名空间 SET 默认不再广播、接收端阻止 legacy SET 绕过。**没有证据把九条主线整体判为全部已修复。**

## 5. 发现、影响与隔离最小复现

以下复现全部**未运行**。通用前置：未来另行授权后，在仓外隔离副本中复制本轮磁盘版本（包括相关未提交/未跟踪文件并逐个核 hash）；独立构建目录、独立 BEAM 节点与注册名、独立临时数据库和回环端口，禁用应用自动迁移及所有真实短信/邮件/IdP 外呼。不能只 archive HEAD。时间/调度测试用隔离测试控制点；生产模块关键计数链不得被 mock 掉。HTTP 用合成账户，任何已有测试 env/ETS/meck 修改串行运行并还原。

### F1（P1，S）：失败事件在真实登录链丢失

链：H:168–192 → P:390–424/440–466 → S:72–80 → D:174–193。最小片段：

```erlang
Ip = <<>>,                         % passport_logic:441、540
_ = login_security_logic:record_login_failure(Account, Ip).
% login_security_logic:77–80
{error, invalid_ip} -> {error, invalid_ip};
{ok, _IpCount} -> {ok, Count} = login_attempt_ds:record_failure(Identifier, Ip)
```

影响：密码错误、错误验证码不会进入账户失败 ETS；即使仅将 S 绕过空 IP，D:87–98 仍拒绝空 IP。IP 桶满还会让一个**已发生**的认证失败不写账户计数，即便以后修好了传参。错误记录不能依赖另一桶“准入成功”。

复现：单节点用真实 handler/logic/S/D，合成有效账户连续 5 次错误密码，另做验证码失败；测试 HTTP 配额只允许合成测试 key 足够次数，避免先被 HTTP 桶挡住。逐次读实际状态，应观察当前仍为 0；修复目标为 1…5 并在后续检查锁定。再将 IP 桶置满，完成一个已在途的认证失败，修复后在账户状态容量充足时仍必须记账户失败；若在途建行容量耗尽则按§7.3置域degraded并拒绝后续请求，不谎称已记账。

修复：把真实 IP 从入口传递，但同时拆开**账户身份键**和**IP 请求准入**，不能简单把 account-only 意图改为账户×IP。每个结果只记录一次，明确处理记录不可用；兼容导出 login/3、do_login/3 的缺 IP 情形不得伪造 IP。

### F2（P1，S）：验证码锁定返回值漏匹配

链：P:440–447 → S:57–64。真实结果为三元组，`{error, LockMsg}` 未匹配而落入 `_`。影响独立于 F1：即使先恢复计数，验证码路径仍绕过锁定。

复现：隔离测试在 ETS 注入该入口实际使用的 {Account,<<>>} 阈值状态（只能作为测试 fixture），调用 do_login_by_code/5，以 consume 是否被调用为观测点；当前会继续消费。修复后必须拒绝且不触碰验证码存储。也测试非预期错误返回不得默认放行。

修复：严格匹配 {ok,true} 和实际错误合同；在 handler 边界维持既有外部 envelope，不把内部 map 任意改成新的公开结构。

### F3（P1，S/I）：不同键、不同事件与过早 reset

链：P:392–424/529–538 的 password 用查回 User.account；验证码 P:442/465 用输入 Account，成功 P:475–485 又用 User.account。user_ds:113–124 → user_repo:132–158 直接按对应字段查询，未见此链统一大小写、区号、别名的身份归一层。未知用户变成空标识，D:is_locked(<<>>,_) 放行，failure 输入又不合法。

当前哪些结果尝试计数：
- 密码 verify_user 返回 error：错误口令、查无用户、删除/禁用状态等；但空密码、非法 email 的提前返回不经过记录。
- 验证码 consume 返回 error 才记；验证码有效但用户不存在/禁用不记。非空用户在 status 判断前先 reset，随后仍可能返回失败。
- OIDC 的 fail 包含用户取消、provider 不可用、state 错误、token 网络/校验/映射失败；quota 分支排除。它们并非都属于“错误凭据”。
- password reset 在 verify_user 已成功、login_resp 已产生之后，但早于 handler 的设备持久化；code reset 更早，早于 status 允许。设备持久化失败不等于认证凭据失败，需业务定义。

复现：合成一个 account≠email 的用户，以 email 失败、以账号成功，检查是否清同一键；再以禁用账户的有效验证码执行，观察 reset 后仍返回错误。修复目标：已存在用户所有允许别名映射同一不可变身份；未知身份使用有界、带类型的请求标识，避免把所有未知用户折成空键。不凭空采用全邮箱小写等可能改变账户相等性的规则。

建议合同：正确凭据且账户状态允许即认证成功，提交一次 reset；下游设备写失败不记凭据失败。错误凭据累计；格式错误、供应商超时/取消、配额错误不累计账户失败，但照常消耗已经准入的 IP 请求额度。该建议须 L1 冻结。Admin 身份单独域隔离，是否增加 Admin 账户锁定需明确，不能悄悄串入普通用户键。

### F4（P1，S/I）：reset 可被旧失败覆盖；锁定详情不一致

链：D:94 → handle_call:384–420 与 reset:144/解锁:338–355 并行。允许时序：
1. failure 在 server 中读到 count=4；
2. 请求进程 reset 删除该行并返回；
3. failure 把本地变量 4+1 写回，最终 5。

任何完整串行顺序的结果只会是 failure→reset=0 或 reset→failure=1；5 不满足此合同。管理员解锁同样在外部 fold/delete，只删达到当前 F 的行，既不是清全部失败，也不是跨 IP 原子操作。其缺省权限 `Uid < 10000`（D:294–302）不是可靠管理员校验，但未发现 HTTP 暴露，不夸大为已公开漏洞；不得在迁移时直接接成新端点。

S:145–164 多次 ETS 查询/配置查询与 Cache 查询造成 count、remaining、is_locked 自相矛盾，正常路径又缺 locked_until。D:get_attempts 不判过期：已放行仍可显示 remaining=0。

复现：以 barrier 在 failure 读取后暂停，再执行真实 reset，再放行 failure；观测最终 5。用非空 IP 调 D 累计至 F，再调真实 S:get_lock_info，Cache 不种同名假值，应观察缺 expiry。推进测试时钟到期但不发 cleanup，检查 attempts/remaining 与允许状态。

修复：所有 mutation（含成功、解锁、过期、清理）有一个明确 owner；一次读取/transition 返回 count、expiry、remaining、locked 的同一逻辑快照。优先“**按结果操作提交顺序生效**”：迟到失败在 reset 后计 1，迟到成功在 failure 后清零，二者都按提交顺序解释。仅业务明确要求隔离旧在途尝试才采用 Generation，并同时定义迟到失败和迟到成功，不单挡失败。

### F5（P1，S/I）：Cache IP 非原子、续期及删除竞争

链：D:179–187 → imboy_cache:set/get → depcache。
- 并发新建可同时读 undefined，都写 1；Count=M−1 时多个请求都获准，各自写 M。
- set 每次通过重写 expiry（depcache:1097–1106），连续低频请求能跨很长时间积累至 M；超限拒绝不再续期。它是逐次续TTL累计桶，不等于精确统计最近一小时事件的滑动窗口，也不是首次请求固定一小时。理想串行且无删除/淘汰时，累计旧事件可能比精确滑动一小时更严格；并发覆盖导致漏计、淘汰和flush导致重置放宽须分别归因，不能从“不是滑窗”直接推导必然超放。
- depcache 清理进程读旧 meta 后 `cast(Pid,{flush,Key})`（:1568–1576），owner 不比较旧 serial，直接 flush（:894–896）。若新 set 在消息处理前完成，旧清理可删除新状态。满内存随机淘汰亦以 Key 异步 flush（:1534–1539）。
- 普通全局/远端 flush 能重置 IP 额度；默认 local 策略仅阻止 SET 复制。

复现：两个请求在 get 后用 barrier 汇合；分别初始为空和 M−1，检查通过数/最终计数。以可控时间连续增加，比较 expiry。拦住旧清理的 flush，先续期，再递送消息；新值会被删的时序应被新实现阻止。高基数填充与强制合成 Cache 压力，观测有效限流项被淘汰后重新放行。

修复：限流权威状态移出普通可淘汰 Cache；原子完成存在检查、逻辑过期、阈值判断和额度占用。推荐OIDC采用首次请求固定窗口，须作为**批准后的语义调整**迁移，不能假称与原续期完全兼容。请求桶拒绝不增加、业务失败不退款；账户 failure 不执行请求准入。

### F6（P1，S/I）：OIDC IP 信任分叉、共享桶与无效失败锁

链：auth_oidc_handler:144–154 → D:212–255，无 peer 参数可核实信任；优先 CF、X-Real-IP、XFF 第一段，IPv6 只验证后保留原字符串。中间件走 elib_req:205–264，两个值可不同。默认 nginx 模板覆写 X-Real-IP、追加 XFF（:58–59 等），未见该模板清除 CF-Connecting-IP；能否被上游边界覆盖需部署确认。

复现：同一合成非可信 peer，分别提供两种有效 CF IP，比较中间件 IP 与 OIDC IP；当前前者稳定、后者变化。做可信代理一/多跳、空头、错误头、IPv6 压缩/展开、IPv4-mapped IPv6。不得向公网进行测试。

OIDC authorize 与 callback 共用 Cache IP 桶并分别扣一次；失败另写 oidc ETS 键，但后续 callback 没检查这只锁。密码失败理论也共享 Cache 桶，当前被空 IP 截断。HTTP 重复扣的是另一种短周期额度，不必然是 bug；缺的是明确策略。恢复 F1 后会首次激活 password 对这只桶的干扰，必须一并决定，不能只补 IP 参数。

修复：OIDC 复用可信 helper 并统一有效 IP 表示；账户失败与 OIDC 请求桶分域。推荐 authorize/callback 保持一个 OIDC 请求域、每次 HTTP 请求各占一次；exchange 是否加入须明确。oidc 失败 ETS 若无准入/运维消费者，应删除无用状态写入并保留有界审计，而不是新增一套隐式账户锁。

### F7（P1/P2，S/I）：HTTP throttle 单次原子不等于窗口与装配正确

链：T:48/103 → throttle:27–29 → throttle_driver:53 默认 throttle_ets → :30–39。正常同窗口稳定 table 下，N 次通过可由单个 update_counter 原子性解释；不能把它误报为 get→set 竞争。

其他缺口：
- `throttle_app:start/2` 按配置逐条 setup；`imboy_app:init_throttle_rates/0` 再 setup；每次创建新 worker/table/timer，并覆写 scope→table 行。旧 worker 的 reset 按 scope 查**最新 table**，因此会清新表。配置 api_per_ip 在两处创建，sys.local 的 passport 同理（S）。
- reset 删除计数和更新 NextReset 非同一操作；timer 延迟时 update 不检查 deadline，旧额度一直拒绝，retry 甚至可能负值。reset 期间响应可携带旧 NextReset；文中“整点清零”注释不符合 now+period（S/I）。
- repeated setup 不幂等，长期重复调用增长 worker、旧 table 与 timer 数；新 scope Key 数量没有容量约束。
- `rate_not_set` 被 T 放行并告警；“账户防线仍有效”注释不成立（F1）。ETS table 丢失等 badarg、非预期 driver 结果没有统一错误转换。

复现：独立 throttle_sup，用一个测试 scope setup 两次；保留两个 worker PID，向旧 worker 发 reset_counters，当前新额度被清空。暂停 reset worker 超过周期，check 仍依赖旧 Count。并发 check/reset 验证响应计数与 expiry 同一代。进程重启时检查 table id、scope 映射与规则恢复；不存在规则与表消失分别测试。

方向：源码已表明原库缺逻辑期限与容量合同，工程推荐确定为两只HTTP IP scope局部迁入D；L1冻结选择，L2混合负载通过后才接入L3。scope周期anchor保留，初始化在依赖启动前拆开，旧worker/timer随受控节点停止退出，详§7.5。其它UID、消息、业务scope保持原库，不改vendor。

### F8（P1/P2，S/I）：资源、重启、未知结果和清理合同缺失

D ETS 没有条目数/字节数上限，Identifier 和 Ip 只检查非空二进制；IP guard 不做地址校验。unknown identifier、伪造 IP、超长输入能增加资源压力（具体 HTTP 上限需实测）。每次 failure 有日志，60 秒全表 fold 占 owner，timer:send_interval 消息可积压；gen_server:call 未显式 timeout，阻塞后调用者退出不证明操作没执行。

D 由 permanent child 持有 ETS，无 heir；owner/节点重启丢全部账户状态。表不存在时直接 lookup/delete 会 badarg，不是返回 0；表存在而 Key 缺失才返回 0。Cache 重启会丢 IP 状态；throttle worker/table 与 supervisor/index 表寿命不同。应用先监听再启动 imboy_sup（imboy_app:130–141），D/Cache 存在短暂未就绪窗口；流量门是否覆盖登录必须验证，不能借 TSID ready 注释推定已覆盖。

D cleanup 与 failure 同 server 串行，因此正常已知写入口下**未证明 D 的旧清理会删 server 新写的行**；但 reset/解锁未串行，且 public 表允许任意本地进程写。Cache 的旧 flush 是另一条已发现时序，不能混淆。

复现：合成并发新 Key 达上限（当前无上限）；已有 Key 与新 Key 分开观测；长 Key、owner 队列、日志量与清理耗时采样。延迟 server 超过 call timeout 后恢复，检查操作是否已提交；杀 owner、重启节点、缺规则、丢表分别记录真实响应和恢复空窗。

修复合同候选：有效限制不被 LRU/随机淘汰；达到容量时先有限清过期，已有Key仍处理；IP新Key准入容量满拒绝，不执行业务；账户在途failure无位置则置账户域degraded，拒绝所有后续认证并按D6受控恢复，不伪装已精确记账、rate_limited或0（§7.3、AC-10/12）。缺 owner/规则、超时与未知提交状态拒绝本次准入，不自动重试记账；业务 error 与服务故障分别映射。重启清空是本地易失方案的真实风险，需 L1 接受或选择持久权威，不能伪装为“仍保留限制”。

## 6. 现有测试的证明范围

| 文件 | 目前能证明的测试意图 | 不能证明 |
|---|---|---|
| test/ds/login_attempt_ds_tests.erl:18–62 | 真实 DS ETS 顺序记录、reset、达到 F 锁定；日志/时间有 mock | handler 空 IP、并发 reset、在途认证、到期/清理、容量、超时/重启 |
| 同文件 :69–79 | mock Cache 的初次 IP 调用 | 实际 depcache 的并发、续期、淘汰、旧 flush |
| test/ds/login_attempt_ds_tests_simple.erl | Key 为 tuple、两输入影响键 | 业务身份归一或账户防护 |
| test/logic/login_security_logic_tests.erl | mock D 分支与空 IP error | 真实计数权威；锁定详情用 mock Cache 假造写方，不能证明真实 locked_until |
| test/logic/passport_logic_tests.erl:123 等 | 密码业务分支、兼容导出 | 安全层 mock 接受任意参数，不能证明 IP 传递或 F 次失败 |
| test/api/passport_handler_tests.erl:110–234 | mock do_login/5、成功/设备错误/口令错误响应 | code 锁定分支、真实累计、跨层参数 |
| test/logic/auth_oidc_logic_tests.erl:46–50 | state/claims/provider 与一次性码逻辑 | D 全 mock，不证明 IP 准入/原子或可信代理一致 |
| test/api/throttle_middleware_tests.erl | mock throttle/helper 下的路由分类、429 | 依赖 driver、重复 setup、真实 scope deadline/capacity |
| test/lib/elib_req_client_ip_tests.erl:58–71 | 非可信 peer、从右数跳数、空头、binary hops | OIDC 是否使用 helper、实际部署代理链、规范 IP 文本 |
| test/api/e2ee_throttle_scope_config_tests.erl | 某些真实 throttle scope 的顺序 N 次配额与未注册结果 | 登录 IP 桶端到端/并发/跨节点，不应挪用为完整验收 |
| test/rest/suites/api_v1_login_SUITE.erl | 真实 HTTP 登录、单次错误密码/未知账户等协议断言 | 重复失败至 F、并发锁定、真实 code 分支；fixture:285–301 会把 passport scope 重设为 300/min |
| 未跟踪 test/lib/imboy_cache_capability_tests.erl | 复制能力门、拒绝受管写 | local 登录额度原子性；不能当作本任务测试 |

上述均为读源码所得的“测试意图”，**本轮执行结果全部未运行**。没有读取历史 beam/日志后宣称当前源码通过。未来对真实登录链可以替代外部供应商或构造合成用户，但 D/S/限流存储必须真实。

## 7. 设计比较与推荐合同（待 L1 冻结）

### 7.1 唯一推荐与适用边界

整理现有 login_attempt_ds（D）和 login_security_logic（S），由现有 supervised owner 串行执行账户状态与范围内 IP admit；不新增 elib_limiter、supervisor、Generation、分片或持久队列。Cache get/set 和单次 update_counter 都不能单独满足窗口、reset、快照与容量合同。**两只 HTTP IP scope 局部迁移**已由源码确定为工程推荐：旧库没有逻辑到期与容量合同，保留它无法不改 vendor 地满足本轮要求；UID、widget、消息及其他 scope 留原库。L1 冻结此选择，L2 混合负载是能否接入 L3 的门，不是 L3 临时选择算法的机会。

一个固定期限计数机制，保留三种 anchor：账户首次失败、HTTP scope 周期、OIDC 首次请求。业务改变仍需 D1～D7 的明确批准。保证范围是同一存活且 ready 的节点 owner、已提交结果、有效规则和容量；不承诺集群共享、跨重启保存、最多只发生 F 次失败或调用者崩溃前未提交事件的持久记账。

### 7.2 最小状态、顺序与时间

状态：Key、Count、ExpiresAt、该窗口阈值/规则版本；scope 另有固定数量规则元数据及 ready/degraded 状态。表由 D 唯一写，不能保留 public 外部写口。每次 transition 同时做逻辑过期、容量/阈值判断、变更及返回快照。snapshot 的 count、remaining、locked、expires、rule_version 来自同一次判断；字段缺失与 undefined 分开描述，不向现有 payload 擅加结构。

- check：不增加失败数、不预扣失败表位置；准入认证时只登记最小请求生命周期观察（下文），ready 且账户域未 degraded 时读逻辑状态，缺 Key 才可作为零。
- failure：已确定错误凭据后提交一次；在提交时刻所属窗口计数，不因达到 F 拒收在途失败，不执行 IP admit。
- success/reset、unlock：同 owner 串行删除；success 点见 §7.4；unlock 清指定规范身份在该账户域全部失败，使用现有权威权限判断，不接新管理端点。reset 不退款 IP。
- admit：先判断并占请求额度，再执行业务；拒绝不加数，下游失败不退款。
- cleanup：仅回收逻辑过期行。最小实现由同 owner 分批读取并删除；若跨回调保留游标/候选，删除时复核当前 expiry 与所见状态，迟到旧 Key 不得删新行。不需每 Key timer。

F−1 时 K 个请求均先 check 获准，其后全部 failure，正常无 reset/到期/故障可达 F−1+K。初值4、一次 failure 和一次 reset，在无其它操作/到期时最终只能0或1。迟到失败在 reset 后重新计1；迟到成功在 failure 后清零。跨窗结果按提交时间归属，不用事后回写旧窗口。

持续时间统一 monotonic **毫秒**；now >= expiry 逻辑过期，即使物理行仍在。wall time 仅从同次快照剩余毫秒换算展示，不能反过来决定额度。账户/OIDC 窗口创建时冻结阈值与期限，后续新窗口才采用新规则，已有窗口不因配置变化复活。HTTP scope 的一致切换另见 §7.5。本地 monotonic 不可跨节点/重启原样复制。

### 7.3 容量、在途失败与操作级故障矩阵

**唯一推荐：显式保守降级，不做容量预留。** 每域条目预算与 Key 长度限制在 owner 内执行；有限清理后仍满，不淘汰有效状态。账户 failure 首次建行失败时，无法精确记录这次失败，原子地设置一个固定大小的账户域 degraded 标志，返回 capacity_lost。该标志不占逐 Key 容量；既有行保留，未记录事件不伪装 count+1。**从这次 transition 起所有本域后续 check 拒绝，包含缺 Key；此前已通过 check 的成功也不能在 degraded 下获 reset 成功凭证并继续签发。** 已在途 failure 可继续更新已有有效行；没有位置则继续返回 capacity_lost。此时域整体精确性已经丢失，snapshot 必须标不可用，不能给一份“完整准确”的总状态。

A 无行→check成功→其他 Key 填满→A失败：本次返回服务不可用而非普通口令错误，下一次 check 仍拒绝。已有 Key 被 reset/到期释放，迟到 failure 遇满同样处理。HTTP/OIDC 新 Key admit 容量满是拒绝**尚未执行业务**的请求，不发生漏记，不必把账户域置 degraded；下次有位置时可重新判断。共享 owner 故障则所有域都不可用。

degraded 不随 cleanup、reset、unlock 或“腾出一个位置”自动恢复。最小恢复方案为 §9.3 关入口、排空/终止旧执行环境、确认旧 owner 不再存活后，经 D4/D6 批准受控空状态重启；额度和锁定会重置。无清空批准则维持拒绝，不能偷偷增加持久化或自动定时恢复。它牺牲可用性，避免丢记录后继续按零放行；L0/L2 必须用负载和容量预算评估触发风险。

下表是**拟实施合同，非现有接口已具备**。U=内部 unavailable（可携带原因但不向客户端暴露），X=提交结果未知，S=同次逻辑快照。所有外部错误按 §7.4；“拒绝”不等于 worker 异常。

| 操作 | 正常 / 命中限制 | 容量满 | owner 或规则缺失 | 超时 / X：状态、动作与重试 | 恢复条件 |
|---|---|---|---|---|---|
| check | 不写次数；未锁 allow(S)，锁定 locked(S)，后者不认证 | 不创建位置；尚未 degraded 的零/已有状态可 check；若在途失败漏记已触发 degraded 则 U | U，禁止认证；不能把缺表/规则当缺 Key | 次数只读；认证准入的monitor可能已登记，本次U并终态取消，未确认则deadline关闭域；不继续认证，可在正常ready时发新请求 | 同一 ready owner 新快照；重启遵循 D4/D6 |
| admit | 当前窗低于 N 原子+1并 allow；已满拒绝、不+1、不退款 | 新 Key 返回 capacity，业务不执行；已有 Key 正常处理 | U、不执行业务，不创建默认无限规则 | 可能已扣也可能未扣；本次 U，不执行业务、不退款、不盲重放；之后新请求重新占额度 | 同一 owner 正常则按实际状态；跨 owner 必须重新 ready |
| failure | 已发生事件+1，包括已达F的在途失败，返回 S；不以 locked 拒收记账 | 原子置账户域 degraded，返回 capacity_lost；既有可更新，整体仍 U | U，不能确认记账；禁止当普通失败完成；缺 owner 后禁止自动 ready | 可能已计/未计；本次 U，禁止重复提交同一事件。owner 对**已收到**消息必须最终记账或置 degraded；超时不取消消息 | 同一发送者的有序栅栏确认处理后可看实际权威；无法确认已送达/owner连续性时关域按D6恢复，禁止推定为零 |
| success/reset | 可登录且凭据正确才清账户状态并回 ok；即使提交时已锁，已在途成功按顺序清零 | 删除不需新增位置；degraded 下返回 U且不授权签发（可保留状态不删） | U，不签发/不返回凭据 | 可能已清零；本次不返回凭据，不重试旧 reset，避免删掉后来的 failure；新登录重新走 check | ready 新请求；已有副作用按 §7.4，不宣称可撤回 |
| unlock | 权限通过后串行清指定身份；锁定/未锁均同一规则，未授权不改状态 | 删除不需位置；degraded 不因解锁恢复 | U，不报告解锁成功 | X，不盲重放；查询后由操作者发起新的解锁意图，不能重放旧操作覆盖新失败 | 稳定 owner 快照或 D6恢复；保持权限校验 |
| snapshot | 不记事件；逻辑过期视0；返回同一版本 S，锁定仍可读 | 无需新行；degraded 返回 U而非完整精确 S | U，不能伪造空快照 | 本次 U；只读可重新查询，不据新快照推断旧 mutation是否执行 | 同一 ready owner；degraded 只能按 D6 |

未知提交不引入幂等队列：单请求串行 check→认证→failure/reset；已发送 gen_server 调用超时后，调用者不得重试非幂等动作，不同发送者的新check不保证排在旧failure后，旧结果仍按实际提交顺序生效，不能用一次新snapshot证明旧mutation未执行。最小生命周期接线由现有D承担：S在发起认证check前生成唯一attempt_ref，D获准时monitor实际执行认证的请求进程，回显此ref；S的终态failure/reset/不计数结束均带此ref，owner处理终态后释放monitor（包括不计数错误、degraded拒绝及主动取消）；monitor/deadline旧消息需按attempt_ref核对，不能误关闭已完成的新请求。它不预扣失败数，也不预留失败行，不记录凭据；额外内存计入已冻结的入口在途并发预算。请求在终态提交前任何DOWN（含normal但漏交终态）或超出冻结认证deadline，D将账户域置degraded；owner自身重启则ready=false。超时但仍活的调用者只可发送同一发送者有序的结束/故障通知，不能重放mutation；若不通知，deadline触发关闭。deadline用owner固定周期的有界批次检查，不创建每attempt一只timer；超载造成的检测延迟纳入AC-16预算。关闭前已获准请求仍按在途合同处理；不能承诺DOWN/deadline被owner观察前零延迟全站停服。普通snapshot/查询不创建monitor，认证生命周期入口与普通check查询在现有D/S内部明确区分，兼容调用必须接入相同终态收尾。这个最小控制状态只为发现未交付结果和排空，不新增一套在途额度算法、位置预留或持久幂等系统。该接线须经AC-12验证异常与下一请求；预算证明不了生命周期元数据有界则L2不得通过。

L1 冻结预算：条目/Key字节、scope固定元数据/timer、cleanup批次、owner heap/mailbox、调用者及日志队列。mailbox 长度预检只是软水位；若要求硬上限，必须有已验证入口并发上界，不能用 busy 分支冒称硬限。HTTP 流量是否饿死 failure/reset/snapshot 由 AC-16 决定。

### 7.4 成功点、凭据副作用与对外协议

本轮补证（S，新增文件指纹见附录A）：

| 真实链 | 已生成、已持久化、已返回的区别 |
|---|---|
| 密码 P:528–591 → verify_user/3:950–973 | 校验和status通过→可选 KDF 写升级→login_resp/3:1007–1033 生成 token/refreshtoken→返回内部 Data→:549 reset（忽略结果）→审计/踢冲突设备→handler |
| 验证码 P:440–522 | consume_code 已消费凭据→:484 reset（忽略结果）→检查status；status1/2才login_resp。现有禁用账户也可能先reset |
| Token DS:113–138、auth_session_ds:46–104 | 签发读 epoch（可查DB并memo），然后签名返回；这条路径没有 Token 行插入。不能把读epoch/memo或生成字符串说成凭据已返回客户端 |
| H:429–440、P:1193–1199、user_device_ds:108–109、user_device_repo:240–302 | handler先真实设备登记（更新/插入持久化）→cast登录成功/读setting→success响应；登记失败为503。之前已生成 Token，不表示已在HTTP返回；KDF升级/验证码消费/踢设备也不等于可撤销 |
| Admin handler:146–180、adm_session_ds:87–104 | 当前无 limiter reset；认证→清CSRF→读取epoch生成sig→设置响应cookie→success才发送。cookie签名本身非新session行持久化。本轮推荐不新增Admin账户锁 |
| OIDC finish_login/3:543–566 | 使用 P:login_resp/2，无账户reset；保留请求桶合同，不增加本地口令失败语义 |

**唯一成功点建议（D2待批准）**：凭据正确且账户允许登录（现有status1/2）后，reset 必须确认成功，才执行签发与后续登录动作。最小整理密码校验与 login_resp 的调用边界，在真实登录路径把reset移到签发前；导出 verify_user/3 的已有返回兼容需保留并检查全部调用者，不给它另加重复计数。code消费不可回滚，若reset失败，本次不给凭据；后续需要重新取得有效code。reset后设备登记失败仍保持已清零，不算错误凭据，沿用503；不声称“全部登录后处理成功”才reset。

若重构过渡中 Token已生成、尚未对外返回，reset失败/未知必须丢弃响应Data，禁止记录Token；已持久化KDF/设备数据或已踢旧设备不能假称回滚。目标顺序将可延后的签发/设备副作用置reset确认后，但不引入跨存储事务或撤销已发送响应机制。已经返回凭据后才发现漏接reset属于验收失败，不能补发错误抵消。

| 条件 | 推荐外部映射 / 差异 |
|---|---|
| 密码/验证码普通失败、锁定 | 保留现有 HTTP200 + code1 + msg字符串 + 空payload + sv_ts；现有LockInfo经safe_to_binary成为字符串，非结构化payload。本轮不扩展字段；因旧code路径会放行，修后出现锁定错误属于预期行为修复 |
| HTTP IP确实超限 | 保留HTTP429、code429、原msg/envelope；内部N+1改N不改变此分支 |
| OIDC确实超限 | 保留authorize现有ERR_BAD_REQUEST业务code及callback失败消息/handler映射；成功302和exchange形状不变 |
| capacity / degraded / 缺owner规则 / timeout / unknown | 推荐统一复用 error_with_status 的HTTP503、code503、空payload、通用“服务暂不可用，请稍后重试”字符串；不伪装锁定或429。这是这些故障分支新增协议语义，D5需批准并在L1读真实App/Admin消费者、L3实测，未批准阻塞接入 |
| reset成功后设备登记失败 | 现有HTTP503“设备登记失败，请重试”保持；不再加失败次数、不退IP额度 |

### 7.5 IP桶旧→新规则与唯一初始化

| 项目 | passport_per_ip | api_per_ip | OIDC请求域 |
|---|---|---|---|
| 入口与事件 | 原路由分类 /api/v1/passport/；在中间件处理前每请求占一次，保留 | 原匿名通用API含Admin/OIDC；鉴权成功UID链不改，豁免/widget分流不改 | authorize/callback各自前置占一次，exchange不额外占；HTTP外层仍先扣，双层策略D3待批准 |
| Key / owner | 旧scope+helper原文本IP，throttle worker拥有表；新scope+可信规范IP，D唯一owner | 同左，scope隔离 | 旧Cache {login_attempt_ip_limit,Ip}；新独立oidc_request+可信规范IP，D |
| anchor / expiry | 旧setup时now+period定时全清，延迟会延长实际窗；新scope统一t0及period，逻辑deadline到期 | 同左 | 旧通过即续TTL3600秒累计桶；新首次准入t0、固定3600000毫秒；不滑动续期（业务改变） |
| 拒绝计数 / 退款 | 旧N+1饱和；新计数停N，拒绝不加；前后均不退款 | 同左 | 旧新均拒绝不加、不退款；移除账户failure对这只请求桶的调用 |
| 配置生效 | 旧setup替换映射但旧worker仍活；新在**下一scope边界**一次切规则，所有Key相同版本 | 同左 | 旧阈值每调用读env、TTL常量；新单Key窗口冻结规则，下次新窗更新 |
| 初始化/恢复 | 旧throttle_app rates和imboy_app重复setup；新D init一次装配；不ready不准入；重启D4/D6 | 同左 | 旧Cache缺Key自动1；新规则缺失不等于零，owner恢复须ready/批准空态 |
| 外部错误 | 超限429保持，故障503待D5；内部剩余数适配 | 同左 | 超限沿原业务错误；故障503待D5；不把任意error匹配到默认继续 |

HTTP正常推进：同scope初始t0=0、P=60000ms，IP首次t=55000也在60000换窗，不到115000；任何admit先算 floor((now−t0)/P)，跨过多少周期就直接定位当前窗，不逐条补发旧timer、不因迟到timer重置新窗。批次cleanup仅回收旧行。现有worker用system time且延迟reset从执行时now+P漂移，这是明确修正，不假称原实现本已满足。

配置先校验并登记pending；下一旧scope边界B统一切换N/P/version，新周期以B为anchor。timer若晚至跨多个周期，仍以已确定B和新P计算当前窗；已有/新Key都先比较scope当前窗口和版本。重复提交配置在生效前按最后一次有效配置替换，整个owner操作原子，配置缺失/非法不得清空限制。账户/OIDC逐Key冻结和HTTP逐scope切换不可混用。禁止“setup一次就是热更新”的隐式重置。

内部N+1→N：两处生产中间件只匹配结果标签与剩余元组，不直接读raw计数；定向src检索无throttle:peek消费者。故真实允许/拒绝和remaining可兼容，依赖raw表/peek的外部运维未证明，须L1列为内部诊断差异；相应测试fixture改到新owner，不能继续setup旧passport300冒充调高新限额。

唯一装配：未来将两IP规则从传给throttle应用的rates中移出（启动env/配置生成处保留原键值来源），imboy_app:init_throttle_rates/0仅移除这两项setup；两只规则迁至imboy应用下最小配置项limiter_ip_rates，D:init读取经校验的快照并持有scope元信息；imboy_env:override_throttle_rate/2对这两只IP规则改写该配置，保留既有环境变量名与默认值，其他scope照旧。未来配置模板、部署配置生成/运行配置来源同步移出throttle.rates，不能仅临时set_env；旧配置仍含两scope则启动校验拒绝ready并报告迁移缺项，不允许旧新混跑。**不能等imboy_app:start才假设throttle_app尚未读rates**；配置源在依赖app启动前即不含这两项。L1审核此装配和所有本地/release来源，L3验证两种启动。推荐受控完整节点停止/启动，旧throttle workers与timer连同VM退出，新VM相关scope无映射；不热改vendor、不逐个猜PID停止，不影响其他scope重建。部署/配置变更只在未来授权执行。

### 7.6 L0旧基线与L2混合负载预算

L0状态改造前保存两条对照，不能用漏记的高吞吐证明性能好：
1. **E2E旧真实成本**：真实handler→P→S→D，合成用户/真实存储，记录实际记账数；空IP漏计如实为0。新路径在相同请求和认证模型下同时报告行为差异。
2. **同有效记账工作量**：旧真实D/Cache/throttle与新owner处理相同成功提交事件数；输入非空规范键，不mock被测状态。报告调用数、成功记账数、拒绝数及每个有效事件成本，不能将两条基准混为“退化率”。

未来 AC-16 固定硬件CPU/内存/OS、OTP/编译档、HEAD+WIP hash、driver、规则、Key长度/分布、并发、认证耗时模型、warmup/持续时间、独立重复次数和测量顺序；这些值由L0输入表冻结后运行，每次同条件复测，保留各次与汇总/离散范围。缺值先标INPUT_PENDING，不填虚构1ms/1000QPS，不测后改阈值。

场景至少：单IP热点；大量不同IP；HTTP admit与账户failure/reset/snapshot混合（比例与账户请求最大允许等待冻结）；cleanup重叠；持续超载直到拒绝/队列稳定后降载恢复。逐类采集吞吐、p50/p95/p99/最大等待、允许/拒绝/unknown及最终count正确性、CPU/reductions、ETS内存、owner heap/mailbox峰值与恢复、cleanup耗时、日志量及队列成本。不能只看总吞吐掩盖账户操作饥饿；超时/异常单列，不能并入正确拒绝率。

预算输入由部署峰值/突发、NAT共享数、活跃Key、认证耗时、内存分配与上层请求deadline推导：owner可用等待=端到端预算减认证/网络/其他处理的保守分配；存储预算含条目开销、进程及队列安全余量。L0可做基线探索；**测候选前**冻结验收预算及推导。输入不足阻塞L1资源合同冻结和L2性能门，不阻塞隔离基线/功能用例构造；单owner无法满足冻结预算则停L3接入，提出最小调整重审，不预先分片，也不放宽预算造PASS。

## 8. TSID、Cache 与其他任务的具体交集

| 交集 | 可独立推进 | 真正需要等待/协调 |
|---|---|---|
| TSID | 失败事件、返回值匹配、reset/snapshot、非空 IP/可信代理都不依赖发号算法。真实计划在聚合根 .Codex/tsid_plan.md，旧 imboy/.Codex 路径不存在 | D1推荐用户ID规范身份，沿用现有正整数与域；user_device_repo:264–302的新设备写会调用elib_tsid:generate(user_device)，L3/L4绑定此接口和独立数据库/构建目录，不等TSID全任务完成 |
| Cache | 当前Cache源码及新抽出的src/lib/imboy_cache_registry.hrl与未跟踪capability test已核对；默认 local SET 隔离和 get/set 非原子均有当前证据。移出限流权威后主体可独立实现 | L3 冻结旧 login_attempt_ip_limit 停写时点、在途旧写排空、flush 影响、混合版本回退。Cache registry 文档更新由后续明确所有者协调，本轮不改 |
| 共享 app/sup/config | 设计和隔离 DS 测试可先完成 | imboy_app:start/2、init_throttle_rates/0、imboy_sup的D/Cache children、imboy_env:override_throttle_rate/2及config_ds:env/3按代码块协调；auth_session_ds:current_epoch/1受Cache任务影响须重绑；这些文件被其它任务修改时协调装配，末次集成重新 hash；不要求停任务、不占其锁 |
| Admin/Flutter | 后端协议可先固定，当前无需改客户端 | 如改变错误 code/msg/payload 或锁定详情结构，必须读实际客户端消费者并做兼容测试后冻结；本轮未据猜测改协议 |

Cache closeout 中的“账号第一防线”例外理由失效，应作为后续协调事项，不意味着 Cache 整项必须暂停。对外不可报告 Cache 当前仍 W0_BLOCKED，也不可把其他任务自己的 PASS 当 Limiter 验收。

## 9. L0～L4 执行与验收合同（均为未来工作）

### 9.1 阶段、文件与门槛

下表D/S/P/H/T简称同§3。生产修改范围必须包含直接调用者、exports、tests、fixtures与启动配置；只在未来实施授权后执行。本轮没有创建下述测试/工具。

| 阶段 | 目标、具体动作及文件/MFA | 前置与专项 | 通过、失败处理、证据及下一门 |
|---|---|---|---|
| L0 基线与隔离复现 | 绑定源码/WIP/依赖/规则；先保存§7.6两类旧基线，再构造F1～F8最小复现。读D:record_failure/reset/check_ip_rate_limit、S:record_login_failure、P登录分支、throttle setup/reset；用现有test/ds、logic、api、rest/support的隔离fixture | 未来实施/测试授权；独立源码构建目录、VM、DB、端口与脱敏fixture，外呼禁用；先读Makefile副作用。AC-01/02/04/07/14/16旧侧；旧代码预期出现指定坏结果 | 缺陷结果必须是目标非法状态，编译/环境失败不算复现；无法稳定复现明确原因，仍可做无依赖项。产物E目录基线/版本/参数/原始结果，L1需解释每F并有预算输入或明确阻塞 |
| L1 合同冻结 | 在本报告冻结D1～D7、事件表、身份、故障矩阵、窗口/配置、错误映射与容量预算。工程上落定两HTTP scope局部迁移；定位imboy_env覆盖、配置模板/release装配及imboy_app:init_throttle_rates移除位置；读取实际App/Admin错误消费者，不凭猜测扩协议 | L0旧基线、部署/代理/业务输入；AC全部oracle纸面审查；D5客户端503兼容影响清单；不要求TSID/Cache全任务结束 | 业务范围/重启清空/错误协议有批准记录，预算在候选测量前冻结；缺项仅阻相应L2/3门。证据为本报告D表批准来源、冻结hash和预算记录；无批准不得标L1完成 |
| L2 最小权威状态 | D:init/handle_call/handle_info统一mutation与snapshot、容量degraded、认证生命周期monitor/deadline及ready恢复；S:check/record_failure/record_success/get_lock_info统一返回和异常传播。受影响test/ds与logic；配置只添加已批准最小参数 | D1～D5相关合同批准；隔离装配与稳定共享文件块；AC-03～13、15、16候选混合负载先验收 | 正确性、容量漏记关闭、未知结果接线与资源预算全部过关；HTTP不会饿死账户操作，才可进入L3。失败分类修复，不能新增旁路或双权威；产E/L2专项和性能原始数据，预算不达标停接入重审 |
| L3 调用、协议与状态迁移 | H:login、P:do_login/3/5及私有密码/code路径/verify_user/3调用兼容，S接线，OIDC handler:client_ip/authorize及logic:callback/fail，T两IP分支，elib_req仅规范化；app初始化、环境/配置装配；test/rest/support/rest_fixture移除旧setup并设真实新规则。Admin只受HTTP桶影响，D7默认不新增账户锁 | L2通过；D6切换清空批准；AC-01/02/09/12/14/17/18/19；稳定Cache旧写边界、token/epoch/设备与TSID接口。先隔离演练，生产切换另授权 | 所有入口无空IP/三元组漏匹配/失败双记/旧Cache回落；reset失败不返回Token，真实HTTP协议符合冻结表；旧scope worker/timer无残留。按§9.3失败就保持关闭；产E/L3真实入口及切换/回退证据，回退候选未验证不得放行切换 |
| L4 综合验收 | 同一最终候选重放§9.2全部AC，完成兼容导出、正常/故障/资源/节点边界和回退；主集成者统一适用全量EUnit及必要强制门，绑定所有受影响生产/测试/配置/工具hash | L3专项和演练通过；候选稳定、隔离环境、批准预算、测试工具已实现。双节点仅隔离本地环境，无真实供应商调用 | 每AC均有原始输出/断言/退出状态/候选绑定；未知/跳过不是PASS；未决风险不冒作已接受。按§9.3回退演练，本地通过不代表UAT/生产验收。最终候选改变，明确失效AC/全量结果并必要重验 |

执行纪律：每阶段只运行受影响专项；同类环境/fixture/测试阻塞最多两次定向诊断重试，之后记录BLOCKED并推进无依赖项；代码、合同、测试、环境问题分别归因，不降断言、不无限重试。L0复现测试与修复后oracle分开，旧实现坏结果是复现成功，不是产品PASS。最终全量入口为隔离候选 `IMBOYENV=local make eunit`；Makefile:148–150的排除项按适用CI/强制门补齐，涉及共享启动扩大装配回归并说明原因。全量统一做不等于失败后禁止重验。

### 9.2 稳定AC与最小追踪

**每项共同执行合同（是下表每行的一部分）**：独立合成状态，无真实用户/外呼；每次run唯一Ref，每worker唯一Ref+monitor（测试收集ref与生产attempt_ref分开），以ready/go/result/正常DOWN确认**全部K个**参与者完成，禁止sleep代替完成。记录每个结果与结束状态；结果遗漏、异常DOWN、超时、未释放barrier都使本用例失败，不得算成拒绝。finally释放屏障、终止并等DOWN、清该run拥有的表/进程/fixture、还原env；不清共享表。逻辑时钟可注入，D/S/实际存储不可被mock；旧复现只在隔离副本插入最小调度钩子，不替换状态实现。超时时限由隔离测试预算冻结，30s框架上限不是业务SLO。

表中每个名称均为**待实现测试函数或场景**，放到指定现有文件扩展；已有同名测试意图见§6不等于已满足新AC。调度实现需编译验证，不提供未经验证代码冒作可运行示例。每行可有两个oracle：L0明确复现坏结果，L2/L3要求正确结果。

命令与证据别名：
- Q(模块)：现有定向命令形式 `IMBOYENV=local make eunit t=模块`，仅未来隔离环境；新AC加到对应模块后才能运行，当前命令只会跑既有用例。REST/负载/生命周期无需强塞EUnit。
- C：`scripts/limiter_v1_acceptance.escript --case AC-编号 --manifest <候选清单>` **待实现最小入口**，仅用于现有runner无法表达的REST组合、负载、节点/切换；L0须实现并校验参数/失败退出传播，缺文件不能宣称可运行。优先复用现有REST fixture/runner，禁止另起通用测试框架。
- E：未来每run在仓外/既有忽略目录保存 `limiter-v1/<candidate-hash>/<run-ref>/AC-编号/` 的输入、调度消息、逐worker结果、实际状态、stdout/stderr、退出码、测量原始值及文件hash。必要正式fixture/测试/最小工具及**本唯一报告**须在后续授权任务纳入版本控制；日志/临时副本可忽略。E不是唯一长期复现来源。每行证据位置为E/该AC，不能用别的候选结果代替。

| AC / F→D→L | 文件、测试名、命令（均待新增场景） | 初始/输入/调度 → 正确结果与识别的错误 |
|---|---|---|
| AC-01 / F1,F3→D1,D2→L0,L3,L4 | test/rest/suites/api_v1_login_SUITE.erl / limiter_real_failures；C | 合成可登录用户，规则F固定，IP外层预算足够；真实password、错误code分别F次顺序完成，每次读真实S快照，下一次尝试也发请求 → 每次+1，F后拒绝且不继续认证；旧版0计数为L0缺陷复现。检测空IP、吞error、只有mock调用但无状态变更 |
| AC-02 / F2→D2→L0,L3,L4 | test/logic/passport_logic_tests.erl / limiter_code_locked；Q(passport_logic_tests) | 真实D置锁，真实S/P执行code，观察consume前屏障与最终响应 → 锁定拒绝，code消费不发生；检测二元/三元错误漏匹配。消费适配可观察但不得mock掉D/S |
| AC-03 / F1,F4→D2,D5→L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_inflight_results；Q(login_attempt_ds_tests) | 初值F−1、K>1，固定窗/健康/容量足够；全部check结果收到后一次释放K个failure → 最终F−1+K，下一check锁；另调度reset前后迟到失败分别验证0/1、跨窗failure计新窗；检测错误预扣、阈值后丢失败/回写旧窗 |
| AC-04 / F4→D2→L0,L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_reset_interleaving；Q(login_attempt_ds_tests) | 旧版初值4，钩子停在读4后，外部reset完成再放写，L0应见5。新版向owner异步发failure/reset，再释放barrier并收全结果，分别强制两种提交顺序 → 0或1且快照自洽；不能暂停owner后同步等待它reset。检测外部delete与旧值回写，不把自造死锁算实现失败 |
| AC-05 / F4,F8→D1,D5→L2,L4 | test/logic/login_security_logic_tests.erl / limiter_snapshot_unlock；Q(login_security_logic_tests) | 同身份两登录别名、F附近状态，顺序snapshot及异步failure/reset/unlock → 每份count/remaining/locked/expiry/rule一致；未授权unlock不改，授权清规范身份；检测跨Cache伪expiry、混代读、Uid小即授权。用真实权限fixture，不新增HTTP解锁端点 |
| AC-06 / F5,F7→D3→L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_admit_race；Q(login_attempt_ds_tests) | 固定窗口、规则稳定、owner正常、容量充足，当前剩R、K个同IP并发admit → 全部完成后恰min(R,K)允许，余拒绝，count停N；业务失败后不退款。检测get/set丢数、N+1目标残留；unknown不允许混入此数量断言 |
| AC-07 / F5→D3→L0,L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_cache_old_flush；Q(login_attempt_ds_tests) | 旧真实depcache在旧过期meta生成flush后暂停，续写新expiry再递送旧flush；L0观测新值丢失。新版同样迟到消息/cleanup批次不得改变D新状态，下一admit按新count拒绝 → 检测仍依赖Cache/裸Key删新。Cache与D表分别标名、禁止把同owner回调虚画成并发 |
| AC-08 / F4,F5,F7→D3,D5→L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_time_rules；Q(login_attempt_ds_tests) | monotonic毫秒测expiry−1/expiry/expiry+1；账户首次失败、OIDC首次请求、HTTP t0=0/P60000/IP55000；不发cleanup，scope时钟跨三周期；改wall前后、登记pending规则 → 到边界逻辑归零，HTTP60000换窗且各Key同版，账户/OIDC原窗冻结；清理目标D表时新窗行存活，后续admit/failure仍正确。检测单位错、115000换窗、timer漂移、规则复活 |
| AC-09 / F3,F6→D1,D3,D7→L3,L4 | test/lib/elib_req_client_ip_tests.erl + test/logic/auth_oidc_logic_tests.erl / limiter_identity_domains；Q(elib_req_client_ip_tests)、Q(auth_oidc_logic_tests) | 可信/不可信peer与伪CF/XReal/XFF，等价IPv6文本；同账户多IP、别名，不同账户/未知类型、OIDC/Admin边界；authorize/callback/exchange顺序请求 → 规范Key相同/隔离符合D1；OIDC前两各扣一次，exchange不额外扣，失败不写无消费者账户锁；检测信任分叉、别名绕过、双记；供应商可替代 |
| AC-10 / F8→D5→L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_capacity_during_auth；Q(login_attempt_ds_tests) | A无行check通过，在认证barrier停住，其他Key填满M，释放A失败 → capacity_lost、次数不得假称已精确+1、域degraded；**A下一次及另一缺Key账户check均拒绝**，即使reset/cleanup释放位置仍拒绝。再从已有A被reset/到期删除后重复迟到failure → 同合同。检测仅返回error却随后按0放行 |
| AC-11 / F8→D5→L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_capacity_bounds；Q(login_attempt_ds_tests) | M容量、K个新Key、已有锁、长Key/不同IP、cleanup并发 → 从不越条目上限，有效锁不被挤掉；IP新Key capacity不执行下游，腾位后新请求可admit；账户degraded仅按AC-13恢复。记录全内存/队列而非仅ETS；检测先查size再插、随机淘汰、软水位冒充硬限 |
| AC-12 / F8→D2,D5→L2,L3,L4 | test/logic/login_security_logic_tests.erl / limiter_unknown_outcomes；Q(login_security_logic_tests) | 分别对check/admit/failure/reset/unlock/snapshot，在提交前/提交后回包前屏障超时、owner异常、规则缺失；恢复收状态与新请求 → 按§7.3矩阵，不重复记账/退款/旧reset，不返回凭据；已收到failure最终计或degraded，请求异常DOWN或deadline在owner中处理后域degraded并阻断下次，未观察前仅保证已声明的在途边界；缺表不作0。检测吞错/默认放行；所有timeout为预期单独分类，不充当AC-06拒绝 |
| AC-13 / F8→D4,D5,D6→L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_restart_recovery；C | 活跃锁/耗尽IP/账户degraded分别杀owner和停节点，重启未批准ready及批准受控空态两组 → 前者入口拒绝，后者清空风险明确、规则初始化一次，新请求重新正确累计；清理/重启不偷偷解degraded，monotonic旧值不复制。检测重启自动无限放行/假保留锁 |
| AC-14 / F7→D3,D6→L0,L3,L4 | test/api/throttle_middleware_tests.erl + test/rest/support/rest_fixture.erl / limiter_scope_owner；Q(throttle_middleware_tests)、C | L0旧setup两次，旧worker reset清新表；目标两IP装配一次、重复初始化不得重置有效额度，延迟旧timer到达不得改新窗；检查worker/ETS映射及fixture新规则，真实请求至阈值 → 无旧IP worker/index/Cache回落，UID等其它scope仍原库。检测只改调用不改配置、旧worker复活、fixture假调限 |
| AC-15 / F5,F8→D4→L2,L4 | test/ds/login_attempt_ds_tests.erl / limiter_node_boundary；C | 两隔离节点同规则同IP各发N，稳定窗 → 本地合同各允许N，合计2N必须如实报告；若用户要求共享则此结果不合格、停止本地方案。检测把ETS原子性/Cache复制/粘性会话当全局额度 |
| AC-16 / F7,F8→D3,D5→L0,L2,L4 | scripts/limiter_v1_acceptance.escript / mixed_baseline；C | §7.6冻结输入/旧新两条对照，五类混合负载，全部worker完成后继续恢复阶段与账户请求 → 正确count/准入、每类延迟在预先批准预算，mailbox和内存按预算恢复，cleanup/logs不饿死账户操作。检测以旧漏计作优势、总吞吐掩盖饥饿、调低门槛造PASS |
| AC-17 / F1,F2,F4,F8→D2,D5→L3,L4 | test/rest/suites/api_v1_login_SUITE.erl + test/api/passport_handler_tests.erl / limiter_success_order_protocol；C、Q(passport_handler_tests) | status禁用/允许、真密码/code、reset失败与unknown、已reset后设备写失败；观察签发/写/返回顺序及HTTP envelope，OIDC302/Admin cookie基线 → reset未确认无Token/Cookie返回；禁用不reset，登记失败保持已reset且503；其余映射§7.4。检测早reset、错误凭据泄出、把不可逆消费说成回滚；真实D/S与设备存储，签名可观察但不外呼 |
| AC-18 / F5,F7,F8→D4,D6→L3,L4 | scripts/limiter_v1_acceptance.escript / cutover_rollback；C | 合成锁/剩余额度，在途password/code/OIDC/HTTP请求及旧Cache writer/timer；演练§9.3每步、超时、候选失败与真实入口回退 → 关闭期拒绝、全部旧任务结束后才ready、无旧桶复活；安全候选真实累计/超限有效，没候选就继续关闭。检测只回代码、不排空/回失效旧版本 |
| AC-19 / F1,F3→D1,D2,D7→L3,L4 | test/logic/passport_logic_tests.erl / limiter_exports_compat；Q(passport_logic_tests) | 逐个列出现有login/3、do_login/3/5、verify_user/3调用者，HTTP与内部合成账户各走一次成功/失败，结果全部收齐 → 返回兼容、逻辑边界不重复记账/签发、不把低层verify_user当HTTP绕过保护；quick/alipay/Admin只原HTTP保护。检测修签发顺序破exports或暗扩范围 |

### 9.3 切换、降级恢复与回退（D6）

**本轮没有已验证回退候选，ROLLBACK_NOT_READY。** 推荐最小方案为批准后的受控停服/空态重启，不做热滚动双写或持久化迁移。以下是未来演练/部署合同，既不是当前已存在运维脚本，也不是本轮执行授权。

| 步骤 | 动作、排空确认及有界失败处理 | 各类唯一权威 |
|---|---|---|
| 0 准备 | 冻结候选代码/配置/规则及可验证回退候选；AC-18隔离演练。记录关闭入口机制与验证探针：优先部署入口维护响应并阻断直连旧backend；现有配置未证明此机制完备，L3须补接线。等待上限取已冻结HTTP/认证/provider/owner截止期的保守上界及关闭传播时间，不用无依据秒数 | 账户旧D ETS、OIDC旧Cache、HTTP旧throttle；只读准备 |
| 1 关闭 | 关闭passport全路由、走api_per_ip的匿名通用API（含OIDC authorize/callback/exchange、Admin登录及其它实际匹配路由）新准入；禁用内部login兼容调用和相关运维直写。不是只关/login；其他UID scope不借机迁移，但整节点停机会影响其可用性须维护窗口接受 | 旧权威仅供已在途任务完成；外层503维护门只负责拒绝，不冒充独立限流 |
| 2 排空 | 追踪关闭前HTTP请求进程/内部调用进程、认证/provider回调、failure/reset/admit及可能迟到的旧Cache写。逐PID monitor确认结束，记录唯一Ref结果；owner栅栏仅在所有生产者结束后可证明先前队列排空，队列瞬时0单独不够。到冻结deadline仍未排空：保持维护门，终止旧隔离/部署VM并确认进程/节点退出；不能带未知旧写开放新节点 | 仍旧权威；无新请求。已生成凭据不得在关闭超时后绕过门发送；已返回凭据不假称撤销 |
| 3 停旧执行环境 | 完整停旧节点，确认旧D、旧Cache写者、throttle相关workers/timers及旧scope index生命周期结束；包括load balancer移除与直连阻断验证。停止VM避免热退出遗漏重复setup的匿名worker | 不存在活跃额度owner；入口继续关闭，禁止启动旧路径作为兜底 |
| 4 准备新状态 | **推荐受控清空**，D4/D6批准明确“账户锁与IP剩余额度重置”。配置两scope不再交throttle_app，D初始化ready=false。旧续TTL IP没有无损新窗anchor，旧账户Key→新身份也不保证可逆，故不猜测导入；旧Cache遗留行即使存在也永不作为回落读取 | 新D为唯一拟启用权威，尚未准入；其他throttle scope在新VM照常初始化 |
| 5 验证并开放 | 关闭状态下仅合成探针经受限直连访问原真实路由（不新增生产API、不恢复普通直连），进入受控验收路径：真实password/code累计/锁定、IP到阈值、OIDC前置拒绝、无旧setup/映射、错误/恢复；清理仅探针状态。候选/规则/预算/回退证据全绑定后才ready并开放；任一失败继续维护门 | 账户/OIDC/HTTP IP均新D；HTTP同owner，**不是独立后备防线** |
| 6 回退 | 再关闭相同入口，排空/停新节点。仅允许切向已通过本报告关键AC且保留安全状态合同的候选；禁止回到空IP丢记账、旧Cache权威、rate_not_set放行或旧两scope setup。按批准空态策略重启并重做真实入口累计/超限/503/无凭据泄出验收后开放 | 验证过的回退候选D为唯一权威；没有候选或验证失败则保持相关入口拒绝，不恢复已知失效账户保护 |

跨重启保留若成为业务硬要求，推荐空态方案即不适用，D4/D6和L2权威设计重新打开；不在执行中临时加导入器。任何未来导入只可携带可解释的剩余时长与导出wall时间/误差上界，再在新节点以本地monotonic重建，误差按保守延长拒绝处理；无法界定停机时间/时钟偏差则拒绝导入或采用另行批准的清空，不能直接复制原timestamp。此说明是约束，不是本轮新增第二套实施方案。

## 10. D1～D7、批准边界与就绪状态

本轮用户仅批准文档修订；下表“推荐已落定”不代表业务批准/实施授权。批准证据必须是明确人类决定及其适用部署/版本，不能引用本报告自身作批准。

| 决策 | 当前事实与唯一推荐 | 前提、风险及行为变化 | 批准/测量、阻塞阶段与AC；未批准仍可继续 |
|---|---|---|---|
| D1 身份与锁范围 | 旧Key含Identifier/Ip，真实路径空IP且别名分叉。推荐已知用户以部署内稳定用户ID+账户域跨IP锁；未知身份用类型+规范化有限长标识，不混Admin/OIDC | 别名解析必须沿现有user_ds/repo，未知账号错误文案不增加枚举信息；跨IP归并会改变旧键语义，NAT/IP独立；使用既有正整数TSID，不等TSID全部收尾 | **待业务批准**身份/跨IP；阻L2键合同、AC-01/05/09/19。可继续L0复现、IP信任工程核验 |
| D2 失败事件与成功点 | 现有code reset早于status，密码Token早于reset。推荐错误凭据计失败（含未知账户），空值/格式、取消、服务故障、已确认禁用不算错误凭据；凭据正确且status1/2才reset确认后签发；结果按提交顺序 | 不增加在途配额；错误密码即使账户禁用仍按实际错误凭据处理，不能靠事后status覆盖分类。未知账号在校验前用D1键。code消费/KDF等副作用不可撤销；设备后处理失败不回补账户失败 | **待业务批准**事件表/成功点，工程顺序已推荐；阻L2返回合同/L3接线，AC-01～05/12/17/19。可继续L0与消费者census |
| D3 IP范围/窗口 | 旧HTTP周期timer、OIDC续TTL且与failure混用。推荐两HTTP scope局部迁D、保留scope anchor；OIDC独立首次请求固定小时窗，authorize/callback各占一次，保留HTTP外层、不退款 | 旧N+1诊断计数变N、timer延迟不延窗、OIDC语义及双层叠加需明确接受；同owner故障影响两层。阈值沿当前配置来源，NAT容量预算待测 | **工程迁移选择已落定；业务窗口/叠加待批准、混合负载待测**。L1不得留两套实现选择；阻L2性能门/L3迁移，AC-06～09/14/16。可继续旧基线 |
| D4 节点与重启 | 默认部署材料单backend，不是线上实况；旧本地状态易失。推荐一期仅节点本地、重启关门后受控空态，明确不保证跨节点/重启保存 | 需要实际部署拓扑和重启安全要求；多节点合计额度可倍增，清空锁定安全成本显式接受。若必须共享/持久，停止本地方案并另审已有PG权威设计 | **待部署/业务确认**，非生产已批准；阻L2权威承诺与AC-13/15，L3开放。可继续单节点隔离复现 |
| D5 容量/故障/协议预算 | 当前无容量且异常未闭合。推荐§7.3容量漏记触发账户域degraded，故障503、安全拒绝、unknown不重放；保留正常/超限协议 | 可能造成整个账户域不可用，HTTP同owner可能排队；精确性仅在声明前提内。条目/队列/时延预算及503消费者兼容必须先定；无证据不能接受性能退化 | **待批准**拒绝/503策略，**待输入与测量**预算；阻L1资源冻结、L2混合门/L3协议，AC-10～13/16/17。可先测旧基线及故障oracle |
| D6 切换/回退 | 无已验证安全回退候选，旧账户链已知失效。推荐§9.3完整关闭/排空/停节点，明确批准清空，再验证开放；没有安全候选保持关闭 | 维护期间相关匿名API不可用，完整VM停止还影响其它业务连接；回退不能只填旧SHA，不能恢复旧桶。monotonic不可直接迁移 | **待批准**维护与清空，**待验证**回退候选、排空截止期/入口门；阻L3切换/AC-18及L4回退门。可继续隔离实现和演练，生产切换另授权 |
| D7 Admin/OIDC无效失败锁 | Admin无D/S账户锁；OIDC失败ETS无准入消费者。推荐本期不新增Admin账户锁，移除OIDC无消费者账户写，保留有界审计与请求桶 | 不能宣传全部认证账户已覆盖；Admin仍只现有自身认证+HTTP额度，OIDC日志变化需确认无外部运维依赖 | **待业务范围确认**，工程不新建无用锁；阻相关L3接线，AC-09/19。可继续核心password/code与HTTP准备 |

集中待决定项只有上述七个原编号：D1/D2/D7为身份、事件和范围；D3/D4为窗口与部署保证；D5为故障协议/预算；D6为维护、清空及回退。无需对每条AC另立审批。未批准可做的未来工作仍以获得实施/测试授权为前提，不把“可继续”当本轮授权。

**分开判定：**
- 文档：本轮五组合同已贯穿摘要、§7、D表、L阶段与AC，可供评审；不自评“9.9已达成”。
- L0：静态材料和复现/旧基线测量合同已具备；实际启动还需未来实施/测试授权、隔离资源与可编译fixture/最小工具，当前 **NOT_STARTED / ENVIRONMENT_NOT_VERIFIED**。
- L1：工程推荐明确，业务批准与预算输入尚缺，**DECISIONS_PENDING**。
- L2：未实现；需D1～D5相关批准、容量/故障接线与性能预算，随后真实专项/混合负载验证；不能把文档静态检查当通过。
- L3：未实施；需L2通过、D6维护清空批准、稳定共享接口、启动配置装配和已验证回退候选；**ROLLBACK_NOT_READY**。
- L4：动态专项与最终适用全量均 **NOT_RUN**；无UAT、生产、集群共享额度或上线结论。


## 附录 A. 本轮逐文件证据指纹

读取UTC、真实SHA-256与本轮HEAD/暂存区归属如下。SAME=磁盘等于HEAD及index；WIP=磁盘有差异；LOCAL=主仓无HEAD blob（含ignored依赖/配置、未跟踪文件及仓外TSID计划）。不输出敏感配置内容。MFA与行号在§3～§8、§7.4补证表列明；以下哈希不是业务运行结果。

| 文件（相对后端仓） | 读取 UTC | SHA-256 | 归属 |
|---|---|---|---|
| `src/ds/login_attempt_ds.erl` | 2026-10-13T03:27:13.105576+00:00 | `2131b808714d45d755e28edf87347ebe4a22ddf6751af03eca4699be540d8fce` | SAME |
| `src/logic/login_security_logic.erl` | 2026-10-13T03:27:13.129836+00:00 | `7d93fcf12fb43fd0b954d174f4b02449e1582eba156802577d81bb61fcdebbbf` | SAME |
| `src/logic/passport_logic.erl` | 2026-10-13T03:27:13.154754+00:00 | `5b6b355ae9a77bfc2958919320fd764ced1c8826c19d5fcb68e483f390b89506` | SAME |
| `src/api/passport_handler.erl` | 2026-10-13T03:27:13.176373+00:00 | `d4cedf3ac8584e743fe588dce8bc19daea453f5d3031781662093775d36f7bf0` | SAME |
| `src/api/auth_oidc_handler.erl` | 2026-10-13T03:27:13.195887+00:00 | `6efca77dd3a6532b9c0327b83700a3fb8b28d744b4c7d73b3673101f80d167a4` | SAME |
| `src/logic/auth_oidc_logic.erl` | 2026-10-13T03:27:13.214934+00:00 | `8a65889f98e4ac427a2f5a822985e506ba4803fb3b9e554f742566272da1acee` | SAME |
| `src/api/throttle_middleware.erl` | 2026-10-13T03:27:13.236925+00:00 | `d46a1f08dad0bb28c14edbaa5ac8ec1b08ecb693f6c97a91ad2d2dbbaf0368d6` | SAME |
| `src/lib/elib_req.erl` | 2026-10-13T03:27:13.256100+00:00 | `632968d86d94558bb967bec635956964faef8ca868bdf362c76ec9f1ce126697` | SAME |
| `src/lib/elib_dt.erl` | 2026-10-13T03:27:13.276913+00:00 | `2818862237946762b6fddcd0665b74e004904ed0fc4850d8108848b828aea463` | SAME |
| `src/imboy_app.erl` | 2026-10-13T03:27:13.298760+00:00 | `515d6b5583e5d4f3801f8e43685df91a6e140ce8a56802bab8873d74fb50bcb5` | SAME |
| `src/imboy_sup.erl` | 2026-10-13T03:27:13.318583+00:00 | `a65f9f128f2f3af5428700ee99f019278fc1fa8510607b248cacdfb13e641b23` | SAME |
| `src/imboy_router.erl` | 2026-10-13T03:27:13.339336+00:00 | `22bb76fc4f07f035595e4780ba0717eece3adfb56ca8020abcb78e919f486a4b` | SAME |
| `src/lib/imboy_cache.erl` | 2026-10-13T03:27:13.360345+00:00 | `062fa1fa3a0d9c8ed34352b5ee070402b1535b851e4073e5cd1e96a9bc4ecab6` | WIP |
| `src/lib/imboy_cache_policy.erl` | 2026-10-13T03:27:13.381211+00:00 | `dd4bd48072014c9f983e9504ed40a8f44c53418d3e9ba6cae1d0c6e0081c87b7` | WIP |
| `src/lib/imboy_cache_sync.erl` | 2026-10-13T03:27:13.400646+00:00 | `ab72a234c84ec3e6c4cd1f82805b79065a2a8b95d614f13fdbadafdddfbd095b` | WIP |
| `src/lib/imboy_cache_sync_apply.erl` | 2026-10-13T03:27:13.419773+00:00 | `15e19da3cadd198e13b29012a918cd93b41ec1ce4bdd4d667b41efc277eb55ae` | WIP |
| `src/lib/imboy_env.erl` | 2026-10-13T03:27:13.441084+00:00 | `627c06df1b5282d2a8494d65722386394b604b27d24a49d411f04035d17b2cf2` | SAME |
| `Makefile` | 2026-10-13T03:27:13.464498+00:00 | `1687afbada66344dd542433000d6270e87e280fa24f34c20b70aebbb6a1227a1` | SAME |
| `deps/throttle/src/throttle_sup.erl` | 2026-10-13T03:27:13.487725+00:00 | `30c39564017bcdc2fe35c8853c4c53fadd0c9f5ceb407417e66c936a542dd619` | LOCAL |
| `deps/throttle/src/throttle_time.erl` | 2026-10-13T03:27:13.514301+00:00 | `911b991b565bd55bab1326aaf63427da76d441477daa522fec16b78029d667e1` | LOCAL |
| `deps/throttle/src/throttle_app.erl` | 2026-10-13T03:27:13.537620+00:00 | `61180a557f511a46be3793d811cdfd64df3f8acc839356297cc5ce26b26535ae` | LOCAL |
| `deps/throttle/src/throttle.erl` | 2026-10-13T03:27:13.561071+00:00 | `532659f1947136d4dbee8e3757260446052a6fbe94c6f5dd963a6731bb3665a1` | LOCAL |
| `src/lib/elib_response.erl` | 2026-10-13T03:27:13.585928+00:00 | `1d61f80a39cf962dfc30411892b27f60d3699d5d89c99d3eb21bee76f7ffd225` | SAME |
| `src/lib/elib_cnv.erl` | 2026-10-13T03:27:13.610836+00:00 | `06e7b2ed9a26f6d24ede55b389e31efbcb8879d19e376e2dc41636ba86f535cd` | SAME |
| `src/ds/config_ds.erl` | 2026-10-13T03:27:13.637950+00:00 | `1e82037d21288c2072063485c877fad010d108c7225d6d418c523c27974470f7` | WIP |
| `src/lib/imboy_env_overrides.erl` | 2026-10-13T03:27:13.661559+00:00 | `818b5c38d1b232c9e39cfe4e5d6b4b5fe42ed39fc13b97aba1d7a38894289d98` | SAME |
| `src/adm/adm_passport_handler.erl` | 2026-10-13T03:27:13.684271+00:00 | `7a282812c030ffcb4be5113384224abb4b1d3ea16a9b89c13e4c6e2224572abc` | SAME |
| `src/logic/adm_passport_logic.erl` | 2026-10-13T03:27:13.706918+00:00 | `557e033219168e08066155f9222240f02b5713f7c5c5a8af50a6659ef1ff35f3` | SAME |
| `src/api/auth_middleware.erl` | 2026-10-13T03:27:13.728255+00:00 | `d8cab6708580caf8116ff8e1e97aa5e32898d19dfb3d12073be55a37a25ca34b` | SAME |
| `src/api/auth_middleware_api_v1.erl` | 2026-10-13T03:27:13.750965+00:00 | `af186802f8128b6d88bdf5483a2e7426bf95773ec73e479396f92f4bcb2709bb` | SAME |
| `src/adm/adm_auth_middleware.erl` | 2026-10-13T03:27:13.772030+00:00 | `0adb0c4fd521c53e87382859455507b2435597e3fdba3c2d2ad532b76810a61a` | SAME |
| `src/ds/user_ds.erl` | 2026-10-13T03:27:13.792484+00:00 | `f080e00c4049b81678448211f7bfd56a440f2dcfec8bd8ee9de44b42fcf167f2` | SAME |
| `src/repo/user_repo.erl` | 2026-10-13T03:27:13.813811+00:00 | `dfb9fb49152e54a66068f3b20655b0ef7982e83c6b6533956a733784f511dd57` | SAME |
| `src/ds/verification_code_ds.erl` | 2026-10-13T03:27:13.834108+00:00 | `0a758cee75cf97a609479e7051905a0faf9d1b0377d59f283294c695c596c3c5` | SAME |
| `deps/throttle/src/drivers/throttle_driver.erl` | 2026-10-13T03:27:13.853086+00:00 | `f8e193f0ca521c54fd5b5f42ded93ecbdafc575e73cc59886e256b0d02cd1c77` | LOCAL |
| `deps/throttle/src/drivers/throttle_ets.erl` | 2026-10-13T03:27:13.872617+00:00 | `9e51ceacb569273d6156a94c6b68e4a4b7d9b888f4533291ba6c44ebd304ef51` | LOCAL |
| `deps/throttle/src/drivers/throttle_mnesia.erl` | 2026-10-13T03:27:13.891125+00:00 | `77ea1132e66d3c726935132b29c4ff5c275c1103cbfa489533f1912e85ddec86` | LOCAL |
| `deps/throttle/src/throttle.app.src` | 2026-10-13T03:27:13.908596+00:00 | `b8842a9581578e721d01d4fa65f2c886c8c81a82c68cd324f0a4ac3b050877d4` | LOCAL |
| `deps/depcache/src/depcache.erl` | 2026-10-13T03:27:13.926493+00:00 | `6678e09af2322592d6ae01e4c834ecee1b822d3f87ec7576323d9d1f5f0252e5` | LOCAL |
| `test/ds/login_attempt_ds_tests.erl` | 2026-10-13T03:27:13.945009+00:00 | `4ae400db84268db8dbf7634fc4358fe87545430212e16f9bdaab81a86414e6c8` | SAME |
| `test/ds/login_attempt_ds_tests_simple.erl` | 2026-10-13T03:27:13.965855+00:00 | `6c9cb05f16293272f9cbf13abca488b2af1d7e1f1a3ba94b7de82d66c27b261e` | SAME |
| `test/logic/login_security_logic_tests.erl` | 2026-10-13T03:27:13.995802+00:00 | `b66ba54f83fa0b81bc0c2bc77f74ed848792d89d6eaa39064ab6365620cea04b` | SAME |
| `test/logic/passport_logic_tests.erl` | 2026-10-13T03:27:14.018035+00:00 | `619457770c511a0e7c3d2281e7f642115d7408df0f63ee6d9bbd8b586943293b` | SAME |
| `test/logic/auth_oidc_logic_tests.erl` | 2026-10-13T03:27:14.036674+00:00 | `76a47dc8cf237ad521b8d80d01ddeddd1b7d5da183f4aee5e2e6f8b055101e3d` | SAME |
| `test/api/passport_handler_tests.erl` | 2026-10-13T03:27:14.056079+00:00 | `086b5f9f0af459914ed0061b8b51da80f6beda290fc77653f74a4b86557bc1a1` | SAME |
| `test/api/throttle_middleware_tests.erl` | 2026-10-13T03:27:14.074329+00:00 | `5f1fcc0a3f4e3c74461813ec695b6480968355e1724f916f96186d853c7838e5` | SAME |
| `test/lib/elib_req_client_ip_tests.erl` | 2026-10-13T03:27:14.093403+00:00 | `a79ad8ecfdf3dc351d60c864431b8d907caeffa26363513d40ea81005c054bde` | SAME |
| `test/api/e2ee_throttle_scope_config_tests.erl` | 2026-10-13T03:27:14.115352+00:00 | `c102c120f6d262696459c2992fa9531ffce789eae8e87a1e30da71eee073ab46` | SAME |
| `test/rest/suites/api_v1_login_SUITE.erl` | 2026-10-13T03:27:14.139028+00:00 | `78b90c38916b5acd6aaad6799d4082b07653ba6be18b68ebaa51db6ae7a5cfad` | SAME |
| `test/rest/support/rest_fixture.erl` | 2026-10-13T03:27:14.159011+00:00 | `609412b240c3ae895e06d924dc92d83f36405ca04a2040426ab8c57de2fad215` | SAME |
| `test/lib/imboy_cache_capability_tests.erl` | 2026-10-13T03:27:14.179267+00:00 | `f476408da0e1366efd75cd0939c6414de385c59148fd6e62c1c26657658f6836` | LOCAL |
| `config/sys.config.example` | 2026-10-13T03:27:14.202515+00:00 | `d2fc19f941740e1af2015aa61545ed4b83033614fef6ea6fca0533245e41a354` | SAME |
| `config/sys.config` | 2026-10-13T03:27:14.227023+00:00 | `0e4f302ad97c57beaa415f62002c10773a17df9623356f1794c841c0e0f1aef6` | LOCAL |
| `config/sys.local.config` | 2026-10-13T03:27:14.249951+00:00 | `3c1409eae7aaf6a946f63878f70f1b1885f83fce30dab801f1171c8711dab78d` | LOCAL |
| `config/sys.runtime.config` | 2026-10-13T03:27:14.272467+00:00 | `0e4f302ad97c57beaa415f62002c10773a17df9623356f1794c841c0e0f1aef6` | LOCAL |
| `deploy/docker-compose.prod.yml` | 2026-10-13T03:27:14.294663+00:00 | `80b806e60b2264691f170912255594a3d943454a46dbd8b8b2e03713f090d85c` | LOCAL |
| `deploy/helm/values.yaml` | 2026-10-13T03:27:14.317710+00:00 | `eca80c412723f539311b5dff7aa2380999a42dc532226655ccaa540ac8e2a670` | SAME |
| `deploy/helm/values.prod.yaml` | 2026-10-13T03:27:14.341052+00:00 | `f9ea8849509631d1c6be23d13630c7a6a591630312ba38479f5e7506a7207352` | SAME |
| `deploy/helm/README.md` | 2026-10-13T03:27:14.370614+00:00 | `aacfee5066d9f285f9e7658b0cee96182ee6fdcba340b62304e657553d3eb4ce` | SAME |
| `deploy/README.md` | 2026-10-13T03:27:14.395840+00:00 | `8c7e622213ee37e91adbfdf2ad7aa5406c58dc15b225ca1c06cd07d56d7b89ca` | SAME |
| `deploy/nginx/templates/imboy.conf.template` | 2026-10-13T03:27:14.418993+00:00 | `edc219f8466d212134c88bf85ae4e049d44824b599b1f069feae85a6f4ae7ba9` | SAME |
| `docs/plans/2026-10-10-limiter-v1-local-evidence-and-plan.md` | 2026-10-13T03:27:14.439524+00:00 | `fa3ccff340043e0b2ec7f9d6f9109b94ff1042a7a44d90cac54b578011a13705` | LOCAL |
| `docs/architecture/cache-v2.2.1/w0/key-registry.md` | 2026-10-13T03:27:14.462494+00:00 | `4775a9bab5a9e21d6e5cb44e12c9b9409baf256c18a79a7176ed61836ebbab2f` | SAME |
| `docs/architecture/cache-v2.2.1/closeout-v1/registry-w1-closeout.md` | 2026-10-13T03:27:14.484450+00:00 | `05b13a0c286e22a49494db1b044839bbca931c868c66af49aae03e18de2036f0` | WIP |
| `test/lib/imboy_cache_metadata_tests.erl` | 2026-10-13T03:27:14.506269+00:00 | `4b4fd9039c5ebfdf68b3c6cd8b65aa556490bfe00847980359f8ff9f860db865` | WIP |
| `src/api/moya_auth_handler.erl` | 2026-10-13T03:27:14.527265+00:00 | `3b75429345b44117dc2e9d7fb14305ccc3e2d8de271112fcb8586fb03bb87c5c` | SAME |
| `src/api/enterprise_oa_sso_exchange_handler.erl` | 2026-10-13T03:27:14.547831+00:00 | `d901a4398711216b026e455e6df2ffc4c72466eec643c230ee4f26e2738288ef` | SAME |
| `src/logic/moya_auth_logic.erl` | 2026-10-13T03:27:14.567786+00:00 | `d1a7dfc6adbff03befd1ab05549ae8f41e82adfc9728a0bf1c9d66fc683663f1` | SAME |
| `src/logic/enterprise_oa_sso_logic.erl` | 2026-10-13T03:27:14.589738+00:00 | `7f80fe592ee98641866d4d42881330d033fab05bd6e8b56f1b817b166f931702` | SAME |
| `docs/architecture/cache-v2.2.1/closeout-v1/protocol-budget-frozen.md` | 2026-10-13T03:27:14.612135+00:00 | `9a9b39c1a1f7290b2af2ed8ca7ad4e1ac61996f4b9dbe29de4d74e6b0e1791a1` | WIP |
| `../.Codex/tsid_plan.md` | 2026-10-13T03:27:14.630955+00:00 | `547ff8e701bf93a2f4160a801b4cfd2d8b3250b2a650761aac09897bee0adfc5` | LOCAL |
| `src/ds/token_ds.erl` | 2026-10-13T03:27:14.651737+00:00 | `bab4a29ff19879c3c3e027672cfb4a808805be270e9c2e83310395d6b6f32364` | SAME |
| `src/ds/adm_session_ds.erl` | 2026-10-13T03:27:14.672653+00:00 | `5c42a91256b43313d66f7119467ae631375f17acaabd5e359e28ad83ad917ea8` | SAME |
| `src/ds/auth_session_ds.erl` | 2026-10-13T03:27:14.702721+00:00 | `9f716ce258e0f4930b265bd25af0ef46a5ebc272df8b27617e291badfeb1a11e` | WIP |
| `src/ds/user_device_ds.erl` | 2026-10-13T03:27:14.724856+00:00 | `fb8d40789d2c36f481c105453e9d708b4386a186a090bafd5bf446407373e5db` | SAME |
| `src/repo/user_device_repo.erl` | 2026-10-13T03:27:14.746641+00:00 | `1f5ad4465483f4db57e7621de15b4313a2ed09302d19bdf4b0a67d6389234035` | SAME |
| `src/lib/imboy_cache_registry.hrl` | 2026-10-13T03:27:14.767739+00:00 | `35b0c70379a09aa7ef2f83101febc91169f5c75e8198ea9b6c55f077283561d5` | LOCAL |
| `src/lib/imboy_jwt.erl` | 2026-10-13T03:27:14.787117+00:00 | `16ec64104e7f27c13a86981c67d6484230042a7f41249905a364fcada534326b` | SAME |

## 附录 B. 定向刷新、交叉审查与交付边界

本轮刷新读取区间：2026-10-13T03:27:13.105576+00:00 至 2026-10-13T03:27:14.787117+00:00（UTC），HEAD `0a5ade3cd8e8c005a1cc22991633dedd81af0445`，index为空。与修订前报告71项相比变更6项，其余原指纹一致；新补7项凭据、设备、Registry与签名文件。定向重读结论：

| 变更路径 | 当前MFA/行号与影响 |
|---|---|
| src/imboy_app.erl | maybe_migrate/0:159–171修改迁移返回处理；本轮不运行。init_throttle_rates/0现:270–313，两IPsetup:294/297；重复初始化事实未改 |
| src/lib/elib_response.erl | success/3:40仅类型spec扩展list；error/2:92、reply_json/5:206–207仍HTTP200，error_with_status保留显式状态；协议事实未改 |
| src/ds/config_ds.erl | get/2、set/4、save/2改Cache装载/写入边界；env/3:35–51仍读application env，代理/规则配置读取结论未改。共享env接口未来重绑，不挪用其测试PASS |
| src/lib/imboy_cache_policy.erl | policy/1、registry_entry/1:103–120，Registry已移至src/lib/imboy_cache_registry.hrl:11–12，两个login namespace仍local；已更新正文旧行号 |
| src/lib/imboy_cache_sync.erl | legacy_set_allowed/1:93–94、handle_sync_message/2:550–571仍拒local SET且flush能删local；其它快照/成员变化不改变本轮此链结论 |
| docs/architecture/cache-v2.2.1/closeout-v1/registry-w1-closeout.md | 新增其他业务当前进度，不能继续引用旧整体BLOCKED；:421–433仍有“账户第一防线完好”前提，与本报告F1冲突，保留为后续协作项，不改该文件 |

新增凭据路径均读当前磁盘：token_ds:do_encrypt_token/4、auth_session_ds:current_epoch/1/safe_query_epoch/1、imboy_jwt:sign/2（:49–54）、adm_session_ds:issue/1、user_device_ds:save/4、user_device_repo:save/5；auth_session_ds有外部WIP，仅引用现有epoch读链，不修改其事务失效改造。源码中无独立auth_session_repo文件，查询实际位于auth_session_ds，未把猜测路径列为证据。

已经进行一轮正文交叉审查：修正F5术语、HTTP逐scope/账户逐Key配置边界、失败已发生却容量漏记、不同发送者不能假定消息顺序、reset/凭据副作用、旧timer退出与回退缺席；为F1～F8分别追踪到D/L/AC-01～AC-19。没有新增业务代码或测试，不把待实现C入口当成已有命令。原报告不正确/含糊段定点替换，正确静态发现保留。

末次关键文件复核于2026-10-13T03:30:18.110023+00:00（UTC）：78/78路径存在且SHA-256与附录A一致；HEAD未变、暂存区仍空、Git状态路径与本轮基线一致。原71项的6处变化完成一次定向重读后稳定，无需第二轮，无MOVING_TARGET；不能据此承诺后续不变或原子快照。

实际静态检查：10个主章节、F1～F8均有AC追踪；D1～D7和L0～L4无缺号；AC-01～19唯一且连续；Markdown代码围栏配对、指纹行完整；待实现工具路径确实不存在，已明确标注；报告写前原指纹匹配。全部检查只是文档/文件核验R，**业务动态验证全部未运行**。本任务实际仅修改本报告；没有编译、测试、make（含-n）、启动应用、数据库、真实外呼、暂存/提交或其它共享Git状态变更。

未来两轮定向重读后仍变化的证据应标MOVING_TARGET并注明接口影响，不阻塞其它已稳定部分。

存储限制：已核实.gitignore:163忽略/docs/plans/，本报告不会自动进入Git交付。本轮不改ignore、不add。后续经授权应将**本唯一权威报告、必要正式测试/fixture和最小复现工具**纳入版本控制；临时日志/压测输出保留原忽略策略，不能仅留下本机临时目录作为长期验收依据。

---

## 附录 C. L1 实施合同冻结（2026-10-14，用户一次性授权 D1～D7）

> 本附录由 Limiter V1 实施任务（分支 `zcode/limiter-v1-impl-20261014`，基线 main `0d28ad4f`）冻结。
> 授权来源：用户 2026-10-14 实施指令第四节的"一次性授权"，取代 §10 的"待业务批准"状态；
> 本附录是 §7 推荐合同的实施落定版。冲突时以本附录为准（针对实施细节），安全目标不变。

### C.1 身份与键（D1 冻结）

- 账户域身份：已知用户（user_ds 按现有规则查到）用 `{user, Uid :: pos_integer()}`，**账户域跨 IP**（不再按 Identifier×Ip 分叉）。身份解析沿用现有查询（find_by_mobile/find_by_email/find_by_account 及 account→mobile 回退），不改变相等规则、不做大小写折叠。
- 未知身份：`{unknown, Kind, Norm}`，Kind ∈ `email | mobile | account`（按登录 Type 分支定），Norm = trim 后原输入截断 320 字节。不把未知身份折成空键。
- Admin（adm_passport_*）与 OIDC 不进入账户域。
- ETS 行 Key：`{limiter_account, Identity}`、`{limiter_admit, Scope, Key}`。

### C.2 事件表（D2 冻结）

| 事件 | 计账户失败 | 说明 |
|---|---|---|
| 密码错误（含未知账户、含账户禁用但密码错） | 是 | 在凭据校验结果处分类，不靠事后 status 覆盖 |
| 验证码 consume 失败（无效码，含未知账户） | 是 | 同上 |
| 空密码/格式非法提前返回 | 否 | 不进入校验 |
| 服务故障（DB/供应商 error） | 否 | 按故障合同 503 |
| 验证码有效但用户不存在/禁用 | 否 | 非凭据失败 |
| 密码正确但 status 禁用 | 否 | 不 reset、不计失败，按现有禁用错误返回 |
| OIDC 任何失败（D7） | 否 | 移除无消费者的 oidc ETS 失败写 |

成功点：**凭据正确且 status ∈ {1,2} 后，先 reset 确认成功，再签发（login_resp）**。reset 失败/未知 → 丢弃响应 Data、不返回凭据、503。reset 之后设备登记失败 → 保持已清零、503（现有），不回补失败。

### C.3 权威状态 API（§7.2/7.3 实施版，owner = login_attempt_ds gen_server）

```erlang
%% Identity = {user, Uid} | {unknown, Kind, Norm}
check(Identity)        -> {ok, Snap} | {error, unavailable}
failure(Identity)      -> {ok, Snap} | {error, capacity_lost | unavailable}
reset(Identity)        -> ok | {error, unavailable}
snapshot(Identity)     -> {ok, Snap} | {error, unavailable}   %% degraded 也算 unavailable
admin_unlock(AdminUid, Identity) -> {ok, unlocked} | {error, unauthorized | invalid_input | unavailable}
%% Scope ∈ passport_per_ip | api_per_ip | oidc_request；Key = 规范 IP binary
admit(Scope, Key)      -> {ok, Snap} | {error, rate_limited | capacity | unavailable}
%% 生命周期（认证准入专用）
begin_attempt(Identity, Pid, DeadlineMs) -> {ok, AttemptRef} | {error, unavailable}
end_attempt(AttemptRef, Outcome) -> ok | {error, unavailable | capacity_lost}
%% Outcome ∈ failure | reset | no_count；未知 Ref 一律拒绝
activate_empty(OwnerPid) -> ok | {error, unavailable}
ready() -> boolean()
```

`Snap = #{count, remaining, locked, expires_at_ms (integer()|undefined), rule_version}`，同一逻辑判断产出。旧导出（record_failure/2、is_locked/2、get_attempts/2、reset/2、get_remaining_attempts/2、check_ip_rate_limit/1 等）由 A1/A2 重写或删除，全部生产调用者同步更新，不留双权威。

### C.4 时间与窗口

- 统一 `erlang:monotonic_time(millisecond)`；逻辑过期 `now >= expires_at_ms`。
- 账户窗：首次失败 anchor，期限 = login_lock_duration_minutes（默认 30 分钟）。
- OIDC 请求域：首次 admit anchor，固定 3600000 ms，不续期。
- HTTP scope：t0=0、period P（默认 60000 ms），窗口 = floor((now−t0)/P)，scope 级统一换窗，所有 Key 同版本。
- 配置变更：账户/OIDC 逐 Key 窗口创建时冻结规则；HTTP scope 在下一 scope 边界切换（pending 登记）。

### C.5 配置键

| 键 | 默认 | 说明 |
|---|---|---|
| imboy.login_max_attempts | 5 | 现有，保留 |
| imboy.login_lock_duration_minutes | 30 | 现有，保留 |
| imboy.limiter_ip_rates | #{passport_per_ip => {10,60000}, api_per_ip => {60,60000}, oidc_request => {100,3600000}} | 新：两 HTTP scope 从 throttle.rates 迁出；OIDC 沿用旧 login_max_ip_attempts_per_hour 语义 |
| imboy.limiter_max_account_entries | 200000 | 账户域条目上限 |
| imboy.limiter_max_ip_entries | 200000 | admit 域条目上限（每 scope） |
| imboy.limiter_max_key_bytes | 256 | Key 字节上限 |
| imboy.limiter_cleanup_batch | 1000 | cleanup 每批行数 |
| imboy.limiter_auth_deadline_ms | 10000 | 认证生命周期 deadline（本地预算推导值） |

环境变量 `IMBOY_THROTTLE_API_PER_IP`/`IMBOY_THROTTLE_PASSPORT_PER_IP` 保留原名，改写 `limiter_ip_rates`；`imboy.login_max_ip_attempts_per_hour` 键迁移为 oidc_request 阈值默认来源后废弃。旧 throttle.rates 若仍含两 IP scope → 启动校验失败拒绝 ready。

### C.6 协议映射（D5 冻结）

| 条件 | 对外 |
|---|---|
| 账户锁定（密码/验证码） | 现有 HTTP200 code1 msg |
| HTTP IP 超限 | HTTP 429（现 envelope） |
| OIDC 请求域超限 | 沿用现有 authorize/callback 业务错误形态 |
| capacity/degraded/unavailable/timeout/unknown | `error_with_status` 503、code503、"服务暂不可用，请稍后重试" |
| reset 后设备登记失败 | 现有 503 |

### C.7 容量与降级（D5 冻结）

账户域 failure 建行且容量满 → 原子置账户域 degraded、返回 `{error, capacity_lost}`；此后本域 check/reset/snapshot/unlock 全拒绝（unavailable），**不自动恢复**，恢复仅走 §9.3 受控空态重启。HTTP/OIDC 新 Key admit 容量满 → `{error, capacity}`，不置 degraded，有位置后自动可用。cleanup 由 owner 分批，仅删逻辑过期行。

### C.8 初始化与装配（D3 冻结）

- `imboy_app:init_throttle_rates/0` 移除 passport_per_ip / api_per_ip 两项 setup；其余 scope（UID 双桶、cs_widget 等）留 throttle 原库。
- 两 IP 规则迁至 imboy 应用 `limiter_ip_rates`，login_attempt_ds:init 读取并持 scope 元信息；未 ready 不准入（unavailable→503），不再 rate_not_set 放行。
- throttle_middleware 两 IP 分支改调 `login_attempt_ds:admit/2`。
- OIDC：auth_oidc_handler/auth_oidc_logic 统一走 `elib_req:get_client_ip/1`（含可信代理），authorize/callback 各 admit 一次，exchange 不占；移除 Cache IP 桶与 oidc 失败 ETS 写，保留有界审计日志。
- rest_fixture 移除旧 passport scope setup，改设 limiter_ip_rates。

### C.9 本地性能预算（AC-16，测前冻结推导框架；数字在旧基线实测后、候选测量前冻结）

- 硬件/软件固定：本机 Apple Silicon 12C/36GB、macOS 27.0.1、OTP29、同一编译档；输入表（Key 长度/分布、并发、认证耗时模型、warmup、时长、重复次数）由 A4 基线报告冻结。
- 预算形式（本地工程预算，非生产 SLO）：
  1. 同有效记账工作量下，新 owner 单事件成本 ≤ 旧实现同口径成本 × 1.5（µs 级 ETS/gen_server 操作推导：旧 S 侧一次失败记账 ≈ 1 次 gen_server call + 1–2 次 ETS + Cache set；新实现同量级 + 快照构建）。
  2. 账户域操作（check/failure/reset/snapshot）p99 等待 ≤ 100 ms、最大等待 ≤ 500 ms（推导：上层 HTTP 处理超时秒级，单 owner µs 级操作 × 混合负载排队）。
  3. owner mailbox 峰值有界（≤ 10000 消息），降载后 5 s 内回落到 ≤ 100。
  4. 持续超载期间账户操作不被 HTTP admit 饿死：账户 p99 仍 ≤ 上述预算。
- 单 owner 不满足 → 最小瓶颈修复；架构变更须 A0 组织审查；不预分片。

### C.10 就绪状态（本任务起点）

- L0：缺陷复现（AC-01/02/04/07/14 旧侧 oracle）、验收入口、退出码传播已合入 main（f93b071d 一串提交，证据 docs/qa/limiter-v1/）。**缺口：AC-16 旧性能基线两条（E2E 旧真实成本、同有效记账工作量）**，本任务 A4 补齐。
- L1：本附录即冻结记录（D1～D7 已获一次性授权）。
- L2/L3/L4：未实施，按 §9.1 推进。

### C.11 实施结果与就绪状态（2026-10-15，任务分支 zcode/limiter-v1-impl-20261014）

> 实施会话：最多 6 agent（A0 协调集成 / A1 权威状态 / A2 登录链 / A3 IP·装配 /
> A4 基线·验收工具 / A5 独立审查），基线 main `0d28ad4f`，已吸收 main
> `3d46f35b`、`b3865652`（均零重叠）。本节取代 C.10 的缺口描述与 §10 分开判定的对应项；
> 生产适用性边界见 C.13。

#### C.11.1 阶段状态

| 阶段 | 状态 | 绑定候选/证据 |
|---|---|---|
| L0 | **DONE**（基线缺口已补齐） | 旧侧 oracle/证据 f93b071d 一串；AC-16 旧基线 A/B 落盘 `docs/qa/limiter-v1/0d28ad4f/baseline/`（9fb62009 纳入版本控制） |
| L1 | **FROZEN** | 本附录 C（用户 2026-10-14 一次性授权 D1～D7） |
| L2 | **DONE** | `5f22342b`：新权威 API/跨 IP 账户域/窗口/容量降级/生命周期 monitor/兼容层；DS 专项 42 项全绿 |
| L3 | **DONE** | `c53997d6`（登录链：身份归并/F2/成功点/503）+ `b428db06`（两 HTTP scope+OIDC 迁移/装配校验/规范化）；旧桶调用点全部移除（§7.5 表逐项落地） |
| L4 | **部分完成** | 全量 eunit、AC 重放与审查结论见 C.11.2/C.11.3；AC-13 C 档与 AC-18 演练未执行（见 C.12） |

#### C.11.2 AC-01～19 结果（适用验收口径）

| AC | 结果 | 证据（Q=定向 eunit 模块；C=escript case） |
|---|---|---|
| AC-01 | **PASS** | Q(passport_logic_tests 33) + C AC-01 fixed（真实 REST HTTP 链 F 次计数递增、锁定拒正确密码；REST 通道 48/48） |
| AC-02 | **PASS** | Q(passport_logic_tests) limiter_code_locked fixed（真实 DS 置锁、验证码不消费、锁定拒绝） |
| AC-03 | **PASS** | Q(login_attempt_ds_tests) limiter_inflight_results（F−1+K、迟到两序、跨窗归新窗） |
| AC-04 | **PASS** | Q(login_attempt_ds_tests) limiter_reset_interleaving fixed（终态 0/1、快照自洽；A5 反例 A 确认能识别错误实现） |
| AC-05 | **PASS** | Q(login_security_logic_tests 17)（同身份别名一致快照、unlock 权限、无跨 Cache 伪 expiry） |
| AC-06 | **PASS** | Q(login_attempt_limiter_ac_tests) limiter_admit_race（K=8 真并发恰 min(R,K)、count 停 N、不退款；A5 反例 B 确认） |
| AC-07 | **PASS** | Q(login_attempt_ds_tests) limiter_cache_old_flush fixed（迟到 flush/批次不删新状态；权威已无 Cache 依赖） |
| AC-08 | **PASS** | Q(login_attempt_ds_tests) limiter_time_rules（expiry 边界、HTTP 60000 换窗非 115000、窗口冻结、时钟注入三周期） |
| AC-09 | **PASS** | Q(elib_req_client_ip_tests 15) + Q(auth_oidc_logic_tests 39)（信任统一/IPv6 归一/authorize+callback 各扣一次、exchange 零、无 oidc 账户锁行、审计在场） |
| AC-10 | **PASS** | Q(login_attempt_limiter_ac_tests) limiter_capacity_during_auth（capacity_lost→域拒绝、腾位不恢复、admit 域独立） |
| AC-11 | **PASS** | Q(login_attempt_limiter_ac_tests) limiter_capacity_bounds（上限/不挤掉有效锁/超长 capacity/空 Key unavailable/ETS 有界） |
| AC-12 | **PASS**（DS+业务层） | Q(lifecycle 组：DOWN/deadline→degraded、end_attempt 各 outcome、旧 ref 不误伤) + Q(login_security_logic_tests 故障分支不默认放行) |
| AC-13 | **部分 PASS** | DS 层 limiter_restart_recovery PASS（杀 owner→表失→新 owner 空态重新累计）；C 档"停整节点"未执行（escript 退 4 INPUT_PENDING，见 C.12） |
| AC-14 | **PASS** | Q(throttle_middleware_tests 19)（真实 DS admit 计数、重复初始化不重置、无旧 worker/index/Cache 回落、UID/widget 仍原库）+ 迁移校验拒绝旧配置用例 |
| AC-15 | **PASS**（本地合同口径） | C AC-15 两隔离节点各 100、合计 200 如实报告；"若业务要求集群共享额度则此结果不合格"声明按计划保留 |
| AC-16 | **PASS**（本地预算） | 旧基线 A（E2E p50≈30µs、记账数 0=F1 复现）/B（record_failure p50 18.5µs 等）+ fixed 对照（failure 1.3µs≈14×、admit 1.4µs、mixed_8w p99 165µs<100ms 预算内、mailbox 有界回落）；生产 SLO 未验收（C.13） |
| AC-17 | **PASS** | Q(passport_logic_tests)（reset 确认后才签发、禁用不 reset 不计、reset 失败无凭据 503；A5 反例 C 确认）+ Q(passport_handler_tests 9)（503 envelope）+ REST suite 协议基线不变 |
| AC-18 | **未通过（未执行）** | C AC-18 INPUT_PENDING：需隔离演练环境与已验证回退候选（D6）；就绪件已完成（ready 门、fail-fast、受控空态合同），生产切换保持关闭（§9.3） |
| AC-19 | **PASS** | Q(passport_logic_tests) limiter_exports_compat（login/3、do_login/3/5、verify_user/3 全调用者走查+成功/失败不重复计数；user_logic:88 改密直调合规） |

#### C.11.3 独立审查与全量

- A5 独立审查（候选 ca736743）：**无 P0**；1×P1（证据候选重绑——最终候选上已重放）、6×P2 全部关闭或转 C.12 记录；反例 A/B/C 实测确认测试可识别目标错误实现。
- A5 整改已提交（`89fc032b`）：非法 limiter_ip_rates 启动诊断（fail-safe 保留）、admit @doc 对齐、fail 分支注释更正。整改中发现并修正 `maps:partition/2` 运行期 undef（lefthook qs/2 同款"编译不报错远程调用"失效类）。
- 独立审查（A5）后的整改提交（`89fc032b`）与本评审（review）整改由 A0 实施，
  经 A5 审查清单逐项核对与运行时验证（`maps:partition/2` undef 实测复现并修正）。
- 全量 EUnit：**绑定候选 `997aedfb`**（12066 过/132 败全归因非 limiter 既有家族）；
  最终 HEAD `84662f05` 相对 997aedfb 仅含文档/证据提交与一次零重叠 main 吸收
  （`7d2239a5`←`3a5d6f2a`），limiter 11 模块（180 项）已在最终 HEAD 重验全绿，
  按 §9.1"候选改变按失效范围必要重验"已满足；未在最终 HEAD 重跑全量。
- AC-01/15/16 在最终 HEAD 重放 PASS（证据 `docs/qa/limiter-v1/7d2239a5/`，
  review 整改补齐 AC-15 output/exit_code 落盘与 manifest 副本）。

#### C.12 历史交付遗留（完成判定以 C.14 修订为准）

1. **AC-13 C 档（停整节点）与 AC-18（切换/回退演练）未执行**：需隔离演练环境与已验证回退候选；escript 诚实退 4（INPUT_PENDING）。按 §9.3/D6：生产切换在回退候选验证前保持关闭——生产部署本就不在本任务授权内，此两项保持未通过并如实交付。
2. **容量满未先有限清过期**（A5 P2-1）：实现按 C.7 字面"满即 degraded"，未含 §7.3 的"先有限清过期"；60s cleanup 间隔内逻辑过期行仍占条目预算，新 Key 可能提前触发不可自动恢复的账户域 degraded。属合同细化项，如需收紧应走合同修订+重验，不静默改实现。
3. **deadline 批次检查从表头重扫**（A5 P2-2）：大表下非 attempt 行消耗批次预算，deadline 检测最坏数百秒延迟；合同允许有界批次，延迟已纳入 AC-16 预算口径，后续优化记录在案。
4. **channel_webhook_handler:152 仍用 deprecated extract_real_ip**（基线已有，非本轮引入）：webhook 非登录链，留后续清理。
5. **生产 sys.config 运维顺序**：`validate_limiter_ip_migration` fail-fast 为合同要求；生产配置副本须先把两 IP scope 从 `{throttle, rates}` 移出再上线（C.8）。
6. **AC-16 reset 对照口径**：旧 reset 为裸 ets:delete（无 owner 确认，恰是 AC-17 修复的缺陷形态），fixed 对照采用绝对预算口径（p99/max/mailbox），非放宽门槛（脚本注释已说明）。

#### C.13 生产适用性边界（本任务未验收、未授权事项）

- 生产迁移、生产状态清空、真实用户数据操作、真实短信/邮件/IdP 调用：**均未执行**（授权边界）。
- 生产 SLO/容量预算：本地预算为工程预算（C.9），**生产性能未验收**；上线前须按部署实测输入重新推导并过 AC-16 生产口径。
- 集群共享额度、跨重启保存：**不保证**（D4）；多节点合计额度可倍增（AC-15 如实口径）。
- 回退：**ROLLBACK_NOT_READY** 维持——无已验证回退候选前相关入口保持拒绝（§9.3 步骤 6）。
- 生产切换批准、部署执行：须单独授权，不因本任务本地完成而自动生效。



#### C.14 合入后正确性整改（2026-10-15）

本节覆盖 C.11/C.12 中与下述事实冲突的实现和完成表述。当前修复来自
`main@4c48e666` 的独立 `codex/limiter-v1-correctness` worktree，尚未授权提交或合并。
历史专项 PASS 不能替代本节反例及完整计划验收。

- **容量**：admit 过期行原地换窗不增加物理条目计数；满额新 Key 先有界回收过期行。
  账户域回收后仍满才置 degraded，不淘汰有效锁。
- **deadline**：独立、按截止时间排序的 owner ETS 索引，单轮最多处理 cleanup_batch
  个已到期认证；账户/IP 大表不能遮挡认证。终态、DOWN 和到期同步移除索引/monitor；
  在途条目数受账户域容量上限约束。原“最坏数百秒”判断不成立：旧表头重扫可能持续漏检。
- **恢复**：owner init 一律 ready=false，包括 supervisor 自动重启。可信节点操作
  `activate_empty(OwnerPid)` 只批准该 PID 的未就绪空态，旧 PID 批准不能开放新 owner，
  在役或 degraded 状态不能用此 API 清除。默认配置 `limiter_allow_empty_start=false`；
  显式设 true 是每次完整应用启动均生效的持久空态策略，不是一次性令牌，child 重启不走该策略。
  未完成排空/验证的部署应保持默认关闭，由既有受限运维通道绑定 PID 批准；不新增公网恢复接口。
- **认证终态**：密码/验证码凭据失败由 end_attempt(failure) 原子确认记账与释放生命周期，
  capacity_lost/unavailable 返回 service_unavailable；不重试未知结果。成功路径以 end_attempt(reset) 原子核验 Ref、清零及释放；旧 Ref 不能清除新 owner 状态或授权签发。
  AC-12 的 end_attempt 错误不得再吞成 ok；未知 Ref 的所有 outcome 均拒绝，不补记、不伪报成功。
- **时间**：HTTP 窗口使用整数 floor 对齐，支持实际 Erlang VM 常见的负数 monotonic 基准；pending 新 period
  的首个请求即使迟到多个窗口，也按新 period 定位，不返回已经过期的窗口。
- **验证工具**：AC-13 实际运行两个顺序独立 VM，检验未批准拒绝、批准后账户/IP 累计和 degraded；
  证据仅代表 DS/VM 恢复边界。AC-16 交错 80/15/5 操作，运行期间 1ms 取样并与 cleanup 重叠，
  账户等待单独判定；128-worker 压力档持续至少 5 秒，报告实际事件数/时长与降载后队列，
  不将统计取样最大值称为无遗漏的瞬时峰值，不由有限本地负载推导任意生产流量保证。
  REST 夹具在 owner 启动前冻结测试额度，不依赖私有 sys.local.config 或等下一窗口碰巧通过。
  来源标识绑定 HEAD 和本任务文件集合摘要，另记录 BEAM 摘要；未提交实现不冒称原 main 候选。

`login_attempt_correctness_tests` 的前六个反例在修复前均失败（原始退出 1），
修复后新增的 18 项定向回归已通过；正式 eunit 定向入口同样通过。
集成专项已执行，最终结果和脚本重放以仓外原始回执为准，不沿用旧候选的全量结果。
本轮全量 EUnit 在 600 秒上限退出 124，过程中已出现群文件 mock 冲突等失败；
全量门禁未通过。原始 Dialyzer 非零，亦不由专项通过替代。

**整份任务仍须按完整验收判定**：AC-18 真实入口关闭/排空/切换/安全候选回退演练
未执行时保持未通过，不能以“生产未授权”豁免隔离演练；全量 EUnit 失败也不能由专项绿灯覆盖。
部署、生产清空、合并与发布均不在本轮修复授权内。
