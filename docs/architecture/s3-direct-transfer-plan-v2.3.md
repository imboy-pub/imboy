# IMBoy 附件 S3-only 直传与 Disk 退役计划 V2.3

日期：2026-10-31（Asia/Shanghai）。
状态：`PLAN_UPDATED / IMPLEMENTATION_NOT_ASSESSED / ACCEPTANCE_NOT_RUN / NO_GO`。
本次只调整计划；没有实施产品代码、迁移数据、调用真实存储或运行验收。源码中已存在部分 direct 实现，不沿用 V2.2 的“尚未实施”作为当前事实，也不把实现存在当作验收通过。

## 1. 最终合同与版本裁决

**最终产品仅支持 S3 兼容对象存储。所有用户附件上传由客户端直接写入获准的 S3 端点；Erlang/BEAM 不接收、转发、读回或生成附件正文。彻底移除 disk 存储驱动及其专用 C NIF。**

Erlang 保留认证、授权、预签名、状态机、配额、确认、业务绑定、位置映射、审计和生命周期控制。HEAD、签名、服务端对象 Copy/Delete 的有界控制响应可以经过 BEAM；对象正文不可以。控制面不得用 Range GET 或完整 GET 伪装 HEAD。

### 1.1 数据面与 provider 边界

| 场景 | V2.3 最终裁决 |
|---|---|
| 首个必验部署 | Garage，客户端可达端点 `https://s3.imboy.pub`；此处是部署验收目标，不是硬编码常量，也不代表已获实桶写删权限 |
| COS / OSS 等 | 仅通过同一 S3 兼容协议接入，端点、region、寻址、凭据、签名和能力由 profile 配置；逐 provider 实测后才能声明支持，不新增厂商 SDK 或空适配器 |
| provider 能力不足 | 拒绝启用该 profile，或使用已验收的独立 Worker 补足证明；不得回退 Erlang 正文代理。COS/OSS 未验收不阻塞 Garage 交付，但必须标记 `UNVERIFIED / NOT_ENABLED` |
| 用户上传 | App/Web/Seat/Widget/外部应用直接向 S3 PUT；业务凭据只用于控制 API，绝不发往存储 |
| 用户下载 | 可接受有限授权窗口的 scope 使用短期 S3 GET；要求逐请求授权或隐藏 origin 的企业/客服等使用独立边缘内容代理。两者均不经过 BEAM 正文通道 |
| 内容检查、头像处理 | 独立受限 OS Worker 访问 S3，生成绑定最终字节的认证回执；不在 BEAM 内计算附件正文 hash、魔数或变换 |
| 录音拼接、转写输入、导出、预览和服务端生成文件 | 独立处理 Worker 读取/写入 S3；Erlang 只编排任务和接收有界结果。不能为了“全部客户端上传”把服务器生成物错误地转交客户端制造 |
| 临时文件 | 客户端缓存、Worker 临时文件、迁移工具只读源不是 disk 存储驱动；可按隔离、容量、期限与清理合同保留 |
| Disk / FastDFS 等历史存储 | 只允许受控过渡和离线迁移；最终新生产候选无此存储驱动、正文路由或回退开关 |

本计划不保证“任何 S3 兼容服务天然等价”。固定 provider 版本、权限、代理、checksum、Copy、Range 与 CORS 实测证据是启用前置。

### 1.2 对 V2.2 的明确替代

| V2.2 内容 | V2.3 替代 |
|---|---|
| 不强制 Disk 客户安装 Garage | 最终不支持 disk；历史客户先迁移到获准 S3 存储，未完成则阻塞其切换 |
| 可选 Step 17 新建 Disk Worker/internal 下载 | 必交 Step 17 历史 disk 数据盘点、迁移与退役前置；不再建设 Disk 产品能力 |
| 旧 business_gateway 与新 s3_put 长期并列 | gateway 只用于切换前的旧版本/旧会话；最终只剩控制 API 和客户端 S3 上传 |
| 首个普通单 PUT 候选可代表阶段结果 | 可作为中间里程碑，不能代表 V2.3 全部交付；§3 所有必交入口必须闭环 |
| 后台派生/导入可选 | 现有启用功能的正文迁出为必交；新增业务导入功能仍非目标 |
| 未选 Disk 记 OUT_OF_RELEASE_SCOPE | Disk 退役是必交门 E，不能排除；只有 MPU、Moya 扩展、未选 provider 可范围外登记 |
| 旧 T42/T43 验证 Disk 数据面建设 | 编号保留，语义改为历史迁移安全和删除后的恢复验证；旧验收结果不继承 |

V2.2 保留为历史输入，执行以本文为准。本文不修改 AGENTS.md、ADR 或既有历史迁移文件；实施时 Step 3 必须提出存储 ADR 的显式修订，不能留下两份相反的现行规范。

## 2. 证据基线、授权和并行边界

| 仓库 | 本轮静态核对 HEAD |
|---|---|
| Backend | `5e330f30c47d8183f98856177cdff7dcd45b89c6` |
| Flutter | `c13a286cc90d415ec6bbc99b6bd3d5b370306726` |
| Admin | `3a524cd119455c8d0ef1bf28194540b7affd3081` |

这是本轮本地快照，不是最新远端保证或未来执行基线。开始实施前逐仓确认 git 根、当前 main/PR、HEAD、WIP、规则和最新迁移编号；重新评估已有 direct 代码，按实际差距实施，禁止重复造轮子。V2.2 中 2026-10-19 的源码行号和最高迁移号只作历史，不预分配编号。

Backend 当前存在多份外部未跟踪计划与目录；本次只新增本文，保留原 V2.2 和其他 WIP。所有文件归对应实际仓库，不写聚合工作区根。含生产 PII 的盘点材料不得进入工作区或仓库，验收用合成 fixtures 或符合项目要求的脱敏聚合证据。

PR #34 的 PostGIS 包一致性与 OTP 编译门修改独立于 S3-only 业务。其 `c_src/attachment_disk_lock_nif.c` 五行可移植性补丁只服务过渡版本：最终已证明无人调用并删除 NIF 时保留删除，不恢复废弃实现。合并时核对 PR 实际最新差异，workflow/Makefile 冲突逐块合并，保留 PostgreSQL 18/PostGIS/TimescaleDB/pg_jieba 依赖与 OTP 29/28 编译门。已有 E2EE 通过记录不自动证明 S3-only 候选通过。

本轮授权仅为修改计划。后续实施、实桶访问、迁移写删、提交、push、合并、发布依据届时已有授权执行；不重复索取已明确授权的同范围动作。未经授权不操作其他会话、不发送外部消息。并行只按独立写集安排，共享 SQL、数据库、构建目录和最终提交由集成者控制。

## 3. 必交功能与正文消费者清单

每行在 Step 1 产出“入口/调用者、现行协议、实际 source/sink、权限合同、改造文件、owner、测试 case、状态”登记表。没有发现现有功能时提供代码检索证据，不自行增加功能；发现已启用新入口时必须补入，不能仅按下表名称覆盖。

| 功能/认证面 | 当前静态线索（Backend 相对本仓） | 必交结果 |
|---|---|---|
| 普通聊天图片/语音/视频/文件，头像/封面等实际调用方 | `attach_logic.erl`、`attachment_storage_gateway_handler.erl`；App `attachment_api.dart`、resumable uploader | 首传/重试/恢复均直接 S3；上传完成与可发消息分离 |
| 普通 confirm、HEAD、proof | `attach_logic:v5_object_bytes`、garage driver、`elib_oss` | 新会话无正文读回；最终不可改写对象有可信同字节证明 |
| E2EE 附件 | App encryptor、journal、接收解密链 | 逻辑 key/AAD/nonce/chunk/descriptor 不变；文件化有界内存，密文只生成一次 |
| 普通下载与缓存 | `attachment_download_handler.erl`、`attach_logic:view_url`、App resolver/cache | 短 GET 或受控 edge；权限窗口、Range、缓存隔离、历史 URL 排空成立 |
| 群文件 | `group_file_ds.erl`；App `group_file_api.dart` multipart | 改成 confirmed 附件引用绑定；群权限、归档、锚点和幂等不变 |
| 群相册/批量照片 | `group_album_handler.erl`、`group_album_ds.erl`；App `group_album_api.dart` | 无 multipart/base64 正文入 BEAM；逐项结果、批量限额、部分失败/取消可恢复 |
| Admin AI 头像上传与公开发布 | `adm_ai_agent_handler.erl`、`attach_direct_logic:publish_avatar`；Admin AI agent API | Admin 控制面认证+浏览器直传；图片检查与公开复制均不经 BEAM 正文 |
| Internal Application、EB Member/Platform、Seat、Visitor/Widget、App 企业附件 | `eb_asset_store.erl`、`eb_asset_direct.erl`；Seat/Widget/App transport | 各认证面控制凭据保留；新的上传目标全部 s3_put，无同源正文兜底 |
| 企业/客服 content 下载 | 原 content 路由，Widget `fetchAssetContent` | 保留逐请求授权和不暴露存储 URL 合同；正文由边缘传输，不改成无鉴权裸链接 |
| 录音捕获/分片/续期/恢复 | `talk_record_part_upload.erl` | 首次和续期均 S3 直传；分片身份、顺序、deadline、校验和幂等不变 |
| 录音拼接/播放/Range/处理输入/导出 | `attachment_derived_artifact.erl`、`attachment_artifact_reader.erl`、`talk_record_*` | 外部 Worker 数据面+任务回执；Erlang 不再读写音频、导出等正文 |
| 现有后台迁移、导入、预览与派生消费者 | `elib_oss:put_object_from_file` 等实际调用者 | 已启用功能迁到受限 Worker/经验证的存储侧 Copy；新增导入产品不扩范围 |
| 收藏/转发/频道/朋友圈/教学等引用面 | attachment reference、位置映射、resolver 和直接 key 消费者 | 稳定业务引用；读/删命中最终物理 winner，不因迁移改写历史 E2EE 描述符 |
| 生命周期与运营 | deletion worker、EB delete queue、GC、hold/retention、存储统计 | 保留授权、引用守卫、删除重试、配额和审计；迁移后按新位置正确清理 |

纯文本消息、好友、组织、OA SSO、支付不新增设计；其附件/头像消费者按实际调用回归。不能以“只改存储”排除录音、客服、企业或旧客户端兼容义务。

## 4. 冻结协议与数据模型

### 4.1 版本与兼容策略

规划名 `direct-v1` 表示新传输协议，与文档 V2.2 和既有 E2EE descriptor 版本独立。正式 wire 字段/枚举在 step-3 定稿后才能写实现。仅在受限过渡期，旧 session 继续旧协议；新旧分派依据持久化版本，不能根据缺少字段、当前默认 driver 或 URL 外观猜测。Step 24 排空后退役旧正文入口，不保留永久兼容网关。

direct register 只登记稳定逻辑身份和策略，**无写 URL**。E2EE 拿到稳定 logical object_key 后生成一次固定密文文件；grant 冻结实际待传字节 hash/size。register 阶段可预留活动 session/预计字节预算；grant 原子调整为最终密文字节 reservation，失败无新能力，避免“预登记”绕过容量限制。

沿用现有 object_key 作为逻辑引用，保持 `u<Uid>/...` 归属及旧 descriptor/AAD 合同。每个 seal attempt 新建内部物理 key。新协议增加有约束的映射，不能将 candidate key 写回已生成 descriptor，也不能改历史 path 的含义。

### 4.2 最小持久化模型及消费者闭环

| 实体/职责 | 必须保存 | 必须贯通 |
|---|---|---|
| 现有 pending/session 扩展 | protocol、logical_key、主体及 scope 快照、expected、session deadline、winner 引用、reservation | register/status/grant/renew/finalize/abort |
| grant 语义记录 | 精确 staging 身份、profile/config 版本、签名时间与有效期、method、headers 摘要、expected 版本 | 签名失败/COMMIT 未知恢复；URL按需短时生成，不持久化完整签名 URL |
| 最小 seal attempt | attempt/session、唯一 candidate、dispatch CAS、owner/lease/fence、网络终态、proof/receipt、GC claim、容量 | 多 attempt 账本不放进只能表达“删除”的队列；优先复用现有结构，确实不匹配时加最小子表 |
| 正式位置映射 | logical_key/业务附件ID→唯一 winner 的 driver/namespace/container/**physical_key**/locator_digest/proof | view_url、download、普通/企业 content、hash JOIN、收藏/转发、所有以 path/key 为定位的生产者消费者 |
| 现有 deletion intent 扩展 | 在元数据删除事务内捕获完整物理身份与 winner/proof 版本；历史无映射行走原逻辑 | ordinary pending/orphan、正式删除、EB purge、GC worker、hold/retention；删除不能在主行消失后再查映射 |

映射和 winner 只在同一短事务提交后对业务可见；新协议已确认却缺完整映射时失败关闭，不按 logical_key 猜 sealed key。新表/字段需要 UNIQUE、FK、非空与部分 NULL 组合测试，restricted function 的角色、GRANT/REVOKE、SECURITY DEFINER/search_path 同时评审。

旧 V5 pin 是“网关测字节→pin COMMIT→PUT”。新协议必须是“expected 冻结→staging→稳定 candidate proof→最终 pin/winner/confirm”。可复用鉴权/锁序思想，不能原样复用 `verify_and_save_v5` 或将 staging proof 提前写入正式 actual receipt。

### 4.3 传输描述与认证域

描述语义至少含：protocol_version、logical object_key、client_upload_id、state；transfer.kind/method/url/required_headers/expires_at/expected_size/profile_id；finalize/status 所需稳定标识。明确时间单位，不在同一字段混用 epoch 秒、毫秒与 RFC3339。TSID 沿用 JSON integer 与 Web EntityId 约定。

| 类型 | 正文目标 | 允许认证 | 客户端行为 |
|---|---|---|---|
| 过渡期旧 business_gateway（最终退役） | 旧版本中经校验的本实例指定业务路由 | 该认证面现有 Bearer/upload_ref/capability | 仅排空旧会话，不发新协议会话；Step 24 后不再接收正文 |
| 新 s3_put | profile 允许的 staging origin 与精确 method/key | 仅 descriptor 明示、经允许集合校验的存储签名头 | 无 IMBoy/Seat/Admin/企业认证、Cookie；逐项发送 required_headers |
| 新 edge_content | 同源/获准边缘业务 content URL | 该认证面正常读凭据 | 每次 GET/HEAD/Range 检查当前权限，私有 origin 不出客户端 |

禁止从服务端任意 headers 对象照单转发 Authorization/Cookie/Host；descriptor header 集合要与 profile 和签名 canonicalization 一致，浏览器禁设头不作为正常路径前置。不能简单复用目前仅发送 Content-Type 的裸 PUT 函数。

普通 API 已有 HTTP 200 + envelope code 习惯（`elib_response.erl:114,:207`），Seat/Widget 等另有真实状态处理。step-3 必须逐认证面给出 `(HTTP status, envelope code, state, retry_action)` 表，并使用真实客户端 parser 回归。处理中可选择版本化 202 或既有 envelope，**必须先定稿并证明三端不会误当 confirmed**，不能在下层随手返回 202。

### 4.4 封存与恢复算法

1. register/grant：当前权限、scope、声明、session 和共享预算检查后短事务提交；只有已查明 COMMIT 的 grant 才能签发。重放不得换声明或重复预留。
2. 客户端裸传 staging；checksum 签名覆盖原始 32 字节 SHA-256 的 Base64。最终业务 proof 不接受客户端声明、可写 metadata、ETag 或未经证明的 composite。
3. finalize 初步真 HEAD 只用于早拒绝，不充当锁。持久化 attempt 与唯一 target；原子 dispatch 最多授权一次可能写目标的网络执行。
4. Copy 禁透明重试及未批准重定向；元数据显式 REPLACE；解析有界正文中的实际结果，HTTP 200 不自动算成功。
5. 先证明 target_write_closed，后验证精确 candidate 的可信 full SHA-256/size；需要内容规则时 Worker 同一次受控读校验 hash 与规则。无可靠存储 proof 时可选已验收 Worker 流式独立验证；两者都不可用则 profile 阻塞。
6. 当前授权、session/expected 版本、未取消、deadline、proof、Worker ruleset、quota、hold/purge、candidate publish_eligible 均复核后，CAS 唯一 winner，原子完成位置映射/actual pin/confirm。已有 winner 只返回同一结果。
7. bind 只改业务引用，重复绑定幂等。公开发布另经许可；变换输出有独立 locator/hash/lineage，不继承输入 receipt。

target_write_closed 是 profile+操作 provenance 的条件，不能靠随机 key、lease 到期、HEAD 可见/404、等待若干秒或再 hash 一次推导。正常 Copy 成功终态的依据要结合固定版本行为分析与实桶故障测试；无法证明的 unknown attempt 永不晋升，仅探测/隔离。崩溃发生在 dispatch 落库与发包之间也当作可能已发。

需要再发写时新 attempt、新 target；旧 worker 的写只能落旧 candidate，旧 fence 不能写 winner。只读验证可重试但有并发/次数预算。unknown 到上限暂停该 profile 新 grant；不无限创建 attempt 保证“最终成功”。

candidate GC claim 与 winner CAS 共享数据库裁决和锁序；先 claim 则永不发布，先 winner 则 candidate GC 必须失败。正式对象沿现有 intent→COMMIT→worker 删除；hold/引用复核不能省。删除后可能迟到重现的旧 target 保留 tombstone 与预算，不复用 key，不将一次404当物理最终清空。

### 4.5 配额与大小合同

用户逻辑配额和物理容量分账：grant reservation；confirm 即转真实占用或等价持续保留；bind 不重复收费。unknown、staging 重放、失败 candidate、MPU part、Worker 临时文件始终计物理预算；业务取消不代表网络已停，也不立即释放全部容量。

普通现有上限 104,857,600 bytes；EB 上限 26,214,400 bytes。E2EE 单独定义明文及密文限制，密文开销按现有 chunk/tag 计算。不能要求“100 MiB 明文合法”同时要求“密文也不超过100 MiB”而不说明取舍；维持现有服务端上限时，客户端显示可传明文上限并在预检拒绝越界，不擅改服务器限额。

签名与 confirm 大小检查不能取代数据面硬上限。对重放、超大上传和超时，需要实际 NGINX/provider 方法、拒绝点和残留预算证据。控制面 headers、XML/错误体、metadata、batch、ListParts 同样有硬上限，超限提前终止连接。

## 5. 数据面、权限和客户端专项合同

### 5.1 上传与旧路由

最终版本的 register/grant/status/renew/finalize/bind 都是有界控制消息。仅客户端发起用户文件 PUT；同源 Bearer/upload_ref/capability 可用于控制授权，不可作为保留正文网关的理由。所有 transport 显式区分控制 API 与 s3_put，按 descriptor method/精确 origin/required_headers 执行；不得把 IMBoy/Seat/Admin/企业认证、Cookie 或日志凭据附加到 S3。

旧 business_gateway/multipart/base64 仅在过渡旧版本中存在，必须登记允许客户端版本、旧会话集合、最大正文、停止签发时点与排空期限。最终候选不再注册接收正文的这些 handler；可保留不读取请求正文的有界“升级客户端”控制错误，HTTP/envelope/retry_action 与旧 parser 逐项测试。存储失败不能回退该旧路径。

### 5.2 下载与撤权

| 场景 | 最终读合同 | 必测边界 |
|---|---|---|
| private/c2c 等已接受有限窗口的场景 | ACL 后短期 S3 GET | 确切 TTL、签发时权限、旧 URL 排空、已缓存/下载内容不可远程抹除 |
| group/channel/moment/teaching 等 | Step 3 逐 scope 固定“有限窗口”或“逐请求授权”，不得发布时仍未决 | 退群/归档/撤权后重放旧 GET/HEAD/Range，App 缓存不跨主体 |
| Internal/EB/Seat/Visitor/Widget | 原业务 content URL 与认证域，边缘私有 origin；每次请求授权 | 不暴露内部 URL/签名，不能绕 origin；保留无权限隐藏与缓存隔离 |
| 公开头像/素材 | 检查通过且业务允许后发布 | 未发布匿名不可读；公开缓存撤回不承诺即时全网失效 |
| 录音/派生文件 | 受业务权限约束的边缘或已批准短 GET | 播放 Range、HEAD、取消、导出下载及过期错误正确 |

边缘传输不等于 Erlang 正文代理，也不等于所有下载直接暴露 S3 URL。固定边缘实现前复核既有部署方案，优先复用；保留 GET/HEAD/206/416/304、Content-Disposition 和内部错误转换。一次授权不承诺中断已开始的长连接。

### 5.3 Worker 与服务端生成物

Worker 必须是 BEAM 外的独立 OS 进程/服务，不把 Erlang 子进程、流式 binary、外部端口把正文送回 BEAM 误算成退出。可复用已有 Worker 能力，不强制新增语言/容器平台。Worker 只领取精确输入/输出对象能力；CPU、RSS、解压、临时空间、输出大小、并发、期限、重试和未知副作用有上限。

输入绑定稳定对象身份与 proof，输出绑定独立 locator/hash/size/lineage/ruleset；回执有认证、attempt、任务 fence 和幂等，不能接受任意 URL 形成 SSRF。正文不进入任务参数、数据库、日志或回执。录音/转写任务的业务文本和结构化结果可作为有界控制结果返回，音频/图片/文件正文不可以。E2EE 不向 Worker 提供用户明文或私钥。

保持现有 EB 魔数/文本等规则，不能因移出 BEAM 而只信客户端 MIME/hash。图片派生和公开复制需独立输出证明，存储 Copy 只有固定 profile 已验证时可用；否则受限 Worker 复制。现有启用的录音拼接、导出等功能不得标为可选以获得完成状态。

### 5.4 客户端与限额

App 使用稳定长度、可重开流、身份/内容变更检测、取消和所有权清理；首次与恢复不把大文件 readAsBytes。E2EE 文件化保持原算法，复用同一密文；解密临时文件全部认证通过后才向预览器发布。Web 既有小对象 hash 可在明确大小/并发预算内使用 arrayBuffer，大文件应流式/Worker hash；File PUT 本身不证明 hash 有界。

进度 100% 只表示上传完毕，journal 必须表达 uploaded/sealing/confirmed/bound。网络响应未知先查状态，不盲目重传或另发消息。普通/企业限额与密文开销沿 §4，不自动扩单文件上限。S3 端点需客户端可达且满足 HTTPS、CORS、CSP、签名头、路径寻址、移动网络限制和 Range 合同。

## 6. 26 张执行卡与依赖

编号沿用 V2.2 Step 1–22，增加 23–26。Step 17/18 的范围已改变，旧证据不能照搬。owner 是职责角色，不代表已分派 agent；实施者须在 Step 1 填具体负责人和真实测试入口。每卡提交前审查差异；提交粒度依据授权和可评审逻辑，不机械地一张卡一次 commit。

| Step / owner | 修改入口与交付 | 依赖 | 验收及失败处理 |
|---|---|---|---|
| 1 / 集成 | 三仓 main/PR/WIP/规则/入口/正文消费者/测试/历史来源盘点 | 无 | §3 每行有 owner 与证据；未识别入口不宣布全覆盖；历史盘点使用授权的脱敏聚合数据 |
| 2 / 存储 | Garage 固定 profile 的 A0 探测；COS/OSS 分开登记 | 1、资源可用 | PUT/HEAD/Copy/checksum/权限/代理/CORS 实桶负向证据；缺资源 BLOCKED，不以 mock 代替 |
| 3 / 架构+三端 | wire/error/time/限额/下载撤权/旧客户端政策、ADR 修订和任务/回执合同 | 1；2 未知项列启用阻塞 | 无影响安全与兼容的未决分支；冻结旧入口停签发与最低客户端版本 |
| 4 / DB | pending/attempt/逻辑到 physical winner 映射、reader、删除 intent | 3、预约迁移号 | 新旧可解析且部分 NULL 失败关闭；映射与 winner 原子可见，不建立第二附件主表 |
| 5 / 后端存储 | `elib_s3_sign`、`elib_oss`、garage driver 有界 HEAD/Copy/proof/签名 | 3；启用需2 | A1 实桶通过；新路径无正文 GET fallback，未验 profile 不启用 |
| 6 / 后端会话 | register/grant/finalize/reconcile，适配现有 direct 代码 | 4,5 | 单 dispatch、唯一 winner、最终 proof 后确认；COMMIT/网络未知可恢复；旧 v5_object_bytes 不被新协议调用 |
| 7 / 生命周期 | 配额、candidate claim、unknown、purge/GC/hold/intent 适配 | 4,6 | 真实 PG 并发下不超卖/误删；staging/unknown/confirmed 未 bind 均有账 |
| 8 / Flutter | source/API/uploader/journal/续期/传输、错误恢复 | 3；接线4–7 | 普通首传与恢复真实 S3，无 JWT/Cookie 泄漏，无失败转 gateway |
| 9 / Flutter E2EE | 文件 seal/open 及实际调用方 | 8 | 固定向量等价，同 session 不再加密；tag/hash 失败前无可见明文 |
| 10 / 后端+App+边缘 | 普通 view/download/resolver/缓存、强撤权边缘 | 3,4；启用6,7 | 撤权与缓存按冻结合同；历史 URL 排空；正文不进 BEAM |
| 11 / 群业务+App | 群文件/相册 multipart/base64 改引用绑定 | 4,6,7；图片规则按需12 | 权限、归档、锚点、批量/部分成功/幂等均回归；旧路由登记到24 |
| 12 / Worker | 内容检查、图片派生、认证 receipt 与有界任务编排 | 3；接线4,6 | 同字节规则等价；伪造/跨 attempt/超时失败关闭；不处理 E2EE 明文 |
| 13 / Admin+后端 | Admin 头像会话/直传/检查/公开发布 | 4–7,12 | 浏览器 CORS/签名头正确，无借用 Human 身份；无 BEAM 读回再 PUT |
| 14 / 企业+三端 | Internal/Member/Platform/Seat/Visitor/Widget/App 全上传认证面 | 4–7,12 | 每面真实 HTTP；认证隔离；upload_ref 只作控制凭据；无同源正文兜底 |
| 15 / 企业+边缘+前端 | 原 content 路由授权适配、私有 origin 正文传输 | 4,10,14 | 逐请求权限、URL 隐藏、Range/缓存隔离；不能改成暴露内部签名的重定向 |
| 16 / 存储，可选 | MPU 的创建/part/complete/abort/恢复与最终 full proof | 4–7 | 未选仅 T37–39 可记范围外；选入后必须真实完整闭环，不降低单 PUT 保证 |
| 17 / 迁移+DB，必交 | 历史 disk 来源/数量/引用/队列盘点；外部迁移工具、proof、CAS 位置切换与核对 | 1,3–7；源读权限 | 有历史则迁移后可读可删；无历史用明确覆盖范围的盘点证据证明；未知/不可达不当空库，不删源 |
| 18 / Worker+后端，必交 | 现有后台导入、预览、派生、公开复制与 `elib_oss` 正文调用者迁出 | 1,4,6,7,12按需 | 既有启用功能不退化；网络未知可恢复、SSRF/输出 proof 成立；源保留策略明确 |
| 19 / 扩展，可选 | Moya 第四仓 SDK/原始 PUT/POST policy 复核 | 1,3；实测2,5 | 单独范围/版本/证据；不把未做第四仓宣称为全部外部客户端兼容 |
| 20 / 集成验证 | T01–T60、故障/兼容/性能/适用全量门禁 | 1–15,17,18,23–26及22的切换草案；16/19选入则也需完成 | 同一最终候选 A1/B/C/D/E 全过；任何必交入口失败保持 NO_GO |
| 21 / 审查 | 最终差异、SQL权限锁序、认证、正文路径、日志、门禁证据 | 20；准备可提前只读 | 未解决项逐项裁决，不能以旧 E2EE PASS 或扫描工具代替真实验收 |
| 22 / 运维+集成 | 切换、停止签发、排空、撤回与恢复计划 | 草案3后；发布资格需20,21 | 切换前/删除后的恢复边界分开；实际迁移/发布按授权，不自动执行 |
| 23 / 录音+Worker+客户端，必交 | 录音分片首传/续期、拼接、播放、处理输入与导出全部数据面 | 3–8,10,12；派生接线18 | 无 gateway 续期，音频/导出正文不进 BEAM；顺序/幂等/取消/恢复与功能等价 |
| 24 / 三端+集成，必交 | 旧 gateway/multipart/base64 停签发、排空、路由退役及旧客户端错误 | 8–15,17,18,23；政策3 | 全入口有停签发/会话/URL/在途副作用证据；最终只剩有界升级错误，无正文读取 |
| 25 / 存储+构建，必交 | 删除 disk driver、NIF 包装/C 源/构建产物/配置/调用；测试重整与 ADR 落地 | 17,24；调用者迁完 | 无生产 disk 读写、注册和 fallback；构建不再编译/装载此 NIF；历史不靠 driver 才能恢复 |
| 26 / 运维+质量，必交 | S3/边缘/Worker 配置、备份恢复、可观测性、静态及集成门禁 | 3；完整验证需23–25 | endpoint 配置化；不误删其他临时盘或系统依赖；OTP 29/28、PG扩展及附件专项都通过 |

主依赖：1→3→4/5→6/7→各业务接线→24/25→20→21→发布资格。2 的真实资源缺失可允许独立代码/mock 开发，但阻塞 profile 启用。17 历史盘点、12 Worker、10 边缘可提前设计，不依赖最后退役完成。22 的切换草案提前形成，不把实际迁移推迟到删除 driver 之后；20不依赖21的审查完成或22的最终发布资格，避免验收与发布依赖环。

## 7. 不变量、门禁与 T01–T60

I01 身份从上下文推导；I02 提交后发能力；I03 expected/observed 分离；I04 checksum 签名真实绑定；I05 客户端无 sealed 写能力；I06 uploaded/confirmed/bound/published 分离；I07 控制/绑定/读/删逐操作鉴权；I08 hold/retention/引用门；I09 冻结完整物理身份；I10 短事务不等待网络；I11 未知结果可恢复；I12 fence 不撤外部请求；I13 staging 无用户读面；I14 协议/会话版本固定。以上均为要求，不是已验证结论。

| 门 | 最终通过条件 | 当前状态 |
|---|---|---|
| A0 | 固定 provider/版本/权限/代理独立能力探测 | NOT_RUN |
| A1 | 真实应用签名器+driver+HTTP 的 S3 合同，含浏览器/反代负向 | NOT_RUN |
| B | 最终同字节 proof、winner/GC/撤权/配额/恢复真实并发 | NOT_RUN |
| C | §3 全部必交入口上传/confirm/校验/下载/派生/录音正文经 BEAM=0 | NOT_RUN |
| D | 三端与旧客户端/旧会话/历史引用/删除/回滚兼容，真机/浏览器与适用全量检查 | NOT_RUN |
| E（新增） | 历史 disk 清账迁移、旧入口排空、NIF/driver/配置/生产调用退役、备份恢复全部成立 | NOT_RUN |

所有 T 当前为 `NOT_RUN`，harness 未交付时记 `MISSING_HARNESS`，不能写假命令。T42/T43/T50/T48 的新义务不继承 V2.2 结果；T37–39 仅 MPU 未选时可范围外，其他必交项不能为变绿改成可选。

| T | 必须验证的行为与关键断言 | 交付/入口 |
|---|---|---|
| T01 | direct register无URL；grant回滚/COMMIT响应丢失/恢复查询失败均不越权签发，无重复reservation | 4–6；新增direct事务故障harness |
| T02 | 同client_upload_id同声明同session，确认/恢复不重复记账 | 6,7；pending单测+真实PG |
| T03 | 换hash/size/MIME/scope冲突，不复用旧grant | 6；协议与PG |
| T04 | 跨实例/主体/应用凭据scope操作status/renew/finalize不可成功且不泄露 | 6,14；各认证面真实HTTP |
| T05 | 错SHA/换正文，存储拒绝或最终不可确认 | 5,6；A1实桶 |
| T06 | 删除/替换checksum头失败，32字节Base64编码正确 | 5；签名单测+A1 |
| T07 | copy-source/ACL/额外x-amz头/额外method不扩大能力 | 5；A1负向 |
| T08 | 普通104857600+1、EB26214400+1、E2EE边界与tag开销，硬限额拒绝点/残留明确 | 5,8,9,12,14；各数据面 |
| T09 | 空文件、字段/part/batch限制，无无界累积 | 8,11,14；真实入口 |
| T10 | 中文/空格/+/%、显式端口、路径前缀签名与请求一致 | 5；A1真实代理 |
| T11 | 浏览器禁设Content-Length、HTTP2/chunked，不绕过数据面上限 | 5,13,14；真实浏览器/代理 |
| T12 | metadata、无来源SHA、COMPOSITE均不伪作full proof；Worker替代需独立通过 | 5,6,12；A1/B |
| T13 | HEAD后/Copy中/预扫描后换源，最终字节与Worker receipt决定结果，无错winner | 6,12；实桶故障注入 |
| T14 | confirmed后重放旧PUT只能影响staging，不改winner | 6,7；实桶 |
| T15 | GC/Delete后迟到PUT/Copy重现不恢复逻辑资格，tombstone与容量继续核对 | 7；实桶+PG |
| T16 | Copy HTTP200含XML错误、超大错误体均不确认，控制内存有界 | 5,6；受控provider故障代理 |
| T17 | Copy响应丢失/dispatch后发包前kill/确认前kill/HEAD可见但unknown，不重写原target | 6；实桶+故障代理+PG |
| T18 | 旧lease写/旧fence提交/新winner后迟到操作不覆盖，未解决对象有账 | 6,7；多worker故障 |
| T19 | kill/重启/网络分区恢复session/额度/任务，不无限pending | 6–9；应用恢复 |
| T20 | cancel/revoke/hold/purge与winner/GC claim互斥，无已删对象成为winner | 6,7；真实PG并发 |
| T21 | URL/session到期但已开始上传，迟到写受控，不能只凭时间清账 | 6,7；数据面限时注入 |
| T22 | 消息锚点、收藏/转发/群文件等重复绑定不重复引用 | 7,11,14；业务HTTP+DB |
| T23 | 退群/归档/撤权后重放已有GET/HEAD/Range地址，按冻结scope合同拒绝或保留明确窗口 | 10；真实边缘+客户端 |
| T24 | 固定key/nonce/input新旧字节等价；末块/tag/full hash失败前无可见明文 | 9；既有codec向量+文件端到端 |
| T25 | 首传/断线/重启复用同密文文件；App恢复无整文件readAsBytes | 8,9；真机+内存采样 |
| T26 | attempt变化不改logical key/descriptor；新发→旧收；读/删命中正确physical winner | 4,9,10；兼容候选 |
| T27 | 相册批量逐项结果、上限、部分失败与重复调用，未confirm不可绑 | 11；真实业务HTTP |
| T28 | Admin身份建会话/绑定头像，S3无Admin/Human凭据；处理/发布未过不匿名可读 | 12,13；浏览器+后端 |
| T29 | Seat/Widget/App登录切换/AbortSignal/epoch隔离；业务Bearer仅同源允许路由，S3绝无 | 8,13,14；真实认证面 |
| T30 | MIME/魔数/尾部控制字节/扫描后换对象；与现有规则样本等价且绑定winner字节 | 12,14；Worker实对象 |
| T31 | Worker超时/kill/伪造回执/跨attempt同size/过期ruleset全部fail-closed | 12；认证回执负向 |
| T32 | 企业撤权后下一请求拒绝，缓存不跨身份 | 15；真实HTTP |
| T33 | 私有origin/internal鉴权/重定向/SSRF不可绕过，错误不泄漏存在性 | 10,15；真实边缘 |
| T34 | GET/HEAD/Range/206/416/304/取消/鉴权状态转换/缓存隔离正确 | 10,15,23；真实数据面 |
| T35 | staging/candidate未发布无匿名读；公开创建与业务许可顺序正确 | 12,13；真实匿名请求 |
| T36 | 变换/公开复制响应丢失和DB失败可核对，不冒充跨DB/S3原子 | 12,13；故障注入 |
| T37 | MPU乱序/重复/缺失/跨session part、complete重试，服务端决定精确组合 | 16；独立MPU实桶 |
| T38 | composite独立字段；最终完整SHA由可信proof形成 | 16；MPU实桶+独立hash |
| T39 | abort/complete/迟到part、恢复及容量/孤儿收敛 | 16；并发故障 |
| T40 | driver/profile/凭据轮换过程中旧会话冻结身份，不改用默认bucket | 4–7；registry与实存储 |
| T41 | 未知历史/无GC proof/缺映射不猜bucket、不误删 | 4,7；历史fixtures |
| T42 | 历史 disk 盘点与迁移工具拒绝路径穿越/符号链接/源变化；零历史结论覆盖主表、队列、配置与来源 | 17；合成源与隔离历史 fixtures |
| T43 | 迁移中 kill/CAS 失败/并发删或 hold 可恢复；去除 disk runtime 后恢复备份仍可读可删，不依赖 NIF | 17,25,26；故障与恢复演练 |
| T44 | HEAD/Copy/XML/headers/ListParts错误有硬上限，拒绝时不先完整收体 | 5；故障HTTP服务 |
| T45 | 应用/边缘/Worker/客户端日志无signed URL、JWT、Cookie、key | 5–15；合成canary扫描 |
| T46 | 多节点reservation/confirm未bind/cancel/unknown容量，不超卖/重复释放 | 7；真实PG并发 |
| T47 | profile能力缺失明确拒绝新grant，不静默退BEAM | 5,6,8,14；真实响应/客户端 |
| T48 | 停签发/过渡回滚保留 reader/reconciler/映射/intent；删除后只回滚兼容 S3-only 版本，无自动恢复 disk/gateway | 4,7,22,24–26；回滚演练 |
| T49 | 固定样本×并发，吞吐/尾延迟/普通消息延迟/各组件内存/GC恢复达事前阈值 | 20；授权压测harness |
| T50 | §3 全部必交入口上传、confirm、Worker、下载、录音与派生正文经 BEAM=0；无旧网关/编码/流块兜底 | 20；运行观测+完整 source/sink 审查 |
| T51 | 新注册、续期、重试、恢复遇 S3 故障不发 gateway 能力；旧客户端得到明确升级错误且不读取正文 | 8,14,23,24；真实客户端与路由负向 |
| T52 | 无 disk 注册/分派/回退/生产调用；新旧配置处理明确；全新构建和发布包无专用 NIF 源/包装/产物 | 25,26；静态引用审查+干净构建/启动 |
| T53 | 所有活动 disk 引用、pending 和删除意图有迁移/排空证据；未知来源 BLOCKED；迁后逻辑 key/E2EE 描述符不变 | 4,17,24,25；盘点与历史合成 fixture |
| T54 | 录音分片首传/续期/断线/重启均直传，编号/哈希/确认/幂等/取消不退化 | 23；真机+HTTP/DB/S3 |
| T55 | 录音拼接/播放/Range/处理输入/导出由外部 Worker/边缘处理；故障恢复不重绑、不把正文带回 BEAM | 18,23；Worker 故障+业务回归 |
| T56 | 群文件/相册/头像/Internal/Member/Platform/Seat/Visitor/Widget 每面完成首传、恢复、确认、绑定与下载 | 11,13–15；逐面真实 HTTP/浏览器/真机 |
| T57 | S3/DB/映射/任务/回执联合备份恢复后，历史附件、hold、pending/unknown 和删除队列一致 | 17,22,26；隔离恢复演练 |
| T58 | s3.imboy.pub 配置可替换；COS/OSS 未验 profile 不启用；签名/region/寻址/Copy/checksum 不作泛兼容假设 | 2,3,5,26；配置与逐 provider 合同 |
| T59 | 合成 canary 正文及编码/密文/流块不进入 BEAM；Worker/edge 有界控制结果可区分，不把业务文本等误算文件正文 | 12,18,20,23；内存/流量观测与调用追踪 |
| T60 | PostgreSQL 扩展冷启动/迁移、OTP 29/28 编译、E2EE 与适用全量门禁通过；删除 NIF 不误删其他 C 依赖 | 20,25,26；最终候选 CI/专项 |


### 7.1 验证执行与证据

每个新增 harness 必须有实际路径、调用参数、case ID、固定 seed、故障点、行为 oracle、退出码和资源 owner；关键断言核对客户端、数据库与存储最终状态，不能只断言 HTTP 200。附件正文识别覆盖原文件、密文、编码后的正文、流 chunk、派生内容，不能通过改编码绕开 BEAM=0。

后端按项目规则先定向验证，最终统一执行 `IMBOYENV=local make eunit` 及其排除的适用专项、迁移、架构、Elvis、Dialyzer、Xref 与 E2EE；测试真实共享 DB/注册名/ETS/meck 串行或先证明隔离。Flutter 执行适用 `flutter test`/analyze，真机验证 iOS/Android，不用模拟器作功能验收。Admin 用仓库实际 test/lint/build 和浏览器 Seat/Widget E2E。既有 `attachment_lifecycle_v5_gate.sh` 等先核对真实参数和候选兼容性，不复用旧 run 作为新证据。

证据绑定三仓 SHA/dirty、计划 hash、迁移/规则/profile/代理/Worker artifact hash、case、命令/cwd、UTC 时间、exit、脱敏日志 hash、状态和未解决副作用。代码、权限、存储/代理配置或规则变化按影响使证据失效。不能仅用 rg 无命中证明运行链路无正文；需要 source/sink 审查和合成 canary/流量观测两类证据。

实现状态 `NOT_STARTED/PARTIAL/IMPLEMENTED`、验证状态 `STATIC_ONLY/NOT_RUN/PASS/FAIL/BLOCKED/INVALIDATED`、发布状态 `NO_GO/ELIGIBLE/RELEASED` 分开登记。MPU/Moya/未选 provider 的范围外证据必须说明具体范围，不能宣称全产品全部 provider 已通过。

## 8. 历史迁移、退役与恢复

### 8.1 迁移及无历史数据分支

1. 先在授权范围盘点主表、pending/session、位置映射、普通/企业 deletion queue、orphan、派生文件和录音引用；分类 S3、disk、FastDFS、未知。范围需覆盖该部署的所有配置 namespace 与备份恢复来源；只统计一张表或一台目录不算“无历史”。
2. 有历史 disk 时，由隔离外部工具读取允许目录，拒绝路径穿越/符号链接/源变化；保留稳定逻辑 key、业务附件 ID、E2EE 描述符/AAD，密文只复制不解密。目标在获准 S3 写入并核对最终 proof 后，短事务按旧位置/版本 CAS 切换映射；并发删、hold、变更走明确裁决。
3. 同步迁移读/删消费者及冻结的删除意图；不能把旧 disk intent 的位置字段随意改成新对象，必须有版本化账本和对账。源/目标重复期间容量、备份、保留和清理所有权明确，未知结果不重放副作用。
4. 切换后验证历史消息/收藏/群文件/企业/录音可读可删；源数据按保留与回退合同保存。没有数据也需可审阅的覆盖清单和计数证明；未知或不可达是 BLOCKED，不是零。
5. 移除 disk runtime 前，所有活动引用、旧 session 和待执行队列不再需要其 driver。未解决的 disk late-write/unknown intent 必须外部工具可恢复并有责任人与期限，不能静默丢失义务。

### 8.2 清理清单与例外

| 对象 | 最终要求 |
|---|---|
| `src/lib/attachment_storage/attachment_storage_disk.erl` | 删除；不得移名保留等价生产磁盘存储能力 |
| `src/lib/attachment_storage/attachment_disk_lock_nif.erl`、`c_src/attachment_disk_lock_nif.c` | 删除专用 NIF 包装和 C 源；清理构建缓存/发布包中的遗留 `.so/.dylib` 并验证全新构建 |
| `Makefile` 的 `C_SRC_OUTPUT` 等 | 按实际引用删除本 NIF 专属构建/打包规则；不改 vendored `erlang.mk`，不误删其他依赖的 C 构建能力 |
| `attachment_storage`、app/env registry、handler、业务读写/派生分派 | 移除 disk 注册、分派与回退；未知/旧配置启动失败给明确迁移错误，不自动转默认 S3 桶 |
| config/Compose/Helm/运维脚本/ADR/文档 | 移除现行 disk 配置、持久化目录和权限说明；其他服务/Worker 临时盘按用途保留；部署改动先读 deploy/README 与本目录规则 |
| 历史迁移文件与测试 | 不改写已发布迁移；新迁移处理当前约束/数据，保留历史 fixture 用于迁移和拒绝策略测试；删除仅验证已退役产品能力的测试，不能减少业务断言 |
| 备份与离线迁移工具 | 可只读理解历史 disk 标记，但不在新生产请求链装载 driver；职责、受限路径、生命周期和清理权限明确 |

删除专用 NIF 不等于删除所有 C 依赖；pg_jieba、PostGIS、TimescaleDB、E2EE 相关依赖等不在本清理范围。Worker 临时文件与客户端缓存不算历史 disk 产品能力，不应被静态门禁无差别禁止。

### 8.3 切换与回滚边界

可以按入口先交付中间候选，但其状态为 `PARTIAL`，不能称 V2.3 完成。切换前完成新链 A1/B 与所启入口验收、最低客户端政策、停止旧签发、旧会话/URL/在途写排空、容量对账；再撤旧生产路由与 driver，冻结完整候选执行 C/D/E 和最终检查。

删除前回滚可停止新 grant 并使用尚在过渡期的旧部署，但必须保留新协议 reader/reconciler/映射/intent，不把新对象重新解释为旧位置。删除后的正常回滚只允许回到兼容新映射的 S3-only 版本；不得自动打开 disk/gateway。灾难恢复至历史版本需独立恢复方案、DB/对象版本匹配和实际授权，不能靠恢复 NIF 文件冒充可回滚。

## 9. 运行资源、性能与运维验收

沿用固定样本 10,000/100,000/1,000,000/25,000,000/100,000,000 bytes，补普通/企业限额和 E2EE tag 边界；企业超限只测拒绝。并发从1/4起，资源允许后16；冻结每 run 字节/费用、CPU/RSS/临时盘、会话/Copy/Worker 并发、重试与 unknown 上限，未冻结不做性能验收。

同 scope、字节、校验强度、TLS、缓存和网络比较端到端可绑定时延、吞吐/错误率、普通消息 p95/p99、BEAM total/binary/RSS、DB等待、客户端/Worker/边缘/存储 RSS、staging/candidate/unknown 容量、GC年龄和恢复。对照包含旧上传与 confirm GET 全链，不把半链比较当收益。阈值在测试前定稿，p99附样本数，不虚构提升比例。

部署需客户端可达存储 endpoint、公网/内网 endpoint 分离、TLS/CORS/CSP、短期最小权限、凭据轮换及签名时钟；不可硬编码 `s3.imboy.pub`，也不向客户端发永久密钥。故障指标覆盖 presign/PUT/finalize/Worker/edge/GC/配额，监控标签有限枚举，signed URL、JWT、Cookie 和完整对象定位不进入普通日志。

备份恢复同时覆盖数据库、S3 对象/位置映射、必要的 Worker 任务与回执、存储配置；进行带历史迁移对象、hold、pending/unknown 的隔离恢复演练。S3 不可用时新上传明确失败/重试，文字消息与既有控制服务按既定降级合同工作，不启动本地 disk 兜底。合成资源有 owner/run/lease/额度和清理状态，不全桶 purge。

## 10. 执行与完成判定

执行顺序按 §6 依赖选择，不机械串行编号。旧 V2.2 的 22 条命令映射不再作为 V2.3 执行清单；Step 17/18/20/22 已改义，另新增23–26，禁止把旧 batch 直接粘贴执行。使用实际可用工具与各仓规则，工作卡描述本身是 scope；本文不依赖某个 orchestrator 命令能否携带参数，也不隐含启动子 agent。

实施启动提示词：

```text
以 docs/architecture/s3-direct-transfer-plan-v2.3.md 为唯一当前附件改造范围，先做 Step 1，核对三仓最新 main/PR、WIP、已有 direct 实现和适用规则。不要重复实现已有能力，不改其他会话工作。最终要求全部必交用户上传 S3 直传、BEAM 正文为零、完整保留业务授权/生命周期，并退役 disk/NIF；强撤权下载使用独立边缘，内容检查/录音/派生使用独立 Worker。逐卡填写实际 owner、写集、依赖、harness/case 与证据状态，再按依赖实施当前授权范围。历史未知数据、真实资源缺失和关键合同未定明确 BLOCKED，不伪 PASS，不缩范围，不扩基线、不关闭 CI。实际迁移写删、提交、push、合并、发布遵守当时已有授权。本计划修改不是后续全部操作授权。
```

**最终完成条件**：26卡中所有必交卡完成，§3全部启用功能保持等价，Garage 指定部署 A0/A1/B/C/D/E 通过；T01–T60逐项有本候选证据或仅允许的可选项范围说明；真实 Android/iOS 与浏览器验证完成；适用全量与专项门禁通过；disk/NIF/gateway 无生产正文路径；历史对象与队列可恢复、可读可删；最终审查无阻塞。COS/OSS 等未验 profile 不启用、不宣称支持。实际发布状态单独登记。

本次交付仅为本文，产品代码、真实存储、数据迁移、验收与发布均未执行；当前保持 `NO_GO`。不提交、不推送、不合并，不改其他计划文件。
