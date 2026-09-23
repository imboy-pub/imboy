# Changelog

本文件记录 IMBoy 作为**标准 SKU** 对外发布的版本变更。
格式遵循 [Keep a Changelog](https://keepachangelog.com/zh-CN/1.1.0/)，版本号遵循 [Semantic Versioning](https://semver.org/lang/zh-CN/)。

> 工作区包含三个可独立发布的子项目（`imboy` 后端、`imboyapp` 客户端、`imboy-admin-frontend` 管理后台）。
> 1.0.0 起版本号在 workspace 层统一管理，以根目录 `VERSION` 文件为权威。
> 子项目各自的内部小版本（如 backend 0.7.x、app 0.8.x）保留在各自仓库的 CHANGELOG 中。

---

## [Unreleased]

### Fixed

- 契约漂移 #2：管理后台内置角色 4/5/6（内容审核/安全治理/客服）补展示名——抽 `shared/adminRoles.ts` 单一映射对齐后端 `role_acl/1` 真源（imboyadmin `cf9ad7b`）
- 契约漂移 #7（收口）：上行 WS action 注册表与下行 S2C action 清单均入契约门（`api_contract.json` `ws_actions`/`ws_s2c_actions` 段，format_version 3）；App 上行常量收编 `C2SAction` 并接单向比对（imboyapp `783b752f`）；新发现 #9（App WS `message_reaction` 通道服务端不认）登记待拍板

### Docs

- 三端文档体系整理（审计报告 `docs/documentation-system/2026-09-19-documentation-audit.md`）：新增全项目术语表 `docs/glossary.md`、核心概念层 `docs/concepts/`（账号/协作层级/消息/E2EE/智能体/客服/企业业务八篇）、三端 API 对齐与契约漂移登记 `docs/api-contracts/three-platform-alignment.md`；修正 `docs/CONVENTIONS.md` 三处与代码相反的约定（整数码信封、WS action snake_case、TSID 路径现状）；版本锚与部署监控数字对齐（4 job / 33 条告警规则）；全仓内链死链清零；文档站（GitHub Pages）接入概念层与术语表

---

## [1.0.0-alpha.82] - 2026-09-23

- 生产 902 修复：Web 坐席工作台（管理后台 seat/）在 `api_auth_switch=on` 的生产环境被设备签名门拦死（信封 902"签名验证失败，请更新客户端"）——浏览器无法持有 APP 设备 HMAC 密钥，开发期 `off` 掩盖了设计缺口。`cs_http:is_web_seat_surface_path/1` 冻结声明坐席工作台消费的 `/api/v1` 合同路径族（seat-contexts/queue/详情/claim/transfer/close/active-closed 视图/转接目标/SSE 事件流 + 企业消息历史与发送两条复用路径），`auth_middleware_api_v1` 据此免 `verify_sign`；JWT 门不放宽（不在 open/option 名单，缺 Bearer 照常 401）

---

## [1.0.0-alpha.81] - 2026-09-23

- 挂件面 CORS 二段修复：`/api/v1/cs/widget/*` 路径前缀兜底归入 widget 面（此前落 undefined 走全局白名单，frame 跨域上传一律 403）；`cors_widget_origins` 补入网关域 cs.imboy.pub（挂件 frame 是实际跨域发起方）

---

## [1.0.0-alpha.80] - 2026-09-23

- 挂件面 CORS 修复：widget 预检 allow_methods 补 PUT（附件裸上传被浏览器预检 403）；生产配置补 `cors_widget_origins`（宿主页 imboy.pub / www.imboy.pub）

---

## [1.0.0-alpha.79] - 2026-09-23

- 客服挂件建会话幂等接续：同 contact 已有开放会话时原样返回（与新建同形状），访客重开面板/网络重试不再 409 中断；单会话约束保留，`session_already_open`→409 映射保留兼容

---

## [1.0.0-alpha.78] - 2026-09-23

> 本条目覆盖 `1.0.0-alpha.77` 后并入 main 的客服挂件生产化与附件能力扩展。

- 企业附件白名单扩至 20 类常见文件（文本类 md/csv、gif/webp/bmp/svg、zip/7z/gzip、OLE2、OOXML 三件套、mp4/webm/mp3），魔数复核规则同步扩展；新增无 DB 依赖纯单元套件 `eb_asset_content_tests`（45 例）
- 客服挂件前端（imboyadmin 侧同日发布）：附件选择框按白名单 accept 过滤 + MIME 硬校验；聊天界面与 launcher 外壳现代化翻新
- adm 组织 owner-transfer 审计 detail 补记 previous_owner_id；imboy-deploy 部署脚本修正

---

## [1.0.0-alpha.77] - 2026-09-18

> 本条目汇总 `1.0.0-alpha.75` 之后并入 main 的企业组织 V1、客服挂件、Agent Runtime V3.1、
> 墨芽教学修复与运维集成变更。Hirð 运行时为候选态（生产采用需独立 ADR）；AI 模型、生产部署
> 与真实终端验收仍需在目标环境单独完成。

### Added

**企业组织 V1 / Organization**
- 新增组织基座：Owner 真源过渡、成员生命周期（挂起/恢复/离岗 API 与 CAS 冲突 409）、邀请生命周期（含平台邀请）、部门目录与显式默认工作区（迁移 `00000126`-`00000131`）；组织事实与 Agent 边界接口冻结入库
- 新增用户删除预检编排器：五域注册表 fail-closed，任一域缺失或不可得即整体硬拒；新增 agent 域 provider——用户名下存在活跃 bot 时产出 `AGENT_OWNER_ACTIVE` 阻断（bot 归属无外键保护，预检为其删除安全网）
- 新增组织 v2 API 契约与路由集成、平台管理后台组织治理 API

**客服挂件 / Customer Service Widget**
- 新增挂件持久化基础四表（installation / identity_key / nonce / 会话键，迁移 `00000132`）
- 新增挂件 HTTP 面：应用与门面契约、SSE、CORS 与裁剪策略；坐席工作台 HTTP 与生产装配
- 新增管理端挂件安装端点；客服-组织兼容适配器（成员挂起即时撤销、组织归档拒绝新会话/认领）

**智能体运行时 / Agent Runtime V3.1**
- 新增授权基座：Agent Grant 四表（授权/委托/能力/工作区，迁移 `00000133`）与委托命令；委托人限定真实人类账号（account_type=0）
- 新增运行基座：Run/Event/Effect 三表与冻结状态机（迁移 `00000134`）、效果账本 CAS 写入、租约接管与恢复（重授权全链重走、unknown 显式对账、终态冻结出边）
- 新增中央工具授权器：十步 fail-closed 决策链、人工审批（HITL）效果与摘要匹配、终态竞态（TOCTOU）同事务防护
- 新增消息/定时/Webhook 三类触发器：幂等探针、组织归档硬门、默认工作区经组织关系 API 解析（禁最小 ID 推导）；Webhook 须验签通过
- 新增 Native 与 MCP 工具适配：Agent 授权门先于 MCP 治理门；工具参数与结果仅存摘要
- 新增 Hirð 运行时候选桥（防腐层，G01-G20 候选门通过；生产采用需独立 ADR）

**企业业务 V4.1 / Enterprise Business**
- 企业业务 Foundation 全量与客服域全链候选并入主线（企业身份/经办绑定/坐席/会话闭环）

**运维与集成 / Ops & Integrations**
- 接入 OpenTelemetry 并对接 Uptrace（`trace.imboy.pub`），附安装与对接指南
- 新增微信小程序「消息推送」接收端点（GET 验签 + 兼容模式解密）
- 对象存储端点支持 env 解析与 `key_prefix` 命名空间；新增微信登录配置体检脚本与本地密钥模板 `.env.local.example`

### Changed

- 迁移链在 main 统一收口至 `00000134`（组织 `126`-`131`、客服挂件 `132`、智能体 `133`-`134`）；编号经共享台账协调，全链隔离库实跑验证通过，无重复无新增跳号（历史缺号 `00000041` 除外）
- AI 回课超时可配置，新增结构化校验降级开关；`ai_draft.result` 出口统一解码为 JSON 对象

### Fixed

- 认证：过期/畸形 token 统一映射 HTTP 401（错误码 `705`/`706`）
- 墨芽教学：作业截止门、历史访问 staff 组织校验与撤回读守卫；双角色撤回提交读回退 parent 视图；db_error 折叠、事务内发布守卫与僵尸草稿过滤；回放 `ai_status` 反映当前状态并应用白名单/学员上限
- 客服挂件：installation 行 jsonb 列读路径解码、`allowed_origins` 以 jsonb 数组编码、identity 摘要统一小写、presign 转发 `object_hash` 与 422 映射
- 组织：邀请目标缺失映射业务 404、部门读路径按 active 成员资格门控、管理端关键字过滤崩溃修复、内置角色 ACL 补登客服/企业业务权限
- 遥测：3 条 dialyzer 基线新增告警清零（纯类型修复）
- 工程脚本：cron 门禁 macOS 恒红修复、发布门禁补 CHANGELOG 目标版本标题校验、dialyzer 门红退出码 `141` 修正为 `1`

## [1.0.0-alpha.75] - 2026-09-14

> 本条目汇总 `1.0.0-alpha.72` 之后纳入当前发布线的后端变更。外部 AI 模型、生产部署与真实终端验收仍需在目标环境单独完成。

### Added

**墨芽教学 / Moya**
- 新增组织与教学身份、学员绑定、作业布置、提交/撤回、老师回评、私密媒体访问和管理审计后端闭环（迁移 `00000095`-`00000100`）；任务 ID 对外统一为十进制字符串，提交幂等、状态流转和双角色 ACL 均由服务端约束
- 新增点评历史未读数、老师署名、作业最新作品预览句柄及逐字点评 `char_reviews`（迁移 `00000110`）；AI 草稿支持图片与受开关控制的视频输入，并增加 provider、模型、密钥和媒体能力启用前置检查

**组织与工作区 / Organization & Workspace**
- 新增组织成员管理、角色调整与 Owner 转移 API（迁移 `00000113`），区分个人工作区与组织工作区的创建权限，并在教学域统一校验组织上下文
- 新增由客户端选择的产品术语 profile，内置 `generic` 与 `moya` 术语表；无效 profile 回退通用术语

**Agent Hub 与集成 / Agent Hub & Integrations**
- 新增持久化 Agent Task、事件和审批仲裁（迁移 `00000090`），提供幂等任务执行、人工审批及运行审计；Tool Loop 采用有界轮次，写工具默认进入审批
- 新增独立 MCP client 凭证治理（迁移 `00000091`）、Bot webhook 投递 outbox 与 Channel webhook token 摘要（迁移 `00000092`-`00000093`），支持群内 `@Agent` 分派、受控回复与支付指令识别

**附件与治理 / Attachments & Governance**
- 新增 `POST /api/v1/attachment/upload` multipart 流式上传通道，供不支持对象存储 PUT 的客户端使用；上传仍须经过 pending 归属、大小/MIME 校验和 confirm 落库
- 新增群附件消息锚点与当前成员资格授权（迁移 `00000108`），群附件 confirm 必须携带 `anchor_msg_id`，访问时按锚点和 membership fail-closed
- 新增公开内容审核策略、运营审核队列和处置申诉链（迁移 `00000094`）；E2EE 私信不进入服务端明文审核，申诉实行一次提交、独立复审和结果隐私裁剪
- 新增有界用户数据导出、冷却限频与范围声明，以及可配置的过期验证码清理 worker；清理任务默认关闭，需部署配置显式启用

### Changed

**许可协议 / Licensing（全仓）**
- 后端 `imboy` 与管理后台 `imboy-admin-frontend` 的许可证由木兰宽松许可证第 2 版（MulanPSL-2.0）切换为 **Business Source License 1.1（BSL 1.1）**：源码公开；Additional Use Grant 覆盖组织内部生产使用、单客户私有部署、集成进自有产品；多租户托管（SaaS）与竞争性付费产品或服务需要商业授权；每个版本自首次公开发布满四年后自动转为 MPL 2.0
- 移动客户端 `imboy-flutter` 保持 MulanPSL-2.0 不变
- 旧许可文本保留为 `docs/legal/MulanPSL-2.0.txt`。**许可证变更不具追溯力**：2026-09-14 之前发布的版本仍按 MulanPSL-2.0 授权，不受本次变更影响
- 新增许可策略说明 [`docs/legal/licensing.md`](./docs/legal/licensing.md)

**消息与发布 / Messaging & Release**
- E2EE 群历史改为按成员世代、权威收件人快照和群会话证明有界访问（迁移 `00000101`、`00000109`、`00000111`、`00000112`）；退群后重入、祖传无证明记录和解散后的开放世代均按 fail-closed 处理
- C2G 离线时间线绑定权威会话序号，发送链重新校验发送者角色并固化收件人快照；C2C 下发前按设备信封过滤，减少不可解密占位消息
- 发布构建启用 `full-selected` 产品切片及 Feature Slice 边界检查；迁移新增命名、up/down 配对与发布一致性门禁

### Fixed

**墨芽教学 / Moya**
- 修复 AI 回评 provider 未进入运行配置导致功能恒不可用、视频模型与响应脱壳不匹配、点评时间戳口径不一致，以及点评草稿/发布竞态和已发布回评错误码不明确的问题
- 修复老师队列筛选与分页边界，补齐待点评/已点评状态过滤；加固两段式点评写入、逐字点评输出白名单和图片点评发布条件

**消息、频道与附件 / Messaging, Channel & Attachment**
- 恢复 `e2ee_room_key` WebSocket action 注册；结构性落库失败会进入明确终态，C2G 群标识、请求收件人与 staging 快照不一致时拒绝处理
- 修复 multipart 上传鉴权顺序、临时文件/对象存储写失败的 5xx 分流与授权错误吞没问题；临时文件使用独占权限并在请求结束后清理
- 修复频道创建者未随创建事务成为订阅者、直播间列表重复 `list` 键导致客户端解析为空，以及找回密码缺少 mobile/sms 分支的问题

**部署 / Deployment**
- 加固蓝绿部署的版本一致性、健康检查、目标槽位和重复发布判断；切流时同步管理端 upstream，并排除本地运行数据及开发配置误同步

### Security

- 用户端与管理端新增会话 epoch、独立 session 存储、凭证强度/密钥策略和 WebSocket 撤销校验；密码或凭证状态变化后可使旧会话失效
- 管理员消息访问拆分元数据、正文和导出权限；正文访问要求有效工单与原因，每次访问必须写审计记录，审计失败时拒绝返回内容
- 新增 moderator、security_admin、support 最小权限角色，并禁止自我分配角色、自我停用及授予超出操作者权限集的权限
- 日志 sink 统一递归脱敏 token、密钥、手机号、邮箱及 URL 敏感参数；脱敏失败返回占位符，不回退输出原始内容
- MCP、Channel webhook 与 Bot webhook 凭证改为摘要/一次显示/轮换撤销模型，并加固 SSRF 目标固定、投递目标不可变、有限重试和 Bot 自触发环

---

## [1.0.0-alpha.72] - 2026-09-07

### Added

**imboy（后端 / Backend）**
- 举报升级为一等消息举报目标（R-01）：`POST /api/v1/report` 支持 `target_type=message`（c2c/c2g/channel 三表面），工单落库稳定服务端行 ID + 子类型 + 会话范围 + 作者 + 结构化 evidence（迁移 00000087）；reason 8 值白名单、限流复用 `agent_rate_limiter`、目标存在性与举报人可见权校验（跨会话/跨群/跨频道 IDOR fail-closed）、已撤回/已编辑目标语义明确、同对象重复举报幂等；E2EE 消息仅在举报人明确同意（`e2ee_consent`）后接受最小摘录证据，服务端不解密、不产服务端内容哈希
- 管理端新增 `GET /api/adm/report/detail`（`reports:read` 权限门）：仅按工单返回授权范围内证据，不提供任意消息浏览入口
- `/api/v1/init` 下发 effective features（L-01）：三端可见性数据源

### Fixed

**imboy（后端 / Backend）**
- B-01：拉黑/解除拉黑立即使关系旁路缓存失效，消除 300s 旁路窗口
- E2EE：群成员资格边界强制 active 校验（非 active 成员不可收发）
- passport：account 登录查无时回退 mobile 查询
- moderation：sweep 周期任务按 `expire_due/0` 的计数 map 契约消费（修 badarith 崩溃循环）

## [1.0.0-alpha.71] - 2026-08-30

### Documentation

**imboy（后端 / Backend）**
- OpenAPI 契约补齐 project 协作域 28 条路径（W0/W1 项目与任务 + W2 成员/里程碑/频道关联/四聚合 + Admin 治理只读面 6 端点），`redocly lint` 0 errors
- REST 总目录（rest-api-v1-catalog）增补项目协作域 22 端点与频道 incoming webhook 管理 3 端点；错误码文档补 960-968 与 980（工作区归档写守卫）
- 发布基建：`release_gate_check.sh` 前置门机检、`h3_rehearsal.sh` 迁移演练单命令编排、`check_release_consistency.sh` 增无-down 迁移豁免清单（E2EE 迁移 74/75，down=安全降级/数据损毁属设计特性）并修复 relx.config 版本漏 bump；三仓未推提交 DCO Signed-off-by 全覆盖

### Fixed

**imboy（后端 / Backend）**
- 里程碑 `due_date` 经 epgsql 原生 date codec 返回 `{Y,M,D}` 元组，列表/详情接口误透传致客户端解析失败——Repo 层归一为 ISO-8601 字符串
- `imboy_ctl user create` 超列宽写入返回 500：account>40（同步 `user.mobile varchar(40)`）/ 昵称>80 现前置校验并给可读报错
- `/api/v1/init` 的 `ws_url` 未配置时透传空串致客户端 WS 假成功——新增 `derive_ws_url/1` 按请求 Host 同源派生（ws/wss 随 X-Forwarded-Proto），显式配置永远优先
- 里程碑 create/update/reach 在事务提交后回读失败时会把成功写报成失败（create 非幂等，重试致重复）——改为事务内取数（评审 M-5）；repo 层查询错误增加错误日志，不再与「无记录」静默同形（评审 M-4）

**imboyapp（客户端 / Flutter）**
- E2EE C2C 对端从未上线（设备数=0）时静默失败：现抛独立原因 `peer_has_no_device` 并三语引导「对方还没有在任何设备上登录过…」
- init 配置解密失败（服务端密钥不一致/密文损坏）误报为网络问题：新增独立分类文案 `initConfigDecryptFailed`
- 新增 envied 生成物新鲜度守卫测试：`.env.local` 与 `env_local.g.dart` 漂移即红并给处置指引

---

## [1.0.0-alpha.70] - 2026-08-29（Channel-first-class W2 / Project Workspace）

> 本版本完成后端 W2 全链、Flutter/Admin W2 治理面与全部自动化验收（后端 eunit 6490/0、
> Flutter 5940/0、Admin 1410/0、Demo B W2 双遍 66/66）。真机/真人/生产等价演练证据
> 在 ZC-12 人工门（H2/H3）完成前不存在，本条目不声称 Release。

### Added

**imboy（后端 / Backend）**
- W2 Project Workspace schema（迁移 `00000081_project_w2_foundation`，成对含 down）：`project_member`（复合 PK，重复成员只一行）、`project_milestone`（planned→reached 单向，`reached_at` CHECK 同步）、`project_channel_rel`（同 Workspace 复合 FK 强制，personal 频道不可关联）、`project.links` jsonb（形状触发器强制 `[{name,url}]`）
- Project Member ⊆ Workspace Member 双向 fail-closed：写入端/移除端可延迟约束触发器 + 复合 FK；W0 存量回填（Owner 自动入项目，34/34 无孤儿）
- `project_event` 事件契约扩 9 个 W2 值（member_invited/member_removed/member_owner_transferred/milestone_created/milestone_updated/milestone_reached/channel_linked/channel_unlinked/links_updated）
- 19 条 W2 REST 路由：成员管理（list/invite/remove/transfer_owner，邀请移除幂等）、里程碑（create/list/update/reach）、Channel 关联（link/unlink 幂等）、四类有界聚合（Pinned 排公告 / Resources / Activity 无正文 / Related Posts 有界摘要，SQL 条数固定无 N+1）
- Admin 治理只读面 4 端点（`/api/adm/project/{members,milestones,channels,aggregations}`，workspaces:read ACL fail-closed）
- Project 创建事务内 Owner 自动入项目（幂等）

**imboyapp（客户端 / Flutter）**
- W2 项目协作页：成员管理 / 里程碑 / 频道关联 / 四聚合洞察（Tab），403 明确无权限态、Guest 只读、写操作防抖、分页复位；EntityId TSID 安全解析；中英 i18n

**imboy-admin-frontend（管理后台 / Admin）**
- ProjectDetailPage W2 治理 Tabs：成员/里程碑/频道/四聚合只读面板（403 fail-closed、服务端分页、筛选复位 page=1）

### Fixed

**imboy（后端 / Backend，ZC-09 独立审查后修复）**
- 三个 meck 测试套件空转判绿改造为规范形态（67 用例恢复真实断言执行）
- Milestone reach 并发竞态：UPDATE 加 `status='planned'` 守卫，并发下恰好一次事件
- 成员 invite/remove 事务内 actor 权限复检（对齐 transfer 标准）
- Channel Owner 读分支叠加 active workspace membership 校验（fail-closed 403）
- Admin 里程碑分页 total 改独立 COUNT

### Changed

- `.contract/api_contract.json` 再生（endpoints=630）；imboyapp `lib/config/error_code.dart` 同步再生（无新增错误码，常量排序归一）
- `w0_schema_contract_tests` Gate 换档：`project_member`/`project_milestone`/`project_channel_rel`/`project.links` 从 defer 清单移入 now（多态参与表仍禁止）
- 版本：imboy `1.0.0-alpha.70`、imboyapp `1.0.0-alpha.16+6`、imboyadmin `1.0.0-alpha.16`

## [Unreleased — 1.0.0-alpha.46 公测线]

### Changed

**imboy（部署 / Deploy）**
- 生产反向代理从 Caddy 迁移到 nginx（`nginx:1.27-alpine`）+ certbot（`certbot/certbot` 自动签发/续期 Let's Encrypt）；首次部署执行一次 `bash nginx/init-letsencrypt.sh` 签发证书，`.env` 新增 `CERTBOT_EMAIL`；`deploy/caddy/Caddyfile` 已删除
  - Migrated the production reverse proxy from Caddy to nginx (`nginx:1.27-alpine`) + certbot (`certbot/certbot`, automatic Let's Encrypt issuance/renewal); run `bash nginx/init-letsencrypt.sh` once on first deploy to issue certificates; added `CERTBOT_EMAIL` to `.env`; removed `deploy/caddy/Caddyfile`

### Fixed

**imboy（后端 / Backend）**
- `imboy/api/openapi.yaml`：redocly content warnings 17→0（commit `607d943`，2026-05-09）
  - 8 个 endpoint 补 `'4XX'` 错误响应（复用 `Envelope` schema，不新建 `ApiError`）
  - 4 个 operation descriptions + 4 个 param descriptions
  - server URL `localhost` → `127.0.0.1`（绕过 redocly no-server-example.com 规则）
  - `imboy/api/openapi.yaml`: redocly content warnings 17→0 (commit `607d943`, 2026-05-09)
  - 8 endpoints add `'4XX'` error responses (reusing `Envelope` schema)
  - 4 operation descriptions + 4 param descriptions
  - server URL `localhost` → `127.0.0.1` (bypass redocly no-server-example.com rule)
- `imboy/docs/CONVENTIONS.md` §4：移除对 `components.schemas.ErrorCode`（已不存在）的陈旧引用，统一指向 `include/error_code.hrl` 宏（2026-05-12）
  - `imboy/docs/CONVENTIONS.md` §4: removed stale reference to `components.schemas.ErrorCode`; now points to `include/error_code.hrl` macros (2026-05-12)

**imboy-admin-frontend（管理后台 / Admin Frontend）**
- TSID 类型债清零：`custom/admin-tsid-numeric-misuse` 规则 38 findings → 0（commit `f7c930c`，2026-05-09）
  - TSID type debt eliminated: `custom/admin-tsid-numeric-misuse` 38 findings → 0 (commit `f7c930c`, 2026-05-09)
- `src/lib/entityId.ts` 抽取：将分散在 3 处的 `coerceEntityId` / `coerceFeedbackId` helper 统一为单一导出函数，`fallback` 参数支持哨兵值（2026-05-12）
  - `src/lib/entityId.ts` extracted: unified 3 scattered `coerceEntityId` / `coerceFeedbackId` helpers into a single exported function with `fallback` parameter for sentinel values (2026-05-12)

---

## [1.0.0-alpha.x] - 2026-04 ~ 2026-08（公测线）

> **公测线持续交付**。以下内容在 alpha 公测期间持续落地，尚未发布 rc.1 / 1.0.0 GA。
> alpha → 1.0.0 的升级步骤见 `docs/guides/operations/upgrade-runbook.md`（草案）。

### Added

**治理与 CI/CD**
- G1 三端 CI 流水线：`.github/workflows/ci.yml`（DCO / backend / admin / app 四 job，cancel-in-progress）
- G1 发版流水线：`.github/workflows/release.yml`（tag v* 触发，产物：Erlang tarball / bun build / Flutter APK）
- G1 CodeQL 安全扫描：`.github/workflows/codeql.yml`（每周 + push，javascript-typescript security-extended）
- G4 Dependabot：`.github/dependabot.yml`（npm / pub / github-actions 三路每周自动更新 PR）
- G4 Trivy SARIF 扫描 + SBOM 生成：`.github/workflows/security.yml`
- G5 Prometheus SLO 告警规则：`deploy/prometheus/rules/imboy-alerts.yml`（13 条规则：可用性 / p99 延迟 / 错误率 / Erlang VM / PG）
- G6 `ROADMAP.md`（CE 1.0 / EE 1.x / 2.x 长期路线图）
- G6 `SUPPORT.md`（社区支持渠道 + 响应时间表）
- G6 `README.en.md`（英文镜像，与 `README.md` 同步维护）
- G7 DCO sign-off 强制校验（`timarcher/dco-action@v1`，每个 PR commit 必须含 `Signed-off-by`）
- `CONTRIBUTING.md` 贡献者协议章节（DCO 说明 + `git commit -s` 示例 + 批量补签命令）

**交付层**
- S1 `brand/` 品牌资源目录（logo / icon / tokens.json / 使用规范）
- S2 `script/seed_demo.sh` — 幂等 demo 数据灌库（5 用户 / 2 群 / 群成员）
- S3 Grafana + Prometheus 可观测性包（9-panel 总览面板 + 自动装配）
- S4 `imboy/doc/operations/upgrade-runbook.md` — 完整升级剧本（relup / cold restart / PITR 回滚）
- S5 `imboy/doc/api/openapi.yaml` — OpenAPI 3.1.0 扩充至 21 个稳定端点，含群作业 7 端点 + Group/GroupTask schema

**测试覆盖**
- 群作业（group_task）集成测试三端全覆盖：
  - Erlang CT：`imboy/test/ct/group_task_SUITE.erl`（13 用例，真实 PostgreSQL 完整生命周期）
  - Flutter 组件测试：`imboyapp/test/widget/group_task_page_test.dart`（9 用例，FakeGroupTaskService）
  - Admin Playwright E2E：`imboy-admin-frontend/tests/e2e/group-task.spec.ts`（page.route() 拦截）
- CI 三端测试步骤同步串联（CT / widget test / Playwright E2E）

### Changed
- `README.md` 增加语言切换链接 `简体中文 | English`，文档表格补充 ROADMAP / SUPPORT 入口
- `CONTRIBUTING.md` 顶部增加 DCO 贡献者协议章节

### Known gaps（延至 1.0.x）
- 生产 Sentry DSN 注入流程文档化（文档齐全，需 opt-in）
- iOS 上架流程（依赖开发者账户就位）
- 单机百万连接可复现性能白皮书
- docs-site VitePress 文档站

---

### Highlights（alpha 线核心交付）

**🔒 安全底座（Phase 1-2 CRITICAL / HIGH 共 12 步）**
- SQLCipher 加密本地数据库（客户端落地消息全加密）
- PostgreSQL 凭据轮换 + 环境变量迁移 + 权限分离（`imboy_user` / `imboy_app`）
- WAL 归档与 PITR 备份剧本（`imboy/doc/operations/deployment/BACKUP-RESTORE.md`）
- Token 过期逻辑修复（此前反转导致过期 token 可续签）
- WebSocket 消息路径速率限制 + Retry 拦截器固化专属 Dio 实例
- Flutter 全局错误捕获 + HTTP 安全响应头（HSTS / CSP / X-Frame-Options 等）
- 清理开发期测试端点开放路由（0 公网暴露）
- **P0-5 首启初始化向导**：消除默认 `admin/admin888` 硬编码，`/setup` 免鉴权向导仅允许执行一次

**🏎 稳定性与性能（Phase 3 MEDIUM 共 6 步）**
- TSID 分布式 ID 全量迁移（替换 BIGSERIAL，跨数据中心唯一 + 时间近似有序）
- `conv_seq` 游标方案 B：消息永久存储的严格顺序依据（per-conversation 单调递增，不依赖 TSID 排序）
- 热路径分页改游标分页（会话列表、消息历史、频道订阅）
- `conversation` 表 varchar→bigint 迁移（ID 类型统一）
- TimescaleDB hypertable 覆盖审计日志 / 消息时间线
- Admin 密码 MD5 预处理修复 + pgBouncer 连接池评估完成

**🔭 可观测性与可运维性（Phase 4 MEDIUM 共 6 步）**
- Sentry 集成（三端统一 DSN 通道，生产注入流程见 `imboy/doc/operations/observability.md`）
- Erlang 后端结构化日志（lager → JSON）
- CI/CD 基础流水线（lint / test / dialyze / 迁移校验）
- 生产部署文档与一键 `docker-compose.prod.yml` + Caddy 自动 TLS
- Flutter 核心流程集成测试补全（1274 通过 / 0 失败）
- WebSocket 重连稳定性测试（4 步退避 2s→5s→7s→11s 压测验证）

**🧱 架构与工程**
- 工作区架构：三端独立仓 + 共享 workspace 约束 + 根 `VERSION` 单源
- 架构门禁：`script/check_module_boundaries.sh` 在 CI 防止跨域直接依赖
- IMBoy v2 二进制帧协议（自托管 WS 帧包裹 JSON/Protobuf，向前兼容 v1 JSON）
- 管理后台统一分页规范（`DataTablePagination`，默认 `size=10`，搜索/筛选时 `page` 强制重置 1）
- 法务文本：隐私政策 7 节 + 服务条款 6 节完整正式文本（2026-01-01 生效）
- 三端统一 `LICENSE`（MulanPSL-2.0）

### Added
- 10 大功能线代码完整度全部 100%
  - 单聊 (C2C) · 群聊 (C2G) · 会话管理 · 消息提醒（FCM + APNs）
  - WebSocket / ACK · 端到端加密 (E2EE) · Tag 标签 · 收藏
  - 频道（订阅/发布/付费/统计）· 朋友圈（ACL/评论/点赞/审核）
- `deploy/docker-compose.prod.yml` + `.env.example` + `deploy/README.md` 一键生产部署包
- `script/preflight.sh` 部署前置检查（磁盘 / 内存 / 端口 / DNS / PG 扩展）
- 根 `CHANGELOG.md`（本文件）+ 根 `VERSION` + 三端统一 `LICENSE`
- **P0-5 首启初始化向导完整链路**（后端 handler + logic + 前端 SetupPage + 路由守卫 + E2E）

### Changed
- **版本号**：workspace 层当前为 `1.0.0-alpha.46`，三端历史小版本号停止独立递进
- **DB 访问**：所有数据库操作强制通过 `elib_pg` 模块（架构门禁 CI 拦截）
- **ID 规范**：客户端 ID 以 integer 传输，DB 存 BIGINT；不再使用 hashids 编码（`elib_hashids` 已于 2026-04-07 删除）
- **消息顺序**：需要严格顺序的业务统一依赖 `conv_seq` 游标，不再把 `msg_id` / `TSID` 当作全局顺序依据
- **WebSocket 字段命名**：统一使用 `to` / `from`（binary TSID 字符串），不再使用 `to_id` / `from_id`（兼容层保留）
- **管理员创建**：从 `erl remote_console` 手工写库改为 `/setup` Web 向导（P0-5）

### Removed
- `elib_hashids` 模块及其所有调用点（2026-04-07）
- 迁移文件 `00000006_user.sql` / `00000032_adm_user.sql` 中 4 处 `admin888` 默认口令注释残留
- 开发期测试端点（`/test/*`）从 `imboy_router:open/0` 白名单移除
- 根 `README.md` 中的 erl shell 建测试账号笔记（迁移到 `imboy/doc/dev/ws-repl-cheatsheet.md`）

### Fixed
- Token 过期逻辑反转（此前过期 token 仍可续签）
- Retry 拦截器泄漏裸 Dio 实例（绕过认证头）
- `get_staging_stats` SQL 语法错误
- APK SHA256 校验异常处理
- Admin 密码 MD5 预处理流程修复

### Security
- 生产凭据全部从代码库移出到 `.env` / 环境变量
- SQLCipher 加密客户端本地数据库
- PostgreSQL 最小权限账号分离（DDL 与 DML 不同账号）
- WAL 归档 + PITR 支持
- `adm.setup.completed_at` 配置标志 + `adm_user` 表存在性双重防线防止首启向导被重复触发

### Known gaps to 1.0.0 GA
- 生产 Sentry DSN 注入流程尚未完全自动化（文档齐全但需手工 opt-in）
- iOS 上架流程未启动（依赖开发者账户就位）
- 单机百万连接的可复现性能白皮书（有压测数据但未整理成第三方可复现剧本）
- Prometheus 告警规则（Grafana dashboard 已落地，告警阈值待实战打磨）
- docs-site VitePress 文档站（暂以 README + CHANGELOG + doc/ 顶着）

---

## 历史版本（pre-SKU，仅作参考）

### imboy backend
- `0.7.3` - 最后的 pre-SKU 后端版本（2026-01-20）
- `0.7.0` 之前 - 详见 `imboy/doc/changelog.md`

### imboyapp
- `0.8.0` - 最后的 pre-SKU Flutter 版本

### imboy-admin-frontend
- `0.0.0` - 未正式计版

---

[Unreleased — 1.0.0-alpha.46 公测线]: https://github.com/imboy-pub/imboy/compare/v1.0.0-alpha.26...HEAD
[1.0.0-alpha.x]: https://github.com/imboy-pub/imboy/releases/tag/v1.0.0-alpha.26
