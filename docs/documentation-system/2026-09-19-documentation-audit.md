# 文档体系审计报告（Documentation Audit Report）

> 日期：2026-09-19 · 审计人：Documentation Architect（AI 会话）
> 性质：一次性审计记录（HISTORICAL after close-out）。整改动作已当场完成的部分见 §8；未完成项转交维护者。
> 任务边界：**只改文档，不改任何代码/迁移/测试/配置。** 所有「代码与文档差异」均按「以代码为准修文档」处理或登记。

## 1. 扫描范围

- 三仓全量代码事实：`imboy`（763 个 Erlang 模块、133 个迁移、758 条路由契约）、`imboyapp`（lib/modules 11 个 DDD 模块 + store/api 45 个客户端 + SQLite schema v32）、`imboyadmin`（19 个业务模块 + 152 个单测 + 41 个 e2e spec）。
- 三仓全量 Markdown 清点：imboy 646 篇（含 guides 189 / plans 159 / planning 66 / reference 35 / compliance 32 等）、imboyapp 仓内文档树、imboyadmin 仓内文档树、imboy.wiki 12 页、deploy/ 与 scripts/ 文档。
- 交叉核对：路由 ↔ 客户端调用、迁移 ↔ 概念文档、术语在三仓代码中的出现形态。

## 2. 发现的主要问题（按严重度）

| # | 问题 | 证据 | 处理 |
|---|---|---|---|
| P1 | **CONVENTIONS.md 三处与代码相反**：①错误码规定字符串码并「禁止整数」——代码（`elib_response`、`error_code.hrl`、三端解析）全线整数；②WS action 规定点分式 `msg.send`——实际 `message_revoke` 等 snake_case；③「禁止内部 ID 进 URL 路径」——现行 API 全量 TSID 路径 | §4/§6/§1 | ✅ 当场修正（v1.0→v1.1） |
| P2 | **architecture/overview.md 同段自相矛盾**：plugin registry 兼容层「已移除」与「仍保留」两个版本并存；代码事实是 `all/0`/`get/1` 仍在（Deprecated wrapper） | overview.md 迁移状态节 vs `src/lib/imboy_plugin_registry.erl` | ✅ 去重并保留正确结论 + 勘误注 |
| P3 | **版本锚点四处漂移**：ROADMAP/SECURITY=alpha.46、CLAUDE.md=alpha.26、ops/support-matrix=alpha.16，实际 VERSION=alpha.77 | 各文件头部 | ✅ 全部对齐 alpha.77（support-matrix 加同步提醒） |
| P4 | **Agent Runtime V3.1 计划状态头滞后**：标 `READY_NOT_EXECUTED`，但迁移 133/134 与 `src/features/agent/` 已并入 main（CHANGELOG alpha.77） | 计划头 vs CHANGELOG | ✅ 状态勘误横幅 |
| P5 | **客服计划链七连未收尾**：v2 与 system-design 两环缺被取代标注 | plans/ 链 | ✅ 补 HISTORICAL 横幅（v3/v4 原有 SUPERSEDED 保持） |
| P6 | **目录分裂**：`ops/` vs `operations/` vs `guides/operations/` 三处运维位；`plans/` vs `planning/` 双计划目录 | 目录树 | ✅ `operations/` 并入 `runbooks/`（唯一文件归位+修 2 处入链）；`ops/support-matrix.md` 因被 `scripts/check_release_consistency.sh` 引用**原地保留**；plans/planning 之分在 docs/README.md 明确口径（2026-09-20 瘦身：两目录已整体移除，口径同步撤销） |
| P7 | **死链**：CONVENTIONS 引 `.claude/plans/quality-loop.md`、`CONVENTIONS_EXCEPTIONS.md`（均不存在）；SECURITY 引 `doc/`；ROADMAP 引 `script/`、`doc/api/` | grep | ✅ 主体轮已修（CONVENTIONS/SECURITY/ROADMAP）；⚠️ 续轮复核发现首轮 P7 结论「全部修正」为过度声明——api/{,codegen/,proto/}README 的 quality-loop 死链与 reference 两份 WS 文档的 imboy-frame-protocol 死链当时未处理，**已在本续轮补齐**（指向 CONVENTIONS / api/proto 真源）；overview.md 链接显示文字过期亦同轮修正 |
| P8 | **imboyapp 文档滞后**：SQLite schema 写 v30，实际 v32 | `lib/service/sqlite.dart` | ✅ 修正 |
| P9 | imboyadmin README 代理描述不全（漏 `/api/v1`、`/brand`） | `vite.config.ts` | ✅ 修正 |
| P10 | ~~imboyadmin 法务材料不一致~~ **误判（2026-09-19 续轮修正）**：`docs/legal/MulanPSL-2.0.txt` 是**有意保留的历史授权文本**——imboyadmin README.md 明确声明「2026-09-14 之前发布的版本按木兰第 2 版授权……该文件作为历史授权文本保留」（换证不追溯的法务安排）。无动作，文件维持原状 | README.md:86 声明 | ✅ 复核撤销（非问题） |
| P11 | E2EE 文档三体系并存（reference 规范 / guides/e2ee/standard / guides/e2ee/v2 31 篇 ADR，其中 9 篇已被取代仍留原位）+ 两份「顶级行业标准」定义 | guides/e2ee/ | ⚠️ 各篇头部已有 supersedes 链，未做大搬家（避免破坏证据树互链）；在 concepts/e2ee.md 明确现行入口与阅读规则 |
| P12 | Wiki 五页过期（Quick-Start/FAQ/Upgrade-Guide/System-Requirements/Admin-Console：引用商务版前 prod.yml 时代口径、Caddy 残留、OTP 版本低报） | imboy.wiki vs deploy/README | ⚠️ wiki 属独立仓推送机制（push-all.sh 三平台），本轮未动；**建议单独小任务刷新** |
| P13 | 根 ROADMAP 的 GA 目标日期（2026 Q2）已过期、docs/roadmap/ 8 篇停在 alpha.15 基线 | ROADMAP.md | ⚠️ 路线图内容重写超出本轮「以代码对齐文档」范围，仅修事实错误；**转交产品决策** |
| P14 | ~~scripts/README 迁移列表停在 00000112~~ **误判（2026-09-19 续轮修正）**：该段陈述的是引入蓝绿机制版本的**既定评审结论**（64/108/109/111/112），不是滚动清单；113 之后迁移是否纳入 `DEPLOY_EXPAND_MIGRATIONS` 由每次发布的兼容性评审决定（README 已写明规则：只放已评审的 expand SQL）。文档不应代运维拍板追加，无动作 | scripts/README.md:32-42、`.env.deploy` 逐机配置 | ✅ 复核撤销（非问题） |

## 3. 文档结构调整

新增（imboy 仓）：

```
docs/
├── glossary.md                              ← 新：全项目术语权威（三端共享）
├── concepts/                                ← 新：核心业务概念层（8 篇）
│   ├── README.md（概念地图 + 四态标注约定）
│   ├── accounts-and-actors.md（账号与主体）
│   ├── collaboration-hierarchy.md（组织/工作区/项目/群组/频道）
│   ├── messaging-model.md（消息模型）
│   ├── agent.md（智能体：Grant/Run/Hirð/HITL）
│   ├── customer-service.md（客服域）
│   ├── enterprise-business.md（企业业务域）
│   └── e2ee.md（端到端加密概念入口）
├── api-contracts/
│   └── three-platform-alignment.md          ← 新：三端 API 对齐 + 契约漂移登记簿
└── documentation-system/
    └── 2026-09-19-documentation-audit.md    ← 本报告
```

调整：`docs/README.md` 重写为完整索引（概念/术语/对齐入口 + plans/planning 口径 + 四态状态模型）；`docs/operations/` 撤销（文件归位 runbooks/）。

**不动**的既有分类（合理保留）：Diátaxis 主体（guides/reference/explanation/tutorials）、archive 只进不出规则、ADR 目录制、evidence 证据树。

## 4. 术语统一（关键裁定）

权威落点：`docs/glossary.md`（11 节，含易混清单）。关键裁定：

| 裁定 | 依据 |
|---|---|
| 智能体（Agent）为正式名；「AI 助手」仅指客户端 UI 显示名；「代理」弃用 | CHANGELOG 现行用法 + UI 事实 |
| Bot ≠ 智能体（两套体系，account_type 3 vs 1） | 表/模块/路由三处证据 |
| Hirð（文档）/ hird（代码 ASCII）双形态合法；`hirdir` 不存在 | `imboy_hird`、`runtime_type` 枚举 |
| 委派（Delegation）= 概念名（Grant+事件谱系），不是代码实体 | `agent_grant_domain.erl` 注释、ADR-AG31-003 |
| 企业业务（EB，功能域）≠ 商务版（Business Edition，分发版次），英文必须写全称 | 路由 `/enterprise` vs deploy 双轨 |
| 授权（Grant）≠ 商业授权（License）：技术语境默认 Grant，商业语境写 License | 三实体同名后缀（Agent/MCP Client/Bot OAuth Grant）+ `imboy_license` 无数据表 |
| 频道（Channel）≠ 支持渠道（Support Channel）；群组（Group）实体名弃「社群」；会话三义分立（个人/企业/客服） | 术语混用高发区 |
| 代码前缀约定入表：`cs_`（表 `customer_service_`）、`eb_`（表 `enterprise_`）、`adm_`、`elib_`、`imboy_` | 三仓 grep 量级验证 |

## 5. 重复与过期文档处理原则

- **原地标注优先于搬移**：计划链互链密集（evidence 树），搬移会制造断链；SUPERSEDED/HISTORICAL 横幅 + 继任指针已达成治理目标。
- **搬移仅一例**：`operations/agent-hub-local-golden-flow.md` → `runbooks/`（入链 2 处已同步）。
- **不删除任何文档**：本轮未发现「完全重复且无历史价值」到可删程度的文件。

## 6. 代码与文档差异登记（Contract Drift，未改代码）

详见 `docs/api-contracts/three-platform-alignment.md` §4 登记簿，要点：Admin 内置角色 '4'-'6' 前端无名称映射（UNKNOWN 语义缺失）；WS action 注册表与 App 常量表两处人工同步；Admin 法务残留；App 对 Bot 类型无独立建模。

## 7. 仍存在的 UNKNOWN

| 项 | 状态 |
|---|---|
| Hirð 完整运行时（hird_actor/hird_tool_dispatch 等）的交付与版本来源 | 不在本仓（deps 无 hird），外部组件 |
| Admin 内置角色 4/5/6 的正式语义 | 前端无映射，需后端 adm_role 种子确认 |
| ~~deploy README「3 个抓取 job / 8 条告警规则」vs 实际~~ | ✅ 续轮已修复（实际为 4 job / 33 条规则 14 组；首轮「13 规则」亦为低估），UNKNOWN 关闭 |
| compose fallback tag `alpha.71` 与 README 叙述 `rc.1` 并存 | 发布口径问题，转交维护者 |

## 8. 最终状态

- 三仓变更全部为 Markdown（验收见 git status/diff：CODE CHANGED=NO / MIGRATION=NO / TEST=NO / CONFIG=NO / DOCUMENTATION=YES）。
- imboy：新增 11 篇（glossary + concepts×8 + alignment + 本报告）、重写 1（docs/README）、修正 10（CONVENTIONS、overview、ROADMAP、SECURITY、CLAUDE.md、support-matrix、3 份计划标注）、归位 1（runbook 移动）。
- imboyapp：docs/README.md 事实修正 + 术语链接。
- imboyadmin：README 代理描述修正 + CLAUDE.md 头部刷新与术语链接。
- ~~转交维护者 4 项（P10 法务文件、P12 wiki、P13 路线图内容、P14 scripts README 迁移列表）~~ → 见 §9 续轮。

## 9. 续轮（2026-09-19 同日）：转交项处理结果

| 项 | 结果 |
|---|---|
| P10 法务文件 | **复核撤销**（误判，见 §2 P10 修正行）：README 已声明历史授权文本保留，双许可并存是换证不追溯的有意安排 |
| P12 Wiki 六页 | ✅ 已修（imboy.wiki 本地，**未推送**——对外发布需用户确认）：Quick-Start/FAQ/Upgrade-Guide 生产路径从商务版 `docker-compose.prod.yml` 改为开源入口 `install.sh --edition community` / `docker-compose.community.yml`；System-Requirements 去 Caddy 残留、资源口径对齐（最低 4/10 建议 8/20）、注明生产镜像 OTP 29、PG18 自维护镜像扩展；Admin-Console 补 `/setup` 首启向导（仅一次，已对照 `adm_setup_handler` 核实）、功能范围对齐当前域（AI 助手/Bot/MCP/组织/客服/企业业务）、认证现状（Cookie+CSRF）；Deployment-Kubernetes 写明 Helm 实验性/副本固定 1/不在交付范围 |
| P13 路线图 | ✅ 折中处理：ROADMAP.md 加状态基线注记（已完成清单已对齐 alpha.77；计划区日期/勾选未重排，以 CHANGELOG/RELEASES 为准；重排属产品决策不代拟）。内容重排仍留产品侧 |
| P14 scripts README | **复核撤销**（误判，见 §2 P14 修正行）：该清单是历史评审结论而非滚动清单，规则已覆盖新增迁移的决策方式 |
| deploy/README 观测数字（§7 遗留） | ✅ 已修：抓取 job 3→4；告警表从「8 条」（实际只列 13 条）改为完整 **33 条 / 14 组**全表（阈值摘要提取自 yml 真源）；grafana/README 补社区版 `--profile monitoring` 口径；删除 `priv/README.md`（erlang.mk 模板残留，零引用零信息） |
| 死链补齐（P7 首轮遗漏） | ✅ 已修：api/ 三份 README 与 reference 两份 WS 文档共 6 处死链/过期引用；audit 自身过度声明已在 §2 P7 修正 |
| ROADMAP 计划区事实重排（§9 唯一开放项） | ✅ 事实层完成（2026-09-19 第三轮）：12 个条目按代码/CI/文档证据核对——8 项打勾附验证依据（DCO CI/Loki/集群文档/只读副本/FTS 搜索/API Sandbox/dev_setup/README.en），SDK 条目标注 2026-09-09 暂不做决策，Bot OAuth Grant 标注「表已建无 API」；**GA/中期目标日期仍未代拟**（待产品决策，条目上注「需人工确认」） |
| P11 补充核实 | E2EE「两份顶级标准定义」实为**上位决策（v2/31 ADR）→ 标准操作化（standard/top-tier-standard-2026）**配对，互链清晰职责分明，非重复定义——不干预，疑云解除 |
| 分层 CLAUDE.md 模块数 | ✅ 已修：api 54→89、logic 76→149、ds 77→105、repo 72→123、lib 61→116、adm 27→39（原计数截至 2026-06） |
| 全仓内链死链清零（第四轮） | ✅ 四仓全量扫描：imboy 活跃区 16 条报告项中 6 条为 GitHub 相对导航误报（../../issues）、5 条为 %20 编码误报（目标存在）；**真断链 4 处已修**（compliance Gap Matrix 少词文件名×1、clustering/read-replica 指向不存在的 config/sys.config→example×2）+ 修复首轮自查引入的 api/ 两处相对路径错（../docs→../../docs）。app 仓 10 条全为正则误捕代码片段；admin/wiki 零断链。archive/plans/.claude 区不扫不修（历史快照断链属正常） |
| imboyapp/CLAUDE.md 事实核查（第四轮） | ✅ 已修：schema v31→v32（两处，v32=group.user_id_sum 死列退役对齐服务端迁移 79）；模块索引补 organization/enterprise/customer_service 三个 DDD 域模块 |
| deploy/CLAUDE.md | ✅ 已重写：目录树补 install.sh/社区版编排/5 个 overlay/alertmanager/uptrace/cron；资源口径对齐 deploy/README（4/10→8/20）；命令改社区版推荐路径；保留 Helm 单副本警示 |
| 09-20 复核两轮（机制面+事实面） | ✅ 全过：交付 13 笔逐笔核验仅动文档；错误码 #8 修复与 hrl 逐字对齐（705/706 后端本无默认文案、仅 5433 有=正确形态）；docs-site 实跑构建 10.6s 通过（上轮「未验证项」闭合）；六条承重断言代码级核验全过（account_type 0-3、e2ee_room_key 双路、响应信封、CSRF/widget token、**App schema v32 真源=`sqlite.dart:48` 私有常量 `_dbVersion`**、迁移 133/134=agent_grant/agent_run_foundation）；全仓链接清扫抓到历史死链 `.github/pull_request_template.md`（`.github/` 下写 `docs/` 相对路径不可解析）→ 修 `1261c7a7` |
| 09-20 代码修复轮（用户授权「修复当前会话发现的代码问题」，两轮） | ✅ 三项闭环+一项新登记（漂移登记簿为准）：**#2 已修**（imboyadmin `cf9ad7b`；翻案=内置角色 1-6 的代码级定义一直在 `adm_index_handler:role_acl/1`，本审计 09-19 复核「无代码级定义」系漏检）；**#7 收口**（imboy `be11a1d3`+`00f056bb`：上行 registry/下行 S2C 16 条/App `C2SAction` 单向比对三面入契约物，负向注入实测红门）；**#8 勘误**（跨仓 CI 门 `contract.yml` P3-C1 与 pre-push 兜底早已存在，本审计「未见门禁强制」系漏检，漂移存活仅因本地积压未推送）；**新登记 #9**（App WS `message_reaction` 上行死路、生效通道 HTTP，删留待拍板）；登记簿 #6 处置栏同步撤销前旧判断 |

仍开放的转交项（4，均为决策类）：①ROADMAP 的 GA/中期**目标日期**重排（产品决策；完成状态核对已完成）；②#9 App WS reaction 死通道去留（产品/架构二选一：删 App 路径 or 服务端注册）；③imboyapp analyzer ratchet 基线重采（13 条既有欠账，非文档/修复轮引入，git stash A/B 对比定性）；④四仓本地提交推送（用户授权后执行）。§7 表中「deploy README 3 job/8 规则」条目已在续轮修复，该 UNKNOWN 关闭。
