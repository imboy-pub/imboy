# 企业与 Workspace 全功能并行验收 — 最终报告 / Final Report

Run: `ews-20261004T065500Z` ｜ 计划 SHA: `afefa4d9…a06679e9`（校验一致）
日期：2026-10-04 ｜ 协调者：企业线 ZCODE（A0）

## 1. 结论状态（不推导声明）

| 维度 | 状态 |
|---|---|
| BACKEND_LOCAL（合同+域门+迁移+安全+E2EE） | `LOCAL_CANDIDATE_PASS`（全量 eunit 2F=main 既有缺陷 D1/D2，见 §5；企业域专项门全绿） |
| BACKEND_FULL_EUNIT_RC0 | `FAIL(2)` — 10889P/2F/0skip（R6 终局），两个失败机械证明为 main 既有，缺陷卡已交付 |
| ADMIN_BROWSER_REAL（真实浏览器层） | `PASS` — 32/32（nine-leaves 10 + int01 5 + writeops 17） |
| INTERNAL_CONTRACT | `PASS` — 28 paths/42 ops/0 ref 三方一致；18 scope 四方逐字一致 |
| APP_DEVICE（Android/macOS） | `BLOCKED_NO_HANDOFF` — 案例清单已交付全产品线 owner，未收到设备/路径交接 |
| OA/SSO 本地边界 | `PASS`（oa_sso 53/53 + context_binding + expiry）；客户 OA 真实联调 `BLOCKED_EXTERNAL`（保留未删） |
| EXTERNAL / PRODUCTION / RELEASE | `NOT_EXECUTED` / `NOT_EXECUTED` / `NO_GO` |

不推导声明：本报告不表示生产部署、上线、用户视觉满意或外部联调完成。

## 2. 三仓候选

| 仓 | base | run 分支提交 | 状态 |
|---|---|---|---|
| imboy | `0be001e3` | `fd53e9b3`(契约重导出) `1b8b8c0d`(EB审计计数同步) | worktree `/private/tmp/imboy-ews-be`，干净 |
| imboyadmin | `c9e73ad` | `b89141e1` `a132da2` `2750da6`（3 处 spec 数据锚参数化） | worktree `/private/tmp/imboy-ews-admin`，干净 |
| imboyapp | `d17bb020` | —（只读，全产品线持有） | 未动 |

全部本地提交，身份 `leeyi <leeyisoft@qq.com>`，零 push。

## 3. 修复清单（全部为验收阻断的最小修复）

1. **fd53e9b3** `.contract/api_contract.json` 重导出 — main 既有漂移：`c38adf79`（附件下载网关企业访问复查，恰属 EWS-09）新增 `/api/v1/attachment/content` 路由未同步契约产物；重导出后 Contract Gate 全绿（835 endpoints，含 admin/flutter 完整比对）。
2. **1b8b8c0d** `enterprise_business_db_it.sh` 冻结语句审计计数同步 — purge-intent 两阶段提交（`2855d749`）与孤儿资产清理（`f11bb7a0`）使语句集 26→30、SKIP LOCKED 1→2（bad 全 0，形态全部合规）；期望值同步后 153 PASS/0 FAIL。
3. **b89141e1 / a132da2 / 2750da6** Admin e2e spec 数据锚参数化（`ADM03_UNIQUE_ORG_NAME`、`ENTINT01_ORG_NAME_RE/ORG2_NAME_RE/MEMBER_RE`、`ENTW_PG_DB/ORACLE_LOG`）— 原 spec 硬编码历史 run 的种子组织名/库名，无法在 run 级隔离栈复用；参数化后默认值不变，行为零改动。

## 4. 已执行门与证据索引（全部绑定上述候选）

| 门 | 结果 | 证据（RUN_ROOT=evidence/…） |
|---|---|---|
| CON-01 flatten | PASS 28p/42ops/0ref | CON-01/flatten.log |
| CON-02 scope 四方 | PASS 18 逐字一致（DB CHECK 经 144→160→161 演化） | inherited-requirements + 比对记录 |
| CONTRACT-GATE | PASS 835 endpoints + flutter C2S 9⊆8(豁免) | backend/contract-check2.log |
| MIG-01 | PASS empty-up 164/240表 + db_it up/down/up + hold/purge 负例 153/0 | MIG-01/ |
| ORG-01 | PASS 12/12 授权门矩阵/搜索白名单/N+1/keyset/跨org | ORG-01/eunit.log |
| EUNIT 域批 | ~60 模块绿（internal_pg 40/40、grant 10/10、OA 53/53、eb e2e a01–a06、eb-infra 87 测试、workspace 全域、群/频道域） | eunit/batch1+3/（34+68 日志+hash） |
| EB-01 db_it | PASS 153/0（hold 优先级/受限 purge/跨 Org 拒绝/consent CHECK/tenancy 28 语句零 OFFSET） | MIG-01/eb-db-it.log |
| E2EE-LOCAL | PASS 497/497（38 模块 S2–S21）+ 服务端零密码学 | E2EE-LOCAL/suite.log |
| SECURITY-GATE | 组件级 PASS（三脚本单独全绿；make 包装触发 macOS perl env-长度 dyld 崩溃 138/6——本机工具链问题，附日志） | E2EE-LOCAL/ |
| ADM-02 | PASS 单测 2386/0/222文件 + lint + typecheck + build | ADM-02/ |
| ADM-03 L0 mock | 13 pass / 7 声明性 skip（dist-widget/后端可达性条件） | ADM-03-L0/ |
| ADM-03 real | PASS 32/32 | ADM-03/（nine-leaves/ent-int01/writeops + db-oracle.log） |
| 旧证据台账 | PASS 完整性（30父/42子，旧 SHA 绑定——仅 N4 资格输入） | input-manifest 记录 |

安全矩阵（SEC-01 行级）：internal authorization/audit http_checks（零 Grant 拒绝/撤权/跨域/IDOR/personal vacuum/信封精确断言）+ writeops W14 权限矩阵/W15 撤权缓存失效/W16 跨租户零副作用/W17 匿名 401 —— 真实 HTTP + UI + DB 三重 oracle。db_it 另含 hold/purge/consent/FK/tenancy 数据库级负例。

**本 run 过程中实证的数据库级不变量**（种子工程副产物，作为 EWS-03/04 佐证）：
- `organization_member` 恰一 active owner 且与 `organization.owner_id` 一致（提交期校验）
- `group_member ⊆ workspace_member`（active）——双向保护（插入与移除两侧）
- `uq_organization_department_org_active_name` 部门名组织内唯一

## 5. 全量 eunit 终局（R5→R6，环境三重根因修复后）

- R1–R4 作废（环境污染，见 recovery.jsonl）：heart 自愈节点占 20800 / vite 占 20706 / eunit 默认 TEST_HTTP_PORT=19800 被主仓 foreign 节点 `imboy_local` wildcard 持有 → 统一排除（节点先杀 heart、浏览器门后停 vite、eunit 改 29800）。
- **R5（干净环境）**：10548 P / 5 F / 0 skip，0 eaddrinuse。5 个失败仲裁：4×infra（marker 库 provisioning 遭遇 **PG 实例磁盘耗尽崩溃**——170 个历史 inttest 残留库累积；实例自愈后 3 个单跑复验全绿，1 个顺序依赖确认）；1×main 真缺陷 D1。
- **R6（最终）**：**10889 P / 2 F / 0 skipped**。R5 的 4 个 infra 失败全部转绿（归因闭合）。
- 2 个失败均为 **main 既有缺陷**（机械证明：本候选 `git diff 0be001e3 -- src/ test/` = 0 行，行为与 main 逐字节一致；不豁免、不改标，按缺陷卡交付）：
  - **EWS-BE-D1（P2）** `olm_identity_logic.erl:350` 调用未注册的 `trust_audit` TSID 生成器（注册真源 `imboy_app:tsid_generator_names()` 缺项；catalog 有、registration 无）。
  - **EWS-BE-D2（P1，企业域 INT-14）** OA SSO code `ck_eosc_consumed_after_created` 跨双时钟竞态：`issue_tx` 用 VM 时钟写 `created_at`，`consume_locked`（`baecf5ae` 引入）用 DB `clock_timestamp()`；应用时钟领先毫秒级即 23514 崩溃吞掉 `{error, redirect_mismatch}` 错误合同。**SQL 硬复现已归档**（BEGIN...ROLLBACK 内 created_at=clock_timestamp()+20ms → UPDATE → 违约逐字复现）。本机时钟方向翻转完整解释 R5 绿/R6 红"非确定性"。

结论：全量 eunit RC0 未达成（2F 均为 main 既有、非本 run 引入）；企业域全部专项门绿。

## 6. App 线交接（给全产品 owner）

- `app-case-manifest.md`：16 旅程点（含测试锚点与平台矩阵）+ 缺陷卡 D1（OrganizationEntryCard 无宿主引用——ContactPage 无企业入口，需 owner 裁决替代决定以关闭 V2.1 §14.1 P0 口径）/ D2（auto_test specs 企业端到端旅程缺口）/ D3（macOS OA 入口空=预期裁剪确认）。
- 未申请设备租约；App 源码零修改；共享路径（app_router/i18n/Token）未动。

## 7. 剩余阻断与解锁条件（NEXT_UNLOCK）

| 项 | 状态 | 解锁 |
|---|---|---|
| APP_DEVICE 两轮真机/macOS | BLOCKED_NO_HANDOFF | 全产品线正式 handoff（路径/设备时间窗） |
| 客户 OA/SSO/Webhook 真实联调 | BLOCKED_EXTERNAL | 用户单独授权（合同 §1 明确保留） |
| GET /api/adm/passport/login 模板 500 | 登记非阻断 | login.html 从未入仓（历史缺口）；React /login 不受影响——所有真实浏览器门均经真实登录链通过 |
| 用户视觉评审 | USER_VISUAL_ACCEPTANCE_PENDING | 人工评审 |

## 8. 资源与清理

- 本 run 拥有：两 worktree（run/ews-acceptance）、PG database `imboy_ews_20261004T0655Z`、节点 `ews_be@127.0.0.1`、relay 26543、admin dev 20706/8906、seed/日志 /tmp/ews_seed/。
- 未触碰：15432 foreign relay、4323 runtime 库其余 database、设备、其他线 run 目录。
- worktree 分支按用户工作流偏好（分支→提交→合并 main→删分支）待用户确认后合并；RUN_ROOT 证据全部落仓（docs/design 被 git 跟踪）。

## 9. 违禁确认

零 push、零部署、零生产迁移、零真实客户数据、零第三方通知、零真实支付、保留区文件零修改。
