# IMBoy 工程效能演进路线（Engineering Roadmap）

> 基于 `docs/review/code-quality-review.md` 与全量评审的头号根因 · 日期 2026-07-22
> 核心判断：头号根因是"约定驱动而非机制驱动"。工程效能演进 = **把注释约定升级为 lint/schema/CI 机制**,把巨型文件与死设施清理掉,让"正确"成为默认路径而非自觉。

---

## ENG-01 · Flutter custom_lint 门禁 【P1】
- **目标**：为四条只存在于注释/CLAUDE.md 的铁律建 lint 规则：autoDispose 显式化、裸 URL 禁令、800 行红线、token 化（颜色/间距/字号）。
- **原因**：评审头号根因 + P1-F1——autoDispose 历史两次真 bug 仅注释防御,复发面 67 个 Notifier;裸 URL/800 行/token 化全靠自觉。
- **收益**：约定变强制,类复发 bug 在 PR 阶段拦;新人无需记忆铁律。
- **风险**：低；存量违规先设 baseline 豁免,只拦新增（沿用"增量冻结"成功模式,如 design-tokens 门）。
- **影响范围**：`imboyapp` lint 配置 + CI。
- **工作量**：M。**PR 数**：2–3（规则 + baseline + CI 接线）。
- **验收**：4 类违规新增即 CI 失败;存量 baseline 递减。

## ENG-02 · Flutter DDL 单一真源 【P1】
- **目标**：以 embedded 常量为单一真源生成 .sql 副本 + CI 校验一致;无脚本降级显式失败（不再静默返回 success）。
- **原因**：评审 P1-F3——常量 + 2 个 .sql 镜像 + CLAUDE.md 四处手工同步且已矛盾（CLAUDE.md v21/代码 v23）;v23 降级无脚本却 success。
- **收益**：schema 三镜像不可能漂移;降级失败可见。
- **风险**：低。
- **影响范围**：`embedded_schema_scripts.dart`、`sqlite.dart`、迁移服务 + CI。
- **工作量**：M。**PR 数**：2。
- **验收**：常量改动自动同步副本,不一致 CI 失败;无脚本降级抛错而非 success。

## ENG-03 · 消息主链路巨型文件拆分 【P2】
- **目标**：拆 `chat_page.dart`（2234 行,规范 2.8 倍）及 12+ 个 >800 行手写文件。
- **原因**：评审 P1-Q2——巨型文件是历史 bug 密度最高区;800 行红线无 lint（由 ENG-01 补）。
- **收益**：降低认知负荷,压制 bug 密度;为 ARCH-05 运行时收敛铺路。
- **风险**：中；改核心 UI。缓解:逐 widget/职责拆分,每步真机验收,行为不变。
- **影响范围**：`imboyapp/lib` 消息主链路。
- **工作量**：L。**PR 数**：6–8。
- **验收**：消息主链路文件全部 <800 行;真机回归无退化。

## ENG-04 · 死设施清理 + ADR 补齐 【P3】
- **目标**：① 清死资产（liveRoom 四层齐全挂生产路由却零测试引用,先冻结确认无用再删）② 补 ADR 覆盖 E2EE/支付/LiveKit/MCP 等重决策（当前仅 3 条）。
- **原因**：评审 P2-8/P3-4——死设施掩盖真实架构;重决策无记录不可追溯。
- **收益**：代码面收敛;决策可追溯。
- **风险**：低（删除前先冻结确认）。
- **影响范围**：liveRoom 相关 + `docs/adr/`。
- **工作量**：M。**PR 数**：3–4。
- **验收**：liveRoom 或转正或下线;E2EE/支付/LiveKit/MCP 各有 ADR。

## ENG-05 · 后端分层门禁强化 + 破窗清理 【P2】
- **目标**：清唯一分层破窗（`adm_feedback_handler:147,189` 直调 repo）;把 `config_ds` 直连 elib_pg 与 6 个 logic→repo 跳层纳入门禁评估。
- **原因**：评审 P3-3/P2-11——`check_module_boundaries.sh` 已机制化;但 `adm_feedback_handler:147,189` 用 `feedback_repo:tablename()` 借表名,**可能属"借用型穿层"逃过脚本检测**（脚本是否捕获此形态需实测确认，不断言"全仓仅 1 破窗"）。真正的增量是**扩展脚本以捕获 tablename() 借用型绕过**。
- **收益**：分层纪律 100% 机制保证。
- **风险**：低；破窗极少。
- **影响范围**：`adm_feedback_handler`、`config_ds`、边界脚本。
- **工作量**：S。**PR 数**：1–2。
- **验收**：`check_module_boundaries` 零违规;跳层白名单显式记录。

## ENG-06 · 文档漂移批量修正 【P3】
- **目标**：修根级/模块级文档与代码的漂移：`msg_archive_enabled` 默认值（sys.config:104 为 true 但 CLAUDE.md 称 false）、imboyadmin 记为 Vue（实为 React 19.2）、`please_refresh_token` 8s 刷新链叙述、schema 版本号等。
- **原因**：评审 P3-1/P3-2 + 协议 #8——文档漂移误导 AI/新人。
- **收益**：文档可信;AI 上下文不被误导。
- **风险**：低。
- **影响范围**：各级 CLAUDE.md、docs。
- **工作量**：S。**PR 数**：2–3。
- **验收**：抽查关键事实文档与代码一致。

## ENG-07 · 监督树健壮性 【P2】
- **目标**：修 `imboy_cache:start_link` 返回 `self()` 丢弃 depcache 真实 Pid（缓存崩溃不自愈）;评估 listener 先于监督树启动的可用性窗口。
- **原因**：评审 P2-4/P2-5——两个监督树盲区。
- **收益**：缓存崩溃自愈;滚动发布无可用性窗口。
- **风险**：中；改启动时序。缓解:灰度验证启动顺序。
- **影响范围**：`imboy_cache`、`imboy_app` 启动序。
- **工作量**：M。**PR 数**：2。
- **验收**：depcache 崩溃被监督树重启;listener 在依赖就绪后接流量。

## ENG-08 · 依赖与许可治理 【P2/P3】
- **目标**：建依赖许可扫描门禁（防再引入 AGPL 类冲突）;跟踪 TS7 升级受阻项（typescript-eslint 生态就绪后一次性升）。
- **原因**：AGPL vodozemac 暴露许可盲区;TS7 升级受阻（本地 main 领先 github 9 commit 未推）。
- **收益**：许可风险 PR 阶段拦;技术栈升级不无限期搁置。
- **风险**：低。
- **影响范围**：三仓 CI + 依赖配置。
- **工作量**：S–M。**PR 数**：2–3。
- **验收**：许可扫描进 CI,AGPL/GPL 类依赖告警;TS7 升级有跟踪 issue 与就绪判据。

---

## 机制化优先级（头号根因的直接对策）

评审证明这些约定已有,只缺机制。按"复发风险 × 机制成本"排序:

| 约定 | 现状 | 机制 | 任务 | 优先级 |
|---|---|---|---|---|
| autoDispose 陷阱 | 注释,67 复发面 | custom_lint | ENG-01 | 高 |
| DDL schema 一致 | 四处手工,已矛盾 | 生成+CI | ENG-02 | 高 |
| 鉴权豁免 | 4 处平行 path | 路由声明 | ARCH-01 | 高 |
| 钱包不变量 | 注释 | 表级 CHECK | SEC-02 | 高 |
| 覆盖率目标 | 宣称 | 阈值门 | TEST-02 | 高 |
| 800 行/裸URL/token | CLAUDE.md | lint | ENG-01 | 中 |
| 分层边界 | ✅ 已机制化 | check_boundaries | ENG-05 补破窗 | 中 |
| 协议契约 | 无门禁 | diff CI | ARCH-03 | 高 |

## 汇总表

| 编号 | 任务 | 优先级 | 工作量 | PR |
|---|---|---|---|---|
| ENG-01 | Flutter custom_lint | P1 | M | 2–3 |
| ENG-02 | DDL 单一真源 | P1 | M | 2 |
| ENG-03 | 巨型文件拆分 | P2 | L | 6–8 |
| ENG-04 | 死设施清理+ADR | P3 | M | 3–4 |
| ENG-05 | 分层门禁补破窗 | P2 | S | 1–2 |
| ENG-06 | 文档漂移修正 | P3 | S | 2–3 |
| ENG-07 | 监督树健壮性 | P2 | M | 2 |
| ENG-08 | 依赖许可治理 | P2/P3 | S–M | 2–3 |
