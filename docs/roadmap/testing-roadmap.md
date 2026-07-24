# IMBoy 测试演进路线（Testing Roadmap）

> 基于 `docs/archive/review/testing-review.md` · 渐进升级 · 日期 2026-07-22
> 核心判断：ratchet 框架已搭好（xref=0 是成功样板），但绝大多数门是软门（continue-on-error / `test -f lcov.info` / E2E 零进 CI）。演进 = **把软门逐个收紧成硬门**,并补最缺的契约测试与真边界测试。

---

## TEST-01 · 后端 full-eunit + dialyzer 收紧为硬门 【P1】
- **目标**：按 `backend-ci.yml` 注释里的自定计划,把全量 eunit 与 dialyzer 从 continue-on-error 收紧为阻塞门。
- **原因**：评审 P1-T1——目标覆盖率全是无据宣称,全量 eunit 仍非阻塞。
- **收益**：回归自动拦截;dialyzer 类型错误不再漏过。
- **风险**：中；需先清存量失败测试才能收紧。缓解:先跑一轮,隔离/修复现存红项,再翻硬门。
- **影响范围**：`imboy/.github/workflows/backend-ci.yml`、存量红测试。
- **工作量**：M。**PR 数**：2–3（清红项 + 翻硬门 + dialyzer）。
- **验收**：full-eunit 与 dialyzer 为必过门;新 PR 红即拦。

## TEST-02 · 三仓覆盖率阈值门 【P1】
- **目标**：三仓补真实覆盖率阈值（当前仅 `test -f lcov.info` 存在性检查）。
- **原因**：评审 P1-T1——书面目标 Repo80/Logic70/Handler60/整体65 不可验证。
- **收益**：覆盖率退化可拦;目标从宣称变可度量。
- **风险**：低；先设当前实测值为地板,ratchet 只升不降。
- **影响范围**：三仓 CI + Makefile（加 cover）。
- **工作量**：M。**PR 数**：3（每仓一）。
- **验收**：覆盖率低于阈值 CI 失败;阈值只升不降。

## TEST-03 · admin Playwright E2E 进 CI 【P1】
- **目标**：把 9 个已存在的 Playwright spec 接入 CI（当前所有 workflow 零引用）。
- **原因**：评审 P1-T2——E2E 纯手动等于无回归。
- **收益**：前端关键流自动回归。
- **风险**：低；spec 已有,只需接线 + 稳定化。
- **影响范围**：`imboyadmin/.github/workflows`。
- **工作量**：S–M。**PR 数**：1–2。
- **验收**：E2E 在 CI 跑;flaky 隔离机制就位。

## TEST-04 · 协议契约测试（真边界,反 mock 反模式）【P1】
- **目标**：为"非快乐路径 × v2 编码"矩阵补真 PG/真帧测试;SDK 端到端冒烟（并入 ARCH-04）。
- **原因**：评审 P1-T3——mock 协议/存储边界使 5 个真生产 bug（离线撤回必崩等）404 单测无一发现,仅真 PG 的 CT 抓出;协议漂移全在错误路径。
- **收益**：契约错误在 CI 拦;错误路径不再裸奔。
- **风险**：低。
- **影响范围**：`imboy/test`（CT）、`imboy-sdk-js` E2E。
- **工作量**：M–L。**PR 数**：3–4。
- **验收**：C2S ACK/C2G 错误/撤回离线等错误路径有真边界测试;SDK 登录→握手→收发→确认 E2E 绿。

## TEST-05 · 修复坏死工作流 + Flutter integration 【P1，含 CRITICAL 排查】
> **2026-07-22 升级**：`integration_test.yml` 配 `paths: imboyapp/** + imboy/**` 且 `working-directory: imboyapp`（:6-7,35,63）——在 umbrella（非 git）工作区下要么永不触发、要么路径多嵌一层，**该测试基础设施根本未运行**。故从"S 工作量小修"升级为 **CRITICAL 排查项**：需先确认此 workflow 是否曾运行过一次。注意 `ci.yml` 另有在跑的 integration job（见 testing-strategy 更正），故 `integration_test.yml` 可能是冗余的坏死副本。
- **目标**：排查 `integration_test.yml` 是否从未触发（paths/working-directory 在 umbrella 下失效）;决定修复接线或删除冗余副本;补进程隔离 runner 的 CI 接线。
- **原因**：评审 P1-T4 + 本次核实——工作流配置在 umbrella 下永不/错误触发，测试基础设施未运行。
- **收益**：厘清 integration 真实 CI 状态，消除"以为在跑其实没跑"的假象。
- **风险**：中（涉 CI 基础设施真实性判断）。
- **影响范围**：`imboyapp/.github/workflows/integration_test.yml`（对照 `ci.yml` 的 integration job）。
- **工作量**：M（排查为主）。**PR 数**：1–2。
- **验收**：确认 integration_test.yml 历史触发情况;修复或删除;integration 在 CI 真实可见地绿。

## TEST-06 · 死测试清理 【P2】
- **目标**：清 `dead-tests-census.md` B 类 7 个死测试文件（含全部 3 个性能测试）。
- **原因**：评审 P3-5——死测试掩盖真实覆盖。
- **收益**：测试信号真实;CI 时间不浪费在死测试。
- **风险**：低；census 已定位。
- **影响范围**：`imboy/test`。
- **工作量**：S。**PR 数**：1。
- **验收**：census B 类清零;性能测试或转正或删。

## TEST-07 · 契约门禁作为回归基础设施 【P1→持续】
- **目标**：把 ARCH-03（proto/OpenAPI/ws_url diff）作为测试基础设施常态化维护。
- **原因**：契约漂移是三端不一致的根因,需持续门禁而非一次性修。
- **收益**：契约一致性长期保持。
- **风险**：低。
- **影响范围**：三仓 CI。
- **工作量**：并入 ARCH-03。**PR 数**：见 ARCH-03。
- **验收**：proto/OpenAPI/ws_url 三 diff 常驻 CI。

---

## 测试成熟度目标演进

| 维度 | 现状 | M2(GA) 目标 | M3 目标 |
|---|---|---|---|
| 后端 full-eunit | continue-on-error | 硬门 | 硬门 + 覆盖率阈值 |
| 覆盖率度量 | 无（仅文件存在检查）| 三仓阈值门（地板=实测）| ratchet 递增 |
| admin E2E | 零进 CI | 进 CI | 关键流全覆盖 |
| 协议契约测试 | mock 掩盖真 bug | 真边界矩阵 | 契约门禁常驻 |
| SDK | 零集成验证 | E2E 冒烟门禁 | — |
| Flutter integration | 坏死工作流 | 修复并跑 | 真机 CI |

## 汇总表

| 编号 | 任务 | 优先级 | 工作量 | PR |
|---|---|---|---|---|
| TEST-01 | full-eunit+dialyzer 硬门 | P1 | M | 2–3 |
| TEST-02 | 三仓覆盖率门 | P1 | M | 3 |
| TEST-03 | admin E2E 进 CI | P1 | S–M | 1–2 |
| TEST-04 | 协议契约真边界测试 | P1 | M–L | 3–4 |
| TEST-05 | 修坏死工作流 | P1 | S | 1 |
| TEST-06 | 死测试清理 | P2 | S | 1 |
| TEST-07 | 契约门禁常驻 | P1 | 并入ARCH-03 | — |
