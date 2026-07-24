# 工程质量总览（Engineering Overview）

> 聚焦工程能力(非业务功能)· 描述现状 + 增量改进 · 日期 2026-07-22
> 本目录是既有评审的工程视角延伸,不重复内容。深度分析见 `docs/archive/review/`,演进排期见 `docs/roadmap/`。
> 本文是索引与跨主题小结;各主题详见同目录专项。

---

## 范围与定位

| 已有文档 | 本目录关系 |
|---|---|
| `docs/archive/review/*` | 深度评审(架构/安全/性能/协议/DB/测试),含 P0-P3 台账 |
| `docs/roadmap/*` | 2 年演进排期,任务编号 ARCH/PERF/SEC/TEST/ENG |
| `docs/reference/engineering/*`（本目录）| 工程基建现状描述:结构/依赖/配置/日志/可观测/发布/Docker/CI/债务 |

本目录**只做工程质量的现状记录与增量改进建议**,不重设计架构、不提大重构。重复处一律引用而非重写。

---

## 工程质量一句话画像

> 骨架成熟、工装齐全、基建到位;短板集中在"**可观测与日志的覆盖密度**"和"**门禁的软硬程度**"——工具都有,但用得不够满、拦得不够严。

---

## 跨主题工程小结

| 主题 | 现状评级 | 一句话 | 专项 |
|---|---|---|---|
| 项目结构 | 良好 | 后端四层清晰(仅 1 破窗);Flutter 三套运行时并存是主要杂音 | 见 `docs/archive/review/code-quality-review.md` + 本目录 technical-debt |
| 依赖管理 | 中等偏上 | 显式 pin + SBOM;生产 profile 混入调试工具、Flutter 多个同类库 | dependency-notes |
| 配置布局 | 良好 | 三层配置 + IMBOY_* 优先 + 生产 fail-fast;少量默认值与文档漂移 | configuration-notes |
| 日志一致性 | **偏弱** | `elib_log` 封装存在但全后端仅 ~18 文件调用,覆盖稀疏 | logging-notes |
| 错误处理 | 中等 | 分层错误码规范;Flutter 20 处静默吞错、epgsql 吞错返空 | technical-debt（引用 review）|
| 可观测性 | 中等 | Prometheus/Grafana/Loki/Alertmanager 齐;埋点与追踪偏薄 | observability-notes |
| CI 流程 | 中等偏上 | ratchet 框架好;多数门 continue-on-error 软门 | ci-notes |
| 发布流程 | 良好 | 蓝绿 + preflight + 冒烟;版本双源需同步 | release-notes |
| Docker 布局 | 良好 | 多阶段自包含,Dockerfile 文档详尽 | docker-notes |
| 可维护性 | 中等 | 巨型文件(chat_page 2234/imboy_pb 6018)是热点 | technical-debt |

> 评级为工程视角相对判断,非严格度量。

---

## 贯穿性观察（与既有评审一致,此处记工程含义）

1. **工具齐、机制软**:lefthook(erlfmt/gitleaks/conventional)、xref=0、check_module_boundaries、SBOM diff、Sonar 都在,但全量 eunit/dialyzer/覆盖率/E2E 多为软门。工程效能的最高杠杆是"把已有软门收紧"(见 ci-notes 与 roadmap TEST-01/02)。
2. **覆盖密度不足**:日志、指标埋点、测试覆盖率三处都是"设施在但用得薄"。这是可增量补齐的,不需重构。
3. **正确范本已存在**:多处"对的做法 + 未推广"(见 `docs/planning/tech-debt.md`),工程改进 = 推广而非发明。

---

## 优先级总览（本目录建议,均为增量非重构）

| 优先级 | 建议 | 主题 |
|---|---|---|
| 高 | 生产 profile 剥离调试依赖(sync/observer_cli/recon/redbug) | dependency-notes |
| 高 | 关键路径补结构化日志 + request id | logging-notes |
| 高 | 软门收紧为 ratchet 硬门 | ci-notes |
| 中 | 补业务指标埋点 + 最小追踪 | observability-notes |
| 中 | 配置默认值/文档漂移修正 | configuration-notes |
| 中 | 版本双源同步机制 | release-notes |
| 低 | Docker 镜像瘦身与扫描 | docker-notes |

---

## 文档索引

dependency-notes · configuration-notes · logging-notes · observability-notes · release-notes · docker-notes · ci-notes · technical-debt
