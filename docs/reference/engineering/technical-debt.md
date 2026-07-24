# 工程技术债笔记（Technical Debt — Engineering Lens）

> 工程视角的债务汇总 · 补充而非替代 `docs/planning/tech-debt.md`(那份含全量 P0-P3 + 演进路线)
> 本文只聚焦**工程质量维度**(结构/可维护/组织/门禁)的债务,不含业务功能缺陷。

## 现状（工程债盘点）

### 结构与组织
- **巨型文件**:`imboy_pb.erl` 6018(生成物,可接受)、`adm_channel_handler.erl` 1044、`barrel_mcp_session.erl` 1027、`imboy_router.erl` 977、`moment_logic.erl` 962、`msg_c2c_logic.erl` 937、`websocket_handler.erl` 925;Flutter `chat_page.dart` 2234 + 12+ 个 >800 行。均超 800 行规范。
- **Flutter 三套运行时并存**:Riverpod 图外手写单例(WS/Retry/Message)+ `lib/modules/` DDD 试点(38 文件)+ 传统 page/service——迁移中间态(见 review flutter-review §架构)。
- **工作区根级卫生**:非 git 仓库的工作区根散落 `REPAIR_PLAN_v2`、`CLEANUP_PLAN.md`、`spikes/`、`releases/` 等,归属需梳理(根 CLAUDE.md 已警示只允许 AI/工具配置在根级)。

### 门禁与机制(头号工程债)
- **软门未收紧**:Full EUnit/dialyzer continue-on-error、覆盖率仅存在性检查、admin E2E 零进 CI、契约无 diff 门。
- **有约定无 lint**:autoDispose/裸URL/800行/token/console.log 靠自觉(见 review 头号根因)。
- **坏死工作流**:`integration_test.yml` 路径失效。

### 错误处理
- Flutter 20 处 `catch(_){}` 静默吞错(7 处阅后即焚)、后端 epgsql cast 吞错返空(见 review P1-Q1/P1-D2)——工程含义是"失败不可见,排障靠临时加日志"。

### 死代码与重复
- 死资产:`liveRoom` 四层齐全挂生产路由却零测试引用(见 review P2-8,删前先冻结确认)。
- 重复:钱包守卫两套写法、ACK 编码两套、DDL 三镜像、鉴权豁免 4 处平行 path——"正确范本 + 未推广"(见 review tech-debt)。
- 翻案(非死代码,勿误报):`msg_rate_logic` 已接线、`textStream` 已复活。

### 文档漂移
- `msg_archive_enabled` 默认值、`ws_url`、imboyadmin 记为 Vue(实 React)、schema 版本号(CLAUDE.md v21/代码 v23)、ADR 仅 3 条未覆盖 E2EE/支付/LiveKit/MCP。

## 优点（工程债处于可控状态的信号）

- 分层边界机制化(check_module_boundaries,全仓仅 1 破窗),结构债有护栏。
- ratchet 框架 + 成功样板(xref=0),门禁债有收紧路径。
- 死代码/重复多为"未收口"而非"不会做",范本已存在,清理成本可控。
- 债务已被系统性记录(本次 review/roadmap/testing/engineering 四套文档),可追踪。

## 潜在改进（增量,非大重构）

| 债务 | 增量动作 | 优先级 |
|---|---|---|
| 软门未收紧 | 按 backend-ci 自带计划翻 ratchet 硬门 | 高 |
| 有约定无 lint | 补 custom_lint / 边界脚本(ENG-01) | 高 |
| 静默吞错 | 补 error 日志使失败可见 | 高 |
| 文档漂移 | 批量修正(ENG-06) | 中 |
| 巨型文件 | 逐个拆分,每步真机(ENG-03) | 中 |
| 死资产 liveRoom | 冻结确认后下线(ENG-04) | 中 |
| 重复"正确范本" | 推广范本收口(见 review 对照表) | 中 |
| 根级卫生 | 散落文件归属梳理 | 低 |

> 说明:以上均为增量收口,不涉及架构重设计。大重构类(运行时收敛/平台职责分离/E2EE 收敛)见 `docs/roadmap/`,不在本工程债范围。

## 相关模块

见各条证据指向;交叉参考 `docs/archive/review/code-quality-review.md`、`docs/planning/tech-debt.md`、`docs/roadmap/engineering-roadmap.md`

## 优先级

高:软门收紧、lint 机制化、静默吞错可见化。中:文档漂移、巨型文件拆分、死资产清理、重复收口。低:根级卫生。
