# 测试体系缺口分析（Phase D）

> 2026-11-23 · 原则：复用既有测试体系（工作约束 3），只盘点缺口、不重建
> 依据教程 Ch.08 Testing

---

## 1. 既有测试资产盘点（实测数字）

| 维度 | 现状 | 证据 |
|---|---|---|
| 单元测试 | test/ 1467 个 .erl；分层目录 adm/api/auto_test/common/domain/ds/features/fixtures/integration/lib | `find test -name "*.erl" \| wc -l` |
| Mock | meck 用于 688 个测试文件 | grep -rl meck test/ |
| 公差测试 | test/pbt/ 8 个 PropEr 套件：tsid 序列/一般、cjson、dt、manifest、oracle_faults、demo、frame | grep ?FORALL |
| Common Test | 19 个 `*_SUITE.erl`（集成面） | find |
| 故障注入 | 66 个文件含 exit(kill)/meck:exit（含 CI-00 kill 风暴 10 连杀形态） | grep |
| 真机/E2E | scripts/smoke/（c2c_ws、上传、商业 RPC）、RTC 双端手册、W7 UAT 车道、prodready_e2e release 验证车道、auto_test/ | 目录实测 + 项目记忆 |
| 质量门 | eunit/dialyzer(OTP28 固定)/gradualizer/erlfmt/边界校验脚本 + 收据制 quality 门（1958/1961 量级） | 项目既有（不在本轮重验） |

**结论：测试金字塔厚度远超一般 Erlang 项目，教程 Ch.8 checklist 大部分已覆盖。**

## 2. 对照教程 Ch.8 checklist

| 检查项 | 状态 | 说明 |
|---|---|---|
| 不用 sleep 做同步 | ⚠️ 部分 | test/ 有 242 处 timer:sleep。抽查多位于"deadline 轮询循环"（有界等待、到点即失败），非裸睡固定时长；但也存在少量固定 sleep。既有体系不重构（约束 3）；新增测试应遵循"call 栅栏/monitor 等待"（教程 §4.1） |
| 测试清理注册名 | ✅ | quality 门既有 silent-name 清理与 companion 语义修复链（项目历史 50 条坑记录） |
| 纯函数有属性测试 | ✅ | pbt/ 覆盖 TSID、编解码、时间、manifest |
| 杀进程测监督恢复 | ✅ | 66 文件故障注入 + CI-00 kill 风暴测试（imboy_sup intensity 50/60 即由该测试论证） |
| 状态ful组件有模型化属性测试 | ❌ **缺口 1** | 见 §3.1 |
| 集成测试启动真实 app | ✅ | CT 套件 + prodready_e2e（release 形态启动验证）+ smoke 编排 |
| Dialyzer 在 CI + spec 如实 | ✅ | nightly dialyzer 固定 OTP28 + 指纹基线管理（项目记忆）；spec 收窄即坏味道的纪律已有先例（websocket_handler.erl:292-295 删除不可达兜底子句的注释） |
| 期望值来自需求而非实现 | ✅/⚠️ | 大量测试带合同编号注释（如 W2-H、T02 冻结合同引用）；未逐案验证 |

## 3. 真实缺口（按价值排序）

### 3.1 缺口 1（P3→建议优先补）：WS 投递/重试状态机无模型化属性测试

- **对象**：`websocket_delivery_guard`（重试节奏 MsLi + 每跳重鉴权 + cancel_ack）+ BE-14 预算门（allow/defer/合并通知）+ `ack_retry_cache`。
- **现状**：这些组件有单元测试（ack_retry_cache_tests 等），但均为示例驱动；无 PropEr stateful（命令序列 + 模型）测试。
- **为什么值得**：教程 Ch.8 §2（"状态机随机序列 + 模型是发现排序/重启 bug 最有效手段"）。投递重试链是"超时=未知结果"语义最密集的组件，历史缺陷（转发派发无消费者等产品级 bug 见项目记忆）恰恰发生在状态交互处。
- **建议形态**：PropEr statem，命令集 = {connect, disconnect, send, ack, defer_over_budget, cancel, crash_ws}，模型 = 期望"每消息最终态 ∈ {confirmed, pulled_offline, dropped_by_budget+noticed}"。
- **工作量**：M（1 个新 pbt 套件 + 模型）。

### 3.2 缺口 2（P3）：连接断开/恢复的混沌用例未系统化

- 现有：RTC 双端手册与 W7 UAT 有人工/脚本化断连场景；eunit 层有 kill 注入。
- 缺口：**自动化**的"杀掉 N% WS 进程 → 验证 syn 摘除、预算行清理、消息走离线补拉、重连后补齐"回归编排（目前散在 smoke/手工）。
- 建议：并入既有 smoke 体系做一个 `scripts/smoke/chaos_reconnect.py`（复用 c2c_ws_smoke 骨架），不新建框架。
- **工作量**：S-M。

### 3.3 缺口 3（观察项）：242 处 timer:sleep 的防回归

- 不动存量（约束 3）。新增测试的纪律已在团队先例中（"call 栅栏代替 sleep" 教程 §4.1 与项目 eunit 修复链一致）。
- 可选低成本动作：CI 上跑 `grep -c "timer:sleep" test/` 做棘轮（只减不增）——与项目既有棘轮文化一致。

## 4. 明确不做的（本轮结论）

- 不引入新测试框架、不重建 CT/eunit 结构（约束 3）。
- 不为 PBT 覆盖率设硬指标（pbt/ 已存在，扩面按 §3.1 价值驱动）。
- 不动 242 处存量 sleep（体系内既有，多数有界）。

## 5. Phase D 结论

测试体系是本项目的显著强项（1467 文件、故障注入/杀风暴/真机/release 验证俱全），教程 Ch.8 八条 checklist 已覆盖六条半。真实缺口仅两处：投递状态机的模型化属性测试（价值最高）与断连混沌的自动化编排；均为**加法**而非重构，工作量合计 M 级。
