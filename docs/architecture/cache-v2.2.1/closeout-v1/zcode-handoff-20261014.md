# ZCODE 缓存收口接续指令

以下正文供用户发送给 ZCODE；其中“额外两轮”是交接后新修复窗口的明确授权，
不表示此前执行已获豁免。发送前可按用户决定删除该授权；删除后沿用计划停止条件。

```text
继续 CACHE_V221_BACKEND_CLOSEOUT_V1，保留已有实现和证据，不重启项目。
目标：完成 BACKEND_LOCAL_CLOSEOUT_PASS；使用实际门禁判定，不降标准。
仓库：/Users/leeyi/project/imboy.pub/imboy（如在 dot，使用迁移后的对应仓库）。
从最新 main 建立唯一任务 worktree；先核对 HEAD、dirty、外来 WIP 和活动进程。

计划：docs/architecture/cache-v2.2.1/backend-closeout-plan-v1.md
SHA256：806114da93a904108dfa9a2747d1ee59d4efcb1a2562a92ae7ea6ae905882f0f
原计划：docs/architecture/cache-v2.2.1/w0/plan-v2.2.1.md
SHA256：a5523ee731bc822018831586e3d747ba109c2c66e734daf5633b494d9b18c713
先完整读取计划及 closeout-v1/closeout-report-v1.md 当前复验与停止点、
evidence-schema-v2.md、c12-window-plan.md。机械核对计划摘要，禁止自动重绑。

此次我明确授权：对上一会话已达到上限的 snapshot fixture 根因，开放额外两轮
自动修复窗口；计数跨所有 agent 累计，失败不得通过换 agent/换名称重置预算。
其余根因遵守原计划每项最多两轮。超限停止该项，完成独立合法任务后交付 NO_GO。

当前事实：
1. 已完成的实现与文档已提交并合入 main；旧任务 worktree 已清理。
2. 两个失败中的测试文件没有作为通过代码合入 main，已封存为 unverified-fixture.patch。
   补丁 SHA256=e9e99a88c6a5b8a947955ec42bd3e7e9303ffb14959a5ae803bc1dd3932afdfd。
   本机证据目录：~/.codex/cache-closeout-runs/cache-review-20261014/。
   配套传输包：同父目录 cache-review-20261014.tar.gz；在 dot 先传输、核对清单。
   补丁基于 17a7aed0 的源码（后续会话提交只有文档）；不要直接套用后签 PASS。
3. 补丁状态七组 EUnit：141 passed / 6 failed / 0 skipped，真实 exit=1。
   它不是当前 main 的实测结果，main 未包含这份失败补丁。六个用例及 oracle 见收口报告。
4. 独立审查 HIGH：done/5 不能从收到的页集反推 CutSeq 再覆盖 origin_target；
   不能清空 retained_obligations 来制造恢复闭合。来源 target/cut 必须独立冻结。
   same_epoch 用例实际失败为 snapshot_content_conflict；fixture 缺少真实 cache
   服务读取路径。按合同提供存储读取、来源身份和真实 apply_v2 增量，不能改生产校验放行。
5. 修复须保留 timer 清理；覆盖 helper 全部直接调用方。control/metadata 额外失败
   必须逐项判定产品缺口还是 fixture 缺口，不可统一标为无关环境失败。
6. C14 自测 53/53、ID 96/96、计划绑定 2/2 曾通过；完整 --all exit=1、2/6。
   candidate/exits/utc/receipts 未闭合，未冻结最终候选，不能把自测当签发。
7. QR 当前源码的真实 PG 双节点 status/status、SSE/status、SSE/SSE 3/3，exit=0，
   每种仅一名消费者取得凭据，测试行清理为 0；Cowboy 运输为 observer，
   不等于 HTTP 全链路、迁移回滚或最终候选证据。
8. 旧版 SHA=3a01829d32c94bdb15f80150445d0c1b71b88a8b。
   旧运行树临时路径不能跨机器继承；需重新从该提交构建、记录依赖和 BEAM 摘要。
   上一会话只验证 cache/sync/metric/log 的局部运行树启动，非 904 模块全应用重建。

执行顺序：
- 先建立失败 oracle 与精确所有权；最多四个活动 agent 含协调者。
  一个写者处理同步/fixture，一个处理证据入口，一个独立只读 reviewer；按需要复用。
- 修复 C04/AC-19 完整页集、来源内容、增量无遗漏和 READY 屏障；补齐直接调用方回归。
  在真实三节点中证明值与删除集合，不用 counter/空 done/已见水位替代内容证明。
- 检查受影响 C02/C03/C05/C06/C07，COMMIT 后失效、ROLLBACK 不广播及资源边界。
- 独立审查通过后冻结代码、配置、依赖、driver 与候选清单，停止产品写入。
- 串行执行完整 C09 和 C10 各两轮、QR 完整门禁及专属库迁移回滚、C11 HTTP/scrape/
  告警消费、最终全量 EUnit、本地性能；具体顺序服从原计划依赖。
  当前文档矩阵数需从 driver 再核对，历史 22/13 不能覆盖新增场景。
- 全量必须最终候选 exit=0、Failed=0、非预期取消/跳过=0；定向重跑不能替代整轮通过。
- 性能按 env_r1/base_r1/cand_r1/base_r2/cand_r2/base_r3/cand_r3 七份真实回执，
  守住冻结阈值、资源预算和安静窗口，不调参数制造 PASS。
- C14 默认 ledger-v2，显式传本轮 evidence-dir、candidate-manifest、source-root。
  required FAIL、非零 exit、异常日志、证据篡改、缺失/过期/混用候选必须拒绝。
  更新既有权威台账和报告，不每轮新增过程报告。

权限和环境：
只修改 imboy；保护 imboyapp/imboyadmin 和外来 WIP。禁止 reset/clean/stash、全量暂存。
允许验证后按完整逻辑功能本地提交，Git 身份 leeyi <leeyisoft@qq.com> 命令级设置。
不 push/部署/发布，不重启主机，不停止其他会话资源，不修改共享或生产数据库。
专属库配置需单独安全提供并验证目标；本机 main 的既有配置指向
127.0.0.1:4323/imboy_cache_closeout_v1，不能在 dot 猜测凭据或复用生产配置。
全量、故障与性能遵守互斥租约，旧 PID/锁文件不能代替活跃进程取证。
生产 SLO、目标硬件和峰值缺失独立登记，不掩盖本地缺口。

只在全部合法可执行工作完成或达到停止条件后结束，并列返回：
BACKEND_LOCAL_CLOSEOUT=PASS 或精确未闭合集合；LOCAL_CANDIDATE 独立判定，
缺生产输入继续 BLOCKED；PRODUCTION_ACCEPTANCE=NOT_ISSUED。
附最终候选、实际命令/exit/计数、证据 SHA256、本地提交与剩余 owner/解锁条件。
```
