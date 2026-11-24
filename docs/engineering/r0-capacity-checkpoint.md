# R0 Checkpoint — 阶段 G 收口（2026-11-24）

**当前态**：阶段 E/F/G 全部执行完毕；分支 `fix/r0-capacity-limits-20261123` @ `6825219c2`（8 提交）；PR 已创建待属主审阅（**禁自行合并**）。六态总账：`r0-capacity-test-results.md` §3。

**已收口**：E（准入/指标/VM/部署/测试全绿）→ F（阶梯 + rig 天花板 ≈1.7k + cap=1/5/1500 直测）→ G（report/test-results/rollback/status/RESUME/设计 §13 批准记录与 §11 勘误）。

**关键勘误（阶段 E/F 新增）**：cowboy supervisor 升级**换进程**（准入必须在幸存进程 websocket_init 认领，提交 6825219c2）；ranch list 形态 opts 只当 socket_opts（钉住须 map）；ets 阈值式 badarg；bench ≥3k 为客户端幻觉；rig 服务端稳态 ≈1.7k。

**事故档案**：prodready_e2e 误杀×2 → 均已原样恢复（ping=pong/healthz=200）；eunit 险指共享 dev 库（迁移守卫拦截零写入）→ 改用一次性测试库。

**续接**：若属主批准合并 → 由属主操作；若要求补充验证（Prometheus 端到端/生产只读回填/镜像构建检视）→ 从 RESUME-R0.md 进入。
