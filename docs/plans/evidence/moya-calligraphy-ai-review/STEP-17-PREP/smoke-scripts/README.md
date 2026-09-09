# smoke-scripts — 教学契约打点固定前置（Step 17 联调用）

用途：对 moya-teaching 冻结契约做真实 HTTP 级偏差实测（R8/R9 实测的可复现资产）。
配置来源：jwt_key 等全部运行时读 `config/sys.local.config`（**脚本零硬编码密钥**）；
节点 boot 配方照 `../backend-boot-smoke.md`（直连 erl，imboy_smoke@127.0.0.1:9811，
**不要 make run**）；库 `moya_boot_smoke@127.0.0.1:4323`。

运行顺序：

1. `psql -d moya_boot_smoke -f seed_smoke.sql`（首次；7xxxx 段最小链）
2. boot 冒烟节点（配方见上）
3. `./matrix_teaching.sh`（核心链：context/assignment/submission 幂等/review 全链/history/撤回）
4. `./matrix_bind.sh`（bind/unbind 守卫与本人 history 分支；依赖第 3 步产生的提交）
5. `psql -f attach_scope_prep.sql`（附件 teaching scope + 撤回态附件；其中 sid2 替换为第 3 步实际输出）
6. `./matrix_attach.sh`（附件三门：presign/confirm/view_url 场景矩阵）
7. `./halt_smoke.sh`（收尾，确认端口/epmd 释放）

预期结论对照：`../contract-deviations.md` §1 矩阵（R8/R9 实测基线）。
