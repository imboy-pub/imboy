# 客服坐席 Internal API：阶段验证 / Seat Internal API progress

状态：LOCAL_API_PASS，候选待提交集成；完整六项需求仍在实施。基线：45cd112465a1e6bc0ec04cadfbd18776f30ab198。

本次候选增加 INT-33..36（列表、详情、创建、修改），独立 customer_service:read/write scope，企业级 Grant、事务内幂等与应用主体审计。Seat 属于企业；workspace_id 仅为审计位置。停用使用 PATCH enabled=false，保留坐席和历史事件。

已验证：

- `python3 test/migrations/test_customer_service_internal_api.py`：退出 0。独立 PG18 合成数据库；159 up 可重复执行、down/up 往返；非法 scope/metric 被约束拒绝。两种新 Grant scope、两种 application allowed_scopes、新 seat.read 用量分别阻止 down；拒绝后原行不变。只有测试自有合成行被清理。
- EUnit `enterprise_internal_id_validation_tests`：6 项通过。新增 business_identity_id 范围/认证先后校验；Seat 列表非法 limit 和越权过滤参数在访问数据库前拒绝。
- 本次修改的 Internal API 模块、新 handler/logic、usage repo、相关测试编译成功。为避免旧 ebin 造成假失败，相关 Workspace/Project/Channel handler/logic 从当前候选重新编译到 /tmp/gz-seat-api-beams，未覆盖主仓构建产物。
- `bash scripts/check_feature_architecture.sh`：退出 0，裁剪门通过，警告 0。
- `git diff --check`：退出 0。

契约同步已完成：36 端点/28 路径 manifest 已入 api/internal/v1/manifest.yaml，保留历史 run manifest 不动；OpenAPI 编辑真源/零 ref bundle/聚合入口/Postman/README/endpoints/CHANGELOG 已同步。12 项 manifest 门禁通过；6 项接线检查通过；Redocly lint 退出 0、零警告；两个生成器 --check 退出 0。contract-export 从当前源码补齐此前已提交的路由漂移，非手改生成物。

Admin 权限目录从旧 10 项补齐当前 16 项（包括原有四项只读权限），与后端逐字、顺序一致。51 项相关测试、typecheck、定向 ESLint、提交 hooks 通过；独立本地提交 878c1df 已 fast-forward 主仓，原有无关 WIP 文件与 diff 校验一致。

真实 HTTP conformance 已通过：在本机已有 imboy/pg18:3.6.1-2 镜像上创建独立容器、合成凭证和全量迁移后的 marker 库，真实 Cowboy 中间件/池化数据库请求，无 HTTP 或 DB mock。既有外部 DNS/对象 HEAD 用服务替身，未发送外部 Webhook。

- 36 个 route ID 成功响应覆盖集与冻结表精确相等。
- 19 个 required mutation 验证同 key 同 body 状态/响应字节一致及 Idempotent-Replayed=true。新增两条 mutation 各验证不同 body 的 digest 冲突及缺/超长 key 拒绝；旧全量套件的不同 body 负例仍是 INT-19，不能声称旧 17 条各自都覆盖了该负例。
- 新坐席接口覆盖四条缺 scope、窄 Workspace Grant 不足、跨企业身份拒绝、重复坐席、旧版本冲突，撤销 write scope 后旧 key 重放被拒绝。
- 两个同时发出的真实 PATCH 对同 version=2：恰好一个 200、一个 409 version_conflict，最终 version=3。
- 注入合成审计约束失败：PATCH 返回 internal_error；Seat version 不变化，幂等 reservation 不残留。
- 修正 EUnit setup instantiator：流程在显式 test fun 中运行，报告不再是“没有测试”；完整接线模块 7/7 通过。
- 全部当前源码及三个 harness 模块（871 个文件）重新编译通过，输出仅在 /tmp；存在既有 deprecated catch 警告，未宣称零编译警告。
- 可复跑：`IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh`，退出0。脚本默认依赖本仓 deps/ebin，不自动安装或拉取镜像；现有镜像 init hook 的 template_postgis 缺陷由测试专用空 init 挂载绕过，所有真实扩展及迁移由 marker harness 执行，无扩展 mock。
- 最新证据 /tmp/imboy-seat-http.OpJijY/http.log、compile.log、image-id.txt；完整门禁容器已自动清理。宿主无扩展 PG 的失败记录保留为历史诊断，已不是当前阻塞。

待完成：完整六项需求、设备端与投产门禁。本地 API 验证不能证明全部六项需求可投产；没有生产部署或迁移执行。

English summary: Candidate-only progress. Migration 159 preserves permission and usage rows on rejected rollback, with locks held through the guard and constraint restoration. Compilation, six validation checks and the architecture gate pass. Contracts and Admin scopes are synchronized and locally checked. The real HTTP journey now passes on an isolated existing Docker image with all extensions and actual migrations. Thirty-six operations, replay headers, grant denial, concurrent optimistic updates and audit rollback are checked. The complete six-part product objective remains unfinished; production readiness is not proven.
