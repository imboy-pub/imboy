# 企业工作空间 Internal 写管理

本批新增 INT-37 POST 创建、INT-38 PATCH 修改、INT-39 DELETE 软归档。当前冻结表 39 操作 / 28 路径；渠道写入、客服 App 管理和全六项投产验收仍未完成。

- 独立 workspaces:write，创建须 Org 全域 Grant，指定本企业活跃 Owner/Admin 为资源负责人；应用主体在审计中单独记录，不冒充负责人。
- 修改/归档须目标 Workspace Grant；指定替代默认项也须同企业与其 Grant。创建复用模板事务，归档复用共用事务核。无自动 Grant。
- expected_version 防止并发覆盖；迁移 160 的 DB trigger 覆盖所有原有人类端/管理端 UPDATE，避免旧路径绕过版本。GET 列表/详情追加 version。
- 资源、Application 审计、幂等 reservation/完成响应同一 Conn / 同一事务；审计拒绝会回滚资源与 reservation。先重新验证权限，再重放；归档后仍能按同 key 重放首次原字节。
- 严格 body 白名单及 ID 范围；create request_id 为 application namespace 加幂等键哈希，避免不同应用或长键截断碰撞。公开 branding 保留白名单，内部 request_id 不输出。
- 迁移 down 在锁内拒绝仍有新 Grant、allowed_scopes 或 version>1 的回退，不静默删除授权和版本证据。

## 本地证据

- 真实 PG 全量迁移 + Cowboy：8/8。39 路由成功覆盖集精确相等；新增 3 写接口同 key 原字节重放；缺/超长 key、未知参数、缺权限、窄 Grant 创建/修改、跨企业空间/负责人、旧版本及不同 body 冲突均拒绝。
- 两个同时 PATCH 同 version=2：一个 200、一个 409 version_conflict，最终 version=3；归档 version=4。
- 注入审计 CHECK 拒绝：PATCH 500、名称与 version 不变、幂等 reservation 无残留。新空间审计共 4 条（创建/修改/并发成功修改/归档），重放不追加；actor_user_id NULL，detail 记录 application_id/correlation_id；归档后默认群和频道保留。
- 撤销 workspaces:write 后旧 DELETE key 拒绝。仅合成 marker 库内执行撤权；现有容器与生产数据不变。
- 迁移独立往返/拒绝保全：1/1。新 Scope 与 allowed_scopes 各阻止 down，完整行保持；自动 version+1 无法被旧写入手动 version=1 绕过；version>1 阻止 down。
- manifest 对齐 12/12、两个生成器 --check 通过。OpenAPI / bundle / Postman / README / endpoints / CHANGELOG 已同步。Redocly lint 退出0，存在身份映射旧 oneOf 重叠的一项警告，未宣称全合同零警告。
- 全部当前产品源码与 HTTP harness 重新编译通过（存在既有 deprecated catch 警告）。修改后的 internal_pg 测试额外编译通过，未把未执行的该套件算入 PASS。
- Admin 权限目录 17 项与后端一致：11 个相关测试、typecheck、定向 ESLint 和 hooks 通过，ae9fd3e 已本地集成，原 WIP 哈希不变。

复跑：`IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh`；`python3 test/migrations/test_workspace_internal_write.py`。
证据：[HTTP](evidence/workspace-internal-write-2026-10-01/http.txt)、[迁移](evidence/workspace-internal-write-2026-10-01/migration.txt)、[源码绑定](evidence/workspace-internal-write-2026-10-01/sha256.json)。基线 d609244f。

人工复查共用核调用链、白名单、Grant/跨企业边界、归档重放、版本触发器、锁与错误路径、审计身份和生成契约。没有执行子代理审查。没有运行生产迁移、推送或部署。完整六项目标保持未完成；还需频道写管理及完整产品/设备/投产验收。

English summary: Three Workspace write operations are implemented with explicit grants, transactionally reused creation/archive cores, database versions, application audit and byte-exact authorized replay. Thirty-nine operations are covered by real HTTP success journeys; negative cases, concurrent optimistic updates and audit rollback pass. Migration roundtrip and preservation checks pass. Admin exposes the seventeenth scope. A pre-existing identity-schema overlap warning and the complete six-part production objective remain open.
