# 配置布局笔记（Configuration Notes）

> 工程视角 · 描述现状 + 增量改进 · 补充 `docs/archive/review/` 中零散的配置漂移记录

## 现状

**后端配置架构**(`imboy/config/`):
- Git 跟踪 `sys.config`（默认基线）、`debug.config`、`cron.config` 与 `sys.config.example`、`sys.local.config.example` 模板。
- 本地、生产和运行时生成的覆盖文件按 `.gitignore:11-15,43-45` 排除；`IMBOYENV=local` 会加载本地覆盖并生成运行时配置。仓库只能审计模板与加载逻辑，不能据未跟踪文件推断买家/生产环境的实际值。
- `IMBOY_*` 环境变量运行时优先级最高,经 `imboy_env`/`config_ds` 读取
- `vm.args`(节点名/cookie/端口)、`nginx-imboy.conf`。原 `turnserver.conf`（coturn）已删除——TURN 不再有独立配置文件，由 LiveKit embedded TURN 承载：后端侧是 `{livekit, #{ws_url, api_key, api_secret}}` 配置段（`sys.config.example` 模板 + `IMBOY_LIVEKIT_*` env 覆盖，缺键时入会返回受控错误 `livekit_not_configured`），媒体侧是 `deploy/docker-compose.livekit-turn.yml` overlay（`LIVEKIT_TURN_ENABLED` 开关）
- 生产 fail-fast:`imboy_app.erl` 的 `validate_runtime_config()` 在 strict env 下 `ensure_required_secret`(jwt_key/postgre_aes_key/adm_cookie_secret/solidified_key 等)+ `ensure_required_file` + `ensure_api_auth_switch_on`

**Flutter**:`example.env`、`flutter_options.yaml`、build flavor;**Admin**:vite env(`VITE_API_BASE_URL` 等 build-arg)。

## 优点

- 三层 + env 覆盖的优先级清晰,本地/生产隔离良好。
- 生产启动 fail-fast 校验敏感项,漏配即崩(强于静默用默认值)。
- `.example` 模板齐全,新环境可复制。
- 密钥经 env/config 注入,未入库(gitleaks 门 + 评审复核确认)。

## 潜在改进

1. **默认值/文档漂移修正**(优先级中,增量):
   - `msg_archive_enabled` 在 `sys.config` 为 true,根 `CLAUDE.md` 称默认 false(见 review P3-1)——统一文档与配置。
   - 默认 `ws_url` 指向不存在的 `/ws`,真实路由 `/api/v1/ws`(见 review P1-P6)——修默认值或加 preflight 校验。
   - `adm_cookie_secret` 有硬编码默认 `imboy-adm-cookie`(见 review P1-A3)——建议无默认值直接依赖 fail-fast,消除误配穿透。
2. **配置项文档化**(中):关键配置项集中说明(用途/默认/生产要求),降低漂移。当前配置语义散落在多个 `.config` 注释与 CLAUDE.md。
3. **多环境一致性校验**(低):`.example` 与实际 config 的键集 diff 进 preflight,防漏配新键。
4. **fail-fast 覆盖面复核**(中):`validate_runtime_config` 依赖 `is_strict_env` 判定;记录并测试"误配 IMBOYENV 导致 strict 判定错误"的兜底(见 review 对 P1-A3 的裁决)。

## ecron 定时任务键名约束（易错）

周期作业统一在 `config/sys.config.example` 的 `{ecron, [...]}` 段定义。当前 pin 的 ecron v1.1.1（`include/deps.mk`，`sys.config.example` 内注释所写 v1.1.0 已过时）只消费 `local_jobs`（每节点各跑）或 `global_jobs`（集群单实例，依赖 global 注册与 quorum）两个键；写成 `{jobs, [...]}` 不报错但**整段静默不生效**（2026-09-09 之前入仓的作业块曾因此从未被调度，已修正为 `local_jobs`；仓内作业均幂等/带守卫，双节点重复执行安全）。

两点运维须知（源自 wiki Configuration 页回流，2026-09-30）：

1. **作业在节点重启后才真正激活**。旧版本升级后首次重启时各作业第一次实际运行：支付对账首跑回看约 25 小时，清理类按各自阈值扫描——属预期行为，不必告警。
2. **验活方法**：节点启动后执行 `ecron:statistic().`，应返回各作业且 `status => activate`、`ok` 计数随周期增长；返回 `[]` 即配置键名有误或 ecron 未启动。

## 相关模块

`imboy/config/*.config`、`imboy/config/vm.args`、`imboy/src/lib/imboy_env.erl`、`imboy/src/ds/config_ds.erl`、`imboy/src/imboy_app.erl`(validate_runtime_config)、`imboyapp/example.env`、`imboyadmin/vite.config.ts`

## 优先级

| 建议 | 优先级 |
|---|---|
| 默认值/文档漂移修正(archive_enabled/ws_url/cookie) | 中 |
| fail-fast 覆盖面复核与测试 | 中 |
| 配置项集中文档化 | 中 |
| .example 键集 diff 进 preflight | 低 |
