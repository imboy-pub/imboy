# 配置布局笔记（Configuration Notes）

> 工程视角 · 描述现状 + 增量改进 · 补充 `docs/archive/review/` 中零散的配置漂移记录

## 现状

**后端配置架构**(`imboy/config/`):
- Git 跟踪 `sys.config`（默认基线）、`debug.config`、`cron.config` 与 `sys.config.example`、`sys.local.config.example` 模板。
- 本地、生产和运行时生成的覆盖文件按 `.gitignore:11-15,43-45` 排除；`IMBOYENV=local` 会加载本地覆盖并生成运行时配置。仓库只能审计模板与加载逻辑，不能据未跟踪文件推断买家/生产环境的实际值。
- `IMBOY_*` 环境变量运行时优先级最高,经 `imboy_env`/`config_ds` 读取
- `vm.args`(节点名/cookie/端口)、`turnserver.conf`(TURN)、`nginx-imboy.conf`
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

## 相关模块

`imboy/config/*.config`、`imboy/config/vm.args`、`imboy/src/lib/imboy_env.erl`、`imboy/src/ds/config_ds.erl`、`imboy/src/imboy_app.erl`(validate_runtime_config)、`imboyapp/example.env`、`imboyadmin/vite.config.ts`

## 优先级

| 建议 | 优先级 |
|---|---|
| 默认值/文档漂移修正(archive_enabled/ws_url/cookie) | 中 |
| fail-fast 覆盖面复核与测试 | 中 |
| 配置项集中文档化 | 中 |
| .example 键集 diff 进 preflight | 低 |
