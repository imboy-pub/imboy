# 日志一致性笔记（Logging Notes）

> 工程视角 · 描述现状 + 增量改进

## 现状

**后端**:日志走 `lager`(依赖)+ 自封装 `elib_log`(`src/lib/elib_log.erl`,内部 `lager:log`)。实测调用面稀疏——全后端约 **18 个源文件**使用 `elib_log`,集中在 `imboy_cluster`(约 18 处调用)、`user_tag_relation_*`、`adm_auth_middleware`、`msg_c2c_logic`、`config_ds` 等少数模块;分级用 info/warning/error/debug。相对 477 个源文件,**日志覆盖密度很低**。

**Flutter**:`debugPrint`/`log`;评审记录 20 处 `catch (_) {}` 静默吞错(7 处在阅后即焚 `chat_burn_service.dart`),失败无日志痕迹。

**Admin**:项目规范禁 `console.log`;评审记录仍有零散残留待收口。

**聚合**:后端日志经 `promtail` → `loki`(deploy 栈),集中查询。

## 优点

- 有统一封装层 `elib_log`,分级机制到位。
- 日志聚合链路(promtail→loki)完整,具备集中查询基础。
- Admin 有"禁 console.log"规范意识。

## 潜在改进

1. **提升关键路径日志覆盖**(优先级高,增量):消息投递/ACK/鉴权/支付/E2EE 等关键链路应有一致的进入/结果/异常日志。当前覆盖稀疏,故障排查依赖临时加 `debugPrint`(评审记录朋友圈上传排障、grep 被 i18n 钩子污染等取证困难)。
2. **消除静默吞错**(高):Flutter 20 处 `catch(_){}`、后端 epgsql cast 吞错返空(见 review P1-Q1/P1-D2 与 epgsql 记忆),至少补 error 级日志,让失败可见。
3. **结构化日志 + request id**(高):当前为文本日志,建议关键路径加请求关联 id(见 observability-notes 追踪缺口),使日志可关联、可检索。
4. **敏感数据审计**(中):建立"日志不得含密码/token/密钥/PII"的检查(gitleaks 管密钥入库,但运行时日志泄露需单独审计),尤其鉴权/E2EE 路径。
5. **日志分级规范文档化**(低):明确 info/warning/error/debug 各自语义与生产级别,统一封装用法。

## 相关模块

`imboy/src/lib/elib_log.erl`、`imboy/src/lib/imboy_cluster.erl`、`imboy/src/logic/msg_c2c_logic.erl`、`imboyapp/lib/.../chat_burn_service.dart`、`imboy/deploy/promtail/`、`imboy/deploy/loki/`

## 优先级

| 建议 | 优先级 |
|---|---|
| 关键路径补一致日志 | 高 |
| 消除静默吞错(补 error 日志) | 高 |
| 结构化日志 + request id | 高 |
| 日志敏感数据审计 | 中 |
| 分级规范文档化 | 低 |
