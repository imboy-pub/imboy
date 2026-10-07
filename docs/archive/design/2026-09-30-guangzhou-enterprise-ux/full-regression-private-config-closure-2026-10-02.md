# 完整回归与私人配置依赖裁剪

冻结候选：`2d3b22473dca17e37becf0d4c71063a81b45df41`。

## 完整运行终态

运行 `/tmp/gz-enterprise-full-origin-qualified-gate.sh`，证据目录 `/tmp/imboy-seat-http.XLlKsK`，进程退出 1：10,736 项通过、1 项失败、0 项跳过；没有 EUnit 取消标记。唯一失败为 `llm_provider_config_contract_tests:local_config_has_bailian_provider_test_` 读取未入库 `config/sys.local.config` 返回 `enoent`。该终态为 FAIL，不能因其余检查通过而改写为 PASS。

本轮从冻结源码重新编译，排除同名配置替身覆盖；运行前生产配置模块来源、应用启动和 PostgreSQL 时间编解码通过。终态逐项核对源码哈希，无变化。使用一次性 PostgreSQL 和合成配置，没有读取私人配置或生产数据。原始日志仅保留在临时目录，仓库不保存可能含测试令牌的全文。

模块选择按照当前 Makefile 的集合和排除规则，共 1,280 个模块。排除的 VM 全局/专项套件：`elib_tsid`、`elib_tsid_guard`、`elib_tsid_bootstrap_harness_tests`、`agent_grant_pg`、`agent_run_pg`、`agent_grant_pg_tests`、`agent_recovery_pg_tests`、`agent_run_pg_tests`、`agent_tool_authorizer_pg_tests`。这 9 项需要另行隔离验证，不能把排除视为已通过。

终态记录 `/tmp/gz-enterprise-full-origin-qualified-terminal.json` 包含候选、退出码、摘要、源码绑定和日志哈希；模块集合、源码和 beam 哈希保存在运行目录。

## 裁剪与专项验证

`git ls-files` 确认发布配置源只有 `config/sys.config.example`。删除重复依赖私人本地文件的测试，发布模板测试固定读取入库 example；不再按本机是否存在私人 `sys.config` 切换验证对象。保留 provider 名称、模块、URL、密钥 env 占位、模型 env 占位和注册表 lookup 的全部断言，不调整实际配置值或生产注册表。

检查了该测试函数及 helper 的全部引用，已逐项检查差异；未进行独立代理审查。专项验证重新编译当前测试模块，使用正确来源的生产 beam 和既有依赖：`/tmp/gz-llm-shipped-template-check`，退出 0，两项通过。

新候选的连续两轮完整回归、专项隔离、三端旅程、真机和外部 OA 验收仍未完成。设备只读库存已识别到两台真机，其中 Android 已授权连接；库存可用不代表设备功能验收通过。整体投产状态仍未证实。
