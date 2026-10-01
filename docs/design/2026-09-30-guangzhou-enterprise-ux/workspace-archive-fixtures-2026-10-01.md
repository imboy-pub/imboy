# 工作区归档夹具与实际 SQL 对齐

后续复核：旧临时构建包含同名配置替身覆盖，原运行记录保留但环境结论受限。当前候选重新编译并确认生产模块来源后，五套归档检查再次通过。详见 [原生模块来源复核](native-module-origin-requalification-2026-10-01.md)。

源码基线 `14eb81bee970eec547ad0840069f13dc13d71dfe`。此前全量运行中的 6 项归档失败均为 mock 的 function_clause：测试期待无 schema 的 `UPDATE workspace`，实际 `workspace_repo:archive_tx/4` 已使用 `UPDATE public.workspace ... RETURNING organization_id`。

只修改两个测试文件的归档 SQL 前缀匹配，保留原参数、返回语义、审计列、Owner 限制、重复归档、默认工作区交接与归档后禁写检查。生产 SQL、权限和事务处理未改。已逐项检查差异；未进行独立代理审查。

运行 `/tmp/gz-workspace-archive-native-gate.sh`，复用现有隔离 PostgreSQL 生命周期、原生迁移和 EUnit runner。未变化的模块复用从冻结源码编译的 beam，两个修改测试模块重新编译。

- 运行目录 `/tmp/imboy-seat-http.onqfVq`，进程退出 0，启动和数据库时间编解码探针通过。
- `workspace_admin_tests`、`workspace_archive_tests`、`workspace_admin_archive_fk_tests`、`workspace_archive_concurrency_tests`、`workspace_archive_closure_tests`：全部 70 项通过。
- 包含真实数据库的归档审计列、并发写与归档顺序、禁写边界检查；不以修正 mock 本身推导数据库通过。

这关闭对应的归档测试失配，不代表企业全部旅程或整体投产验收完成。旧全量运行依然是 FAIL；新候选完整回归、专项隔离、设备和外部验收仍待执行。
