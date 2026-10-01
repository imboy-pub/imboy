# 客服接口层检查误判修复

后续复核：旧临时构建包含同名配置替身覆盖，原运行记录保留但环境结论受限。当前候选重新编译并确认生产模块来源后，客服分层和 Widget 配置检查再次通过；此次未重跑 data_disposition_tests。详见 [原生模块来源复核](native-module-origin-requalification-2026-10-01.md)。

基线 `a2f6293eb3f6673be27dda0d51e55df8e45e2251`。接口层检查之前搜索任意 `presign` / `view_url` 字符串，把 `cs_http` 的路由动作字符串误报为签名操作。现在使用 Erlang 原生词法扫描，仅检查相应名称的调用、定义和函数引用，保留既有数据库、DS、Repo、动态派发和 crypto 检查。

新增检查同时证明路由字符串与注释可接受，带空格的真实调用、带后缀调用及 `fun store:view_url/1` 仍被检测。未调整业务路由或授权行为。已逐项检查本次差异及扫描边界；未进行独立代理审查。

运行 `/tmp/gz-enterprise-route-scan-native-gate.sh`，使用既有隔离 PostgreSQL 生命周期、原生迁移和 EUnit runner。未变化的模块复用从基线源码编译的 beam，本次测试模块重新编译。

- 运行目录 `/tmp/imboy-seat-http.E86Xvq`，进程退出 0。
- `data_disposition_tests`、`cs_route_contract_tests`、`cs_widget_env_tests`：44 项通过。
- 应用启动与原生数据库时间编解码探针通过。

前一次 `/tmp/imboy-seat-http.n2JAXw` 虽有 44 项通过，但包装脚本误要求这三套件并不生成的 identity-responses.json，整体退出 1，不能计为完整门通过。修正临时包装脚本后重跑得到上述退出 0。当前结论仅覆盖这三套件；全量、专项隔离套件、真实客户端及生产状态仍待验收。
