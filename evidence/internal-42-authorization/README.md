# Internal v1 全路由授权负例

真实 Cowboy 中间件和隔离 PostgreSQL：42 路由 × 无授权、缺 scope、已撤销、已过期四场景；消息 INT-09/10 另含 Human 代发，共 176 条实际 HTTP 403 / insufficient_scope。有效授权有 HTTP 200 对照。18 个指定业务表行内容摘要前后不变；凭证使用时间和拒绝响应幂等记录不纳入业务快照。

已接入 enterprise_internal_wiring_http_tests 的 conformance，完整成功链和新增负例共同通过。生产代码未改。现有成功链使用的对象 HEAD / 公网 DNS 服务替身继续保留，因此本证据不代替资料真实存储和 Webhook 真实投递证据，也不覆盖客户 OA 或 iOS。

复现：将附带两个 .txt 启动器恢复到 /tmp 下对应 .sh / .py 文件名；配置 IMBOY_DEPS_ROOT 为依赖构建目录，必要时修改 shell 的 REPO 到当前独立仓，然后 bash /tmp/gz-internal-auth-conformance.sh。需要 Docker 的 imboy/pg18:3.6.1-2 镜像。默认重新编译当前源码；复用编译产物必须逐一匹配源码、依赖和 beam 摘要。

首次独立测试启动器缺少配置缓存进程，接入链未进入业务断言；补齐真实缓存后通过。最终启动器明确核验 176 个 ID/场景/模式组合，防止 EUnit 输出捕获或证据缺失导致假通过。
