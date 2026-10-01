# 原生模块来源复核

候选代码：`0cbc626441593fd73953ce1727bf419f5d32fd0e`。本次只修正临时验证环境，不修改生产配置或关闭设备签名。

## 原因与旧证据限制

`src/ds/config_ds.erl` 和 `test/common/config_ds.erl` 同名。后者是无数据库写入的测试替身，`set` 返回 `ok`、`get` 返回默认值。临时全量脚本将全部生产和测试模块编译到同一目录，替身覆盖了生产模块；因此配置写入返回成功而数据库无记录，不能据此判断生产配置写入故障。

原生 erlang.mk 将测试与生产构建分开，运行路径让生产 ebin 优先。原生规则未改。此前临时全量 `/tmp/imboy-seat-http.ueIFxh` 的 FAIL 仍保留，但其配置相关失败需要在正确模块来源下复现。复用该构建的归档、流水线及客服检查也不能单独作为完整生产环境验证。

## 本次验证

临时脚本 `/tmp/gz-native-origin-requalification.sh` 从当前源码重新编译生产及测试模块，排除同名配置替身，保留原生测试 runner。在应用启动前读取实际加载的 `config_ds` 编译来源，要求其为 `src/ds/config_ds.erl`。

- 证据目录：`/tmp/imboy-seat-http.RLKsoi`；包装进程退出 0。
- 实际生产配置模块来源、应用启动、PostgreSQL 时间编解码探针均通过。
- 五套工作区归档检查、两套历史密文流水线检查、压力测试、客服接口层约束和 Widget 配置检查：全部 111 项通过。
- `source-binding.json` 记录候选 SHA 和所有 src/test Erlang 源文件 SHA-256，供下一轮复用前逐项验证。

后续运行 `/tmp/gz-native-internal-origin-gate.sh`，先逐项核对上述源文件哈希，再复用正确构建执行 `enterprise_internal_wiring_http_tests`。证据目录 `/tmp/imboy-seat-http.ebsuSW`，退出 0，全部 9 项通过，包含真实 HTTP conformance 检查；模块来源、启动及时间编解码探针通过。此前该套件的设备签名失败在正确生产模块下未复现；不能据此关闭组织目录的独立签名夹具问题，也不能把这 9 项计为所有 Internal API 场景完备。

没有复制原始日志到仓库。没有使用生产数据库或私密配置。上述结果仅覆盖列出的领域；完整回归、专项隔离、真机和外部 OA 验收仍未完成，整体投产状态仍为未证实。
