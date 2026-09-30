# OA 签发企业绑定 / OA issue organization binding

基线：`02d141f51904404a9ee403f99d504b081cf999af`。日期：2026-10-01。

Human 签发接口新增可选 `organization_id`。语法前置验证正数 int64；应用查询结果先按所选企业过滤，再执行原有成员、企业有效状态、身份映射、重定向及一次性 code 校验。旧请求兼容路径不变。没有修改 Internal 交换认证或凭证。

回归检查：`enterprise_oa_context_binding_tests` 在旧逻辑下失败（所选企业未消除跨企业同 key 多义），修改后 1 项 EUnit 通过、0 跳过；包含两企业同 key、无匹配企业、不能降级另一企业、非法 ID 及兼容请求断言。业务模块与测试编译通过。日志 `/tmp/gz-oa-binding-before.log`、`/tmp/gz-oa-binding-test.log`。

测试使用合成应用行和 mock 数据访问，验证实际签发逻辑的范围收敛及拒绝路径；不证明真实 PostgreSQL、完整签发交换、App 请求绑定或设备 OA 会话。客户端字段传递、企业应用发现、恢复登录、OA 服务端退出仍未完成。整体可投产状态尚未成立。
