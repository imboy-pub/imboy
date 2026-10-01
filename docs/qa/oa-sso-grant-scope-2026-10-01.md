# OA SSO 当前范围、Grant 撤销/到期与并发新建授权

基线 1e8a182be413cd9906d49d2c0ba0465a2353dc73。evidence/source-sha256.txt 绑定最终五个入口文件；仅覆盖 OA-02 / INT-B01 的授权范围子场景，不将整个验收 ID 或六项需求标为完成。

根因：交换业务虽已锁组织/应用/凭证状态，却仍信任中间件结束事务时的旧 scope 结果。应用 allowed_scopes 移除 sso:exchange 后，实际HTTP仍200，基线S9NLcn因此失败。

修复：既有交换权限复核在消费前及成功前调用同一 Grant repo scope_tx。已锁住应用后，按ID全序共享锁住该应用的Grant父行，再用第二次原生查询求应用允许范围与有效Grant范围的交集。只允许使用第一条查询实际加锁的Grant ID集合，排除两条查询间并发新建的未锁行。时间用clock_timestamp，避免事务开始时间在等锁后仍把Grant视为有效。缺权限403/insufficient_scope；读取失败security_gate_closed；失败由既有交换入口整体回滚消费和审计。没有迁移、新依赖、新接口或零Grant兼容回退。

范围降级复用现有replace_scopes_tx：先CAS更新父行版本，再替换scope子表，因此业务父行共享锁覆盖生产范围变更流程。当前schema的Grant有效视图仍供其他接口使用，本轮未改变那些接口的时间或授权契约。SSO沿用INT-14 kind=none的scope判定，不额外引入workspace边界限制。

真实HTTP与一次性PG验证：移除应用允许scope、撤销Grant、用真实repo CAS将Grant降级为application:read，分别暂不提交，启动交换确认原生锁等待再提交；三者均403、码未消费、审计未增加。第四场景把合成Grant有效期压缩为DB时间加3秒，持有码行锁直到到期，返回403且回滚码/审计。

并发新建：先在事务中降级旧Grant，等交换真实阻塞在父行锁后，同事务通过真实repo新建另一枚有效SSO Grant，再提交。正在等锁的请求返回403，不能使用未包含在锁查询快照中的新Grant；同一码重试才锁住并使用新Grant，返回200，审计仅增一。finally恢复旧scope并撤销合成新增Grant。没有mock认证/锁/时间。这些夹具并不等于完整管理端撤销/降级旅程。

最终命令：IMBOY_DEPS_ROOT=/tmp/gz-oa-revoke-deps.1t0g2u2x bash scripts/test/customer_service_internal_http_gate.sh。独立元数据目录复用前一成功运行留存imboy.app（metadata-sha256.txt），只读现有依赖；每次编译当前全部生产源码到独立beams。最终 /tmp/imboy-seat-http.ZmdKzl exit0、八顶层检查通过，42 Internal操作/28路径与Seat/Widget回归保持通过；native CS XML26项，失败/错误/跳过均0。接口清单12/12、erlfmt和diff检查通过。TKRxL3/Ao3lyT/3hxWZU为中间绿灯，最终结果仅绑定最终run及最终源哈希。

人工复核唯一生产交换入口、Grant父行CAS/子scope变更、新记录快照过滤、授权错误与审计回滚，未执行独立agent审查。自有容器由门禁清理，未触碰共享/生产DB或对外OA。其他Internal写面相同保护、所有治理写锁序与审计、真实管理命令/浏览器/原生设备/外部OA会话、真实存储、全局EUnit和完整三端冻结验收仍待闭环，PRODUCTION_READY=NO_GO。未push/部署/生产迁移/第三方通知。
