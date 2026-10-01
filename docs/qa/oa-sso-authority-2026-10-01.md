# OA SSO 交换事务的组织、应用及凭证权限复核

基线：16e87647b8b2bf7981f485440a76c82e7287bc48。evidence/source-sha256.txt 绑定最终五个入口文件；本记录覆盖 OA-02 / INT-B01 的权限生命周期子场景，不将整个验收 ID 或六项需求标为完成。

根因：中间件认证事务先结束，交换业务只使用旧 context。当应用停用事务已更新但未提交时，认证仍见旧 active 状态；业务未锁定或重新检查应用，真实HTTP返回200。基线 /tmp/imboy-seat-http.6f9cI3 因预期403而失败。

修复：生产 handler 的唯一池化交换入口 exchange/2 在业务事务中按 context 的组织/应用/凭证 ID 查询并持有共享行锁，复核凭证状态、过期、应用状态和组织状态后调用既有交换事务核心。有效期在取得权限锁后由 PostgreSQL clock_timestamp() 读取，成功前再次复核，失败整体回滚码消费和审计。错误顺序保持凭证状态→凭证有效期→应用→组织。既有 exchange_tx/3 保留为受信任事务核心/直连测试接口；生产调用只从 exchange/2 进入。没有再次传递或保存凭证明文、没有新schema/API/依赖。

认证的 last_used_at 原本就是尽力记录、不影响授权结果。改用原生 FOR NO KEY UPDATE SKIP LOCKED，遇权限或撤销行锁时跳过，避免一枚凭证的慢交换阻塞其他请求认证。原返回契约保留，调用方仍忽略记录失败；该时间字段不承诺完整请求计数。

实际生产HTTP链及自有一次性PG：签发码后分别暂不提交应用disabled、组织archived、凭证revoked事实，启动HTTP并确认原生锁等待，再提交。返回403/application_disabled、403/organization_disabled、401/invalid_credential，码保持未消费、成功交换审计不增加。另把合成凭证有效期设为数据库当前时间加3秒，持有码行锁直到自然过期，交换返回401/credential_expired，消费和审计均回滚。组合场景等待应用停用锁跨过凭证截止时间，仍优先返回401/credential_expired。原有码截止时间、两个消费者单次成功、审计软失败重试及三个身份撤销检查仍通过。

这些撤销事实由合成夹具直接更新隔离数据库，不等于真实管理命令/UI完整旅程。每个夹具finally恢复状态，所有容器由门禁清理，未触碰共享或生产数据库。

最终命令：IMBOY_DEPS_ROOT=/tmp/gz-oa-revoke-deps.1t0g2u2x bash scripts/test/customer_service_internal_http_gate.sh。独立元数据目录复用前一成功运行留存的imboy.app（哈希见metadata-sha256.txt），只读现有依赖，所有生产源码重新编译到当次beams，不复用旧产品beam。最终 /tmp/imboy-seat-http.GERH9e exit0：八顶层检查通过，42 Internal操作/28路径及Seat/Widget回归保持通过；native CS XML26项，失败/错误/跳过均0。接口清单12/12、erlfmt及diff检查通过。两次中间回归PPXTIg/h9Kmgl也通过，但最终源码只以上述最终run绑定。

人工复核共享入口、唯一生产caller、凭证repo与last_used调用方、锁/时间检查和rollback边界，没有独立agent审查。这里仅证明OA交换的权限状态边界，未为其他Internal业务增加相同保护。Grant/allowed_scopes变更竞态、所有治理写锁序、真实管理撤销到外部OA会话注销、真实存储、原生设备、全局EUnit及三端完整冻结验收继续待闭环，PRODUCTION_READY=NO_GO。未push/部署/生产迁移/第三方通知。
