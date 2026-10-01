# OA SSO 等锁跨截止时间与并发单次消费

源码基线：b83a6d715e85ce5274c096852671eaedd578bc97。四个最终业务/验证入口文件以配套 evidence/source-sha256.txt 绑定。对应 OA-02 / INT-B01 子场景，不将整个验收 ID 标记完成。

根因：repo 用 CURRENT_TIMESTAMP（事务开始时间）做过期检查与 CAS，并用宿主 elib_dt:now 记录消费。HTTP 请求已开始、等待码行锁到过期后仍能消费，基线真实响应200且写出旧消费时间。修复在同事务先 SELECT FOR UPDATE 取码行锁，再用 PostgreSQL clock_timestamp() 判断有效期及记录 consumed_at；lookup 的 expired 同样用 <= 当前数据库时间。单次 CAS/绑定校验/错误信封/审计事务及60秒公共 TTL 保持现有合同，没有迁移和新 API。

真实隔离 DB + 完整生产 Cowboy 路由/中间件/认证/logic/repo：真实签发码，合成夹具仅将 expires_at 在请求前设为三秒以压缩等待，不 mock 时钟/认证/消费。先持有码行锁，确认 HTTP 查询被真实锁阻塞，数据库时钟自然越过截止时间后释放：最终404 resource_not_found、consumed_at仍null、审计数不增加。另一枚正常TTL码的两个实际HTTP请求都被阻塞后释放，结果200/404、仅一次消费且审计增一。没有交换码/nonce/凭证明文进入留存响应。

回归 oracle 修正有留存记录：初版错读错误信封根层（实际error.code）；pg_stat_activity 活动列在事务中缓存，需 pg_stat_clear_snapshot 刷新；排队的第二个消费者会阻塞在第一个消费者后面，不能只统计直接被主锁持有者阻塞的进程。最终要求码表两个实时锁等待者，未降低为请求数量/串行模拟。失败到恢复依次记录在三份 oracle 日志；生产修复始终保持同一版。

最终命令 IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh，exit0，run /tmp/imboy-seat-http.PY25Fc：八顶层检查通过；现有42 Internal操作/28路径、Seat及Widget HTTP/SSE组合回归仍通过；native CS XML为26项，failures/errors/skipped=0。额外 manifest 12/12，erlfmt --check 与 diff --check 通过；没有完整全局EUnit/真机/真实OA声明。测试只使用自有一次性数据库/合成租户，容器终止后删除，未触碰现有数据库或对外OA。对象存储/DNS替身沿用既有门禁，其真实存储验证仍待完成。

人工复核全部消费调用方、取锁/时间判断/审计边界，未执行独立 agent 审查。这里只证明过期等待和并发单次消费；成员/映射撤销与交换提交的所有竞态、真实 Human HTTP签发到外部OA、Cookie/原生设备、完整六目标冻结验收继续待闭环，PRODUCTION_READY=NO_GO。
