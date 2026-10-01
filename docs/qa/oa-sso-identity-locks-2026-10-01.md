# OA SSO 与身份撤销并发边界

基线 7073aabb8c084d45c66daa87111a4e320181b31b。配套 evidence/source-sha256.txt 绑定四个业务/验证入口。仅证明 OA-02 / INT-B01 的身份撤销子场景，未将整个验收 ID 标记完成。

根因：交换事务消费码后通过无锁 JOIN 读取 active 映射、成员和正常 Human 账号。当映射撤销事务已更新行但尚未提交时，读取仍可见旧 active 快照，真实交换返回200。基线HTTP检查因此失败。

修复：现有身份 JOIN 增加 FOR SHARE OF om, u, eei，使成员、账号及映射的有效性保持到消费/审计事务提交；撤销先取更新锁时，查询等待提交并按新版本重新检查条件。沿用已有失败统一回滚，422不消费码。没有迁移、新依赖、全局锁或新增协议。

真实HTTP + PostgreSQL：签发真实交换码，合成夹具分别在独立事务更新映射为removed、成员为suspended、账号status为0且暂不提交；启动真实交换请求，观察原生锁等待后提交夹具事务。三者均返回422/identity_not_mapped、consumed_at仍null、成功交换审计数不增加。每个场景finally恢复合成状态。该夹具直接操纵隔离数据库事实，不等于实际管理端撤销/离岗命令全旅程。

最初测试启动因共享 ebin/imboy.app 已被清理而失败，没有执行产品验证。后续用 /tmp/gz-oa-revoke-deps.1t0g2u2x 独立元数据目录，复用上一成功运行留存的应用元数据（metadata-sha256.txt），依赖只读指向现有已安装依赖；门禁仍编译当前全部生产源码到新的独立beams目录，不复用旧产品beam，不修改共享构建目录。

命令：IMBOY_DEPS_ROOT=/tmp/gz-oa-revoke-deps.1t0g2u2x bash scripts/test/customer_service_internal_http_gate.sh。基线 /tmp/imboy-seat-http.Rvxtq2 exit1（实际200）；最终 /tmp/imboy-seat-http.mzqWXd exit0、八顶层检查通过，42 Internal操作/28路径以及现有Seat/Widget回归保持通过。native CS XML26项，失败/错误/跳过均0。接口清单12/12、erlfmt及diff检查通过。所有响应和记录均不含交换码/nonce/凭证明文，只有合成用户标识。自有容器由门禁清理。

人工复核共享入口、调用方、身份触发器及成员/映射现有写路径；没有独立agent审查。这里只证明三个撤销先取锁场景，不证明所有治理写锁序、应用/凭证撤销、真实管理命令到OA外部会话注销、原生设备或真实存储。全局EUnit及完整六目标冻结验收仍待完成，PRODUCTION_READY=NO_GO。没有部署/生产迁移/第三方通知。
