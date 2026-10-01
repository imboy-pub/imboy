# OA SSO 审计失败回滚与同码重试

基线：baecf5aecc142a687378684764ded8989216847e。源文件由配套 evidence/source-sha256.txt 绑定。本记录仅覆盖 OA-02 / INT-B01 的审计原子性子场景，不表示整个验收 ID 或六项需求完成。

根因：exchange/2 只为 identity_not_mapped 强制回滚。epgsql 的事务函数对普通返回值（包括 error tuple）仍 COMMIT；审计 INSERT RETURNING 无行返回时，repo 明确返回 audit_not_inserted，exchange_tx 返回 internal_error，但消费状态已提交。基线实际 HTTP500，数据库 consumed_at 非空，违反 REQUIRED_AUDIT 契约。

修复：共享 exchange/2 对所有错误发出既有 rollback 信号，使消费、身份解析、审计统一回滚。保持错误码、60秒 TTL、绑定校验和成功响应协议，无迁移或新增依赖。日志只含阶段和稳定错误码，不含交换码、nonce 或凭证。

验证：自有一次性 PostgreSQL、合成租户及完整生产 HTTP 路由/中间件/认证/logic/repo。测试在真实审计表临时加入 BEFORE INSERT 触发器，仅对 oa.sso.exchanged RETURN NULL，造成事务未被 PostgreSQL 自动中止的软失败；不是 SQL 异常或 mock。失败返回500/internal_error、consumed_at仍null、审计数不变。finally 删除测试触发器后，同一码重试200、消费非空、审计增一，再次重放404。触发器只存在于隔离测试数据库。

命令：IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh。基线 exit1；最终 /tmp/imboy-seat-http.jVowIV exit0，八顶层检查通过，42 Internal操作/28路径及现有Seat/Widget组合回归保持通过；native CS XML26项、失败/错误/跳过均0。接口清单12/12、erlfmt及git diff检查通过。证据含基线失败、最终日志、脱敏响应、XML及文件哈希。

人工复核共享入口、唯一生产 handler 调用方、事务驱动及连接回收、测试触发器清理，没有独立 agent 审查。没有使用已有数据库或真实OA、没有生产迁移/部署/通知。成员与映射撤销竞态、原生WebView与真实OA端到端、真实存储、全局EUnit及完整冻结验收仍待闭环，PRODUCTION_READY=NO_GO。
