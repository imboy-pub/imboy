# 钱包可用余额约束：生产规模隔离克隆验收

本手册用于关闭 `W0-SEC-02` 的最后一个外部 Gate。它验证迁移 65/66 在与生产规模
相当的数据上是否满足历史数据、锁等待和执行时延要求。

> 这里的“克隆”必须是从生产备份恢复出的、与线上完全隔离的可写数据库，不能是仍与
> 主库复制的只读副本。脚本会执行 DDL，但不会自动修正任何钱包余额或冻结金额。

## 1. 通过标准

- PostgreSQL 必须为 18.x。
- 数据库名必须以 `_wallet_acceptance` 结尾，数据库注释必须精确等于
  `IMBOY_WALLET_CONSTRAINT_ACCEPTANCE_CLONE`。
- 数据库必须是非恢复态、可写，且没有其他会话。
- 脚本记录 PostgreSQL `system_identifier`，并在每个会修改状态的数据库会话中重新核对
  集群身份、数据库名、克隆标记、版本、读写状态和迁移基线；DNS、负载均衡或故障切换
  使目标变化时必须停止。
- 65 创建约束时在提交前取得 OID；66 验证、66 down 重建与 65 down 删除都在同一事务
  取得表锁并核对该 OID，拒绝操作在间隙被其他会话替换的同名对象。
- `schema_migrations` 必须是 `64:false`，目标约束尚不存在。
- `wallet` 行数达到本次书面声明的最小规模；`frozen > balance` 必须为 0。
- 65 在并发 `ROW EXCLUSIVE` 下于 500ms 内以 SQLSTATE `55P03` 停止等待；释放锁后
  能添加 `NOT VALID` 约束。
- 66 在相同并发写锁仍存活时，于声明的 `statement_timeout` 内完成验证。
- 已验证约束对事务内违规 UPDATE 返回 SQLSTATE `23514`，连接退出后无数据残留。
- `66 down → 65 down` 后约束、钱包聚合指纹和迁移版本都恢复到验收前状态。

任一项失败即为 `NO-GO`。发现历史违规时必须人工对账，禁止在 schema migration 中
自动改动真钱数据。

## 2. 准备隔离克隆

1. 使用生产备份恢复一个独立数据库，确保应用、定时任务和复制链路都不会连接它。
2. 数据库命名示例：`imboy_20260817_wallet_acceptance`。
3. 由数据库管理员在该克隆上设置一次性标记：

```sql
COMMENT ON DATABASE imboy_20260817_wallet_acceptance
IS 'IMBOY_WALLET_CONSTRAINT_ACCEPTANCE_CLONE';
```

4. 使用 `.pgpass`、临时 `PGPASSFILE` 或临时 `PGPASSWORD` 提供认证；不要把密码写入
   仓库、命令参数、验收记录或聊天内容。

## 3. 先执行只读预检

最小行数必须来自本次克隆规模的书面预期，不能为了让脚本通过而临时降低。

```bash
export PGHOST='<clone-host>'
export PGPORT='5432'
export PGDATABASE='imboy_20260817_wallet_acceptance'
export PGUSER='imboy_acceptance'
export WALLET_ACCEPTANCE_MIN_ROWS='<expected-min-wallet-rows>'

bash scripts/verify_wallet_constraint_clone.sh --precheck
```

预检只执行 SELECT。出现版本不符、标记缺失、其他会话、迁移状态不符、规模不足或历史
违规时立即停止，不得继续执行模式。

## 4. 执行正反迁移与锁演练

选择超时阈值时应参考发布维护窗口和相同硬件上的基线。例如 60 秒：

```bash
export WALLET_ACCEPTANCE_VALIDATE_TIMEOUT_MS='60000'
export WALLET_ACCEPTANCE_APPLY='YES_ISOLATED_CLONE'

bash scripts/verify_wallet_constraint_clone.sh --execute \
  | tee '/approved/evidence/wallet-constraint-clone-acceptance.log'
```

日志会包含行数、表占用字节数、psql 的实际执行时间、SQLSTATE 与每个断言结果，但不
包含连接密码。证据文件必须写入团队批准的位置，不要提交含内部主机名的日志。

异常退出时，脚本只会按 PostgreSQL OID 删除本次运行已确认创建的约束，并给恢复操作
设置有界锁等待与语句超时。OID 不一致、身份复核失败或无法确认恢复完成时会输出
`CLEANUP-FAIL`；此时该克隆必须记为 `NO-GO` 并由 DBA 人工核对，不能再次运行脚本来
掩盖现场。

## 5. 验收记录

```text
Gate: W0-SEC-02 production-sized isolated clone
Result: GO | NO-GO
Snapshot time:
Source release/commit:
PostgreSQL version:
wallet rows:
wallet relation bytes:
Declared minimum rows:
Declared VALIDATE timeout:
65 ADD elapsed / SQLSTATE under lock:
66 VALIDATE elapsed / concurrent holder survived:
23514 proof:
Down sequence and wallet fingerprint:
Evidence location:
Operator:
Reviewer:
Notes:
```

只有记录为 `GO`、证据完整且经第二人复核后，才能把 `W0-SEC-02` 从 `in_progress`
改为 `done`。脚本存在、本地 mock 或合成 PostgreSQL 通过都不等于该外部 Gate 已完成。
