# 数据库测试（Database Testing）

## 为什么需要
120 张表（up.sql 唯一 CREATE TABLE，可复现口径见 database-review）、8 张 TimescaleDB hypertable、钱包资金完整性、epgsql cast 陷阱(评审:$N::float8 对 binary 崩溃吞错返空)。数据库测试保证 SQL 正确、约束有效、事务原子、无注入。

## 覆盖范围
- Repo 层全部 SQL:参数化、cast 类型、返回结构(评审:query 二元组陷阱)
- 约束:CHECK(钱包 frozen<=balance)、唯一键(幂等,含 hypertable msg_id 去重键含 created_at 缺陷)、外键 CASCADE(频道/朋友圈域)
- 事务:钱包两腿结算 with_tx、recharge 单事务、SKIP LOCKED 消息持久化
- 注入面:elib_pg_sql raw 逃生门、标识符拼接
- TimescaleDB:hypertable 分区、生命周期(队列/timeline/msg_store)、create_hypertable 版本耦合

## 推荐框架
CT + 真 PG(docker imboy_pg18)。schema 断言用 information_schema 查询。可用 MCP `local_imboy`(只 SELECT)辅助验证 schema。

## 目录结构
```
imboy/test/repo/*_repo_tests.erl(升级为 CT 真 PG)
imboy/test/migration/(见 migration-testing)
```

## Mock 策略
**零 mock**——数据库测试的全部价值在于真 PG。用真实 epgsql 连接,真实类型编码(暴露 cast 陷阱)。

## Fixture 策略
事务回滚隔离;每用例独立播种。约束测试用"违约输入应被拒"(冻结态借记应拒、frozen>balance 应被 CHECK 拦)。cast 测试用真实 binary/float 参数暴露编码路径。

## 数据准备
迁移到 head 的真 PG;工厂造钱包/订单/消息/群成员。TimescaleDB 测试需真 hypertable(不能用普通表 mock)。

## CI 执行方式
Stage 2,PG18 + timescaledb 服务容器,迁移到 head,每 PR。

## 覆盖率要求
Repo 80%;资金/结算路径 100%;所有 CHECK/唯一约束有"违约被拒"测试。

## 验收标准
- [ ] 全 repo SQL 真 PG 测(cast/参数化/返回结构)
- [ ] 钱包 frozen/status 守卫 + 表级 CHECK 有测试
- [ ] hypertable 去重键、生命周期有测试
- [ ] raw SQL 逃生门有注入防护断言
- [ ] epgsql cast 陷阱有回归(binary 经纬度不再吞错返空)
