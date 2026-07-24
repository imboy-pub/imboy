# 迁移 / 升级 / 回滚测试（Migration / Upgrade / Rollback Testing）

## 为什么需要
schema 演进是数据完整性最高风险动作。评审:迁移体系良好偏优(strict 乱序 + advisory lock + 单文件事务 + down 全覆盖),但有 41 空号 renumber、history 回填无 ON CONFLICT、旧环境映射靠手工、create_hypertable 版本耦合、Flutter v23 降级静默 success。升级/回滚必须可测可演练,不能上生产才发现。

## 覆盖范围
- 前滚:每条迁移 up 成功、幂等(重跑不炸)、strict 乱序检测生效
- 回滚:每条 down 可逆、数据不丢
- 升级:跨版本(旧 schema → head)、灰度双写、双读兼容(加列双写→读切→停旧写)
- 兼容:旧客户端对新 schema(不破坏兼容宪法)
- 边界:advisory lock 并发迁移、单文件事务原子、history 差步(schema_migrations 映射)
- TimescaleDB:hypertable 迁移、生命周期链
- Flutter 本地库:embedded 常量 = 单一真源,降级脚本完整、无脚本降级显式失败

## 推荐框架
- 后端:erlang_migrate(strict 模式)+ CT(真 PG 跑全迁移链)
- 库自测:`erlang_migrate/` 单测(乱序检测、多库、fmt Unicode)
- Flutter:migration_service 测试 + DDL 生成校验(ENG-02)
- 校验脚本:`check_out_of_order`、`validate_p5_manifest`

## 目录结构
```
imboy/priv/migrations/*.sql(含事故复盘注释)
imboy/test/migration/(全链 up/down CT)
erlang_migrate/test/
imboyapp/test/(migration_service_test)
```

## Mock 策略
零 mock——迁移测试全部用真 PG。版本矩阵测试用真实历史 schema 快照。

## Fixture 策略
schema 快照:各历史版本的 schema dump 作 fixture,测"从 vN 升到 head";数据 fixture 验证升级不丢数据、双写一致。

## 数据准备
干净 PG 跑全链 up→down→up 幂等;带数据 PG 测升级保数据;旧版本快照测跨版本升级。

## CI 执行方式
每 PR:全迁移链 up + 幂等重跑 + 全 down(Stage 2 真 PG)。strict 乱序检测 + check_out_of_order 门。Flutter DDL 一致性门(ENG-02)。

## 覆盖率要求
每条迁移有 up+down+幂等测试;跨版本升级矩阵覆盖近 N-2 版本。

## 验收标准
- [ ] 全迁移链 up/down/幂等在 CI 真 PG 跑
- [ ] strict 乱序 + advisory lock + 单事务有测试
- [ ] 跨版本升级 + 双写双读兼容有测试
- [ ] 回滚不丢数据(带数据演练)
- [ ] Flutter DDL 单一真源 + 降级失败显式
- [ ] history 回填幂等(ON CONFLICT)
