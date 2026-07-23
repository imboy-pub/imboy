# 集成测试（Integration Testing）

## 为什么需要
评审最痛的教训:mock 掉协议/存储边界使 5 个真生产 bug(离线撤回必崩、群主转让恒失败、解散群吞错报成功等)逃过 404 个单测,仅**真 PG 的 CT** 抓出。集成测试是契约正确性的唯一可信防线。

## 覆盖范围
- 后端:handler→logic→ds→repo 全链路 + 真 PG(所有 `*_repo` 的 SQL、事务边界、缓存穿透、跨模块编排)
- 跨层:消息投递(staging→msg_c2c/s2c→ACK→msg_delivery)、好友/群流、钱包两腿结算
- Flutter:`test/integration/`(sqflite 真库 + service 编排)
- Admin↔后端:API 契约(见 api-testing)

## 推荐框架
- 后端:Common Test(CT,现有 9 suite)负责真 PG 集成;EUnit 负责单元。**明确职责边界:凡触真 DB/真编排走 CT,纯逻辑走 EUnit。**
- Flutter:`flutter test` + 真 sqflite(sqlcipher setDbForTest 注入 ffi)
- 数据库集成配置:`EUNIT_ERL_OPTS` 须自带 `-config` + 两个 `-pa`,`eunit_runner` 加 application:load(评审记忆)

## 目录结构
```
imboy/test/*_SUITE.erl(CT)+ test/integration/
imboyapp/test/integration/
```

## Mock 策略
**最小 mock**:只 mock 真正的外部(推送网关、S3 presign 的远端、第三方支付回调)。DB、编解码、投递管道一律真实。这是集成测试与单元测试的本质分界。

## Fixture 策略
真 PG + 事务隔离:每用例 BEGIN→执行→ROLLBACK,零脏数据残留。CT 用 init_per_testcase/end_per_testcase 管理。播种用工厂函数(用户/好友/群/会话)。Flutter 用真 sqflite + setDbForTest。

## 数据准备
本地 PG(docker imboy_pg18:4323 imboy_v1)迁移到最新 + 幂等播种。CT 每 suite 独立 schema/事务。禁止跨用例共享可变状态(评审:contact 未播种致级联登出)。

## CI 执行方式
Stage 2 集成门,PG18 服务容器(pg_jieba/timescaledb),迁移到 head,每 PR 跑。CT 与覆盖率一起收集。

## 覆盖率要求
Repo 层 80%(SQL 正确性根基),关键投递/结算链路 100% 路径覆盖。

## 验收标准
- [ ] 所有 repo SQL 用真 PG 测(含 epgsql cast 陷阱、参数化)
- [ ] 消息投递/ACK/撤回/离线全链路 CT 覆盖
- [ ] 钱包两腿结算原子性 CT 覆盖(含冻结资金守卫)
- [ ] 事务隔离,零脏数据
- [ ] 5 个历史 bug 全有 CT 回归
