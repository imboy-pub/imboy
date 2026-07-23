# 性能测试（Performance Testing）

## 为什么需要
评审定位 P0 级并发单点(user_server/depcache 单进程横穿热路径)、连接池 sleep 盲重试、投递管道 re-encode。性能测试提供**基线**,让这些重构可量化验证、退化可拦截。已有 `test/performance/` 4 个 benchmark 是起点。

## 覆盖范围
- 后端热路径:消息发送(C2C/C2G)吞吐与延迟、WS 投递、ACK、离线拉取、上下线
- 数据库:关键查询延迟(会话列表/好友/群成员)、连接池行为、慢查询影响
- 单进程瓶颈基线:user_server、depcache、msg_store_worker 的吞吐天花板
- 客户端:启动时间、列表滚动、图片缓存
- 端到端:消息端到端延迟(发送→送达)

## 推荐框架
- 后端:现有 `test/performance/*_benchmark.erl` 与 `*_performance_tests.erl`(后缀并存) + `bench_websocket.sh`;可加 `basho_bench`/自研 harness
- 数据库:`db_query_performance_tests.erl`(现有)+ EXPLAIN ANALYZE 断言
- 客户端:Flutter DevTools timeline、`flutter test` 性能断言
- 指标:延迟 p50/p95/p99、吞吐 msg/s、资源(CPU/mem/进程数)

## 目录结构
```
imboy/test/performance/{channel_perf,channel_ws_push,db_query,msg_send}_benchmark.erl
imboy/scripts/bench_websocket.sh
```

## Mock 策略
最小 mock,尽量真实(真 PG、真连接),否则性能数字无意义。可用合成负载替代真实客户端。

## Fixture 策略
标准负载画像:N 用户、M 群、群规模分布(测 C2G 扇出与群规模的关系)。基线数据集固定,保证可比。

## 数据准备
隔离性能环境(非共享 CI runner,避免噪声);预置规模化数据(万级用户/千人群)。

## CI 执行方式
Nightly/周(非每 PR,太慢);基线存档,对比历史,退化超阈值告警。重构前采基线,重构后对比(PERF-01~08 验收依据)。

## 覆盖率要求
性能测试不用行覆盖率;要求**关键热路径全部有基线**(消息发送/投递/ACK/上下线/关键查询),退化阈值门(如 p99 延迟增长 >20% 告警)。

## 验收标准
- [ ] 消息发送/投递/ACK/上下线有延迟+吞吐基线
- [ ] C2G 扇出性能与群规模关系可测(PERF-03 验收)
- [ ] user_server/depcache 重构前后有对比数据(PERF-01/02)
- [ ] 关键查询有 EXPLAIN 断言,慢查询可发现
- [ ] 基线存档,退化 nightly 告警
