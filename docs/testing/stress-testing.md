# 压力测试（Stress Testing）

## 为什么需要
IM 面临重连风暴、大群扇出、消息洪峰。评审:user_server 单进程"重连风暴必积压"、C2G 同步扇出、连接池耗尽盲重试。压力测试验证系统在极限负载下的行为(降级而非雪崩),并找到容量上限。已有 `test/stress/` 2 个 stress 测试。

## 覆盖范围
- 连接风暴:海量并发 WS 建连/重连(暴露 user_server 积压)
- 消息洪峰:高频 C2C/C2G 发送,验证 QoS 与背压
- 大群扇出:千人/万人群发送(C2G O(N) 扇出上限)
- 连接池:并发查询压满 80 连接池(暴露 sleep 重试放大)
- 群成员上限:群规模边界(现有 group_member_limit_stress)
- 持久化:msg_store_worker 批写吞吐上限
- 资源耗尽:进程/内存/ETS 增长,GC 行为

## 推荐框架
- 后端:现有 `test/stress/{high_concurrency,group_member_limit}_stress_tests.erl` + 分布式负载生成器(多节点打流)
- WS 压测:`bench_websocket.sh` 扩展为并发 N 连接
- 指标:成功率、延迟退化曲线、错误率、恢复时间

## 目录结构
```
imboy/test/stress/{high_concurrency,group_member_limit}_stress_tests.erl
imboy/scripts/bench_websocket.sh(并发扩展)
```

## Mock 策略
合成客户端(不用真 app)生成负载;后端全真实。负载生成器与被测系统分离部署。

## Fixture 策略
可调负载画像(连接数/消息率/群规模阶梯递增),找拐点。稳态 + 尖峰两种模式。

## 数据准备
隔离压测集群(接近生产配置);预置规模化数据;负载分布贴近真实(长尾群规模)。

## CI 执行方式
周/发布前(非每 PR);阶梯加压找容量拐点;记录容量基线(如"单节点支持 X 并发连接、Y msg/s")。规模化重构(P2)的验收依据。

## 覆盖率要求
无行覆盖;要求覆盖所有已知瓶颈的极限行为(连接风暴/大群/池耗尽/洪峰),并给出容量数字。

## 验收标准
- [ ] 连接风暴下系统降级不雪崩,给出并发上限
- [ ] 大群扇出容量曲线(群规模 vs 延迟)
- [ ] 连接池压满行为可测(sleep 重试放大验证,PERF-05 前后对比)
- [ ] 消息洪峰下 QoS 保证(不丢消息)
- [ ] 容量基线存档,指导集群扩容
