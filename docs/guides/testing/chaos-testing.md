# 混沌 / 恢复测试（Chaos / Recovery Testing）

## 为什么需要
SRE 核心:IM 的价值承诺是"消息不丢、服务不断"。评审暴露多个故障放大点——集群 syn 远端 Pid 崩溃、imboy_cache 返回 self() 崩溃不自愈、listener 先于监督树启动、msg_store_worker 无效阻塞记录、连接池雪崩。这些只有主动注入故障才能发现。不做 chaos 就是把故障演练留给生产事故。

## 覆盖范围
- 进程故障:杀 worker(user_server/depcache/msg_store_worker/plugin)验证监督树自愈
- 集群故障:节点宕机、网络分区(syn 跨节点投递、远端 Pid 崩溃 P1-C1)、脑裂
- 依赖故障:PG 断连/慢/连接池耗尽、Garage S3 不可用、LiveKit 不可达、推送网关超时
  （无 Redis 场景——项目级约束:全栈不引入 Redis,缓存走进程内 depcache）
- 数据故障:无效阻塞记录、迁移中断、磁盘满
- 恢复:重启后消息不丢(staging→持久化)、重连后离线拉取、缓存崩溃重建
- 发布故障:蓝绿切换失败回滚、滚动发布可用性窗口(listener 时序)

## 推荐框架
- 进程级:Erlang 原生(`exit(Pid, kill)` + 监督树断言),CT 编排
- 系统级:容器编排注入(docker pause/kill/netem 延迟丢包)、Toxiproxy(网络故障)、Chaos Mesh(k8s,若上 helm)
- 恢复验证:故障后自动跑消息完整性检查(发 N 条,故障,验收 N 条不丢不重)

## 目录结构
```
imboy/test/chaos/(进程/集群故障注入 CT，**待建**)
imboy/deploy/(容器级 chaos 剧本)
imboy/scripts/(recovery 验证脚本,复用 smoke)
```

## Mock 策略
不 mock 故障——真实注入(真杀进程、真断网、真满盘)。被测系统全真实。

## Fixture 策略
故障剧本库:每种故障(kill/partition/dep-down)+ 预期恢复行为(SLA:恢复时间、零丢失)作 fixture。稳态假设(steady-state hypothesis)先定义再注入。

## 数据准备
隔离 chaos 环境(接近生产的集群 + 依赖栈);消息完整性探针(注入前后对账)。

## CI 执行方式
**不进 PR 门**;季度/月 game day 演练 + nightly 轻量进程级 chaos。每次演练记录:注入什么、预期、实际、恢复时间、发现的缺陷。

## 覆盖率要求
无行覆盖;要求覆盖所有已知故障放大点(监督树盲区、集群 syn、无效阻塞记录、连接池雪崩、发布回滚),每个有稳态假设 + 恢复 SLA。

## 验收标准
- [ ] 杀任一 worker,监督树自愈,消息不丢(暴露 imboy_cache self() 缺陷)
- [ ] 集群节点宕机/分区,消息投递正确恢复(P1-C1 验收)
- [ ] PG/S3/LiveKit 故障,系统降级不雪崩,恢复后一致
- [ ] 无效阻塞记录不阻塞整批持久化
- [ ] 蓝绿回滚 + 滚动发布可用性演练有记录
- [ ] 季度 game day 常态化,缺陷回流为回归测试
