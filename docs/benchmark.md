# IMBoy 性能基准 / Performance Benchmark

> **状态**: 待执行 — 测试代码已就绪，需人工在目标硬件跑出数据后填写本文档。
> 测试文件：`test/performance/`（7 个文件）

## 如何运行

```bash
cd /Users/leeyi/project/imboy.pub/imboy
IMBOYENV=local make run HTTP_PORT=9800   # 先启动服务
# 另开终端
make eunit SUITE=websocket_performance_tests
make eunit SUITE=channel_perf_benchmark
```

## 结果（待填写）

| 指标 | 目标 | 实测值 | 硬件 | 日期 |
|------|------|--------|------|------|
| WebSocket 并发连接数 | 100,000 | — | — | — |
| 消息吞吐（msg/s） | 50,000 | — | — | — |
| P99 延迟（ms） | < 100 | — | — | — |
| 内存占用（万连接） | < 4GB | — | — | — |
| CPU 使用率（峰值） | < 80% | — | — | — |

## 测试环境模板

```
硬件: ___（CPU 核数 / 内存 / 网卡）
OS: ___
Erlang/OTP: 28.x
PostgreSQL: 18.x
连接数: ___
测试时长: ___ 秒
```

## 说明

README 头条卖点为"单机百万并发"——该数字需本文档实测数据支撑后方可对外声称。
哪怕首次只跑到 1 万连接，有可复现的硬件规格+曲线也比零数据有说服力。
