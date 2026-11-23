# R0 容量探针证据（阶段 B，只读核验）

隔离的裸 cowboy 微实验，用于取得设计文档 §1 / §3 的实测数字。**不涉及任何仓库文件、配置或运行中的节点。**

## 环境

| 项 | 值 |
|---|---|
| 日期 | 2026-11-23（本地时间 19:07–19:32，Asia/Shanghai） |
| 机器 | macOS darwin 27.2.0 arm64，12 逻辑核，36 GB |
| OTP / ERTS | 29.0.2 / 17.0.2（`/opt/homebrew/Cellar/erlang/29.0.2`） |
| cowboy / ranch / gun | 2.19.0 / 2.3.0 / 2.6.0（复用主检出 `imboy/deps/*/ebin` 已编译 beam，只读） |
| fd 限额 | soft 1048575；`kern.maxfiles 65536`、`kern.maxfilesperproc 65536` |
| 网络 | 仅 127.0.0.1，每轮随机空闲端口 |

## 文件

| 文件 | 说明 |
|---|---|
| `src/ws_probe.erl` | WS handler，流级选项逐键复刻 `websocket_handler:websocket_options/0`（含两个 `infinity`） |
| `src/stats_handler.erl` | `GET /stats`：process/port count 与 limit、ranch 有效值、spawn 探针、按 `$initial_call` 的进程构成；`GET /stats/stop` 结束探针节点 |
| `src/probe_srv.erl` | 探针服务端：`imboy` 形态或显式 `cap=,sups=,acc=` 形态启动 listener；打印有效 TransportOpts |
| `src/probe_client.erl` | 探针客户端：**独立节点**（不污染服务端计数），分批建连 → 统计 → 满载 ping → 释放配额 → 拆除 |
| `src/probe_debug.erl` | 一次性排障用（在升级完成前就发帧，属已知瑕疵），仅用于暴露 gun 2.6 的 5 元组 `gun_upgrade` 消息格式 |
| `src/run_probe.sh` | 编排脚本：起服务端 → 起客户端 → 停服务端 |
| `runs/*.log` | 4 轮关键运行的报告行（每轮头部记录命令、时间、退出码） |

重跑：`erlc -o ebin -pa <deps>/cowlib/ebin -pa <deps>/ranch/ebin -pa <deps>/cowboy/ebin src/*.erl` 后
`bash run_probe.sh <name> <shape> <N> <phase1_timeout_ms> <close_k> <phase2_wait_ms> "<erl flag>" [trickle_n] [trickle_gap_ms]`。

## 结果

| 运行 | 形态 / 标志 | 尝试 | 建立 | 被拒 | 关键读数 |
|---|---|---|---|---|---|
| `cap300-192159` | `cap=100,sups=3,acc=3`（名义上限 300） | 380 | **380** | 0 | 已升级 WS 不计入 `max_connections`；每连接 1 进程 / 1 端口；满载 ping 20/20 |
| `pcliff-192625` | 同上 + `+P 1024` | 1100 | 937 | 163 | `process_count` 触顶；服务端 2,378 行 `system_limit` 错误；拆除后回落到 86 |
| `pcliff2-192829` | 同上 + `+P 1024` + 滴灌 100 | 1100 | 940 | 160 | 满表期 **HTTP API 20/20 → 500**；已建立 WS ping 20/20；拆除后恢复 |
| `fullscale-192949` | `imboy` 形态 + `+P 20480` | 21000 | **16,201** | 4,799（`connect_failed`，客户端/OS 侧） | 实际 `process_limit` = **32768**；超名义上限 10,240 仍被接受；内存 193 MB @16.2k |

## 标志与优先级速测（内联命令，均 OTP 29.0.2）

| 输入 | 结果 |
|---|---|
| `+P 286` / `+P 1023` | 拒绝启动（最小 1024） |
| `+P 1024` / `+P 1025` / `+P 5000` / `+P 20480` | 1024 / **2048** / **8192** / **32768**（向上取整到 2 的幂） |
| `+Q 20480` / `+Q 65536` / `+Q 65537` | 32768 / 65536 / 131072 |
| 无 `+Q`，fd soft=4096 / 1048575 | port_limit 65536 / 1048576（缺省随 fd 上限上调） |
| `ERL_MAX_PORTS=777 / 3000 / 9000 / 20000` | 1024 / 4096 / 16384 / 32768（生效但未文档化：`erl_init.c:1335`） |
| `ERL_MAX_PORTS=777` + args_file `+Q 4096` | 4096（显式 `+Q` 覆盖） |
| `ERL_MAX_PROCESSES=5000` | 无效（变量不存在） |
| args_file + `ERL_FLAGS="+Q 8192 +P 16384"` | 16384 / 8192（**ERL_FLAGS 覆盖 args_file**） |
| args_file + `ERL_AFLAGS=…` | 8192 / 4096（被 args_file 覆盖） |
| `ERL_AFLAGS=+Q 8192` + `ERL_FLAGS=+Q 2048` | 2048 |

## 限制

- 服务端为裸 cowboy：真实 imboy 的空闲基线（进程/端口）、握手期 DB 成本未测，留待阶段 F。
- 本机单客户端节点并发上限约 16.2k（`fullscale` 的 4,799 条 `connect_failed` 为客户端/OS 侧限制，服务端当时仅用 16.3k/32768）→ 阶段 F 的 >16k 档需多客户端节点或 Linux 压测机。
- 原始完整日志在 `/tmp/r0-capacity-probe-20261123/run/`（易失目录），本目录只保留报告行。
