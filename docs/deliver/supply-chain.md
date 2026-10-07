# 依赖与许可证清单（AC27 供应链）

> 状态：current | 依据：Makefile `DEPS`/`BUILD_DEPS`/`TEST_DEPS` + erlang.mk 锁定机制
> 免责：许可证按生态公知版本标注，`unknown` 表示需人工核实后再对外承诺；
> 精确版本以 `deps/` 目录与构建日志为准（Q12 冻结时随 AC30 一并记录）。

## 运行时依赖（Erlang/OTP 28+，erlang.mk 构建）

| 依赖 | 用途 | 许可证 |
|---|---|---|
| ranch / cowlib / cowboy / gun | HTTP/WS 服务端与客户端 | Apache-2.0 |
| erlware_commons | 通用工具库 | Apache-2.0 |
| jose | JWT/JWS/JWE | MIT |
| gen_smtp | 邮件发送 | MIT |
| throttle | 限流 | MIT |
| goldrush | lager 传递依赖（显式钉版） | MIT |
| jsone | JSON 编解码 | MIT |
| epgsql | PostgreSQL 驱动 | BSD-3-Clause |
| pooler | 连接池 | Apache-2.0 |
| erlang_migrate | DB 迁移（同工作区自有库） | 自有（Apache-2.0 口径，见仓内 LICENSE） |
| depcache | 缓存 | MIT |
| syn | 进程注册/发现 | MIT |
| ecron | 定时任务 | MIT |
| uid | ID 生成 | MIT |
| telemetry | 遥测接口 | Apache-2.0 |
| lager | 日志 | Apache-2.0 |
| observer_cli | 观察终端 | MIT |
| recon / redbug | 诊断 | Apache-2.0 / unknown（需核实） |
| opentelemetry_api / opentelemetry / opentelemetry_exporter | OpenTelemetry | Apache-2.0 |
| grpcbox | gRPC（OTLP 上游） | Apache-2.0 |
| tls_certificate_check / ssl_verify_fun / chatterbox | TLS 校验/HTTP2 | Apache-2.0 / MIT / unknown（需核实） |
| ctx / acceptor_pool / gproc | OT 传递依赖 | MIT / MIT / LGPL（需核实动态链接方式） |
| simple_captcha | 验证码 | MIT |
| erlydtl | 模板 | MIT |
| erlang_pay | 支付网关（同工作区自有库：支付宝/微信/Stripe 回调验签） | 自有（见仓内 LICENSE） |
| sync | **仅本地开发**热重载（`IMBOYENV=local` 才挂载，生产 `make rel` 不含） | MIT |

## 构建/测试期依赖（不进交付运行时）

| 依赖 | 用途 | 许可证 |
|---|---|---|
| relx | release 打包 | Apache-2.0 |
| gpb / bbmustache | protobuf 生成 / 模板 | LGPL / MIT |
| meck | 测试 mock | Apache-2.0 |
| edown | 文档 | BSD-3-Clause |
| xref/dialyzer（OTP 自带） | 静态检查 | ErlPL |

## 系统/平台依赖（交付环境）

- Erlang/OTP 28+、PostgreSQL 18+（扩展：pg_jieba、postgis、timescaledb、pgcrypto）
- Garage S3（对象存储，可选部署）、Docker Compose（deploy/）
- 各扩展自身许可随发行版携带；商用交付前建议跑一次 `scripts/deliver/` 附带的
  `make_delivery_pack.sh` 指纹校验并补齐此处 `unknown` 项。

## 核实待办（交付 owner 复核期限：Q12 冻结前）

1. redbug、chatterbox、acceptor_pool 许可证实测（deps/<name>/LICENSE 逐个确认）。
2. gproc LGPL 与静态/动态链接方式确认（Erlang NIF/端口语境下的合规口径）。
3. opentelemetry 传递依赖完整闭包（以构建日志 deps 树为准）。
