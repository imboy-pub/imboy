<!-- Generated: 2026-04-17 | Erlang deps: 40 | Token estimate: ~600 -->

# Dependencies Codemap / 依赖图谱

> 双语 / Bilingual. 所有依赖通过 `erlang.mk` + `Makefile` 管理，锁定在 `rebar.lock`。

## Runtime / 运行依赖

| Pkg | Purpose / 用途 |
|---|---|
| `cowboy`, `cowlib`, `ranch` | HTTP/WS 服务器（2.10） |
| `epgsql`, `pooler` | PostgreSQL 客户端 + 连接池 |
| `pure_migrations` | 数据库迁移（imboy_migrate） |
| `depcache` | 内存缓存（imboy_cache） |
| `syn` | 分布式进程注册 / pubsub |
| `lager`, `goldrush` | 结构化日志 |
| `jsone`, `jsx` | JSON 编解码（多协议兼容） |
| `jwerl` | JWT 签发 / 校验（auth_middleware） |
| `gun` | 出站 HTTP/2 客户端（jPush、OSS、webhook） |
| `gen_smtp` | 邮件发送（elib_email） |
| `qdate`, `qdate_localtime` | 日期 / 时区 |
| `gpb` + `imboy.proto` | Protobuf 编解码（WS 消息） |
| `aho_corasick` | 敏感词扫描 |
| `bbmustache`, `erlydtl` | 模板（admin / 邮件） |
| `simple_captcha` | 验证码 |
| `throttle`, `fuse` | 限流 / 熔断 |
| `ecron` | 定时任务调度 |
| `telemetry` | 指标埋点 → `metrics_handler` |
| `uid` | 短 ID 生成（非主键场景） |
| `cf`, `datum`, `erlware_commons` | 函数式工具 |
| `redbug`, `recon`, `observer_cli` | 在线诊断 / 远程 trace |
| `qianfan_api` | 百度文心 LLM SDK（占位/可选） |

## Dev / 开发依赖

| Pkg | Purpose |
|---|---|
| `meck` | EUnit mock |
| `sync`, `fs` | 热重载（dev shell） |
| `relx` | OTP release 打包 |

## External Services / 外部服务

| Service | Module | Config Key |
|---|---|---|
| jPush（推送） | `imboy_sms`, push_notification_* | `jpush_app_key`, `jpush_master_secret` |
| 云极 SMS | `imboy_sms` | `yjsms_account`, `yjsms_secret`, `yjsms_url` |
| eturnal TURN/STUN | `user_ds:webrtc_credential/1` | `eturnal_secret`, `eturnal_turn_urls`, `eturnal_stun_urls` |
| OSS / Upload | `elib_oss`, `auth_ds` | `upload_url`, `upload_key`, `upload_scene` |
| SMTP | `elib_email` | smtp_* in sys.config |

## Secret Hygiene / 密钥规约

- 来源优先级：`IMBOY_*` env > `sys.config` > app default。
- 生产 fail-fast（`imboy_app:validate_runtime_config/0`）：`jwt_key`、`pg_password ≠ default`、`api_auth_switch=on`、若配置 `eturnal_turn_urls` 则 `eturnal_secret` 必填。
- 切勿硬编码；切勿在 DB `config` 表存放运行期密钥（已迁出，仅留动态业务配置）。

## Erlang OTP / 运行时

- OTP 28+，crypto 使用 `crypto:mac(hmac, sha, Key, Data)`（empty key 已加守卫）。
- 集群：`-name imboy@host -setcookie ...`；通过 `imboy_cluster` + `syn` 加入。
