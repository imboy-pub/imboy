<!-- Generated: 2026-04-17 | Migrations: 86 | Token estimate: ~700 -->

# Data Codemap / 数据图谱

> 双语 / Bilingual. PostgreSQL 18+ with `pg_jieba`, `postgis`, `timescaledb`, `pgcrypto`, `pg_trgm`.

## Migration Sequence / 迁移序列

`priv/migrations/00000000_*.sql` … `00000085_*.sql`（86 个，~6.3k LOC）。Runtime 由 `imboy_migrate.erl` (`pure_migrations`) 应用，单调递增、不可回退。

## Key Tables / 关键表（按领域）

| Domain | Tables | Migration |
|---|---|---|
| Config | `config` | 00 |
| App | `app_version`, `app_ddl`, `app_upgrade_log`, `app_version_upgrade_strategy` | 01,02,81,82 |
| Feedback | `feedback`, `feedback_reply` | 03,04 |
| Verification | `verification_code`, `login_attempt` | 05 |
| User | `user`, `user_log`, `user_setting`, `user_device`, `user_collect`, `user_denylist`, `user_tag`, `user_tag_relation` | 06–14 |
| Friend | `user_friend_category`, `user_friend` | 15,16 |
| Attachment | `attachment` | 17 |
| Group | `group`, `group_member`, `group_log`, `group_random_code`, `group_notice`, `user_group` | 18–22, 24 |
| Geo | `geo_people_nearby` (postgis) | 23 |
| Conversation | `conversation`, `conversation_pin/mute/delete` | 25, 56, 64 |
| Message C2C/C2G/C2S/S2C | `msg_c2c`, `msg_c2g`, `msg_c2g_timeline`, `msg_c2s`, `msg_s2c`, `msg_topic` | 26–31 |
| Admin | `adm_user`, `adm_role`, role separation | 32,33,85 |
| FTS / Indexer | `fts_user` + functions | 36–41 |
| ID/TSID | `id_segment_service`, TSID migration | 43, 80 |
| E2EE | E2EE keys, shard transmission, social/local backup, compliance | 44, 78 |
| Read state | `msg_read` | 45 |
| Channel | `channel`, `channel_subscribe`, `channel_message_revoke` | 46, 57, 66 |
| Group ext | `group_schedule/tag/file/album/task/vote`, member role, soft-delete, remark | 47–61, 67, 72 |
| Msg ext | `msg_forward`, `msg_mentions`, `msg_reaction`, `msg_burn_after` | 62, 63, 65, 77 |
| Moment | `moment_*` (core + perf indexes) | 68, 69 |
| Report | `report_ticket`, `report_action_log` | 70 |
| Announcement | `announcement` | 71 |
| Live | `live_room` | 73 |
| Wallet | `wallet` | 74 |
| Msg Archive | `msg_store`, `msg_store_seq`, `msg_store_staging` | 75 |
| Push | `push_token` | 76 |
| FK indexes | bulk index for FKs | 79 |
| Hypertables | timescaledb time-series | 83 |
| Schema fixes | conversation varchar→bigint | 84 |

## Conv-Seq Ordering / 严格顺序

`msg_store_seq` per `conv_key`（`c2c:{min}:{max}` / `c2g:{gid}`）原子 +1，`msg_store.conv_seq` 单调递增；客户端历史同步以此为游标，不要用 `TSID`。

## Cache Keys / 缓存键

depcache：`{Table, Id}`、`{Uid, Did}`；`imboy_cache:get/set/del`；多节点用 `imboy_cache_sync`。

## Access Rules / 访问规约

- 所有 SQL 走 `elib_pg`（封装连接、参数化、超时、重试）。
- 禁直接 `epgsql:` 调用；禁字符串拼接 SQL。
- TSID 入库由 Repo 层 `elib_tsid:generate/1` 生成。
