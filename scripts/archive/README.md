# scripts/archive/ — 归档工具索引 / Archived Tools

> 归档原因与恢复入口。每个条目记录：原用途、适用版本、执行条件、风险、恢复限制、关联文档。
> Archived on 2026-11-27 (IMBOY_SCRIPTS_DOCS_GOVERNANCE_V1 / S2-03)。历史 PASS 仅作历史证据，
> 不构成现行验收。恢复使用：`git mv` 回 `scripts/` 原路径并补回调用方登记。

## legacy-backup/backup_imboy_db.sh

- **原用途**：旧生产主机的 PostgreSQL 逻辑备份 cron 脚本（schema+data 拆分 dump，排除
  msg_*/verification_code 等敏感表数据，tar.gz 轮换保留 10 份），安装到
  `/usr/local/bin/backup_imboy_db.sh` 由服务器 crontab 每日 02:00 调用。
- **适用版本**：imboy_v1 时代（硬编码容器 `q18qzozxvhaqxg`、库 `imboy_v1`、密码文件
  `/etc/imboy_db_password`）。现行部署统一使用 `scripts/backup_pg.sh`（env 可配、
  `-Fc` 格式、`RETENTION_DAYS` 轮换）与 `scripts/backup_garage.sh`。
- **执行条件**：仅适用于旧生产主机布局；容器名/库名/密码路径全部硬编码。
- **风险**：`set -x` 会把 `-e PGPASSWORD=<密码>` 打进 cron 日志（密码泄漏面）；
  若在现网复用会写错目标容器。
- **恢复限制**：不适用于 PG18 现行卷拓扑；恢复旧备份须先核对排除表清单
  （msg_c2c/msg_c2g/msg_c2g_timeline/msg_c2s/msg_s2c/fts_user/verification_code/
  database_migrations_history 无数据）。
- **关联文档**：`docs/compliance/retention-policy.yml` 的 `backups_imboy_db` 条目
  （已改 pending-owner，直至服务器侧 cron 下线确认）；归档前调用方
  `scripts/release_gate_check.sh`、`scripts/validate_retention_policy.py`、
  `docs/deliver/runbook-template.md` 已同步更新。

## fastdfs-migration/（fdfs_reference_census.sql + migrate_fdfs_avatars.erl）

- **原用途**：go-fastdfs → Garage 附件迁移的一次性工具：SQL 普查仓内 fastdfs 引用 +
  prod 节点头像迁移 escript（脚本头自述“一次性脚本，prod 节点运行”）。
- **适用版本**：FastDFS 存量头像迁移窗口（迁移已完成）。
- **执行条件**：迁移期 prod 节点 + 旧 fastdfs 存储在线。现 fastdfs 引用已清零，
  重新执行无害但无意义（普查 SQL 只读）。
- **风险**：migrate_fdfs_avatars.erl 重复执行会重复搬运对象；需 prod 停写窗口。
- **恢复限制**：仅当出现新的 fastdfs 存量数据源时才有意义。
- **关联文档**：`scripts/README.md`（原登记行已同步改为归档注记）。

## demo/（dual_exp_demo_b.sh + dual_exp_demo_b_w2.sh）

- **原用途**：双体验（dual experience）方案 B 的本地演示编排（w2 为第二轮变体）。
- **适用版本**：演示期一次性产物，方案已定案；w2 无任何引用，b 仅被 w2 调用。
- **执行条件**：本地开发栈 + 演示数据；依赖当时的产品形态，现行版本未必可跑通。
- **风险**：无；纯本地演示，不触生产。
- **恢复限制**：如需复现当时演示，须检出归档时的提交并配合当期数据 seed。
- **关联文档**：无（演示结论已并入产品决策记录）。

## one-off-sql/（billing_cancel_snapshot.sql + organization_directory_fixture_oracle.sql）

- **原用途**：一次性排查/验收 SQL：billing 取消链路现场快照取证；组织目录
  fixture oracle（隔离合成数据验收比对）。
- **适用版本**：各自专项验收当期；均无任何静态或动态调用方（zero-ref 双重复核）。
- **执行条件**：手工会话中对目标库人工执行；oracle SQL 依赖当期 fixture 结构。
- **风险**：对生产库执行前未审读可能锁表；仅限只读会话使用。
- **恢复限制**：表结构漂移后需人工适配列名。
- **关联文档**：无现行文档引用；取证结论已沉淀在对应专项验收记录。

## obsolete-config/dev_verify_oss_config.escript

- **原用途**：校验 config 中 garage 块的服务端 dev_use_public_bucket 行为；
  其注释引用的 `dev_use_public_bucket.sh` 已从仓库消失，校验目标不复存在。
- **适用版本**：OSS/Garage 开发配置调整期。
- **执行条件**：本地节点 + 旧配置形态；现行 config 键位已漂移。
- **风险**：无；只读校验。
- **恢复限制**：恢复前先确认现行 garage 配置键位并改写校验目标。
- **关联文档**：无。

## hird-poc-v1/（compile.sh、emit_pg_env.py、run_pg_suite.sh、run_suites.sh）

- **原用途**：HIRD（BEAM Agent 类型化语言实验）鉴权重放 POC 套件：编译、PG 环境
  注入、PG 套件与全量套件编排。
- **适用版本**：POC V1（结论已沉淀在 docs/architecture/hird-poc/IMBOY_HIRD_AUTH_REPLAY_POC_V1.md，
  报告文档保留在 docs/ 不随迁）。
- **执行条件**：imboy deps ebin + 真 PG + hird 仓可达；旧 phase1 g01 门曾引用
  compile.sh（该门已同批归档）。
- **风险**：依赖 hird 仓演进，重放需对齐两仓版本。
- **恢复限制**：POC 已结题，仅历史证据用途。
- **关联文档**：`docs/architecture/hird-poc/IMBOY_HIRD_AUTH_REPLAY_POC_V1.md`。

## phase1-gates/（phase1_gate_all.sh + g01–g19，共 20 项）

- **原用途**：广州交付 Q12 一期终验「20 门总控」：Backend 7 + Admin 8 + App 2 + 演练 3
  的串行门禁执行器（约 5.5–7h）。`g19_backup_object_layout_test.sh` 为其备份布局
  drill 的独立包装测试。
- **适用版本**：2026-10 phase1 交付期。现役交付计划已明文废弃整套执行：
  `docs/design/2026-10-23-guangzhou-delivery-v3/plan-v3.2.md`「不整套执行旧
  phase1_gate_all.sh，仅复用已核实的独立入口」。g18 门现役后继为
  `scripts/test/talk_record_g18_gate.sh`（.qa 通道）；发布一致性现由
  `scripts/check_release_consistency.sh`（CI）承接。
- **执行条件**：三仓并排检出 + 真 PG + Admin/App 环境；目标已漂移，不可整套重放。
- **风险**：误跑会得到过时判定（目标过时 ≠ 现行质量结论）。
- **恢复限制**：g06/g07（eunit 双跑）与 g19 是已归档交付证据
  （unified-delivery-closeout materials-index）的组成部分，仅作历史证据复现用。
- **关联文档**：`docs/design/2026-10-11-unified-delivery-closeout/`、
  `docs/design/2026-10-23-guangzhou-delivery-v3/`、`docs/deliver/runbook-template.md`
  （--list 提法已同步更新）。

## phase1-w14/（phase1_w14_delivery_check.sh + oa_stub/3 + webhook_receiver_stub.py）

- **原用途**：W14 交付波次验收：自签证书 + OA stub 服务器/页面 + webhook 环回
  接收器的端到端交付检查。
- **适用版本**：phase1 W14 波次（已收口）；簇内唯一入口 phase1_w14_delivery_check.sh
  零引用，其余文件仅被簇内互相调用。
- **执行条件**：本地端口 8xxx 环回 + Python3；依赖当期交付合同。
- **风险**：无；纯本地 stub。
- **恢复限制**：OA/webhook 契约已演进，重放前须对当期合同核对断言。
- **关联文档**：phase1 交付计划（docs/design/）。

## legacy-attachment-gates/（attachment_edge_gate.sh、eb_asset_edge_gate.sh、avatar_publish_gate.sh、attachment_direct_v1_locations_gate.sh）

- **原用途**：V3/direct-v1 时代的真 Garage 附件门（下载边缘、G17 资产、G13 头像
  直传、direct-v1 位置映射/attempt 账本）。
- **适用版本**：已被 attachment_lifecycle_v5 家族与 `test/lib/*.erl` 通道取代
  （avatar_publish 现役通道为 `test/lib/avatar_publish_gate.erl`，.qa 台账 2026-11 记录）。
- **执行条件**：真 Garage 凭据 + 隔离桶。
- **风险**：旧门断言与新 v5 合同不一致，误跑产生矛盾证据。
- **恢复限制**：仅当需要历史证据复现时使用。
- **关联文档**：`.qa/yield-registry.md`（现役通道记录）、docs/architecture/attachment-lifecycle-v5/。

## one-off-acceptance/（cs_ai_v4_acceptance.sh、enterprise_internal_curl_evidence.sh、csww_e2e_seed.sh）

- **原用途**：一次性验收/取证：智能客服 AI v4 端到端验收（P0-12）、Q08/AC19-6
  缺口 curl 证据、CSWW Playwright 种子重建（配方指向仓外 csww_e2e_chain.sh）。
- **适用版本**：各自专项当期；均零外部引用（双重复核）。
- **执行条件**：cs_ai_v4 需客服域 fixture；curl 证据需运维会话；csww 种子依赖
  仓外 Playwright 链仓。
- **风险**：验收合同已收口，误跑得到过时判定。
- **恢复限制**：重放前对当期验收合同核对；csww 种子需先恢复仓外依赖。
- **关联文档**：对应 design plan（docs/design/2026-10-*）。

## group-file-migration/（run_group_attachment_acl_pg.sh、run_group_file_legacy_migration.sh、group_file_legacy_migration.sql）

- **原用途**：C2G/ACL eunit 人工跑批器 + 群文件 legacy 迁移执行器/SQL（孤儿链：
  三者仅构成闭环，无外部引用）。
- **适用版本**：群文件 legacy 迁移窗口（已收口）。
- **执行条件**：真 PG + 迁移前置态。
- **风险**：迁移重放语义未对现行迁移框架核验。
- **恢复限制**：仅历史证据用途。
- **关联文档**：无现行文档引用。

## orphan-fixtures/（e2ee_alert_rules_test.yml、pg18_bluegreen_mock_command.sh）

- **原用途**：promtool test-rules 输入 fixture（规则文件已换名，无任何 promtool
  调用方）+ pg18 蓝绿方案 mock 替身（蓝绿方案已在 git 30e462862 删除，由目录换像
  方案取代）。
- **适用版本**：各自方案期；孤儿 fixture 双重复核无消费方。
- **执行条件**：promtool / pg18 蓝绿环境（均已不存在）。
- **风险**：无。
- **恢复限制**：恢复前先恢复对应方案与消费命令。
- **关联文档**：`docs/guides/operations/pg18-image-directory-swap-upgrade.md`（替代方案）。

