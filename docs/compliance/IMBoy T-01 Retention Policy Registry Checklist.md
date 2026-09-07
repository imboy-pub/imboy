# IMBoy T-01 Retention Policy Registry Checklist

> 实施计划 Task T-01。对应 Gap Matrix「Retention: Fragmented」行（无统一 policy、注释/配置冲突）。
> 状态：**已实施（2026-09-07）**；owner 对 pending-owner/legal-review 值的批准待翻转。

## Goal（计划原文要点）

数据类保留的单一可核验来源：scope/profile/tenant 覆盖、duration/event/action/exception；未经 counsel 设计不支持 legal-hold；避免 per-country 分支（口径由 profile 承载）；messages/attachments/logs/audit/sessions/payments/backups/vendors 全部有值。

## 交付物

### 1. 注册表 docs/compliance/retention-policy.yml（12 数据类全覆盖）

| 类 | duration | status | 实证 |
|---|---|---|---|
| logs_observability（Loki） | **180d** | evidence-backed | deploy/loki/loki.yml retention_period=4320h |
| metrics_prometheus | **180d** | evidence-backed | --storage.tsdb.retention.time=180d |
| backups_pg | **7d** | evidence-backed | scripts/backup_pg.sh RETENTION_DAYS=7 |
| backups_imboy_db | **10 份轮换** | evidence-backed | scripts/backup_imboy_db.sh |
| sessions_tokens | event-driven（过期即删） | evidence-backed | token 过期/验证码一次性消费为现状 |
| messages_e2ee / messages_channel / moments / attachments | indefinite | **pending-owner** | 无到期清理为现状；建议值待 owner 定 |
| audit_security_moderation | indefinite（retain） | **pending-owner** | 等保建议 ≥180d，翻转待 owner |
| payments_billing | indefinite（retain+anonymize） | **legal-review** | 财税法定留存口径待 counsel |
| vendors | per-vendor | pending-owner | 引用 data-disposition.yml（D-02 单一真源），核实属 V-01 |

- 结构字段：id/description/storage/duration/trigger/action/status/owner(+notes)；
- 顶层 `legal_hold: unsupported`（counsel 设计落地前全仓不支持）；`profiles` 段承载部署口径（overseas_baseline 可下调日志保留，类结构不变）——**无任何 per-country 分支**。

### 2. 校验脚本 scripts/validate_retention_policy.py

- schema 门：必填字段、duration 形态（`Nh/Nd/Nw/Nmo/Ny | indefinite | event-driven | ...`）、action/status 枚举；
- 覆盖门：12 数据类缺一即 FAIL；
- 红线门：出现 `country` 字段即 FAIL；`legal_hold != unsupported` 即 FAIL；
- **实证一致性门**：evidence-backed 条目与 deploy/loki/loki.yml、Prometheus 启动参数、backup_pg.sh 默认值逐一比对；并检测 Loki 注释中与 180d 冲突的「30 天」表述回潮；
- `--root` 可对任意 checkout 运行；`--no-files` 仅跑 schema 门。退出码 0/1。

### 3. Loki 注释/配置冲突修复（Gap Matrix 点名项）

- deploy/loki/loki.yml：旧注释「保留 30 天日志（与 Prometheus 保留期一致）」与实配 4320h(180d)、Prometheus 180d 双重矛盾 → 改为与实配一致的 180d 说明并指向本注册表；校验脚本含防回归检测。

### 4. 测试 test/scripts/test_validate_retention_policy.py（unittest，10 用例）

真仓注册表 schema PASS；缺必填字段/country 分支/legal_hold 开启/非法 duration+action/非法 status/覆盖缺失六类负向；实证一致性（真仓 PASS、mismatch 检出、30 天注释回潮检出）。

## 测试证据（2026-09-07）

- `python3 scripts/validate_retention_policy.py` → **PASS**
- `python3 -m unittest test.scripts.test_validate_retention_policy` → **10/10 OK**

## 已知边界与后续

- **owner 批准翻转**：pending-owner 各项的建议值（如审计 ≥180d、频道帖保留期）与 legal-review 的财税口径需 owner/counsel 确认后更新注册表（status 翻转即可，校验脚本保证实证一致性不漂移）。
- **T-02（执行层）未做**：到期删除/匿名化的 worker（token 过期行清理、备份含删除重放的恢复演练、attachments 跟随消息生命周期）——下一可执行项。
- Admin 配置渲染（config renders against policy）未接：当前以脚本校验为门，接入运行时配置读取后可复用本注册表。
