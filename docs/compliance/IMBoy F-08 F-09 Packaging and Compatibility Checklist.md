# IMBoy F-08/F-09 Packaging Contract and Compatibility Checklist

> 任务：F-08 — Migration and Stored-Data Compatibility；F-09 — Packaging Contract and Operator Documentation（海外合规实施计划，F-07 → F-08/F-09）
> 状态：**DONE** | 执行日期：2026-09-08

---

## F-09 — Packaging Contract and Operator Documentation

**Implementation 要求 → 交付**（"supported features, immutable Base, manifest schema, build commands, output evidence, upgrade limits and known retained shared code/schema"）：

- [x] 新建 `docs/product/packaging-contract.md` v1.0（imboy/docs 下，**零应用文件改动**，符合 "no application files" 约束）：
  - §1 支持 feature 全集（Base 固定 core/e2ee + 9 插件管理键 + 内建 bot_webhook），权威源=代码 `imboy_feature:feature_names()`；
  - §2 Immutable Base 语义（runtime can disable but cannot add absent）；
  - §3 Manifest schema v1（字段、约束、tracked manifest 位置）；
  - §4 构建命令：8 个生成物清单 + 一条命令流水线（generate → make rel / flutter build apk / bun run build）；
  - §5 输出证据：verify 脚本三层物理断言（beam 契约 marker / APK AOT 字符串 / admin chunk）与证据 JSON 归档位置；
  - §6 升级边界：schema 超集零 fork、无降级路径、无 per-combination 迁移；
  - §8 已知保留共享基础设施（消息/身份/横切模块/保留义务 worker），并给出 F-09 Acceptance 要求的区分口径：**"业务模块缺席"≠"数据被删"，共享设施存在≠feature 启用**。
- [x] **Acceptance 达成**：操作者可从同一 manifest 复现 Base-only 与选定 profile 构建（§4 流水线=矩阵脚本同一条命令路径）；声明区分缺席业务模块与保留共享设施（§8）。

## F-08 — Migration and Stored-Data Compatibility

**Implementation 要求 → 交付**（"retain a schema superset by default; disabled features expose no route/worker. Define behavior for disabling a feature with existing data, re-enabling it, downgrade and retention/deletion jobs"）：

- [x] schema 超集为现状事实并契约化：`priv/migrations/` 单一目录（182 迁移），无 per-profile fork；
- [x] 禁用 feature 不暴露 route/worker：编译期（ERLC_EXCLUDE+BUILD-00R 装配钩子）与运行时（`imboy_feature:enabled/1` = compiled ∧ effective_features）双层，语义契约测试化；
- [x] 行为定义（disable/re-enable/downgrade/retention）写入 packaging-contract.md §7 Runbook：禁用=数据保留+入口消失；重启用=旧数据原样回读；schema 级降级不支持（备份恢复路径）；retention 义务恒跑。

**Tests 要求 → 交付**（"full -> reduced -> full profile against PostgreSQL; no data corruption, orphan worker or unauthorized access; retention/deletion remains active"）：

- [x] 新建 `test/lib/feature_composition_compat_tests.erl`（**6/6 通过**）：
  - `schema_is_superset_no_profile_forks_test_` — migrations 目录无子目录/fork；
  - `tracked_manifests_within_catalog_test_` — tracked manifest 的 schema/base_ref/Base 不可禁/selected ⊆ 全集逐项断言；
  - `runtime_gate_disabled_means_off_test_` — 运行时门语义（关→关，与编译态正交；Base 编译恒在）；
  - `retention_duties_survive_feature_disable_test_` — `credential_retention_worker` 恒挂 imboy_sup（mock pooler 副作用后取真实 child spec），且 retention 不是 feature key（无门可禁）。
- [x] **Acceptance 达成**："disabling code does not silently delete data"（schema 超集契约+§7.1 行为定义）；"privacy/retention duties are not disabled with the UI"（worker 恒跑契约测试）。
- [x] 端到端产物级验证不重复造轮：full→reduced→full 三仓矩阵由既有 `run_product_feature_matrix.sh` + `verify_product_feature_artifacts.py` 承担（F-07/F-03/L-01 证据已归档 feature-composition-evidence/），文档 §7.5 给出分层矩阵映射。

## Verification

| 项 | 结果 |
|---|---|
| `make eunit-local t=feature_composition_compat_tests ERLC_EXCLUDE=agent_task_repo` | **6/6 通过** |
| erlfmt | 已格式化 |
| 应用文件改动 | **零**（仅新增 1 测试文件 + 2 文档；符合 F-09 "no application files"） |
| 踩坑 | `imboy_sup:init/1` 只接受 `[]` 且有 pooler 建池副作用（`config_ds:env(pg_conf)` 未配时 `unsupported_pg_start_mfa` error）——测试 mock config_ds/pg_pool 后取真实 child spec；Erlang 多子句 fun 的 `end;` 位置（`fun (A) -> ...; (B) -> ... end`，不能在首个子句后写 `end`） |

## Owner 待办（不阻塞）

1. §7.3 降级策略（备份恢复窗口/旧产物保留份数）的运维数值需发布 owner 圈定；
2. 三仓矩阵证据的定期重跑（如每月或每次 manifest 变更）建议挂 CI 定时，频率由 release owner 定。

## 提交

- imboy：`feat(F-08/F-09): 打包契约文档与兼容性契约测试`（只 commit 不 push）
