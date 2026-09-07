# IMBoy 打包契约与运维指南（Packaging Contract & Operator Guide）

> 版本：v1.0 | 最后更新：2026-09-08
> 任务：海外合规计划 **F-09 — Packaging Contract and Operator Documentation** 与 **F-08 — Migration and Stored-Data Compatibility**（兼容性 Runbook 见 §7）
> 受众：负责构建/发布 IMBoy 组合产物（Base-only / full-selected / overseas_baseline / agent_hub 等 profile）的运维与发布工程师
> 关联：`scripts/generate_product_features.py`（契约源）、`scripts/verify_product_feature_artifacts.py`（产物验证）、`scripts/run_product_feature_matrix.sh`（三仓矩阵）、[F-07 三产物构建矩阵](../compliance/IMBoy%20Feature%20Composition%20F07%20Execution%20Record.md)

---

## 1. 支持的 feature 全集

Feature 全集的**唯一权威源**是代码：`imboy_feature:feature_names()`（`src/lib/imboy_feature.erl`），静态目录见 `src/lib/imboy_policy_catalog.erl`。构成：

| 类别 | feature | 裁剪性 |
|---|---|---|
| Base 固定 | `core`、`e2ee` | **不可裁剪**（任何 manifest 不得禁用；生成器 `disabled_base_features` 强制为空） |
| 插件管理（plugin-managed） | `channel`、`location`、`moment`、`channel_discover`、`channel_invitation`、`channel_order`、`group_vote`、`group_schedule`、`group_task` 及插件注册表追加键 | 可按 manifest 选择性编译/禁用 |
| 平台内建 | `bot_webhook` | 可禁用（overseas_baseline 默认关） |

依赖关系（如 `channel_discover`/`channel_order` 依赖 `channel`）由 `imboy_policy_catalog:dependencies/1` 与生成器 catalog 校验。

## 2. Immutable Base

- `core` 与 `e2ee` 是组合产物的不变底座：**运行时 policy 可以进一步关闭 e2ee 的可选面（`e2ee_mode=disabled`），但代码永远物理在产物内**——"runtime can disable but cannot add absent"（F-01 定案）。
- 契约测试：`test/lib/feature_composition_compat_tests.erl`（`compiled(e2ee)`/`compiled(core)` 恒真断言）。

## 3. Manifest schema（v1）

```json
{
  "schema_version": 1,
  "product_id": "imboy",
  "profile": "<profile 名>",
  "base_ref": "imboy-feature-inventory-v1",
  "selected_features": ["..."]
}
```

约束（`generate_product_features.py:validate` 强制）：schema_version=1；product_id=imboy；`base_ref` 固定；`disabled_base_features` 必须为空（Base 不可禁）；`selected_features` ⊆ 全集且满足依赖闭包；`compiled_features = selected + 依赖闭包 ∪ Base`，生成器输出 `manifest_hash`（sha256）。

**Tracked manifest 位置**：`config/product-feature-manifests/*.json`（如 `overseas_baseline.json`）；`config/product-feature-manifest.json` 为 full-selected 默认清单。运行时镜像校验见 `feature_composition_compat_tests:tracked_manifests_within_catalog_test_`。

## 4. 构建命令（单 manifest 复现）

生成器从 manifest 派生 **8 个确定性产物**（全部带 `do not edit` 头，`--check` 可校验陈旧）：

| 端 | 产物 | 裁剪作用 |
|---|---|---|
| 后端 | `imboy/include/generated/imboy_product_features.hrl` | `?IMBOY_COMPILED_FEATURES` 宏 + manifest hash |
| 后端 | `imboy/include/generated/imboy_product_features_erlc.mk` | `IMBOY_FEATURE_ERLC_EXCLUDE`：未选中 feature 的 beam 物理不编译（erlang.mk ERLC_EXCLUDE + BUILD-00R 装配钩子：rel-deps → app → prune → relx） |
| Flutter | `lib/app_core/feature_flags/generated_product_features.dart`、`lib/config/router/generated_product_feature_routes.dart`、`generated_channel_order_widget.dart` | 编译集常量 + 路由树物理裁剪（未选中路由不进编译单元，AOT payload 无字符串残留） |
| Admin | `src/generated/productFeatures.ts`、`generatedFeatureComposition.tsx` | 路由/菜单/chunk 编译集 |
| Android | `product-features.properties` + debug/profile/release `AndroidManifest.xml` overlay | 权限/组件按需注入 |

**一条命令流水线**（等价 `scripts/run_product_feature_matrix.sh <preset>`）：

```bash
python3 imboy/scripts/generate_product_features.py --manifest <manifest.json>
make -C imboy rel                                   # 后端 release
(cd imboyapp && flutter pub get && flutter build apk --release)
(cd imboyadmin && bun run build)
python3 imboy/scripts/verify_product_feature_artifacts.py \
  --manifest <manifest.json> \
  --backend-beam "$(find imboy/_rel -path '*/ebin/imboy_feature.beam' | tail -1)" \
  --flutter-apk imboyapp/build/app/outputs/flutter-apk/app-release.apk \
  --admin-dist imboyadmin/dist \
  --output docs/compliance/feature-composition-evidence/<preset>.json
```

## 5. 输出证据（artifact 断言）

`verify_product_feature_artifacts.py` 三层物理断言（不是菜单级、是字节级）：

1. **后端 beam**：release 内 `imboy_feature.beam` 的 compiled_features 契约 marker 与 manifest hash 一致；未选中 feature 的业务模块（`agent_task_repo` 类）beam 不得存在；
2. **Flutter APK**：release `libapp.so` AOT payload 中不得残留被裁剪路由的 GoRoute name 字符串（marker 选自生成路由文件，避开核心页面硬编码路径）；
3. **Admin dist**：feature 专属动态 chunk 文件必须物理缺席。

证据 JSON 归档 `docs/compliance/feature-composition-evidence/<preset>.json`（含三仓 SHA 与产物哈希；矩阵铁律=先提交后跑，verify 记录 head/dirty/worktree hash）。

## 6. 升级边界（Upgrade Limits）

- **Schema 超集，零 per-profile fork**：`priv/migrations/` 是唯一迁移目录（182 个迁移文件），所有 profile 共享同一迁移序列。禁用 feature 只关代码路径，**不回滚、不删除任何表或数据**。
- **无 schema 降级路径**：迁移只有 up；down 文件仅供开发清理，发布序列不含 downgrade。
- **无 per-combination 迁移**：新增 feature 的迁移照常追加到同一序列；旧产物升级到新 profile 不需要补历史行。
- 生成物 `--check` 门防漂移：源码与 manifest 不同步时 CI/`--check` 即红。

## 7. Compatibility Runbook（F-08）

### 7.1 禁用一个已有数据的 feature

行为：路由/端点/UI/worker 消失（编译缺席或运行时门拒绝 `ERR_FEATURE_DISABLED`），**库表与既有行原样保留**（schema 超集，见 §6）。禁用动作本身不触发任何删除路径。已知读侧语义：feature 关闭后相关 API 返回 feature-disabled 错误或空集，不是数据丢失；举报工单/审核队列里的历史引用行保留（审核面见 `moderation_policy` surface 白名单）。

### 7.2 重新启用

把 feature 加回 `selected_features` → 重新生成产物 → 发布。旧数据原样回读（表一直在、无数据迁移需要）；feature 关闭期间产生的迁移照常已应用（超集序列），无需补迁。

### 7.3 降级（回到旧 profile/旧版本）

- 产物级：允许回到裁剪更少/更多的 profile（同一 schema 序列），beam/APK/dist 按目标 manifest 重建即可；
- Schema 级：**不支持**降级到更旧迁移版本——发布 runbook 必须先备份（`imboy/scripts/backup_pg`），回退版本=恢复备份+旧产物。

### 7.4 隐私/保留义务不随禁用停（Acceptance）

- `credential_retention_worker`（T-02，验证码 PII 过期清理）恒挂 `imboy_sup`，**不经任何 feature 门**——契约测试 `feature_composition_compat_tests:retention_duties_survive_feature_disable_test_`；
- 账号删除链（D-01~D-03 的 job/清扫/墓碑重放）同理为 Base 义务，不随 UI/feature 关闭而停；
- "禁用代码不静默删数据"的可测面：`schema_is_superset_no_profile_forks_test_`（无 fork 目录）+ §7.1 行为定义。

### 7.5 兼容性测试矩阵

| 层 | 载体 | 覆盖 |
|---|---|---|
| 静态契约（eunit，本批新增） | `test/lib/feature_composition_compat_tests.erl`（6/6） | schema 超集、manifest ⊆ 全集、运行时门语义、retention worker 恒跑 |
| 端点级 | `feature_route_http_tests`（矩阵脚本内 `make eunit t=`） | base-only/full-selected 下可选路由 401/404 语义 |
| 产物级（三仓矩阵） | `run_product_feature_matrix.sh {base-only\|full-selected\|overseas_baseline\|agent_hub}` | 生成→构建→三层物理断言→证据 JSON（含三仓 SHA） |
| 双 profile boot | F-03 执行记录（9803/9804） | 运行时 effective_features/capability 全景 |

## 8. 已知保留的共享代码/Schema（Retained Shared Infrastructure）

组合裁剪裁的是**业务模块与路由/UI 面**，以下共享基础设施在任何 profile 恒在（它们不属于任何单一 feature，属 Base）：

- **消息/会话域**：`msg_*` 表族与 C2C/C2G 管道（E2EE 与举报、审核、导出的载体）；
- **身份/合规域**：user/user_device/user_setting/review_queue/report_ticket/user_deletion_job 等表与逻辑；
- **平台库**：`imboy_policy`/`imboy_feature`/`imboy_profile_preset`/`moderation_policy`/`log_redact`/`elib_log` 等横切模块；
- **保留义务 worker**：`credential_retention_worker`、删除清扫/墓碑重放链（§7.4）；
- **插件壳**：feature 以插件注册表键存在（`imboy_plugin_registry`），壳与目录恒在，被裁的是键对应的业务实现模块。

> 区分口径（F-09 Acceptance）：产物里**找不到的是业务模块**（如某 feature 专属 handler/logic/repo 的 beam），**找不到≠删了数据**；找到的共享基础设施不构成 feature 存在的声明——feature 是否启用只以 manifest + 运行时 policy 为准。
