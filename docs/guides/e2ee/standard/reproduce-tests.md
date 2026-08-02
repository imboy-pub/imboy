# 测试复现说明（审计向）

> 对应 gap-matrix **D7**。目标：第三方**不问我们任何问题**就能把测试跑起来，
> 并且**事先知道哪些会红、为什么红**。
> 最后核对：2026-08-02

## 0. 先读这一段

跑测试最容易浪费时间的地方，不是环境搭不起来，是**分不清"我搭错了"还是"它本来就红"**。
所以本文把已知会红/会跳过的项全部前置列出（§4）。

命令都是仓里既有的，本文不发明新入口。

---

## 1. 后端（imboy）

### 前置

| 项 | 要求 |
|---|---|
| Erlang/OTP | 28+ |
| PostgreSQL | 18+，需扩展 `pg_jieba` / `postgis` / `timescaledb` / `pgcrypto` |
| 本地库 | 项目使用 docker 容器 `imboy_pg18`（端口 4323） |
| 配置 | `config/sys.local.config`（由 `sys.local.config.example` 复制） |

### 命令

```bash
cd imboy
make compile

# E2EE 安全验证套件（S2–S21 全守护）——审计首选入口
make e2ee-verify

# 单元测试（本地配置，自动注入 -config 与 -pa）
make eunit-local

# 静态检查
make xref
make dialyze
scripts/check_module_boundaries.sh
```

> `make eunit-local` 会自动带上 `-config config/sys.local -pa ebin -pa test`。
> **直接 `make eunit` 会因缺这三个参数而失败**——这不是测试的问题。

### 单模块跑法

```bash
erl -noshell -eval "eunit:test([user_ds_tests],[verbose])" -s init stop
```

---

## 2. 客户端（imboyapp）

### 前置

| 项 | 要求 |
|---|---|
| Flutter | 3.8+ |
| 依赖 | `flutter pub get` |
| 平台 | 部分用例断言 macOS 平台行为，见 §4 |

### 命令

```bash
cd imboyapp
flutter pub get

# E2EE 客户端安全验证套件（一键，脚本自述"可审计"）
bash scripts/run_e2ee_suite.sh

# 仅 e2ee 目录（60 个测试文件）
flutter test test/service/e2ee/

# 全量
flutter test
```

---

## 3. 许可证与证据门

```bash
cd imboy
scripts/license_inventory.sh --selftest   # 判别函数自检，应 10/10
scripts/license_inventory.sh --check      # ⚠️ 当前必然退出 1，见 §4
scripts/evidence_manifest.sh              # 输出证据清单
```

---

## 4. 已知会红 / 会跳过（**跑之前先看**）

| 现象 | 是否预期 | 原因 |
|---|---|---|
| `license_inventory.sh --check` 退出 1 | ✅ **预期** | 两个 AGPL 包仍在（IMB-2026-001）。X15 完成前必然红，这是门禁在正确工作 |
| CI 中 2 个 Dart 测试文件被排除 | ✅ 预期 | 5 例直接实例化 `E2EEApi` 打真实后端（本地 9800 在跑则全绿）+ 1 例断言 macOS 平台行为。**正解是注入 mock / 平台守卫而非排除**，P4 段复核撤销（IMB-2026-026） |
| `imboy SonarCloud` 工作流红 | ✅ 预期 | 需 `SONAR_TOKEN` 与 sonarcloud.io 项目（IMB-2026-025） |
| 全量 `make eunit` 超过 40 分钟 | ✅ 已知 | harness 结构性慢（IMB-2026-024），故未进 CI 关键路径 |
| 契约测试连不上后端 | 取决于环境 | 需本地后端在 9800 且注入 `IMBOY_SOLIDIFIED_KEY`；未起后端时应优雅跳过 |
| elvis 违规 10352 条 / 298 个 FAIL 文件 | ✅ 预期 | 棘轮基线即为此值，**超过才红**。基线于 2026-08-02 重设，属债务承认（IMB-2026-023） |
| 13 个文件超过 800 行 | ✅ 预期 | 同上，基线 13，第 14 个才红 |

---

## 5. **无法**复现的部分（诚实声明）

| 项 | 原因 |
|---|---|
| **双端跨进程测试** | 无 harness。今天只有单进程双 Account round-trip，无进程/网络操纵能力（IMB-2026-022） |
| **真机端到端** | 从未验证过（IMB-2026-021） |
| **附件加密的运行时行为** | 开关未翻开（IMB-2026-005）。代码与测试可跑，运行时无从观测 |
| **KT 的运行时行为** | 未部署（IMB-2026-007）。只有 golden vectors 可核 |
| **故障注入 / fuzz / 崩溃一致性** | 均未做（证据清单 §6 全为 ⛔） |

> 这五项是 SOW 中列为**排除项**的依据。若审计方认为必须覆盖，
> 属我方前置工作，不是 SOW 内可完成的内容。

---

## 6. 期望值参考

跑通后应看到的量级（详见[证据清单](./evidence-manifest.generated.md)，随代码变动）：

| 项 | 量级 |
|---|---|
| Erlang eunit 测试函数 | ~5200 |
| Erlang 测试文件 | ~478 |
| Dart 测试用例 | ~5300 |
| Dart 测试文件 | ~511（其中 e2ee 专项 60） |

数字对不上不一定是问题（代码在演进），但**数量级差一档**值得追问。
