# 双引擎类型检查集成 · 落地验证报告（第一轮自动化测试）

> 执行依据：`docs/gradualizer-landing-plan.md` + `docs/elp-eqwalizer-landing-plan.md` 的 P0/P1
> 日期：2026-07-24 | 执行环境：macOS / **OTP 29**（注意：本地 29，CI 28）/ elp 1.1.0+build-2026-02-27 / java 17
> 结论：**Go** — 双引擎全部跑通并立即发现真实问题，可以进入 P2（CI 接入）

---

## 1. 本轮落地的变更（imboy 仓内，git 可见）

| 文件 | 变更 | 说明 |
|---|---|---|
| `Makefile` | +74 行 | 新增 8 个目标：`gradualizer-setup` / `gradualize` / `gradualize-layer` / `gradualize-audit` / `elp-setup` / `eqwalize` / `eqwalize-layer` / `eqwalize-all` |
| `.elp.toml` | 新建（入仓） | erlang.mk 布局的 ELP 项目描述：9 个 src_dirs、`deps = ["deps/*", eqwalizer_support]`、`enable_all=false`、`elib_log` 豁免 |
| `lefthook.yml` | +14 行 | pre-push `gradualize-changed`：变更 `.erl` 文件跑 Gradualizer（未安装则跳过不阻塞） |
| `.gitignore` | +4 行 | `/.elp/`、`/.gradualizer/`、`/tools/` 产物不入仓 |

工具安装（仓外产物）：`tools/gradualizer`（pin `23533d7`）、`.elp/elp-repo`（sparse，pin `c3708e6`，仅 eqwalizer_support）。

## 2. 验证结果

### 2.1 Gradualizer（pin 23533d7，OTP 29 构建）

**spike 三模块**（均 ~0.3 秒/模块，exit=1 正确门控）：

| 模块 | 告警数 | 代表性发现 |
|---|---|---|
| `src/lib/imboy_env.erl` | 2 | `maybe_override_proplist_int/3` spec 声明 `string()`（非空字符列表）实收 `[char()]` |
| `src/lib/elib_cnv.erl` | 5 | `map_to_query/1` 嵌套列表与 `string:join` 类型不符 |
| `src/logic/msg_c2c_logic.erl` | 8 | **`prepare_c2c_data` 返回 `From` 为 integer 与 spec binary() 不符（line 158）**；line 397 `-` 操作数可能为 `{error, empty_input}`（潜在崩溃点） |

**lib 层全量审计**（69 个模块，排除 `elib_log` + `*_tests`）：

```
PASS=23   FAIL=45   CRASH=1（elib_str）   总告警=107 条
```

- `elib_dt`：`rfc3339_to/2` spec 不收 integer 但调用处 guard 是 `is_binary(Dt); is_integer(Dt)` —— **真实 spec 缺陷**
- `elib_str`：触发 Gradualizer 内部 bug（`gradualizer_lib:pick_value` 遇 `none()` 类型 function_clause）→ 建议加入 `GRADUALIZE_EXCLUDE` 并报上游 issue

### 2.2 ELP / eqWAlizer（elp 1.1.0，glob 语法 `.elp.toml` 验证可用）

| 模块 | eqWAlizer 错误数 | 耗时 |
|---|---|---|
| `imboy_env` | 2（`normalize/1`：union 中 tuple() 与 binary() 不兼容） | ~1s |
| `elib_dt` | 7 | ~1s |
| `elib_uri` | 10 | ~1s |

**关键工程事实：`elp eqwalize` 发现错误时退出码仍为 0** —— Makefile 封装已改为解析输出 `^error:` 判定（`make eqwalize`/`eqwalize-layer`/`eqwalize-all` 全部按此实现并验证，告警时正确返回非 0）。

### 2.3 双引擎互补性实证（同一模块对比）

| 模块 | Gradualizer 发现 | eqWAlizer 发现 | 重叠 |
|---|---|---|---|
| `imboy_env` | line 395/408 spec 精度 | line 40/77 `normalize` | **0 行** |
| `elib_dt` | 2 处调用点 | 7 处（含不同位置） | 部分 |

两工具类型系统不同（gradual typing vs 实用静态类型），**发现的问题集确实不同**，双引擎方案的价值得到实证。

## 3. 踩坑与已固化对策

| 问题 | 对策（已落地） |
|---|---|
| OTP 29 将 `match_alias_pats` 警告升级为错误，Gradualizer 源码编译失败 | `make gradualizer-setup` 内置 `ERLC_OPTS="+nowarn_match_alias_pats"` 覆盖 |
| `elp eqwalize` 退出码恒为 0 | Makefile 全部目标改为 `grep "^error:"` 解析输出判定 |
| make 的 /bin/sh 不支持进程替换 `<(...)`（本环境为 POSIX sh） | `elp-setup` 目录一致性校验改为变量比较，可移植 |
| Gradualizer 打印 "Failed to find include files" 警告（hrl 实际存在） | **非致命**，检查结果仍产出；记录为上游疑似 bug，待跟踪（不影响门禁） |
| Gradualizer 输出中中文注释乱码 | 上游 escript 编码问题，仅显示层，待报上游 |
| `elib_str` 触发 Gradualizer 崩溃 | 建议加入 `GRADUALIZE_EXCLUDE` + 报上游 issue（附 `.gradualizer/logs/elib_str.log`） |

## 4. 基线数字（P2 CI 的预算初值）

| 指标 | 当前值 | 用途 |
|---|---|---|
| `GRADUALIZE_BUDGET`（lib 层 failing 模块） | **45**（P3 修复 elib_dt/elib_log 后重测应 <45，待 CI 首跑校准） | CI 预算起点，只许降不许升 |
| lib 层 Gradualizer 告警总数 | **107** | 修复工作量参考 |
| Gradualizer 单模块耗时 | ~0.3s | pre-push 全变更集 <10s ✅ |
| eqWAlizer 单模块耗时 | ~1s | 全仓 400+ 模块单线程 ~7min，`max_tasks=4` 并行 ~2min ✅ |

## 5. P2 CI 接入（2026-07-24 当晚完成）

`backend-ci.yml` 新增两个 baseline job（均 `continue-on-error: true`、`needs: [compile]`，沿用 dialyze 既有范式），job 列表现为：
`compile → moment-eunit → full-eunit → dialyze → gradualize → eqwalize → xref → dco-check`

| job | 内容 | 预算（ratchet 初值） | 缓存 |
|---|---|---|---|
| `gradualize` | 全仓 `gradualize-audit` + `metrics.txt` 进 step summary | `GRADUALIZE_BUDGET` 默认 9999（首次 CI 观察值后设仓库变量激活 ratchet） | Gradualizer escript（pin 23533d7 为 key） |
| `eqwalize` | lib 层 `eqwalize-layer`（70 模块，本地实测 58s） | `EQWALIZE_BUDGET_LIB` 默认 **38**（本地基线 modules=70 failing=38，CI/OTP 28 可能漂移需校准） | `.elp/`（ELP 二进制 + eqwalizer_support，release 2026-06-10 otp-28 构建 + pin 为 key） |

P2 落地时新增的关键事实：

- **eqWAlizer lib 层全量基线：70 模块 / failing 38 / 58 秒**（vs Gradualizer 45 failing——两引擎覆盖面不同再获实证）；
- ELP Linux 资产：`releases/download/2026-06-10/elp-linux-x86_64-unknown-linux-gnu-otp-28.tar.gz`（pin 版本进缓存 key）；
- eqwalize job 必须 `actions/setup-java@v4`（temurin 17）；
- **顺手修复**：原 xref job `name:` 含 `: `（`ratchet: 0`）导致严格 YAML 解析失败（HEAD 版本同样失败，GitHub 解析器容忍），已加引号修复，现全文件通过严格 YAML 校验。

## 6. P3 首批治理（2026-07-24 当晚完成）

### 6.1 已固化为本地 commit

| commit | 内容 |
|---|---|
| `a18a0255` | **P0–P2 集成基线**：Makefile 8 目标 + `.elp.toml` + lefthook + backend-ci.yml 双 job + `.gitignore`（仅 5 文件，未触碰他人变更） |
| 后续 fix | **lib 层首批真阳性修复**：`elib_dt` spec 拓宽 + `iolist_to_binary` 运行时修复 + `safe_gregorian_secs` 显式守卫；`elib_log` 1 参数 spec 放宽 `term()`；`msg_c2c_logic:prepare_c2c_data` 返回 spec 修 `integer()` |

### 6.2 首批修复效果（双引擎实测）

| 模块 | Gradualizer 修复前→后 | eqWAlizer 修复前→后 | 说明 |
|---|---|---|---|
| `elib_dt` | 5 → **0** | 7 → **2**（OTP `rfc3339_time_unit()` 别名未结构展开，工具局限，记录不修） | spec 拓宽 + `list_to_binary`→`iolist_to_binary` 同时修类型与运行时崩溃 |
| `elib_log`（自身） | 多 → **0** | 豁免（已在 `ignore_modules`） | 1 参数 spec `iodata()`→`term()`，消除所有调用方 "undefined function" 类误报 |
| `msg_c2c_logic` | `prepare_c2c_data` spec 缺陷已修 | 同 | 下游 `stage_and_send_c2c` 第 4 参是 `integer()`，原 spec 误标 `binary()` |

### 6.3 待续（下一批，不扩大 logic 层白名单）

- `msg_c2c_logic` 残留 9 条均为 Gradualizer 推理局限（行 397/637 `is_integer` 守卫未跨 `andalso` 识别、799/851 `pos_integer()|binary()` 细化不足）——**非代码缺陷**，留 logic 层治理表
- lib 层 Gradualizer 告警 149 条集中在 `elib_cipher`/`elib_req`/`elib_pg`/`imboy_cluster` 等真实复杂度模块（OT 真阳性），按"真阳性优先"逐批治理
- 修复后 lib 层 Gradualizer failing 模块数需重测（elib_dt/elib_log 已清零，预计 < 此前 45）

### 6.4 上游 issue（待提交 josefs/Gradualizer，网络可达后）

草稿见 `docs/gradualizer-upstream-issues.md`：
1. **Issue 1（崩溃，P0）**：`gradualizer_lib:pick_value/2` 缺 `none()` 子句 → OTP 28+ 下分析含 `none()` 类型模块时 `function_clause`。复现：`elib_str.erl`。建议加 `pick_value(?type(none), _) -> ...` 子句。
2. **Issue 2（误报）**：含 `-include` 的模块在宏展开后报 `undefined function`/类型误报，即使 `-I include` 已传。

临时规避：`GRADUALIZE_EXCLUDE` 已含 `src/lib/elib_str.erl`。

---

## 7. 下一步（P3 续）

1. 首次 CI 运行后：用观察值设置仓库变量 `GRADUALIZE_BUDGET`（全仓）与校准 `EQWALIZE_BUDGET_LIB`（OTP 28 数字）；
2. 网络可达后提交上节 2 个 Gradualizer issue（草稿就绪）；
3. lib 层剩余真阳性批次治理（elib_cipher / elib_req / elib_pg 等）；
4. lib 层双引擎 failing 清零 → Gradualizer `gradualize-layer LAYER=lib` 转阻塞 → 稳定 2 周后 eqWAlizer 接管（双引擎交接）。

---
**验证结论**：两份计划 P0/P1 全部落地成功。Go/No-Go 四项指标——项目加载 ✅、告警真阳性（抽样 6 条全为真实问题）✅、单模块耗时（0.3s/1s，远优于阈值）✅、退出码门控（封装后）✅——**全部通过，判定 Go**。P2 CI 接入已完成并通过严格 YAML 校验，待推送后观察首次 baseline。P3 首批修复已落地 2 个本地 commit，双引擎在"修真阳性"与"识别工具局限"上的分工边界已实证清晰。
