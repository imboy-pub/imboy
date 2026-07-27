# imboy 引入 Gradualizer 落地计划（与 ELP/eqWAlizer 双引擎协同）

> 前置文档：`docs/erlang-static-typecheckers-analysis.md`（选型分析）、`docs/elp-eqwalizer-landing-plan.md`（ELP 落地计划）
> 目标：Gradualizer 以 **escript CLI** 方式接入 erlang.mk 构建体系，与 eqWAlizer 形成"本地快检 + CI 深检"的双引擎类型门禁
> 日期：2026-07-24 | 状态：待执行（P0 为 1 天 spike）

---

## 0. 为什么是"双引擎"而不是二选一

Gradualizer 与 eqWAlizer 都查"spec 一致性"，能力重叠约 70%。同时引入的理由不是冗余，而是**各取所长**：

| 维度 | Gradualizer | eqWAlizer (ELP) |
|---|---|---|
| 运行时依赖 | **仅 Erlang**（escript） | Erlang + **JVM 17+**（Scala jar） |
| 单模块冷启动 | **~1–3 秒** | ~10–60 秒（JVM 启动） |
| 类型系统 | gradual typing（无 spec 不报错） | 实用静态类型（无 spec 也做推断检查） |
| 生产背书 | beta，社区项目 | WhatsApp 超大规模生产验证 |
| `--infer` 模式 | ✅ 可从字面量推断类型，覆盖无 spec 代码 | — |
| 全仓"宽网"扫描 | ✅ 天然零噪音起步（opt-in） | 全量开跑告警量大，需豁免治理 |

### 0.1 双引擎分工矩阵（核心设计，避免告警打架）

| 场景 | 承担工具 | 理由 |
|---|---|---|
| **pre-push 本地快检**（变更模块） | **Gradualizer** | escript 秒级冷启动、无 JVM 依赖，lefthook 体验好 |
| **CI 全仓基线扫描**（宽网 + 趋势指标） | **Gradualizer** | 无 spec 的模块零告警，全仓可跑；failing 文件数作为"类型健康度 KPI" |
| **CI 分层阻塞门禁**（逐层转绿） | **eqWAlizer** | 检查强度更高、生产背书，当"严"的那道闸 |
| 写 spec 时的即时反馈（保存即查） | Gradualizer | 编辑器外手动 `make gradualize FILE=...`，秒回 |
| 核心消息链路深度检查 | eqWAlizer | occurrence narrowing 等高级能力 |

**收编规则**：某一层在 eqWAlizer 侧**转为阻塞门禁后**，该层 Gradualizer 在 CI 中降为"仅记录不拦截"（防回归参考），本地 pre-push 仍用 Gradualizer——**任一模块同一时刻只有一个阻塞者**，杜绝双跑双重门禁的维护成本。

### 0.2 总体路线

```
P0 spike（1 天）          P1 固化（0.5 周）            P2 CI 化（1-2 周）           P3 协同治理（持续）
escript+三模块验证 ──▶ Makefile 目标+pre-push 钩子 ──▶ 全仓基线 continue-on-error ──▶ 随 eqWAlizer 逐层收编
   Go/No-Go 决策           lib 层本地可跑                 趋势指标进 CI summary        KPI 周报+豁免治理
```

> 节奏上建议 **Gradualizer 先行**（P0/P1 本周完成，依赖少见效快），ELP 的 P0 spike 下周进行——Gradualizer 跑通后，Makefile/hook/CI 的骨架就是现成的，ELP 接入时直接复用同一套模式。

---

## P0 — Spike（1 天）：验证可行性

### Step 1：安装（源码 escript，pin 版本，主路径）

```bash
cd ~/project/imboy.pub/imboy
git clone https://github.com/josefs/Gradualizer.git tools/gradualizer
cd tools/gradualizer && git checkout <PINNED_COMMIT> && cd -   # pin commit，记入 tools/gradualizer/PINNED
make -C tools/gradualizer escript        # 产出 tools/gradualizer/bin/gradualizer
tools/gradualizer/bin/gradualizer --help | head -5
```

> `tools/` 加入 `.gitignore`。本地便利备选：`brew install gradualizer`（若 formula 可用，spike 顺手验证；**CI 一律走源码 escript** 保证可复现）。

### Step 2：验证三模块（先 `make compile`，跨模块 spec 从 beam 读取）

```bash
IMBOYENV=local make compile
G=tools/gradualizer/bin/gradualizer
$G -pa ebin -pa deps/*/ebin -I include --no_color src/imboy_env.erl          # ① 最小模块
$G -pa ebin -pa deps/*/ebin -I include --no_color src/lib/ec_cnv.erl         # ② lib 纯函数
$G -pa ebin -pa deps/*/ebin -I include --no_color src/logic/msg_c2c_logic.erl # ③ logic 复杂模块
```

逐条记录：能否跑通 / 告警条数 / 真阳性率（人工抽 10 条）/ 单文件耗时。

**Spike 重点排查项**（imboy 特有的三个坑位）：

| 排查项 | 预期 | 应对 |
|---|---|---|
| `elib_log.erl`（唯一直接用 lager + parse_transform 的模块） | Gradualizer 分析**未转换的源码 AST**，`lager:info(...)` 就是普通远程调用，应可检查 | 若报错/误报 → 加入排除清单 `GRADUALIZE_EXCLUDE` |
| `imboy_pb.erl`（gpb 生成，带 type_specs） | 生成代码可能告警 | 直接排除（生成代码不参与人工门禁） |
| 大 union 类型（如 error_code 原子枚举 >30 成员） | 超出默认 `--union_size_limit 30` 会退化为 `any()`，检查变松 | 调优 `--union_size_limit 100`，观察耗时变化 |

### Step 3：Go / No-Go 标准（量化）

| 指标 | Go 阈值 |
|---|---|
| OTP 28 兼容 | 3 个模块均能出结果，无崩溃 |
| lib 层告警真阳性率 | ≥ 50%（Gradualizer 定位是宽网快检，容忍度比 eqWAlizer 低一档） |
| 单文件耗时 | ≤ 5 秒（决定 pre-push 体验） |
| 与既有代码兼容 | `-dialyzer(nowarn_function)` 等豁免属性不影响其运行 |

---

## P1 — Makefile 固化与 pre-push 钩子（0.5 周）

### 1.1 Makefile 追加（完整片段，与 ELP 片段并列共存）

```make
# ==================== Gradualizer ====================
GRADUALIZER_DIR  := tools/gradualizer
GRADUALIZER      ?= $(GRADUALIZER_DIR)/bin/gradualizer
GRADUALIZER_REF  ?= master                      # 生产使用前 pin 到 commit
GRADUALIZE_BUDGET ?= 0
# gpb 生成代码与 lager 封装层不参与门禁
GRADUALIZE_EXCLUDE := src/imboy_pb.erl src/lib/elib_log.erl
GRADUALIZER_OPTS ?= -pa ebin $(addprefix -pa ,$(wildcard deps/*/ebin)) \
                    -I include --no_color --fmt_location brief

.PHONY: gradualizer-setup
gradualizer-setup: ## 拉取并构建 Gradualizer escript（pin 版本）
	@test -x $(GRADUALIZER) || { \
		git clone https://github.com/josefs/Gradualizer.git $(GRADUALIZER_DIR) && \
		git -C $(GRADUALIZER_DIR) checkout $(GRADUALIZER_REF) && \
		git -C $(GRADUALIZER_DIR) rev-parse HEAD > $(GRADUALIZER_DIR)/PINNED && \
		$(MAKE) -C $(GRADUALIZER_DIR) escript; }
	@echo "✅ Gradualizer ready ($(GRADUALIZER_REF))"

.PHONY: gradualize
gradualize: compile ## 单文件检查: make gradualize FILE=src/lib/ec_cnv.erl
	@test -n "$(FILE)" || { echo "用法: make gradualize FILE=<path.erl>"; exit 1; }
	@$(GRADUALIZER) $(GRADUALIZER_OPTS) $(FILE)

.PHONY: gradualize-layer
gradualize-layer: compile ## 分层门禁（单发模式，用于转阻塞后）: make gradualize-layer LAYER=lib
	$(GRADUALIZER) $(GRADUALIZER_OPTS) \
		$(filter-out $(GRADUALIZE_EXCLUDE),$(wildcard src/$(LAYER)/*.erl))

.PHONY: gradualize-audit
gradualize-audit: compile ## 全仓逐模块审计（预算制，用于基线期）: make gradualize-audit
	@mkdir -p .gradualizer/logs; fail=0; total=0; \
	for f in $(filter-out $(GRADUALIZE_EXCLUDE),$(wildcard src/*.erl src/*/*.erl)); do \
		case "$$f" in *_tests.erl) continue;; esac; \
		total=$$((total+1)); \
		log=.gradualizer/logs/$$(basename $$f .erl).log; \
		if ! $(GRADUALIZER) $(GRADUALIZER_OPTS) $$f > $$log 2>&1; then \
			fail=$$((fail+1)); echo "❌ $$f"; \
		fi; \
	done; \
	echo "== gradualize-audit: modules=$$total failing=$$fail budget=$(GRADUALIZE_BUDGET)"; \
	echo "gradualizer_failing $$fail" > .gradualizer/metrics.txt; \
	test $$fail -le $(GRADUALIZE_BUDGET)
```

> 两个门禁目标的取舍：`gradualize-layer` 单发调用全层文件（快，适合**已转绿层**的阻塞检查）；`gradualize-audit` 逐文件循环（慢但按模块出明细 + 产出 `metrics.txt` 趋势指标，适合**基线期**）。

### 1.2 lefthook pre-push（接管本地快检；替代 ELP 计划中的 eqwalize-changed 方案）

```yaml
# lefthook.yml 追加
pre-push:
  commands:
    gradualize-changed:
      run: |
        files=$(git diff --name-only origin/main...HEAD -- 'src/*.erl' 'src/**/*.erl' \
          | grep -v '_tests\.erl$' | grep -v 'imboy_pb\.erl$' | grep -v 'elib_log\.erl$' || true)
        [ -z "$files" ] && exit 0
        tools/gradualizer/bin/gradualizer -pa ebin -I include \
          --no_color --fmt_location brief --stop_on_first_error $files
```

> 依赖本地最近一次 `make compile` 的 `ebin/`（跨模块 spec 从 beam 读）；钩子脚本里不强制重编译以保证速度，CI 兜底正确性。

---

## P2 — CI 接入（1–2 周）：全仓基线 → 趋势指标

### 2.1 `backend-ci.yml` 追加 job（无 JVM 依赖，比 eqwalize job 轻）

::: v-pre
```yaml
gradualize:
  runs-on: ubuntu-latest
  continue-on-error: true        # ⚠️ 基线期（预计 1-2 周），lib 层转绿后分层转阻塞
  steps:
    - uses: actions/checkout@v4
    - uses: erlef/setup-beam@v1
      with: { otp-version: "28", rebar3-version: "3" }

    - name: Cache Gradualizer
      uses: actions/cache@v4
      with:
        path: tools/gradualizer
        key: gradualizer-${{ vars.GRADUALIZER_REF }}

    - name: Build Gradualizer
      run: make gradualizer-setup GRADUALIZER_REF=${{ vars.GRADUALIZER_REF }}

    - name: Compile (reuse PLT/beam cache from dialyze job)
      run: IMBOYENV=local make test-build

    - name: Gradualize full-repo audit
      run: make gradualize-audit GRADUALIZE_BUDGET=${{ vars.GRADUALIZE_BUDGET || 9999 }}

    - name: Trend metric → step summary
      run: |
        echo "### Gradualizer 类型健康度" >> "$GITHUB_STEP_SUMMARY"
        cat .gradualizer/metrics.txt >> "$GITHUB_STEP_SUMMARY"
```
:::

### 2.2 与 eqWAlizer CI 的共存规则

1. **两个 job 并存**：`gradualize`（宽网基线，continue-on-error 期长）+ `eqwalize`（分层门禁，逐层转阻塞）；
2. **转阻塞顺序**：同一层先由 Gradualizer `gradualize-layer` 转阻塞（宽松、误报少），稳定 2 周后再由 eqWAlizer 接管该层阻塞权，Gradualizer 该层降级为"仅记录"；
3. **CI summary 分开展示**：两个工具各自一段，标注" Gradualizer = 宽网快检 / eqWAlizer = 深度门禁"，避免开发者混淆告警来源。

### 2.3 预算校准（ratchet 激活值 = 首次 CI 干净基线的观测值）

> 校准动作由 **仓库 maintainer 在 GitHub 后台执行**（需设 `vars.GRADUALIZE_BUDGET`），
> 不能靠本地命令完成。本段给观测值 + 操作流程，避免本地 OTP29 与 CI OTP28 漂移误设。

#### 2.3.1 本地观测基线（2026-07-25，本地 OTP 29，Gradualizer `23533d7`）

```
make gradualize-audit GRADUALIZE_BUDGET=9999   # 不阻断，仅统计
==> modules=481  failing=266  budget=9999
```

分层 failing 分布（按目录，仅含真实类型错误，已排除 hrl-only 良性警告）：

| 目录 | modules | failing | 占比 |
|------|--------:|--------:|-----:|
| `src/api`    |  66 | 62 | 94% |
| `src/repo`   |  92 | 90 | 98% |
| `src/ds`     |  89 | 78 | 88% |
| `src/logic`  | 110 | 101 | 92% |
| `src/lib`    |  70 | 52 | 74% |
| `src/adm`    |  33 | 33 | 100% |
| `src/mcp`    |   8 |  5 | 63% |
| `src/domain` |   9 |  3 | 33% |
| **合计**     | **481** | **266** | **55%** |

> 说明：本地 481 模块含 35 个 hrl-only 良性警告模块（仅 `Failed to find include` 警告，
> 退出码 0，audit 不计入 failing）；全仓 266 failing 中绝大部分为 P3 已识别的
> 「map()/binary 推理过严 + OTP29 re/crypto 覆盖不全」工具误报，见
> `docs/gradualizer-upstream-issues.md` 的「已知误报模式」表。

#### 2.3.2 ⚠️ OTP 28 vs 29 漂移风险（校准前必读）

- CI 用 **OTP 28**（`backend-ci.yml` `erlef/setup-beam` `otp-version: "28"`），本地是 **OTP 29**。
- 已知差异：OTP 29 把 `match_alias_pats` 警告升为错误（已用 `+nowarn_match_alias_pats` 压制）；
  且 OTP 29 stdlib 大量引入 `none()` 类型，触发 Gradualizer `pick_value` 崩溃（elib_str）。
- **后果**：OTP 28 下的 failing 数可能与本地 266 不同（可能更高或更低），**不能**直接把
  本地观测值抄进 `vars.GRADUALIZE_BUDGET`，否则 CI 首跑必定误阻断或误放行。

#### 2.3.3 首次 CI 干净基线捕获流程（maintainer 操作）

1. 确认 `backend-ci.yml` 的 `gradualize` job 已合入且 `continue-on-error: true`、
   <code v-pre>GRADUALIZE_BUDGET=${{ vars.GRADUALIZE_BUDGET || '9999' }}</code>（当前已满足）。
2. 在 `main` 分支推一个空改动 / 或手动触发一次 `backend-ci.yml` 工作流。
3. 进入该次运行的 `gradualize` job → 读 step summary 里的 `gradualizer_failing N`
   （即 CI OTP28 下的真实基线，记为 `N_CI`）。
4. 仓库 Settings → Secrets and variables → Actions → Variables，新增
   `GRADUALIZE_BUDGET = N_CI`（**只准减不准增**；后续靠 P3 逐层真阳性治理把 N 往下压）。
5. 下次 CI 起，`gradualize-audit` 以 `N_CI` 为 ratchet 上界：任何 PR 使 failing 超过
   `N_CI` 即红（阻断合并），直至治理把预算调小。
6. **严禁**：本地跳过捕获直接 `git` 改 Makefile 硬编码 budget——变量必须由 CI 观测值
   驱动，否则 OTP 漂移会让门禁失效。

#### 2.3.4 当前默认状态

- `Makefile` 本地 `GRADUALIZE_BUDGET ?= 0`（本地 pre-push / 手动 `gradualize-audit`
  **会阻断** —— 这是本地零容忍约束，与 CI 的宽松基线期是两回事，不要动它）。
- CI 默认 `9999`（不阻断），**直到 maintainer 设了 `vars.GRADUALIZE_BUDGET` 才激活 ratchet**。
- 现状：仓库变量尚未设置 → Gradualizer CI job 目前是纯趋势观测，不挡合并。

---

## P3 — 协同治理（持续）

### 3.1 统一的分层推进表（与 ELP 计划共用同一张表，双工具各行其列）

| 层 | Gradualizer 阻塞 | eqWAlizer 阻塞 | 说明 |
|---|---|---|---|
| `lib` | P2 第 1 周目标 | lib 层 Gradualizer 稳定 2 周后启动 | 纯函数，两工具误报都最少 |
| `repo` | lib 转绿后 | 紧随 | SQL 返回类型对齐 epgsql spec |
| `ds` | repo 转绿后 | 紧随 | map 组装函数类型细化 |
| `logic` | ds 转绿后 | 紧随 | 消息 payload 精确 map 类型 |
| `api` | logic 转绿后 | 紧随 | cowboy 回调对齐 |
| `adm`/`mcp`/`domain` | 同模式后排 | 同模式后排 | — |

### 3.2 类型健康度 KPI（Gradualizer 独有产出）

`gradualize-audit` 每次 CI 产出 `gradualizer_failing N`：

- 目标：**N 单调不增**（新代码必须过自己这一关）；
- 每月随 spec 补齐，N 应稳定下降；纳入月度质量回顾（与 DIALYZER_WARNINGS 预算同表跟踪）。

### 3.3 调优旋钮（按需开启，非默认）

| 旋钮 | 何时开 | 代价 |
|---|---|---|
| `--infer` | spec 覆盖率 >70% 后，想覆盖无 spec 函数 | 误报率上升，需新一轮豁免治理 |
| `--solve_constraints` | 多态函数（如泛型容器工具）告警存疑时单点验证用 | 显著变慢，**不进 CI 默认** |
| `--union_size_limit 100` | 发现 error_code 等大 union 被退化为 `any()` 时 | 检查耗时上升 |
| `--specs_override_dir` | 某 dep 的 spec 错误但改不了上游时 | 维护 overlay 文件，需文档化 |

### 3.4 豁免治理

- Gradualizer 无模块级豁免属性，豁免 = `GRADUALIZE_EXCLUDE` 清单（Makefile 内，入仓可评审）；
- 与 eqWAlizer 的 `-eqwalizer(ignore).`、`ignore_modules` 统一登记在豁免台账，季度评审，目标单调不增。

---

## 4. 风险登记册（Gradualizer 特有部分，通用风险见 ELP 计划）

| 风险 | 概率 | 影响 | 缓解 |
|---|---|---|---|
| OTP 28 新语法/特性不支持 | 低-中 | 个别文件检查失败 | Gradualizer 2026-03 仍活跃提交；spike 实测；失败文件进 EXCLUDE 并报上游 issue |
| 对 `lager_transform` 模块的行为不确定 | 低 | `elib_log.erl` 误报/报错 | 已预置 `GRADUALIZE_EXCLUDE`；spike 第①步验证 |
| 全仓 audit 耗时（逐文件 escript 启动 × 400+ 模块） | 中 | CI job 10–20 分钟 | 接受（基线期 continue-on-error 不挡合并）；必要时改批量传文件单发 + 仅统计总 exit code |
| 无正式 release（pin commit 漂移） | 中 | CI 不可复现 | `GRADUALIZER_REF` pin commit + actions/cache 按 ref 做 key；升级走显式 PR |
| 与 eqWAlizer 告警重复导致开发者疲劳 | 中 | 门禁信誉下降 | 0.1 分工矩阵 + "单阻塞者"收编规则；CI summary 分区展示 |

## 5. 验收指标（Done 的定义）

- **P1 完成**：任意成员 `make gradualizer-setup && make gradualize-layer LAYER=lib` 本地通过；pre-push 钩子对变更 `.erl` 文件生效且中位耗时 <10s。
- **P2 完成**：CI `gradualize` job 全仓 audit 稳定运行，`gradualizer_failing` 指标进 step summary；lib 层 `gradualize-layer` 摘掉 `continue-on-error`。
- **P3 完成（本季度）**：lib/repo/ds 三层完成"Gradualizer 阻塞 → eqWAlizer 接管"交接；`gradualizer_failing` 较引入时下降 ≥50%。
- **北极星**：开发者本地 10 秒内拿到类型反馈（Gradualizer），CI 用生产级强度守门（eqWAlizer），Dialyzer 预算同步递减——三道防线各管一段，零告警重叠困惑。

---

### 附：对 `elp-eqwalizer-landing-plan.md` 的修订点

1. **P1-1.3 lefthook pre-push**：`eqwalize-changed` 方案**废止**，本地快检改由 Gradualizer 承担（本文 1.2）；eqWAlizer 集中在 CI 分层门禁。
2. **P2-2.2 转阻塞条件**追加第 4 条：该层 Gradualizer `gradualize-layer` 已先转阻塞并稳定 2 周。
3. 其余（`.elp.toml`、`make elp-setup/eqwalize*`、spike 标准、分层表）不变；两计划共用同一张分层推进表（本文 3.1）。
