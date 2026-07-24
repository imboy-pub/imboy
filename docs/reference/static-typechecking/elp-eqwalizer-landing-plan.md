# imboy 引入 ELP / eqWAlizer 落地计划（erlang.mk 适配）

> 前置文档：`docs/erlang-static-typecheckers-analysis.md`（三工具对比分析，本文是其中 Phase 2 的展开）
> 目标：在 **erlang.mk 构建体系**下，将 WhatsApp ELP + eqWAlizer 接入 imboy 的本地开发、Git hooks 与 CI，形成渐进式类型门禁
> 日期：2026-07-24 | 状态：待执行（P0 为 2 天限时 spike）

---

## 0. 方案总览

### 0.1 接入原理（spike 前已被官方文档确认的关键事实）

| 问题 | 结论 |
|---|---|
| erlang.mk 项目能用 ELP 吗？ | **能**。官方支持非 rebar3/buck2 项目：根目录 `.elp.toml` 的 `[build_info]` 段直接描述项目结构，支持 **glob 模式**（`deps = "deps/*"`），与 erlang.mk 布局天然匹配 |
| 需要生成 project.json 吗？ | 不是必须。`.elp.toml` 内联描述即可（也可用 `file = "xxx.json"` 指向独立 JSON，二选一）。`.elp.toml` 同时是 **项目根标记**，LSP 与 CLI 共用 |
| eqWAlizer 怎么跑？ | `elp eqwalize <module>`（单模块）/ `elp eqwalize-all`（全量 apps，**deps 不参与类型检查**，正好符合预期） |
| 额外运行时依赖 | **JVM 17+**（eqWAlizer 是 Scala jar）；本地已验证 java 17.0.19 可用；CI 加 `setup-java` |
| 必需配套组件 | `eqwalizer_support`（OTP/BIF 的 spec overlay 库），从 ELP 仓 sparse-clone，作为 dep 挂进 `.elp.toml` |
| 白名单/豁免机制 | 三级：`.elp.toml` 的 `[eqwalizer] ignore_modules`、`enable_all = false` 全局开关、模块内 `-eqwalizer(ignore).` / `-typing([eqwalizer]).` 属性 |

### 0.2 总体路线

```
P0 spike（2 天）        P1 固化（1 周）         P2 CI 化（2-4 周）        P3 治理（持续）
安装+配置+三模块验证 ──▶ Makefile 目标+本地门禁 ──▶ CI 基线→白名单转阻塞 ──▶ 逐层扩张+规范
   Go/No-Go 决策           lib 层本地可跑            预算制对齐 dialyze 模式    IDE 全面切换
```

---

## P0 — 限时 Spike（Day 1–2）：验证可行性

**目标：用最小成本确认 ELP 能加载 imboy 项目并产出可信结果，做出 Go/No-Go 决策。**

### Step 1：安装 ELP

```bash
# 本地（macOS）
brew install erlang-language-platform
elp version          # 验证安装
```

> CI 用 release tarball：`elp-linux-x86_64-unknown-linux-gnu-otp-27.tar.gz`（官方说明：构建版本低于运行时 OTP 可兼容，OTP 28 运行时用 otp-27 构建即可；若已有 otp-28/29 包优先选新的）。

### Step 2：拉取 eqwalizer_support（vendored，pin 版本）

```bash
cd ~/project/imboy.pub/imboy
mkdir -p .elp
git clone --depth 1 --filter=blob:none --sparse \
  https://github.com/WhatsApp/erlang-language-platform.git .elp/elp-repo
git -C .elp/elp-repo sparse-checkout set eqwalizer/eqwalizer_support
# 记录 pin 的 commit，写进 .elp/PINNED（供 CI 缓存 key 与可复现）
git -C .elp/elp-repo rev-parse HEAD > .elp/PINNED
```

`.elp/` 加入 `.gitignore`（工具产物，不入仓）。

### Step 3：编写 `.elp.toml`（入仓，git 跟踪）

```toml
# .elp.toml — ELP/eqWAlizer 项目配置（imboy 使用 erlang.mk，非 rebar3）
# 本文件同时是 ELP 项目根标记：LSP 与 CLI 共用同一份项目描述。

[build_info]
# 主 app：erlang.mk 多 src 子目录布局，需显式列出
apps = [
  { name = "imboy", dir = ".",
    src_dirs       = ["src", "src/adm", "src/api", "src/domain", "src/ds",
                      "src/lib", "src/logic", "src/mcp", "src/repo"],
    extra_src_dirs = ["test", "test/adm"],   # 测试目录（IDE 导航用；eqwalizer 默认跳过 test 模块）
    ebin           = "ebin",
    include_dirs   = ["include"] },
]
# 第三方依赖：glob 展开为 dep apps（只做索引/跳转，不做类型检查）
# 第二项是 eqwalizer_support（OTP/BIF spec overlay，eqwalizer 必需）
deps = ["deps/*", ".elp/elp-repo/eqwalizer/eqwalizer_support"]

[eqwalizer]
enable_all     = false              # 白名单阶段：不默认全量；逐层/逐模块开启
max_tasks      = 4                  # eqwalizer 实例内存密集，小机器调低到 2
ignore_modules = ["elib_log"]       # lager parse_transform 封装层，豁免

[otp]
exclude_apps = ["megaco", "diameter", "wx", "et", "debugger", "observer"]   # 减少索引量
```

**Spike 验证项（逐条打勾）：**

```bash
IMBOYENV=local make compile        # 先有 ebin/ 产物（deps beam 供交叉引用）
elp eqwalize imboy_env             # ① 最小模块：验证项目加载 + JVM 链路
elp eqwalize ec_cnv                # ② lib 层纯函数模块：验证业务代码
elp eqwalize msg_c2c_logic         # ③ logic 层复杂模块：验证真实告警质量
```

逐条记录：能否跑通 / 告警条数 / 人工判定的真阳性率 / 单模块耗时。

### Step 4：Go / No-Go 决策标准（量化）

| 指标 | Go 阈值 | 说明 |
|---|---|---|
| 项目加载 | 3 个模块均能出结果 | 加载失败 → 检查 `include` 解析、`-include_lib` 对 deps 的解析 |
| lib 层告警真阳性率 | ≥ 60%（人工抽 10 条） | 低于此说明误报淹没信号，No-Go 转 IDE-only |
| 单模块耗时 | ≤ 60s（含 JVM 冷启动） | 决定 lefthook 是否可行 |
| spec 缺失报错占比 | 可豁免/可忽略 | 若大量报"缺 spec"，策略改为只查有 spec 函数 |

**预案**：若 `elp eqwalize` 报找不到 eqwalizer jar → 设置环境变量后重试：
```bash
export ELP_EQWALIZER_PATH=$(find .elp/elp-repo -name 'eqwalizer*.jar' | head -1)
export EQWALIZER_DIR=$PWD/.elp/elp-repo/eqwalizer/eqwalizer_support
```
若 jar 不在 repo 中（repo 里是源码），则从 release tarball 提取 jar，或在 spike 中 `sbt assembly` 构建一次并把 jar 缓存到 `.elp/`。

---

## P1 — Makefile 固化与本地门禁（Week 1）

**目标：`make` 一条龙，团队成员零记忆成本；白名单从 `src/lib` 起步。**

### 1.1 Makefile 追加（完整片段，直接可用）

```make
# ==================== ELP / eqWAlizer ====================
ELP ?= elp
ELP_REPO_DIR := .elp/elp-repo
EQWALIZER_SUPPORT := $(ELP_REPO_DIR)/eqwalizer/eqwalizer_support
# 分层白名单：lib → repo → ds → logic → api（逐层开启）
ELP_LAYERS := lib repo ds logic api
# 失败预算：与 DIALYZER_WARNINGS 同模式，逐层清零
EQWALIZE_BUDGET ?= 0

.PHONY: elp-setup
elp-setup: ## 校验 elp + JVM，拉取/更新 eqwalizer_support
	@command -v $(ELP) >/dev/null || { echo "❌ 未找到 elp，请执行: brew install erlang-language-platform"; exit 1; }
	@command -v java >/dev/null   || { echo "❌ eqWAlizer 需要 JVM 17+"; exit 1; }
	@if [ ! -d "$(EQWALIZER_SUPPORT)" ]; then \
		mkdir -p .elp && \
		git clone --depth 1 --filter=blob:none --sparse \
		  https://github.com/WhatsApp/erlang-language-platform.git $(ELP_REPO_DIR) && \
		git -C $(ELP_REPO_DIR) sparse-checkout set eqwalizer/eqwalizer_support && \
		git -C $(ELP_REPO_DIR) rev-parse HEAD > .elp/PINNED; \
	fi
	@echo "✅ ELP ready ($(shell $(ELP) version 2>/dev/null || echo unknown))"

.PHONY: eqwalize
eqwalize: ## 单模块检查: make eqwalize MOD=msg_c2c_logic
	@test -n "$(MOD)" || { echo "用法: make eqwalize MOD=<module>"; exit 1; }
	@$(ELP) eqwalize $(MOD)

.PHONY: eqwalize-layer
eqwalize-layer: ## 分层检查: make eqwalize-layer LAYER=lib
	@test -n "$(LAYER)" || { echo "用法: make eqwalize-layer LAYER=$(ELP_LAYERS)"; exit 1; }
	@mkdir -p .elp/logs; fail=0; total=0; \
	for f in src/$(LAYER)/*.erl; do \
		m=$$(basename $$f .erl); \
		case "$$m" in *_tests) continue;; esac; \
		total=$$((total+1)); \
		if ! $(ELP) eqwalize $$m > .elp/logs/$$m.log 2>&1; then \
			fail=$$((fail+1)); echo "❌ $$m (详见 .elp/logs/$$m.log)"; \
		fi; \
	done; \
	echo "== layer=$(LAYER) modules=$$total failing=$$fail budget=$(EQWALIZE_BUDGET)"; \
	test $$fail -le $(EQWALIZE_BUDGET)

.PHONY: eqwalize-all
eqwalize-all: ## 全量检查（CLI 路径，供 CI 使用）
	@$(ELP) eqwalize-all
```

### 1.2 配套维护规则（防腐化）

1. **新增 `src/` 子目录时**必须同步 `.elp.toml` 的 `src_dirs`——写入 PR 模板检查项；同时 `make elp-setup` 加一行校验：
   ```bash
   @diff <(ls -d src/*/ | xargs -n1 basename | sort) \
         <(grep -o 'src/[a-z]*' .elp.toml | cut -d/ -f2 | sort -u) || \
      echo "⚠️ .elp.toml 的 src_dirs 与实际目录不一致"
   ```
2. `.elp.toml` 入仓；`.elp/` 入 `.gitignore`；`PINNED` 升级走显式 PR（与 deps 升级同级评审）。

### 1.3 lefthook pre-push

> ⚠️ **修订（2026-07-24）**：本地 pre-push 快检改由 **Gradualizer** 承担（escript 秒级冷启动、无 JVM 依赖，体验显著优于每模块启动一次 eqWAlizer JVM）。eqWAlizer 集中在 CI 分层门禁。分工详见 `docs/gradualizer-landing-plan.md` 的"双引擎分工矩阵"。以下配置仅在不引入 Gradualizer 时作为备选保留：

```yaml
# lefthook.yml（备选方案：不引入 Gradualizer 时使用）
pre-push:
  commands:
    eqwalize-changed:
      run: |
        mods=$(git diff --name-only origin/main...HEAD -- 'src/lib/*.erl' 'src/repo/*.erl' \
          | xargs -n1 basename 2>/dev/null | sed 's/\.erl$//' | grep -v '_tests$' || true)
        for m in $mods; do elp eqwalize "$m" || exit 1; done
```

---

## P2 — CI 接入（Week 2–4）：基线 → 阻塞

**模式完全复用现有 dialyze 策略：`continue-on-error` 建基线 → 预算递减 → 摘掉转阻塞。**

### 2.1 `backend-ci.yml` 追加 job

```yaml
eqwalize:
  runs-on: ubuntu-latest
  continue-on-error: true        # ⚠️ 基线期（预计 2-4 周），转阻塞时移除此行
  steps:
    - uses: actions/checkout@v4

    - uses: erlef/setup-beam@v1
      with: { otp-version: "28", rebar3-version: "3" }

    - uses: actions/setup-java@v4   # eqWAlizer 是 Scala jar，必须 JVM
      with: { distribution: temurin, java-version: "17" }

    - name: Cache ELP + eqwalizer_support
      uses: actions/cache@v4
      with:
        path: |
          /usr/local/bin/elp
          .elp
        key: elp-${{ vars.ELP_VERSION }}-support-${{ hashFiles('.elp/PINNED') }}

    - name: Install ELP
      run: |
        curl -sL "https://github.com/WhatsApp/erlang-language-platform/releases/download/${{ vars.ELP_VERSION }}/elp-linux-x86_64-unknown-linux-gnu-otp-27.tar.gz" | tar xz
        sudo mv elp /usr/local/bin/ && elp version

    - name: Compile (reuse PLT cache steps from dialyze job)
      run: IMBOYENV=local make test-build

    - name: eqWAlize lib layer
      run: make elp-setup eqwalize-layer LAYER=lib EQWALIZE_BUDGET=${{ vars.EQWALIZE_BUDGET_LIB || 0 }}
```

> `ELP_VERSION` 用仓库变量 pin 住（如 `2026-07-XX`），升级走 PR；复用 dialyze job 已有的 PLT/beam 缓存段，避免重复编译。

### 2.2 转阻塞条件（全部满足才摘 `continue-on-error`）

1. 该层 `failing=0` 连续 2 周稳定；
2. 该层所有豁免（`ignore_modules` / `-eqwalizer(ignore).`）都有 issue 链接；
3. 单 job 耗时 ≤ 5 分钟（超时则拆 matrix 按层并行，或先只跑变更模块）；
4. **该层 Gradualizer `make gradualize-layer` 已先转阻塞并稳定运行 2 周**（双引擎交接规则，见 `docs/gradualizer-landing-plan.md`）。

---

## P3 — 扩张与治理（Month 1–3）

### 3.1 分层推进表（准入条件驱动，非日历驱动）

| 顺序 | 层 | 模块量级 | 准入条件（上一层达标才启动） | 典型工作 |
|---|---|---|---|---|
| 1 | `lib` | ~40 | spike Go | 补纯函数 spec，误报最少 |
| 2 | `repo` | ~30 | lib 层 CI 转阻塞 | SQL 返回类型对齐 epgsql spec |
| 3 | `ds` | ~50 | repo 转阻塞 | 缓存/组装函数的 map 类型细化 |
| 4 | `logic` | ~80 | ds 转阻塞 | 消息 payload 的精确 map 类型（`#{to := binary(), ...}`） |
| 5 | `api` | ~60 | logic 转阻塞 | handler 返回值与 cowboy 回调对齐 |

> `adm`/`mcp`/`domain` 排在 api 之后按同样模式推进；每层推进时顺手把该层 exported 函数 spec 补齐（顺路做，不单独立项）。

### 3.2 豁免治理（与现有 `-dialyzer(nowarn_function)` 评审同级）

- 新增 `-eqwalizer(ignore).` / `ignore_modules` 条目：PR 描述必须附原因 + issue 链接；
- 每季度评审豁免清单，能修则修（目标：豁免数单调不增）。

### 3.3 新代码规则（lefthook 脚本，防新增债）

```bash
# 新增 exported 函数必须带 -spec（简化版检查，放 pre-push）
git diff origin/main...HEAD -U0 -- 'src/**/*.erl' | grep -E '^\+-export' >/dev/null && \
  git diff origin/main...HEAD -U0 -- 'src/**/*.erl' | grep -qE '^\+-spec' || \
  { echo "⚠️ 新增导出函数请补充 -spec"; exit 1; }
```

### 3.4 IDE 切换（零成本即时收益，与 CI 进度解耦）

- `.elp.toml` 已入仓 → VS Code 装 ELP 扩展 / 其他编辑器指向 `elp` 二进制即用；
- `erlang_ls.config` 保留一个季度作为回退，之后下线（避免双 LSP 告警打架，过渡期二选一）。

---

## 4. 风险登记册

| 风险 | 概率 | 影响 | 缓解 |
|---|---|---|---|
| eqWAlizer 对 `lager_transform` 全局编译选项的边角误报 | 中 | 个别模块告警噪音 | ELP 不执行 parse_transform（只做语法分析），实测仅 `elib_log` 直接依赖 lager，已入 `ignore_modules`；spike 验证 |
| CI runner 内存不足（eqwalizer 实例内存密集） | 中 | job OOM | `max_tasks = 2`；按层 matrix 拆分；必要时 `continue-on-error` 期间观察 |
| ELP release 与 OTP 28 兼容性问题 | 低 | 类型检查结果异常 | 官方承诺向后兼容（otp-27 构建可跑 OTP 28 项目）；spike 实测；pin 版本 + 缓存 |
| `deps/*` glob 把 `BUILD_DEPS`（relx/gpb/bbmustache）也索引 | 低 | 索引变慢 | 无害（不参与类型检查）；必要时在 `[build_info]` 用"最后条目胜出"规则覆盖排除 |
| 团队成员未装 JVM 导致本地跑不了 | 中 | 本地门禁失效 | `make elp-setup` 前置校验并给出安装提示；lefthook 失败信息附引导 |
| 白名单扩张演变为大重构 | 中 | 挤占业务迭代 | 准入条件驱动（非日历驱动）；每层只做"spec 补齐+告警清零"，禁止顺手重构 |

## 5. 验收指标（Done 的定义）

- **P1 完成**：任意成员 `make elp-setup && make eqwalize-layer LAYER=lib` 本地通过；pre-push 钩子生效。
- **P2 完成**：CI `eqwalize` job 在 lib 层摘掉 `continue-on-error`，成为合并阻塞门禁。
- **P3 完成（本季度）**：lib/repo/ds 三层转阻塞；全量失败预算单调递减；IDE 完成 erlang_ls → ELP 切换。
- **北极星**：消息主链路（api→logic→ds→repo）核心模块 eqWAlizer 零告警，新增导出函数 100% 带 `-spec`。

---

### 附：与上一轮分析文档的差异说明

`erlang-static-typecheckers-analysis.md` 中将"erlang.mk 适配 project.json"标记为**待 spike 验证**；本轮已依据官方文档（Custom Project / `.elp.toml` 章节）确认：`[build_info]` 的 glob 模式直接覆盖该场景，**可行性风险从"中"降为"低"**，P0 spike 预期通过率高。spike 保留的目的是验证**告警质量**（真阳性率）而非**能否跑通**。
