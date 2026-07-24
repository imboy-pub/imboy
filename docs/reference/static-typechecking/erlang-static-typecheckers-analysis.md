# Erlang 静态类型检查工具深度对比与 imboy 引入方案

> 分析对象：Gradualizer / erlang-language-platform (ELP + eqWAlizer) / etylizer
> 目标：评估三者对 imboy 后端的价值，给出可落地的自动化测试集成路线
> 日期：2026-07-24 | 分析基线：imboy@当前 main（erlang.mk + OTP 28）

---

## 1. 三个项目深度剖析

### 1.1 Gradualizer（josefs/Gradualizer）

| 维度 | 结论 |
|---|---|
| 定位 | Erlang 渐进式（gradual typing）静态类型检查器，Josef Svenningsson 主导的学术+社区项目 |
| 类型理论 | Gradual typing：**没有 `-spec` 不检查，有 spec 就严格检查，spec 越多检查越强** |
| 语法 | 复用 Erlang 标准 `-spec`，零新语法学习成本 |
| 集成方式 | CLI escript（`make escript` → `bin/gradualizer`）、rebar3 插件、Elixir 前端 Gradient、Erlang Shell API |
| 关键选项 | `--infer`（从字面量推断类型）、`--solve_constraints`（多态调用检查，默认关）、`--stop_on_first_error`、`--union_size_limit`（默认 30，性能妥协） |
| 维护状态 | **活跃**：1707 commits，最近提交 2026-03（bounded types、约束求解增强、spec 数据库扩充）；自评 "close to beta"，最新 release 准备记录为 0.3.0（2023-06） |
| OTP 要求 | ≥ OTP 21，imboy OTP 28 满足 |
| 已知限制 | 部分语言构造未支持（`test/known_problems/` 有非穷尽清单）；大 union 类型降级为 `any()`；约束求解默认关闭 |

**工作原理要点**：以模块为粒度 opt-in。检查依赖链上外部函数的 spec（从源码或 beam 的 debug info 读取，`gradualizer_db` 支持 beam），不要求全代码库标满 spec。

### 1.2 ELP / eqWAlizer（whatsapp/erlang-language-platform）

| 维度 | 结论 |
|---|---|
| 定位 | WhatsApp/Meta 出品的 Erlang 语言平台：Rust 实现的**增量语义分析库 + LSP 服务器 + CLI + linter 框架 + eqWAlizer 类型检查器**。2026-06 eqWAlizer 仓库已并入 ELP 主仓（PR #240） |
| 类型理论 | eqWAlizer：实用主义静态类型检查（非 success typing），目标是在 WhatsApp 规模代码库上**低误报地**捕获类型错误；持续改进 occurrence narrowing、overloaded specs 等 |
| 组成 | ① LSP（goto-def / references / call-hierarchy，对标 rust-analyzer）② lint 框架（W0032 等持续演进，EEP-79 native records 诊断 L1332–L1345）③ CLI（`elp eqwalize-all`、lint、shell 补全生成，clap 4） |
| 构建系统 | **官方：rebar3 + buck2**；自身已 buckified。⚠️ **erlang.mk 不在官方支持列表**，需通过 build_info/project 配置适配（可行性需 spike 验证） |
| OTP 支持 | CI 覆盖 OTP 27–29（已淘汰 26）；VS Code 扩展捆绑 OTP 27 构建 |
| 维护状态 | **非常活跃**：3483 commits，最新提交 2026-07-24；Meta 内部流程（fbshipit 同步）+ 外部 PR |
| 许可证 | Apache-2.0 / MIT 双许可 |
| 已知限制 | eqwalizer 存在已知误报边界（官方持续修）；项目级 `EqwalizeAll` IDE 功能因性能问题被移除（CLI `elp eqwalize-all` 不受影响）；旧 OTP 不支持 |

**核心优势**：三者中唯一有**超大规模生产背书**（WhatsApp 代码库）的检查器；LSP 可同时替代 imboy 现有 erlang_ls，一份投入两份收益（IDE + CI 类型门禁）。

### 1.3 etylizer（etylizer/etylizer）

| 维度 | 结论 |
|---|---|
| 定位 | 基于 **set-theoretic types**（集合论类型）的 Erlang 静态类型检查器，学术研究项目 |
| 类型理论 | Set-theoretic types：表达力最强（精确的 union/intersection/negation），理论上限高于前两者 |
| 集成方式 | rebar3 插件 `rebar3_etylizer`（`rebar3 etylizer`）；或 escript（`etylizer -P . -S src`） |
| 硬性要求 | **每个顶层函数必须有 spec**（开发者文档明确："Make sure every top-level function has a type annotation"）；需要所有 BIF/外部模块的 spec 数据库 |
| 维护状态 | 活跃但研究性质：627 commits，最近 2026-07；无 release 概念，`NOTES.md` + `test_files/` 驱动开发 |
| 成熟度判断 | **研究原型，非生产工具**：对真实大型项目的兼容性（parse_transform、宏、behaviour 回调）未经工业验证 |

### 1.4 三者和 Dialyzer 的对比坐标

```
检查强度（soundness 方向）──▶
Dialyzer        Gradualizer        eqWAlizer          etylizer
success typing  gradual typing     实用静态类型        set-theoretic
零误报/有漏报    opt-in 严格        低误报/生产级        理论最强/原型
零配置全量跑     按 spec 增量        需较完整 spec        强制全量 spec
```

四者**互补不互斥**。Dialyzer 找"确定会崩"的问题；其余三者找"类型不一致"的问题。

---

## 2. imboy 现状盘点（决定落地形态的关键事实）

| 事实 | 数据/位置 | 对引入的影响 |
|---|---|---|
| 构建系统 | **erlang.mk**（`Makefile` + `erlang.mk`，非 rebar3 主项目） | 三者的 rebar3 插件均**不能直接用**，必须走 CLI/escript 路径 |
| OTP 版本 | 28+ | 三者均满足；ELP CI 已覆盖 OTP 29 |
| spec 覆盖率 | **4893 specs / ~9609 函数 ≈ 51%**；核心模块质量不错（如 `msg_c2c_logic` 有精确 union 类型 `ok \| {reply, map()}`） | Gradualizer 可直接受益；eqWAlizer 需补 spec；etylizer 全量要求不满足 |
| parse_transform | 全局 `+{parse_transform, lager_transform}`，但业务代码统一走 `elib_log` 封装，**仅 1 个模块直接依赖 lager** | 影响可控：Gradualizer 分析源码 AST 不执行 transform；宏展开经 `log.hrl` 已隔离 |
| Dialyzer 现状 | `make dialyze` 已有（PLT 缓存 + `-Wunmatched_returns`），`DIALYZER_WARNINGS=50` 警告预算；CI 中 **continue-on-error（失败基线策略）** | 说明类型门禁尚未硬化 → 引入顺序应先收紧存量再上增量 |
| 豁免机制 | 已用 `-dialyzer({nowarn_function, ...})` | 团队熟悉基线豁免模式，可平移到 eqwalizer 豁免 |
| CI | GitHub Actions：`backend-ci.yml`（compile+快速 eunit 阻塞 / full-eunit+dialyze 基线）、`quality.yml`、xref 门禁、PLT 缓存 | 新检查可直接复用"基线→转阻塞"的既定模式 |
| Git hooks | lefthook：pre-commit（erlfmt + gitleaks）、commit-msg 校验 | 可加 pre-push 增量类型检查 |
| LSP | `erlang_ls.config`（Erlang LS） | ELP 可替换，IDE 体验升级 |

---

## 3. 价值评估：谁值得引入、引入什么

### 3.1 结论速览

| 工具 | 对 imboy 的价值 | 建议 |
|---|---|---|
| **Dialyzer（存量）** | 零新增成本的第一道防线，当前被 continue-on-error 弱化 | **Phase 0 先收紧**：警告预算 50 → 逐月递减 → 0 后转阻塞门禁 |
| **Gradualizer** | 与"渐进式 + 51% spec 覆盖率"现状**天然契合**；CLI 模式绕开 erlang.mk 限制；能查出 Dialyzer 漏掉的 spec 不一致 | **Phase 1 试点引入**（近期性价比最高） |
| **ELP / eqWAlizer** | 三者中上限最高：生产级检查 + LSP 替代 erlang_ls + lint 框架；但 erlang.mk 适配需 spike 验证 | **Phase 2 中期引入**：先 LSP 落地（IDE 价值即时兑现），再 spike eqwalize-all CI 化 |
| **etylizer** | 理论最强但成熟度不足，强制全量 spec 与现状差距大 | **不引入，仅跟踪**：等其发布稳定版 + imboy spec 覆盖率 >90% 后重新评估 |

### 3.2 Gradualizer 对 imboy 的具体价值

1. **补齐 Dialyzer 盲区**：Dialyzer 的 success typing 对 `msg_c2c_logic:c2c/3` 这类"spec 写了 `ok | {reply, map()}` 但某分支返回了 `{error, _}`"的不一致**可能漏报**；Gradualizer 对有 spec 的函数做严格双向检查，能抓到。
2. **消息链路是最大受益区**：`api/ → logic/ → ds/ → repo/` 分层清晰、payload 全是 map，字段级 typo（如 `maps:get(<<"tot">>, Data)`）目前是运行时才炸；为 `Data :: map()` 逐步细化出 `#{to := binary(), ...}` 类型的过程中，Gradualizer 是唯一"加多少 spec 给多少保障"的工具。
3. **增量无压力**：可以先只检查 `src/lib/`（纯函数工具库，无 lager、无 NIF、无 behaviour 回调），失败面可控。

### 3.3 ELP 对 imboy 的具体价值

1. **IDE 即时收益**（零风险）：替换 erlang_ls → 更准的 goto-def/references、内置 lint（如 `maps:find/2` 误用模式 W0032）、`elp generate-completions fish` 补全。1737 个 erl 文件的代码库，导航体验提升明显。
2. **CI 上限收益**（需验证）：`elp eqwalize-all` 是 WhatsApp 内部同款检查。若 erlang.mk 适配跑通，类型检查强度直接升到三者中生产验证最充分的水平。
3. **lint 框架**：可作为 elvis 之外的第二道静态检查（语义级 lint vs elvis 的风格级 lint）。

### 3.4 为什么不现在引入 etylizer

- 强制全量 spec：imboy 48% 函数无 spec，补齐工作量以月计，且 etylizer 自身对 OTP 28 新特性的支持未见验证；
- 无 release、无生产案例：CI 门禁依赖研究原型 = 把不稳定性引入交付管道；
- set-theoretic types 的表达力对 imboy 当前的 map-heavy 消息 payload 是"杀鸡用屠龙刀"，且 eqWAlizer 的 occurrence narrowing 已在覆盖同类场景。

---

## 4. 落地路线图（自动化测试集成方案）

### Phase 0 — 收紧 Dialyzer（第 1–2 周，零新依赖）

```make
# Makefile：警告预算递减机制
DIALYZER_WARNINGS ?= 50   # 每月评审下调：50 → 30 → 10 → 0
```

- CI `backend-ci.yml` 的 dialyze job：警告数 ≤ 预算才 pass；预算降到 0 的当月把 `continue-on-error` 摘掉；
- 新增的 `-dialyzer(nowarn_function)` 豁免要求 code review 说明理由（防止预算作弊）。

### Phase 1 — Gradualizer 试点（第 2–4 周）

**Step 1：工具安装（escript 路径，绕开 rebar3 插件）**

```bash
# 一次性安装到 tools/（gitignore，不入仓）
git clone --depth 1 https://github.com/josefs/Gradualizer.git tools/gradualizer
cd tools/gradualizer && make escript     # 产出 bin/gradualizer
```

**Step 2：Makefile 目标**

```make
# --- Gradualizer 渐进式类型检查 ---
GRADUALIZER ?= tools/gradualizer/bin/gradualizer
# 试点白名单：先 lib/ 纯函数，逐步扩到 ds/ → logic/ → api/
GRADUALIZER_DIRS ?= src/lib
GRADUALIZER_OPTS = -pa ebin $(addprefix -pa ,$(wildcard deps/*/ebin)) -I include \
                   --no_color --fmt_location brief

.PHONY: gradualize
gradualize: compile
	$(GRADUALIZER) $(GRADUALIZER_OPTS) $(wildcard $(GRADUALIZER_DIRS)/*.erl)
```

**Step 3：CI job（复用基线模式）**

```yaml
# .github/workflows/backend-ci.yml 追加
gradualize:
  runs-on: ubuntu-latest
  continue-on-error: true        # 首月建基线，转阻塞时移除
  steps:
    - uses: actions/checkout@v4
    - uses: erlef/setup-beam@v1
      with: {otp-version: "28", rebar3-version: "3"}
    - name: Install Gradualizer
      run: |
        git clone --depth 1 https://github.com/josefs/Gradualizer.git tools/gradualizer
        make -C tools/gradualizer escript
    - name: Build PLT cache reuse + compile
      run: IMBOYENV=local make test-build
    - name: Gradualize (whitelist)
      run: make gradualize GRADUALIZER_DIRS=src/lib
```

**Step 4：lefthook pre-push（只查变更模块，秒级）**

```yaml
# lefthook.yml 追加
pre-push:
  commands:
    gradualize-changed:
      run: |
        files=$(git diff --name-only origin/main...HEAD -- 'src/lib/*.erl' 'src/ds/*.erl')
        [ -n "$files" ] && make gradualize GRADUALIZER_DIRS="$(dirname $files | sort -u | tr '\n' ' ')" || true
```

**Step 5：白名单扩张节奏**（每周评审）：`src/lib` → `src/repo` → `src/ds` → `src/logic` → `src/api`。每扩一层，顺手把该层 exported 函数的 spec 补全（顺路做，不单独立项）。

### Phase 2 — ELP（第 1–2 月，两步走）

**Step A（零风险，本周就能做）：IDE 替换 erlang_ls**

```bash
brew install erlang-language-platform   # 或下载 release 二进制
# 编辑器指向 elp；保留 erlang_ls.config 时期可 A/B 对比
```

**Step B（需 spike）：`elp eqwalize-all` CI 化**

erlang.mk 项目不在 ELP 官方支持列表，需 1 个 spike（建议限时 2 天）验证：
1. 手写 build_info/项目描述文件让 ELP 理解 `src/{api,logic,ds,repo,lib}` + `deps/*/ebin` 布局；
2. `elp eqwalize-all` 在 `src/lib` 上跑出结果并统计误报率；
3. 误报可接受 → 接入 CI（同样先 continue-on-error）；误报爆炸 → 退回仅 IDE 用途，等 ELP 官方支持或 imboy 迁 rebar3 再评估。

eqWAlizer 支持以 `{eqwalizer, ignore}` 属性做函数级豁免，与现有 `-dialyzer(nowarn_function)` 基线模式同构。

### Phase 3 — etylizer：仅跟踪

- 订阅 etylizer release；当满足「etylizer 有稳定 release」**且**「imboy exported 函数 spec 覆盖率 ≥ 90%」**且**「Gradualizer/eqWAlizer 已稳定入 CI」三个条件时重新评估，预计 ≥ 6 个月后。

---

## 5. 风险与缓解

| 风险 | 影响 | 缓解 |
|---|---|---|
| erlang.mk 与 ELP 不兼容 | eqwalize-all 无法 CI 化 | 限时 spike 先行；失败则 ELP 仅作 IDE 工具，Gradualizer 为主力 |
| Gradualizer 对 lager_transform/宏的边角 case 误报 | 白名单扩张受阻 | `log.hrl` 已隔离 lager；遇阻模块可加入 known-issues 清单并给上游报 issue（社区活跃，2026-03 仍在修） |
| 类型检查 CI 时长 | 拖慢门禁 | Gradualizer 秒级（模块级）；PLT 缓存已有；eqwalize-all 是 Rust 增量引擎，WhatsApp 规模都在用 |
| 补 spec 演变为大重构 | 挤占业务迭代 | 白名单按层推进 + 只在新代码强制 spec（lefthook 检查新增 exported 函数必须有 `-spec`） |
| 双类型检查器警告重叠 | 开发者困惑 | 职责切分：Dialyzer 管"必崩"，Gradualizer/eqWAlizer 管"spec 一致性"；CI summary 分开展示 |

---

## 6. 一页纸结论

> **现在**：Dialyzer 警告预算 50→0 并转阻塞（零成本）。
> **本月**：Gradualizer escript 进 Makefile + CI（`src/lib` 白名单起步，continue-on-error 建基线）。
> **本季度**：ELP 替换 erlang_ls（IDE 即时收益）+ 2 天 spike 验证 `eqwalize-all` 在 erlang.mk 下的可行性，可行则按层推进 eqwalize 白名单。
> **不做**：etylizer 进 CI（研究原型 + 全量 spec 门槛），仅跟踪。
> **贯穿机制**：白名单逐层扩张 + 新增代码强制 spec + 豁免需评审——与现有 dialyzer 基线策略完全同构，团队零学习成本。
