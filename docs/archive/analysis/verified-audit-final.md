# IMBoy 可验证审计终版 / Verified Audit Final

> **日期**: 2026-06-03 | **方法**: 双模型交叉取证 + 逐条实测纠正
> **原则**: 每条结论附验证命令，从 `/Users/leeyi/project/imboy.pub` 根目录运行。
> **项目结构**: 5 模块工作区（非单一 git repo），`imboy/` 为独立 git 仓库，其余为同级目录。

---

## 〇、项目全貌

| 模块 | 语言 | 源文件数 | 职责 |
|------|------|---------|------|
| `imboy/` | Erlang/OTP | 363 .erl | HTTP/WS 后端主服务 |
| `imboyapp/` | Flutter/Dart | 1,284 .dart | iOS/Android/macOS/Windows/Linux 客户端 |
| `imboy-admin-frontend/` | React/TS | 296 .tsx/.ts | Web 管理后台 |
| `sdk/js/` | TypeScript | 11 .ts | 官方 JS SDK |
| `deploy/` | Shell/Docker | — | 生产部署 |

```bash
# 验证
cd /Users/leeyi/project/imboy.pub
ls -d imboy imboyapp imboy-admin-frontend sdk/js deploy
# 5 个目录均存在

# 各模块源文件数
for d in imboy imboyapp imboy-admin-frontend sdk/js; do
  echo "$d: $(find $d -type f \( -name '*.erl' -o -name '*.dart' -o -name '*.tsx' -o -name '*.ts' \) 2>/dev/null | wc -l | tr -d ' ')"
done
```

| 指标 | 数值 | 验证命令 |
|------|------|---------|
| 后端源码 | 363 模块 / 87,834 行 | `cd imboy && find src -name "*.erl" \| wc -l` |
| 后端测试 | 367 模块 / 102,914 行 / 7,251 断言 | `cd imboy && find test -name "*.erl" \| wc -l` |
| API 路由 | 278 条 /v1/* | `cd imboy && grep -oE '"/v1/[^"]*"' src/imboy_router.erl \| sort -u \| wc -l` |
| 数据库迁移 | 138 个 | `cd imboy && ls priv/migrations/ \| wc -l` |
| Git 历史 | 2020-05 → 2026-06（6 年） | `cd imboy && git log --reverse --format=%ai \| head -1` |
| 最高 git tag | 0.7.2 | `cd imboy && git tag \| tail -1` |
| VERSION 文件 | 1.0.0 | `cat VERSION` |
| CLAUDE.md 版本 | 1.0.0-rc.2 | `head -5 imboy/CLAUDE.md \| grep -i version` |
| 贡献者 | 1 人 | `cd imboy && git shortlog -sn --all \| head -3` |
| Flutter 客户端 | 1,284 .dart / 739 lib 文件 | `find imboyapp -name "*.dart" \| wc -l` |

---

## 一、确认的优势

### ✅ S1. 后端测试体系真实有效

- **367 测试 / 363 源码 = 1.01:1**，测试行数(102K) > 源码(88K)
- **零空断言文件**
- **Logic 测试不碰数据库**（meck 隔离，541 次 mock）
- **Domain 层测试完整**：9 个源文件对应 10 个测试文件

```bash
cd /Users/leeyi/project/imboy.pub/imboy
# 零空断言（应无输出）
find test -name "*.erl" -exec grep -c "?assert" {} + | awk -F: '$2==0{print}'
# Logic 测试无 DB（应无输出）
grep -rn "epgsql:equery\|epgsql:squery" test/logic/ --include="*.erl"
```

### ✅ S2. SQL 全参数化，零注入风险

`elib_pg:query/2` 内部调 `epgsql:equery`（参数化）。`epgsql:squery` 仅出现在 `imboy_migrate.erl`（DDL）和 `elib_pg.erl`（ROLLBACK）。

```bash
cd /Users/leeyi/project/imboy.pub/imboy
# 业务代码中的 squery（应无输出）
grep -rn "epgsql:squery" src/ --include="*.erl" | grep -v "%%" | grep -v "imboy_migrate\|elib_pg.*ROLLBACK"
```

### ✅ S3. 层间底层反向依赖为零

- **Repo → Logic**: 0 处
- **API → Repo**: 0 处

```bash
cd /Users/leeyi/project/imboy.pub/imboy
grep -rn "_logic:" src/repo/ --include="*.erl" | grep -v "%%"   # 应无输出
grep -rn "_repo:" src/api/ --include="*.erl" | grep -v "%%"     # 应无输出
```

### ✅ S4. 架构选型正确

Erlang/OTP + Cowboy + syn 做 IM 是教科书级正确（WhatsApp/ejabberd 同源）。138 个迁移说明数据模型经过 6 年持续演进。全栈覆盖（5 端 Flutter + React 后台 + JS SDK）是完整产品形态。

### ✅ S5. DDD 自我评估文档诚实

`docs/archive/architecture/ddd-rich-model-status.md` 明确承认"工程侧落地 ≠ 运行时验证"，标注真机回归未签收。这种自我批判能力在开源项目中罕见。

```bash
cd /Users/leeyi/project/imboy.pub/imboy
grep "运行时验证" docs/archive/architecture/ddd-rich-model-status.md
# 输出：工程侧落地 ≠ 运行时验证
```

---

## 二、确认的问题

### 🔴 C1. `catch _:_` 静默吞错 — 16 处（含消息编解码核心）

最危险：`imboy_codec.erl` 6 处 `catch _:_`，解码失败时返回假值（`#{}`、`0`、`<<>>`）而非错误，会无声污染消息管道。

```bash
cd /Users/leeyi/project/imboy.pub/imboy
# 总数
grep -rn "catch _:_ ->" src/ --include="*.erl" | grep -v "%%" | wc -l    # 16
# codec 核心（消息编解码器）
grep -n "catch _:_ ->" src/lib/imboy_codec.erl                            # 6 处
# 路由热更新失败被吞
grep -B2 -A1 "catch _:_ -> ok" src/lib/imboy_router_registry.erl
```

**修复**：不改逻辑，只加 `logger:warning`。30 分钟全部完成。

### 🔴 C2. 版本号三处不一致

| 来源 | 版本 | 验证命令 |
|------|------|---------|
| `VERSION` 文件 | **1.0.0** | `cat VERSION` |
| `imboy/CLAUDE.md` | **1.0.0-rc.2** | `head -5 imboy/CLAUDE.md \| grep -i version` |
| Git tag（最高） | **0.7.2** | `cd imboy && git tag \| tail -1` |

核心 IM 流程未完成真机回归（项目自评文档确认），VERSION 标 1.0.0 属虚标。

```bash
cd /Users/leeyi/project/imboy.pub
cat VERSION                           # 1.0.0
head -5 imboy/CLAUDE.md | grep -i version  # rc.2
cd imboy && git tag | tail -1         # 0.7.2
```

**修复**：统一为 `0.8.0-dev` 或 `1.0.0-rc.3`，核心流程真机签收后再标 GA。

### 🔴 C3. "单机百万并发"零压测证据

README 第一段以「单机百万并发」为头条卖点，但：
- `imboy/test/performance/` 有 5 个性能测试文件
- **没有任何 benchmark 报告**（文档/曲线/火焰图）
- 没有可复现的压测结果

```bash
cd /Users/leeyi/project/imboy.pub
# README 卖点
grep "百万" README.md | head -1
# 性能测试存在
ls imboy/test/performance/
# 无 benchmark 报告（以下应无输出）
find . -name "benchmark.md" -o -name "benchmark_*.html" -o -name "perf_report*" | grep -v deps | grep -v _build | grep -v node_modules | grep -v checkouts
```

**修复**：跑一次 `test/performance/websocket_performance_tests.erl`，出一份 `docs/guides/operations/benchmark.md`（硬件规格 + 连接数 + 内存/CPU）。哪怕只跑到 1 万连接也比零数据有说服力。**这是 ROI 最高的单项工作。**

### 🟠 C4. API Handler 大规模绕过 Logic 层直调 DS

**34 个 handler 文件**直接调用 DS 层，共 **89 个不同的 DS 函数**。最严重：

- `group_handler.erl`：29 处直调 `group_ds:*`
- `websocket_handler.erl`：12 处
- `user_handler.erl`：10 处
- `friend_category_handler.erl`：完全绕过 Logic 层

```bash
cd /Users/leeyi/project/imboy.pub/imboy
# 受影响 handler 数（排除 middleware）
grep -rl '_ds:' src/api/ --include="*.erl" | grep -v middleware | wc -l   # 34
# 最严重的文件
grep -rn '_ds:' src/api/ --include="*.erl" | grep -v "%%" | grep -v "auth_ds:" | grep -v "middleware" | sed 's/:.*//' | sort | uniq -c | sort -rn | head -5
# friend_category 完全绕过 Logic
grep "friend_category_ds:" src/api/friend_category_handler.erl | grep -v "%%"
```

**修复**：不需要立刻全改。CI 加检查脚本，新代码禁止新增违规。

### 🟠 C5. 超标文件（手写代码 > 800 行）— 6 个

| 文件 | 行数 | 性质 |
|------|------|------|
| `src/adm/adm_group_handler.erl` | 2,218 | Admin 后台，最易拆分 |
| `src/lib/imboy_policy.erl` | 2,185 | 策略引擎，职责过载 |
| `src/api/channel_handler.erl` | 999 | 新功能域 |
| `src/imboy_router.erl` | 908 | 手工路由表 |
| `src/api/websocket_handler.erl` | 847 | 消息核心热路径 |
| `src/adm/adm_channel_handler.erl` | 817 | Admin 后台 |

```bash
cd /Users/leeyi/project/imboy.pub/imboy
find src -name "*.erl" ! -name "imboy_pb.erl" -exec wc -l {} + | awk '$1 > 800 && !/total/'
```

**修复**：优先拆 `adm_group_handler.erl`（2,218→3 文件），零风险物理拆分。

### 🟠 C6. OpenAPI 文档与代码严重脱节

278 条路由中 **148 条未出现在 OpenAPI**。未文档化的包括核心端点：`passport/login`、`friend/add`、`msg/history`、`user/update`。

```bash
cd /Users/leeyi/project/imboy.pub/imboy
# 未文档化路由数
grep -oE '"/v1/[^"]*"' src/imboy_router.erl | sort -u | while read route; do
  r=$(echo "$route" | tr -d '"')
  grep -qF "$r" api/openapi.yaml 2>/dev/null || echo "$r"
done | wc -l   # 148
# 核心端点缺失
grep -oE '"/v1/[^"]*"' src/imboy_router.erl | sort -u | while read route; do
  r=$(echo "$route" | tr -d '"')
  grep -qF "$r" api/openapi.yaml 2>/dev/null || echo "$r"
done | grep -E "passport|friend|msg|user"
```

**修复**：先删掉 OpenAPI 中不存在的虚构端点（避免误导），再逐步补全核心端点。

### 🟠 C7. 仓库卫生问题

根目录存在以下不应出现的文件：

| 文件 | 大小 | 问题 |
|------|------|------|
| `erl_crash.dump` | 3.3 MB | Erlang 崩溃转储，暗示运行时崩溃过 |
| `imboy_pjyv83.db` | 256 KB | SQLite 数据库文件 |
| `imboy.sublime-workspace` | 326 KB | 个人 IDE 工作区 |
| `.DS_Store` | 10 KB | macOS 系统文件 |

注：根目录本身不是 git repo，这些文件未"入库"，但作为工作区根目录仍影响项目形象。

```bash
cd /Users/leeyi/project/imboy.pub
ls -lh erl_crash.dump imboy_pjyv83.db imboy.sublime-workspace .DS_Store
```

**修复**：删除这些文件，确认 `.gitignore` 已覆盖。

### 🟡 C8. DDD 迁移无终点线

Domain 层 9 个文件 vs Logic 层 71 个。部分 logic 标注"退化外壳"，部分仍承载完整业务。无文档说明"新代码该写在哪一层"。

```bash
cd /Users/leeyi/project/imboy.pub/imboy
echo "domain: $(find src/domain -name '*.erl' | wc -l | tr -d ' ')"   # 9
echo "logic: $(find src/logic -name '*.erl' | wc -l | tr -d ' ')"     # 71
```

**修复**：写 ADR，设定迁移截止日期和未迁移模块的最终归属。

### 🟡 C9. DS→Logic 反向调用 — 4 处

```bash
cd /Users/leeyi/project/imboy.pub/imboy
grep -rn "_logic:" src/ds/ --include="*.erl" | grep -v "%%"
# 输出：
# friend_ds.erl:215  → user_logic:batch_online_state
# friend_ds.erl:250  → user_logic:batch_online_state
# user_tag_relation_ds.erl:47  → user_tag_relation_logic:set
# user_tag_relation_ds.erl:83  → user_tag_relation_logic:delete_object_tag
```

`friend_ds` 调 `user_logic:batch_online_state` 是为了组装在线状态，属于显示层关注点，可接受但应标记。`user_tag_relation_ds` 直接委托给 logic 属于职责倒置。

---

## 三、已纠正的历史断言

以下是之前两轮评估中的错误，经从**根目录**实测后纠正：

| 原始断言 | 来源 | 纠正后事实 | 原因 |
|----------|------|-----------|------|
| "不是 monorepo，只有后端" | GLM | **❌ 是 5 模块工作区** | GLM 在 `imboy/` 子目录运行，未切到根 |
| "VERSION 文件不存在" | GLM | **❌ 根目录存在，内容 1.0.0** | 同上 |
| "ROADMAP.md 不存在" | GLM | **❌ 根目录存在，104 行** | 同上 |
| "README 无百万并发文案" | GLM | **❌ 第一段即有** | 看了 `imboy/` 内的 README 而非根 README |
| "erl_crash.dump 不存在" | GLM | **❌ 存在 3.3MB**（未入库但物理存在） | 同上 |
| "过程字典 276 处" | GLM | **❌ 高估 100x+**。`erlang:put` 仅 2 处 | 把 `maps:put` / `persistent_term` 计入 |
| "API→DS 违规 15 处" | GLM | **❌ 低估 2x+**。实际 34 handler / 89 函数 | 只数了显眼案例 |
| "squery 存在注入风险" | Opus | **❌ 无风险**。`elib_pg:query` 内部调 `equery`，`fts_user_repo` 全用 `$1/$2` | 未追踪调用链 |

---

## 四、优先级行动清单

### P0 — 今天（止血，1-2 小时）

| # | 行动 | 效果 | 工作量 | 验证 |
|---|------|------|--------|------|
| 1 | 16 处 `catch _:_` 加 `logger:warning` | 消息管道异常可见化 | 30 min | `cd imboy && grep -rn "catch _:_" src/ --include="*.erl" \| grep -v "%%" \| wc -l` 应仍 16 但每处有日志 |
| 2 | 删除根目录脏文件 | 改善第一印象 | 5 min | `ls erl_crash.dump imboy_pjyv83.db imboy.sublime-workspace` 应不存在 |
| 3 | 统一版本号为 `1.0.0-rc.3` | 消除三处矛盾 | 10 min | `cat VERSION && cd imboy && git tag \| tail -1` |

### P1 — 本周（坐实卖点 + 纪律化，4-8 小时）

| # | 行动 | 效果 | 工作量 |
|---|------|------|--------|
| 4 | 跑性能测试，出 `docs/guides/operations/benchmark.md` | **ROI 最高单项**，从零数据变为有数据 | 2-3 h |
| 5 | CI 加层边界检查脚本 | 新增违规自动阻断 | 1 h |
| 6 | 拆分 `adm_group_handler.erl`（2,218→3 文件） | 消除最大超标文件 | 2 h |
| 7 | OpenAPI 删虚构端点 | 消除误导性文档 | 30 min |

```bash
# P1-5: CI 层边界检查脚本示例
cd /Users/leeyi/project/imboy.pub/imboy
grep -rn "_repo:" src/api/ --include="*.erl" | grep -v "%%" && echo "VIOLATION: API→Repo"
grep -rn "_logic:" src/repo/ --include="*.erl" | grep -v "%%" && echo "VIOLATION: Repo→Logic"
find src -name "*.erl" ! -name "imboy_pb.erl" -exec wc -l {} + | awk '$1 > 800 && !/total/ {print "VIOLATION: " $2 " is " $1 " lines"}'
```

### P2 — 本月（收口）

| # | 行动 | 效果 |
|---|------|------|
| 8 | DDD 迁移写 ADR 定终点线 | 消除架构模糊 |
| 9 | 签收 P3 真机回归（好友状态机 / 已读回执） | 核心流程验证，这才是 1.0.0 的真正前提 |
| 10 | codec `catch _:_` 改为 `{error, Reason}` | 消除消息管道无声丢失 |

---

## 五、项目总评

| 维度 | 评分(/10) | 依据 |
|------|-----------|------|
| 技术选型 | **9** | Erlang/OTP 做 IM 是正确答案 |
| 代码工程质量 | **7** | 测试真实、SQL 安全、底层反向依赖为零；但 handler 绕层 34 个、吞错 16 处、超标 6 文件 |
| 真实成熟度 | **5** | 编译/测试/静态分析三关过；核心流程未真机验证；版本号虚标 1.0.0 |
| 可信度/品控 | **4** | "百万并发"无数据、OpenAPI 148 条脱节、版本三处矛盾、根目录有 crash dump |
| 差异化叙事 | **5** | Erlang+全 Flutter 五端+E2EE 是真亮点；但并发能力无数据坐实 |
| 可持续性 | **4** | 单人维护 5 模块 + Erlang 人才池小 |

**一句话**: 技术内核优秀、工程量扎实，但被"GA 叙事"透支了可信度。最缺三样东西：**一份压测报告、一次真机签收、一个诚实的版本号**。补上即可从"又一个自托管 IM"变成"那个真有 Erlang 并发数据的自托管 IM"。
