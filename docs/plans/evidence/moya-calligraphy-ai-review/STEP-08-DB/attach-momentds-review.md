# attach 测试 moment_ds 环境失败复核 + ecron 接线验证（R6）

Owner: Agent C (DATABASE 泳道) | 日期: 2026-09-09

## 一、moment_ds 环境失败复核（任务三）

### 1. 复现命令与结果

final-report:95 的原文是 "**attach_logic_tests** 既有 moment_ds 环境失败复核"——比任务书描述的多一层：
teaching 前缀的两套件与无前缀的 attach_logic_tests 是三个文件。

```
# teaching 两套件（任务书指定复核对象）：
$ erlc -I include -o test/ test/logic/teaching_attach_logic_tests.erl test/repo/teaching_attach_integration_tests.erl
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval 'eunit:test([teaching_attach_logic_tests], ...)'
RESULT: ok   # EXIT=0（STEP-16 handoff #4 记录的 ?WITH_MECKS 编译失败已被修复）
$ erl ... eunit:test([teaching_attach_integration_tests], [verbose])
All 3 tests passed.   # 0.039/0.026/0.032s 真连 4323 库执行（非 skip 假绿；setup_conn 失败路径才返回空列表）

# 真正的失败点（第三文件）：
$ erlc -I include -o test/ test/logic/attach_logic_tests.erl   # ERLC_OK
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval 'eunit:test([attach_logic_tests], ...)'
RESULT: error   # EXIT=1；verbose: Failed 1. Skipped 0. Passed 36.
```

唯一失败用例：`attach_logic_tests:603 authorize_moment_visible_grants_test_/0-fun-2-`
（期望 `{ok,<<"https://sig">>}`，实际 `{error,forbidden}`）。另两个 moment 用例
（authorize_moment_invisible_denies / authorize_moment_missing_post_denies）"通过"——但见 §3 的假覆盖说明。

### 2. 定性：环境型（产品装配 preset 物理裁剪），非 src bug，非本轮回归

根因证据链（每步有命令输出）：

1. `include/generated/imboy_product_features.hrl`：当前编译 preset 为 **agent_hub** profile，
   `IMBOY_COMPILED_FEATURES = [bot_webhook, channel, channel_discover, channel_invitation, channel_order, core, e2ee]`
   ——**不含 moment**，无 `IMBOY_FEATURE_MOMENT` 宏定义。
   （注：config/product-feature-manifest.json 是 full-selected 含 moment 的另一份；生成产物对应 agent_hub，两份并存是 preset 机制现状。）
2. `src/logic/attach_logic.erl:467` `-ifdef(IMBOY_FEATURE_MOMENT)`——宏未定义时
   `authorize_moment_scope/2` 编译为**恒 false（fail-closed）**，这是 BUILD-00R 的有意设计（452 行注释明示）。
3. `include/generated/imboy_product_features_erlc.mk`：ERLC_EXCLUDE 物理裁剪 moment 族 11 模块
   （moment_ds/moment_logic/moment_handler/...），`ls ebin/moment_ds.beam` → No such file。
4. 测试侧 `attach_logic_tests.erl:598/617/635` 三用例 meck mock moment_ds：
   - meck:new 对不存在模块报 `{undefined_module,moment_ds}`（CRASH REPORT ×9，mock setup 失败）；
   - visible_grants 用例因 fail-closed 返回 forbidden ≠ 期望 {ok,...} → 失败；
   - denies 两用例期望本就是 forbidden，fail-closed 恰好也给 forbidden → **碰巧通过（假覆盖）**：
     它们此时验证的是裁剪分支而非 moment ACL 分支。
5. 根因完备性反证：把 `erlc +debug_info -o /tmp src/ds/moment_ds.erl` 的产物加入 code path 重跑
   （CRASH 归零、meck mock 成功），visible_grants **仍失败**——证明瓶颈不止 beam 缺失，
   attach_logic.beam 本体已编译为 fail-closed 版，环境型定性坐实、src 无 bug。

### 3. 处置：C 侧零改动 + 移交

- **teaching 两套件全绿**（任务书"修完两套件全绿"的验收对象）——无 C 侧修复需求。
- `test/logic/attach_logic_tests.erl` **不在 C 的可写清单**（任务书只授权
  teaching_attach_logic_tests / teaching_attach_integration_tests），按文件所有权保守原则未改。
- **移交 Coordinator（建议归属 B 或 feature-inventory 维护者）**，建议补丁形态：

```erlang
%% 文件头 include 生成 hrl 或运行时探测：
moment_feature_available() ->
    code:which(moment_ds) =/= non_existing.   %% 或 ifdef IMBOY_FEATURE_MOMENT 编译期守卫

authorize_moment_visible_grants_test_() ->
    case moment_feature_available() of
        true  -> ?WITH_MECKS(...原样...);
        false -> {skip, "moment feature trimmed (agent_hub preset/BUILD-00R): fail-closed branch"}
    end.
%% 三个 moment 用例（visible/invisible/missing）都建议加守卫——
%% denies 两例当前"碰巧过"属假覆盖，应 skip 或在裁剪构建下改断言 fail-closed 语义。
```

## 二、ecron cleanup_unbound 接线验证（任务二）

### 1. 事实修正：接线已存在（R6 复核发现），非零调用

任务书称"src/ 与 config/ 目前零调用——首次接入"。实际核对：

- 接线点 `config/sys.config.example:543-544`（入仓模板；sys.runtime.config 为 Makefile:28
  从模板 cp 的生成产物，.gitignore `*runtime*.config` 排除）：

```erlang
{teaching_unbound_cleanup, "23 * * * *",
    {teaching_attach_logic, run_unbound_cleanup, []}},
```

- 该条目为**未提交的工作区修改**（`git status: M config/sys.config.example`，+10 行），
  与 ownership-registry.md:94/96 中 B 认领的"ecron 一行（随 Step 11）"吻合——归 B 的收官批次产物，C 不重复接线、不改动条目本身。
- 调度语义（B 已实现，C 复核认可）：每小时 23 分（"23 * * * *"）调
  `teaching_attach_logic:run_unbound_cleanup/0`（src/logic/teaching_attach_logic.erl:150-155），
  阈值读 config `teaching_unbound_cleanup_age_hours`（默认 24，`cleanup_unbound/1` 内
  `max(?MIN_UNBOUND_AGE_HOURS=2, AgeHours)` 强制下限）。
  **保守性符合任务书要求（AgeHours≥24 默认）**；小时级轮询+24h 阈值与同文件
  attachment_pending_cleanup（"17 * * * *"+24h）同款惯例，错峰分钟位，单轮失败下小时自愈——设计合理，R6 零改动。

### 2. C 的唯一改动：修复 B 工作区修改引入的 config 语法错误

`config/sys.config.example:546`（teaching_ai_worker 条目注释）行首为 `///`——Erlang 配置注释必须
`%` 开头。实证后果：`file:consult` 返回 `{error,{546,erl_scan,{illegal,character}}}`
——**整份 sys.config 不可解析，所有 ecron 作业（含 teaching_unbound_cleanup）随配置加载失败而失效**。
修复：`///` → `%%`（附一行修复说明注释）。修复后 consult OK。

### 3. 验证输出（全绿链）

```
$ erl -noshell -eval '{ok, C} = file:consult("config/sys.config.example")'      → CONSULT_OK（修复前 error）
# 提取条目（ecron_verify 探针，/tmp 已清理）：
ECRON_JOB_OK spec=23 * * * * mfa=teaching_attach_logic:run_unbound_cleanup/0 jobs_total=7
$ erl ... ecron_spec:parse_spec("23 * * * *")
  → {ok, cron, #{second => [0], minute => [23], ...}}   # spec 合法（秒=0 分=23）
$ make compile   → MAKE_EXIT=0；sys.runtime.config 刷新（22:11，无 ///，含接线条目 ×2）
$ erl -noshell -pa ebin ... 
EXPORT_OK teaching_attach_logic:run_unbound_cleanup/0 in ebin
RUNTIME_CFG_OK 23 * * * * teaching_attach_logic:run_unbound_cleanup/0
```

未做完整节点级验证（启动 imboy app 观察 ecron 注册）——需加载全部依赖 app 与数据源，
超出最小验证边界；上述 consult + spec parse + ebin 导出 + runtime 刷新四层已证明接线就绪。

## 三、Coordinator 落地记录（R6，接 §一.3 移交）

- **归属裁量**：`test/logic/attach_logic_tests.erl` 本轮无人认领（C 可写清单未含、B 被明确禁止），由 Coordinator 直接落地。
- **实现**：新增 `moment_guarded_/1`（`code:which(moment_ds)` 探针）+ 三个 moment 用例改为
  `moment_guarded_(?WITH_MECKS(...))` 表示层包裹：裁剪时返回
  `[{"moment feature trimmed", fun() -> {skip, "..."} end}]`，可用时原样传 `{setup,...}` 表示
  （宏展开是纯数据元组，构造后丢弃安全，meck setup 不会执行）。
- **eunit 语义教训**：`{skip, Why}` **不能**作为 `_test_()` generator 的顶层返回值
  （eunit 2.11 实测：测试被 cancelled、Skipped 计 0、RESULT error，探针 /tmp/skip_probe_test 已证）；
  必须作为**测试 fun 的返回值**——仓内惯用法见 `test/integration/*_integration_tests.erl` 的
  `{"Database not available", fun() -> {skip, "..."} end}`。
- **验证**：`erlc -I include -o test/ test/logic/attach_logic_tests.erl` +
  `eunit:test([attach_logic_tests], [verbose])` → **All 37 tests passed. RESULT: ok**
  （裁剪构建下 3 条走 "(moment feature trimmed)" skip 分支；修复前为 Failed 1 / Passed 36）。
- **R8 二次修复（混合态）**：make compile 波动补编了 moment_ds.beam 而 attach_logic.beam 仍是
  fail-closed 旧态 → 原 `code:which(moment_ds)` 判据**误放行**、visible_grants 真失败。
  守卫改为 beam 级权威判据（`beam_lib:chunks(abstract_code)` 查 `can_view_post`——仅真分支调用），
  模块存在性不可靠。教训：**preset 裁剪环境的 skip 守卫必须测目标 beam 的编译产物，而非依赖模块可加载性**。


## 四、给 Coordinator 的风险提示

1. `config/sys.config.example` 的 /// 修复是 C 对 B 工作区未提交修改的最小必要干预
   （不修则 B 自己的 ecron 接线也无法生效）；teaching_ai_worker / teaching_unbound_cleanup
   两条目本体未动。B 提交前请知悉。
2. attach_logic_tests 的 moment 用例需按 preset 分层（skip 守卫），见 §3 建议补丁——待归属确认。
3. 本机存在两个 PG 实例（4323 docker 与 socket 默认实例）且都有 moya_mig_test 同名库：
  4323 为教学泳道 scratch 全量态（158 表），socket 侧为空库。后续泳道取证务必显式
  `PGHOST=127.0.0.1 PGPORT=4323`，避免误连。
