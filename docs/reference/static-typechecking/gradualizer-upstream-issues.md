# Gradualizer 上游 Issue 草稿 + 误报决策日志（josefs/Gradualizer）

> 背景：imboy 项目（Erlang/OTP 29 + erlang.mk）引入 Gradualizer 作类型检查基线。
> 本文件含两部分：
> 1. **上游 Issue 草稿**（Issue 1/2）—— 可复现的 Gradualizer 缺陷，待提交
>    https://github.com/josefs/Gradualizer/issues
> 2. **误报决策日志**（下方「已知误报模式」）—— 经逐代码核验确认的 Gradualizer
>    类型推理过严 / OTP29 覆盖不全导致的**误报**，本地决策为「不修」，避免把
>    工具局限误当真阳性而制造 churn。另含已修复真阳性的快速索引。

测试环境：
- Gradualizer commit: `23533d7`（pin 于 `tools/gradualizer/PINNED`）
- OTP: 29（本地）；CI 用 28
- 构建方式：`make escript ERLC_OPTS="...+nowarn_match_alias_pats"`（OTP 29 把 match_alias_pats 警告升为错误，见 issue 2）
- 本地运行单文件：`make gradualize FILE=src/logic/xxx.erl`（Makefile 已固化 `-I include` / `--no_color` / deps ebin 路径）

---

## Issue 1：`pick_value/2` 缺少 `none()` 子句 → 分析 `none()` 类型模块时崩溃

### 复现

对任意包含 `none()` 类型（`-spec` 或类型展开后出现）的模块运行：

```
gradualizer src/lib/elib_str.erl   # imboy 内部模块
```

### 崩溃栈（节选）

```
escript: exception error: no function clause matching
             gradualizer_lib:pick_value({type,0,none,[]},
                                        {env, ...})
```

### 根因

`gradualizer_lib.erl` 的 `pick_value/2` 列出了 `integer`/`atom`/`union`/`tuple`/
`record`/`map`/`list` 等子句，但**没有 `none()`** 的匹配分支。
`none()` 是 OTP 28+ 引入的"不可能类型"（等价于"无值"），类型展开时遇到即抛
`function_clause`。

OTP 29 的 stdlib 中大量 spec 使用 `none()`（如某些 `@doc` 标注的"此函数不返回"），
导致任何引用了这些 spec 的模块都会被波及。

### 建议修复

在 `pick_value/2` 子句集中增加：

```erlang
pick_value(?type(none), _Env) ->
    {atom, erl_anno:new(0), undefined};   % 或适合 represent "no value" 的 AST
```

或加一个 catch-all 默认子句避免未处理类型直接崩溃：

```erlang
pick_value(_Ty, _Env) ->
    {atom, erl_anno:new(0), undefined}.
```

### 影响范围

- 所有在 OTP 28+ 下、类型图中出现 `none()` 的项目均会崩溃
- 当前临时规避：`GRADUALIZE_EXCLUDE` 排除触发模块（imboy 已排除 `src/lib/elib_str.erl`）

---

## Issue 2：含 `-include` / `?MACRO` 的模块报 `undefined` / 误报警告（hrl 依赖未解析）

### 现象

对通过 `-include("xxx.hrl")` 引入宏/记录的模块运行 Gradualizer 时，
宏展开后的表达式会报 `undefined function` 或类型不匹配误报，
即使该宏/记录在 hrl 中已正确定义且项目可正常编译。

### 预期

Gradualizer 应尊重 `-I include` 路径并先展开 `?MACRO` / `?RECORD` 再分析，
与 erlc 编译行为一致（我们在 CLI 已传 `-I include`）。

### 复现线索

- imboy 项目 `src/` 与 `include/` 分离（erlang.mk 布局）
- 带 `-include` 的模块告警数明显高于纯 src 模块
- 传 `-I include` 后部分消除，但宏内联的类型仍会误报

### 建议

- 确认 hrl 搜索路径在类型展开阶段（pre-parse）已生效
- 对 `?MACRO` 调用结果做 `any()` 悲观推断时不应产生 `undefined function` 硬告警

---

---

## 已知误报模式（决策：不修，记录下来防止重复研判）

> 以下均经「运行 gradualize → 读源码 → 确认真伪」三步验证，确认是 Gradualizer
> 对 OTP29 / re / crypto / binary / map 联合类型的推理局限或覆盖不全，而非代码
> 真实缺陷。标注**模块:行号 / 表现 / 根因 / 决策**。

### 类型推理过严（map / binary）

| 模式 | 表现 | 根因 | 出现位置 |
|------|------|------|----------|
| `map()` vs `#{any()=>any()} \| {error,any()}` | 期望 `map()` 但推断为 `map()\|{error,any()}` | DS 返回类型是 map union error，调用点已保证非 error 分支，Gradualizer 跨函数联合类型不收窄 | `moment_logic:68/174/218/358`（get_post 成功前置） |
| `binary()` vs `<<_:_*8>>` | 期望 `binary()` 但推断为 `<<_:_*8>>` | OTP29 `binary()` 与位串字面类型的子类型识别差异 | `friend_logic:48/82/140`（to_rfc3339 / jsone:decode 结果） |
| `#{any()=>any(), _=>_}` vs `json_value()` | maps:get 期望 map 但推断含 json_value() | `jsone:decode` 返回 `json_value()` 联合类型，内部已 is_map 守卫 | `friend_logic:203`、`user_collect` 修复前同模式 |
| `binary()\|pos_integer()` vs `integer()` | user_logic:find_by_id 入参推断不匹配 | `ec_cnv:to_binary` 返回类型标注偏窄 | `friend_logic:279/338` |
| `maybe_improper_list(byte()\|binary()\|iolist) ...` vs `unicode:chardata()` | `list_to_binary(string:trim(...))` 推断不匹配 | `unicode:chardata()` 与 `iolist()` 子类型过严 | `moment_logic:708`（normalize_non_empty_binary） |
| `binary()` vs `<<_:_*8>>` | 字面值 binary 推断差异 | 同上 | `moment_logic:914` |

### 不可达子句误报（`clause cannot be reached`）

根因：guard + catch-all + 字面子句组合时 Gradualizer 的不可达分析过度保守，
删掉防御性 `_` 分支会损失健壮性。抽样均为误报：
- `moment_logic:730`（media_object_keys/1 的 `_` 兜底，与上一条 L729 的 `is_list` 守卫并存，工具误判不可达）
- `payment_gateway:58` / `payment_stripe_gateway:86` / `user_device_logic:150` / `msg_burn_logic:84`（guard + catch-all）

### OTP / re / crypto / binary / 位运算 覆盖不全

| 模块:行 | 表现 | 根因 |
|---------|------|------|
| `elib_req` | `<<0:8>>` / `{0,0}` | `binary` 模式 + `when is_binary` guard 组合；`binary:match` 返回类型推导缺陷 |
| `elib_oss:341` | `{error, match_limit}` | `re:run` 无 `match_limit` 选项不会返回，Gradualizer 把整个 re_run 联合类型算入 |
| `elib_cipher:536` | `{<<>>,<<>>}` | `crypto_one_time_aead` decrypt 模式返回类型推导，未区分 encrypt/decrypt |
| `imboy_frame:278` | `-1` | `band`/`bsr` 位运算类型推导，`F band MASK` 非负 |
| `imboy_migrate:150` | `""` 模式 | `binary_to_list` 被推断 `[byte()]`，空 list 误报 |
| `elib_type:97/114` | `re:run` `<<_:_*8>>` | re 模块 spec 接受 binary\|string，Gradualizer 收窄为 binary |
| `msg_rate_logic:122` / `eunit_runner:200` | `Undefined type time/0` | Gradualizer 对 `non_neg_integer()` 内置类型解析异常（文件中无 time()） |
| `group_schedule_logic:406` | `RemindBefore*60` 的 `undefined` | case 子句 `_` 分支未收窄（同函数 399/401 已 guard） |

### 暂缓（deferred，冻结模块）

- `imboy_plugin_loader:77`：`filename:join(code:priv_dir(...),...)` 真实类型不匹配
  （priv_dir 可能返回 `{error,bad_name}`）；但该模块标注 **FROZEN (roadmap-only)**，
  冻结模块不应改业务逻辑，列入 deferred 清单，待模块解冻时处理。
- `friend_logic` / `moment_logic` 的 `map()` 收窄类：跨函数 spec 协调改动面大，
  留待「专门 map 治理轮」或 eqWAlizer 转阻塞时协同处理（Gradualizer 对 map 联合
  类型推断过严，单独修会牵动多处 spec）。

---

## 已修复真阳性快速索引（反例，便于对照"什么算真阳性"）

| commit | 模块 | 真阳性类型 | 风险 |
|--------|------|-----------|------|
| `f56a8cd1` | elib_log / elib_dt / msg_c2c_logic | spec 与实现不符（1 参 term()/整数返回 binary→integer） | 调用方类型不一致 |
| `2e609e63` | qianfan_api | `uri_string:parse/1` 未处理 `{error,_}` 直接 maps:get | 潜在 badmap 崩溃 |
| `9b78b103` | elib_type / eunit_runner | spec 漏 undefined / 返回 3 元组未声明 | 调用方类型不一致 |
| `831b0c2c` | friend/group/group_schedule/login_*/moment_ds/push/msg_burn/user_collect(logic+ds) | spec 漏 undefined / DS 返回不一致 / null+integer 未 guard / 死分支 | msg_burn 属真实 badarg 崩溃风险 |
| `f5cc5d29` | user_collect_logic | `jsone:decode` 解出非 map 时 downstream maps:get 抛 badmap；`ensure_map` 返回类型标错 | **真实 badmap 运行时崩溃** |

判定口诀：**spec 与实现不符 / 真实崩溃路径（null/undefined/非 map 未 guard）→ 修；
工具对 OTP29/re/crypto/map 联合类型的推理局限 → 记误报不修。**

---

## eqWAlizer lib 层全量扫描（R8，2026-07-25）

> 目的：把双引擎的另一半（eqWAlizer，set-theoretic types，是 CI 真正的阻塞门禁）
> 在 lib 层整层跑一遍，挖「Gradualizer 漏报、eqWAlizer 捕获」的真阳性。
> 结果：**36/70 模块、140 errors、0 真阳性** —— 全部为工具误报 / 上游 overlay 局限 / 控制流近似噪声。

### 运行方式（规避 /tmp jar race）
`elp` 并行批量会触发「Eqwalizer exe has disappeared」→ jar 被回收 → 崩溃。
**必须逐个模块顺序跑**（70 个 × 约 1s）：

```bash
cd imboy
for m in $(ls src/lib/*.erl | sed 's|src/lib/||;s|\.erl||'); do
  elp eqwalize "$m" 2>/dev/null
done
```

### 误报分类（按根因分组）

| 误报根因 | 代表模块:行 | 说明 | 决策 |
|----------|------------|------|------|
| **gen_server:start_link 返回 `start_ret()` 含 `'ignore'`** | billing_invoice_worker:29 / elib_metric:47 / imboy_cache_sync:28 / imboy_plugin_loader:82 / imboy_router_registry:63 / imboy_ws_action_registry:68 / license_notice_worker:32 / olm_otk_cleanup_worker:39 | `start_ret()` = `{'ok',pid()}\|{'error',term()}\|'ignore'`；本地 gen_server init 永不返回 ignore，但 eqWAlizer 按 spec 全量 union 报 | **记录不修**（运行时 ignore 不可达） |
| **`unicode:characters_to_binary/1` 返回含 `{error,\|incomplete}` 元组** | imboy_env:77 / imboy_feature:75 / imboy_policy_codec:215 / imboy_policy:641/672/690/705 | OTP 返回 `binary()\|{error,...}\|{incomplete,...}`；输入 is_list/atom 保证成功，但 eqWAlizer 不收窄 | **记录不修**（输入已 guard） |
| **`ec_cnv`/`jsone` 等第三方：内部 `term()` 向外漏** | elib_param:132/252 / elib_cnv:34/48/73/135/137/155/179 / elib_pg:*/ elib_response / elib_policy* / imboy_plugin_manager:52 / imboy_plugin_signature:61 / qianfan_api:** | 错误核心都指向 `Expression has type: term()` → 上游 `eqwalizer_support` overlay 未给 erlware_commons / epgsql / cowboy / jsone 标注精确类型，导致内部 `term()` 渗出污染调用方 | **记录不修**（overlay 局限，非本仓 bug） |
| **`cowboy_req:parse_qs` 返回含 `'true'` 的 2 元组** | elib_param:132/252 | `parse_qs` 单值键返回 `{K,'true'}`；业务用 `ec_cnv:to_binary('true')`→`<<"true">>`（已实测不崩），但 eqWAlizer 按 spec union 报 | **记录不修**（runtime 实测 `to_binary(true)`=`<<"true">>`） |
| **`crypto`/`public_key`/`re` spec 严格度（OTP 29 overlay 过窄）** | elib_cipher:74/89/132/133/154/197/241/263/264/326/352/357/574/575/606/607 / elib_type:30/97/114 / imboy_license:115 / imboy_migrate:84/137 | `cipher_iv()`/`rsa_public_key()`/`hash_algorithm()`/`re:mp()` 等联合类型未覆盖运行时合法原子 → 上游 overlay 对 OTP29 crypto 标注不全 | **记录不修**（同 Gradualizer `crypto` 误报类，overlay 局限） |
| **`string:join` 要求 `[string()]` 但 Pairs 元素可能为嵌套 list** | elib_cnv:34 `map_to_query` | 实际 3 个调用点（user_logic:416 / elib_uri:46 / passport_handler:109）均传标量 map（整数/二进制），`ec_cnv:to_list` 恒返回扁平 char-list，嵌套不可达；误报源自 erlware_commons `to_list/1` 接受 `list()` 入参 | **记录不修**（调用点数据形状已确认） |
| **闭 union 的 normalize_* 函数返回 `atom()` 不符闭集** | imboy_policy_normalize:183/211/233 | `_ -> Default`（Default 也是调用点传入的闭集原子），eqWAlizer 按 `Default :: atom()` 报不符闭集；实际 Default 与闭集同源 | **记录不修**（控制流同源） |
| **`scope_segment(<<"group">>, ScopeRef)` ScopeRef 可能为 undefined** | elib_oss:171 | spec 为 `binary()\|undefined`，但 group 子句仅由带真实 Gid 的调用点进入，undefined 不可达 | **记录不修**（调用点形状已确认） |
| **`license_expiry_notice:due_threshold` 返回含 `none` 被 `send_notices` 排除** | license_notice_worker:79 | 调用前 `should_send(none,_)->false` 已保证传入值≠none；eqWAlizer 不跨函数收窄 | **记录不修**（控制流已 guard） |
| **`elib_async:async_with_timeout` TimeoutMs::timeout() vs number()** | elib_async:144 | 未找到任何传 `infinity` 的调用点；`infinity` 是合法 timer 值，且函数无外部调用 → 即便传也不会崩 | **记录不修**（未触达 + infinity 合法） |

### 关键结论（给 maintainer）
1. **eqWAlizer 在 lib 层目前不能作为阻塞门禁** —— 140 条里 0 真阳性、绝大多数是「第三方 overlay 未标精确类型导致 `term()` 渗出」。贸然 block 会逼出大量无意义 churn（给每个调用点加 `expect`/强制类型断言）。
2. **Gradualizer 仍是无 overlay 依赖的「宽网基线」** 主力（已 6 个真阳性 commit）；eqWAlizer 的真价值在 **有精确 overlay 的模块**（如已修的 `elib_dt`）。
3. **唯一这次实测抓到的 eqWAlizer 真阳性仍是 `elib_dt:rfc3339_to`**（R7，`31147564`）—— 因为它只依赖 OTP stdlib spec，overlay 覆盖好。印证「eqWAlizer 在 OTP stdlib-only 路径上强、在第三方依赖路径上被 overlay 拖后腿」。

### 后续可选项（非必须）
- 给 erlware_commons / epgsql / cowboy / jsone 写本地 `eqwalizer_support` 覆盖（标注 `to_binary/to_list` 等返回精确类型）→ 可消掉 ~60% 的 `term()` 渗出误报，之后才能谈 eqWAlizer 在 lib 层转阻塞。
- 或维持 eqWAlizer `continue-on-error` 仅记录，等上游 overlay 自然完善。

---

## eqWAlizer api / repo 层全量扫描（R9，2026-07-25）

> 目的：在 lib 层 0 真阳性之后，扫 api（handler 层）与 repo（数据层）找
> 「OTP stdlib-only 路径上的真阳性」（理论上比 lib 更接近 stdlib，第三方依赖少）。
> 结果：**api 42/66 模块 240 errors，repo 64/92 模块 228 errors，合计 468 errors，0 真阳性。**

### api 层错误根因分布（聚合后）
| 根因 | 占比 | 说明 |
|------|------|------|
| `config_ds:env/2` 返回 `term()` 沿调用链渗出 | ~65%（150+ 条） | 本地 overlay 未标精确类型，调用点实际都有值约束（ctx 期望 `atom\|string\|binary\|number`） |
| `jsx`/`jsone` 第三方 JSON overlay 误报 | ~20% | `jsx:json_term()`、`{with_tail, jsx:json_term(), binary()}`、`jsx_config:options()` 等 |
| `inet:ntoa/1` 返回含 `{error,einval}` | ~3%（4 条） | `list_to_binary(inet:ntoa(PeerIp))` 出现在 passport/websocket/channel_webhook/auth_oidc 4 个 handler；`cowboy_req:peer` spec 保证返回 `{inet:ip_address(), port()}`，运行时 never 走 error 分支 → **缝隙不崩，判误报** |
| cowboy `dynamic()` / `http_headers` / `resp_body` | ~10% | cowboy overlay 把 req/headers/resp_body 标成 `dynamic()` 地图，无法收窄 |

### repo 层错误根因分布
| 根因 | 代表 | 说明 |
|------|------|------|
| `config_ds:env` 的 `term()` 渗出 | group_repo:182-185 等 | 同 api |
| `epgsql:connection()` overlay 把连接标成 `dynamic()` / 不接受 `'mock_conn'` 字面量 | user_setting_repo:170 `elib_pg:execute(mock_conn,...)` | 项目自定义降级约定（`whereis(pooler)==undefined` 时传原子，内部 `epgsql:parse(mock_conn)` 报错被 try/catch 收敛为 `{error,_}`），运行时安全不崩，仅类型契约违规 |
| `unicode:characters_to_binary` error/incomplete 元组 | 多条 | 同 lib 层模式（输入已 guard） |

### 真阳性候选逐一排除（实测/读源码）
1. **`inet:ntoa` 4 处** → 确认 OTP29 spec 含 `{error,einval}`，但 `cowboy_req:peer` spec 保证 `inet:ip_address()`（合法 tuple），运行时不可达 error 分支 → **误报，不修**（改了反而破坏 cowboy 契约一致性）。
2. **`user_setting_repo:170 mock_conn`** → 项目内降级约定，被 try/catch 覆盖，不崩 → **误报，不修**（重构降级语义改动面大、零真实收益）。
3. 其余全部 `term()`/`dynamic()` 渗出 → **误报（overlay 局限）**。

### 关键结论（巩固 R8）
- **eqWAlizer 在 handler / repo 层同样不能做阻塞门禁** —— 468 条 0 真阳性，全部 overlay 噪声。
- 进一步印证：**eqWAlizer 仅在「OTP stdlib-only、无第三方依赖渗出」的路径上能抓真阳性**（全仓唯一实例 `elib_dt`，R7/`31147564`）。任何经过 `config_ds`/`jsx`/`cowboy`/`epgsql` 的代码路径都被 overlay 拖垮。
- **消除 overlay 噪声是 eqWAlizer 可用的前置工程**：给 `config_ds`（标注 `env/2` 按 key 返回精确类型）、erlware_commons、cowboy、epgsql 写本地 `eqwalizer_support` 覆盖，能消掉 ~80% 的 api/repo 误报。

---

## eqWAlizer ds / logic 层全量扫描（R10，2026-07-25）

> 目的：补齐全仓双引擎噪音研判最后两块（ds 数据层 / logic 业务逻辑层），
> 彻底闭环。结果：**ds 36/89 模块 126 errors，logic 63/110 模块 317 errors，合计 443 errors，0 真阳性。**

### ds 层根因
| 根因 | 代表 | 说明 |
|------|------|------|
| `config_ds:env/2` 的 `term()` 渗出 | user_ds / group_ds / auth_ds 等 | 同 api/repo |
| ds 内部 spec 过度声明引发 union 不匹配 | `message_ds:369` 的 Uid（`integer()\|binary()`）传入 `msg_c2g_ds:read_msg/3`（spec 仅 `integer()`） | 3 个调用方（`user_server`/`passport_logic`）均经 `ec_cnv:to_integer` 保证 integer，binary 不可达 → **误报** |
| epgsql / `term()` overlay 噪声 | account_ds / app_ddl_ds / channel_ds | 同前 |

### logic 层真阳性候选逐一排除（实测/读调用链）
1. **`auth_oidc_logic:499 iolist_to_binary(uri_string:compose_query(Pairs))`** → `compose_query` spec 返回 `uri_string()|error()`，error 分支未处理看似会 badarg；但 2 个调用点（L61/L188）传入 Pairs **全为硬编码 `{binary(),binary()}` 二元组**，compile_query 对全 binary 合法 list 永远成功，error 不可达 → **误报**（和 `inet:ntoa` 同类缝隙）
2. **`rtc_room_logic:47 room_name(<<"group">>, Gid)` 的 `integer()|{integer(),integer()}` union** → spec 把 c2c/group 两场景输入合并声明，但两子句按首原子参数（`<<"group">>`/`<<"c2c">>`）严格分流，group 分支永收不到二元组；调用方 L29/L36 分别只传 integer / 二元组 → **误报**（eqWAlizer 不跨子句收窄 union）
3. **`number()|{number(),number()} => number()`（rtc_room_logic:47 同族）** / **`binary() => number()`（adm_moderation/attach_logic）** / **`{binary()|{error,invalid_datetime},...}`（datetime guard）** → 全部 `config_ds:env` 的 `term()` 渗出或 spec 过度声明，调用点实际形态已 guard → **误报**
4. **`uri_string` / `unicode:characters_to_binary` error|incomplete 元组** → 输入已 guard，同前

### 全仓 eqWAlizer 扫描总账（lib+api+repo+ds+logic）
| 层 | 模块(有错/总) | errors | 真阳性 |
|----|--------------|--------|--------|
| lib | 36/70 | 140 | 0（仅 `elib_dt` 已在 R7 修，属 stdlib-only） |
| api | 42/66 | 240 | 0 |
| repo | 64/92 | 228 | 0 |
| ds | 36/89 | 126 | 0 |
| logic | 63/110 | 317 | 0 |
| **合计** | **~241/427** | **1051** | **0（除 elib_dt）** |

### 终极结论
- **eqWAlizer 在 imboy 当前状态下（无自定义 overlay）只能作为非阻塞「仅供参考」门禁**；全仓 1051 条 errors 中，除已修的 `elib_dt`（R7）外，**0 真阳性**，全是第三方/stdlib overlay 噪声。
- **双引擎分工最终定型**：
  - **Gradualizer** = 宽网基线（无 overlay 依赖，已 6 个真阳性 commit，是主力真阳性挖掘器）
  - **eqWAlizer** = 深度门禁，**但仅对 OTP stdlib-only 路径有效**（elib_dt 实证）；handler/repo/ds/logic 路径被 `config_ds`/`jsx`/`cowboy`/`epgsql` overlay 淹没，block 只会逼出 churn
- **让 eqWAlizer 真正可用的唯一路径**：写本地 `eqwalizer_support` 覆盖（优先 `config_ds:env/2` 按 key 标精确返回类型 + erlware_commons/cowboy/epgsql），预计消掉 ~80% 误报后才谈转阻塞。

---

## 提交前检查

### 上游 Issue（Issue 1 / 2）
- [ ] 网络可达后确认 josefs/Gradualizer 无重复 issue（搜 `none()` / `pick_value`）
- [ ] 附最小复现仓库或上述 `elib_str` 崩溃日志（`.gradualizer/logs/elib_str.log`）
- [ ] 标注 OTP 版本 / Gradualizer commit
- [ ] 本地已 `GRADUALIZE_EXCLUDE` 临时规避，issue 关闭前保持

### 误报决策日志维护纪律
- [ ] 每轮 P3 治理若发现新的误报模式，追加到上方「已知误报模式」表，避免下一轮重复研判
- [ ] 每修复一个真阳性，在「已修复真阳性快速索引」追加一行（含 commit / 模块 / 类型 / 风险）
- [ ] 真阳性 vs 误报的判定必须由「运行 gradualize + 读源码 + 确认真伪」三步支撑，不可凭直觉

---

## P3 续作 R11：白名单 enable 试点 + R10 方向误判纠正（2026-07-25）

### R11.1 纠正 R10 记忆的方向性误判（重要）

R10 结论写过「让 eqWAlizer 真正可用的**唯一路径**是写本地 `eqwalizer_support` 覆盖」。这句**不准确**，已验证纠偏：

- **正确路径是白名单分层 enable**（规划 P3「分层推进表」正经工作），而非优先写 overlay 覆盖。
- 实测反驳「写 overlay 能消噪声」的假设：`config_ds:env/2` 返回 `any()`，其沿调用链的 `term()` 渗出**无法通过 spec override 消除**——override 只能改返回类型签名，`any()→any()` 无法按运行时 key 精细化成 `binary()`/`integer()`。覆盖它对下游 `term()` 当 `binary()` 的报错毫无帮助。
- 真正能让 lib 全层转阻塞（预算→0）的工程是「写 eqwalizer_support 覆盖消 36 个第三方 overlay 噪声」（erlware_commons/cowboy/epgsql/jsone + crypto/re 的 OTP29 overlay 过窄），这是**高阶专项**，不是唯一路径，也不是第一步。
- **第一步（也是 R11 做的）**：把已严格验证 0-error 的模块纳入 eqWAlizer 白名单（`-eqwalizer(enable).`），先纳入能纳入的，渐进式推进。

### R11.2 工具行为澄清（避免后续 agent 再误判）

`elp eqwalize <mod>` 与 `-eqwalizer(enable).` 属性的语义边界（实测确认）：

| 调用方式 | 行为 | 受 `enable_all`/`enable` 属性影响？ |
|---|---|---|
| `elp eqwalize <mod>`（单模块） | **强制检查该模块**，无论是否 enable | ❌ 不受影响（总查） |
| `make eqwalize-layer LAYER=x` | 遍历 `src/x/*.erl` 逐文件 `elp eqwalize`，靠 `EQWALIZE_BUDGET` 兜住 | ❌ 不受影响（逐文件强查） |
| `elp eqwalize-all` | 只查带 `-eqwalizer(enable).` / `-typing([eqwalizer]).` 的模块（白名单语义） | ✅ 受影响 |
| IDE/LSP 实时检查 | 只对 enable 模块标红 | ✅ 受影响 |

**推论**：当前 CI 的 `eqwalize` job 跑 `eqwalize-layer LAYER=lib`（逐文件强查 + 预算 38），与模块是否加 `enable` 属性**无关**；加 `enable` 属性的价值在于为 `eqwalize-all` 白名单模式 + IDE 保护铺路，是 P2→P3 演进（某层全 0-error 后切 `eqwalize-all` + enable 转阻塞）的前提。

### R11.3 全仓 0-error 绿名单（白名单 enable 弹药库）

经 R8-R10 全量扫描 + R11 lib 重跑（逐模块顺序规避 jar race）**双重验证**，以下模块 eqWAlizer 0-error，可直接 enable 纳入检查（不新增任何 CI 阻断）：

| 层 | 0-error 模块数 | 模块清单 |
|---|---|---|
| lib | 33（elib_log 已在 ignore_modules 豁免） | agent_rate_limiter, agent_trigger_policy, elib_dt, elib_id, elib_retry_config, elib_retry, elib_s3_sign, elib_str, elib_tsid, elib_uuid, epgsql_codec_rfc3339_bin, eunit_runner, group_member_transfer, imboy_cluster, imboy_codec, imboy_dtl, imboy_error, imboy_frame, imboy_llm_openai, imboy_llm_qianfan, imboy_llm_registry, imboy_llm, imboy_message_helper, imboy_plugin_dependency, imboy_plugin_lifecycle, imboy_plugin_registry, imboy_plugin_toml, imboy_plugin, imboy_policy_catalog, imboy_policy_persistence, imboy_profile_preset, license_expiry_notice, llm_stream |
| api | 24 | agent_card_handler, agent_mandate_handler, agent_task_demo_handler, agent_task_handler, ai_agent_handler, app_feature_handler, app_manifest_handler, app_upgrade_log_handler, attach_handler, brand_handler, e2ee_backup_handler, e2ee_trust_handler, fts_handler, group_member_handler, group_notice_handler, group_tag_handler, live_room_handler, location_handler, payment_callback_handler, rtc_room_handler, security_headers_middleware, test_handler, throttle_middleware, user_denylist_handler |
| repo | 28 | announcement_repo, app_ddl_repo, app_upgrade_log_repo, app_version_policy_repo, attachment_repo, billing_invoice_repo, billing_usage_repo, compliance_key_repo, conversation_delete_repo, conversation_pin_repo, feedback_reply_repo, feedback_repo, fts_user_repo, group_random_code_repo, imboy_plugin_audit_repo, mcp_audit_repo, mcp_client_grant_repo, msg_forward_repo, msg_reaction_repo, msg_read_repo, olm_identity_repo, ops_report_repo, payment_transaction_repo, report_action_log_repo, sso_config_repo, sso_identity_repo, user_dnd_rule_repo, user_tag_repo |
| ds | 53 | adm_operation_log_ds, adm_user_ds, agent_payment_mandate_ds, announcement_ds, app_upgrade_log_ds, app_version_policy_ds, attachment_ds, billing_invoice_ds, billing_plan_ds, billing_subscription_ds, billing_usage_ds, channel_admin_ds, channel_comment_ds, channel_invitation_ds, channel_message_ds, channel_order_ds, channel_subscription_ds, channel_webhook_ds, compliance_key_ds, conversation_pin_ds, e2ee_backup_ds, feedback_ds, fts_user_ds, geo_people_nearby_ds, group_category_ds, group_file_ds, group_log_ds, group_notice_ds, group_random_code_ds, group_schedule_ds, group_task_ds, group_vote_ds, imboy_plugin_audit_ds, mention_ds, moderation_ds, msg_archive_ds, msg_c2s_ds, msg_forward_ds, msg_operation_ds, msg_read_ds, olm_identity_ds, payment_transaction_ds, push_token_ds, qr_login_event_ds, recharge_order_ds, report_action_log_ds, report_ticket_ds, sso_identity_ds, trust_audit_ds, user_collect_ds, user_log_ds, user_setting_ds, wallet_ds |
| logic | 47 | adm_setup_logic, agent_payment_command, agent_payment_mandate_logic, agent_task_demo, agent_task_observer, ai_agent_group_reply, ai_agent_logic, ai_agent_prompt, ai_agent_reply, app_upgrade_log_logic, app_version_logic, auth_logic, billing_logic, channel_logic_notify, channel_logic_sync, conversation_pin_logic, e2ee_backup_logic, e2ee_logic, e2ee_recovery_logic, e2ee_trust_logic, feedback_logic, finance_adm_logic, friend_category_logic, group_event_handler, group_file_logic, group_notice_logic, group_tag_logic, group_vote_logic, live_room_logic, moment_logic_notify, msg_ack_logic, msg_forward_logic, msg_pinned_logic, msg_reaction_logic, olm_identity_logic, payment_gateway, payment_mock_gateway, payment_reconcile_logic, payment_sign, push_notification_logic, qr_login_logic, red_packet_logic, sso_logic, transfer_logic, user_tag_logic, wallet_logic, webrtc_ws_logic |

**绿名单合计：185 个 0-error 模块**（lib 33 + api 24 + repo 28 + ds 53 + logic 47）。

### R11.4 lib 层白名单 enable 试点（已落地，验证通过）

- **动作**：给 lib 层代表性纯函数模块加 `-eqwalizer(enable).` 属性（在 `-module/1` 后插入单行）。
- **实际提交 3 个**（commit `69a26f94`，erlfmt/conventional/pre-commit 全过）：
  `elib_dt`（R7 已修真阳性，最干净）、`imboy_error`、`elib_uuid`
- **暂缓 2 个**：`imboy_dtl`、`elib_id` —— 这 2 文件**原本就 erlfmt 不合规**（pre-existing，stash 验证非本次引入），为避免超出 P3 范围的格式修复改动，暂缓 enable，留待格式专项。其 0-error 结论已在 R11.3 绿名单记录，待格式修复后即可 enable。
- **验证**：
  - `make eqwalize MOD=<m>` 三者均 0-error（enable 语义生效）
  - `erlc` 重编译通过（`-eqwalizer` 属性不破坏 beam 构建）；erlfmt 合规
  - 确认 `elp eqwalize` 对未 enable 模块仍强查（elib_cipher 16 errors 照报）→ 印证 R11.2 工具行为表
- **结论**：试点机制跑通。后续按层推进时，可直接从 R11.3 绿名单批量 enable（建议仍按层小批量，避免一次性 185 文件大改违反防腐规则）；真正让某层「预算→0 转阻塞」需先做 R11.1 所述的高阶 overlay 覆盖工程。
- **提交范围**：仅 3 个 lib .erl 文件；不动 36 个 lib 报错模块（overlay 噪声，留专项）；本文档在游离根级 docs/（非 git 仓，与 R5/R6/R10 一致）。

---

## R14 — lib 层 140 error 根因定性与 route ② 可行性裁决（2026-07-25）

> 背景：R11 把「写 `eqwalizer_support` 覆盖」列为让 eqWAlizer lib 层转阻塞的唯一路径，但未验证「项目本地能否挂覆盖」。R13 后用户要求继续推进 route ②，本轮用数据裁决。

### R14.1 route ② 可行性实证 spike（决定性负面）

- **假设**：在 `src/lib/` 建项目本地 `eqwalizer_specs.erl`，给 `jsone:decode/2` 写更精确 override（`{ok, map()} | {error, term()}` 取代 vendored 的 `eqwalizer:dynamic()`），观察 `elib_uri` line 115 的 jsone error 是否消失。
- **结果**：`elib_uri` 仍 10 error；line 115 仍报 `jsone:decode` 返回 `term()`；**无任何模块名冲突告警，也无任何改善**。
- **结论**：**elp 只加载 vendored `eqwalizer_support` 里的 `eqwalizer_specs`，项目本地同名模块无效**。要给 `crypto`/`uri_string`/`gen_server`/`epgsql` 加覆盖，必须 **fork / 维护一份独立的 `eqwalizer_support` 依赖**（写入 `.elp.toml` 的 dep 指向自有 fork），这是真实的依赖运维承诺，非「写个模块」能解决。spike 模块已清理，未提交。

### R14.2 lib 层 140 error 根因分布（全量抓取 36 模块 / 140 error）

> 方法：`elp eqwalize` 逐模块顺序跑（避 jar race），抓取全部 error 上下文，按涉及的外部模块/类型前缀统计（含类型名与调用，为量级信号非精确归因）。

| 根因类别 | 量级 | 主导模块 | 是否可消 |
|---|---|---|---|
| **OTP `crypto:*` 类型缝隙**（`rsa_public`/`ecdh_public`/`cipher_iv`/`crypto_init` 等 opaque 类型，vendored support 未覆盖） | ~30 | `elib_cipher`(16)、`elib_cnv`(10) | 仅能靠 fork support 覆盖 |
| **OTP `uri_string:*` 类型缝隙**（`uri_map`/`error`/`normalize`/`unquote`，vendored 未覆盖） | ~25 | `elib_uri`(10) 等 | 仅能靠 fork support 覆盖 |
| **`gen_server:start_link`/`start_ret` 回调模式** | ~24 | worker 类（`billing_invoice_worker`/`olm_otk_cleanup_worker`/`license_notice_worker` 等） | 部分可在源码补 `-spec` 修复 |
| **第三方 `epgsql:*` 精度不足**（`query_error`/`connect_opts`/`transaction_opts` 等） | ~12 | `elib_pg`(10)、`elib_pg_sql`(2) | 仅能靠 fork support 覆盖 epgsql spec |
| **`maps:*` 操作作用于不精确 map**（`maps:get`/`remove`/`from_list`/`find`） | ~15 | 多模块 | 部分可在源码收窄 map 类型修复 |
| **`config_ds:env` 渗出**（`term()` 沿调用链渗） | **仅 3** | 个别 | **不可消**（R11 已证：override 仅改签名，`any()` 无法按 key 精细化） |
| **`jsone`/`jsx`/`cowboy` 等第三方** | ~1–3 | 极小 | 可覆盖但量级可忽略 |

### R14.3 纠正 R8–R11 的一处误判

- R8–R10 称「`config_ds:env` 渗出是最大噪声头」——这是针对**全仓**（api/repo/ds/logic）的判断；在 **lib 层它只占 3 处**，并非主因。lib 层真正主因是 **OTP `crypto`/`uri_string` 类型缝隙（合计 ~55，占 40%）+ `gen_server` 回调模式（~24）+ `epgsql` 第三方精度（~12）**，三者**全在 vendored support 覆盖列表之外**。
- 这解释了为何 R11 的「写 override 消 env 渗出」设想在 lib 层根本不对题：env 渗出不是 lib 层瓶颈，瓶颈是 OTP/epgsql 类型缝隙，而消这些必须 fork support。

---

## R15：源码级 spec 补全消 6 个 lib 模块（commit e5d4feba）

> **动机**：R14 根因分布表里 `gen_server` 回调 ~24 是唯二「源码可修」类之一（另一类是 maps 不精确 ~15，但收窄 map 类型需触业务逻辑，风险更高）。先用最小代价消 gen_server 误报。

### R15.1 gen_server start_link 规格化（9 行 spec → `gen_server:start_ret()`）

**8 模块、9 行 spec** 从 `{ok, pid()} | {error, term()}` 收敛为 `gen_server:start_ret()`：

- `billing_invoice_worker` / `elib_metric` / `imboy_router_registry` / `imboy_ws_action_registry` / `olm_otk_cleanup_worker`：start_link/0（5 行）
- `imboy_plugin_loader`：start_link/0 + start_link/1（2 行）
- `imboy_cache_sync` / `license_notice_worker`：start_link/0（2 行）

**安全论证**：`gen_server:start_ret()` = `{ok,pid()}|ignore|{error,term()}`，是当前 spec 的超集。这 8 个 worker 的 init 均返回 `{ok,State}`，实际绝不产生 `ignore`；收敛为更精确的 OTP 语义使 eqWAlizer 不再标记为 incompatible。

### R15.2 send_notices 规格化（1 行）

`license_notice_worker:send_notices/1` spec 从 `expired|1|7|30` 补 `none` 候选。实际 `should_send(none, _) -> false` 保证运行时绝不渗入，但 eqWAlizer 不感知流控，spec 需全量 union。修复后错误从 `send_notices` 转移到它内部的 `license_expiry_notice:render`（第三方隔阂，不可本地消）。

### R15.3 附带修 imboy_cache_sync pre-existing erlfmt

`imboy_cache_sync` export 列表多行→单行，纯格式，与 R13 同类。erlfmt -w 后提交通过。

### 结果

| 指标 | 修复前 | 修复后 | 说明 |
|---|---|---|---|
| lib failing 模块 | 36 | **30** | 6 模块 0-error |
| 本次消 | — | 7 error（9 gen_server + send_notices，imboy_cache_sync 3→2 抵消 1） | |
| 源码可修类 | ~24+~15 | **gen_server 已清完**，maps 随业务迭代 | gen_server 是唯二源码可修类之一 |
| 残留第三方隔阂 | — | OTP crypto/uri_string ~55 + epgsql ~12 + depcache + render 等 | **全部需要 fork eqwalizer_support** |

### 结论

**lib 层源码可修（勿需 fork 依赖、勿需触业务逻辑）的部分已清完。** 剩余 30 failing 模块的 error **全部属于 OTP/epgsql/depcache/cowboy 第三方类型隔阂 + config_ds:env 渗出 + maps 不精确**——前三者需 fork eqwalizer_support、后者需逐模块收窄 map 类型（触业务逻辑/高 touch 面）。本地 P3 治理的 gen_server 回头债已偿。

### R14.4 route ② GO / NO-GO 裁决

- **GO 条件（若要做）**：fork `eqwalizer_support` → 在自有 fork 的 `eqwalizer_specs` 中补 `crypto`/`uri_string`/`gen_server`/`epgsql` 的精确/替代 spec → 接 `.elp.toml` dep 指向 fork → 重测 140 error 回落幅度。
- **代价与风险**：① 长期维护一个 WhatsApp 上游的 fork（上游更新需 rebase）；② override 写错会变**不健全（unsound）**，掩盖真实 bug；③ 即使 fork 完成，`config_ds:env` 渗出的 3 处 + 部分源码级 spec 缺失仍残留，预算无法归零。
- **NO-GO 推荐**：**本地 P3 治理的低风险价值已见底**。route ② 是「依赖运维承诺 + 不健全风险」的 deliberate 工程，不应作为「继续」的反射动作。剩余 140 error 中，**`gen_server` 回调与 `maps` 不精确 map 子集可在源码级渐进补 spec 修复**（随相关模块业务迭代顺手做，不单独立项）；OTP/epgsql 缝隙留给 fork support 专项，需团队决策。

### R14.5 阶段收口结论

| 维度 | 状态 |
|---|---|
| 真阳性治理 | ✅ 7 精确 commit（Gradualizer 6 + eqWAlizer 1） |
| 白名单机制 | ✅ lib 5 模块 enable（R11+R13）+ 全仓 185 绿名单 |
| 知识资产 | ✅ 入仓可见（R12），本文档即其中之一 |
| 格式技术债 | ✅ imboy_dtl/elib_id 已消（R13） |
| lib 140 error 定性 | ✅ 本轮完成（R14.2 分布表 + R14.1 spike 裁决） |

**剩余路线（需团队决策 / 外部动作，非本地可独立完成）：**
1. **激活 CI 预算 ratchet** —— maintainer 在 GitHub 后台设 `vars.GRADUALIZE_BUDGET`（本地无法）
2. **fork `eqwalizer_support` 覆盖 OTP/epgsql 缝隙** —— 让 lib 层预算部分回落（高阶专项，R14.4 已拆解代价）
3. **源码级补 spec**（gen_server 回调 + maps 精确化）—— 随业务迭代顺手做
4. ~~config_ds:env 渗出~~ —— ✅ 已证不可消，放弃
5. ~~friend/moment map() 收窄~~ —— R4 判误报，不动

> 注：本 R14 章节已随文档在 R12 迁入 `imboy/docs/reference/static-typechecking/`（git 仓内，对买家/CI 可见），原「游离根级 docs」表述作废。
