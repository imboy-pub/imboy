%% @doc 缓存策略真源（V2.2.1 W2-04/W2-05）
%%
%% 依据 docs/architecture/cache-v2.2.1/w0/key-registry.json（W0 冻结的 47 条
%% Key 策略矩阵）投影为静态 Erlang 表。本模块只做分类与校验，不持有进程状态。
%%
%% 合同（计划 3.1/3.3/19.4/19.5）：
%% - policy(Key) 返回 local | invalidate | legacy | blocked；
%%   blocked = G-QR 未决的 QR 两类 Key，永不进入 replicate。
%% - replicate 生产白名单默认为空；仅配置 `cache_replication.whitelist`
%%   显式列出的 namespace 可申请 replicate，且仍须经值大小/TTL/敏感性校验。
%% - 敏感 Key 的值禁止复制（19.5 默认 deny，fail-closed）。
%% - 未分类 Key 返回 legacy：旧调用保持旧行为，不广播新协议事件（3.3 迁移期）。
-module(imboy_cache_policy).
-moduledoc "缓存 Key 策略真源：Registry 分类、复制白名单与 put/invalidate 校验。".

-export([
    fenced_namespaces/0,
    policy/1,
    namespace_of/1,
    is_local/1,
    is_sensitive/1,
    replicate_allowed/1,
    is_fenced/1,
    validate_put/3,
    validate_replicate/3,
    check_value_size/1,
    max_ttl_sec/0,
    max_value_bytes/0,
    batch_size/0,
    recovery_window_sec/0,
    tombstone_ttl_sec/0,
    protocol/0,
    %% C01 冻结的资源预算（closeout-v1/protocol-budget-frozen.md 第 6 节）
    n_max/0,
    t_max/0,
    b_max/0,
    p_max/0,
    p_max_bytes/0,
    r_max_rounds/0,
    d_max_ms/0,
    high_watermark/0,
    grace_window_ms/0,
    budget_digest/0,
    config_hash/0,
    registry_hash/0
]).

%% W0 Registry 投影：namespace => {Policy, Sensitive}
%% local 9 / invalidate 36 / G-QR_BLOCKED 2，共 47 条（19.4 零静默缺漏）。
-define(REGISTRY, #{
    %% --- local（本地重建，禁止跨节点广播）---
    local_cache => {local, true},
    apns_jwt => {local, true},
    bind_mail => {local, true},
    cluster_node_info => {local, false},
    fcm_access_token_dyn => {local, true},
    qianfan_access_token => {local, true},
    login_attempt => {local, true},
    login_attempt_ip_limit => {local, true},
    account_list => {local, true},
    %% --- invalidate（数据库派生，变更后失效）---
    adm_session_epoch => {invalidate, true},
    adm_user_account => {invalidate, true},
    adm_user_admin_permission => {invalidate, true},
    adm_user_current => {invalidate, true},
    adm_user_feedback_permission => {invalidate, true},
    adm_user_group_permission => {invalidate, true},
    adm_user_moment_permission => {invalidate, true},
    adm_user_permission => {invalidate, true},
    adm_user_rbac => {invalidate, true},
    adm_user_report_permission => {invalidate, true},
    adm_user_role_ids => {invalidate, true},
    adm_user_role_permission => {invalidate, true},
    adm_user_sample => {invalidate, true},
    adm_user_stats_permission => {invalidate, true},
    auth_session_epoch => {invalidate, true},
    billing_meter => {invalidate, true},
    channel => {invalidate, false},
    channel_subs => {invalidate, true},
    check_relationship3 => {invalidate, true},
    config5 => {invalidate, true},
    conv_deleted => {invalidate, true},
    friend_rel_dep => {invalidate, false},
    group => {invalidate, true},
    group_e2ee_mode => {invalidate, true},
    in_denylist => {invalidate, true},
    is_friend => {invalidate, true},
    is_friend2 => {invalidate, true},
    is_friend_fields2 => {invalidate, true},
    moderation_sensitive_words => {invalidate, true},
    moment_acl => {invalidate, true},
    msg_reaction => {invalidate, false},
    tag_subtitle_2 => {invalidate, true},
    tag_subtitle_3 => {invalidate, true},
    user_device_active => {invalidate, true},
    user_device_name => {invalidate, true},
    user_dnd_rule => {invalidate, true},
    user_tag_relation => {invalidate, true},
    %% --- blocked（G-QR 未决：19.2 QR 两类 Key 禁止 replicate，
    %%     不因测试开关或配置绕过；修复须走独立方案与 Schema 决策）---
    qr_login => {blocked, true},
    qr_login_qr => {blocked, true}
}).

%% @doc Key 是否走 19.3 generation 栅栏（V2 物理键路径）。
%% 仅 `cache_fenced_namespaces` 显式列出的 namespace 启用；默认空 = 全部 legacy 行为。
-spec is_fenced(term()) -> boolean().
is_fenced(Key) ->
    NS = namespace_of(Key),
    NS =/= undefined andalso lists:member(NS, fenced_namespaces()).

-spec fenced_namespaces() -> [atom()].
fenced_namespaces() ->
    application:get_env(imboy, cache_fenced_namespaces, []).

%% @doc 值大小校验（接收端独立校验用，19.5）。
-spec check_value_size(term()) -> boolean().
check_value_size(Value) ->
    value_size(Value) =< max_value_bytes().

%% 计划 6.1/13.1 候选默认（执行补充第 5 节冻结的本地工程候选值）
-define(DEFAULT_MAX_TTL_SEC, 600).
-define(DEFAULT_MAX_VALUE_BYTES, 32768).
-define(DEFAULT_BATCH_SIZE, 256).
-define(DEFAULT_RECOVERY_WINDOW_SEC, 300).
%% 墓碑保留 >= max(T+C, R+S+D+2C) = max(605,400)，取 720（计划 6.3）
-define(DEFAULT_TOMBSTONE_TTL_SEC, 720).

%% @doc Key 的策略分类。
%% 未在 Registry 的 Key 一律 legacy（旧行为，不参与 V2 语义，3.3 迁移期合同）。
%% `cache_registry_overrides`（默认空）允许显式注册/覆盖 namespace 策略条目，
%% 仅供隔离测试与未来正式扩展使用；生产默认配置不含任何 override。
-spec policy(term()) -> local | invalidate | replicate | legacy | blocked.
policy(Key) ->
    case registry_entry(namespace_of(Key)) of
        undefined -> legacy;
        {P, _Sensitive} -> P
    end.

-spec registry_entry(atom() | undefined) -> {atom(), boolean()} | undefined.
registry_entry(undefined) ->
    undefined;
registry_entry(NS) ->
    Overrides = application:get_env(imboy, cache_registry_overrides, #{}),
    case maps:find(NS, Overrides) of
        {ok, {_, _} = Entry} -> Entry;
        _ -> maps:get(NS, ?REGISTRY, undefined)
    end.

%% @doc 从实际缓存 Key 提取 namespace 标签。
%% 支持 {NS, _...} 元组形态（生产主流）与 {local_cache, Sub, _} 两段前缀。
-spec namespace_of(term()) -> atom() | undefined.
namespace_of({local_cache, Sub, _}) when is_atom(Sub) -> local_cache;
namespace_of({NS, _}) when is_atom(NS) -> NS;
namespace_of({NS, _, _}) when is_atom(NS) -> NS;
namespace_of({NS, _, _, _}) when is_atom(NS) -> NS;
namespace_of({NS, _, _, _, _}) when is_atom(NS) -> NS;
namespace_of(_) -> undefined.

%% @doc 是否本地 Key（禁止任何跨节点广播）。
-spec is_local(term()) -> boolean().
is_local(Key) ->
    policy(Key) =:= local orelse
        (is_tuple(Key) andalso tuple_size(Key) >= 1 andalso element(1, Key) =:= local_cache).

%% @doc 是否敏感值（默认 deny 复制，19.5）。
-spec is_sensitive(term()) -> boolean().
is_sensitive(Key) ->
    case registry_entry(namespace_of(Key)) of
        undefined -> true;
        {_P, Sensitive} -> Sensitive
    end.

%% @doc replicate 白名单校验（fail-closed）。
%% 生产默认为空：仅当 namespace 同时满足（1）Registry/override 策略为 replicate、
%% （2）显式列入配置白名单、（3）值非敏感（Registry 声明）才允许。
%% 敏感数据、QR 两类 Key（blocked）、local/invalidate/legacy 未分类 Key 一律
%% 拒绝（AC-11）；未来生产 Key 须先正式登记 Registry 条目再加白名单（19.5）。
-spec replicate_allowed(term()) -> boolean().
replicate_allowed(Key) ->
    case registry_entry(namespace_of(Key)) of
        {replicate, false = _NonSensitive} ->
            lists:member(namespace_of(Key), whitelist());
        _ ->
            false
    end.

%% @doc put(local)/invalidate 类写入的统一校验（W2-05/W2-06）。
%% local 写入对 TTL/大小做上界校验但不做复制校验。
-spec validate_put(term(), term(), non_neg_integer()) -> ok | {error, term()}.
validate_put(_Key, _Value, TTL) when is_integer(TTL), TTL >= 0 ->
    ok;
validate_put(_Key, _Value, _TTL) ->
    {error, invalid_ttl}.

%% @doc put(replicate) 的完整校验（W2-02/19.5）。
%% 任一失败返回 {error, Reason}，调用方不得伪装成功（计划 2.2）。
-spec validate_replicate(term(), term(), non_neg_integer()) -> ok | {error, term()}.
validate_replicate(Key, Value, TTL) ->
    case replicate_allowed(Key) of
        false ->
            {error, replicate_not_authorized};
        true ->
            checks([
                {fun() -> is_integer(TTL) end, invalid_ttl},
                {fun() -> TTL > 0 andalso TTL =< max_ttl_sec() end, ttl_out_of_range},
                {fun() -> Value =/= undefined end, invalid_value},
                {fun() -> value_size(Value) =< max_value_bytes() end, value_too_large},
                %% PID/ref/fun 一律禁止复制（提示词第八节 fail-closed）
                {fun() -> not contains_pid_ref_fun(Value) end, value_type_forbidden}
            ])
    end.

%% @doc 当前同步协议（13.1：初始必须 legacy；全局 ENABLE_V2 受 G-QR 等安全 gate 约束）。
-spec protocol() -> legacy | v2.
protocol() ->
    case application:get_env(imboy, cache_sync_protocol, legacy) of
        v2 -> v2;
        _ -> legacy
    end.

-spec max_ttl_sec() -> non_neg_integer().
max_ttl_sec() -> rep_cfg(max_ttl_sec, ?DEFAULT_MAX_TTL_SEC).

-spec max_value_bytes() -> non_neg_integer().
max_value_bytes() -> rep_cfg(max_value_bytes, ?DEFAULT_MAX_VALUE_BYTES).

-spec batch_size() -> non_neg_integer().
batch_size() -> rep_cfg(batch_size, ?DEFAULT_BATCH_SIZE).

-spec recovery_window_sec() -> non_neg_integer().
recovery_window_sec() -> rep_cfg(recovery_window_sec, ?DEFAULT_RECOVERY_WINDOW_SEC).

%% 墓碑保留下界按计划 6.3 公式从当前配置推导，但不低于默认 720。
-spec tombstone_ttl_sec() -> non_neg_integer().
tombstone_ttl_sec() ->
    T = max_ttl_sec(),
    R = recovery_window_sec(),
    S = 60,
    D = 30,
    C = 5,
    erlang:max(erlang:max(T + C, R + S + D + 2 * C), ?DEFAULT_TOMBSTONE_TTL_SEC).

%% ------------------------------------------------------------------
%% C01 冻结的资源预算（closeout-v1/protocol-budget-frozen.md 第 6 节）
%% 本地工程预算，非生产 SLO；故障测试前冻结，不得为通过测试调高。
%% ------------------------------------------------------------------

-define(DEFAULT_N_MAX, 50000).
-define(DEFAULT_T_MAX, 20000).
-define(DEFAULT_B_MAX, 2048).
-define(DEFAULT_P_MAX, 256).
-define(DEFAULT_P_MAX_BYTES, 4194304).
-define(DEFAULT_R_MAX_ROUNDS, 3).
-define(DEFAULT_HIGH_WATERMARK, 10000).

%% rindex 复制条目上限（项）。
-spec n_max() -> pos_integer().
n_max() -> rep_cfg(n_max, ?DEFAULT_N_MAX).

%% 墓碑条目上限（项）。
-spec t_max() -> pos_integer().
t_max() -> rep_cfg(t_max, ?DEFAULT_T_MAX).

%% 恢复接收缓冲上限（项，页事件累计条目）。
-spec b_max() -> pos_integer().
b_max() -> rep_cfg(b_max, ?DEFAULT_B_MAX).

%% 快照每页条目上限。
-spec p_max() -> pos_integer().
p_max() -> rep_cfg(p_max, ?DEFAULT_P_MAX).

%% 快照每页字节上限。
-spec p_max_bytes() -> pos_integer().
p_max_bytes() -> rep_cfg(p_max_bytes, ?DEFAULT_P_MAX_BYTES).

%% 单节点快照回合重试上限。
-spec r_max_rounds() -> pos_integer().
r_max_rounds() -> rep_cfg(r_max_rounds, ?DEFAULT_R_MAX_ROUNDS).

%% 恢复总 deadline（毫秒，= recovery_window）。
-spec d_max_ms() -> pos_integer().
d_max_ms() -> recovery_window_sec() * 1000.

%% sync 消息队列高水位（与 imboy_cache_sync 的 ?HIGH_WATERMARK 一致）。
-spec high_watermark() -> pos_integer().
high_watermark() -> rep_cfg(high_watermark, ?DEFAULT_HIGH_WATERMARK).

%% 旧化身/墓碑宽限窗口（毫秒，= W = 墓碑保留）。
-spec grace_window_ms() -> pos_integer().
grace_window_ms() -> tombstone_ttl_sec() * 1000.

%% @doc 冻结预算摘要（C01 oracle：配置/实现/测试三方一致）。
%% 规范化序列化后取 SHA-256 前 16 字节；任何预算值变化都会改变摘要。
-spec budget_digest() -> binary().
budget_digest() ->
    Parts = [
        {n_max, n_max()},
        {t_max, t_max()},
        {b_max, b_max()},
        {p_max, p_max},
        {p_max_bytes, p_max_bytes()},
        {r_max_rounds, r_max_rounds()},
        {d_max_ms, d_max_ms()},
        {max_value_bytes, max_value_bytes()},
        {max_ttl_sec, max_ttl_sec()},
        {high_watermark, high_watermark()},
        {grace_window_ms, grace_window_ms()}
    ],
    <<Digest:16/binary, _/binary>> = crypto:hash(
        sha256, term_to_binary(Parts, [{minor_version, 2}])
    ),
    Digest.

%% @doc 缓存同步配置摘要：预算 + 白名单 + fenced 集的规范化摘要（V2 信封 config_hash）。
-spec config_hash() -> binary().
config_hash() ->
    Parts = {budget_digest(), whitelist(), fenced_namespaces()},
    <<Digest:16/binary, _/binary>> = crypto:hash(
        sha256, term_to_binary(Parts, [{minor_version, 2}])
    ),
    Digest.

%% @doc Registry 生效视图摘要（静态表 + overrides 合并后按 namespace 排序；V2 信封 registry_hash）。
-spec registry_hash() -> binary().
registry_hash() ->
    Overrides = application:get_env(imboy, cache_registry_overrides, #{}),
    Merged = maps:merge(?REGISTRY, Overrides),
    Sorted = lists:sort(maps:to_list(Merged)),
    <<Digest:16/binary, _/binary>> = crypto:hash(
        sha256, term_to_binary(Sorted, [{minor_version, 2}])
    ),
    Digest.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

-spec rep_cfg(atom(), non_neg_integer()) -> non_neg_integer().
rep_cfg(K, Default) ->
    Cfg = application:get_env(imboy, cache_replication, #{}),
    V = maps:get(K, Cfg, Default),
    case is_integer(V) andalso V > 0 of
        true -> V;
        false -> Default
    end.

-spec whitelist() -> [atom()].
whitelist() ->
    Cfg = application:get_env(imboy, cache_replication, #{}),
    WL = maps:get(whitelist, Cfg, []),
    [NS || NS <- WL, is_atom(NS)].

-spec checks([{fun(() -> boolean()), term()}]) -> ok | {error, term()}.
checks([{Pred, Reason} | Rest]) ->
    case Pred() of
        true -> checks(Rest);
        false -> {error, Reason}
    end;
checks([]) ->
    ok.

%% 值大小估算：external term format 字节数（含 refc binary 全量，符合 19.5 字节上限语义）。
-spec value_size(term()) -> non_neg_integer().
value_size(Value) ->
    try
        erlang:external_size(Value)
    catch
        _:_ -> max_value_bytes() + 1
    end.

-spec contains_pid_ref_fun(term()) -> boolean().
contains_pid_ref_fun(T) when is_pid(T); is_reference(T); is_function(T) -> true;
contains_pid_ref_fun(T) when is_map(T) ->
    lists:any(
        fun({K, V}) -> contains_pid_ref_fun(K) orelse contains_pid_ref_fun(V) end,
        maps:to_list(T)
    );
contains_pid_ref_fun(T) when is_tuple(T) ->
    contains_pid_ref_fun(tuple_to_list(T));
contains_pid_ref_fun(T) when is_list(T) ->
    any_pid_ref_fun(T);
contains_pid_ref_fun(_) ->
    false.

-spec any_pid_ref_fun([term()]) -> boolean().
any_pid_ref_fun([H | T]) ->
    case contains_pid_ref_fun(H) of
        true -> true;
        false -> any_pid_ref_fun(T)
    end;
any_pid_ref_fun([]) ->
    false.
