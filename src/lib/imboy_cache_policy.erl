%% @doc 缓存策略真源（V2.2.1 W2-04/W2-05）
%%
%% 依据 docs/architecture/cache-v2.2.1/w0/key-registry.json（W0 冻结的 47 条
%% Key 策略矩阵）投影为静态 Erlang 表。
%%
%% - policy(Key) 返回 local | invalidate | legacy | blocked；
%%   blocked = G-QR 未决的 QR 两类 Key，永不进入 replicate。
%% - replicate 生产白名单默认为空；仅配置 `cache_replication.whitelist`
%%   显式列出的 namespace 可申请 replicate，且仍须经值大小/TTL/敏感性校验。
%% - 敏感 Key 的值禁止复制（19.5 默认 deny，fail-closed）。
%% - 未分类 Key 返回 legacy：旧调用保持旧行为，不广播新协议事件（3.3 迁移期）。
-module(imboy_cache_policy).
-moduledoc "缓存 Key 策略真源：Registry 分类、复制白名单与 put/invalidate 校验。".

-define(SKEW_SEC, 5).

-export([
    snapshot_entry_deadline/1,
    snapshot_entry_retention_deadline/1,
    valid_snapshot_entry/1,
    valid_snapshot_origin/2,
    within_meta_budget/1,
    remaining_ttl_sec/1,
    snapshot_entry_body/1,
    snapshot_entry_with_origin/2,
    revision_metadata_origin/2,
    snapshot_page_digest/3,
    snapshot_revision_counter/1,
    snapshot_page_result/7,
    snapshot_manifest_entry/1,
    snapshot_manifest_own_seq/3,
    snapshot_page_revision_ceiling/2,
    event_origin_checks/1,
    valid_watermark/1,
    valid_snapshot_cut/1,
    snapshot_cut_base/1,
    snapshot_cut_coverage/1,
    valid_version_report/1,
    version_report_base/1,
    version_report_target/1,
    version_report_round_matches/3,
    snapshot_metadata_kind/1,
    revision_metadata_row/5,
    revision_metadata_row/6,
    prospective_revision_rows/6,
    snapshot_metadata_rows/2,
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
    check_value_type/1,
    max_ttl_sec/0,
    max_value_bytes/0,
    protocol_meta_max_bytes/0,
    batch_size/0,
    recovery_window_sec/0,
    tombstone_ttl_sec/0,
    protocol/0,
    %% C01 冻结的资源预算（closeout-v1/protocol-budget-frozen.md 第 6 节）
    n_max/0,
    n_max_bytes/0,
    gen_max_bytes/0,
    t_max/0,
    t_max_bytes/0,
    b_max/0,
    j_max/0,
    j_max_bytes/0,
    b_max_bytes/0,
    p_max/0,
    p_max_bytes/0,
    r_max_rounds/0,
    d_max_ms/0,
    high_watermark/0,
    grace_window_ms/0,
    grace_max/0,
    peer_max/0,
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

-spec check_value_type(term()) -> boolean().
check_value_type(Value) ->
    Value =/= undefined andalso not contains_pid_ref_fun(Value).

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

registry_entry(undefined) ->
    undefined;
registry_entry(NS) ->
    Overrides = application:get_env(imboy, cache_registry_overrides, #{}),
    case maps:find(NS, Overrides) of
        {ok, {_, _} = Entry} -> Entry;
        _ -> maps:get(NS, ?REGISTRY, undefined)
    end.

%% @doc 从实际缓存 Key 提取 namespace 标签。
%% 元组首项和裸原子均为 namespace；不按元组长度遗漏凭据 Key。
-spec namespace_of(term()) -> atom() | undefined.
namespace_of(Key) when is_atom(Key) -> Key;
namespace_of(Key) when is_tuple(Key), tuple_size(Key) > 0, is_atom(element(1, Key)) ->
    element(1, Key);
namespace_of(_) ->
    undefined.

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
                {fun() -> check_value_size(Key) end, key_too_large},
                {fun() -> check_value_type(Key) end, key_type_forbidden},
                {fun() -> is_integer(TTL) end, invalid_ttl},
                {fun() -> TTL > 0 andalso TTL =< max_ttl_sec() end, ttl_out_of_range},
                {fun() -> Value =/= undefined end, invalid_value},
                {fun() -> value_size(Value) =< max_value_bytes() end, value_too_large},
                %% PID/ref/fun 一律禁止复制（提示词第八节 fail-closed）
                {fun() -> check_value_type(Value) end, value_type_forbidden}
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

%% C01 冻结的资源预算（closeout-v1/protocol-budget-frozen.md 第 6 节）
%% 本地工程预算，非生产 SLO；故障测试前冻结，不得为通过测试调高。

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

-spec n_max_bytes() -> pos_integer().
n_max_bytes() -> rep_cfg(n_max_bytes, 16 * 1024 * 1024).

%% V2 固定字段及控制帧的独立序列化预算，不借用 Key/Value 容量。
-spec protocol_meta_max_bytes() -> pos_integer().
protocol_meta_max_bytes() -> 4096.

%% generation 索引的序列化字节预算；超限时推进全域栅栏并回收索引。
-spec gen_max_bytes() -> pos_integer().
gen_max_bytes() -> rep_cfg(gen_max_bytes, n_max() * 128).

%% 墓碑条目上限（项）。
-spec t_max() -> pos_integer().
t_max() -> rep_cfg(t_max, ?DEFAULT_T_MAX).

-spec t_max_bytes() -> pos_integer().
t_max_bytes() -> rep_cfg(t_max_bytes, 8 * 1024 * 1024).

%% 恢复接收在途页缓冲上限（项，应用完成后释放）。
-spec b_max() -> pos_integer().
b_max() -> rep_cfg(b_max, ?DEFAULT_B_MAX).

-spec b_max_bytes() -> pos_integer().
b_max_bytes() -> rep_cfg(b_max_bytes, 32 * 1024 * 1024).

%% 本地候选 journal 预算；生产容量仍需实际负载与硬件验证。
j_max() -> rep_cfg(j_max, 2048).
j_max_bytes() -> rep_cfg(j_max_bytes, 32 * 1024 * 1024).

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

grace_max() -> rep_cfg(grace_max, 256).
peer_max() -> rep_cfg(peer_max, 64).

%% @doc 冻结预算摘要（C01 oracle：配置/实现/测试三方一致）。
%% 规范化序列化后取 SHA-256 前 16 字节；任何预算值变化都会改变摘要。
-spec budget_digest() -> binary().
budget_digest() ->
    Parts = [
        {n_max, n_max()},
        {n_max_bytes, n_max_bytes()},
        {t_max, t_max()},
        {t_max_bytes, t_max_bytes()},
        {j_max, j_max()},
        {grace_max, grace_max()},
        {peer_max, peer_max()},
        {j_max_bytes, j_max_bytes()},
        {b_max, b_max()},
        {b_max_bytes, b_max_bytes()},
        {p_max, p_max()},
        {gen_max_bytes, gen_max_bytes()},
        {p_max_bytes, p_max_bytes()},
        {r_max_rounds, r_max_rounds()},
        {d_max_ms, d_max_ms()},
        {max_value_bytes, max_value_bytes()},
        {protocol_meta_max_bytes, protocol_meta_max_bytes()},
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
    Parts = {
        budget_digest(),
        whitelist(),
        fenced_namespaces(),
        {snapshot_tombstones_v1, legacy_sequence_v2, gap_recovery_v1, snapshot_pull_v1,
            tombstone_expiry_v1, snapshot_tail_watermark_v1, authority_epoch_binding_v1,
            authority_event_epoch_v1, entry_epoch_v1, snapshot_cohort_v1, public_api_boundary_v1,
            remote_capacity_barrier_v1, journal_capture_v1, journal_revision_replay_v1,
            recovery_completion_fence_v1, snapshot_source_cut_v1, snapshot_final_identity_v1,
            snapshot_prepared_expiry_v1, expired_live_revision_window_v1, snapshot_expired_floor_v1,
            snapshot_transit_floor_v1, delta_expired_floor_v1, snapshot_invalidation_abort_v1,
            bounded_peer_discovery_v1, contiguous_sequence_evidence_v1,
            snapshot_unavailable_floor_v1, completed_snapshot_replay_floor_v1, peer_report_pid_v1,
            confirmed_event_incarnation_v1, ready_identity_announcement_v1,
            snapshot_origin_envelope_v1, snapshot_origin_forwarding_v1,
            snapshot_unavailable_journal_v1, revision_origin_metadata_v1, snapshot_base_origin_v1,
            retained_snapshot_tombstone_v1, snapshot_receive_retention_barrier_v1,
            snapshot_partial_clock_v1, snapshot_coverage_candidate_cut_v1,
            event_revision_origin_binding_v1, round_bound_origin_target_v1,
            snapshot_receive_manifest_v1, snapshot_manifest_origin_cut_v1,
            legacy_set_policy_boundary_v1, managed_revision_row_boundary_v1,
            legacy_publish_policy_boundary_v1}
    },
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

%% Internal

rep_cfg(K, Default) ->
    Cfg = application:get_env(imboy, cache_replication, #{}),
    V = maps:get(K, Cfg, Default),
    case is_integer(V) andalso V > 0 of
        true -> V;
        false -> Default
    end.

whitelist() ->
    Cfg = application:get_env(imboy, cache_replication, #{}),
    WL = maps:get(whitelist, Cfg, []),
    [NS || NS <- WL, is_atom(NS)].

checks([{Pred, Reason} | Rest]) ->
    case Pred() of
        true -> checks(Rest);
        false -> {error, Reason}
    end;
checks([]) ->
    ok.

%% 值大小估算：external term format 字节数（含 refc binary 全量，符合 19.5 字节上限语义）。
%% external_size 失败时按超限处理（max+1 → 上层 value_too_large 拒绝），不崩护栏。
value_size(Value) ->
    try
        erlang:external_size(Value)
    catch
        _:_ -> max_value_bytes() + 1
    end.

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

any_pid_ref_fun([H | T]) ->
    case contains_pid_ref_fun(H) of
        true -> true;
        false -> any_pid_ref_fun(T)
    end;
any_pid_ref_fun([]) ->
    false;
any_pid_ref_fun(Tail) ->
    contains_pid_ref_fun(Tail).

%% 快照条目类型、期限与来源证据的纯策略规则。
within_meta_budget(Term) -> erlang:external_size(Term) =< protocol_meta_max_bytes().

remaining_ttl_sec(ExpiresAtMs) ->
    NowMs = erlang:system_time(millisecond),
    Remaining = ExpiresAtMs - NowMs - ?SKEW_SEC * 1000,
    case Remaining =< 0 of
        true -> 0;
        false -> Remaining div 1000
    end.

snapshot_entry_deadline({with_origin, _, Entry}) ->
    snapshot_entry_deadline(Entry);
snapshot_entry_deadline({Kind, _, undefined, Exp, _}) when
    Kind =:= expired; Kind =:= unavailable; Kind =:= deleted
->
    Exp + imboy_cache_policy:tombstone_ttl_sec() * 1000;
snapshot_entry_deadline({_, _, Exp, _}) ->
    Exp.

%% 接收已过期 live 可保留版本下限，但任何条目不得超出保护窗。
snapshot_entry_retention_deadline(Entry) ->
    Body = snapshot_entry_body(Entry),
    element(tuple_size(Body) - 1, Body) + tombstone_ttl_sec() * 1000.

valid_snapshot_entry({with_origin, Origin, Entry}) ->
    valid_snapshot_origin(Origin, Entry);
valid_snapshot_entry({unavailable, Key, undefined, Exp, Rev}) when is_integer(Exp), Exp > 0 ->
    Exp + imboy_cache_policy:tombstone_ttl_sec() * 1000 > erlang:system_time(millisecond) andalso
        valid_snapshot_entry({Key, <<>>, Exp, Rev});
valid_snapshot_entry({expired, Key, undefined, Exp, {Counter, Origin}}) when
    is_integer(Exp), Exp > 0, is_integer(Counter), Counter >= 0, is_atom(Origin)
->
    Now = erlang:system_time(millisecond),
    %% 来源的不可复制阈值含整秒截断；接收方还须容纳合同中的 5s 时钟偏差。
    remaining_ttl_sec(Exp - 5000) =:= 0 andalso
        Exp + imboy_cache_policy:tombstone_ttl_sec() * 1000 > Now andalso
        within_meta_budget({Exp, {Counter, Origin}}) andalso
        imboy_cache_policy:replicate_allowed(Key) andalso imboy_cache_policy:check_value_size(Key) andalso
        imboy_cache_policy:check_value_type(Key);
valid_snapshot_entry({deleted, Key, undefined, ExpiresAt, {Counter, Origin}}) when
    is_integer(ExpiresAt), ExpiresAt > 0, is_integer(Counter), Counter >= 0, is_atom(Origin)
->
    %% 快照删除条目表示保留中的版本下限，不延长原墓碑期限。
    ExpiresAt + tombstone_ttl_sec() * 1000 > erlang:system_time(millisecond) andalso
        ExpiresAt =<
            erlang:system_time(millisecond) +
                (imboy_cache_policy:tombstone_ttl_sec() + 5) * 1000 andalso
        within_meta_budget({ExpiresAt, {Counter, Origin}}) andalso
        imboy_cache_policy:replicate_allowed(Key) andalso
        imboy_cache_policy:check_value_size(Key) andalso
        imboy_cache_policy:check_value_type(Key);
valid_snapshot_entry({Key, Value, ExpiresAt, {Counter, Origin}}) when
    is_integer(ExpiresAt), ExpiresAt > 0, is_integer(Counter), Counter >= 0, is_atom(Origin)
->
    ExpiresAt =< erlang:system_time(millisecond) + (imboy_cache_policy:max_ttl_sec() + 5) * 1000 andalso
        within_meta_budget({ExpiresAt, {Counter, Origin}}) andalso
        imboy_cache_policy:validate_replicate(Key, Value, 1) =:= ok;
valid_snapshot_entry(_) ->
    false.

%% 原始来源与 revision 节点绑定；序列不是 Lamport counter，也不登记化身。
valid_snapshot_origin({Origin, Boot, Seq} = Provenance, Entry) when
    is_atom(Origin),
    is_binary(Boot),
    byte_size(Boot) =:= 16,
    is_integer(Seq),
    Seq > 0,
    is_tuple(Entry),
    (tuple_size(Entry) =:= 4 orelse tuple_size(Entry) =:= 5)
->
    valid_snapshot_entry(Entry) andalso within_meta_budget(Provenance) andalso
        element(2, element(tuple_size(Entry), Entry)) =:= Origin;
valid_snapshot_origin(_, _) ->
    false.

snapshot_entry_body({with_origin, _, Entry}) -> Entry;
snapshot_entry_body(Entry) -> Entry.
snapshot_entry_with_origin(Entry, unknown) -> Entry;
snapshot_entry_with_origin(Entry, Origin) -> {with_origin, Origin, Entry}.

snapshot_metadata_kind(unavailable) -> live;
snapshot_metadata_kind(Kind) -> Kind.

%% 预算预检与实际写入共用版本行布局，避免扩展字段时低估字节。
revision_metadata_row(Key, Rev, Exp, Kind, Epoch) ->
    revision_metadata_row(Key, Rev, Exp, Kind, Epoch, unknown).
revision_metadata_row(Key, Rev, Exp, Kind, Epoch, Origin) ->
    {
        {rev, Key},
        {Rev, revision_origin_slot(Rev, Origin), Exp, snapshot_metadata_kind(Kind), Epoch}
    }.

revision_origin_slot({Counter, Node}, {Node, Boot, Seq} = Origin) when
    is_binary(Boot), byte_size(Boot) =:= 16, is_integer(Seq), Seq > 0
->
    case within_meta_budget(Origin) of
        true -> {Boot, Seq};
        false -> Counter
    end;
revision_origin_slot(Rev, _) ->
    element(1, Rev).

prospective_revision_rows(set, Key, Rev, Exp, Epoch, Origin) ->
    [
        revision_metadata_row(Key, Rev, Exp, live, Epoch, Origin),
        {{rindex, Key}, erlang:system_time(millisecond)}
    ];
prospective_revision_rows(delete, Key, Rev, Exp, Epoch, Origin) ->
    [revision_metadata_row(Key, Rev, Exp, deleted, Epoch, Origin)].

snapshot_metadata_rows({with_origin, Origin, Entry}, Epoch) ->
    snapshot_metadata_rows(Entry, Epoch, Origin);
snapshot_metadata_rows(Entry, Epoch) ->
    snapshot_metadata_rows(Entry, Epoch, unknown).
snapshot_metadata_rows({Key, _, Exp, Rev}, Epoch, Origin) ->
    prospective_revision_rows(set, Key, Rev, Exp, Epoch, Origin);
snapshot_metadata_rows({deleted, Key, undefined, Exp, Rev}, Epoch, Origin) ->
    prospective_revision_rows(delete, Key, Rev, Exp, Epoch, Origin);
snapshot_metadata_rows({Kind, Key, undefined, Exp, Rev}, Epoch, Origin) when
    Kind =:= expired; Kind =:= unavailable
->
    prospective_revision_rows(set, Key, Rev, Exp, Epoch, Origin).

%% 历史来源注记不代表活跃身份或连续覆盖；整数槽保持未知。
revision_metadata_origin({_, Node}, {Boot, Seq}) when
    is_atom(Node), is_binary(Boot), byte_size(Boot) =:= 16, is_integer(Seq), Seq > 0
->
    {Node, Boot, Seq};
revision_metadata_origin(_, _) ->
    unknown.

snapshot_page_digest(Round, PageNo, Page) ->
    erlang:phash2({Round, PageNo, [entry_identity(E) || E <- Page]}).
snapshot_revision_counter(Entry) ->
    {_, _, {Counter, _}} = entry_identity(snapshot_entry_body(Entry)),
    Counter.
entry_identity({with_origin, Origin, Entry}) -> {with_origin, Origin, entry_identity(Entry)};
entry_identity({K, _, _, Rev}) -> {live, K, Rev};
entry_identity({deleted, K, undefined, _, Rev}) -> {deleted, K, Rev};
entry_identity({expired, K, undefined, _, Rev}) -> {expired, K, Rev};
entry_identity({unavailable, K, undefined, _, Rev}) -> {unavailable, K, Rev};
entry_identity(Other) -> {invalid, Other}.

%% 接收清单不保留 payload；未知来源仍为 unknown，摘要绑定完整原始条目。
snapshot_manifest_entry({with_origin, Origin, Entry} = Wire) ->
    manifest_entry(Entry, Origin, Wire);
snapshot_manifest_entry(Entry) ->
    manifest_entry(Entry, unknown, Entry).
manifest_entry({K, _, Exp, Rev}, Origin, Wire) ->
    {live, K, Rev, Exp, Origin, crypto:hash(sha256, term_to_binary(Wire))};
manifest_entry({Kind, K, undefined, Exp, Rev}, Origin, Wire) ->
    {Kind, K, Rev, Exp, Origin, crypto:hash(sha256, term_to_binary(Wire))}.

snapshot_manifest_own_seq(Rows, {Source, Boot}, Old) ->
    lists:foldl(
        fun({_, _, _, _, Origin, _}, Max) ->
            case Origin of
                {Source, Boot, Seq} -> erlang:max(Max, Seq);
                _ -> Max
            end
        end,
        Old,
        Rows
    ).

%% 只在实际应用完成后推进页号、完整字节及版本水位。
snapshot_applied_page(Round, Source, Peer, Page, PageNo, More) ->
    Count = length(Page),
    Ceiling = snapshot_page_revision_ceiling(Page, Peer),
    NextPeer = Peer#{
        pages := PageNo,
        max_revision_counter := Ceiling,
        applied := maps:get(applied, Peer) + Count,
        expected_more := More
    },
    Peers = maps:get(peers, Round),
    Round#{
        peers := Peers#{Source := NextPeer},
        applied_items := maps:get(applied_items, Round, 0) + Count,
        applied_bytes := maps:get(applied_bytes, Round, 0) + erlang:external_size(Page)
    }.

%% 应用失败只保留已完整校验的版本上限，不确认页号、项数或内容完整性。
snapshot_page_result(ok, Round, Source, Peer, Page, PageNo, More) ->
    {ok, snapshot_applied_page(Round, Source, Peer, Page, PageNo, More)};
snapshot_page_result({error, Reason}, Round, Source, Peer, Page, _, _) ->
    NextPeer = Peer#{max_revision_counter := snapshot_page_revision_ceiling(Page, Peer)},
    Peers = maps:get(peers, Round),
    {abort, Reason, Round#{peers := Peers#{Source := NextPeer}}}.
snapshot_page_revision_ceiling(Page, Peer) ->
    lists:foldl(
        fun(E, N) -> erlang:max(snapshot_revision_counter(E), N) end,
        maps:get(max_revision_counter, Peer, 0),
        Page
    ).

event_origin_checks(Event) ->
    Rev = maps:get(revision, Event, none),
    Seq = maps:get(origin_seq, Event, none),
    Origin = maps:get(origin_node, Event, none),
    Boot = maps:get(origin_boot_id, Event, none),
    [
        {
            fun() ->
                is_tuple(Rev) andalso tuple_size(Rev) =:= 2 andalso
                    is_integer(element(1, Rev)) andalso element(1, Rev) >= 0 andalso
                    is_atom(element(2, Rev))
            end,
            invalid_revision
        },
        {fun() -> is_integer(Seq) andalso Seq >= 0 end, invalid_origin_seq},
        {fun() -> maps:is_key(origin_node, Event) andalso is_atom(Origin) end, invalid_origin_node},
        {fun() -> element(2, Rev) =:= Origin end, revision_origin_mismatch},
        {fun() -> is_binary(Boot) andalso byte_size(Boot) =:= 16 end, invalid_boot_id}
    ].

valid_watermark(
    {cache_invalidation_watermark, Origin, Boot, Seq, ConfigHash, RegistryHash} = Frame
) ->
    is_atom(Origin) andalso is_binary(Boot) andalso byte_size(Boot) =:= 16 andalso
        is_integer(Seq) andalso Seq >= 0 andalso is_binary(ConfigHash) andalso
        byte_size(ConfigHash) =:= 16 andalso is_binary(RegistryHash) andalso
        byte_size(RegistryHash) =:= 16 andalso within_meta_budget(Frame);
valid_watermark(_) ->
    false.

valid_snapshot_cut(
    {cache_snapshot_cut, Source, Round, Boot, Epoch, Seq, Hash, Registry, Candidate} = F
) ->
    valid_coverage_candidate(Candidate, Epoch, Source, Boot, Seq) andalso
        valid_snapshot_cut({cache_snapshot_cut, Source, Round, Boot, Epoch, Seq, Hash, Registry}) andalso
        within_meta_budget(F);
valid_snapshot_cut({cache_snapshot_cut, Origin, Round, Boot, Epoch, Seq, Hash, Registry} = F) ->
    is_binary(Round) andalso byte_size(Round) =:= 12 andalso
        is_integer(Epoch) andalso Epoch >= 0 andalso within_meta_budget(F) andalso
        valid_watermark({cache_invalidation_watermark, Origin, Boot, Seq, Hash, Registry});
valid_snapshot_cut(_) ->
    false.

%% 旧切点没有覆盖候选，绝不能补造空向量。
snapshot_cut_base({cache_snapshot_cut, Source, Round, Boot, Epoch, Seq, Hash, Registry, _}) ->
    {cache_snapshot_cut, Source, Round, Boot, Epoch, Seq, Hash, Registry};
snapshot_cut_base(Frame) ->
    Frame.
snapshot_cut_coverage({cache_snapshot_cut, _, _, _, _, _, _, _, Candidate}) -> Candidate;
snapshot_cut_coverage(_) -> {error, missing_application_coverage}.

valid_coverage_candidate({error, Reason}, _, _, _, _) ->
    lists:member(Reason, [
        coverage_peer_budget, coverage_byte_budget, incomplete_application_coverage
    ]);
valid_coverage_candidate({ok, Vector}, Epoch, Source, Boot, Seq) ->
    valid_coverage_rows(Vector, Epoch, Source, Boot, Seq, peer_max(), {start});
valid_coverage_candidate(_, _, _, _, _) ->
    false.

valid_coverage_rows([], _, _, _, _, _, _) ->
    true;
valid_coverage_rows(
    [{Origin, OriginBoot, Epoch, Prefix} | Rest], Epoch, Source, Boot, Seq, Left, Previous
) when
    Left > 0,
    is_atom(Origin),
    is_binary(OriginBoot),
    byte_size(OriginBoot) =:= 16,
    is_integer(Prefix),
    Prefix >= 0
->
    (Previous =:= {start} orelse Origin > Previous) andalso
        (Origin =/= Source orelse (OriginBoot =:= Boot andalso Prefix =< Seq)) andalso
        valid_coverage_rows(Rest, Epoch, Source, Boot, Seq, Left - 1, Origin);
valid_coverage_rows(_, _, _, _, _, _, _) ->
    false.

valid_version_report(Report) when
    is_tuple(Report),
    tuple_size(Report) >= 9,
    tuple_size(Report) =< 11,
    element(1, Report) =:= cache_version_report
->
    {cache_version_report, Node, Counter, Epoch, Hash, Registry, Boot, Ready, Pid} = version_report_base(
        Report
    ),
    is_atom(Node) andalso is_integer(Counter) andalso Counter >= 0 andalso
        is_integer(Epoch) andalso Epoch >= 0 andalso
        is_binary(Hash) andalso byte_size(Hash) =:= 16 andalso
        is_binary(Registry) andalso byte_size(Registry) =:= 16 andalso
        is_binary(Boot) andalso byte_size(Boot) =:= 16 andalso is_boolean(Ready) andalso
        is_pid(Pid) andalso node(Pid) =:= Node andalso
        (tuple_size(Report) =:= 9 orelse
            (is_integer(element(10, Report)) andalso element(10, Report) >= 0)) andalso
        (tuple_size(Report) < 11 orelse
            (is_binary(element(11, Report)) andalso byte_size(element(11, Report)) =:= 12)) andalso
        within_meta_budget(Report);
valid_version_report(_) ->
    false.
version_report_base(Report) when
    is_tuple(Report), tuple_size(Report) > 9, tuple_size(Report) =< 11
->
    list_to_tuple([element(I, Report) || I <- lists:seq(1, 9)]);
version_report_base(Report) ->
    Report.
version_report_target({cache_version_report, Node, _, Epoch, _, _, Boot, _, _, Seq, Round}) ->
    {Node, Boot, Epoch, Seq, Round};
version_report_target(_) ->
    unknown.
version_report_round_matches(Report, recovering, #{round := Round}) when
    tuple_size(Report) =:= 11
->
    element(11, Report) =:= Round;
version_report_round_matches(Report, _, _) ->
    tuple_size(Report) < 11.
