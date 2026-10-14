-module(login_attempt_state).
-moduledoc "Limiter V1 权威状态纯逻辑（计划附录 C）：行结构、窗口规则、配置。".
%%%===================================================================
%%% login_attempt_ds gen_server 的纯状态辅助模块（>800 行拆分产物）：
%%% 无进程、无 ETS 写入（行构造/快照构造为纯函数）；owner 是唯一调用者
%%% 与唯一写者。时钟统一经 login_attempt_clock:now_ms/0（测试注入 seam）。
%%%===================================================================

-include("log.hrl").

%% 默认配置（C.5）
-define(MAX_ATTEMPTS_DEFAULT, 5).
-define(LOCK_DURATION_MIN_DEFAULT, 30).
-define(IP_RATES_DEFAULT, #{
    passport_per_ip => {10, 60000},
    api_per_ip => {60, 60000},
    oidc_request => {100, 3600000}
}).
-define(MAX_ACCOUNT_ENTRIES_DEFAULT, 200000).
-define(MAX_IP_ENTRIES_DEFAULT, 200000).
-define(MAX_KEY_BYTES_DEFAULT, 256).
-define(CLEANUP_BATCH_DEFAULT, 1000).
-define(AUTH_DEADLINE_MS_DEFAULT, 10000).

-export([
    initial_state/0,
    current_account_rule/0,
    env_pos/2,
    init_ip_rates/0,
    valid_identity/1,
    normalize_identity/1,
    mk_compat_identity/1,
    gate_account/1,
    mark_degraded/1,
    lookup_account/1,
    account_row_key/1,
    new_account_row/2,
    live_row/4,
    account_snap/3,
    admit_snap/3,
    advance_scope/2,
    apply_ip_rates/2,
    valid_rule/1,
    valid_rates/1,
    sanitize_id/1
]).

-type scope() :: passport_per_ip | api_per_ip | oidc_request.
-type identity() :: {user, pos_integer()} | {unknown, email | mobile | account, binary()}.
-type rule() :: {Threshold :: pos_integer(), WindowMs :: pos_integer()}.
%% scope 元信息：anchor_mode=scope（HTTP，t0 对齐统一换窗）| per_key（OIDC）
-type scope_meta() ::
    #{
        rule := rule(),
        window_start_ms := integer(),
        anchor_mode := scope | per_key,
        pending => {integer(), rule()}
    }.
-type snapshot() ::
    #{
        count := non_neg_integer(),
        remaining := non_neg_integer(),
        locked := boolean(),
        expires_at_ms := integer() | undefined,
        rule_version := term()
    }.
-type state() ::
    #{
        ready := boolean(),
        degraded := boolean(),
        account_max_entries := pos_integer(),
        admit_max_entries := pos_integer(),
        max_key_bytes := pos_integer(),
        cleanup_batch := pos_integer(),
        auth_deadline_ms := pos_integer(),
        scopes := #{scope() => scope_meta()},
        account_count := non_neg_integer(),
        admit_counts := #{scope() => non_neg_integer()},
        cleanup_cursor := term()
    }.

-export_type([scope/0, identity/0, rule/0, scope_meta/0, snapshot/0, state/0]).

%%%===================================================================
%%% 初始化与配置
%%%===================================================================

-spec initial_state() -> state().
initial_state() ->
    Rates = init_ip_rates(),
    Scopes = maps:map(
        fun(Scope, R) ->
            #{
                rule => R,
                window_start_ms => 0,
                anchor_mode => anchor_mode_of(Scope)
            }
        end,
        Rates
    ),
    #{
        ready => false,
        degraded => false,
        account_max_entries =>
            env_pos(limiter_max_account_entries, ?MAX_ACCOUNT_ENTRIES_DEFAULT),
        admit_max_entries =>
            env_pos(limiter_max_ip_entries, ?MAX_IP_ENTRIES_DEFAULT),
        max_key_bytes => env_pos(limiter_max_key_bytes, ?MAX_KEY_BYTES_DEFAULT),
        cleanup_batch => env_pos(limiter_cleanup_batch, ?CLEANUP_BATCH_DEFAULT),
        auth_deadline_ms =>
            env_pos(limiter_auth_deadline_ms, ?AUTH_DEADLINE_MS_DEFAULT),
        scopes => Scopes,
        account_count => 0,
        admit_counts => #{},
        cleanup_cursor => '$end_of_table'
    }.

%% C.5：limiter_ip_rates（缺省取内置默认）；逐 scope 校验为正整数二元组。
-spec init_ip_rates() -> #{scope() => rule()}.
init_ip_rates() ->
    case application:get_env(imboy, limiter_ip_rates) of
        {ok, Rates} when is_map(Rates) ->
            maps:filter(fun(_S, R) -> valid_rule(R) end, Rates);
        _ ->
            ?IP_RATES_DEFAULT
    end.

-spec env_pos(atom(), pos_integer()) -> pos_integer().
env_pos(Key, Default) ->
    case application:get_env(imboy, Key) of
        {ok, Val} when is_integer(Val), Val > 0 -> Val;
        _ -> Default
    end.

valid_rule({N, P}) when is_integer(N), N > 0, is_integer(P), P > 0 -> true;
valid_rule(_) -> false.

valid_rates(Rates) ->
    maps:size(Rates) > 0 andalso
        lists:all(fun valid_rule/1, maps:values(Rates)) andalso
        lists:all(fun is_atom/1, maps:keys(Rates)).

anchor_mode_of(oidc_request) -> per_key;
anchor_mode_of(_) -> scope.

%%%===================================================================
%%% 身份
%%%===================================================================

-spec valid_identity(identity()) -> boolean().
valid_identity({user, Uid}) when is_integer(Uid), Uid > 0 -> true;
valid_identity({unknown, Kind, Norm}) when
    (Kind =:= email orelse Kind =:= mobile orelse Kind =:= account),
    is_binary(Norm)
->
    true;
valid_identity(_) ->
    false.

%% 兼容层第二参数归一：binary Identifier → {unknown, account, Identifier}；
%% identity() 直通；其它非法返回 invalid_input 标记。
-spec normalize_identity(term()) -> identity() | {error, invalid_input}.
normalize_identity(I) ->
    case valid_identity(I) of
        true -> I;
        false -> {error, invalid_input}
    end.

-spec mk_compat_identity(term()) -> identity().
mk_compat_identity(Identifier) when is_binary(Identifier) ->
    {unknown, account, Identifier};
mk_compat_identity(I) ->
    I.

-spec sanitize_id(identity()) -> term().
sanitize_id({user, Uid}) -> Uid;
sanitize_id({unknown, _Kind, Norm}) -> sanitize_identifier(Norm);
sanitize_id(X) -> X.

sanitize_identifier(Identifier) ->
    case byte_size(Identifier) of
        N when N > 8 ->
            Prefix = binary:part(Identifier, 0, 4),
            Suffix = binary:part(Identifier, N - 4, 4),
            <<Prefix/binary, "***", Suffix/binary>>;
        _ ->
            Identifier
    end.

%%%===================================================================
%%% 账户域门与行结构
%%%===================================================================

%% 账户域门：未 ready 或 degraded → blocked。
-spec gate_account(state()) -> ok | blocked.
gate_account(State) ->
    case maps:get(ready, State) andalso not maps:get(degraded, State) of
        true -> ok;
        false -> blocked
    end.

-spec mark_degraded(state()) -> state().
mark_degraded(State) ->
    ok = ?ERROR_LOG([login_attempt, account_domain_degraded, attempt_not_settled]),
    State#{degraded => true}.

account_row_key(I) ->
    {limiter_account, I}.

%% 读行并判定逻辑过期（now >= expires 视过期，物理行留给 cleanup）。
-spec lookup_account(identity()) ->
    none
    | {live, Count :: non_neg_integer(), ExpiresAt :: integer(), RV :: rule(), Max :: pos_integer()}
    | {expired, non_neg_integer(), integer(), rule(), pos_integer()}.
lookup_account(I) ->
    case ets:lookup(login_attempt_ets, account_row_key(I)) of
        [] ->
            none;
        [{_, #{count := C, expires_at_ms := Exp, rule_version := RV}}] ->
            {Max, _} = RV,
            case login_attempt_clock:now_ms() >= Exp of
                true -> {expired, C, Exp, RV, Max};
                false -> {live, C, Exp, RV, Max}
            end
    end.

new_account_row({F, D}, Now) ->
    live_row(1, Now + D, {F, D}, F).

live_row(Count, Exp, RV, Max) ->
    #{count => Count, expires_at_ms => Exp, rule_version => RV, max => Max}.

account_snap(Count, {F, _D} = RV, Exp) ->
    #{
        count => Count,
        remaining => max(0, F - Count),
        locked => Count >= F,
        expires_at_ms => Exp,
        rule_version => RV
    }.

admit_snap(Count, Meta, Exp) ->
    {N, P} = maps:get(rule, Meta),
    #{
        count => Count,
        remaining => max(0, N - Count),
        locked => Count >= N,
        expires_at_ms => Exp,
        rule_version => {N, P}
    }.

%%%===================================================================
%%% 窗口与规则（C.4）
%%%===================================================================

%% 账户域规则每次窗口创建时读取当前 env（冻结进新窗）。
-spec current_account_rule() -> rule().
current_account_rule() ->
    F = env_pos(login_max_attempts, ?MAX_ATTEMPTS_DEFAULT),
    D = env_pos(login_lock_duration_minutes, ?LOCK_DURATION_MIN_DEFAULT) * 60000,
    {F, D}.

%% 推进 scope 到 now 所属窗口；HTTP 跨边界时消费 pending 规则（以新边界
%% 为 anchor），OIDC per_key 无 scope 级窗口。
-spec advance_scope(scope(), state()) -> {scope_meta(), state()}.
advance_scope(Scope, State) ->
    Scopes = maps:get(scopes, State),
    Meta = maps:get(Scope, Scopes),
    case maps:get(anchor_mode, Meta) of
        per_key ->
            {Meta, State};
        scope ->
            Now = login_attempt_clock:now_ms(),
            {Meta2, Scopes2} = advance_scope_window(Scope, Meta, Now, Scopes),
            {Meta2, State#{scopes => Scopes2}}
    end.

-spec advance_scope_window(scope(), scope_meta(), integer(), map()) ->
    {scope_meta(), map()}.
advance_scope_window(Scope, Meta, Now, Scopes) ->
    {_N, P} = maps:get(rule, Meta),
    Anchor = maps:get(window_start_ms, Meta),
    %% monotonic 基准任意（可为负）：统一 floor((now-anchor)/P) 定位当前窗，
    %% 正/负方向跨界都直接对齐，不依赖"now 已越过 anchor+P"的正向假设。
    Boundary = Anchor + ((Now - Anchor) div P) * P,
    case Boundary =:= Anchor of
        true ->
            {Meta, Scopes};
        false ->
            apply_boundary(Scope, Meta, Boundary, Now, Scopes)
    end.

%% 边界推进：pending 到期则切换规则（所有 Key 同版本）。
-spec apply_boundary(scope(), scope_meta(), integer(), integer(), map()) ->
    {scope_meta(), map()}.
apply_boundary(Scope, Meta, Boundary, _Now, Scopes) ->
    case maps:find(pending, Meta) of
        {ok, {FromMs, NewRule}} when FromMs =< Boundary ->
            Meta2 = Meta#{rule => NewRule, window_start_ms => Boundary},
            {maps:remove(pending, Meta2), Scopes#{Scope := Meta2}};
        _ ->
            Meta2 = Meta#{window_start_ms => Boundary},
            {Meta2, Scopes#{Scope := Meta2}}
    end.

%% HTTP pending 登记下一边界；OIDC 直接替换（逐 Key 冻结，仅新窗生效）。
-spec apply_ip_rates(#{scope() => rule()}, state()) -> state().
apply_ip_rates(Rates, State) ->
    Scopes0 = maps:get(scopes, State),
    Scopes = maps:fold(
        fun(Scope, Rule, Acc) ->
            case maps:find(Scope, Acc) of
                error -> Acc;
                {ok, Meta} -> register_scope_rule(Scope, Meta, Rule, Acc)
            end
        end,
        Scopes0,
        Rates
    ),
    State#{scopes => Scopes}.

-spec register_scope_rule(scope(), scope_meta(), rule(), map()) -> map().
register_scope_rule(Scope, Meta, Rule, Scopes) ->
    case maps:get(anchor_mode, Meta) of
        per_key ->
            Scopes#{Scope := Meta#{rule => Rule}};
        scope ->
            {_N, P} = maps:get(rule, Meta),
            Anchor = maps:get(window_start_ms, Meta),
            Now = login_attempt_clock:now_ms(),
            FromMs = next_boundary(Anchor, P, Now),
            case maps:find(pending, Meta) of
                {ok, {FromMs0, _}} ->
                    %% 生效前重复提交：规则按最后一次替换，保留最早边界
                    Scopes#{Scope := Meta#{pending => {FromMs0, Rule}}};
                _ ->
                    Scopes#{Scope := Meta#{pending => {FromMs, Rule}}}
            end
    end.

-spec next_boundary(integer(), pos_integer(), integer()) -> integer().
next_boundary(Anchor, _P, Now) when Now < Anchor ->
    Anchor;
next_boundary(Anchor, P, Now) ->
    Anchor + ((Now - Anchor) div P + 1) * P.
