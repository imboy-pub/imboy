-module(login_attempt_ds).
%%% @doc Limiter V1 唯一权威 owner，合同见计划附录 C.14。
%%% 账户按身份跨 IP 累计；HTTP scope 对齐固定窗，OIDC 按 Key 固定窗。
%%% 认证终态串行确认；未交付的 DOWN/deadline 令账户域 degraded。
%%% owner 唯一写 protected ETS；重启默认关闭，空态批准绑定具体 PID。
%%% 时间统一经 login_attempt_clock；纯规则在 login_attempt_state。

-behaviour(gen_server).

-dialyzer({nowarn_function, [handle_call/3]}).

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% ---- 权威 API ----
-export([
    check/1,
    failure/1,
    reset/1,
    snapshot/1,
    admin_unlock/2,
    admit/2,
    begin_attempt/3,
    end_attempt/2,
    ready/0,
    activate_empty/1
]).
%% ---- 配置登记辅助（C.4 HTTP scope pending；A3 装配用） ----
-export([set_ip_rates/1]).
%% ---- 生命周期（imboy_sup） ----
-export([start_link/0]).
%% ---- 兼容层（deprecated：A2/A3 重写调用者后移除） ----
-export([
    record_failure/2,
    is_locked/2,
    get_attempts/2,
    reset/2,
    get_remaining_attempts/2,
    cache_key/2,
    check_ip_rate_limit/1,
    extract_real_ip/1,
    validate_ip/1,
    is_admin/1
]).
%% ---- gen_server 回调 ----
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(ETS, login_attempt_ets).
-define(DEADLINES, login_attempt_deadlines).
-define(K_ADMIT(Scope, Key), {limiter_admit, Scope, Key}).
-define(K_ATTEMPT(Ref), {limiter_attempt, Ref}).

-define(CLEANUP_INTERVAL_MS, 60000).
-define(DEADLINE_CHECK_INTERVAL_MS, 1000).
-define(CALL_TIMEOUT, 5000).

-type scope() :: login_attempt_state:scope().
-type identity() :: login_attempt_state:identity().
-type attempt_ref() :: reference().
-type attempt_outcome() :: failure | reset | no_count.
-type snapshot() :: login_attempt_state:snapshot().
-type state() :: login_attempt_state:state().

%% @doc 认证准入检查：不计数、不预扣；degraded/未 ready/owner 缺失 → unavailable。
-spec check(identity()) -> {ok, snapshot()} | {error, unavailable}.
check(Identity) ->
    safe_call({check, Identity}).

%% @doc 提交一次已确定的凭据失败记账（含已达阈值的在途失败）。
%% 账户域建行容量满 → 原子置 degraded 并返回 capacity_lost；此后本域全部
%% 操作返回 unavailable，不自动恢复（C.7）。
-spec failure(identity()) -> {ok, snapshot()} | {error, capacity_lost | unavailable}.
failure(Identity) ->
    safe_call({failure, Identity}).

%% @doc 登录成功清账户域失败状态；degraded 下拒绝（不授权签发凭据）。
-spec reset(identity()) -> ok | {error, unavailable}.
reset(Identity) ->
    safe_call({reset, Identity}).

%% @doc 只读快照：count/remaining/locked/expires_at_ms/rule_version 出自同一次
%% 逻辑判断；degraded 算 unavailable（不给"完整准确"的假快照）。
-spec snapshot(identity()) -> {ok, snapshot()} | {error, unavailable}.
snapshot(Identity) ->
    safe_call({snapshot, Identity}).

%% @doc 管理员解锁：清指定身份在账户域的失败状态（新鲜身份域）。
%% 第二参数兼容旧 Identifier binary（映射 {unknown, account, Identifier}）。
-spec admin_unlock(integer(), identity() | binary()) ->
    {ok, unlocked} | {error, unauthorized | invalid_input | unavailable}.
admin_unlock(AdminUid, Identity) when is_integer(AdminUid) ->
    case login_attempt_state:normalize_identity(Identity) of
        {error, invalid_input} -> {error, invalid_input};
        NormId -> safe_call({admin_unlock, AdminUid, NormId})
    end;
admin_unlock(_, _) ->
    {error, invalid_input}.

%% @doc 原子请求准入：先判断并占额度，再由调用者执行业务；拒绝不加数、
%% 不退款。HTTP scope 按 t0=0 对齐 period 统一换窗；OIDC 每 Key 首次 admit
%% anchor 固定 3600000ms。新 Key 容量满 → {error, capacity}（不降级，有位置
%% 后自动可用）；超长 Key 同 capacity（资源边界）、空 Key → {error,
%% unavailable}（无效请求）。
-spec admit(scope(), binary()) ->
    {ok, snapshot()} | {error, rate_limited | capacity | unavailable}.
admit(Scope, Key) when is_binary(Key) ->
    safe_call({admit, Scope, Key});
admit(_, _) ->
    {error, capacity}.

%% @doc 认证准入登记：monitor 实际执行认证的进程。DeadlineMs 用 `default'
%% 读 imboy.limiter_auth_deadline_ms（默认 10000）。degraded/未 ready →
%% unavailable（本次不认证）。
-spec begin_attempt(identity(), pid(), pos_integer() | default) ->
    {ok, attempt_ref()} | {error, unavailable}.
begin_attempt(Identity, Pid, DeadlineMs) when is_pid(Pid) ->
    safe_call({begin_attempt, Identity, Pid, DeadlineMs});
begin_attempt(_, _, _) ->
    {error, unavailable}.

%% @doc 认证终态提交并释放 monitor：failure/reset 附带记账，no_count 仅释放
%% （不计数错误、主动取消）。记账错误必须回传，未知 Ref 一律拒绝。
-spec end_attempt(attempt_ref(), attempt_outcome()) ->
    ok | {error, unavailable | capacity_lost}.
end_attempt(Ref, Outcome) when Outcome =:= failure; Outcome =:= reset; Outcome =:= no_count ->
    safe_call({end_attempt, Ref, Outcome});
end_attempt(_, _) ->
    {error, unavailable}.

%% @doc 未经受控空态批准、owner 死亡/未起均 false。
-spec ready() -> boolean().
ready() ->
    try gen_server:call(?MODULE, ready, ?CALL_TIMEOUT) of
        R when is_boolean(R) -> R
    catch
        _:_ -> false
    end.

%% @doc 仅供可信节点运维：批准指定空 owner 开放。PID 绑定防止迟到批准
%% 落到自动重启的新 owner；不得用于清除在役状态或 degraded。
-spec activate_empty(pid()) -> ok | {error, unavailable}.
activate_empty(Owner) when is_pid(Owner) ->
    try
        gen_server:call(Owner, activate_empty, ?CALL_TIMEOUT)
    catch
        _:_ -> {error, unavailable}
    end;
activate_empty(_) ->
    {error, unavailable}.

%% @doc 登记新的 IP 限流规则（C.4 配置变更合同）：
%% - passport_per_ip / api_per_ip：登记 pending，在本 scope 下一窗口边界统一
%%   切换（所有 Key 同版本）；重复提交在生效前按最后一次替换规则、保留最早
%%   生效边界。
%% - oidc_request：直接替换 scope 规则（逐 Key 冻结：仅影响新窗口/新 Key）。
%% 非法规则（非正整数阈值/期限、未知 scope）整体拒绝，不清空既有限制。
-spec set_ip_rates(#{scope() => login_attempt_state:rule()}) -> ok | {error, invalid_rates}.
set_ip_rates(Rates) when is_map(Rates) ->
    case login_attempt_state:valid_rates(Rates) of
        true -> safe_call({set_ip_rates, Rates});
        false -> {error, invalid_rates}
    end;
set_ip_rates(_) ->
    {error, invalid_rates}.

%% @doc 启动权威 owner（imboy_sup permanent child）。
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% deprecated: 旧 (Identifier, Ip) 键映射为 {unknown, account, Identifier}
%% 过渡语义（跨 IP，符合 C.1 账户域合同）。返回 {ok, Count} | 新错误 tuple。
record_failure(Identifier, _Ip) when is_binary(Identifier), byte_size(Identifier) > 0 ->
    case failure({unknown, account, Identifier}) of
        {ok, Snap} -> {ok, maps:get(count, Snap)};
        Other -> Other
    end;
record_failure(Identifier, Ip) ->
    ok = ?ERROR_LOG([login_attempt, invalid_input, Identifier, Ip]),
    {error, invalid_input}.

%% deprecated: 兼容转发 check/1。任何 unavailable（含 degraded/owner 缺失）
%% 保守返回 true（拒绝认证方向），不得把故障放行为 false。
is_locked(<<>>, _Ip) ->
    false;
is_locked(Identifier, _Ip) when is_binary(Identifier) ->
    case check({unknown, account, Identifier}) of
        {ok, Snap} -> maps:get(locked, Snap);
        {error, _} -> true
    end;
is_locked(_, _) ->
    false.

%% deprecated: 兼容转发 snapshot/1 的 count。
get_attempts(Identifier, _Ip) ->
    case snapshot(login_attempt_state:mk_compat_identity(Identifier)) of
        {ok, Snap} -> {ok, maps:get(count, Snap)};
        {error, R} -> {error, R}
    end.

%% deprecated: 兼容转发 reset/1（清新身份域，跨 IP）。
reset(Identifier, _Ip) ->
    reset(login_attempt_state:mk_compat_identity(Identifier)).

%% deprecated: 兼容转发 snapshot/1 的 remaining。
get_remaining_attempts(Identifier, _Ip) ->
    case snapshot(login_attempt_state:mk_compat_identity(Identifier)) of
        {ok, Snap} -> {ok, maps:get(remaining, Snap)};
        {error, R} -> {error, R}
    end.

%% deprecated: 旧键构造器；新权威不写 Cache。
cache_key(Identifier, Ip) ->
    {login_attempt, Identifier, Ip}.

%% deprecated: 转发 admit(oidc_request, Ip)（C.5 默认 {100, 3600000}）。
%% 空/非法 IP 保留旧 invalid_ip 语义；{ok, Snap} → {ok, Count}，错误 tag
%% 原样透传（rate_limited | capacity | unavailable），不伪装成放行。
check_ip_rate_limit(Ip) when is_binary(Ip), byte_size(Ip) > 0 ->
    case admit(oidc_request, Ip) of
        {ok, Snap} -> {ok, maps:get(count, Snap)};
        Other -> Other
    end;
check_ip_rate_limit(Ip) ->
    ok = ?WARN_LOG([login_attempt, invalid_ip, Ip]),
    {error, invalid_ip}.

%% deprecated: A3 将迁移至 elib_req:get_client_ip/1（含可信代理）。
extract_real_ip(Headers) when is_map(Headers) ->
    CfConnectingIp = maps:get(<<"cf-connecting-ip">>, Headers, <<>>),
    XRealIp = maps:get(<<"x-real-ip">>, Headers, <<>>),
    XForwardedFor = maps:get(<<"x-forwarded-for">>, Headers, <<>>),
    RawIp =
        case CfConnectingIp of
            <<>> ->
                case XRealIp of
                    <<>> -> XForwardedFor;
                    _ -> XRealIp
                end;
            _ ->
                CfConnectingIp
        end,
    case validate_ip(parse_first_ip(RawIp)) of
        {ok, ValidIp} -> {ok, ValidIp};
        {error, _} -> {ok, <<"0.0.0.0">>}
    end;
extract_real_ip(_) ->
    {ok, <<"0.0.0.0">>}.

%% deprecated: A3 将迁移至 elib_req 统一规范化。
validate_ip(Ip) when is_binary(Ip), byte_size(Ip) > 0 ->
    TrimmedIp = string:trim(Ip),
    case inet:parse_ipv4strict_address(binary_to_list(TrimmedIp)) of
        {ok, _} ->
            {ok, TrimmedIp};
        {error, _} ->
            case inet:parse_ipv6strict_address(binary_to_list(TrimmedIp)) of
                {ok, _} -> {ok, TrimmedIp};
                _ -> {error, invalid_ip}
            end
    end;
validate_ip(_) ->
    {error, invalid_ip}.

%% @doc 管理员判定（现逻辑保留；unlock 入口经 admin_unlock/2 权限校验）。
-spec is_admin(integer()) -> boolean().
is_admin(Uid) when is_integer(Uid) ->
    case application:get_env(imboy, admin_uids) of
        {ok, AdminUids} when is_list(AdminUids) -> lists:member(Uid, AdminUids);
        _ -> Uid < 10000
    end;
is_admin(_) ->
    false.

-spec init([]) -> {ok, state()}.
init([]) ->
    _ = ets:new(?ETS, [
        named_table, set, protected, {read_concurrency, true}
    ]),
    _ = ets:new(?DEADLINES, [named_table, ordered_set, protected]),
    {ok, _} = timer:send_interval(?CLEANUP_INTERVAL_MS, self(), cleanup_expired),
    {ok, _} = timer:send_interval(?DEADLINE_CHECK_INTERVAL_MS, self(), deadline_check),
    {ok, login_attempt_state:initial_state()}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(ready, _From, State) ->
    {reply, maps:get(ready, State), State};
handle_call(activate_empty, _From, State) ->
    case
        not maps:get(ready, State) andalso not maps:get(degraded, State) andalso
            ets:info(?ETS, size) =:= 0
    of
        true -> {reply, ok, State#{ready => true}};
        false -> {reply, {error, unavailable}, State}
    end;
handle_call({check, I}, _From, State) ->
    {R, S2} = do_read(I, State),
    {reply, R, S2};
handle_call({snapshot, I}, _From, State) ->
    {R, S2} = do_read(I, State),
    {reply, R, S2};
handle_call({failure, I}, _From, State) ->
    {R, S2} = do_failure(I, State),
    {reply, R, S2};
handle_call({reset, I}, _From, State) ->
    {R, S2} = do_reset(I, State),
    {reply, R, S2};
handle_call({admin_unlock, Uid, I}, _From, State) ->
    {R, S2} = do_admin_unlock(Uid, I, State),
    {reply, R, S2};
handle_call({admit, Scope, Key}, _From, State) ->
    {R, S2} = do_admit(Scope, Key, State),
    {reply, R, S2};
handle_call({begin_attempt, I, Pid, DeadlineMs}, _From, State) ->
    {R, S2} = do_begin_attempt(I, Pid, DeadlineMs, State),
    {reply, R, S2};
handle_call({end_attempt, Ref, Outcome}, _From, State) ->
    {R, S2} = do_end_attempt(Ref, Outcome, State),
    {reply, R, S2};
handle_call({set_ip_rates, Rates}, _From, State) ->
    {reply, ok, login_attempt_state:apply_ip_rates(Rates, State)};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    {noreply, do_cleanup(State)};
handle_info(deadline_check, State) ->
    {noreply, do_deadline_check(State)};
handle_info({'DOWN', Mref, process, _Pid, _Reason}, State) ->
    {noreply, handle_attempt_down(Mref, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% check/snapshot 共用只读转换；逻辑过期视 0，不删行（cleanup 负责）。
-spec do_read(identity(), state()) -> {{ok, snapshot()} | {error, unavailable}, state()}.
do_read(I, State) ->
    case gate(State) of
        blocked ->
            {{error, unavailable}, State};
        ok ->
            case login_attempt_state:valid_identity(I) of
                false -> {{error, unavailable}, State};
                true -> {{ok, read_snapshot(I)}, State}
            end
    end.

%% 读逻辑快照：缺 Key 或已逻辑过期 → 当前规则下的零快照。
-spec read_snapshot(identity()) -> snapshot().
read_snapshot(I) ->
    case login_attempt_state:lookup_account(I) of
        {live, Count, Exp, RV, _Max} ->
            login_attempt_state:account_snap(Count, RV, Exp);
        _ExpiredOrNone ->
            login_attempt_state:account_snap(
                0, login_attempt_state:current_account_rule(), undefined
            )
    end.

%% failure：提交时刻所属窗口计数；建行容量满 → 原子 degraded + capacity_lost。
-spec do_failure(identity(), state()) ->
    {{ok, snapshot()} | {error, capacity_lost | unavailable}, state()}.
do_failure(I, State) ->
    case gate(State) of
        blocked ->
            {{error, unavailable}, State};
        ok ->
            case login_attempt_state:valid_identity(I) of
                false -> {{error, unavailable}, State};
                true -> failure_apply(I, State)
            end
    end.

-spec failure_apply(identity(), state()) ->
    {{ok, snapshot()} | {error, capacity_lost}, state()}.
failure_apply(I, State) ->
    case login_attempt_state:lookup_account(I) of
        {live, Count, Exp, RV, Max} ->
            NewCount = Count + 1,
            ets:insert(?ETS, {row_key(I), login_attempt_state:live_row(NewCount, Exp, RV, Max)}),
            {{ok, login_attempt_state:account_snap(NewCount, RV, Exp)}, State};
        {expired, _C, _E, _RV, _M} ->
            %% 过期行原地换新窗（条目数不变，不占新容量）
            Rule = login_attempt_state:current_account_rule(),
            Now = login_attempt_clock:now_ms(),
            ets:insert(?ETS, {row_key(I), login_attempt_state:new_account_row(Rule, Now)}),
            Exp = Now + element(2, Rule),
            {{ok, login_attempt_state:account_snap(1, Rule, Exp)}, State};
        none ->
            insert_new_account_row(I, State)
    end.

%% 无行建行：容量满 → 原子 degraded（不淘汰有效状态，不伪装记账）。
-spec insert_new_account_row(identity(), state()) ->
    {{ok, snapshot()} | {error, capacity_lost}, state()}.
insert_new_account_row(I, State) ->
    S1 = reclaim_if_full(
        maps:get(account_count, State),
        maps:get(account_max_entries, State),
        State
    ),
    Rule = login_attempt_state:current_account_rule(),
    Now = login_attempt_clock:now_ms(),
    case maps:get(account_count, S1) >= maps:get(account_max_entries, S1) of
        true ->
            ok = ?ERROR_LOG([login_attempt, account_domain_degraded, capacity_full]),
            {{error, capacity_lost}, S1#{degraded => true}};
        false ->
            ets:insert(?ETS, {row_key(I), login_attempt_state:new_account_row(Rule, Now)}),
            S2 = S1#{account_count => maps:get(account_count, S1) + 1},
            Exp = Now + element(2, Rule),
            {{ok, login_attempt_state:account_snap(1, Rule, Exp)}, S2}
    end.

%% reset：degraded 拒绝（不授权签发）；同 owner 串行删除。
-spec do_reset(identity(), state()) -> {ok | {error, unavailable}, state()}.
do_reset(I, State) ->
    case gate(State) of
        blocked ->
            {{error, unavailable}, State};
        ok ->
            case login_attempt_state:valid_identity(I) of
                false -> {{error, unavailable}, State};
                true -> {ok, delete_account_row(I, State)}
            end
    end.

%% admin_unlock：权限校验（is_admin 现逻辑）后串行清指定身份域。
-spec do_admin_unlock(integer(), identity(), state()) ->
    {{ok, unlocked} | {error, unauthorized | invalid_input | unavailable}, state()}.
do_admin_unlock(AdminUid, I, State) ->
    case gate(State) of
        blocked ->
            {{error, unavailable}, State};
        ok ->
            case login_attempt_state:valid_identity(I) andalso is_admin(AdminUid) of
                false ->
                    ok = ?WARN_LOG([login_attempt, admin_unlock_unauthorized, AdminUid]),
                    {{error, unauthorized}, State};
                true ->
                    S2 = delete_account_row(I, State),
                    ok = ?INFO_LOG([
                        login_attempt,
                        admin_unlock_success,
                        login_attempt_state:sanitize_id(I)
                    ]),
                    {{ok, unlocked}, S2}
            end
    end.

%% 删账户行并同步条目计数（存在才 -1）。
-spec delete_account_row(identity(), state()) -> state().
delete_account_row(I, State) ->
    case ets:take(?ETS, row_key(I)) of
        [] -> State;
        [_Row] -> State#{account_count => maps:get(account_count, State) - 1}
    end.

row_key(I) ->
    {limiter_account, I}.

%% admit：先判断并占请求额度；HTTP scope 统一换窗（含 pending 生效），
%% OIDC/HTTP 新 Key 容量满 → capacity（不降级）。
-spec do_admit(scope(), binary(), state()) ->
    {{ok, snapshot()} | {error, rate_limited | capacity | unavailable}, state()}.
do_admit(Scope, Key, State) ->
    Gates =
        [
            maps:get(ready, State),
            maps:is_key(Scope, maps:get(scopes, State)),
            byte_size(Key) > 0,
            byte_size(Key) =< maps:get(max_key_bytes, State)
        ],
    case Gates of
        [true, true, true, true] -> admit_apply(Scope, Key, State);
        %% 超长 Key 属资源边界：capacity（与条目容量同语义，不执行下游）
        [true, true, true, false] -> {{error, capacity}, State};
        %% 空 Key 属无效请求：unavailable
        _ -> {{error, unavailable}, State}
    end.

-spec admit_apply(scope(), binary(), state()) ->
    {{ok, snapshot()} | {error, rate_limited | capacity}, state()}.
admit_apply(Scope, Key, State0) ->
    {Meta, State1} = login_attempt_state:advance_scope(Scope, State0),
    Now = login_attempt_clock:now_ms(),
    RowKey = ?K_ADMIT(Scope, Key),
    case ets:lookup(?ETS, RowKey) of
        [{_, Row}] ->
            %% 行新鲜度：逻辑过期（now >= expires）即旧窗/旧 Key 窗，重置；
            %% 对 HTTP 等价于"行窗口 ≠ 当前窗"，对 OIDC 为 per_key 窗过期。
            case maps:get(expires_at_ms, Row) > Now of
                true -> admit_bump(RowKey, Row, State1);
                false -> admit_new_row(RowKey, Meta, Now, Scope, State1)
            end;
        _NotFound ->
            admit_new_row(RowKey, Meta, Now, Scope, State1)
    end.

%% 已有当前窗行：按**行内冻结规则**判断阈值与快照（scope 规则变更只影响
%% 新窗）；低于阈值才 +1，拒绝不加、不退款。
-spec admit_bump(term(), map(), state()) ->
    {{ok, snapshot()} | {error, rate_limited}, state()}.
admit_bump(RowKey, Row, State) ->
    RV = maps:get(rule_version, Row),
    {N, _P} = RV,
    #{count := C, expires_at_ms := Exp} = Row,
    case C >= N of
        true ->
            {{error, rate_limited}, State};
        false ->
            NewC = C + 1,
            ets:insert(?ETS, {RowKey, Row#{count => NewC}}),
            Snap = #{
                count => NewC,
                remaining => max(0, N - NewC),
                locked => NewC >= N,
                expires_at_ms => Exp,
                rule_version => RV
            },
            {{ok, Snap}, State}
    end.

%% 新 Key（或跨窗旧行重置）建行：每 scope 条目上限检查。HTTP 行窗口对齐
%% scope 当前边界（t0=0）；OIDC 行以首次 admit 时刻为 anchor（per_key）。
-spec admit_new_row(term(), map(), integer(), scope(), state()) ->
    {{ok, snapshot()} | {error, rate_limited | capacity}, state()}.
admit_new_row(RowKey, Meta, Now, Scope, State) ->
    {N, P} = maps:get(rule, Meta),
    Existing = ets:member(?ETS, RowKey),
    S1 =
        case Existing of
            true ->
                State;
            false ->
                reclaim_if_full(
                    maps:get(Scope, maps:get(admit_counts, State), 0),
                    maps:get(admit_max_entries, State),
                    State
                )
        end,
    Used = maps:get(Scope, maps:get(admit_counts, S1), 0),
    case not Existing andalso Used >= maps:get(admit_max_entries, S1) of
        true ->
            {{error, capacity}, S1};
        false ->
            WS =
                case maps:get(anchor_mode, Meta) of
                    scope -> maps:get(window_start_ms, Meta);
                    per_key -> Now
                end,
            Row = #{
                count => 1,
                window_start_ms => WS,
                expires_at_ms => WS + P,
                rule_version => {N, P}
            },
            ets:insert(?ETS, {RowKey, Row}),
            Added =
                case Existing of
                    true -> 0;
                    false -> 1
                end,
            Counts = (maps:get(admit_counts, S1))#{Scope => Used + Added},
            {{ok, login_attempt_state:admit_snap(1, Meta, WS + P)}, S1#{admit_counts => Counts}}
    end.

-spec do_begin_attempt(identity(), pid(), pos_integer() | default, state()) ->
    {{ok, attempt_ref()} | {error, unavailable}, state()}.
do_begin_attempt(I, Pid, DeadlineMs, State) ->
    case gate(State) of
        blocked ->
            {{error, unavailable}, State};
        ok ->
            case
                login_attempt_state:valid_identity(I) andalso
                    ets:info(?DEADLINES, size) < maps:get(account_max_entries, State)
            of
                false ->
                    {{error, unavailable}, State};
                true ->
                    Ref = erlang:make_ref(),
                    Dl =
                        case DeadlineMs of
                            default -> maps:get(auth_deadline_ms, State);
                            _ when is_integer(DeadlineMs), DeadlineMs > 0 -> DeadlineMs;
                            _ -> maps:get(auth_deadline_ms, State)
                        end,
                    Mref = erlang:monitor(process, Pid),
                    Now = login_attempt_clock:now_ms(),
                    ets:insert(?ETS, {?K_ATTEMPT(Ref), {Mref, Pid, I, Now + Dl}}),
                    ets:insert(?DEADLINES, {{Now + Dl, Ref}, true}),
                    {{ok, Ref}, State}
            end
    end.

%% 终态记账与生命周期释放在同一次 owner 调用内完成。
-spec do_end_attempt(attempt_ref(), attempt_outcome(), state()) ->
    {ok | {error, unavailable | capacity_lost}, state()}.
do_end_attempt(Ref, Outcome, State) ->
    case ets:lookup(?ETS, ?K_ATTEMPT(Ref)) of
        [] ->
            {{error, unavailable}, State};
        [{_, {_Mref, _Pid, I, _Dl}}] ->
            remove_attempt(Ref),
            apply_outcome(Outcome, I, State)
    end.

-spec apply_outcome(attempt_outcome(), identity(), state()) ->
    {ok | {error, unavailable | capacity_lost}, state()}.
apply_outcome(no_count, _I, State) ->
    case gate(State) of
        ok -> {ok, State};
        blocked -> {{error, unavailable}, State}
    end;
apply_outcome(reset, I, State) ->
    do_reset(I, State);
apply_outcome(failure, I, State) ->
    case do_failure(I, State) of
        {{ok, _}, S2} -> {ok, S2};
        {Error, S2} -> {Error, S2}
    end.

remove_attempt(Ref) ->
    case ets:take(?ETS, ?K_ATTEMPT(Ref)) of
        [{_, {Mref, _Pid, _I, Dl}}] ->
            erlang:demonitor(Mref, [flush]),
            ets:delete(?DEADLINES, {Dl, Ref});
        [] ->
            ok
    end.

%% DOWN：按 monitor ref 核对本 run 的 attempt；终态未提交的进程死亡
%% （含 normal 但漏交终态）→ 账户域 degraded。旧消息（已 end_attempt）
%% 查无此行，不误伤新请求。
-spec handle_attempt_down(reference(), state()) -> state().
handle_attempt_down(Mref, State) ->
    case find_attempt_by_mref(Mref) of
        none ->
            State;
        {Ref, _Entry} ->
            remove_attempt(Ref),
            login_attempt_state:mark_degraded(State)
    end.

%% 周期性按最早 deadline 处理最多 cleanup_batch 个认证，超时置 degraded。
-spec do_deadline_check(state()) -> state().
do_deadline_check(State) ->
    Now = login_attempt_clock:now_ms(),
    Batch = maps:get(cleanup_batch, State),
    case expire_attempts(Now, Batch, false) of
        false -> State;
        true -> login_attempt_state:mark_degraded(State)
    end.

%% 按 deadline 排序，仅处理已到期认证，不受账户/IP 大表遮挡。
expire_attempts(_Now, 0, Expired) ->
    Expired;
expire_attempts(Now, Budget, Expired) ->
    case ets:first(?DEADLINES) of
        {Dl, Ref} when Dl =< Now ->
            remove_attempt(Ref),
            expire_attempts(Now, Budget - 1, true);
        _ ->
            Expired
    end.

%% 分批游标扫描。ets:next/2（OTP 28+）对已消失的键 badarg，故：
%% - 扫描期间持 safe_fixtable 且 owner 单写者不删行（收集后统一删）；
%% - 跨轮游标是尚未处理的键，存在则从它继续，
%%   断点消失则回头重扫（删除前复核保证重扫不误删任何有效行）。
-spec do_cleanup(state()) -> state().
do_cleanup(State) ->
    ets:safe_fixtable(?ETS, true),
    try
        Budget = maps:get(cleanup_batch, State),
        Start = restart_point(maps:get(cleanup_cursor, State)),
        {Expired, NextCursor} = scan_expired(Start, Budget, []),
        %% 删除前复核当前 expires_at_ms（合同字面：迟到批次不删新行；
        %% owner 串行下 scan→delete 原子，此复核同时兜底跨轮次的更新行）。
        S2 = lists:foldl(fun delete_if_still_expired/2, State, Expired),
        S2#{cleanup_cursor => NextCursor}
    after
        ets:safe_fixtable(?ETS, false)
    end.

-spec restart_point(term()) -> term().
restart_point('$end_of_table') ->
    ets:first(?ETS);
restart_point(Cursor) ->
    case ets:member(?ETS, Cursor) of
        true -> Cursor;
        false -> ets:first(?ETS)
    end.

%% 满额先做一批过期回收；不扫描全表，不淘汰有效锁。
reclaim_if_full(Used, Max, State) when Used >= Max -> do_cleanup(State);
reclaim_if_full(_Used, _Max, State) -> State.

-spec scan_expired(term(), non_neg_integer(), [term()]) -> {[term()], term()}.
scan_expired('$end_of_table', _Budget, Acc) ->
    {lists:reverse(Acc), '$end_of_table'};
scan_expired(Cursor, 0, Acc) ->
    {lists:reverse(Acc), Cursor};
scan_expired(Cursor, Budget, Acc) ->
    Next = ets:next(?ETS, Cursor),
    Now = login_attempt_clock:now_ms(),
    Acc2 =
        case ets:lookup(?ETS, Cursor) of
            [{K, #{expires_at_ms := Exp}}] when
                element(1, K) =/= limiter_attempt, Exp =< Now
            ->
                [K | Acc];
            _ ->
                Acc
        end,
    scan_expired(Next, Budget - 1, Acc2).

-spec delete_if_still_expired(term(), state()) -> state().
delete_if_still_expired(Key, State) ->
    case ets:lookup(?ETS, Key) of
        [{_, Row}] ->
            case maps:get(expires_at_ms, Row) =< login_attempt_clock:now_ms() of
                true ->
                    ets:delete(?ETS, Key),
                    adjust_capacity_count(Key, State);
                false ->
                    State
            end;
        _ ->
            State
    end.

%% 清理删除后同步各域条目计数。
-spec adjust_capacity_count(term(), state()) -> state().
adjust_capacity_count({limiter_account, _}, State) ->
    State#{account_count => maps:get(account_count, State) - 1};
adjust_capacity_count({limiter_admit, Scope, _}, State) ->
    Counts = maps:get(admit_counts, State),
    Counts2 = maps:update_with(Scope, fun(C) -> max(0, C - 1) end, 0, Counts),
    State#{admit_counts => Counts2};
adjust_capacity_count(_, State) ->
    State.

%% owner 不存在/超时/异常统一 unavailable，不重试未知结果。
-spec safe_call(term()) -> term().
safe_call(Msg) ->
    try gen_server:call(?MODULE, Msg, ?CALL_TIMEOUT) of
        Reply -> Reply
    catch
        _:_ -> {error, unavailable}
    end.

%% 账户域门（纯判断在 state 模块）。
gate(State) ->
    login_attempt_state:gate_account(State).

find_attempt_by_mref(Mref) ->
    %% ets:match/2 返回 [[V1, V2], ...] 嵌套列表
    Rows = ets:match(?ETS, {{limiter_attempt, '$1'}, '$2'}),
    find_mref(Rows, Mref).

find_mref([], _Mref) ->
    none;
find_mref([[Ref, Entry] | Rest], Mref) ->
    case element(1, Entry) =:= Mref of
        true -> {Ref, Entry};
        false -> find_mref(Rest, Mref)
    end.

parse_first_ip(<<>>) ->
    <<"0.0.0.0">>;
parse_first_ip(XForwardedFor) ->
    case binary:split(XForwardedFor, <<",">>) of
        [FirstIp | _] -> string:trim(FirstIp);
        _ -> <<"0.0.0.0">>
    end.
