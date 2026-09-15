%%% @doc 企业 Workspace 保留策略、hold 与 purge 资格的领域纯函数。
%%%
%%% 依据：plan v4.1 EB-D12、§2.1 #13/#16/#17、§4.3、§5.4。
%%%
%%% 纯净性（铁律 4）：**时间是参数**。本模块不调用 `os:timestamp/0`、
%%% `erlang:timestamp/0`、`calendar:universal_time/0`、`rand:*`；所有「现在」
%%% 都由调用方注入（clock port），因此结果对同一输入完全可复现。
%%%
%%% 冻结的不变量：
%%%   * 消息接受时把 policy 版本与 `retain_until` 固化进消息，旧消息不随未来
%%%     策略缩短；策略只能后移（延长）。
%%%   * hold 覆盖 workspace / conversation / message 三级 scope，release 后
%%%     立即失效。
%%%   * active hold 优先于 `retain_until`；`retain_until` 未到一律不得清理；
%%%     缺少 `retain_until` 一律不清理（宁可多保留）。
%%%
%%% 边界冻结：`Now >= retain_until` 判为「已到期」，即 `Now == retain_until`
%%% 时允许 purge（无 active hold 时）。
-module(eb_retention).

-export([
    policy_snapshot/3,
    shorten_forbidden/2,
    hold_covers/2,
    purge_eligible/3,
    policy_extension_only/2
]).

-define(SECONDS_PER_DAY, 86400).

-type policy() :: map().
-type snapshot() :: map().
-type hold() :: map().
-type scope_target() :: map().
-type eligibility() :: eligible | {ineligible, term()}.

-export_type([policy/0, snapshot/0, hold/0, eligibility/0]).

%% ===================================================================
%% policy snapshot
%% ===================================================================

%% @doc 把 policy 版本固化为消息上的 retention snapshot。
%%
%% `retain_until = MessageAcceptedAt + retention_days * 86400`。
%% `Clock` 是注入的「现在」，只用于拒绝未来的 `MessageAcceptedAt`；本函数
%% 不读取任何系统时间。
%%
%% 失败：
%%   {error, {invalid_policy, policy_id | policy_version | retention_days}}
%%   {error, {accepted_at_in_future, MessageAcceptedAt, Clock}}
-spec policy_snapshot(policy(), integer(), integer()) -> {ok, snapshot()} | {error, term()}.
policy_snapshot(Policy, MessageAcceptedAt, Clock) when is_map(Policy) ->
    case validate_policy(Policy) of
        {error, _} = Err ->
            Err;
        {ok, PolicyId, PolicyVersion, RetentionDays} ->
            snapshot_with_clock(PolicyId, PolicyVersion, RetentionDays, MessageAcceptedAt, Clock)
    end;
policy_snapshot(_NotAMap, _MessageAcceptedAt, _Clock) ->
    {error, {invalid_policy, undefined}}.

snapshot_with_clock(PolicyId, PolicyVersion, RetentionDays, MessageAcceptedAt, Clock) ->
    case {is_integer(MessageAcceptedAt), is_integer(Clock)} of
        {false, _} ->
            {error, invalid_accepted_at};
        {true, false} ->
            {error, invalid_clock};
        {true, true} when MessageAcceptedAt > Clock ->
            {error, {accepted_at_in_future, MessageAcceptedAt, Clock}};
        {true, true} ->
            {ok, #{
                policy_id => PolicyId,
                policy_version => PolicyVersion,
                retention_days => RetentionDays,
                retain_until => MessageAcceptedAt + RetentionDays * ?SECONDS_PER_DAY
            }}
    end.

validate_policy(Policy) ->
    case maps:get(policy_id, Policy, undefined) of
        PolicyId when is_integer(PolicyId) ->
            validate_policy_version(Policy);
        _MissingOrInvalid ->
            {error, {invalid_policy, policy_id}}
    end.

validate_policy_version(Policy) ->
    case maps:get(policy_version, Policy, undefined) of
        Version when is_integer(Version), Version > 0 ->
            validate_retention_days(Policy);
        _MissingOrInvalid ->
            {error, {invalid_policy, policy_version}}
    end.

validate_retention_days(Policy) ->
    case maps:get(retention_days, Policy, undefined) of
        Days when is_integer(Days), Days >= 0 ->
            {
                ok,
                maps:get(policy_id, Policy),
                maps:get(policy_version, Policy),
                Days
            };
        _MissingOrInvalid ->
            {error, {invalid_policy, retention_days}}
    end.

%% ===================================================================
%% retain_until 只允许后移
%% ===================================================================

%% @doc 已接受消息的 `retain_until` 只允许后移，任何前移都是硬失败。
-spec shorten_forbidden(integer(), integer()) -> ok | {error, term()}.
shorten_forbidden(OldRetainUntil, NewRetainUntil) when
    is_integer(OldRetainUntil), is_integer(NewRetainUntil)
->
    case NewRetainUntil >= OldRetainUntil of
        true -> ok;
        false -> {error, {retention_shorten_forbidden, OldRetainUntil, NewRetainUntil}}
    end;
shorten_forbidden(OldRetainUntil, NewRetainUntil) ->
    {error, {invalid_retain_until, OldRetainUntil, NewRetainUntil}}.

%% ===================================================================
%% hold 覆盖判定
%% ===================================================================

%% @doc hold 是否覆盖给定资源。
%%
%% `released_at` 非 `undefined` 即视为已释放，一律不覆盖。
%% scope 语义：
%%   workspace    → 该 Workspace 内全部资源
%%   conversation → 该会话内全部消息
%%   message      → 仅该条消息
%% 未知 scope 一律不覆盖（fail-closed：不因数据脏而误判「已覆盖」或「未覆盖」）。
-spec hold_covers(hold(), scope_target()) -> boolean().
hold_covers(Hold, Target) when is_map(Hold), is_map(Target) ->
    case maps:get(released_at, Hold, undefined) of
        undefined -> scope_covers(maps:get(scope, Hold, undefined), Hold, Target);
        _ReleasedAt -> false
    end;
hold_covers(_Hold, _Target) ->
    false.

scope_covers(workspace, Hold, Target) ->
    same_org(Hold, Target) andalso
        maps:get(workspace_id, Hold, undefined) =:= maps:get(workspace_id, Target, undefined);
scope_covers(conversation, Hold, Target) ->
    same_org(Hold, Target) andalso
        maps:get(workspace_id, Hold, undefined) =:= maps:get(workspace_id, Target, undefined) andalso
        maps:get(conversation_id, Hold, undefined) =:= maps:get(conversation_id, Target, undefined);
scope_covers(message, Hold, Target) ->
    same_org(Hold, Target) andalso
        maps:get(conversation_id, Hold, undefined) =:= maps:get(conversation_id, Target, undefined) andalso
        maps:get(message_id, Hold, undefined) =:= maps:get(message_id, Target, undefined);
scope_covers(_UnknownScope, _Hold, _Target) ->
    false.

same_org(Hold, Target) ->
    maps:get(organization_id, Hold, undefined) =:= maps:get(organization_id, Target, undefined).

%% ===================================================================
%% purge eligibility
%% ===================================================================

%% @doc 判定一条企业消息当前是否可被 bounded purge 物理清理。
%%
%% 判定顺序冻结：
%%   1. `retain_until` 缺失/非整数 → `{ineligible, missing_retain_until}`；
%%   2. `Now < retain_until` → `{ineligible, retain_not_reached}`
%%      （`Now == retain_until` 视为已到期，见模块头「边界冻结」）；
%%   3. 任一 active hold 覆盖该消息 → `{ineligible, active_hold}`；
%%   4. 否则 `eligible`。
%%
%% 客户端隐藏 / 未来撤回只改可见性，不改变本裁决；离职与 suspend 也不得使
%% 本函数在 `retain_until` 之前返回 `eligible`。
-spec purge_eligible(map(), [hold()], integer()) -> eligibility().
purge_eligible(Message, Holds, Now) when is_map(Message), is_list(Holds), is_integer(Now) ->
    case maps:get(retain_until, Message, undefined) of
        RetainUntil when is_integer(RetainUntil) ->
            eligibility_at(Message, Holds, Now, RetainUntil);
        _MissingOrInvalid ->
            {ineligible, missing_retain_until}
    end;
purge_eligible(_Message, _Holds, _Now) ->
    {ineligible, invalid_message}.

eligibility_at(_Message, _Holds, Now, RetainUntil) when Now < RetainUntil ->
    {ineligible, retain_not_reached};
eligibility_at(Message, Holds, _Now, _RetainUntil) ->
    Target = scope_target(Message),
    case lists:any(fun(Hold) -> hold_covers(Hold, Target) end, Holds) of
        true -> {ineligible, active_hold};
        false -> eligible
    end.

scope_target(Message) ->
    #{
        organization_id => maps:get(organization_id, Message, undefined),
        workspace_id => maps:get(workspace_id, Message, undefined),
        conversation_id => maps:get(conversation_id, Message, undefined),
        message_id => maps:get(message_id, Message, undefined)
    }.

%% ===================================================================
%% policy 版本只允许延长
%% ===================================================================

%% @doc 新策略版本的 `retention_days` 不得小于已有最大版本。
%%
%% `Existing` 可以是整数列表，也可以是 policy map 列表。
-spec policy_extension_only(term(), [term()]) -> ok | {error, term()}.
policy_extension_only(NewRetentionDays, Existing) when
    is_integer(NewRetentionDays), NewRetentionDays >= 0
->
    case existing_days(Existing) of
        {error, _} = Err ->
            Err;
        {ok, []} ->
            ok;
        {ok, Days} ->
            MaxExisting = lists:max(Days),
            case NewRetentionDays >= MaxExisting of
                true -> ok;
                false -> {error, {retention_shorten_forbidden, MaxExisting, NewRetentionDays}}
            end
    end;
policy_extension_only(NewRetentionDays, _Existing) ->
    {error, {invalid_retention_days, NewRetentionDays}}.

existing_days(Existing) when is_list(Existing) ->
    collect_days(Existing, []);
existing_days(_NotAList) ->
    {error, invalid_existing_policies}.

collect_days([], Acc) ->
    {ok, lists:reverse(Acc)};
collect_days([Item | Rest], Acc) ->
    case retention_days_of(Item) of
        {ok, Days} -> collect_days(Rest, [Days | Acc]);
        error -> {error, {invalid_existing_policy, Item}}
    end.

retention_days_of(Days) when is_integer(Days), Days >= 0 ->
    {ok, Days};
retention_days_of(#{retention_days := Days}) when is_integer(Days), Days >= 0 ->
    {ok, Days};
retention_days_of(_Other) ->
    error.
