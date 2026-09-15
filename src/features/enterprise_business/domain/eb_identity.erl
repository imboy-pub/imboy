%%% @doc 企业业务身份（business identity）与 assignment 的领域纯函数。
%%%
%%% 依据：plan v4.1 EB-D02 / EB-D03 / EB-D11、§4.1 基数不变量、§2.1 #1/#4。
%%%
%%% 纯净性（铁律 4）：本模块无 I/O、无进程、无隐式时间/随机源；时间、ID、actor
%%% 一律由调用方作为参数传入。所有函数可零 mock 单测。
%%%
%%% 冻结的不变量：
%%%   * `function_key` V1 仅 `sales | customer_service`（EB-D11；出现第三个真实
%%%     职能再走新决策扩容，不得用 function 字符串替代 permission）；
%%%   * 同一 identity 同时最多一个 active assignment；
%%%   * 同一 `(organization_id, user_id, function_key)` 同时最多一个 active；
%%%   * active assignment 必须有 `user_id`、必须 `ended_at = undefined`；
%%%   * owner 只能是 `organization_id`，`user_id` 永远不得充当 owner。
-module(eb_identity).

-export([
    function_keys/0,
    valid_transition/2,
    assignment_invariants/1,
    new_identity/1,
    owner_assignee_actor/1
]).

-type function_key() :: binary().
-type identity_status() :: active | retired.
-type assignment_status() :: active | ended.
-type identity() :: map().
-type assignment() :: map().
-type owner_assignee_actor() :: #{
    owner := integer(),
    assignee := integer(),
    actor := integer() | undefined
}.

-export_type([function_key/0, identity_status/0, assignment_status/0]).

%% ===================================================================
%% 职能值域（V1 冻结）
%% ===================================================================

%% @doc V1 冻结的 `function_key` 值域。返回值必须逐字等于本列表。
-spec function_keys() -> [function_key()].
function_keys() ->
    [<<"sales">>, <<"customer_service">>].

%% ===================================================================
%% 状态迁移
%% ===================================================================

%% @doc 判定 identity / assignment 的状态迁移是否合法。
%%
%% identity（active | retired）：
%%   active  -> retired                        ok
%%   active  -> active                         {error, duplicate_occupation}
%%   retired -> active                         {error, {invalid_transition, retired, active}}
%%   retired -> retired                        {error, {invalid_transition, retired, retired}}
%%
%% assignment（active | ended）：
%%   active         -> ended                   ok
%%   {ended,reopen} -> active                   ok（必须显式 reopen 意图，不得自由复活）
%%   active         -> active                  {error, duplicate_occupation}
%%   ended          -> active                  {error, reopen_required}
%%   ended          -> ended                   {error, {invalid_transition, ended, ended}}
%%   {ended,reopen} -> ended                   {error, {invalid_transition, {ended, reopen}, ended}}
-spec valid_transition(identity | assignment, term()) -> ok | {error, term()}.
valid_transition(identity, {active, retired}) ->
    ok;
valid_transition(identity, {active, active}) ->
    {error, duplicate_occupation};
valid_transition(identity, {retired, active}) ->
    {error, {invalid_transition, retired, active}};
valid_transition(identity, {retired, retired}) ->
    {error, {invalid_transition, retired, retired}};
valid_transition(assignment, {active, ended}) ->
    ok;
valid_transition(assignment, {{ended, reopen}, active}) ->
    ok;
valid_transition(assignment, {active, active}) ->
    {error, duplicate_occupation};
valid_transition(assignment, {ended, active}) ->
    {error, reopen_required};
valid_transition(assignment, {ended, ended}) ->
    {error, {invalid_transition, ended, ended}};
valid_transition(assignment, {{ended, reopen}, ended}) ->
    {error, {invalid_transition, {ended, reopen}, ended}};
valid_transition(Kind, _Transition) ->
    {error, {unknown_transition_kind, Kind}}.

%% ===================================================================
%% assignment 基数与时间不变量
%% ===================================================================

%% @doc 断言一组 assignment 满足 DB 层同款的基数/时间约束。
%%
%% 返回 `ok` 或四类可区分违约之一：
%%   {error, {active_missing_user, {OrgId, IdentityId}}}
%%   {error, {active_has_ended_at, {OrgId, IdentityId}}}
%%   {error, {ended_missing_ended_at, {OrgId, IdentityId}}}
%%   {error, {multiple_active_identity, {OrgId, IdentityId}}}
%%   {error, {multiple_active_user_function, {OrgId, UserId, FunctionKey}}}
%%
%% 判定顺序固定（user → ended_at → identity 基数 → (org,user,function) 基数），
%% 使同一输入的失败原因唯一可复现。
-spec assignment_invariants([assignment()]) -> ok | {error, term()}.
assignment_invariants(Assignments) when is_list(Assignments) ->
    Active = [A || A <- Assignments, is_active(A)],
    Ended = [A || A <- Assignments, is_ended(A)],
    case first_active_missing_user(Active) of
        ok ->
            case first_active_with_ended_at(Active) of
                ok ->
                    case first_ended_without_ended_at(Ended) of
                        ok ->
                            case first_duplicate_identity(Active) of
                                ok -> first_duplicate_user_function(Active);
                                {error, _} = Err -> Err
                            end;
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
assignment_invariants(_NotAList) ->
    {error, invalid_assignments}.

is_active(A) -> maps:get(status, A, undefined) =:= active.

is_ended(A) -> maps:get(status, A, undefined) =:= ended.

first_active_missing_user([]) ->
    ok;
first_active_missing_user([A | Rest]) ->
    case maps:get(user_id, A, undefined) of
        undefined -> {error, {active_missing_user, identity_key(A)}};
        _UserId -> first_active_missing_user(Rest)
    end.

first_active_with_ended_at([]) ->
    ok;
first_active_with_ended_at([A | Rest]) ->
    case maps:get(ended_at, A, undefined) of
        undefined -> first_active_with_ended_at(Rest);
        _EndedAt -> {error, {active_has_ended_at, identity_key(A)}}
    end.

first_ended_without_ended_at([]) ->
    ok;
first_ended_without_ended_at([A | Rest]) ->
    case maps:get(ended_at, A, undefined) of
        undefined -> {error, {ended_missing_ended_at, identity_key(A)}};
        _EndedAt -> first_ended_without_ended_at(Rest)
    end.

first_duplicate_identity(Active) ->
    scan_keys(Active, #{}, fun identity_key/1, fun(K) -> {error, {multiple_active_identity, K}} end).

first_duplicate_user_function(Active) ->
    scan_keys(Active, #{}, fun user_function_key/1, fun(K) ->
        {error, {multiple_active_user_function, K}}
    end).

scan_keys([], _Seen, _KeyFun, _ErrFun) ->
    ok;
scan_keys([Item | Rest], Seen, KeyFun, ErrFun) ->
    Key = KeyFun(Item),
    case maps:is_key(Key, Seen) of
        true -> ErrFun(Key);
        false -> scan_keys(Rest, Seen#{Key => true}, KeyFun, ErrFun)
    end.

identity_key(A) ->
    {maps:get(organization_id, A, undefined), maps:get(business_identity_id, A, undefined)}.

user_function_key(A) ->
    {
        maps:get(organization_id, A, undefined),
        maps:get(user_id, A, undefined),
        maps:get(function_key, A, undefined)
    }.

%% ===================================================================
%% 构造 + 校验
%% ===================================================================

%% @doc 校验并归一化一个新 business identity。
%%
%% 校验顺序固定：organization_id 必须是整数 → function_key 必须在值域 →
%% display_name 非空。成功时补齐 `status = active`、`version = 1`。
-spec new_identity(map()) -> {ok, identity()} | {error, term()}.
new_identity(Params) when is_map(Params) ->
    OrganizationId = maps:get(organization_id, Params, undefined),
    FunctionKey = maps:get(function_key, Params, undefined),
    DisplayName = maps:get(display_name, Params, undefined),
    case is_integer(OrganizationId) of
        false ->
            {error, {invalid_organization_id, OrganizationId}};
        true ->
            case lists:member(FunctionKey, function_keys()) of
                false ->
                    {error, {unknown_function_key, FunctionKey}};
                true ->
                    case normalize_display_name(DisplayName) of
                        {ok, Name} ->
                            {ok, #{
                                organization_id => OrganizationId,
                                function_key => FunctionKey,
                                display_name => Name,
                                status => active,
                                version => 1
                            }};
                        {error, _} = Err ->
                            Err
                    end
            end
    end;
new_identity(_NotAMap) ->
    {error, invalid_params}.

normalize_display_name(Value) when is_binary(Value) ->
    case string:trim(Value) of
        <<>> -> {error, empty_display_name};
        Trimmed -> {ok, Trimmed}
    end;
normalize_display_name(Value) when is_list(Value) ->
    normalize_display_name(unicode:characters_to_binary(Value));
normalize_display_name(Value) ->
    {error, {invalid_display_name, Value}}.

%% ===================================================================
%% Owner / assignee / actor 三分
%% ===================================================================

%% @doc 从入参 map 抽出三分三元组。
%%
%% owner 只能来自 `organization_id`、assignee 只能来自 `business_identity_id`、
%% actor 只能来自 `actor_user_id`。任何把自然人 user 当作 owner 的输入
%% （缺 organization_id 却带 user_id / owner_id / owner_user_id / creator_user_id
%% / uploader_user_id，或显式给 owner_user_id）一律返回
%% `{error, user_cannot_be_owner}`。
%%
%% `actor_user_id` 允许为 `undefined`（客户入站消息没有 actor）。
-spec owner_assignee_actor(map()) -> {ok, owner_assignee_actor()} | {error, term()}.
owner_assignee_actor(Input) when is_map(Input) ->
    case maps:get(owner_user_id, Input, undefined) of
        undefined ->
            OrganizationId = maps:get(organization_id, Input, undefined),
            case user_used_as_owner(Input, OrganizationId) of
                true ->
                    {error, user_cannot_be_owner};
                false ->
                    resolve_triple(Input, OrganizationId)
            end;
        _OwnerUserId ->
            {error, user_cannot_be_owner}
    end;
owner_assignee_actor(_NotAMap) ->
    {error, invalid_params}.

resolve_triple(Input, OrganizationId) ->
    IdentityId = maps:get(business_identity_id, Input, undefined),
    Actor = maps:get(actor_user_id, Input, undefined),
    case is_integer(OrganizationId) of
        false ->
            {error, {missing_owner, organization_id}};
        true ->
            case is_integer(IdentityId) of
                false ->
                    {error, {missing_assignee, business_identity_id}};
                true ->
                    case Actor =:= undefined orelse is_integer(Actor) of
                        true ->
                            {ok, #{
                                owner => OrganizationId,
                                assignee => IdentityId,
                                actor => Actor
                            }};
                        false ->
                            {error, {invalid_actor, Actor}}
                    end
            end
    end.

%% 有合法 organization_id 时，user 字段只是 actor，不构成违例。
user_used_as_owner(_Input, OrganizationId) when is_integer(OrganizationId) ->
    false;
user_used_as_owner(Input, _OrganizationId) ->
    has_user_owner_key(Input).

has_user_owner_key(Input) ->
    lists:any(
        fun(Key) ->
            case maps:get(Key, Input, undefined) of
                undefined -> false;
                _Value -> true
            end
        end,
        [user_id, owner_id, owner_user_id, creator_user_id, uploader_user_id]
    ).
