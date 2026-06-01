%%% @doc 群组聚合根 / Group Aggregate Root
%%%
%%% DDD 充血改造 Phase 2 / T2.1：把「成员上限 / 成员增减 / 群主转让 /
%%% 群解散」四类业务不变量内聚到聚合根内部，由纯函数守护，杜绝散落在
%%% group_logic / group_member_logic 的 guard 与外部直接赋值。
%%%
%%% Functional-core aggregate guarding group invariants:
%%%   - member_count stays within [0, MAX_MEMBERS] (上限 500);
%%%   - owner transfer is permitted only when the actor IS the owner;
%%%   - a dissolved group is terminal: membership/transfer commands rejected;
%%%   - dissolve is idempotent (re-dissolving emits no event).
%%% Operations return {ok, NewState, [event()]} | {error, atom()}; the
%%% imperative shell (logic) persists state and publishes events after commit.
%%% Zero side effects -> eunit without mocks.
%%%
%%% group_role_vo 接入：role_of/2 以角色值对象表达成员角色，转让不变量
%%% 经由角色 VO（actor 角色须为 ROLE_OWNER）判定，权威源 group_role.hrl。
%%%
%%% 渐进策略（NOT Building）：本 task 仅新增纯领域层，不改既有 logic；
%%% DS load/save_aggregate 接线（T2.2）与 logic 退化外壳（T2.3）留后续。
-module(group_agg).

-include("group_role.hrl").

-export([rehydrate/1]).
-export([add_member/2, remove_member/2, transfer_owner/2, dissolve/1]).
-export([id/1, owner/1, member_count/1, status/1, role_of/2]).

-export_type([t/0, event/0]).

%% 群组聚合状态：
%%   id           群组 id（binary，可序列化跨进程派发）
%%   owner        群主 uid（binary）
%%   member_count 成员数，不变量 0 =< N =< MAX_MEMBERS
%%   type         群类型（透传，业务语义不在本聚合）
%%   status       active | dissolved（dissolved 为终态）
-record(group, {
    id :: binary(),
    owner :: binary(),
    member_count = 0 :: non_neg_integer(),
    type :: integer() | undefined,
    status = active :: active | dissolved
}).
-opaque t() :: #group{}.

%% 领域事件：id/uid 以 binary 透传（可序列化、跨进程派发）。
-type event() ::
    {member_added, GroupId :: binary(), UserId :: binary()}
    | {member_removed, GroupId :: binary(), UserId :: binary()}
    | {owner_transferred, GroupId :: binary(), From :: binary(), To :: binary()}
    | {group_dissolved, GroupId :: binary()}.

-define(MAX_MEMBERS, 500).

%% ===================================================================
%% 重建
%% ===================================================================

%% @doc 从持久化裸 map 重建聚合（DS 层调用），规整成员数维持下限不变量。
-spec rehydrate(map()) -> t().
rehydrate(M) when is_map(M) ->
    #group{
        id = maps:get(<<"group_id">>, M),
        owner = maps:get(<<"owner">>, M),
        member_count = clamp_count(maps:get(<<"member_count">>, M, 0)),
        type = maps:get(<<"type">>, M, undefined),
        status = status_of(maps:get(<<"status">>, M, active))
    }.

%% ===================================================================
%% 成员上限不变量
%% ===================================================================

%% @doc 新增成员：已解散拒绝；触及上限（500）拒绝；否则 +1 并产出事件。
-spec add_member(t(), UserId :: binary()) ->
    {ok, t(), [event()]} | {error, group_dissolved | member_limit_reached}.
add_member(#group{status = dissolved}, _) ->
    {error, group_dissolved};
add_member(#group{member_count = N}, _) when N >= ?MAX_MEMBERS ->
    {error, member_limit_reached};
add_member(G = #group{id = Gid, member_count = N}, Uid) ->
    {ok, G#group{member_count = N + 1}, [{member_added, Gid, Uid}]}.

%% @doc 移除成员：已解散拒绝；空群（count=0）幂等无事件（守下限）；
%% 否则 -1 并产出事件。
-spec remove_member(t(), UserId :: binary()) ->
    {ok, t(), [event()]} | {error, group_dissolved}.
remove_member(#group{status = dissolved}, _) ->
    {error, group_dissolved};
remove_member(G = #group{member_count = 0}, _) ->
    {ok, G, []};
remove_member(G = #group{id = Gid, member_count = N}, Uid) ->
    {ok, G#group{member_count = N - 1}, [{member_removed, Gid, Uid}]}.

%% ===================================================================
%% 转让不变量（仅 owner，group_role_vo 接入）
%% ===================================================================

%% @doc 转让群主：已解散拒绝；发起人非群主（角色 VO 判定 ≠ ROLE_OWNER）
%% 拒绝 not_owner；否则 owner 改写并产出 owner_transferred 事件。
-spec transfer_owner(t(), {Actor :: binary(), NewOwner :: binary()}) ->
    {ok, t(), [event()]} | {error, group_dissolved | not_owner}.
transfer_owner(#group{status = dissolved}, _) ->
    {error, group_dissolved};
transfer_owner(G = #group{id = Gid, owner = Owner}, {Actor, NewOwner}) ->
    case group_role_vo:value(role_of(G, Actor)) =:= ?ROLE_OWNER of
        true ->
            {ok, G#group{owner = NewOwner}, [{owner_transferred, Gid, Owner, NewOwner}]};
        false ->
            {error, not_owner}
    end.

%% ===================================================================
%% 解散不变量（终态幂等）
%% ===================================================================

%% @doc 解散群：已解散则幂等无事件；否则置 dissolved 并产出事件。
-spec dissolve(t()) -> {ok, t(), [event()]}.
dissolve(G = #group{status = dissolved}) ->
    {ok, G, []};
dissolve(G = #group{id = Gid}) ->
    {ok, G#group{status = dissolved}, [{group_dissolved, Gid}]}.

%% ===================================================================
%% 访问器 / 角色 VO 接入
%% ===================================================================

-spec id(t()) -> binary().
id(#group{id = Id}) -> Id.

-spec owner(t()) -> binary().
owner(#group{owner = Owner}) -> Owner.

-spec member_count(t()) -> non_neg_integer().
member_count(#group{member_count = N}) -> N.

-spec status(t()) -> active | dissolved.
status(#group{status = S}) -> S.

%% @doc 成员角色值对象：群主返回 ROLE_OWNER VO，其余 uid 返回 ROLE_MEMBER VO。
%% 聚合只持有 owner 身份，细粒度角色由 group_member 表权威；此处守转让不变量足矣。
-spec role_of(t(), Uid :: binary()) -> group_role_vo:t().
role_of(#group{owner = Owner}, Owner) ->
    {ok, Vo} = group_role_vo:new(?ROLE_OWNER),
    Vo;
role_of(#group{}, _Uid) ->
    {ok, Vo} = group_role_vo:new(?ROLE_MEMBER),
    Vo.

%% ===================================================================
%% 内部：持久化规整（防御脏数据，维持不变量）
%% ===================================================================

-spec clamp_count(term()) -> non_neg_integer().
clamp_count(N) when is_integer(N), N >= 0 -> N;
clamp_count(_) -> 0.

-spec status_of(term()) -> active | dissolved.
status_of(dissolved) -> dissolved;
status_of(<<"dissolved">>) -> dissolved;
status_of(_) -> active.
