%%% @doc 好友关系聚合根 / Friendship Aggregate Root
%%%
%%% DDD 充血改造 Phase 3 / T3.3：把好友关系的「申请 / 通过 / 拒绝 /
%%% 拉黑 / 解除拉黑 / 删除」生命周期内聚为状态机，由纯函数守护转换不变量，
%%% 杜绝散落在 friend_logic（add_friend/confirm_friend/delete_friend）与
%%% user_denylist_logic 的状态判定。
%%%
%%% Functional-core aggregate modeling the directed from→to relationship
%%% as a state machine guarding legal transitions:
%%%   none --request--> pending --accept--> friends
%%%   pending --reject--> none
%%%   {none|pending|friends} --block--> blocked --unblock--> none
%%%   friends --remove--> none
%%% Operations return {ok, NewState, [event()]} | {error, atom()}; the
%%% imperative shell (logic) persists state and publishes events after commit.
%%% Zero side effects -> eunit without mocks.
%%%
%%% 关系视角：本聚合表达 from→to 单向关系状态，与现状「双向 friend 行
%%% （status=1 已确认）+ 单向 user_denylist（拉黑）」语义对齐；申请态在现状
%%% 为 S2C 消息（无 friend 行），聚合显式建模为 pending。
%%%
%%% 渐进策略（NOT Building）：本 task 仅新增纯领域层，不改既有 friend_logic /
%%% user_denylist_logic；DS load/save 接线与 logic 退化外壳留后续（T3.4）。
-module(friend_agg).

-export([new/2, rehydrate/1]).
-export([request/1, accept/1, reject/1, block/1, unblock/1, remove/1]).
-export([from/1, to/1, status/1]).

-export_type([t/0, event/0, status/0]).

%% 好友关系聚合状态（from→to 单向）：
%%   from   关系发起方 uid（binary，可序列化跨进程派发）
%%   to     关系目标方 uid（binary）
%%   status none | pending | friends | blocked
-record(friendship, {
    from :: binary(),
    to :: binary(),
    status = none :: status()
}).
-opaque t() :: #friendship{}.

-type status() :: none | pending | friends | blocked.

%% 领域事件：uid 以 binary 透传（可序列化、跨进程派发）。
-type event() ::
    {friend_requested, From :: binary(), To :: binary()}
    | {friend_accepted, From :: binary(), To :: binary()}
    | {friend_rejected, From :: binary(), To :: binary()}
    | {friend_blocked, From :: binary(), To :: binary()}
    | {friend_unblocked, From :: binary(), To :: binary()}
    | {friend_removed, From :: binary(), To :: binary()}.

%% ===================================================================
%% 构造 / 重建
%% ===================================================================

%% @doc 新建关系（初始 none 态）。
-spec new(From :: binary(), To :: binary()) -> t().
new(From, To) ->
    #friendship{from = From, to = To, status = none}.

%% @doc 从持久化裸 map 重建聚合（DS 层调用）。
%% status 兼容现状 friend 表整型（1 = 已确认）与领域 atom。
-spec rehydrate(map()) -> t().
rehydrate(M) when is_map(M) ->
    #friendship{
        from = maps:get(<<"from_user_id">>, M),
        to = maps:get(<<"to_user_id">>, M),
        status = status_of(maps:get(<<"status">>, M, none))
    }.

%% ===================================================================
%% 申请 / 通过 / 拒绝
%% ===================================================================

%% @doc 发起好友申请：仅 none 态可发；已申请/已是好友/已拉黑各自拒绝。
-spec request(t()) ->
    {ok, t(), [event()]}
    | {error, already_requested | already_friends | blocked}.
request(F = #friendship{status = none, from = From, to = To}) ->
    {ok, F#friendship{status = pending}, [{friend_requested, From, To}]};
request(#friendship{status = pending}) ->
    {error, already_requested};
request(#friendship{status = friends}) ->
    {error, already_friends};
request(#friendship{status = blocked}) ->
    {error, blocked}.

%% @doc 通过申请：仅 pending 态可通过。
-spec accept(t()) -> {ok, t(), [event()]} | {error, no_pending_request}.
accept(F = #friendship{status = pending, from = From, to = To}) ->
    {ok, F#friendship{status = friends}, [{friend_accepted, From, To}]};
accept(#friendship{}) ->
    {error, no_pending_request}.

%% @doc 拒绝申请：仅 pending 态可拒绝，回到 none。
-spec reject(t()) -> {ok, t(), [event()]} | {error, no_pending_request}.
reject(F = #friendship{status = pending, from = From, to = To}) ->
    {ok, F#friendship{status = none}, [{friend_rejected, From, To}]};
reject(#friendship{}) ->
    {error, no_pending_request}.

%% ===================================================================
%% 拉黑 / 解除拉黑 / 删除
%% ===================================================================

%% @doc 拉黑：none/pending/friends 任意态均可拉黑；已拉黑则幂等无事件。
-spec block(t()) -> {ok, t(), [event()]}.
block(F = #friendship{status = blocked}) ->
    {ok, F, []};
block(F = #friendship{from = From, to = To}) ->
    {ok, F#friendship{status = blocked}, [{friend_blocked, From, To}]}.

%% @doc 解除拉黑：仅 blocked 态可解除，回到 none。
-spec unblock(t()) -> {ok, t(), [event()]} | {error, not_blocked}.
unblock(F = #friendship{status = blocked, from = From, to = To}) ->
    {ok, F#friendship{status = none}, [{friend_unblocked, From, To}]};
unblock(#friendship{}) ->
    {error, not_blocked}.

%% @doc 删除好友：仅 friends 态可删除，回到 none。
-spec remove(t()) -> {ok, t(), [event()]} | {error, not_friends}.
remove(F = #friendship{status = friends, from = From, to = To}) ->
    {ok, F#friendship{status = none}, [{friend_removed, From, To}]};
remove(#friendship{}) ->
    {error, not_friends}.

%% ===================================================================
%% 访问器
%% ===================================================================

-spec from(t()) -> binary().
from(#friendship{from = From}) -> From.

-spec to(t()) -> binary().
to(#friendship{to = To}) -> To.

-spec status(t()) -> status().
status(#friendship{status = S}) -> S.

%% ===================================================================
%% 内部：持久化状态规整
%% ===================================================================

-spec status_of(term()) -> status().
status_of(1) -> friends;
status_of(<<"1">>) -> friends;
status_of(friends) -> friends;
status_of(pending) -> pending;
status_of(blocked) -> blocked;
status_of(_) -> none.
