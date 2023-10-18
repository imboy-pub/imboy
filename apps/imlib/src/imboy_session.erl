-module(imboy_session).
%%%
% imboy 用户websocket会话 模块
% imboy user websocket session module
% 计划对 https://github.com/ostinelli/syn 封装
%%%

-include_lib("stdlib/include/qlc.hrl").
-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/chat.hrl").

% for user
-export([init/0, join/4, leave/2]).
-export([publish/2, publish/3]).
-export([count_user/0, count_user/1, count/0]).
-export([list_by_uid/1, list_by_limit/1, online_dids/1]).

-export([is_online/2]).


%% ===================================================================
%% API
%% ===================================================================
init() ->
    % ok.
    syn:add_node_to_scopes([
        ?CHAT_SCOPE
        % , ?GROUP_SCOPE
        , ?ROOM_SCOPE
    ]),
    ok.

-spec join(integer(), binary(), pid(), binary()) ->
    ok | {error, Reason :: term()}.
join(Uid, DType, Pid, DID) ->
    % Li = list_by_uid(Uid),
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    % ?LOG(["imboy_session:join/4", Uid, Li]),
    % [P ! stop || {P, {_DT, DID1}} <- Li, DID1 == DID],
    syn:join(?CHAT_SCOPE, Uid, Pid, {DType, DID}),
    ok.

-spec leave(integer(), pid()) ->
    ok | {error, Reason :: term()}.
leave(Uid, Pid) ->
    syn:leave(?CHAT_SCOPE, Uid, Pid).

% 在线用户数量统计，一个用户在多个不同的设备类型登录，算一个
-spec count_user() -> non_neg_integer().
count_user() ->
    syn:group_count(?CHAT_SCOPE).

% 用户在线设备统计
-spec count_user(integer()) -> non_neg_integer().
count_user(Uid) ->
    syn:member_count(?CHAT_SCOPE, Uid).

% 所有用户在线设备统计
-spec count() -> non_neg_integer().
count() ->
    Scope = ?CHAT_SCOPE,
    case syn_backbone:get_table_name(syn_pg_by_name, Scope) of
        undefined ->
            error({invalid_scope, Scope});
        TableByName ->
            DuplicatedGroups = ets:select(TableByName, [{
                {{'$1', '_'}, '_', '_', '_', '_'},
                [],
                ['$1']
            }]),
            length(DuplicatedGroups)
            % DuplicatedGroups
            % ordsets:from_list(DuplicatedGroups)
    end.

-spec list_by_limit(integer() | error) -> list().
list_by_limit(error) ->
    [
        % {<<"tips">>, "Limit参数有误"}
    ];
list_by_limit(Limit) ->
    Scope = ?CHAT_SCOPE,
    case syn_backbone:get_table_name(syn_pg_by_name, Scope) of
        undefined ->
            error({invalid_scope, Scope});
        TableByName ->
            % {{Uid, Pid}, {DType, DID}, Nanosecond, Ref, Node}
            case ets:select(TableByName, [{ '$1', [], ['$1']}], Limit) of
                '$end_of_table' ->
                    [];
                {Li, '$end_of_table'} ->
                    Li;
                {Li, {TableByName, _, _, _, _, _, _, _}} ->
                    Li
            end
    end.

-spec list_by_uid(integer()) -> list().
list_by_uid(Uid) ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    syn:members(?CHAT_SCOPE, Uid).

-spec is_online(integer(), tuple()) -> boolean().
is_online(Uid, {dtype, DType}) ->
    Li1 = list_by_uid(Uid),
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    Li2 = [DType1 || {_P, {DType1, _DID}} <- Li1, DType1 == DType],
    lists:member(DType, Li2);
is_online(Uid, {did, DID}) ->
    Li1 = list_by_uid(Uid),
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    Li2 = [DID1 || {_P, {_DType1, DID1}} <- Li1, DID1 == DID],
    lists:member(DID, Li2).

% 用户在线设备ID列表
-spec online_dids(integer()) -> list().
online_dids(Uid) ->
    Li1 = list_by_uid(Uid),
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    [DID1 || {_P, {_DType1, DID1}} <- Li1].

publish(Uid, Msg) ->
    publish(Uid, Msg, 0).
% Delay: 最大的值为2^32 -1 milliseconds, 大约为49.7天。
-spec publish(integer(), term(), non_neg_integer()) -> {ok, non_neg_integer()}.
publish(Uid, Msg, Delay) ->
    Members = syn:members(?CHAT_SCOPE, Uid),
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    do_publish(Members, Msg, Delay).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec do_publish(list(), term(), non_neg_integer()) ->
    {ok, non_neg_integer()}.
do_publish(Members, Message, Delay) ->
    lists:foreach(fun({Pid, _Meta}) ->
        % Pid ! Message
        % Delay: 最大的值为2^32 -1 milliseconds, 大约为49.7天。
        erlang:start_timer(Delay, Pid, Message)
    end, Members),
    {ok, length(Members)}.
