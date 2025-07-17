-module(imboy_syn).
%%%
% imboy 用户websocket会话 模块
% imboy user websocket session module
% 对 https://github.com/ostinelli/syn 封装
%%%

-include_lib("stdlib/include/qlc.hrl").
-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/chat.hrl").

% for user
-export([init/0,
         join/4,
         leave/2]).
-export([publish/2, publish/3]).
-export([count_user/0, count_user/1,
         count/0]).
-export([list_by_uid/1,
         list_by_limit/1,
         online_dids/1]).

-export([is_online/2]).

%% ===================================================================
%% API
%% ===================================================================
init() ->
    application:ensure_all_started(syn),
    case syn:add_node_to_scopes([
        ?CHAT_SCOPE
        % , ?GROUP_SCOPE
        , ?ROOM_SCOPE
    ]) of
        ok -> ok;
        {error, Reason} -> ?LOG(["syn:add_node_to_scopes error", Reason]), {error, Reason}
    end.

-spec join(integer(), binary(), pid(), binary()) -> ok | {error, term()}.
join(Uid, DType, Pid, DID) ->
    % Li = list_by_uid(Uid),
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    % ?LOG(["imboy_syn:join/4", Uid, Li]),
    % [P ! stop || {P, {_DT, DID1}} <- Li, DID1 == DID],
    try
        ok = syn:join(?CHAT_SCOPE, Uid, Pid, {DType, DID}),
        ok
    catch
        _:Reason -> {error, Reason}
    end.

-spec leave(integer(), pid()) -> ok | {error, term()}.
leave(Uid, Pid) ->
    try
        syn:leave(?CHAT_SCOPE, Uid, Pid)
    catch
        _:Reason -> {error, Reason}
    end.

% 在线用户数量统计，一个用户在多个不同的设备类型登录，算一个
-spec count_user() -> non_neg_integer().
count_user() ->
    try
        syn:group_count(?CHAT_SCOPE)
    catch
        _:_ -> 0
    end.

% 用户在线设备统计
-spec count_user(integer()) -> non_neg_integer().
count_user(Uid) ->
    try
        syn:member_count(?CHAT_SCOPE, Uid)
    catch
        _:_ -> 0
    end.

% 所有用户在线设备统计
-spec count() -> non_neg_integer().
count() ->
    try
        TableByName = syn_backbone:get_table_name(syn_pg_by_name, ?CHAT_SCOPE),
        case TableByName of
            undefined -> 0;
            _ ->
                ets:info(TableByName, size)
        end
    catch
        _:_ -> 0
    end.

-spec list_by_limit(integer() | error) -> list().
list_by_limit(error) -> [];
list_by_limit(Limit) when is_integer(Limit), Limit > 0 ->
    try
        case syn_backbone:get_table_name(syn_pg_by_name, ?CHAT_SCOPE) of
            undefined -> [];
            TableByName ->
                case ets:select(TableByName, [{'$1', [], ['$1']}], Limit) of
                    '$end_of_table' -> [];
                    {Li, _} -> Li
                end
        end
    catch
        _:_ -> []
    end;
list_by_limit(_) -> [].

-spec list_by_uid(integer()) -> list().
list_by_uid(Uid) ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    try
        syn:members(?CHAT_SCOPE, Uid)
    catch
        _:_ -> []
    end.

-spec is_online(integer(), tuple()) -> boolean().
is_online(Uid, {dtype, DType}) ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    try
        lists:any(fun({_P, {DType1, _DID}}) -> DType1 == DType end, list_by_uid(Uid))
    catch
        _:_ -> false
    end;
is_online(Uid, {did, DID}) ->
    try
        lists:any(fun({_P, {_DType1, DID1}}) -> DID1 == DID end, list_by_uid(Uid))
    catch
        _:_ -> false
    end.

% 用户在线设备ID列表
-spec online_dids(integer()) -> list().
online_dids(Uid) ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    try
        [ DID1 || {_P, {_DType1, DID1}} <- list_by_uid(Uid) ]
    catch
        _:_ -> []
    end.

publish(Uid, Msg) ->
    publish(Uid, Msg, 0).


% Delay: 最大的值为2^32 -1 milliseconds, 大约为49.7天。
-spec publish(integer(), term(), non_neg_integer()) -> {ok, non_neg_integer()}.
publish(Uid, Msg, Delay) when is_integer(Delay), Delay >= 0 ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    Members = list_by_uid(Uid),
    do_publish(Members, Msg, Delay);
publish(_Uid, _Msg, _Delay) -> {ok, 0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec do_publish(list(), term(), non_neg_integer()) -> {ok, non_neg_integer()}.
do_publish(Members, Message, 0) ->
    [ Pid ! Message || {Pid, _Meta} <- Members ],
    {ok, length(Members)};
do_publish(Members, Message, Delay) when Delay > 0 ->
    % Pid ! Message
    % Delay: 最大的值为2^32 -1 milliseconds, 大约为49.7天。
    [ erlang:start_timer(Delay, Pid, Message) || {Pid, _Meta} <- Members ],
    {ok, length(Members)}.
