-module(imboy_syn).
%%%
% imboy 用户websocket会话 模块
% imboy user websocket session module
% 对 https://github.com/ostinelli/syn 封装
%%%

-include_lib("stdlib/include/qlc.hrl").
-include("log.hrl").
-include("chat.hrl").

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

%% ACK 同步相关导出
-export([broadcast_ack_cancel/3]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化 syn 模块，添加节点到 scopes
-spec init() -> ok.
init() ->
    _ = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([
        ?CHAT_SCOPE
        % , ?GROUP_SCOPE
        , ?ROOM_SCOPE
        , ?CACHE_SCOPE
    ]),
    ok.

-spec join(integer(), binary(), pid(), binary()) -> ok | {error, term()}.
join(Uid, DType, Pid, DID) ->
    % Li = list_by_uid(Uid),
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    % ?DEBUG_LOG(["imboy_syn:join/4", Uid, Li]),
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

% [{{Uid, Pid}, {DType, DID}, Nano, Ref, Node}, ...]
-spec list_by_limit(integer() | error) -> [{{integer(), pid()}, {binary(), binary()}, integer(), reference(), node()}].
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

-spec list_by_uid(integer()) -> [{pid(), {binary(), binary()}}].
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
-spec online_dids(integer()) -> [binary()].
online_dids(Uid) ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    try
        [ DID1 || {_P, {_DType1, DID1}} <- list_by_uid(Uid) ]
    catch
        _:_ -> []
    end.

%% @doc 发布消息到指定用户的所有设备
-spec publish(integer(), term()) -> {ok, non_neg_integer()}.
publish(Uid, Msg) ->
    publish(Uid, Msg, 0).


% Delay: 最大的值为2^32 -1 milliseconds, 大约为49.7天。
-spec publish(integer(), term(), non_neg_integer()) -> {ok, non_neg_integer()}.
publish(Uid, Msg, Delay) when is_integer(Delay), Delay >= 0 ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    Members = list_by_uid(Uid),
    % ?DEBUG_LOG(["imboy_syn:publish/3", Uid, length(Members), Delay, Msg]),
    do_publish(Members, Msg, Delay);
publish(_Uid, _Msg, _Delay) -> 
    % ?DEBUG_LOG(["imboy_syn:publish/3 invalid parameters", _Uid, _Msg, _Delay]),
    {ok, 0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec do_publish(list(), term(), non_neg_integer()) -> {ok, non_neg_integer()}.
do_publish(Members, Message, 0) ->
    % ?DEBUG_LOG(["imboy_syn:do_publish/3 immediate", length(Members), Message]),
    % 使用 start_timer(0, ...) 确保立即投递和延迟投递的消息格式统一
    % 消息会被包装为 {timeout, Ref, Message} 格式，与延迟投递一致
    [ erlang:start_timer(0, Pid, Message) || {Pid, _Meta} <- Members ],
    {ok, length(Members)};
do_publish(Members, Message, Delay) when Delay > 0 ->
    % ?DEBUG_LOG(["imboy_syn:do_publish/3 delayed", length(Members), Delay, Message]),
    % Pid ! Message
    % Delay: 最大的值为2^32 -1 milliseconds, 大约为49.7天。
    [ erlang:start_timer(Delay, Pid, Message) || {Pid, _Meta} <- Members ],
    {ok, length(Members)}.


%% ===================================================================
%% ACK 同步功能（复用 ?CHAT_SCOPE）
%% ===================================================================

%% @doc 广播 ACK 取消消息到指定 UID 的所有设备
%% 使用现有的 ?CHAT_SCOPE，无需额外注册
%% 优势：
%% 1. 零额外资源消耗（复用现有注册）
%% 2. 极高性能（直接查询 UID，不扫描全表）
%% 3. 代码简洁（单 Scope 设计）
%% @param Uid 用户ID
%% @param DID 设备ID（用于标识当前设备）
%% @param MsgId 消息ID
-spec broadcast_ack_cancel(pos_integer(), binary(), binary()) -> ok.
broadcast_ack_cancel(Uid, DID, MsgId) ->
    %% 构建 ACK 取消消息
    Message = {ack_cancel, Uid, DID, MsgId, erlang:monotonic_time(millisecond)},

    try
        %% 【优化】直接查询该 UID 的所有设备进程（跨节点）
        %% syn:members/?CHAT_SCOPE 会返回所有节点上该 UID 的进程
        % [
        %     {
        %         <0.9568.0>,
        %         {<<"macos">>,<<"03F17F1F-770F-5CBC-AADA-F3722CF4E90B">>}
        %     }
        % ]
        Members = syn:members(?CHAT_SCOPE, Uid),

        _ = ?DEBUG_LOG([ack_cancel_broadcast, MsgId, Uid, DID, length(Members)]),

        %% 异步发送到所有相关进程（跨节点自动处理）
        lists:foreach(fun({Pid, _Meta}) ->
            Pid ! Message
        end, Members),
        _ = ?DEBUG_LOG([ack_cancel_broadcast_complete, MsgId, Uid, DID, length(Members)]),
        ok
    catch
        _:Error ->
            _ = ?ERROR_LOG([ack_cancel_broadcast_failed, MsgId, Uid, DID, Error]),
            %% 降级：确保本地至少执行一次
            local_ack_cancel(Uid, DID, MsgId),
            ok
    end.

%% @doc 本地执行 ACK 取消（降级方案）
-spec local_ack_cancel(pos_integer(), binary(), binary()) -> ok.
local_ack_cancel(Uid, DID, MsgId) ->
    %% 调用 websocket_logic:handle_ack_cancel
    websocket_logic:handle_ack_cancel(Uid, DID, MsgId),
    ok.
