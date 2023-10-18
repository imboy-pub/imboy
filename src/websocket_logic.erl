-module(websocket_logic).
%%%
% websocket 业务逻辑模块
%%%

-include_lib("imlib/include/log.hrl").

% -export ([subprotocol/1]).
-export([c2c/3]).
-export([c2c_client_ack/3]).
-export([c2c_revoke/3]).
-export([c2g/3]).
-export([s2c/3]).
-export([s2c_client_ack/3]).
-export([c2g_client_ack/3]).


%% ===================================================================
%% API
%% ===================================================================

%% 单聊消息
-spec c2c(binary(), integer(), Data :: list()) -> ok | {reply, Msg :: list()}.
c2c(MsgId, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    ToId = imboy_hashids:uid_decode(To),
    % CurrentUid = imboy_hashids:uid_decode(From),
    ?LOG([CurrentUid, ToId, Data]),
    % 判断当前用户是否是 ToId 用户的朋友
    IsFriend = friend_ds:is_friend(ToId, CurrentUid),
    % 判断当前用户是否在 ToId 的黑名单里面
    InDenylist = user_denylist_logic:in_denylist(ToId, CurrentUid),
    case {IsFriend, InDenylist} of
        {true, 0} ->
            NowTs = imboy_dt:millisecond(),
            From = imboy_hashids:uid_encode(CurrentUid),
            Payload = proplists:get_value(<<"payload">>, Data),
            CreatedAt = proplists:get_value(<<"created_at">>, Data),
            % 存储消息
            msg_c2c_ds:write_msg(CreatedAt, MsgId, Payload, CurrentUid, ToId, NowTs),
            %
            self() ! {reply, [{<<"id">>, MsgId}, {<<"type">>, <<"C2C_SERVER_ACK">>}, {<<"server_ts">>, NowTs}]},

            Msg = [{<<"id">>, MsgId},
                   {<<"type">>, <<"C2C">>},
                   {<<"from">>, From},
                   {<<"to">>, To},
                   {<<"payload">>, Payload},
                   {<<"created_at">>, CreatedAt},
                   {<<"server_ts">>, NowTs}],
            MsgJson = jsone:encode(Msg, [native_utf8]),
            MsLi = [0, 3000, 5000, 7000, 11000],
            message_ds:send_next(ToId, MsgId, MsgJson, MsLi),
            ok;
        {_, InDenylist2} when InDenylist2 > 0 ->
            Msg = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, To),
            {reply, Msg};
        {false, _InDenylist} ->
            Msg = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, To),
            {reply, Msg}
    end.


%% 客户端确认C2C投递消息
-spec c2c_client_ack(binary(), integer(), binary()) -> ok.
c2c_client_ack(MsgId, CurrentUid, _DID) ->
    Column = <<"id">>,
    Where = <<"WHERE msg_id = $1 AND to_id = $2">>,
    Vals = [MsgId, CurrentUid],
    {ok, _CList, Rows} = msg_c2c_repo:read_msg(Where, Vals, Column, 1),
    [msg_c2c_repo:delete_msg(Id) || {Id} <- Rows],
    ok.


%% 客户端撤回消息
-spec c2c_revoke(binary(), Data :: list(), binary()) -> ok | {reply, Msg :: list()}.
c2c_revoke(MsgId, Data, Type) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    ToId = imboy_hashids:uid_decode(To),
    % ?LOG([From, To, ToId, Type, Data]),
    NowTs = imboy_dt:millisecond(),

    Msg = [{<<"id">>, MsgId}, {<<"from">>, From}, {<<"to">>, To}, {<<"server_ts">>, NowTs}],
    % 判断是否在线
    case user_logic:is_online(ToId) of
        true ->
            Msg2 = jsone:encode([{<<"type">>, Type} | Msg], [native_utf8]),
            imboy_session:publish(ToId, Msg2),
            ok;
        false ->  % 对端离线处理
            FromId = imboy_hashids:uid_decode(From),
            msg_c2c_ds:revoke_offline_msg(NowTs, MsgId, FromId, ToId),
            {reply, [{<<"type">>, <<"C2C_REVOKE_ACK">>} | Msg]}
    end.


%% 群聊发送消息
-spec c2g(binary(), integer(), list()) -> ok | {reply, list()}.
c2g(MsgId, CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = imboy_hashids:uid_decode(Gid),
    % TODO check is group member
    MemberUids = group_member_ds:member_uids(ToGID, CurrentUid),
    % Uids.
    NowTs = imboy_dt:millisecond(),
    Msg = [{<<"id">>, MsgId},
           {<<"type">>, <<"C2G">>},
           {<<"from">>, imboy_hashids:uid_encode(CurrentUid)},
           {<<"to">>, Gid},
           {<<"payload">>, proplists:get_value(<<"payload">>, Data)},
           {<<"created_at">>, proplists:get_value(<<"created_at">>, Data)},
           {<<"server_ts">>, NowTs}],
    % ?LOG(Msg),
    Msg2 = jsone:encode(Msg, [native_utf8]),
    MsLi = [0, 3500, 3500, 3000, 5000],
    [message_ds:send_next(Uid, MsgId, Msg2, MsLi) || Uid <- MemberUids, CurrentUid /= Uid],

    % 存储消息
    msg_c2g_ds:write_msg(NowTs, MsgId, Msg2, CurrentUid, MemberUids, ToGID),

    self() ! {reply, [{<<"id">>, MsgId}, {<<"type">>, <<"C2G_SERVER_ACK">>}, {<<"server_ts">>, NowTs}]},
    ok.


%% 客户端确认C2G投递消息
-spec c2g_client_ack(binary(), integer(), binary()) -> ok.
c2g_client_ack(MsgId, CurrentUid, _DID) ->
    msg_c2g_timeline_repo:delete_timeline(CurrentUid, MsgId),
    case msg_c2g_timeline_repo:check_msg(MsgId) of
        0 ->
            msg_c2g_repo:delete_msg(MsgId);
        _ ->
            ok
    end.


%% 系统消息
-spec s2c(binary(), integer(), list()) -> ok | {reply, list()}.
s2c(_Id, _CurrentUid, _Data) ->
    ok.


%% 客户端确认S2C投递消息
-spec s2c_client_ack(binary(), integer(), binary()) -> ok.
s2c_client_ack(MsgId, CurrentUid, _DID) ->
    Column = <<"id">>,
    Where = <<"WHERE msg_id = $1 AND to_id = $2">>,
    Vals = [MsgId, CurrentUid],
    {ok, _CList, Rows} = msg_s2c_repo:read_msg(Where, Vals, Column, 1),
    [msg_s2c_repo:delete_msg(Id) || {Id} <- Rows],
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
