-module(msg_c2g_logic).
%%%
% msg_c2g 业务逻辑模块
%%%
-export([c2g/3]).
-export([c2g_client_ack/3]).
-export([c2g_revoke/5]).

-export([check_msg/3]).

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").


%% ===================================================================
%% API
%% ===================================================================

%% 群聊发送消息
-spec c2g(binary(), integer(), list()) -> ok | {reply, list()}.
c2g(MsgId, CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = imboy_hashids:decode(Gid),
    % TODO check is group member
    MemberUids = group_ds:member_uids(ToGID),
    % Uids.
    NowTs = imboy_dt:now(),
    Msg = [{<<"id">>, MsgId},
           {<<"type">>, <<"C2G">>},
           {<<"from">>, imboy_hashids:encode(CurrentUid)},
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
c2g_client_ack(MsgId, Uid, _DID) ->
    msg_c2g_timeline_repo:client_ack(Uid, MsgId),
    ok.

%% 客户端撤回消息 for c2g
-spec c2g_revoke(integer(), binary(), Data :: list(), binary(), binary()) -> ok | {reply, Msg :: list()}.
c2g_revoke(_CurrentUid, _MsgId, _Data, <<"c2g_revoke_ack">>, _Type2) ->
    ok;
c2g_revoke(CurrentUid, MsgId, Data, Type, Type2) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    ToGID = imboy_hashids:decode(To),
    % TODO check is group member
    MemberUids = group_ds:member_uids(ToGID),
    % Uids.
    NowTs = imboy_dt:now(),

    Payload = [
        {<<"msg_type">>, Type},
        {<<"custom_type">>, <<"peer_revoked">>}
    ],
    Msg = [
        {<<"id">>, MsgId},
        {<<"from">>, From},
        {<<"to">>, To},
        {<<"type">>, Type},
        {<<"payload">>, Payload},
        {<<"server_ts">>, NowTs}
    ],

    % ?LOG(Msg),
    Msg2 = jsone:encode(Msg, [native_utf8]),
    MsLi = [0, 3500, 3500, 3000, 5000],
    [message_ds:send_next(Uid, MsgId, Msg2, MsLi) || Uid <- MemberUids, CurrentUid /= Uid],

    % 存储消息
    msg_c2g_ds:revoke_offline_msg(Msg2, NowTs, MsgId, CurrentUid, MemberUids, ToGID),

    self() ! {reply, [{<<"id">>, MsgId}, {<<"type">>, Type2}, {<<"server_ts">>, NowTs}]},
    ok.


check_msg(Uid, Pid, _DID) ->
    GMsgs = msg_c2g_ds:read_msg(Uid),
    sent_offline_msg(Uid, Pid, GMsgs, 0),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


sent_offline_msg(_Uid, _Pid, [], _Index) ->
    ok;
sent_offline_msg(Uid, Pid, [Row | Tail], Index) ->
    {<<"payload">>, Msg} = lists:keyfind(<<"payload">>, 1, Row),
    ?LOG([Uid, Pid, Index, Msg]),
    Delay = 100 + Index * 100,
    erlang:start_timer(Delay, Pid, Msg),
    sent_offline_msg(Uid, Pid, Tail, Index + 1).
