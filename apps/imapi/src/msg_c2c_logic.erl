-module(msg_c2c_logic).
%%%
%  msg_c2c 业务逻辑模块
%%%

-export([c2c/3]).
-export([c2c_client_ack/3]).
-export([c2c_revoke/4]).

-export([check_msg/3]).

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% 单聊消息
-spec c2c(binary(), integer(), Data :: list()) -> ok | {reply, Msg :: list()}.
c2c(MsgId, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    ToId = imboy_hashids:decode(To),
    % CurrentUid = imboy_hashids:decode(From),
    ?LOG([CurrentUid, ToId, Data]),
    % 判断当前用户是否是 ToId 用户的朋友
    IsFriend = friend_ds:is_friend(ToId, CurrentUid),
    % 判断当前用户是否在 ToId 的黑名单里面
    InDenylist = user_denylist_logic:in_denylist(ToId, CurrentUid),
    case {IsFriend, InDenylist} of
        {true, 0} ->
            NowTs = imboy_dt:now(),
            From = imboy_hashids:encode(CurrentUid),
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
            MsLi = [0, 5000, 7000, 11000],
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
    Where = <<"WHERE msg_id = '", (ec_cnv:to_binary(MsgId))/binary,"' AND to_id = ", (ec_cnv:to_binary(CurrentUid))/binary>>,
    {ok, _CList, Rows} = msg_c2c_repo:read_msg(Where, Column, 1),
    [msg_c2c_repo:delete_msg(Id) || {Id} <- Rows],
    ok.


%% 客户端撤回消息 for c2c
-spec c2c_revoke(binary(), Data :: list(), binary(), binary()) -> ok | {reply, Msg :: list()}.
c2c_revoke(MsgId, Data, Type, Type2) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    ToId = imboy_hashids:decode(To),
    % ?LOG([From, To, ToId, Type, Data]),
    NowTs = imboy_dt:now(),

    Payload = [
        {<<"text">>, <<>>},
        {<<"msg_type">>, <<"custom">>},
        {<<"custom_type">>, <<"peer_revoked">>}
    ],
    Msg = [
        {<<"id">>, MsgId},
        {<<"from">>, From},
        {<<"to">>, To},
        {<<"server_ts">>, NowTs},
        {<<"type">>, Type},
        {<<"payload">>, Payload}
    ],
    Msg2 = jsone:encode(Msg, [native_utf8]),
    % 判断是否在线
    case user_logic:is_online(ToId) of
        true ->
            imboy_syn:publish(ToId, Msg2),
            ok;
        false ->  % 对端离线处理
            Payload2 = jsone:encode(Payload, [native_utf8]),
            FromId = imboy_hashids:decode(From),
            msg_c2c_ds:revoke_offline_msg(Payload2, NowTs, MsgId, FromId, ToId),
            % {reply, [{<<"type">>, <<"C2C_REVOKE_ACK">>} | Msg]}
            {reply, [{<<"type">>, Type2} | Msg]}
    end.

%% 检查离线消息
% 单聊离线消息，每个离线用户的消息获取10条（差不多一屏幕多），如果多于10条，再返回消除总数量
%%
check_msg(Uid, Pid, _DID) ->
    % ?LOG(["msg_c2c_logic/check_msg/2", Uid, Pid]),
    case msg_c2c_ds:read_msg(Uid, ?SAVE_MSG_LIMIT) of
        [] ->
            ok;
        MsgsC2C ->
            % 发送C2C离线消息
            sent_offline_msg(Pid, <<"C2C">>, MsgsC2C, 0)
    end,
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


sent_offline_msg(_Pid, _Type, [], _Index) ->
    ok;
sent_offline_msg(Pid, Type, [Row | Tail], Index) ->
    {<<"msg_id">>, MsgId} = lists:keyfind(<<"msg_id">>, 1, Row),
    {<<"from_id">>, FromId} = lists:keyfind(<<"from_id">>, 1, Row),
    {<<"to_id">>, ToId} = lists:keyfind(<<"to_id">>, 1, Row),
    {<<"payload">>, Payload} = lists:keyfind(<<"payload">>, 1, Row),
    % ?LOG(["Row", Row, "; Payload: ", Payload]),
    Delay = 100 + Index * 100,
    Msg = [{<<"id">>, MsgId},
           {<<"type">>, Type},
           {<<"from">>, imboy_hashids:encode(FromId)},
           {<<"to">>, imboy_hashids:encode(ToId)},
           {<<"payload">>, jsone:decode(Payload, [{object_format, proplist}])},
           lists:keyfind(<<"created_at">>, 1, Row),
           lists:keyfind(<<"server_ts">>, 1, Row)],
    % ?LOG([Delay, "Msg: ", Msg]),
    erlang:start_timer(Delay, Pid, jsone:encode(Msg, [native_utf8])),
    sent_offline_msg(Pid, Type, Tail, Index + 1).
