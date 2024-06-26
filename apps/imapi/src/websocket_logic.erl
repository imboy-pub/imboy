-module(websocket_logic).
%%%
% websocket 业务逻辑模块
%%%

-include_lib("imlib/include/log.hrl").

% -export ([subprotocol/1]).
-export([c2c/3]).
-export([c2c_client_ack/3]).
-export([c2s/3]).
-export([c2s_client_ack/3]).
-export([revoke/4]).
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
    ToId = imboy_hashids:decode(To),
    % CurrentUid = imboy_hashids:decode(From),
    ?LOG([CurrentUid, ToId, Data]),
    % 判断当前用户是否是 ToId 用户的朋友
    IsFriend = friend_ds:is_friend(ToId, CurrentUid),
    % 判断当前用户是否在 ToId 的黑名单里面
    InDenylist = user_denylist_logic:in_denylist(ToId, CurrentUid),
    case {IsFriend, InDenylist} of
        {true, 0} ->
            NowTs = imboy_dt:utc(millisecond),
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

%% 单聊消息
-spec c2s(binary(), integer(), Data :: list()) -> ok | {reply, Msg :: list()}.
c2s(MsgId, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    % CurrentUid = imboy_hashids:decode(From),
    % ?LOG([CurrentUid, ToId, Data]),
    % 判断当前用户是否是 ToId 用户的朋友
    % 判断当前用户是否在 ToId 的黑名单里面
    case cowboy_bstr:to_lower(To) of
        <<"bot_qian_fan">> ->
            NowTs = imboy_dt:utc(millisecond),

            self() ! {reply, [{<<"id">>, MsgId}, {<<"type">>, <<"C2S_SERVER_ACK">>}, {<<"server_ts">>, NowTs}]},

            From = imboy_hashids:encode(CurrentUid),
            Payload = proplists:get_value(<<"payload">>, Data),
            Text = proplists:get_value(<<"text">>, Payload),
            TopicId = proplists:get_value(<<"topic_id">>, Payload, 0),
            TopicTitle = proplists:get_value(<<"topic_title">>, Payload, <<>>),
            CreatedAt = proplists:get_value(<<"created_at">>, Data),

            msg_c2s_ds:write_topic(<<"C2S">>, TopicId, CurrentUid, To, TopicTitle, CreatedAt),
            RespMap = qianfan_api:create_chat(CurrentUid, Text, []),
            Payload2 = [{<<"bot_response">>, RespMap} | Payload],
            % 存储消息
            % 消息状态： 10 服务端收到 11 投递给三方  12 收到三方结果 20 已投递客户端'
            msg_c2s_ds:write_msg(MsgId, #{
                status => 12,
                topic_id => TopicId,
                from_id => CurrentUid,
                to_id => To,
                msg_id => MsgId,
                payload => imboy_str:replace_single_quote(jsone:encode(Payload2, [native_utf8])),
                created_at => CreatedAt
                }),

            MsgId2 = <<"bot_response", MsgId/binary>>,
            Msg = [{<<"id">>, MsgId2},
                   {<<"type">>, <<"C2S">>},
                   {<<"topic_id">>, TopicId},
                   {<<"from">>, To}, % 这里交换from to
                   {<<"to">>, From},
                   {<<"payload">>, #{
                        <<"msg_type">> => <<"text">>,
                        <<"text">> => imboy_str:replace_single_quote(maps:get(<<"result">>, RespMap))
                   }},
                   {<<"created_at">>, CreatedAt}],
            MsgJson = jsone:encode(Msg, [native_utf8]),
            MsLi = [0, 5000, 7000, 11000],
            message_ds:send_next(CurrentUid, MsgId2, MsgJson, MsLi),
            ok;
        _ ->
            % 不支持的c2s消息
            Msg = message_ds:assemble_s2c(MsgId, <<"c2s_unsupported">>, To),
            {reply, Msg}
    end.


%% 客户端确认C2s投递消息
-spec c2s_client_ack(binary(), integer(), binary()) -> ok.
c2s_client_ack(_MsgId, _CurrentUid, _DID) ->
    % Column = <<"id">>,
    % Where = <<"WHERE msg_id = '", (ec_cnv:to_binary(MsgId))/binary,"' AND to_id = ", (ec_cnv:to_binary(CurrentUid))/binary>>,
    % {ok, _CList, Rows} = msg_c2s_repo:read_msg(Where, Column, 1),
    % [msg_c2s_repo:delete_msg(Id) || {Id} <- Rows],
    ok.


%% 客户端撤回消息
-spec revoke(binary(), Data :: list(), binary(), binary()) -> ok | {reply, Msg :: list()}.
revoke(MsgId, Data, Type, Type2) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    ToId = imboy_hashids:decode(To),
    % ?LOG([From, To, ToId, Type, Data]),
    NowTs = imboy_dt:utc(millisecond),

    Msg = [{<<"id">>, MsgId}, {<<"from">>, From}, {<<"to">>, To}, {<<"server_ts">>, NowTs}],
    % 判断是否在线
    case user_logic:is_online(ToId) of
        true ->
            Msg2 = jsone:encode([{<<"type">>, Type} | Msg], [native_utf8]),
            imboy_syn:publish(ToId, Msg2),
            ok;
        false ->  % 对端离线处理
            FromId = imboy_hashids:decode(From),
            msg_c2c_ds:revoke_offline_msg(NowTs, MsgId, FromId, ToId),
            % {reply, [{<<"type">>, <<"C2C_REVOKE_ACK">>} | Msg]}
            {reply, [{<<"type">>, Type2} | Msg]}
    end.


%% 群聊发送消息
-spec c2g(binary(), integer(), list()) -> ok | {reply, list()}.
c2g(MsgId, CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = imboy_hashids:decode(Gid),
    % TODO check is group member
    MemberUids = group_ds:member_uids(ToGID),
    % Uids.
    NowTs = imboy_dt:utc(millisecond),
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
    [ msg_s2c_repo:delete_msg(Id) || {Id} <- Rows ],
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
