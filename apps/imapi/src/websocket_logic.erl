-module(websocket_logic).
%%%
% websocket 业务逻辑模块
%%%

-include_lib("imlib/include/log.hrl").

% -export ([subprotocol/1]).

-export([ack_before/3]).

-export([c2s/3]).
-export([c2s_client_ack/3]).

%% ===================================================================
%% API
%% ===================================================================

ack_before(CurrentUid, DID, MsgId) ->
    Key = {CurrentUid, DID, MsgId},
    ?LOG(["CLIENT_ACK", Key]),
    % 缓存在 message_ds:send_next/5 中设置
    case imboy_cache:get(Key) of
        undefined ->
            ok;
        {ok, TimerRef} ->
            ?LOG(["CLIENT_ACK", Key, TimerRef]),
            erlang:cancel_timer(TimerRef),
            imboy_cache:flush(Key)
    end.

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
            % NowTs = imboy_dt:now(),

            self() ! {reply, [{<<"id">>, MsgId}, {<<"type">>, <<"C2S_SERVER_ACK">>}, {<<"server_ts">>, imboy_dt:millisecond()}]},

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



%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
