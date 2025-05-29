-module(msg_s2c_logic).
%%%
%  msg_c2c 业务逻辑模块
%%%

-export([s2c/4]).
-export([s2c_client_ack/3]).


-export([check_msg/3]).

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% 系统消息
-spec s2c(binary(), binary(), integer(), list()) -> ok | {reply, list()}.
s2c(<<"C2C_DEL_EVERYONE">>, MsgId, CurrentUid, Data) ->
    Payload = proplists:get_value(<<"payload">>, Data),
    To = proplists:get_value(<<"to">>, Data),
    ToId = imboy_hashids:decode(To),

    OldMsgId = proplists:get_value(<<"old_msg_id">>, Payload),
    % CurrentUid = imboy_hashids:decode(From),
    ?DEBUG_LOG([CurrentUid, ToId, Data]),
    NowTs = imboy_dt:now(),

    % 删除原有消息
    % use index uk_c2c_MsgId
    Where = <<"WHERE msg_id = $1 AND from_id = $2">>,
    msg_c2g_repo:delete_msg(Where, [OldMsgId, CurrentUid]),
    % 数据库会自动删除 相关 msg_c2g_timeline

    % 存储s2c消息
    msg_s2c_ds:write_msg(NowTs, MsgId, Payload, CurrentUid, ToId, NowTs),

    % 按策略发送消息
    From = imboy_hashids:encode(CurrentUid),
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId),
    % ?DEBUG_LOG(Msg),
    MsLi = [0, 1500, 1500, 3000, 5000, 7000],
    message_ds:send_next(ToId, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    % 给操作者回复消息
    {reply, Msg};


s2c(<<"C2G_DEL_FOR_ME">>, MsgId, CurrentUid, Data) ->
    Payload = proplists:get_value(<<"payload">>, Data),
    Gid = proplists:get_value(<<"to">>, Data),
    % ToGID = imboy_hashids:decode(Gid),
    From = imboy_hashids:encode(CurrentUid),
    OldMsgId = proplists:get_value(<<"old_msg_id">>, Payload),
    msg_c2g_timeline_repo:delete_timeline(CurrentUid, OldMsgId),
    % 给操作者回复消息
    Msg = message_ds:assemble_msg(<<"S2C">>, From, Gid, Payload, MsgId),
    {reply, Msg};
s2c(<<"C2G_DEL_EVERYONE">>, MsgId, CurrentUid, Data) ->
    Payload = proplists:get_value(<<"payload">>, Data),
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = imboy_hashids:decode(Gid),
    MemberUids = group_ds:member_uids(ToGID),

    OldMsgId = proplists:get_value(<<"old_msg_id">>, Payload),
    % CurrentUid = imboy_hashids:decode(From),
    NowTs = imboy_dt:now(),

    % 删除原有消息
    % use index uk_c2c_MsgId
    Where = <<"WHERE msg_id = $1 AND from_id = $2">>,
    msg_c2c_repo:delete_msg(Where, [OldMsgId, CurrentUid]),

    From = imboy_hashids:encode(CurrentUid),

    % 存储s2c消息
    [s2c_for_c2g(NowTs, CurrentUid, From, Uid, Payload ) || Uid <- MemberUids, CurrentUid /= Uid],

    % 给操作者回复消息
    Msg = message_ds:assemble_msg(<<"S2C">>, From, Gid, Payload, MsgId),
    {reply, Msg}.

%% 1 存储s2c消息
%% 2 按策略发送消息
s2c_for_c2g(NowTs, CurrentUid, From, Uid, Payload ) ->
    To = imboy_hashids:encode(Uid),
    % s2c.5ia0V5.Kr3aUs.F
    MsgId = imboy_func:uid("s2c"),
    % 存储s2c消息
    msg_s2c_ds:write_msg(NowTs, MsgId, Payload, CurrentUid, Uid, NowTs),

    % 按策略发送消息
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId),
    % ?DEBUG_LOG(Msg),
    MsLi = [0, 1500, 1500, 3000, 5000, 7000],
    message_ds:send_next(Uid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
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

%% 检查离线消息
% 单聊离线消息，每个离线用户的消息获取10条（差不多一屏幕多），如果多于10条，再返回消除总数量
%%
check_msg(Uid, Pid, _DID) ->
    % ?DEBUG_LOG(["msg_c2c_logic/check_msg/2", Uid, Pid]),
    case msg_s2c_ds:read_msg(Uid, ?SAVE_MSG_LIMIT) of
        [] ->
            ok;
        MsgsS2C ->
            % 发送S2c离线消息
            sent_offline_msg(Pid, <<"S2C">>, MsgsS2C, 0)
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
    % ?DEBUG_LOG(["Row", Row, "; Payload: ", Payload]),
    Row2 = imboy_cnv:convert_at_timestamps(Row),
    Delay = 100 + Index * 100,
    Msg = [{<<"id">>, MsgId},
           {<<"type">>, Type},
           {<<"from">>, imboy_hashids:encode(FromId)},
           {<<"to">>, imboy_hashids:encode(ToId)},
           {<<"payload">>, jsone:decode(Payload, [{object_format, proplist}])},
           lists:keyfind(<<"created_at">>, 1, Row2),
           lists:keyfind(<<"server_ts">>, 1, Row2)],
    % ?DEBUG_LOG([Delay, "Msg: ", Msg]),
    erlang:start_timer(Delay, Pid, jsone:encode(Msg, [native_utf8])),
    sent_offline_msg(Pid, Type, Tail, Index + 1).
