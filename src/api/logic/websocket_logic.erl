-module (websocket_logic).
%%%
% websocket 业务逻辑模块
%%%
% -export ([subprotocol/1]).
-export ([c2c/3]).
-export ([c2c_client_ack/3]).
-export ([c2c_revoke/3]).
-export ([c2g/3]).
-export ([system/3]).

-include("common.hrl").

%% 单聊消息
-spec c2c(binary(), integer(), Data::list()) -> ok | {reply, Msg::list()}.
c2c(Id, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    ToId = hashids_translator:uid_decode(To),
    % CurrentUid = hashids_translator:uid_decode(From),
    ?LOG([CurrentUid, ToId, Data]),
    case friend_ds:is_friend(CurrentUid, ToId) of
        true ->
            NowTs = dt_util:milliseconds(),
            From = hashids_translator:uid_encode(CurrentUid),
            Payload = proplists:get_value(<<"payload">>, Data),
            CreatedAt = proplists:get_value(<<"created_at">>, Data),
            % 存储消息
            if
                is_binary(Payload)  ->
                    msg_c2c_ds:write_msg(CreatedAt, Id, Payload, CurrentUid, ToId, NowTs);
                true ->
                    msg_c2c_ds:write_msg(CreatedAt, Id, jsone:encode(Payload), CurrentUid, ToId, NowTs)
            end,
            Msg = [
                {<<"id">>, Id},
                {<<"type">>,<<"C2C">>},
                {<<"from">>, From},
                {<<"to">>, To},
                {<<"payload">>, Payload},
                {<<"created_at">>, CreatedAt},
                {<<"server_ts">>, NowTs}
            ],
            _TimerRefList = message_ds:send(ToId, jsone:encode(Msg), 1),
            {reply, [
                {<<"id">>, Id},
                {<<"type">>,<<"C2C_SERVER_ACK">>},
                {<<"server_ts">>, NowTs}
            ]};
        false ->
            Msg = [
                {<<"type">>,<<"error">>},
                {<<"code">>, 1},
                {<<"msg">>, unicode:characters_to_binary("非好友关系，没法单聊")},
                {<<"timestamp">>, dt_util:milliseconds()}
            ],
            {reply, Msg}
    end.

%% 客户端确认投递消息
-spec c2c_client_ack(MsgId::binary(), CurrentUid::integer(), Data::binary()) -> ok.
c2c_client_ack(MsgId, CurrentUid, _DID) ->
    Column = <<"`id`">>,
    Where = <<"WHERE `msg_id` = ? AND `to_id` = ?">>,
    Vals = [MsgId, CurrentUid],
    {ok, _ColumnList, Rows} = msg_c2c_repo:read_msg(Where, Vals, Column, 1),
    [msg_c2c_repo:delete_msg(Id) || [Id] <- Rows],
    ok.

%% 客户端撤回消息
-spec c2c_revoke(binary(), Data::list(), binary()) -> ok | {reply, Msg::list()}.
c2c_revoke(Id, Data, Type) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    ToId = hashids_translator:uid_decode(To),
    ?LOG([From, To, ToId, Type, Data]),
    NowTs = dt_util:milliseconds(),
    % 判断是否在线
    case user_ds:is_offline(ToId) of
        {ToPid, _UidBin, _ClientSystemBin} ->
            erlang:start_timer(1, ToPid, jsone:encode([
                {<<"id">>, Id},
                {<<"type">>, Type},
                {<<"from">>, From},
                {<<"to">>, To},
                {<<"server_ts">>, NowTs}
            ])),
            ok;
        true -> % 对端离线处理
            FromId = hashids_translator:uid_decode(From),
            msg_c2c_ds:revoke_offline_msg(NowTs, Id, FromId, ToId),
            Msg3 = [
                {<<"id">>, Id},
                {<<"type">>, <<"C2C_REVOKE_ACK">>},
                {<<"from">>, From},
                {<<"to">>, To},
                {<<"server_ts">>, NowTs}
            ],
            {reply, Msg3}
    end.

%% 群聊发送消息
c2g(Id, CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = hashids_translator:uid_decode(Gid),
    % TODO check is group member
    Column = <<"`user_id`">>,
    {ok, _ColumnLi, Members} = group_member_repo:find_by_group_id(ToGID, Column),
    Uids = [Uid || [Uid] <- Members, Uid /= CurrentUid],
    % Uids.
    NowTs = dt_util:milliseconds(),
    Msg = [
        {<<"id">>, Id},
        {<<"type">>,<<"C2G">>},
        {<<"from">>, hashids_translator:uid_encode(CurrentUid)},
        {<<"to">>, Gid},
        {<<"payload">>, proplists:get_value(<<"payload">>, Data)},
        {<<"created_at">>, proplists:get_value(<<"created_at">>, Data)},
        {<<"server_ts">>, NowTs}
    ],
    % ?LOG(Msg),
    Msg2 = jsone:encode(Msg),
    _UidsOnline = lists:filtermap(fun(Uid) ->
        message_ds:send(Uid, Msg2, 1)
    end, Uids),
    % 存储消息
    msg_c2g_ds:write_msg(NowTs, Id, Msg2, CurrentUid, Uids, ToGID),
    ok.

%% 系统消息
-spec c2g(binary(), integer(), Data::list()) -> ok | {reply, Msg::list()}.
system(_Id, _CurrentUid, _Data) ->
    ok.
