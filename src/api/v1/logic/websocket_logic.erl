-module (websocket_logic).
%%%
% websocket 业务逻辑模块
%%%
% -export ([subprotocol/1]).
-export ([dialog/3]).
-export ([group_dialog/3]).
-export ([system/3]).

-include("common.hrl").

-spec dialog(binary(), integer(), Data::list()) -> ok | {reply, Msg::list()}.
-spec group_dialog(binary(), integer(), Data::list()) -> ok | {reply, Msg::list()}.

%% 单聊发送消息
dialog(Id, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    ToId = hashids_translator:uid_decode(To),
    % CurrentUid = hashids_translator:uid_decode(From),
    ?LOG([CurrentUid, ToId, Data]),
    case friend_ds:is_friend(CurrentUid, ToId) of
        true ->
            NowTs = dt_util:milliseconds(),
            Msg = [
                {<<"id">>, Id},
                {<<"type">>,<<"C2C">>},
                {<<"from">>, hashids_translator:uid_encode(CurrentUid)},
                {<<"to">>, To},
                {<<"payload">>, proplists:get_value(<<"payload">>, Data)},
                {<<"created_at">>, proplists:get_value(<<"created_at">>, Data)},
                {<<"server_ts">>, NowTs}
            ],
            Msg2 = jsx:encode(Msg),
            % 存储消息
            dialog_msg_ds:write_msg(NowTs, Id, Msg2, CurrentUid, ToId),
            message_ds:send(ToId, Msg2),
            ok;
        false ->
            Msg = [
                {<<"type">>,<<"error">>},
                {<<"code">>, 1},
                {<<"msg">>, unicode:characters_to_binary("非好友关系，没法单聊")},
                {<<"timestamp">>, dt_util:milliseconds()}
            ],
            {reply, Msg}
    end.

%% 群聊发送消息
group_dialog(Id, CurrentUid, Data) ->
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
        {<<"type">>,<<"GROUP">>},
        {<<"from">>, hashids_translator:uid_encode(CurrentUid)},
        {<<"to">>, Gid},
        {<<"payload">>, proplists:get_value(<<"payload">>, Data)},
        {<<"created_at">>, proplists:get_value(<<"created_at">>, Data)},
        {<<"server_ts">>, NowTs}
    ],
    % ?LOG(Msg),
    Msg2 = jsx:encode(Msg),
    _UidsOnline = lists:filtermap(fun(Uid) ->
        message_ds:send(Uid, Msg2)
    end, Uids),
    % 存储消息
    group_msg_ds:write_msg(NowTs, Id, Msg2, CurrentUid, Uids, ToGID),
    ok.

%% 系统消息
system(_Id, _CurrentUid, _Data) ->
    ok.
