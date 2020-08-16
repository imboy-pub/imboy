-module (websocket_as).
%%%
% websocket_as 是 user application service 缩写
%%%
% -export ([subprotocol/1]).
-export ([dialog/3]).
-export ([group_dialog/3]).
-export ([system/3]).

-include("common.hrl").

-spec dialog(binary(), integer(), Data::list()) -> ok | {reply, Msg::list()}.
-spec group_dialog(binary(), integer(), Data::list()) -> ok | {reply, Msg::list()}.

%% 单聊发送消息
dialog(MsgMd5, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    ToId = hashids_tl:uid_decode(To),
    % CurrentUid = hashids_tl:uid_decode(From),
    ?LOG([CurrentUid, ToId, Data]),
    case friend_ds:is_friend(CurrentUid, ToId) of
        true ->
            Payload = proplists:get_value(<<"payload">>, Data),
            NowTs = dt_util:milliseconds(),
            Msg = [
                {<<"type">>,<<"C2C">>},
                {<<"from">>, hashids_tl:uid_encode(CurrentUid)},
                {<<"to">>, To},
                {<<"payload">>, Payload},
                {<<"server_ts">>, NowTs}
            ],
            Msg2 = jsx:encode(Msg),
            % 存储消息
            dialog_msg_ds:write_msg(NowTs, MsgMd5, Msg2, CurrentUid, ToId),
            case user_ds:is_offline(ToId) of
                {ToPid, _Uid, _Type} ->
                    erlang:start_timer(1, ToPid, Msg2);
                true ->
                    ok
            end,
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
group_dialog(MsgMd5, CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = hashids_tl:uid_decode(Gid),
    % TODO check is group member
    Column = <<"`user_id`">>,
    {ok, _ColumnLi, Members} = group_member_repo:find_by_group_id(ToGID, Column),
    Uids = [Uid || [Uid] <- Members, Uid /= CurrentUid],
    % Uids.
    Payload = proplists:get_value(<<"payload">>, Data),
    NowTs = dt_util:milliseconds(),
    Msg = [
        {<<"type">>,<<"GROUP">>},
        {<<"from">>, hashids_tl:uid_encode(CurrentUid)},
        {<<"to">>, Gid},
        {<<"payload">>, Payload},
        {<<"server_ts">>, NowTs}
    ],
    % ?LOG(Msg),
    Msg2 = jsx:encode(Msg),
    _UidsOnline = lists:filtermap(fun(Uid) ->
        case user_ds:is_offline(Uid) of
            {Pid, _Uid, Type}->
                % ?LOG([{Uid, Pid, Type}, Msg2]),
                % 给在线群成员发送消息
                erlang:start_timer(80, Pid, Msg2),
                {true, {Uid, Pid, Type}};
            true ->
                false
        end
    end, Uids),
    % 存储消息
    group_msg_ds:write_msg(NowTs, MsgMd5, Msg2, CurrentUid, Uids, ToGID),
    ok.

%% 系统消息
system(_MsgMd5, _CurrentUid, _Data) ->
    ok.
