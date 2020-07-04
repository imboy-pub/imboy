-module (websocket_as).
%%%
% websocket_as 是 user application service 缩写
%%%
% -export ([subprotocol/1]).
-export ([dialog/2]).
-export ([group_dialog/2]).
-export ([system/2]).

-include("imboy.hrl").

-spec dialog(integer(), Data::list()) -> ok | {reply, Msg::list()}.
-spec group_dialog(integer(), Data::list()) -> ok | {reply, Msg::list()}.

%% 单聊发送消息
dialog(CurrentUid, Data) ->
    ToId = proplists:get_value(<<"to">>, Data),
    ?LOG([CurrentUid, ToId, Data]),
    case friend_ds:is_friend(CurrentUid, ToId) of
        true ->
            Payload = proplists:get_value(<<"payload">>, Data),
            Msg = [
                {<<"type">>,<<"C2C">>},
                {<<"from">>, CurrentUid},
                {<<"to">>, ToId},
                {<<"payload">>, Payload},
                {<<"server_ts">>, imboy_func:milliseconds()}
            ],
            case user_ds:is_offline(ToId) of
                {ToPid, _Uid, _Type} ->
                    erlang:start_timer(1, ToPid, jsx:encode(Msg));
                true ->
                    % 存储离线消息
                    dialog_msg_ds:write_msg(jsx:encode(Msg), CurrentUid, ToId)
            end,
            ok;
        false ->
            Msg = [
                {<<"type">>,<<"error">>},
                {<<"code">>, 1},
                {<<"msg">>, unicode:characters_to_binary("非好友关系，没法单聊")},
                {<<"timestamp">>, imboy_func:milliseconds()}
            ],
            {reply, Msg}
    end.

%% 群聊发送消息
group_dialog(CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    % TODO check is group member
    Column = <<"`user_id`">>,
    {ok, _ColumnLi, Members} = group_member_repo:find_by_group_id(Gid, Column),
    Uids = [Uid || [Uid] <- Members, Uid /= CurrentUid],
    % Uids.
    Data2 = jsx:encode(Data),
    UidsOnline = lists:filtermap(fun(Uid) ->
        case user_ds:is_offline(Uid) of
            {Pid, _Uid, Type}->
                % ?LOG([{Uid, Pid, Type}, Data2]),
                % 给在线群成员发送消息
                erlang:start_timer(80, Pid, Data2),
                {true, {Uid, Pid, Type}};
            true ->
                false
        end
    end, Uids),
    % 存储离线消息
    UidsOffline = [Uid || Uid <- Uids, lists:keymember(Uid, 1, UidsOnline) == false],
    % ?LOG(UidsOffline),
    case UidsOffline of
        [] ->
            ok;
        UidsOffline2 ->
            group_msg_ds:write_msg(Data2, CurrentUid, UidsOffline2, Gid),
            ok
    end.

%% 系统消息
system(_CurrentUid, _Data) ->
    ok.
