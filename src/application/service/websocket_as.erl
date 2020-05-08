-module (websocket_as).
%%%
% websocket_as 是 user application service 缩写
%%%
% -export ([subprotocol/1]).
-export ([dialog/2]).
-export ([group_dialog/2]).

-include("imboy.hrl").

-spec dialog(integer(), Data::list()) -> ok | {reply, Msg::list()}.
-spec group_dialog(integer(), Data::list()) -> ok | {reply, Msg::list()}.

%% 单聊发送消息
dialog(CurrentUid, Data) ->
    {<<"to_id">>, ToId} = lists:keyfind(<<"to_id">>, 1, Data),
    case friend_ds:is_friend(CurrentUid, ToId) of
        true ->
            {<<"content">>, Content} = lists:keyfind(<<"content">>, 1, Data),
            Msg = [
                {<<"type">>,<<"dialog">>},
                {<<"from_id">>, CurrentUid},
                {<<"to_id">>, ToId},
                {<<"content">>, Content},
                {<<"timestamp">>, imboy_func:milliseconds()}
            ],
            case user_ds:is_online(ToId) of
                {ToId, ToPid, _Type} ->
                    erlang:start_timer(1, ToPid, jsx:encode(Msg));
                false ->
                    % 存储离线消息
                    chat_message_ds:write_msg(jsx:encode(Msg), CurrentUid, ToId)
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
    {<<"to_id">>, Gid} = lists:keyfind(<<"to_id">>, 1, Data),
    Column = <<"`user_id`">>,
    {ok, _ColumnLi, Members} = group_member_repo:find_by_group_id(Gid, Column),
    Uids = [Uid || [Uid] <- Members, Uid /= CurrentUid],
    % Uids.
    Data2 = jsx:encode(Data),
    UidsOnline = lists:filtermap(fun(Uid) ->
        case user_ds:is_online(Uid) of
            {Uid, Pid, Type}->
                % ?LOG([{Uid, Pid, Type}, Data2]),
                % 给在线群成员发送消息
                erlang:start_timer(80, Pid, Data2),
                {true, {Uid, Pid, Type}};
            false ->
                false
        end
    end, Uids),
    % ?LOG(UidsOnline),
    % 存储离线消息
    UidsOffline = [Uid || Uid <- Uids, lists:keymember(Uid, 1, UidsOnline) == false],
    % ?LOG(UidsOffline),
    case UidsOffline of
        [] ->
            ok;
        UidsOffline2 ->
            group_chat_message_ds:write_msg(Data2, CurrentUid, UidsOffline2, Gid),
            ok
    end.
