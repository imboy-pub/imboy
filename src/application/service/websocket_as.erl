-module (websocket_as).
%%%
% websocket_as 是 user application service 缩写
%%%
% -export ([subprotocol/1]).
-export ([dialog/2]).

-include("imboy.hrl").

%% 对聊发送消息
dialog(CurrentUid, Data) ->
    {<<"to_id">>, ToId} = lists:keyfind(<<"to_id">>, 1, Data),
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
            ?LOG(["CurrentUid", CurrentUid, "ToId", ToId, "ToPid:", ToPid, jsx:encode(Msg)]),
            erlang:start_timer(1, ToPid, jsx:encode(Msg));
        false ->
            % 存储离线消息
            ?LOG(["write offline message ", ToId]),
            chat_message_ds:write_msg(jsx:encode(Msg), CurrentUid, ToId)
    end,
    ok.
