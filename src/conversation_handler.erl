-module(conversation_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("imboy/include/log.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        online ->
            online(Req0, State);
        mine ->
            mine(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

online(Req0, _State) ->
    List = chat_online:lookall(),
    Msg = io_lib:format("在线总人数: ~p", [length(List)]),
    Res = cowboy_req:match_qs([{type, [], undefined}], Req0),
    % ?LOG(Res),
    List2 = case maps:get(type, Res) of
        <<"list">> ->
            [[{<<"pid">>, list_to_binary(pid_to_list(Pid))},
              {<<"uid">>, Uid},
              {<<"dtype">>, DType},
              {<<"did">>, DID}] || [Pid, Uid, DType, DID] <- List];
        _ ->
            []
    end,
    imboy_response:success(Req0, List2, Msg).


mine(Req0, State) ->
    #{last_server_ts := ServerTS} =
        cowboy_req:match_qs([{last_server_ts, [], undefined}], Req0),
    % ?LOG(ServerTS),
    CurrentUid = maps:get(current_uid, State),
    List = msg_c2c_ds:read_msg(CurrentUid, 1000, ServerTS),
    % ?LOG(["mine_list", List]),
    List2 = mine_transfer(List),
    imboy_response:success(Req0, List2).

mine_transfer(List) ->
    [[{<<"id">>, proplists:get_value(<<"id">>, Msg)} |
      jsone:decode(proplists:get_value(<<"payload">>, Msg),
                   [{object_format, proplist}])] || Msg <- List].

