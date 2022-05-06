-module(handler_conversation).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").


init(Req0, State) ->
    % ?LOG(State),
    Req1 =
        case lists:keyfind(action, 1, State) of
            {action, online} ->
                online(Req0, State);
            {action, mine} ->
                mine(Req0, State);
            {action, msgbox} ->
                chat_msgbox(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


online(Req0, _State) ->
    List = repo_chat_store:lookall(),
    Msg = io_lib:format("在线总人数: ~p", [length(List)]),
    Res = cowboy_req:match_qs([{type, [], undefined}], Req0),
    % ?LOG(Res),
    List2 = case Res of
        #{type := <<"list">>} ->
            [[{<<"pid">>, list_to_binary(pid_to_list(Pid))},
              {<<"uid">>, Uid},
              {<<"did">>, DID}] || [Pid, Uid, DID] <- List];
        #{type := _Type} ->
            []
    end,
    dto_resp_json:success(Req0, List2, Msg).


mine(Req0, State) ->
    #{last_server_ts := ServerTS} =
        cowboy_req:match_qs([{last_server_ts, [], undefined}], Req0),
    % ?LOG(ServerTS),
    CurrentUid = proplists:get_value(current_uid, State),
    List = ds_msg_c2c:read_msg(CurrentUid, 1000, ServerTS),
    % ?LOG(["mine_list", List]),
    List2 = aas_conversation_mine:data(List),
    dto_resp_json:success(Req0, List2).


chat_msgbox(Req0, State) ->
    %%
    CurrentUid = proplists:get_value(current_uid, State),
    Data = [{<<"mine">>,
             [{<<"id">>, CurrentUid},
              {<<"account">>, <<"leeyi">>},
              {<<"avatar">>,
               <<"/static/image/default_avatar_male_180.gif">>},
              {<<"sign">>, <<"">>},
              {<<"status">>, <<"1">>}]},
            {<<"friend">>,
             [[{<<"id">>, 2},
               {<<"account">>, <<"leeyi2">>},
               {<<"avatar">>,
                <<"/static/image/default_avatar_male_180.gif">>},
               {<<"sign">>, <<"">>},
               {<<"status">>, <<"1">>}]]}],
    dto_resp_json:success(Req0, Data, "操作成功.").
