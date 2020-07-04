-module(chat_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include("imboy.hrl").

init(Req0, State) ->
    % ?LOG(State),
    Req1 = case lists:keyfind(action, 1, State) of
        {action, chat_msgbox} ->
            chat_msgbox(Req0, State);
        {action, online} ->
            online(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

online(Req0, _State) ->
    List = chat_store_repo:lookall(),
    Msg = io_lib:format("在线总人数: ~p", [length(List)]),
    Res = cowboy_req:match_qs([{type, [], undefined}], Req0),
    % ?LOG(Res),
    List2 = case Res of
        #{type := <<"list">>} ->
            [[{<<"pid">>, list_to_binary(pid_to_list(Pid))}, {<<"uid">>, Uid}, {<<"type">>, Type}] || [Pid, Uid, Type] <- List];
        #{type := _Type} ->
            []
    end,
    resp_json_dto:success(Req0, List2, Msg).

chat_msgbox(Req0, State) ->
    %%
    CurrentUid = proplists:get_value(current_uid, State),
    Data = [
        {<<"mine">>, [
            {<<"id">>, CurrentUid}
            , {<<"account">>, <<"leeyi">>}
            , {<<"avatar">>, <<"/static/image/default_avatar_male_180.gif">>}
            , {<<"sign">>, <<"">>}
            , {<<"status">>, <<"1">>}
        ]}
        , {<<"friend">>, [
            [
            {<<"id">>, 2}
            , {<<"account">>, <<"leeyi2">>}
            , {<<"avatar">>, <<"/static/image/default_avatar_male_180.gif">>}
            , {<<"sign">>, <<"">>}
            , {<<"status">>, <<"1">>}
            ]
        ]}
    ],
    resp_json_dto:success(Req0, Data, "操作成功.").

