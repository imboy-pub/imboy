-module(chat_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include("imboy.hrl").

init(Req0, State) ->
    % ?LOG(State),
    Req1 = case lists:keyfind(action, 1, State) of
        {action, myfriend} ->
            myfriend(Req0, State);
        {action, chat_msgbox} ->
            chat_msgbox(Req0, State);
        {action, online} ->
            online(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

online(Req0, _State) ->
    %% length(chat_store_repo:lookall()).
    Data = chat_store_repo:lookall(),
    % ?LOG(Data),
    % [chat_store_repo:dirty_delete(Uid) || [Uid, _Pid, _] <- Data],
    % Data2 = [[{<<"uid">>, Uid}, {<<"pid">>, list_to_binary(pid_to_list(Pid)) }, {<<"type">>, Type}] || [Uid, Pid, Type] <- Data],
    Msg = io_lib:format("在线总人数: ~p", [length(Data)]),
    resp_json_dto:success(Req0, [], Msg).


myfriend(Req0, State) ->
    %%
    {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
    Mine = user_ds:find_by_id(CurrentUid),
    Friend = friend_as:category_friend(CurrentUid),
    Group = group_as:user_group(CurrentUid),
    Data = chat_myfriend_aas:data(Mine, Friend, Group),
    % ?LOG(Data),
    resp_json_dto:success(Req0, Data, "操作成功.").

chat_msgbox(Req0, State) ->
    %%
    {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
    Data = [
        {<<"mine">>, [
            {<<"id">>, CurrentUid}
            , {<<"username">>, <<"leeyi">>}
            , {<<"avatar">>, <<"/static/image/default_avatar_male_180.gif">>}
            , {<<"sign">>, <<"">>}
            , {<<"status">>, <<"1">>}
        ]}
        , {<<"friend">>, [
            [
            {<<"id">>, 2}
            , {<<"username">>, <<"leeyi2">>}
            , {<<"avatar">>, <<"/static/image/default_avatar_male_180.gif">>}
            , {<<"sign">>, <<"">>}
            , {<<"status">>, <<"1">>}
            ]
        ]}
    ],
    resp_json_dto:success(Req0, Data, "操作成功.").

