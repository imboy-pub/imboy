-module(chat_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include("imboy.hrl").

init(Req0, State) ->
    ?LOG(State),
    Req1 = case lists:keyfind(action, 1, State) of
        {action, myfriend} ->
            myfriend(Req0, State);
        {action, chat_msgbox} ->
            chat_msgbox(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

myfriend(Req0, State) ->
    %%
    {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
    Mine = user_ds:find_by_id(CurrentUid),
    Friends = friend_as:group_friend(CurrentUid),
    Data = myfriend_aas:data(Mine, Friends),
    ?LOG(Data),
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

