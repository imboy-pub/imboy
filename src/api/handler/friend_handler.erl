-module(friend_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include("imboy.hrl").

init(Req0, State) ->
    ?LOG(State),
    Req1 = case lists:keyfind(action, 1, State) of
        {action, find} ->
            find(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

find(Req0, State) ->
    %%
    {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
    Mine = user_ds:find_by_id(CurrentUid),
    Friends = friend_as:get_by_uid(CurrentUid),
    Data = myfriend_aas:data(Mine, Friends),
    resp_json_dto:success(Req0, Data, "操作成功.").
