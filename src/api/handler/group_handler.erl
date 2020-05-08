-module(group_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include("imboy.hrl").

init(Req0, State) ->
    ?LOG(State),
    Req1 = case lists:keyfind(action, 1, State) of
        {action, member} ->
            member(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

member(Req0, _State) ->
    %%
    case cowboy_req:match_qs([{id, [], undefined}], Req0) of
        #{id := undefined} ->
            resp_json_dto:error(Req0, "group id 必须");
        #{id := Gid} ->
            Members = group_as:member(Gid),
            Data = group_member_aas:data(Members),
            resp_json_dto:success(Req0, Data, "操作成功.")
    end.
