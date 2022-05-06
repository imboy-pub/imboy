-module(handler_group).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").


init(Req0, State) ->
    ?LOG(State),
    Req1 =
        case lists:keyfind(action, 1, State) of
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
            dto_resp_json:error(Req0, "group id 必须");
        #{id := Gid} ->
            Members = logic_group:member(Gid),
            Data = aas_group_member:data(Members),
            dto_resp_json:success(Req0, Data, "操作成功.")
    end.
