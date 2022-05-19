-module(group_handler).
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
            response:error(Req0, "group id 必须");
        #{id := Gid} ->
            Members = group_logic:member(Gid),
            Data = member_transfer(Members),
            response:success(Req0, Data, "操作成功.")
    end.

member_transfer(Members) ->
    [{<<"list">>, [imboy_hashids:replace_id(M) || M <- Members]}].
