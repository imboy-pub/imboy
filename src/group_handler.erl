-module(group_handler).
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        member ->
            member(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.


member(Req0, _State) ->
    %%
    case cowboy_req:match_qs([{id, [], undefined}], Req0) of
        #{id := undefined} ->
            imboy_response:error(Req0, "group id 必须");
        #{id := Gid} ->
            Members = group_logic:member_list(Gid),
            Data = member_transfer(Members),
            imboy_response:success(Req0, Data, "success.")
    end.

member_transfer(Members) ->
    [{<<"list">>, [imboy_hashids:replace_id(M) || M <- Members]}].
