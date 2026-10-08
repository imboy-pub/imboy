-module(micro_community_handler).
-export([init/2, handle_action/3]).

init(Req, State) ->
    Reply = handle_action(maps:get(action, State, discussion), Req, State),
    {ok, Reply, State}.

handle_action(Action, Req, State) ->
    Uid = maps:get(current_uid, State, 0),
    Cid = cowboy_req:binding(channel_id, Req),
    Result = dispatch(Action, cowboy_req:method(Req), Req, Uid, Cid),
    case Result of
        {ok, Data} -> elib_response:success(Req, Data);
        {error, {Code, Msg}} -> elib_response:error(Req, Msg, Code)
    end.

dispatch(candidates, <<"GET">>, Req, Uid, Cid) ->
    Params = cowboy_req:parse_qs(Req),
    micro_community_binding_logic:candidates(
        Uid,
        Cid,
        number(proplists:get_value(<<"page">>, Params, <<"1">>)),
        number(proplists:get_value(<<"size">>, Params, <<"10">>))
    );
dispatch(discussion, <<"GET">>, _, Uid, Cid) ->
    micro_community_binding_logic:get(Uid, Cid);
dispatch(discussion, <<"DELETE">>, _, Uid, Cid) ->
    micro_community_binding_logic:set(Uid, Cid, null);
dispatch(discussion, <<"PUT">>, Req, Uid, Cid) ->
    try
        Payload = elib_param:post(Req),
        micro_community_binding_logic:set(
            Uid, Cid, maps:get(<<"discussion_group_id">>, Payload, invalid)
        )
    catch
        _:_ -> {error, {400, <<"Invalid discussion binding input">>}}
    end;
dispatch(_, _, _, _, _) ->
    {error, {405, <<"Method not allowed">>}}.

number(Bin) ->
    try
        binary_to_integer(Bin)
    catch
        _:_ -> 0
    end.
