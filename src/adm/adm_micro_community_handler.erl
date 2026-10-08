-module(adm_micro_community_handler).
-export([init/2, handle_action/3]).

init(Req, State) ->
    Reply = handle_action(maps:get(action, State), Req, State),
    {ok, Reply, State}.

handle_action(Action, Req, State) ->
    case adm_acl:ensure_permission(State, <<"channels:update">>, Req) of
        {error, Reply} ->
            Reply;
        ok ->
            case group_permission(Action, State, Req) of
                {error, Reply} ->
                    Reply;
                ok ->
                    response(
                        Req,
                        dispatch(
                            Action,
                            cowboy_req:method(Req),
                            Req,
                            maps:get(adm_user_id, State, 0),
                            cowboy_req:binding(channel_id, Req)
                        )
                    )
            end
    end.

group_permission(essence, _, _) -> ok;
group_permission(_, State, Req) -> adm_acl:ensure_permission(State, <<"groups:update">>, Req).

dispatch(discussion, <<"GET">>, _, AdminId, Cid) ->
    adm_micro_community_logic:get(AdminId, Cid);
dispatch(discussion, <<"DELETE">>, _, AdminId, Cid) ->
    adm_micro_community_logic:set(AdminId, Cid, null);
dispatch(discussion, <<"PUT">>, Req, AdminId, Cid) ->
    with_body(Req, fun(Body) ->
        adm_micro_community_logic:set(
            AdminId,
            Cid,
            maps:get(<<"discussion_group_id">>, Body, invalid)
        )
    end);
dispatch(candidates, <<"GET">>, Req, AdminId, Cid) ->
    Qs = cowboy_req:parse_qs(Req),
    adm_micro_community_logic:candidates(
        AdminId,
        Cid,
        number(proplists:get_value(<<"page">>, Qs, <<"1">>)),
        number(proplists:get_value(<<"size">>, Qs, <<"10">>))
    );
dispatch(essence, <<"PUT">>, Req, AdminId, Cid) ->
    with_body(Req, fun(Body) ->
        adm_micro_community_logic:essence(
            AdminId,
            Cid,
            cowboy_req:binding(message_id, Req),
            maps:get(<<"is_essence">>, Body, invalid)
        )
    end);
dispatch(_, _, _, _, _) ->
    {error, {405, <<"Method not allowed">>}}.

with_body(Req, Fun) ->
    try
        Fun(elib_param:post(Req))
    catch
        _:_ -> {error, {400, <<"Invalid request body">>}}
    end.
number(V) ->
    try
        binary_to_integer(V)
    catch
        _:_ -> 0
    end.
response(Req, {ok, Data}) -> elib_response:success(Req, Data);
response(Req, {error, {Code, Message}}) -> elib_response:error(Req, Message, Code).
