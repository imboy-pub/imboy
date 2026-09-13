-module(organization_handler).

-behavior(cowboy_rest).

-export([init/2, handle_action/3]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    {ok, handle_action(Action, Req0, State), State}.

-spec handle_action(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(collection, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> create(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
handle_action(mine, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> mine(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET">>)
    end;
handle_action(detail, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> detail(Req0, State);
        <<"PATCH">> -> update(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET, PATCH">>)
    end.

create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    respond(Req0, organization_logic:create(Uid, maps:get(<<"name">>, Params, undefined))).

mine(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    respond(Req0, organization_logic:mine(Uid, Page, Size)).

detail(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case positive_binding(organization_id, Req0) of
        {ok, OrgId} ->
            respond(Req0, organization_logic:detail(Uid, OrgId));
        error ->
            elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400)
    end.

update(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    case positive_binding(organization_id, Req0) of
        {ok, OrgId} ->
            respond(
                Req0,
                organization_logic:update(
                    Uid,
                    OrgId,
                    maps:get(<<"name">>, Params, undefined),
                    maps:get(<<"branding">>, Params, undefined),
                    maps:get(<<"settings">>, Params, undefined)
                )
            );
        error ->
            elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400)
    end.

respond(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond(Req0, {error, {Code, Msg}}) ->
    elib_response:error(Req0, Msg, Code).

positive_binding(Name, Req0) ->
    case elib_cnv:safe_to_integer(cowboy_req:binding(Name, Req0)) of
        Id when is_integer(Id), Id > 0 -> {ok, Id};
        _ -> error
    end.

method_not_allowed(Req0, Allow) ->
    cowboy_req:reply(
        405,
        #{<<"allow">> => Allow, <<"content-type">> => <<"text/plain; charset=utf-8">>},
        <<"Method Not Allowed">>,
        Req0
    ).
