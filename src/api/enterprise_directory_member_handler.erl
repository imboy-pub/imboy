-module(enterprise_directory_member_handler).
-export([init/2]).

init(Req, #{enterprise_internal := Ctx} = State) ->
    Reply =
        case cowboy_req:method(Req) of
            <<"GET">> ->
                read(Req, Ctx);
            <<"PATCH">> ->
                enterprise_internal_write_handler:write(
                    cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req),
                    Ctx,
                    update,
                    0,
                    enterprise_directory_member_logic
                );
            _ ->
                enterprise_internal_error:reply(Req, <<"resource_not_found">>)
        end,
    {ok, Reply, State}.

read(Req, Ctx) ->
    case query(Req) of
        {ok, Query} ->
            Result = elib_pg:with_tx(fun(C) ->
                enterprise_directory_member_logic:read_tx(C, Ctx, Query)
            end),
            reply(Req, Result);
        error ->
            enterprise_internal_error:reply(Req, <<"invalid_request">>)
    end.

query(Req) ->
    try cowboy_req:parse_qs(Req) of
        Pairs ->
            Query = maps:from_list(Pairs),
            case length(Pairs) =:= map_size(Query) of
                false ->
                    error;
                true ->
                    {ok, Query}
            end
    catch
        error:_ -> error;
        exit:{request_error, qs, _} -> error
    end.

reply(Req, {ok, Value}) ->
    cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"application/json">>, <<"cache-control">> => <<"no-store">>},
        jsone:encode(Value),
        Req
    );
reply(Req, {rollback, {internal_error_code, Code}}) ->
    enterprise_internal_error:reply(Req, Code);
reply(Req, _) ->
    enterprise_internal_error:reply(Req, <<"internal_error">>).
