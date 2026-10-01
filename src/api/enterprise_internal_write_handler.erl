-module(enterprise_internal_write_handler).
-export([write/5]).

write(Req, Ctx, Operation, IdentityId, Logic) ->
    Key = cowboy_req:header(<<"idempotency-key">>, Req),
    case enterprise_internal_idempotency:valid_key(Key) of
        false ->
            enterprise_internal_error:reply(Req, <<"invalid_request">>);
        true ->
            case cowboy_req:read_body(Req, #{length => 65536}) of
                {ok, Raw, Req1} when byte_size(Raw) =< 65536 ->
                    Body =
                        try
                            jsone:decode(Raw)
                        catch
                            _:_ -> invalid
                        end,
                    Digest = enterprise_internal_idempotency:request_digest(
                        cowboy_req:method(Req1), cowboy_req:path(Req1), Raw
                    ),
                    case {is_map(Body), Digest} of
                        {true, {ok, Hash}} ->
                            Result = elib_pg:with_tx(fun(Conn) ->
                                Logic:write_tx(
                                    Conn, Ctx, Operation, Key, Hash, IdentityId, Body
                                )
                            end),
                            reply(Req1, Result);
                        _ ->
                            enterprise_internal_error:reply(Req1, <<"invalid_request">>)
                    end;
                {_, _, Req1} ->
                    enterprise_internal_error:reply(Req1, <<"invalid_request">>)
            end
    end.

reply(Req, {ok, Map}) -> json(Req, 200, jsone:encode(Map), []);
reply(Req, {ok, Status, Body, Headers}) -> json(Req, Status, Body, Headers);
reply(Req, {rollback, {internal_error_code, Code}}) -> enterprise_internal_error:reply(Req, Code);
reply(Req, _) -> enterprise_internal_error:reply(Req, <<"internal_error">>).
json(Req, Status, Body, Headers) ->
    cowboy_req:reply(
        Status, maps:from_list([{<<"content-type">>, <<"application/json">>} | Headers]), Body, Req
    ).
