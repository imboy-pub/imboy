%% Revisit completed deletion tombstones for writes that arrive after DELETE.
-module(attachment_cross_rescan).
-export([run_batch/2]).

run_batch(Conn, Delete) when is_function(Delete, 1) -> loop(Conn, Delete, 100, 0).

loop(_, _, 0, Count) ->
    {ok, Count};
loop(Conn, Delete, Left, Count) ->
    Token = token(),
    case elib_pg:query(Conn, <<"SELECT * FROM fn_cross_rescan_claim($1)">>, [Token]) of
        {ok, []} ->
            {ok, Count};
        {ok, [Row]} ->
            case run_one(Conn, Delete, Row, Token) of
                ok -> loop(Conn, Delete, Left - 1, Count + 1);
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

run_one(Conn, Delete, Row, Token) ->
    Key = maps:get(<<"out_physical_key">>, Row),
    Frozen = #{
        <<"storage_driver">> => maps:get(<<"out_driver">>, Row),
        <<"storage_namespace">> => maps:get(<<"out_namespace">>, Row),
        <<"storage_container">> => maps:get(<<"out_container">>, Row),
        <<"storage_locator_sha256">> => maps:get(<<"out_locator">>, Row),
        <<"storage_physical_key">> => Key
    },
    Result =
        case attachment_storage:frozen_location(Key, Frozen) of
            {ok, Location} when is_map(Location) -> Delete(Location);
            _ -> {error, location_unverified}
        end,
    Ok = Result =:= ok orelse Result =:= {error, not_found},
    case
        elib_pg:query(
            Conn,
            <<"SELECT fn_cross_rescan_ack($1,$2,$3) AS state">>,
            [maps:get(<<"out_id">>, Row), Token, Ok]
        )
    of
        {ok, [#{<<"state">> := <<"ready">>}]} when Ok -> ok;
        {ok, [#{<<"state">> := <<"ready">>}]} -> {error, delete_failed};
        Other -> {error, {ack_failed, Other}}
    end.

token() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.
