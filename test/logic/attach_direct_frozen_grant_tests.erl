-module(attach_direct_frozen_grant_tests).
-include_lib("eunit/include/eunit.hrl").

frozen_grant_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [?_test(frozen_location_used()), ?_test(unverified_location_aborts())]
    end}.

setup() ->
    [meck:new(M, [passthrough, no_link]) || M <- [elib_pg, attachment_storage, elib_s3_sign]],
    Key = <<"tenant/object">>,
    Hash = binary:copy(<<"a">>, 64),
    put(test_key, Key),
    put(test_hash, Hash),
    meck:expect(elib_pg, query, fun(_Sql, [Key0]) when Key0 =:= Key ->
        {ok, [
            #{
                <<"object_key">> => Key,
                <<"creator_user_id">> => 0,
                <<"session_state">> => <<"registered">>,
                <<"direct_state">> => <<"DECLARED">>,
                <<"direct_expected">> => #{
                    <<"sha256">> => Hash,
                    <<"size_bytes">> => 10,
                    <<"mime">> => <<"text/plain">>
                },
                <<"storage_driver">> => <<"garage">>,
                <<"storage_namespace">> => <<"a">>,
                <<"storage_container">> => <<"bucket-a">>,
                <<"storage_locator_sha256">> => binary:copy(<<"b">>, 64)
            }
        ]}
    end),
    meck:expect(elib_pg, query, fun(_Conn, _Sql, _Args) -> {ok, [#{<<"no">> => 1}]} end),
    meck:expect(elib_pg, with_physical_guard_retry_tx, fun(F) ->
        try
            F(conn)
        catch
            throw:{abort_tx, R} -> {error, R}
        end
    end),
    meck:expect(elib_s3_sign, presign_signed_url_at, fun(
        _Method, Endpoint, Container, _Key, _Headers, _Now, _Ttl, Config
    ) ->
        put(signed_location, {Endpoint, Container, Config}),
        <<"https://signed.invalid">>
    end),
    ok.

cleanup(_) ->
    meck:unload([elib_pg, attachment_storage, elib_s3_sign]),
    erase(test_key),
    erase(test_hash),
    erase(signed_location),
    ok.

frozen_location_used() ->
    Config = #{
        endpoint => <<"https://frozen.invalid">>,
        access_key => binary:encode_hex(crypto:strong_rand_bytes(12)),
        secret_key => binary:encode_hex(crypto:strong_rand_bytes(24))
    },
    meck:expect(attachment_storage, frozen_location, fun(_, _) ->
        {ok, #{driver => garage, container => <<"bucket-a">>, garage_config => Config}}
    end),
    {ok, #{attempt_id := 1}} = attach_direct_logic:grant(
        <<"tenant/object">>, binary:copy(<<"a">>, 64)
    ),
    ?assertEqual({<<"https://frozen.invalid">>, <<"bucket-a">>, Config}, get(signed_location)).

unverified_location_aborts() ->
    meck:expect(attachment_storage, frozen_location, fun(_, _) -> {error, locator_mismatch} end),
    ?assertEqual(
        {error, {location_unverified, locator_mismatch}},
        attach_direct_logic:grant(<<"tenant/object">>, binary:copy(<<"a">>, 64))
    ).
