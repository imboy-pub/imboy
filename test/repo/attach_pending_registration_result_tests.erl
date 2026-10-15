-module(attach_pending_registration_result_tests).
-include_lib("eunit/include/eunit.hrl").

registration_result_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
        [fun original_identity/0, fun original_window/0,
         fun transaction_identity/0, fun missing_readback/0]}.

setup() ->
    meck:new(elib_pg, [non_strict, no_link]),
    Row = #{<<"object_key">> => <<"original-key">>, <<"bucket">> => <<"original-bucket">>,
            <<"scope">> => <<"private">>, <<"session_state">> => <<"registered">>,
            <<"client_upload_id">> => cid(), <<"confirmed_attachment_id">> => null,
            <<"deadline">> => 1800000000, <<"expires_at">> => 1799999000},
    meck:expect(elib_pg, query, fun(Sql, Args) -> query(Sql, Args, Row) end),
    meck:expect(elib_pg, query, fun(_Conn, Sql, Args) -> query(Sql, Args, Row) end).

cleanup(_) -> meck:unload(elib_pg).

cid() -> <<"12345678-1111-4222-8333-123456789012">>.

query(Sql, _Args, Session) ->
    case binary:match(iolist_to_binary(Sql), <<"fn_v5_register_pending">>) of
        nomatch ->
            case Session of
                missing -> {ok, []};
                Row -> {ok, [Row]}
            end;
        _ -> {ok, [#{<<"out_object_key">> => <<"original-key">>,
                     <<"out_session_state">> => <<"registered">>,
                     <<"out_client_upload_id">> => cid()}]}
    end.

register() ->
    attach_pending_repo:register_v5(<<"new-random-key">>, <<"bucket">>, <<"private">>,
                                    7, cid(), #{<<"filename">> => <<"a.png">>}).

original_identity() ->
    {ok, Session} = register(),
    ?assertEqual(<<"original-key">>, maps:get(object_key, Session)),
    ?assertEqual(<<"original-bucket">>, maps:get(bucket, Session)).

original_window() ->
    {ok, Session} = register(),
    ?assertEqual(1800000000, maps:get(deadline, Session)),
    ?assertEqual(1799999000, maps:get(expires_at, Session)).

transaction_identity() ->
    {ok, Session} = attach_pending_repo:register_v5_tx(
        self(), <<"new-key">>, <<"bucket">>, <<"enterprise">>, 0, cid(),
        #{<<"subject">> => #{<<"kind">> => <<"application">>}}),
    ?assertEqual(<<"original-key">>, maps:get(object_key, Session)),
    ?assertEqual(1799999000, maps:get(expires_at, Session)).

missing_readback() ->
    meck:expect(elib_pg, query, fun(Sql, Args) -> query(Sql, Args, missing) end),
    ?assertEqual({error, registration_not_returned}, register()).
