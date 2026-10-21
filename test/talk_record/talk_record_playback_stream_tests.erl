-module(talk_record_playback_stream_tests).
-include_lib("eunit/include/eunit.hrl").

range_test() ->
    F = fun talk_record_playback_handler:byte_range/2,
    ?assertEqual({ok, 200, 0, 100}, F(undefined, 100)),
    ?assertEqual({ok, 206, 10, 10}, F(<<"bytes=10-19">>, 100)),
    ?assertEqual({ok, 206, 95, 5}, F(<<"bytes=95-999">>, 100)),
    ?assertEqual({ok, 206, 90, 10}, F(<<"bytes=90-">>, 100)),
    ?assertEqual({ok, 206, 95, 5}, F(<<"bytes=-5">>, 100)),
    ?assertEqual({ok, 206, 0, 100}, F(<<"bytes=-999">>, 100)),
    lists:foreach(
        fun(R) -> ?assertEqual({error, range_not_satisfiable}, F(R, 100)) end,
        [
            <<"bytes=100-">>,
            <<"bytes=9-8">>,
            <<"bytes=0-1,4-5">>,
            <<"bytes=-0">>,
            <<"bytes=+1-2">>,
            <<"items=1-2">>,
            <<"bytes=1-2-3">>
        ]
    ).

identity_acl_media_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        ?_test(begin
            Ticket = ticket(),
            ?assertMatch({ok, _}, talk_record_grant_playback:verify_ticket(Ticket, 700001, 800001)),
            Calls = meck:num_calls(talk_record_repo, find_visible, '_'),
            ?assertEqual(
                {error, playback_ticket_invalid},
                talk_record_grant_playback:verify_ticket(Ticket, 700002, 800001)
            ),
            ?assertEqual(
                {error, playback_ticket_invalid},
                talk_record_grant_playback:verify_ticket(Ticket, 700001, 800002)
            ),
            ?assertEqual(Calls, meck:num_calls(talk_record_repo, find_visible, '_')),
            meck:expect(talk_record_repo, find_visible, fun(_, _) -> {error, not_found} end),
            ?assertEqual(
                {error, not_found}, talk_record_grant_playback:verify_ticket(Ticket, 700001, 800001)
            ),
            meck:expect(talk_record_repo, find_visible, fun(_, _) ->
                {ok, row(2, <<"available">>)}
            end),
            ?assertEqual(
                {error, acl_stale}, talk_record_grant_playback:verify_ticket(Ticket, 700001, 800001)
            ),
            meck:expect(talk_record_repo, find_visible, fun(_, _) ->
                {ok, row(1, <<"assembling">>)}
            end),
            ?assertMatch(
                {error, _}, talk_record_grant_playback:verify_ticket(Ticket, 700001, 800001)
            ),
            Expired = imboy_jwt:sign(
                #{
                    <<"aud">> => <<"talk_record_playback">>,
                    <<"sub">> => <<"talk_record_playback">>,
                    <<"rid">> => 800001,
                    <<"uid">> => 700001,
                    <<"acl">> => 1,
                    <<"iat">> => os:system_time(second) - 120,
                    <<"exp">> => os:system_time(second) - 60
                },
                key()
            ),
            ?assertMatch(
                {error, _}, talk_record_grant_playback:verify_ticket(Expired, 700001, 800001)
            )
        end)
    end}.

stream_bytes_and_revocation_test_() ->
    {setup, fun stream_setup/0, fun stream_cleanup/1, fun(_) ->
        ?_test(begin
            put(stream_bytes, []),
            put(revoke_after_chunk, false),
            T = ticket(),
            Base = #{
                method => <<"GET">>,
                bindings => #{id => <<"800001">>},
                qs => <<"ticket=", T/binary>>,
                headers => #{<<"range">> => <<"bytes=2-5">>}
            },
            {ok, Resp, _} = talk_record_playback_handler:init(Base, #{current_uid => 700001}),
            ?assertEqual(206, maps:get(status, Resp)),
            ?assertEqual(
                <<"bytes 2-5/150000">>,
                maps:get(<<"content-range">>, maps:get(response_headers, Resp))
            ),
            ?assertEqual(<<"nthe">>, iolist_to_binary(lists:reverse(get(stream_bytes)))),
            Reads = meck:num_calls(attachment_derived_artifact, with_range, '_'),
            put(stream_bytes, []),
            {ok, Head, _} = talk_record_playback_handler:init(Base#{method => <<"HEAD">>}, #{
                current_uid => 700001
            }),
            ?assertEqual(206, maps:get(status, Head)),
            ?assertEqual([], get(stream_bytes)),
            ?assertEqual(Reads, meck:num_calls(attachment_derived_artifact, with_range, '_')),
            {ok, Denied, _} = talk_record_playback_handler:init(Base, #{current_uid => 700002}),
            ?assertEqual(403, maps:get(status, Denied)),
            put(revoke_after_chunk, true),
            ?assertExit(
                playback_authorization_changed,
                talk_record_playback_handler:init(
                    Base#{headers => #{}}, #{current_uid => 700001}
                )
            ),
            ?assertEqual(65536, byte_size(iolist_to_binary(get(stream_bytes))))
        end)
    end}.

stream_setup() ->
    Prev = setup(),
    put(stream_bytes, []),
    put(revoke_after_chunk, false),
    ok = meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, with_conn, fun(F) -> F(test_connection) end),
    ok = meck:new(talk_record_assembly_logic, [passthrough, no_link]),
    meck:expect(talk_record_assembly_logic, find_tx, fun(_, 800001) ->
        {ok, #{<<"audio_attachment_id">> => 900001}}
    end),
    ok = meck:new(attachment_derived_artifact, [passthrough, no_link]),
    meck:expect(attachment_derived_artifact, metadata, fun(900001) ->
        {ok, #{<<"size">> => 150000}}
    end),
    meck:expect(attachment_derived_artifact, with_range, fun(900001, Offset, Length, F) ->
        attachment_upload_spool:with_source(
            binary:part(binary:copy(<<"synthetic-audio">>, 10000), Offset, Length), F
        )
    end),
    ok = meck:new(cowboy_req, [passthrough, unstick, no_link]),
    meck:expect(cowboy_req, reply, fun(Status, H, _B, Req) ->
        Req#{status => Status, response_headers => H}
    end),
    meck:expect(cowboy_req, stream_reply, fun(Status, H, Req) ->
        Req#{status => Status, response_headers => H}
    end),
    meck:expect(cowboy_req, stream_body, fun(B, _Fin, _Req) ->
        put(stream_bytes, [B | get(stream_bytes)]),
        case get(revoke_after_chunk) of
            true ->
                meck:expect(
                    talk_record_repo,
                    find_visible,
                    fun(_, _) -> {error, not_found} end
                );
            _ ->
                ok
        end,
        ok
    end),
    Prev.
stream_cleanup(Prev) ->
    lists:foreach(fun meck:unload/1, [
        cowboy_req, attachment_derived_artifact, talk_record_assembly_logic, elib_pg
    ]),
    erase(stream_bytes),
    erase(revoke_after_chunk),
    cleanup(Prev).

setup() ->
    Prev = application:get_env(imboy, jwt_key),
    application:set_env(imboy, jwt_key, key()),
    ok = meck:new(talk_record_repo, [passthrough, no_link]),
    meck:expect(talk_record_repo, find_visible, fun(_, _) -> {ok, row(1, <<"available">>)} end),
    Prev.
cleanup(Prev) ->
    meck:unload(talk_record_repo),
    case Prev of
        {ok, V} -> application:set_env(imboy, jwt_key, V);
        undefined -> application:unset_env(imboy, jwt_key)
    end.
row(Acl, Media) -> #{<<"acl_revision">> => Acl, <<"media_status">> => Media}.
key() -> <<"synthetic-playback-signing-key-000000000001">>.
ticket() ->
    imboy_jwt:sign(
        #{
            <<"aud">> => <<"talk_record_playback">>,
            <<"sub">> => <<"talk_record_playback">>,
            <<"rid">> => 800001,
            <<"uid">> => 700001,
            <<"acl">> => 1,
            <<"iat">> => os:system_time(second),
            <<"exp">> => os:system_time(second) + 60
        },
        key()
    ).
