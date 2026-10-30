-module(talk_record_playback_parts_tests).
-include_lib("eunit/include/eunit.hrl").

parts_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        ?_test(begin
            {ok, Payload} = talk_record_playback_parts:open(7, 8),
            ?assertEqual(<<"8">>, maps:get(<<"record_id">>, Payload)),
            [P] = maps:get(<<"parts">>, Payload),
            ?assertEqual(0, maps:get(<<"seq">>, P)),
            Url = maps:get(<<"url">>, P),
            [_, Ticket] = binary:split(Url, <<"?ticket=">>),
            ?assertMatch({ok, _}, talk_record_playback_parts:metadata(7, 8, Ticket, 0)),
            ?assertMatch({error, _}, talk_record_playback_parts:metadata(9, 8, Ticket, 0)),
            ?assertMatch({error, _}, talk_record_playback_parts:metadata(7, 9, Ticket, 0)),
            ?assertMatch({error, _}, talk_record_playback_parts:metadata(7, 8, Ticket, 1)),
            ?assertEqual(
                {ok, <<"audio">>},
                talk_record_playback_parts:with_source(
                    7,
                    8,
                    Ticket,
                    0,
                    fun(Source, Size, _) -> attachment_upload_spool:read_at(Source, 0, Size) end
                )
            ),
            meck:expect(attachment_derived_artifact, with_live_spool, fun(22, _, F) ->
                attachment_upload_spool:with_source(<<"other">>, F)
            end),
            ?assertEqual(
                {error, playback_media_unavailable},
                talk_record_playback_parts:with_source(7, 8, Ticket, 0, fun(_, _, _) -> leaked end)
            ),
            meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
            ?assertEqual(
                {error, playback_media_unavailable},
                talk_record_playback_parts:recheck(7, 8, Ticket, 0)
            ),
            meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"id">> => 22}]} end),
            meck:expect(talk_record_assembly_logic, confirmed_parts_tx, fun(_, _) -> {ok, []} end),
            ?assertMatch({error, _}, talk_record_playback_parts:metadata(7, 8, Ticket, 0)),
            ?assertMatch({error, _}, talk_record_playback_parts:open(7, 8)),
            meck:expect(talk_record_content_common, visible_or_404, fun(_, _) ->
                {error, not_found}
            end),
            ?assertMatch({error, _}, talk_record_playback_parts:metadata(7, 8, Ticket, 0)),
            ?assertMatch({error, _}, talk_record_playback_parts:open(7, 8))
        end)
    end}.

stream_revocation_test_() ->
    {setup, fun stream_setup/0, fun stream_cleanup/1, fun(_) ->
        ?_test(begin
            put(sent, []),
            put(revoked, false),
            {ok, Payload} = talk_record_playback_parts:open(7, 8),
            [P] = maps:get(<<"parts">>, Payload),
            [_, Ticket] = binary:split(maps:get(<<"url">>, P), <<"?ticket=">>),
            Req = #{
                method => <<"GET">>,
                bindings => #{id => <<"8">>, seq => <<"0">>},
                qs => <<"ticket=", Ticket/binary>>,
                headers => #{}
            },
            {ok, _, _} = talk_record_playback_parts_handler:init(
                Req#{method => <<"HEAD">>},
                #{current_uid => 7, action => part}
            ),
            ?assertEqual(0, meck:num_calls(attachment_derived_artifact, with_live_spool, '_')),
            {ok, Bad, _} = talk_record_playback_parts_handler:init(
                Req#{headers => #{<<"range">> => <<"bytes=999999-">>}}, #{
                    current_uid => 7, action => part
                }
            ),
            ?assertEqual(416, maps:get(status, Bad)),
            ?assertEqual(0, meck:num_calls(attachment_derived_artifact, with_live_spool, '_')),
            Before = meck:num_calls(talk_record_assembly_logic, confirmed_parts_tx, '_'),
            ?assertExit(
                playback_authorization_changed,
                talk_record_playback_parts_handler:init(Req, #{current_uid => 7, action => part})
            ),
            ?assertEqual(65536, iolist_size(get(sent))),
            %% Two full checks at request entry, regardless of transmitted blocks.
            ?assertEqual(
                2, meck:num_calls(talk_record_assembly_logic, confirmed_parts_tx, '_') - Before
            )
        end)
    end}.

stream_setup() ->
    setup(),
    Bytes = binary:copy(<<"a">>, 150000),
    Parts = [
        #{
            <<"id">> => 11,
            <<"seq">> => 0,
            <<"attachment_id">> => 22,
            <<"sha256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
            <<"byte_size">> => byte_size(Bytes),
            <<"start_ms">> => 0,
            <<"end_ms">> => 1000
        }
    ],
    M = talk_record_assembly_logic:build_manifest(1, Parts),
    meck:expect(talk_record_assembly_logic, find_tx, fun(_, _) ->
        {ok, #{
            <<"capture_id">> => 1,
            <<"manifest_hash">> => maps:get(<<"manifest_hash">>, M),
            <<"segments">> => maps:get(<<"parts">>, M)
        }}
    end),
    meck:expect(talk_record_assembly_logic, confirmed_parts_tx, fun(_, _) -> {ok, Parts} end),
    meck:expect(talk_record_ds, capabilities, fun() -> #{<<"max_part_bytes">> => 200000} end),
    meck:expect(attachment_derived_artifact, with_live_spool, fun(22, _, F) ->
        attachment_upload_spool:with_source(Bytes, F)
    end),
    meck:expect(talk_record_content_common, visible_or_404, fun(_, _) ->
        case get(revoked) of
            true -> {error, not_found};
            _ -> {ok, #{<<"acl_revision">> => 1, <<"media_status">> => <<"assembling">>}}
        end
    end),
    meck:new(cowboy_req, [passthrough, unstick, no_link]),
    meck:expect(cowboy_req, reply, fun(Status, H, _, Req) ->
        Req#{status => Status, headers => H}
    end),
    meck:expect(cowboy_req, stream_reply, fun(Status, H, Req) ->
        Req#{status => Status, headers => H}
    end),
    meck:expect(cowboy_req, stream_body, fun(B, _, _) ->
        put(sent, [B | get(sent)]),
        put(revoked, true),
        ok
    end),
    ok.

stream_cleanup(_) ->
    meck:unload(cowboy_req),
    cleanup(ok).

setup() ->
    application:set_env(imboy, jwt_key, <<"0123456789abcdef0123456789abcdef">>),
    meck:new(talk_record_content_common, [passthrough, no_link]),
    meck:expect(talk_record_content_common, visible_or_404, fun
        (7, 8) ->
            {ok, #{<<"acl_revision">> => 1, <<"media_status">> => <<"assembling">>}};
        (_, _) ->
            {error, not_found}
    end),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, with_conn, fun(F) -> F(fake_conn) end),
    meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"id">> => 22}]} end),
    meck:new(talk_record_assembly_logic, [passthrough, no_link]),
    Parts = [
        #{
            <<"id">> => 11,
            <<"seq">> => 0,
            <<"attachment_id">> => 22,
            <<"sha256">> => binary:encode_hex(crypto:hash(sha256, <<"audio">>), lowercase),
            <<"byte_size">> => 5,
            <<"start_ms">> => 0,
            <<"end_ms">> => 1000
        }
    ],
    Manifest = talk_record_assembly_logic:build_manifest(1, Parts),
    meck:expect(talk_record_assembly_logic, find_tx, fun(_, _) ->
        {ok, #{
            <<"capture_id">> => 1,
            <<"manifest_hash">> => maps:get(<<"manifest_hash">>, Manifest),
            <<"segments">> => maps:get(<<"parts">>, Manifest)
        }}
    end),
    meck:expect(talk_record_assembly_logic, confirmed_parts_tx, fun(_, _) -> {ok, Parts} end),
    meck:new(talk_record_ds, [passthrough, no_link]),
    meck:expect(talk_record_ds, capabilities, fun() -> #{<<"max_part_bytes">> => 1024} end),
    meck:new(attachment_derived_artifact, [passthrough, no_link]),
    meck:expect(attachment_derived_artifact, with_live_spool, fun(22, _, F) ->
        attachment_upload_spool:with_source(<<"audio">>, F)
    end),
    ok.

cleanup(_) ->
    meck:unload([
        talk_record_content_common,
        elib_pg,
        talk_record_assembly_logic,
        talk_record_ds,
        attachment_derived_artifact
    ]),
    application:unset_env(imboy, jwt_key).
