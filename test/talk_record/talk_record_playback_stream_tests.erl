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
            %% 边缘未配置：GET fail-closed 503（不回退 BEAM 取流）。
            %% base_url 一并 unset：edge_base() 有 base_url+/edge 回退语义，
            %% worker VM 配置注入 base_url 时 503 场景会误得 302。
            ok = application:unset_env(imboy, base_url),
            ok = application:unset_env(imboy, edge_base_url),
            {ok, Resp503, _} = talk_record_playback_handler:init(Base, #{
                current_uid => 700001
            }),
            ?assertEqual(503, maps:get(status, Resp503)),
            Reads = meck:num_calls(attachment_artifact_reader, with_range, '_'),
            %% 配置边缘基址：GET 302 到边缘（同一票据，不泄私有签名）。
            ok = application:set_env(imboy, edge_base_url, <<"http://127.0.0.1:13992/edge">>),
            {ok, Resp302, _} = talk_record_playback_handler:init(Base, #{
                current_uid => 700001
            }),
            ?assertEqual(302, maps:get(status, Resp302)),
            ?assertEqual(
                <<"http://127.0.0.1:13992/edge?ticket=", T/binary>>,
                maps:get(<<"location">>, maps:get(response_headers, Resp302))
            ),
            put(stream_bytes, []),
            {ok, Head, _} = talk_record_playback_handler:init(Base#{method => <<"HEAD">>}, #{
                current_uid => 700001
            }),
            ?assertEqual(206, maps:get(status, Head)),
            ?assertEqual([], get(stream_bytes)),
            ?assertEqual(Reads, meck:num_calls(attachment_artifact_reader, with_range, '_')),
            %% 记录不可见（撤权/删除语义）：HEAD recheck → 404。
            meck:expect(talk_record_repo, find_visible, fun(_, _) ->
                {error, not_found}
            end),
            {ok, Revoked, _} = talk_record_playback_handler:init(
                Base#{method => <<"HEAD">>}, #{current_uid => 700001}
            ),
            ?assertEqual(404, maps:get(status, Revoked)),
            meck:expect(talk_record_repo, find_visible, fun(_, _) ->
                {ok, row(1, <<"available">>)}
            end),
            {ok, Denied, _} = talk_record_playback_handler:init(Base, #{current_uid => 700002}),
            ?assertEqual(403, maps:get(status, Denied))
        end)
    end}.

stream_setup() ->
    Prev = setup(),
    put(stream_bytes, []),
    put(revoke_after_chunk, false),
    ok = meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, with_conn, fun(F) -> F(test_connection) end),
    %% T5 读取权威：assembly 已发布成品行（published_audio_tx 查询）
    meck:expect(elib_pg, query, fun(_C, _Sql, _Args) -> {ok, [published_row()]} end),
    ok = meck:new(attachment_artifact_reader, [non_strict, no_link]),
    meck:expect(attachment_artifact_reader, with_range, fun(_Row, Offset, Length, F) ->
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
    application:unset_env(imboy, edge_base_url),
    application:unset_env(imboy, base_url),
    lists:foreach(fun meck:unload/1, [cowboy_req, attachment_artifact_reader, elib_pg]),
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

%% T5 读取权威：published_audio_tx 查询返回的已发布成品附件行。
published_row() ->
    #{
        <<"id">> => 900001,
        <<"size">> => 150000,
        <<"path">> => <<"talk-record/final/synthetic.aac">>,
        <<"file_hash256">> => binary:copy(<<"a">>, 64)
    }.
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
