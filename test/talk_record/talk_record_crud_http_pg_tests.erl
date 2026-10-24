%% Real loopback HTTP/JWT/PG checks; no storage or supplier calls.
-module(talk_record_crud_http_pg_tests).
-export([checks/0]).
-include_lib("eunit/include/eunit.hrl").

%% Explicit Required gate in a fresh VM:
%% eunit:test(talk_record_crud_http_pg_tests:checks(), [verbose]).
%% Not auto-discovered: shared EUnit VMs must not acquire this dedicated PG fixture.
%% This entrypoint retains all three checks; it provides no skip fallback.
checks() ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(S) ->
            {inorder, [
                ?_test(auth(S)),
                ?_test(personal(S)),
                ?_test(organization(S))
            ]}
        end}}.

setup() ->
    assert_fresh_vm(),
    Opts = connection_options(),
    verify_before_setup(Opts),
    S = (talk_record_t05_support:setup_all())#{
        saved_http_env =>
            [{K, application:get_env(imboy, K)} || K <- [api_auth_switch, jwt_key]]
    },
    talk_record_setup_fixture:prepare(
        S,
        fun() ->
            C = maps:get(conn, S),
            {ok, _, [{Db, Marker}]} = epgsql:squery(
                C,
                "SELECT current_database(), current_setting('imboy_test.run_id', true)"
            ),
            Db = list_to_binary(os:getenv("T05_PG_DB")),
            Marker = list_to_binary(os:getenv("TALK_CRUD_OWNED_MARKER")),
            ok = meck:unload(elib_tsid),
            talk_record_t05_support:seed_fixtures(C),
            application:set_env(imboy, api_auth_switch, <<"off">>),
            application:set_env(imboy, jwt_key, crypto:strong_rand_bytes(32)),
            {ok, _} = application:ensure_all_started(cowboy),
            {ok, _} = application:ensure_all_started(inets),
            Dispatch = cowboy_router:compile([{'_', talk_record_routes:routes()}]),
            {ok, _} = cowboy:start_clear(
                talk_record_crud_http,
                [{ip, {127, 0, 0, 1}}, {port, 0}],
                #{
                    env => #{dispatch => Dispatch},
                    middlewares => [cowboy_router, auth_middleware, cowboy_handler]
                }
            ),
            {{127, 0, 0, 1}, Port} = ranch:get_addr(talk_record_crud_http),
            S#{port => Port}
        end,
        fun cleanup/1
    ).

%% This preflight must precede setup_all/0, which removes a named pool.
assert_fresh_vm() ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    lists:foreach(
        fun(Name) -> undefined = whereis(Name) end,
        [pgsql, pooler_pgsql_pool_sup, elib_tsid_meck]
    ),
    not_initialized = elib_tsid:readiness().

connection_options() ->
    "127.0.0.1" = required("T05_PG_HOST"),
    "imboy-delivery-v3" = required("TALK_CRUD_OWNED_MARKER"),
    Port = list_to_integer(required("T05_PG_PORT")),
    Port = list_to_integer(required("TALK_CRUD_EXPECT_PORT")),
    true = Port > 0 andalso Port =< 65535,
    Db = required("T05_PG_DB"),
    Db = required("TALK_CRUD_EXPECT_DB"),
    true = lists:prefix("delivery_v3_t05_crud_", Db),
    true = length(Db) > length("delivery_v3_t05_crud_"),
    true = length(Db) =< 63,
    true = lists:all(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse
                (C >= $0 andalso C =< $9) orelse C =:= $_
        end,
        Db
    ),
    Password =
        case os:getenv("T05_PG_PASSWORD") of
            false -> error({required_crud_env, "T05_PG_PASSWORD"});
            V -> V
        end,
    #{
        host => "127.0.0.1",
        port => Port,
        database => Db,
        username => required("T05_PG_USER"),
        password => Password,
        ssl => false,
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }.

required(Key) ->
    case os:getenv(Key) of
        false -> error({required_crud_env, Key});
        "" -> error({required_crud_env, Key});
        V -> V
    end.

verify_before_setup(Opts) ->
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, C} = epgsql:connect(Opts),
    try
        verify_target(C),
        {ok, _, [{0}]} = epgsql:equery(
            C,
            "SELECT count(*) FROM pg_stat_activity WHERE datname=current_database()"
            " AND pid<>pg_backend_pid()",
            []
        ),
        verify_empty_business(C)
    after
        epgsql:close(C)
    end.

verify_target(C) ->
    Db = list_to_binary(required("TALK_CRUD_EXPECT_DB")),
    SysId = list_to_binary(required("TALK_CRUD_EXPECT_SYSTEM_ID")),
    User = list_to_binary(required("T05_PG_USER")),
    ServerPort = list_to_integer(required("TALK_CRUD_EXPECT_SERVER_PORT")),
    Comment = <<"Owned by imboy-delivery-v3; synthetic talk CRUD test data only">>,
    Marker = list_to_binary(required("TALK_CRUD_OWNED_MARKER")),
    {ok, _, [{Db, SysId, User, ServerPort, Comment, Marker}]} = epgsql:equery(
        C,
        "SELECT current_database(), (SELECT system_identifier::text FROM pg_control_system()),"
        " current_user, inet_server_port(), shobj_description(oid, 'pg_database'),"
        " current_setting('imboy_test.run_id', true)"
        " FROM pg_database WHERE datname=current_database()",
        []
    ),
    ?assertEqual(
        {ok, inttest_marker_db:migration_head(), false},
        erlang_migrate:version(#{conn => C, dir => "priv/migrations"})
    ).

%% Controller must bind a fresh-clone receipt before invoking this fixture.
%% The entire database must be authorized synthetic, rebuildable task resources.
%% These fixed business checks do NOT prove the complete CASCADE closure empty.
verify_empty_business(C) ->
    lists:foreach(
        fun(Sql) ->
            {ok, _, [{0}]} = epgsql:equery(C, Sql, [])
        end,
        empty_business_queries()
    ).

empty_business_queries() ->
    [
        "SELECT count(*) FROM public.\"attach_pending\"",
        "SELECT count(*) FROM public.\"attachment\"",
        "SELECT count(*) FROM public.\"attachment_derived_write\"",
        "SELECT count(*) FROM public.\"attachment_object_deletion\"",
        "SELECT count(*) FROM public.\"enterprise_attachment_retention\"",
        "SELECT count(*) FROM public.\"enterprise_conversation\"",
        "SELECT count(*) FROM public.\"enterprise_retention_policy\"",
        "SELECT count(*) FROM public.\"organization\"",
        "SELECT count(*) FROM public.\"organization_member\"",
        "SELECT count(*) FROM public.\"talk_record\"",
        "SELECT count(*) FROM public.\"talk_record_assembly\"",
        "SELECT count(*) FROM public.\"talk_record_attachment_ref\"",
        "SELECT count(*) FROM public.\"talk_record_audio_part\"",
        "SELECT count(*) FROM public.\"talk_record_audit\"",
        "SELECT count(*) FROM public.\"talk_record_capture\"",
        "SELECT count(*) FROM public.\"talk_record_capture_event\"",
        "SELECT count(*) FROM public.\"talk_record_cleanup_retry\"",
        "SELECT count(*) FROM public.\"talk_record_client_run\"",
        "SELECT count(*) FROM public.\"talk_record_client_run_part\"",
        "SELECT count(*) FROM public.\"talk_record_consent_event\"",
        "SELECT count(*) FROM public.\"talk_record_export\"",
        "SELECT count(*) FROM public.\"talk_record_grant\"",
        "SELECT count(*) FROM public.\"talk_record_idempotency\"",
        "SELECT count(*) FROM public.\"talk_record_job\"",
        "SELECT count(*) FROM public.\"talk_record_mark\"",
        "SELECT count(*) FROM public.\"talk_record_participant\"",
        "SELECT count(*) FROM public.\"talk_record_processor_run\"",
        "SELECT count(*) FROM public.\"talk_record_segment\"",
        "SELECT count(*) FROM public.\"talk_record_speaker\"",
        "SELECT count(*) FROM public.\"talk_record_summary\"",
        "SELECT count(*) FROM public.\"talk_record_transcript\"",
        "SELECT count(*) FROM public.\"user\"",
        "SELECT count(*) FROM public.\"workspace\"",
        "SELECT count(*) FROM public.\"workspace_attachment_retention\"",
        "SELECT count(*) FROM public.\"workspace_member\""
    ].

cleanup(S) ->
    catch cowboy:stop_listener(talk_record_crud_http),
    lists:foreach(
        fun
            ({K, undefined}) -> application:unset_env(imboy, K);
            ({K, {ok, V}}) -> application:set_env(imboy, K, V)
        end,
        maps:get(saved_http_env, S, [])
    ),
    talk_record_t05_support:teardown_all(S).

auth(S) ->
    expect(401, request(get, S, "", [], undefined)),
    expect(401, request(get, S, "", [{"authorization", "Bearer invalid"}], undefined)).

personal(S) ->
    B = body(<<"00000000-0000-0000-0000-000000095001">>),
    H = headers(700001, "crud-personal"),
    {RH, P} = expect(201, request(post, S, "", H, B)),
    Id = maps:get(<<"id">>, P),
    ?assertEqual(700001, maps:get(<<"owner_user_id">>, P)),
    ?assert(is_list(proplists:get_value("etag", RH))),
    {ReplayH, Replay} = expect(201, request(post, S, "", H, B)),
    ?assertEqual(Id, maps:get(<<"id">>, Replay)),
    ?assertEqual("true", proplists:get_value("idempotent-replayed", ReplayH)),
    expect(409, request(post, S, "", H, B#{<<"title">> => <<"different">>})),
    Path = "/" ++ integer_to_list(Id),
    {_, Detail} = expect(200, request(get, S, Path, headers(700001, ""), undefined)),
    ?assertEqual(Id, maps:get(<<"id">>, Detail)),
    expect(404, request(get, S, Path, headers(700007, ""), undefined)),
    {ok, _, [{1}]} = epgsql:equery(
        maps:get(conn, S),
        "SELECT count(*) FROM talk_record WHERE created_by=$1 AND client_recording_id=$2",
        [700001, maps:get(<<"client_recording_id">>, B)]
    ).

organization(S) ->
    B = (body(<<"00000000-0000-0000-0000-000000095002">>))#{
        <<"scope">> => <<"organization">>,
        <<"organization_id">> => 710001,
        <<"workspace_id">> => 720001
    },
    H = headers(700001, "crud-org"),
    {_, P} = expect(201, request(post, S, "", H, B)),
    Id = maps:get(<<"id">>, P),
    ?assertEqual(710001, maps:get(<<"organization_id">>, P)),
    ?assertEqual(720001, maps:get(<<"workspace_id">>, P)),
    expect(404, request(get, S, "/" ++ integer_to_list(Id), headers(700004, ""), undefined)).

body(UUID) ->
    #{
        <<"client_recording_id">> => UUID,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"processing_mode">> => <<"record_only">>,
        <<"title">> => <<"synthetic-crud">>
    }.

headers(Uid, Key) ->
    [
        {"authorization", "Bearer " ++ binary_to_list(token_ds:encrypt_token(Uid))},
        {"idempotency-key", Key}
    ].

request(Method, S, Path, H, Body) ->
    Url =
        "http://127.0.0.1:" ++ integer_to_list(maps:get(port, S)) ++
            "/api/v1/talk-records" ++ Path,
    Req =
        case Body of
            undefined -> {Url, H};
            _ -> {Url, H, "application/json", jsone:encode(Body)}
        end,
    {ok, {{_, Code, _}, RH, Bytes}} = httpc:request(
        Method,
        Req,
        [{timeout, 5000}, {connect_timeout, 2000}],
        [{body_format, binary}]
    ),
    {Code, RH, Bytes}.

expect(Code, Result) ->
    ?assertMatch({Code, _, _}, Result),
    {_, H, Bytes} = Result,
    {H, maps:get(<<"payload">>, jsone:decode(Bytes), undefined)}.
