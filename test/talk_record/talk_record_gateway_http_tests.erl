-module(talk_record_gateway_http_tests).
-include_lib("eunit/include/eunit.hrl").

%% Requires the caller's identity-gated dedicated PostgreSQL and synthetic AAC.
%% Record create/detail use real JWT middleware; device-signature checking is
%% disabled explicitly in this test instance. Capture events
%% and part registration/confirmation also traverse HTTP; PUT uses the real
%% capability check. Assembly and authenticated playback use real storage;
%% model quality and true-device processing remain separate acceptance gates.
gateway_http_test_() ->
    {timeout, 120,
        {setup,
            fun() ->
                Prev = [
                    {K, application:get_env(imboy, K)}
                 || K <-
                        [
                            api_auth_switch,
                            jwt_key,
                            attachment_storage_registry,
                            garage,
                            enterprise_attachment_retention_days
                        ]
                ],
                State = talk_record_t07_support:setup_all(),
                %% Integration must exercise the production allocator, including
                %% its static namespace registration, rather than T05's ID fixture.
                ok = meck:unload(elib_tsid),
                false = lists:member(elib_tsid, meck:mocked()),
                ok = application:set_env(imboy, api_auth_switch, <<"off">>),
                ok = talk_record_t07_support:seed(State),
                ok = organization_index_fixture(State),
                ok = configure_private_garage(),
                {ok, _} = application:ensure_all_started(cowboy),
                {ok, _} = application:ensure_all_started(inets),
                Dispatch = cowboy_router:compile([
                    {'_',
                        talk_record_routes:routes() ++
                            [
                                {"/api/v1/organizations/:organization_id/attachments",
                                    workspace_attachment_handler, #{}},
                                {"/api/v1/attachment/gateway/:session_key",
                                    attachment_storage_gateway_handler, #{action => put}}
                            ]}
                ]),
                {ok, _} = cowboy:start_clear(
                    talk_record_http_relay,
                    [{ip, {127, 0, 0, 1}}, {port, 0}],
                    #{
                        env => #{dispatch => Dispatch},
                        middlewares => [cowboy_router, auth_middleware, cowboy_handler]
                    }
                ),
                State#{http_port => ranch:get_port(talk_record_http_relay), http_prev => Prev}
            end,
            fun(State) ->
                ok = cowboy:stop_listener(talk_record_http_relay),
                ok = talk_record_t07_support:teardown_all(State),
                lists:foreach(
                    fun
                        ({K, undefined}) -> application:unset_env(imboy, K);
                        ({K, {ok, V}}) -> application:set_env(imboy, K, V)
                    end,
                    maps:get(http_prev, State)
                )
            end,
            fun(State) -> ?_test(run(State)) end}}.

%% T07 intentionally has only talk-record foundation columns. The real organization
%% detail and combined attachment index also read these existing production fields.
%% This extends only the controller-owned synthetic HTTP fixture, never production.
organization_index_fixture(State) ->
    Conn = maps:get(conn, State),
    lists:foreach(fun(Sql) -> talk_record_t07_support:sql(Conn, Sql) end, [
        "ALTER TABLE organization ADD COLUMN IF NOT EXISTS branding jsonb NOT NULL DEFAULT '{}'::jsonb,"
        " ADD COLUMN IF NOT EXISTS settings jsonb NOT NULL DEFAULT '{}'::jsonb,"
        " ADD COLUMN IF NOT EXISTS created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,"
        " ADD COLUMN IF NOT EXISTS updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP",
        "ALTER TABLE \"group\" ADD COLUMN IF NOT EXISTS title varchar(200) NOT NULL DEFAULT '',"
        " ADD COLUMN IF NOT EXISTS status smallint NOT NULL DEFAULT 1",
        "ALTER TABLE channel ADD COLUMN IF NOT EXISTS name varchar(200) NOT NULL DEFAULT '',"
        " ADD COLUMN IF NOT EXISTS status smallint NOT NULL DEFAULT 1"
    ]).

%% An explicit controller-owned loopback instance; never use deployment defaults.
configure_private_garage() ->
    case os:getenv("TALK_RECORD_PRIVATE_GARAGE_CONFIG") of
        false ->
            ok;
        Path ->
            {ok, Bytes} = file:read_file(Path),
            C = jsone:decode(Bytes),
            Endpoint = maps:get(<<"endpoint">>, C),
            ?assertMatch(<<"http://127.0.0.1:", _/binary>>, Endpoint),
            ?assertEqual(<<"talk-record-relay">>, maps:get(<<"bucket">>, C)),
            Garage = #{
                endpoint => Endpoint,
                bucket => maps:get(<<"bucket">>, C),
                region => maps:get(<<"region">>, C),
                access_key => maps:get(<<"access_key">>, C),
                secret_key => maps:get(<<"secret_key">>, C)
            },
            application:set_env(imboy, garage, Garage),
            application:set_env(imboy, attachment_storage_registry, #{
                version => 1,
                upload_namespaces => #{<<"talk_record">> => <<"talk-upload-test">>},
                roots => #{
                    talk_upload_test => #{
                        driver => <<"garage">>,
                        namespace => <<"talk-upload-test">>,
                        container => maps:get(bucket, Garage),
                        endpoint => Endpoint
                    }
                }
            })
    end.

run(State) ->
    {ok, Bytes} = file:read_file(os:getenv("TALK_HTTP_AAC_FIXTURE")),
    ?assert(byte_size(Bytes) > 0),
    Uid = maps:get(u1, talk_record_t07_support:ids()),
    {Rid, Auth, RecordETag} = create_over_http(State, Uid),
    Prefix = "http://127.0.0.1:" ++ integer_to_list(maps:get(http_port, State)),
    RecordUrl = Prefix ++ "/api/v1/talk-records/" ++ integer_to_list(Rid),
    ParticipantsETag = participants_over_http(RecordUrl, Auth, RecordETag),
    Device = <<"10000000-0000-0000-0000-000000090001">>,
    Capture = post_json(
        RecordUrl ++ "/captures",
        Auth ++
            [{"if-match", ParticipantsETag}, {"idempotency-key", "http-capture"}],
        #{<<"device_session_id">> => Device, <<"device_id">> => <<"http-device">>},
        201
    ),
    Cid = maps:get(<<"capture_id">>, Capture),
    CaptureUrl = RecordUrl ++ "/captures/" ++ integer_to_list(Cid),
    Started = post_json(
        CaptureUrl ++ "/events",
        Auth,
        #{
            <<"device_session_id">> => Device,
            <<"client_event_id">> => <<"20000000-0000-0000-0000-000000090001">>,
            <<"seq">> => 0,
            <<"event">> => <<"start">>,
            <<"position_ms">> => 0
        },
        200
    ),
    ?assertEqual(<<"recording">>, maps:get(<<"status">>, Started)),
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Part = post_json(
        CaptureUrl ++ "/audio-parts",
        Auth ++
            [{"idempotency-key", "http-part"}],
        #{
            <<"seq">> => 0,
            <<"start_ms">> => 0,
            <<"end_ms">> => 1000,
            <<"sha256">> => Sha,
            <<"byte_size">> => byte_size(Bytes),
            <<"codec">> => <<"aac">>
        },
        201
    ),
    Ref = maps:get(<<"upload_ref">>, Part),
    Url = Prefix ++ binary_to_list(maps:get(<<"put_url">>, Ref)),
    {ok, {{_, 200, _}, _, Reply}} = httpc:request(
        put,
        {Url, [], "audio/aac", Bytes},
        [{timeout, 15000}],
        [{body_format, binary}]
    ),
    ?assertMatch(#{<<"pinned">> := true, <<"sha256">> := Sha}, jsone:decode(Reply)),
    %% Replacing bytes must fail while the session is still registered,
    %% before confirmation can independently prohibit writes.
    {ok, {{_, 409, _}, _, Conflict}} = httpc:request(
        put,
        {Url, [], "audio/aac", <<Bytes/binary, 0>>},
        [{timeout, 15000}],
        [{body_format, binary}]
    ),
    ?assertEqual(<<"content already pinned to different receipt">>, Conflict),
    {ok, Root} = attachment_storage:resolve(<<"talk-upload-test">>),
    Driver = attachment_storage:driver_module(maps:get(driver, Root)),
    {ok, Stored} = Driver:read(
        <<"talk-upload-test">>, maps:get(container, Root), maps:get(<<"object_key">>, Ref)
    ),
    ?assertEqual(Bytes, Stored),
    PartId = maps:get(<<"part_id">>, Part),
    Confirm = post_json(
        CaptureUrl ++ "/audio-parts/" ++ integer_to_list(PartId) ++
            "/confirm",
        Auth,
        #{},
        200
    ),
    ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, Confirm)),
    [SessionUrl, _] = string:split(Url, "?", leading),
    BadUrl = SessionUrl ++ "?capability=invalid",
    {ok, {{_, 403, _}, _, _}} = httpc:request(
        put,
        {BadUrl, [], "audio/aac", Bytes},
        [{timeout, 15000}],
        []
    ),
    complete_record(
        State, RecordUrl, CaptureUrl, Auth, Device, Cid, Confirm, Sha, byte_size(Bytes)
    ),
    governance_over_http(State).

create_over_http(State, Uid) ->
    Prefix = "http://127.0.0.1:" ++ integer_to_list(maps:get(http_port, State)),
    Url = Prefix ++ "/api/v1/talk-records",
    Token = token_ds:encrypt_token(Uid),
    Auth = [{"authorization", "Bearer " ++ binary_to_list(Token)}],
    ClientId = <<"00000000-0000-0000-0000-000000090001">>,
    Body = jsone:encode(#{
        <<"client_recording_id">> => ClientId,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"processing_mode">> => <<"record_only">>,
        <<"title">> => <<"synthetic-http-record">>
    }),
    {ok, {{_, 401, _}, _, _}} = httpc:request(get, {Url, []}, [], []),
    {ok, {{_, 201, _}, Headers, Response}} = httpc:request(
        post,
        {Url, [{"idempotency-key", "http-record"} | Auth], "application/json", Body},
        [],
        [{body_format, binary}]
    ),
    Payload = maps:get(<<"payload">>, jsone:decode(Response)),
    Rid = maps:get(<<"id">>, Payload),
    ?assertEqual(Uid, maps:get(<<"owner_user_id">>, Payload)),
    ?assertEqual(ClientId, maps:get(<<"client_recording_id">>, Payload)),
    ?assert(is_list(proplists:get_value("etag", Headers))),
    DetailUrl = Url ++ "/" ++ integer_to_list(Rid),
    {ok, {{_, 200, _}, _, DetailBytes}} = httpc:request(
        get,
        {DetailUrl, Auth},
        [],
        [{body_format, binary}]
    ),
    Detail = maps:get(<<"payload">>, jsone:decode(DetailBytes)),
    ?assertEqual(ClientId, maps:get(<<"client_recording_id">>, Detail)),
    Stranger = token_ds:encrypt_token(maps:get(u7, talk_record_t07_support:ids())),
    {ok, {{_, 404, _}, _, _}} = httpc:request(
        get,
        {DetailUrl, [{"authorization", "Bearer " ++ binary_to_list(Stranger)}]},
        [],
        []
    ),
    {Rid, Auth, proplists:get_value("etag", Headers)}.

participants_over_http(RecordUrl, Auth, InitialETag) ->
    Url = RecordUrl ++ "/participants",
    Body = jsone:encode(#{
        <<"participants">> => [#{<<"display_name">> => <<"  Synthetic guest  ">>}]
    }),
    {ok, {{_, 428, _}, _, _}} = httpc:request(
        put,
        {Url, Auth, "application/json", Body},
        [],
        [{body_format, binary}]
    ),
    {ok, {{_, 200, _}, Headers, Response}} = httpc:request(
        put,
        {Url, [{"if-match", InitialETag} | Auth], "application/json", Body},
        [],
        [{body_format, binary}]
    ),
    #{<<"list">> := [Participant]} = maps:get(<<"payload">>, jsone:decode(Response)),
    ?assertEqual(null, maps:get(<<"user_id">>, Participant)),
    ?assertEqual(<<"Synthetic guest">>, maps:get(<<"display_name">>, Participant)),
    {ok, {{_, 200, _}, _, Readback}} = httpc:request(get, {Url, Auth}, [], [{body_format, binary}]),
    ?assertEqual(
        [Participant], maps:get(<<"list">>, maps:get(<<"payload">>, jsone:decode(Readback)))
    ),
    ETag = proplists:get_value("etag", Headers),
    ?assert(is_list(ETag)),
    ?assertNotEqual(InitialETag, ETag),
    {ok, {{_, 412, _}, _, _}} = httpc:request(
        put,
        {Url, [{"if-match", InitialETag} | Auth], "application/json", Body},
        [],
        [{body_format, binary}]
    ),
    ETag.

post_json(Url, Headers, Body, Status) ->
    {ok, {{_, ActualStatus, _}, _, Response}} = httpc:request(
        post,
        {Url, Headers, "application/json", jsone:encode(Body)},
        [{timeout, 15000}],
        [{body_format, binary}]
    ),
    ?assertEqual(
        {Status, ok},
        {ActualStatus,
            case ActualStatus =:= Status of
                true -> ok;
                false -> Response
            end}
    ),
    maps:get(<<"payload">>, jsone:decode(Response)).

complete_record(State, RecordUrl, CaptureUrl, Auth, Device, Cid, Part, Sha, Size) ->
    Ended = post_json(
        CaptureUrl ++ "/events",
        Auth,
        #{
            <<"device_session_id">> => Device,
            <<"client_event_id">> => <<"20000000-0000-0000-0000-000000090002">>,
            <<"seq">> => 1,
            <<"event">> => <<"end">>,
            <<"position_ms">> => 1000
        },
        200
    ),
    ?assertEqual(<<"ended">>, maps:get(<<"status">>, Ended)),
    %% Independent canonical wire fixture: no server manifest builder is used.
    Canonical = iolist_to_binary([
        "{\"capture_id\":",
        integer_to_list(Cid),
        ",\"parts\":[{\"attachment_id\":",
        integer_to_list(maps:get(<<"attachment_id">>, Part)),
        ",\"byte_size\":",
        integer_to_list(Size),
        ",\"duration_ms\":1000,\"offset_ms\":0,\"part_id\":",
        integer_to_list(maps:get(<<"part_id">>, Part)),
        ",\"seq\":0,\"sha256\":\"",
        Sha,
        "\"}],\"total_bytes\":",
        integer_to_list(Size),
        ",\"total_duration_ms\":1000,\"v\":1}"
    ]),
    Hash = binary:encode_hex(crypto:hash(sha256, Canonical), lowercase),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(
        get,
        {RecordUrl, Auth},
        [],
        [{body_format, binary}]
    ),
    Completed = post_json(
        RecordUrl ++ "/complete",
        Auth ++
            [
                {"if-match", proplists:get_value("etag", Headers)},
                {"idempotency-key", "http-complete"}
            ],
        #{
            <<"captures">> => [
                #{
                    <<"capture_id">> => Cid,
                    <<"last_seq">> => 0,
                    <<"manifest_hash">> => Hash
                }
            ]
        },
        202
    ),
    ?assertEqual(<<"assembling">>, maps:get(<<"media_status">>, Completed)),
    {ok, _} = talk_record_job_worker:run_once(),
    {ok, {{_, 200, _}, _, Response}} = httpc:request(
        get,
        {RecordUrl, Auth},
        [],
        [{body_format, binary}]
    ),
    Payload = maps:get(<<"payload">>, jsone:decode(Response)),
    ?assertEqual(<<"available">>, maps:get(<<"media_status">>, Payload)),
    Rid = maps:get(<<"record_id">>, Completed),
    Row = talk_record_t07_support:one(
        maps:get(conn, State),
        [
            "SELECT r.audio_manifest_attachment_id, a.audio_attachment_id FROM talk_record r ",
            "JOIN talk_record_assembly a ON a.record_id=r.id WHERE r.id=",
            integer_to_list(Rid)
        ]
    ),
    ?assert(is_integer(maps:get(<<"audio_manifest_attachment_id">>, Row))),
    ?assert(is_integer(maps:get(<<"audio_attachment_id">>, Row))),
    playback_over_http(State, RecordUrl, Auth),
    cleanup_real_artifacts(State, Rid, RecordUrl, Auth).

playback_over_http(State, RecordUrl, Auth) ->
    Session = post_json(RecordUrl ++ "/playback", Auth, #{}, 200),
    Prefix = "http://127.0.0.1:" ++ integer_to_list(maps:get(http_port, State)),
    Url = Prefix ++ binary_to_list(maps:get(<<"url">>, Session)),
    {ok, {{_, 200, _}, Headers, Audio}} = httpc:request(
        get,
        {Url, Auth},
        [{timeout, 15000}],
        [{body_format, binary}]
    ),
    ?assert(byte_size(Audio) > 42),
    ?assertEqual("audio/aac", proplists:get_value("content-type", Headers)),
    {ok, {{_, 206, _}, RangeHeaders, Slice}} = httpc:request(
        get,
        {Url, [{"range", "bytes=7-42"} | Auth]},
        [],
        [{body_format, binary}]
    ),
    ?assertEqual(binary:part(Audio, 7, 36), Slice),
    ?assertEqual(
        "bytes 7-42/" ++ integer_to_list(byte_size(Audio)),
        proplists:get_value("content-range", RangeHeaders)
    ),
    {ok, {{_, 200, _}, HeadHeaders, Empty}} = httpc:request(
        head,
        {Url, Auth},
        [],
        [{body_format, binary}]
    ),
    ?assertEqual(<<>>, Empty),
    ?assertEqual(
        integer_to_list(byte_size(Audio)),
        proplists:get_value("content-length", HeadHeaders)
    ),
    {ok, {{_, 401, _}, _, _}} = httpc:request(get, {Url, []}, [], []),
    Stranger = token_ds:encrypt_token(maps:get(u7, talk_record_t07_support:ids())),
    {ok, {{_, 403, _}, _, _}} = httpc:request(
        get,
        {Url, [{"authorization", "Bearer " ++ binary_to_list(Stranger)}]},
        [],
        []
    ),
    {ok, {{_, 416, _}, _, _}} = httpc:request(
        get,
        {Url, [{"range", "bytes=0-1,7-9"} | Auth]},
        [],
        []
    ),
    {ok, {{_, 200, _}, RecordHeaders, _}} = httpc:request(
        get,
        {RecordUrl, Auth},
        [],
        []
    ),
    {ok, {{_, 202, _}, _, DeletedBytes}} = httpc:request(
        delete,
        {RecordUrl, [{"if-match", proplists:get_value("etag", RecordHeaders)} | Auth]},
        [],
        [{body_format, binary}]
    ),
    Deleted = maps:get(<<"payload">>, jsone:decode(DeletedBytes)),
    ?assertEqual(true, maps:get(<<"logical_deleted">>, Deleted)),
    {ok, {{_, DeniedStatus, _}, _, _}} = httpc:request(get, {Url, Auth}, [], []),
    ?assertMatch({true, _}, {lists:member(DeniedStatus, [403, 404]), DeniedStatus}).

cleanup_real_artifacts(State, Rid, RecordUrl, Auth) ->
    Conn = maps:get(conn, State),
    {ok, Rows} = elib_pg:query(
        Conn,
        <<
            "SELECT DISTINCT a.path,a.storage_namespace,a.storage_container FROM attachment a "
            "JOIN talk_record_attachment_ref r ON r.attachment_id=a.id WHERE r.record_id=$1"
        >>,
        [Rid]
    ),
    ?assertEqual(3, length(Rows)),
    {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
    Delete = fun(_, _, _, Identity) -> attachment_storage:delete_snapshot(Identity) end,
    {ok, 2} = attachment_delete_worker:run_batch(Conn, Delete),
    Initial = [read_artifact(Row) || Row <- Rows],
    ?assertEqual(2, length([missing || {error, not_found} <- Initial])),
    ?assertEqual(1, length([present || {ok, _} <- Initial])),
    {ok, {{_, 200, _}, _, PendingBytes}} = httpc:request(
        get,
        {RecordUrl, Auth},
        [],
        [{body_format, binary}]
    ),
    Pending = maps:get(<<"payload">>, jsone:decode(PendingBytes)),
    ?assertNotEqual(<<"complete">>, maps:get(<<"cleanup_status">>, Pending)),
    %% Only this dedicated synthetic fixture advances capability exhaustion.
    %% This proves reconciliation after expiry, not real elapsed-time behavior.
    {ok, [_]} = elib_pg:query(
        Conn,
        <<
            "UPDATE attach_pending p SET upload_expires_at=now()-interval '1 second' "
            "FROM attachment a JOIN talk_record_attachment_ref r ON r.attachment_id=a.id "
            "WHERE p.object_key=a.path AND r.record_id=$1 AND r.role='audio_part' "
            "RETURNING p.object_key"
        >>,
        [Rid]
    ),
    {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
    {ok, 1} = attachment_delete_worker:run_batch(Conn, Delete),
    lists:foreach(
        fun(Row) ->
            ?assertEqual({error, not_found}, read_artifact(Row))
        end,
        Rows
    ),
    {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
    {ok, {{_, 200, _}, _, Bytes}} = httpc:request(
        get,
        {RecordUrl, Auth},
        [],
        [{body_format, binary}]
    ),
    Envelope = jsone:decode(Bytes),
    ?assertEqual(0, maps:get(<<"code">>, Envelope)),
    Payload = maps:get(<<"payload">>, Envelope),
    ?assertEqual(<<"complete">>, maps:get(<<"cleanup_status">>, Payload)),
    ?assertEqual(<<"deleted">>, maps:get(<<"physical_delete_state">>, Payload)).

read_artifact(Row) ->
    Ns = maps:get(<<"storage_namespace">>, Row),
    {ok, Root} = attachment_storage:resolve(Ns),
    Driver = attachment_storage:driver_module(maps:get(driver, Root)),
    Driver:read(Ns, maps:get(<<"storage_container">>, Row), maps:get(<<"path">>, Row)).

governance_over_http(State) ->
    Prefix = "http://127.0.0.1:" ++ integer_to_list(maps:get(http_port, State)),
    Ids = talk_record_t07_support:ids(),
    Owner = maps:get(u2, Ids),
    OwnerAuth = auth(Owner),
    AdminAuth = auth(maps:get(u1, Ids)),
    Record = post_json(
        Prefix ++ "/api/v1/talk-records",
        [{"idempotency-key", "http-governance-record"} | OwnerAuth],
        #{
            <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000090002">>,
            <<"notice_ack">> => true,
            <<"policy_version">> => <<"talk-record-notice-v1">>,
            <<"scope">> => <<"organization">>,
            <<"organization_id">> => maps:get(org_a, Ids),
            <<"processing_mode">> => <<"record_only">>,
            <<"title">> => <<"private HTTP governance fixture">>
        },
        201
    ),
    Rid = maps:get(<<"id">>, Record),
    Aid = governance_artifact(State, Rid, Owner),
    RecordUrl = Prefix ++ "/api/v1/talk-records/" ++ integer_to_list(Rid),
    Url = RecordUrl ++ "/attachments/" ++ integer_to_list(Aid) ++ "/governance",
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {RecordUrl, AdminAuth}, [], []),
    {ok, {{_, 200, _}, Headers, Bytes}} = httpc:request(
        get,
        {Url, AdminAuth},
        [],
        [{body_format, binary}]
    ),
    Meta = maps:get(<<"payload">>, jsone:decode(Bytes)),
    ?assertEqual(Rid, maps:get(<<"record_id">>, Meta)),
    ?assertEqual(
        false,
        lists:any(
            fun(K) -> maps:is_key(K, Meta) end,
            [<<"title">>, <<"path">>, <<"url">>, <<"audio_url">>]
        )
    ),
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {Url, OwnerAuth}, [], []),
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {Url, auth(maps:get(u4, Ids))}, [], []),
    ETag = proplists:get_value("etag", Headers),
    organization_index_over_http(Prefix, maps:get(org_a, Ids), Rid, Aid, OwnerAuth, AdminAuth),
    governance_hold_cycle(Url, AdminAuth, ETag),
    governance_cleanup(State, RecordUrl, Url, AdminAuth, OwnerAuth).

auth(Uid) -> [{"authorization", "Bearer " ++ binary_to_list(token_ds:encrypt_token(Uid))}].

governance_artifact(State, Rid, Owner) ->
    Conn = maps:get(conn, State),
    application:set_env(imboy, enterprise_attachment_retention_days, 1),
    {ok, created, Job} = elib_pg:with_tx(fun(C) ->
        talk_record_job_repo:enqueue_tx(
            C,
            #{record_id => Rid, kind => <<"export">>, dedupe_key => <<"http-governance-fixture">>}
        )
    end),
    Bytes = <<"synthetic governance artifact">>,
    {ok, Aid} = elib_pg:with_tx(fun(C) ->
        attachment_derived_artifact:reserve_tx(
            C,
            #{job_id => Job, attempt => 1, actor_id => Owner},
            #{
                <<"path">> => <<"http-governance/", (integer_to_binary(Rid))/binary>>,
                <<"url">> => <<>>,
                <<"name">> => <<"synthetic.txt">>,
                <<"size">> => byte_size(Bytes),
                <<"mime_type">> => <<"text/plain">>,
                <<"scope">> => <<"talk_record">>,
                <<"scope_ref">> => integer_to_binary(Rid),
                <<"file_hash256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase)
            }
        )
    end),
    ok = attachment_derived_artifact:write(Aid, Bytes),
    %% Synthetic historical asset: elapsed retention is not simulated real time.
    {ok, 1} = elib_pg:execute(
        Conn,
        <<"UPDATE attachment SET created_at=now()-interval '2 days' WHERE id=$1">>,
        [Aid]
    ),
    {ok, _, _} = elib_pg:with_tx(fun(C) ->
        talk_record_attachment_adapter:bind_artifact_tx(
            C, Rid, Aid, <<"export">>, <<"http-governance">>
        )
    end),
    {ok, 1} = elib_pg:execute(
        Conn,
        <<"UPDATE talk_record_job SET state='succeeded',finished_at=now() WHERE id=$1">>,
        [Job]
    ),
    Aid.

governance_hold_cycle(Url, Auth, ETag) ->
    Body = #{<<"op">> => <<"hold">>, <<"reason">> => <<"synthetic HTTP hold">>},
    mutation_error(Url, [{"idempotency-key", "missing-version"} | Auth], Body, 428),
    HoldAuth = [{"if-match", ETag}, {"idempotency-key", "http-hold"} | Auth],
    Held = post_json(Url, HoldAuth, Body, 200),
    ?assertEqual(<<"held">>, maps:get(<<"hold_state">>, Held)),
    {ok, {{_, 200, _}, Headers, Replay}} = httpc:request(
        post,
        {Url, HoldAuth, "application/json", jsone:encode(Body)},
        [],
        [{body_format, binary}]
    ),
    ?assertEqual("true", proplists:get_value("idempotent-replayed", Headers)),
    ?assertEqual(Held, maps:get(<<"payload">>, jsone:decode(Replay))),
    mutation_error(
        Url,
        [{"if-match", ETag}, {"idempotency-key", "stale-release"} | Auth],
        #{<<"op">> => <<"release_hold">>},
        412
    ),
    mutation_error(
        Url,
        [{"if-match", "2"}, {"idempotency-key", "blocked-purge"} | Auth],
        #{<<"op">> => <<"purge">>},
        409
    ),
    Released = post_json(
        Url,
        [{"if-match", "2"}, {"idempotency-key", "http-release"} | Auth],
        #{<<"op">> => <<"release_hold">>},
        200
    ),
    ?assertEqual(<<"none">>, maps:get(<<"hold_state">>, Released)).

mutation_error(Url, Auth, Body, Status) ->
    {ok, {{_, Actual, _}, _, _}} = httpc:request(
        post,
        {Url, Auth, "application/json", jsone:encode(Body)},
        [],
        [{body_format, binary}]
    ),
    ?assertEqual(Status, Actual).

governance_cleanup(State, RecordUrl, Url, Admin, Owner) ->
    Purged = post_json(
        Url,
        [{"if-match", "3"}, {"idempotency-key", "http-approved-purge"} | Admin],
        #{<<"op">> => <<"purge">>},
        200
    ),
    ?assertEqual(<<"purged">>, maps:get(<<"purge_state">>, Purged)),
    ?assertEqual(false, maps:get(<<"cleanup_scheduled">>, Purged)),
    {ok, {{_, 202, _}, _, _}} = httpc:request(
        delete,
        {RecordUrl, [{"if-match", "4"} | Owner]},
        [],
        [{body_format, binary}]
    ),
    {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
    {ok, 1} = attachment_delete_worker:run_batch(
        maps:get(conn, State),
        fun(_, _, _, Identity) -> attachment_storage:delete_snapshot(Identity) end
    ),
    {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
    {ok, {{_, 200, _}, _, Bytes}} = httpc:request(
        get,
        {RecordUrl, Owner},
        [],
        [{body_format, binary}]
    ),
    Payload = maps:get(<<"payload">>, jsone:decode(Bytes)),
    ?assertEqual(<<"complete">>, maps:get(<<"cleanup_status">>, Payload)).

organization_index_over_http(Prefix, Org, Rid, Aid, Owner, Admin) ->
    Url =
        Prefix ++ "/api/v1/organizations/" ++ integer_to_list(Org) ++
            "/attachments?source=talk_record",
    {ok, {{_, 200, _}, _, Bytes}} = httpc:request(
        get,
        {Url, Owner},
        [],
        [{body_format, binary}]
    ),
    Envelope = jsone:decode(Bytes),
    ?assertEqual(0, maps:get(<<"code">>, Envelope)),
    Payload = maps:get(<<"payload">>, Envelope),
    Rows = maps:get(<<"items">>, Payload),
    [Row] = [R || #{<<"id">> := Id} = R <- Rows, Id =:= Aid],
    ?assertEqual(Rid, maps:get(<<"record_id">>, Row)),
    ?assertEqual(null, maps:get(<<"workspace_id">>, Row)),
    ?assertEqual(<<"talk_record">>, maps:get(<<"open_kind">>, Row)),
    ?assertEqual(false, maps:get(<<"can_govern">>, Row)),
    ?assertEqual(
        false,
        lists:any(
            fun(K) -> maps:is_key(K, Row) end,
            [<<"path">>, <<"url">>, <<"audio_url">>]
        )
    ),
    {ok, {{_, 200, _}, _, AdminBytes}} = httpc:request(
        get,
        {Url, Admin},
        [],
        [{body_format, binary}]
    ),
    AdminEnvelope = jsone:decode(AdminBytes),
    ?assertEqual(0, maps:get(<<"code">>, AdminEnvelope)),
    AdminRows = maps:get(<<"items">>, maps:get(<<"payload">>, AdminEnvelope)),
    ?assertEqual([], [R || #{<<"id">> := Id} = R <- AdminRows, Id =:= Aid]).
