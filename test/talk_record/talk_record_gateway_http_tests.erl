-module(talk_record_gateway_http_tests).
-include_lib("eunit/include/eunit.hrl").
-export([source_descriptor/0, available_source/2]).

%% Requires the caller's identity-gated dedicated PostgreSQL and synthetic AAC.
%% Record create/detail use real JWT middleware; device-signature checking is
%% disabled explicitly in this test instance. Capture events
%% and part registration/confirmation also traverse HTTP; PUT uses the real
%% capability check. Assembly and authenticated playback use real storage;
%% model quality and true-device processing remain separate acceptance gates.
gateway_http_test_() ->
    %% Original TTL/body and timeout envelopes are unchanged.
    {timeout, 4200,
        {setup, fun gateway_setup/0, fun gateway_cleanup/1, fun(State) ->
            {timeout, 4100, ?_test(run(State))}
        end}}.

gateway_setup() ->
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
    case fixture_capture(fun talk_record_t07_support:setup_all/0) of
        {ok, State} ->
            case fixture_capture(fun() -> gateway_ready(State, Prev) end) of
                {ok, Ready} ->
                    Ready;
                Failure ->
                    fixture_finish(
                        gateway_fixture_failure,
                        Failure,
                        gateway_release(State, Prev)
                    )
            end;
        Failure ->
            %% No usable State: T07 partial effects require external reconciliation.
            fixture_finish(
                gateway_state_unavailable,
                Failure,
                fixture_errors([fun() -> ok = gateway_restore(E) end || E <- Prev])
            )
    end.

gateway_ready(State, Prev) ->
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
                        workspace_attachment_handler, #{}}
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
    State#{http_port => ranch:get_port(talk_record_http_relay), http_prev => Prev}.

gateway_cleanup(State) ->
    fixture_finish(
        gateway_fixture_failure,
        {ok, ok},
        gateway_release(State, maps:get(http_prev, State))
    ).
gateway_release(State, Prev) ->
    fixture_errors(
        [
            fun gateway_stop_listener/0,
            fun() -> ok = talk_record_t07_support:teardown_all(State) end
        ] ++
            [fun() -> ok = gateway_restore(E) end || E <- Prev]
    ).
gateway_stop_listener() ->
    case cowboy:stop_listener(talk_record_http_relay) of
        ok -> ok;
        {error, not_found} -> ok
    end.
gateway_restore({K, undefined}) -> application:unset_env(imboy, K);
gateway_restore({K, {ok, V}}) -> application:set_env(imboy, K, V).
fixture_errors(Funs) ->
    [E || {error, _, _, _} = E <- [fixture_capture(F) || F <- Funs]].
fixture_capture(Fun) ->
    try
        {ok, Fun()}
    catch
        Class:Reason:Stack ->
            {error, Class,
                {redacted_failure_sha256,
                    binary:encode_hex(
                        crypto:hash(sha256, term_to_binary(Reason)), lowercase
                    )},
                [
                    {M, F, fixture_arity(A), []}
                 || {M, F, A, _} <- Stack,
                    is_atom(M),
                    is_atom(F)
                ]}
    end.
fixture_arity(A) when is_integer(A), A >= 0 -> A;
fixture_arity(A) when is_list(A) -> length(A);
fixture_arity(_) -> 0.
fixture_finish(gateway_fixture_failure, {ok, Value}, []) -> Value;
fixture_finish(Tag, Outcome, Errors) -> error({Tag, Outcome, Errors}).

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
    case get(client_audio_pending_observer) of
        Observe when is_function(Observe, 4) -> Observe(State, Rid, Cid, Ref);
        _ -> ok
    end,
    %% S3-only direct 合同（V3-18）：register 返回 grant 描述符，PUT 直达
    %% Garage（逐字 required_headers 含 x-amz-checksum-sha256）。旧 gateway
    %% put_url/pinned 回包通道已随 V3-20 退役。
    Grant = maps:get(<<"grant">>, Ref),
    Url = binary_to_list(maps:get(<<"url">>, Grant)),
    Hs = [
        {binary_to_list(K), binary_to_list(V)}
     || {K, V} <- maps:to_list(maps:get(<<"required_headers">>, Grant))
    ],
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {Url, Hs, "audio/aac", Bytes},
        [{timeout, 15000}],
        [{body_format, binary}]
    ),
    %% 同一授权改写正文 → checksum 与声明不符，S3 PUT 即 400（fail-closed）。
    {ok, {{_, 400, _}, _, _}} = httpc:request(
        put,
        {Url, Hs, "audio/aac", <<Bytes/binary, 0>>},
        [{timeout, 15000}],
        [{body_format, binary}]
    ),
    PartId = maps:get(<<"part_id">>, Part),
    Confirm = post_json(
        CaptureUrl ++ "/audio-parts/" ++ integer_to_list(PartId) ++
            "/confirm",
        Auth,
        #{<<"sha256">> => Sha, <<"byte_size">> => byte_size(Bytes)},
        200
    ),
    ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, Confirm)),
    %% 剥掉已签名 checksum 头再 PUT → Garage「signed header not present」
    %% 400 InvalidRequest（fail-closed deny：篡改授权必拒收）。
    BadHs = [H || {K, _} = H <- Hs, K =/= "x-amz-checksum-sha256"],
    {ok, {{_, 400, _}, _, _}} = httpc:request(
        put,
        {Url, BadHs, "audio/aac", Bytes},
        [{timeout, 15000}],
        []
    ),
    Available = complete_record(
        State, RecordUrl, CaptureUrl, Auth, Device, Cid, Confirm, Sha, byte_size(Bytes), Bytes
    ),
    case get(client_audio_source_fixture) of
        true -> Available;
        _ -> governance_over_http(State)
    end.

create_over_http(State, Uid) ->
    Prefix = "http://127.0.0.1:" ++ integer_to_list(maps:get(http_port, State)),
    Url = Prefix ++ "/api/v1/talk-records",
    Token =
        case get(client_audio_source_fixture) of
            true -> token_ds:encrypt_token(Uid, <<"10000000-0000-0000-0000-000000090001">>);
            _ -> token_ds:encrypt_token(Uid)
        end,
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
    {ok, {{_, Status, _}, _, Response}} = httpc:request(
        post,
        {Url, Headers, "application/json", jsone:encode(Body)},
        [{timeout, 15000}],
        [{body_format, binary}]
    ),
    maps:get(<<"payload">>, jsone:decode(Response)).

complete_record(State, RecordUrl, CaptureUrl, Auth, Device, Cid, Part, Sha, Size, Bytes) ->
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
            ],
            %% no-backend-ffmpeg V1：成品直传合同必填 final_audio——成品即
            %% 唯一 confirmed part 的直传附件（对象键/幂等键由服务端派生）。
            <<"final_audio">> => final_audio_decl(State, Part, Sha, Size, Bytes)
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
    case get(client_audio_source_fixture) of
        true ->
            Aid = maps:get(<<"audio_attachment_id">>, Row),
            %% V1 成品=真实客户端直传附件（非派生副本）——按 attachment 行
            %% 经 frozen_location 有界读回（与 playback_stream 同一读原语）。
            RowA = talk_record_t07_support:one(
                maps:get(conn, State),
                [
                    "SELECT id, size, path, file_hash256,"
                    " storage_driver, storage_namespace, storage_container,"
                    " storage_locator_sha256, storage_physical_key"
                    " FROM attachment WHERE id=",
                    integer_to_list(Aid)
                ]
            ),
            {ok, AssemblyBytes} = attachment_artifact_reader:with_source(
                RowA, 134217728, fun(S) -> spool_bytes(S) end
            ),
            {ok, {{_, 200, _}, LatestH, _}} = httpc:request(
                get,
                {RecordUrl, Auth},
                [{autoredirect, false}],
                [{body_format, binary}]
            ),
            #{
                port => maps:get(http_port, State),
                conn => maps:get(conn, State),
                record_id => Rid,
                auth => Auth,
                assembly_bytes => AssemblyBytes,
                record_etag => list_to_binary(proplists:get_value("etag", LatestH))
            };
        _ ->
            playback_over_http(State, RecordUrl, Auth),
            cleanup_real_artifacts(State, Rid, RecordUrl, Auth)
    end.

%% V3-18 P3 边缘承载契约（有界鉴权壳）：GET 正文一律 302 到边缘
%% `/edge?ticket=<同一票据>`（Location 不泄 Garage 签名/endpoint/bucket/
%% object key）；边缘未配置 → 503 fail-closed（不回退 BEAM spool）。
%% HEAD 保留有界元数据（零字节 + 真实 content-length）；206/416 正文语义
%% 随正文移交边缘。字节完整性由 client_audio_source_fixture 分支
%% frozen_location 读回与边缘域测试覆盖，HTTP 层不再断言 200 正文/206 切片。
playback_over_http(State, RecordUrl, Auth) ->
    Session = post_json(RecordUrl ++ "/playback", Auth, #{}, 200),
    Prefix = "http://127.0.0.1:" ++ integer_to_list(maps:get(http_port, State)),
    Url = Prefix ++ binary_to_list(maps:get(<<"url">>, Session)),
    PrevBase = application:get_env(imboy, base_url),
    PrevEdge = application:get_env(imboy, edge_base_url),
    try
        playback_bounded_shell(Url, Auth, Session, RecordUrl)
    after
        restore_playback_env(base_url, PrevBase),
        restore_playback_env(edge_base_url, PrevEdge)
    end.

restore_playback_env(_K, undefined) -> ok = application:unset_env(imboy, _K);
restore_playback_env(K, {ok, V}) -> ok = application:set_env(imboy, K, V).

playback_bounded_shell(Url, Auth, Session, RecordUrl) ->
    Ticket = maps:get(<<"ticket">>, Session),
    %% 302 有界壳：同一票据重定向边缘；缓存头私有、无 origin 泄露。
    ok = application:set_env(imboy, edge_base_url, <<"http://127.0.0.1:13993/edge">>),
    {ok, {{_, 302, _}, RedirHeaders, _}} = httpc:request(
        get,
        {Url, Auth},
        [{timeout, 15000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ?assertEqual(
        <<"http://127.0.0.1:13993/edge?ticket=", Ticket/binary>>,
        list_to_binary(proplists:get_value("location", RedirHeaders))
    ),
    ?assertEqual(
        <<"private, no-store">>,
        list_to_binary(proplists:get_value("cache-control", RedirHeaders))
    ),
    %% HEAD 有界元数据：零字节正文 + 真实 content-length，不 302。
    {ok, {{_, 200, _}, HeadHeaders, Empty}} = httpc:request(
        head,
        {Url, Auth},
        [],
        [{body_format, binary}]
    ),
    ?assertEqual(<<>>, Empty),
    ?assert(list_to_integer(proplists:get_value("content-length", HeadHeaders)) > 42),
    {ok, {{_, 401, _}, _, _}} = httpc:request(get, {Url, []}, [], []),
    Stranger = token_ds:encrypt_token(maps:get(u7, talk_record_t07_support:ids())),
    {ok, {{_, StrangerStatus, _}, _, _}} = httpc:request(
        get,
        {Url, [{"authorization", "Bearer " ++ binary_to_list(Stranger)}]},
        [],
        []
    ),
    ?assertMatch({true, _}, {lists:member(StrangerStatus, [403, 404]), StrangerStatus}),
    %% 边缘未配置 → fail-closed 503；base_url+/edge 回退语义一并 unset 封死
    %% （worker VM 配置注入 base_url 时防止误走 302）。
    ok = application:unset_env(imboy, edge_base_url),
    ok = application:unset_env(imboy, base_url),
    {ok, {{_, 503, _}, _, _}} = httpc:request(get, {Url, Auth}, [], [{body_format, binary}]),
    %% 多段 range：BEAM 壳拒绝（416 正文语义已移交边缘；deny 默认 403）。
    {ok, {{_, RangeStatus, _}, _, _}} = httpc:request(
        get,
        {Url, [{"range", "bytes=0-1,7-9"} | Auth]},
        [],
        []
    ),
    ?assertMatch({true, _}, {lists:member(RangeStatus, [403, 416]), RangeStatus}),
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
    {ok, {{_, DeniedStatus, _}, _, _}} = httpc:request(
        get,
        {Url, Auth},
        [{autoredirect, false}],
        []
    ),
    ?assertMatch({true, _}, {lists:member(DeniedStatus, [403, 404]), DeniedStatus}).

cleanup_real_artifacts(State, Rid, RecordUrl, Auth) ->
    Conn = maps:get(conn, State),
    {ok, Rows} = elib_pg:query(
        Conn,
        <<
            "SELECT DISTINCT a.path,a.storage_physical_key,a.storage_namespace,"
            "a.storage_container FROM attachment a "
            "JOIN talk_record_attachment_ref r ON r.attachment_id=a.id WHERE r.record_id=$1"
        >>,
        [Rid]
    ),
    %% V1 引用世界：客户端直传成品 + 小 JSON manifest（服务端不再产派生音频副本）。
    ?assertEqual(2, length(Rows)),
    %% direct-v1（00000237）confirm 前会话窗由 open_capability_window_tx 落库为
    %% grant+TTL 未来值；本套件秒级走完 grant→PUT→confirm→delete——清理时窗
    %% 必然仍在未来：资格谓词（00000226）读作 23510 not exhausted → 成品
    %% retained → run_batch 缺位。服务端收窗通道不扩展（R16 实证：两步收窗
    %% 击穿 client_audio 巨型 root「清理不得消费孤儿自然到期」断言，两套件
    %% 合同互斥）。会话行是测试产物且触发器无 DELETE 分支——按 delete_tests
    %% 同款旁路移除（confirmed+registered 全形态，删除协调只消费真实对象）。
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "DELETE FROM attach_pending"
                " WHERE object_key IN (SELECT a.path FROM attachment a"
                " JOIN talk_record_attachment_ref r ON r.attachment_id = a.id"
                " WHERE r.record_id = $1)"
                " AND session_state IN ('confirmed', 'registered')"
            >>,
            [Rid]
        ),
    {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
    Delete = fun(_, _, _, Identity) -> attachment_storage:delete_snapshot(Identity) end,
    %% 能力窗随记录清理关闭（direct-CONFIRMED 会话）——两张收据同批就绪，
    %% 一批清空，无遗留对象。
    {ok, 2} = attachment_delete_worker:run_batch(Conn, Delete),
    Initial = [read_artifact(Row) || Row <- Rows],
    ?assertEqual(2, length([missing || {error, not_found} <- Initial])),
    {ok, {{_, 200, _}, _, PendingBytes}} = httpc:request(
        get,
        {RecordUrl, Auth},
        [],
        [{body_format, binary}]
    ),
    Pending = maps:get(<<"payload">>, jsone:decode(PendingBytes)),
    ?assertNotEqual(<<"complete">>, maps:get(<<"cleanup_status">>, Pending)),
    %% V1 直传合同：CONFIRMED 会话能力窗随清理关闭——队列已空，有界轮询
    %% refresh 推进 cleanup complete（不再依赖 attach_pending 能力到期）。
    AwaitDrain =
        fun Await(N) ->
            {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
            case attachment_delete_worker:run_batch(Conn, Delete) of
                {ok, 0} ->
                    ok;
                _ when N >= 300 -> error(await_drain_timeout);
                _ ->
                    timer:sleep(1000),
                    Await(N + 1)
            end
        end,
    ok = AwaitDrain(0),
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
    %% direct-v1：storage_physical_key 为 NULL 即物理键==path（派生副本经
    %% copy_staging 有独立物理键，按 path 读会假 not_found）。
    Key =
        case maps:get(<<"storage_physical_key">>, Row) of
            null -> maps:get(<<"path">>, Row);
            <<>> -> maps:get(<<"path">>, Row);
            K -> K
        end,
    Driver:read(Ns, maps:get(<<"storage_container">>, Row), Key).

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

%% Test-only extraction: original setup, capture and assembly implementation.
source_descriptor() ->
    {timeout, _, {setup, Setup, Cleanup, _}} = gateway_http_test_(),
    {Setup, Cleanup}.
available_source(State, Observe) ->
    put(client_audio_pending_observer, Observe),
    put(client_audio_source_fixture, true),
    try
        run(State)
    after
        erase(client_audio_source_fixture),
        erase(client_audio_pending_observer)
    end.

%% final_audio 声明：成品=唯一 confirmed part 的直传附件。object_key 取
%% attachment.path（与 attach_pending.session_object_key 一致），幂等键由
%% part_id 派生（talk_record_part_upload:client_upload_id/1）；样本轴从
%% 夹具字节实测（服务器会重扫 ADTS 逐项比对）。
final_audio_decl(State, Part, Sha, Size, Bytes) ->
    Aid0 = maps:get(<<"attachment_id">>, Part),
    PartId = maps:get(<<"part_id">>, Part),
    #{<<"path">> := FinalKey} = talk_record_t07_support:one(
        maps:get(conn, State),
        ["SELECT path FROM attachment WHERE id=", integer_to_list(Aid0)]
    ),
    {Rate, Channels, Samples, DurationMs} = adts_axis(Bytes),
    #{
        <<"object_key">> => FinalKey,
        <<"client_upload_id">> => talk_record_part_upload:client_upload_id(PartId),
        <<"sha256">> => Sha,
        <<"byte_size">> => Size,
        <<"sample_rate">> => Rate,
        <<"channels">> => Channels,
        <<"sample_count">> => Samples,
        <<"duration_ms">> => DurationMs,
        <<"attachment_id">> => Aid0
    }.

%% ADTS 全帧扫描：返回 {SampleRate, Channels, Samples, DurationMs}。
%% 帧头结构规则与 talk_record_adts.dart / 服务端 talk_record_audio_validate 一致。
adts_axis(Bin) ->
    adts_axis(Bin, 0, 0, 0, undefined).
adts_axis(Bin, Off, Frames, Samples, Axis) when Off + 7 =< byte_size(Bin) ->
    <<_:Off/binary, 16#FF, B2, B3, B4, B5, B6, _/binary>> = Bin,
    %% syncword + layer 0 + 无保护错误
    true = B2 band 16#F6 =:= 16#F0,
    %% B3=d2：profile(2)+采样率索引(4)+private(1)+声道高位(1)；B4=d3：声道
    %% 低 2 位+帧长高 2 位；B5=d4/B6=d5：帧长中/低 8+3 位。
    Rate = rate_of((B3 bsr 2) band 16#F),
    Channels = ((B3 bsr 1) band 1) bsl 2 bor ((B4 bsr 6) band 3),
    HLen =
        case B2 band 1 of
            1 -> 7;
            _ -> 9
        end,
    %% bor/bsl 同级左结合：中项必须括号，否则 ((x bor B5) bsl 3) 帧长即错。
    FrameLen = ((B4 band 3) bsl 11) bor (B5 bsl 3) bor ((B6 bsr 5) band 7),
    true = FrameLen >= HLen andalso Off + FrameLen =< byte_size(Bin),
    true = Axis =:= undefined orelse Axis =:= {Rate, Channels},
    adts_axis(Bin, Off + FrameLen, Frames + 1, Samples + 1024, {Rate, Channels});
adts_axis(Bin, Off, _Frames, Samples, {Rate, Channels}) when Off =:= byte_size(Bin) ->
    Duration = (Samples * 1000 + Rate div 2) div Rate,
    {Rate, Channels, Samples, Duration}.

rate_of(0) -> 96000;
rate_of(1) -> 88200;
rate_of(2) -> 64000;
rate_of(3) -> 48000;
rate_of(4) -> 44100;
rate_of(5) -> 32000;
rate_of(6) -> 24000;
rate_of(7) -> 22050;
rate_of(8) -> 16000;
rate_of(9) -> 12000;
rate_of(10) -> 11025;
rate_of(11) -> 8000;
rate_of(12) -> 7350.

%% spool 句柄整体读回（分块 read_at 累积）。
spool_bytes(S) ->
    Size = attachment_upload_spool:size(S),
    spool_bytes(S, 0, Size, []).
spool_bytes(_S, Off, Size, Acc) when Off >= Size ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
spool_bytes(S, Off, Size, Acc) ->
    case attachment_upload_spool:read_at(S, Off, 65536) of
        eof -> {ok, iolist_to_binary(lists:reverse(Acc))};
        {ok, B} -> spool_bytes(S, Off + byte_size(B), Size, [B | Acc]);
        Error -> Error
    end.
