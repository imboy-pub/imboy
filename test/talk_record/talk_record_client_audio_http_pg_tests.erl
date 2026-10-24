%% REVIEW DRAFT ONLY. Explicit checks/0; controller supplies an owned marker DB.
%% Never calls a supplier. Every HTTP request is forced to the same loopback port.
-module(talk_record_client_audio_http_pg_tests).
-export([checks/0, checks/1]).
-include_lib("eunit/include/eunit.hrl").
-include("cache.hrl").

client_audio_http_pg_test_() -> checks().

%% Explicit controller entry; requires isolated fresh VM and identity-bound marker
%% DB, exactly as gateway's owned wrapper. No skip branch or empty test list.
checks() ->
    {Setup, Cleanup} = talk_record_gateway_http_tests:source_descriptor(),
    {timeout, 4200,
        {setup, fun() -> setup(Setup, Cleanup) end,
            fun(#{state := S, prev := Prev}) ->
                cleanup_all(S, Cleanup, Prev)
            end,
            fun(#{state := S, signing_key := Key}) ->
                {timeout, 4100,
                    ?_test(begin
                        Outcome = capture(fun() ->
                            F = talk_record_gateway_http_tests:available_source(
                                S,
                                fun(_, Rid, Cid, Ref) -> observe_pending(S, Rid, Cid, Ref) end
                            ),
                            run(F#{signing_key => Key})
                        end),
                        finish(Outcome, capture(fun() -> cleanup_source(S) end))
                    end)}
            end}}.
setup(Setup, Cleanup) ->
    finish(capture(fun() -> setup_owned(Setup, Cleanup) end), {ok, ok}).
setup_owned(Setup, Cleanup) ->
    Prev = [
        {K, application:get_env(imboy, K)}
     || K <-
            [talk_record_client_processing, base_url, jwt_key]
    ],
    undefined = whereis(?DEPCACHE_SERVER),
    ok = preflight(),
    case capture(Setup) of
        {ok, S0} -> setup_ledger(S0, Cleanup, Prev);
        Failure -> error({setup_state_unavailable, Failure})
    end.
setup_ledger(S0, Cleanup, Prev) ->
    case
        capture(fun() ->
            true = is_map(S0),
            ets:new(client_audio_pending, [public, set])
        end)
    of
        {ok, Table} ->
            S = S0#{pending_ledger => Table},
            setup_cache(S, Cleanup, Prev);
        Failure ->
            finish(Failure, release_result(S0, Cleanup, Prev))
    end.
setup_cache(S, Cleanup, Prev) ->
    case capture(fun start_owned_cache/0) of
        {ok, Pid} ->
            Owned = S#{owned_cache_pid => Pid, owned_cache_owner => self()},
            case capture(fun() -> initialize_client(Owned, Prev) end) of
                {ok, Context} -> Context;
                Failure -> finish(Failure, release_result(Owned, Cleanup, Prev))
            end;
        Failure ->
            finish(Failure, release_result(S, Cleanup, Prev))
    end.
start_owned_cache() ->
    %% start_link can adopt an existing PID: absence alone is insufficient proof.
    undefined = whereis(?DEPCACHE_SERVER),
    {ok, Pid} = imboy_cache:start_link([{depcache_memory_max, 10}]),
    true = cache_owned_by(Pid, self()),
    Pid.
cache_owned_by(Pid, Owner) ->
    case {process_info(Pid, dictionary), process_info(Owner, links)} of
        {{dictionary, Dict}, {links, Links}} ->
            case proplists:get_value('$ancestors', Dict) of
                [Owner | _] -> lists:member(Pid, Links);
                _ -> false
            end;
        _ ->
            false
    end.
stop_owned_cache(S) ->
    case maps:find(owned_cache_pid, S) of
        error ->
            ok;
        {ok, Pid} ->
            Owner = maps:get(owned_cache_owner, S),
            true = Owner =:= self(),
            case is_process_alive(Pid) of
                false ->
                    ok;
                true ->
                    true = cache_owned_by(Pid, Owner),
                    ok = gen_server:stop(Pid, normal, 5000)
            end
    end.
initialize_client(S, Prev) ->
    Key = crypto:strong_rand_bytes(32),
    ok = application:set_env(imboy, jwt_key, crypto:strong_rand_bytes(32)),
    ok = application:set_env(imboy, base_url, <<"https://client-audio-fixture.invalid">>),
    ok = application:set_env(imboy, talk_record_client_processing, #{
        enabled => true,
        doubao_enabled => true,
        audio_source_enabled => true,
        personal_enabled => true,
        signing_key => Key,
        policy_version => <<"synthetic-client-audio-v1">>
    }),
    Uid = maps:get(u1, talk_record_t07_support:ids()),
    Did = <<"10000000-0000-0000-0000-000000090001">>,
    {ok, _} = user_device_ds:save(elib_dt:now(), Uid, Did, #{
        <<"cos">> => <<"android">>, <<"dname">> => <<"synthetic-client-audio">>
    }),
    true = user_device_ds:is_active(Uid, Did),
    {ok, Epoch} = auth_session_ds:current_epoch(Uid),
    true = Epoch > 0,
    #{state => S, prev => Prev, signing_key => Key}.
preflight() ->
    {ok, _} = application:ensure_all_started(epgsql),
    R = fun talk_record_owned_eunit_fixture:required/1,
    {ok, C} = epgsql:connect(#{
        host => R("T05_PG_HOST"),
        port => list_to_integer(R("T05_PG_PORT")),
        database => R("T05_PG_DB"),
        username => R("T05_PG_USER"),
        password => R("T05_PG_PASSWORD"),
        ssl => false,
        timeout => 10000
    }),
    try
        talk_record_owned_eunit_fixture:assert_owned_database(
            C,
            list_to_binary(R("T05_PG_DB"))
        )
    after
        epgsql:close(C)
    end.
restore(Prev) ->
    lists:foreach(
        fun
            ({K, undefined}) -> application:unset_env(imboy, K);
            ({K, {ok, V}}) -> application:set_env(imboy, K, V)
        end,
        Prev
    ).

%% F: #{port,record_id,auth,record_etag,assembly_bytes,signing_key,conn}.
%% auth MUST contain a real JWT with current_did matching an active real device.
%% Record MUST be a fresh available assembly produced by gateway capture/complete.
%% Policy configured before fixture creation: enabled/doubao_enabled/audio_source_enabled.
checks(F) ->
    {inorder, [?_test(finish(capture(fun() -> run(F) end), {ok, ok}))]}.

run(F) ->
    Public = talk_record_client_policy:public(),
    Provider = maps:get(<<"transcribe">>, maps:get(<<"providers">>, Public)),
    <<"doubao">> = maps:get(<<"provider">>, Provider),
    Pin = (maps:with([<<"policy_version">>, <<"config_version">>], Public))#{
        <<"execution_mode">> => <<"client_byok">>,
        <<"provider">> => maps:get(<<"provider">>, Provider),
        <<"model">> => maps:get(<<"model">>, Provider)
    },
    Consent = Pin#{<<"confirmed">> => true},
    Record = "/api/v1/talk-records/" ++ integer_to_list(maps:get(record_id, F)),
    Auth = maps:get(auth, F),
    Created = post(
        F,
        Record ++ "/processing-requests",
        Auth,
        maps:get(record_etag, F),
        "draft-processing",
        Pin#{
            <<"mode">> => <<"transcribe">>, <<"processing_consent">> => Consent
        }
    ),
    Job = Record ++ "/jobs/" ++ integer_to_list(maps:get(<<"job_id">>, Created)),
    Claim = post(
        F,
        Job ++ "/client-executions",
        Auth,
        maps:get(record_etag, F),
        "draft-claim",
        #{<<"processing_consent">> => Consent}
    ),
    RunId = maps:get(<<"id">>, Claim),
    RunPath = Job ++ "/client-executions/" ++ integer_to_list(RunId),
    RH = [{"x-talk-run-token", binary_to_list(maps:get(<<"run_token">>, Claim))} | Auth],
    Prepared = post(
        F,
        RunPath ++ "/provider-tasks",
        RH,
        maps:get(<<"etag">>, Claim),
        "draft-prepare",
        #{}
    ),
    Task = maps:get(<<"task">>, Prepared),
    Submit = post(
        F,
        RunPath ++ "/provider-tasks/" ++
            integer_to_list(maps:get(<<"id">>, Task)) ++ "/submit-intents",
        RH,
        maps:get(<<"etag">>, Task),
        "draft-submit",
        #{
            <<"profile_hash">> => maps:get(<<"profile_hash">>, Task)
        }
    ),
    true = maps:get(<<"may_submit">>, Submit),
    Source = source_path(maps:get(<<"source_url">>, Submit)),
    Bytes = maps:get(assembly_bytes, F),
    positive(F, Source, Bytes),
    Base = "/api/v1/talk-records/client-audio-source",
    denied(F, Base),
    denied(F, Base ++ "?ticket=forged"),
    denied(F, Source ++ "&ticket=duplicate"),
    denied(F, Base ++ "?ticket=" ++ binary_to_list(expired_ticket(F, Source))),
    {405, _, <<>>} = request(F, post, Source, [], <<>>),
    %% Mutations are scoped to this owned fixture. Each commits before HTTP;
    %% rollback-only transactions cannot be seen by the listener's PG connection.
    C = maps:get(conn, F),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_client_run SET lease_until=clock_timestamp()-interval '1 second' WHERE id=$1">>,
        [RunId]
    ),
    positive(F, Source, Bytes),
    %% Durable task cancellation should revoke this source even with valid signature.
    {ok, 1} = talk_record_provider_repo:cancel_tx(C, maps:get(<<"id">>, Task)),
    denied(F, Source),
    ok.

positive(F, Path, Bytes) ->
    H = positive_stage(get_response, fun() ->
        {200, Headers, Bytes} = request(F, get, Path, [], <<>>),
        Headers
    end),
    positive_stage(get_headers, fun() ->
        ?assertEqual("audio/aac", proplists:get_value("content-type", H)),
        ?assertEqual("private, no-store", proplists:get_value("cache-control", H))
    end),
    HH = positive_stage(head_response, fun() ->
        {200, Headers, <<>>} = request(F, head, Path, [], <<>>),
        Headers
    end),
    positive_stage(head_length, fun() ->
        ?assertEqual(integer_to_list(byte_size(Bytes)), proplists:get_value("content-length", HH))
    end),
    {HR, Slice} = positive_stage(range_response, fun() ->
        {206, Headers, Part} = request(F, get, Path, [{"range", "bytes=7-42"}], <<>>),
        {Headers, Part}
    end),
    positive_stage(range_content, fun() ->
        ?assertEqual(binary:part(Bytes, 7, 36), Slice),
        ?assertEqual(
            "bytes 7-42/" ++ integer_to_list(byte_size(Bytes)),
            proplists:get_value("content-range", HR)
        )
    end).

positive_stage(Stage, Fun) ->
    try
        Fun()
    catch
        Class:Reason:Stack ->
            Hash = binary:encode_hex(
                crypto:hash(sha256, term_to_binary({Class, Reason})), lowercase
            ),
            erlang:raise(Class, {audio_positive_stage, Stage, Hash}, Stack)
    end.

safe_positive_stage(Stage) ->
    lists:member(Stage, [
        get_response,
        get_headers,
        head_response,
        head_length,
        range_response,
        range_content
    ]).

-ifdef(CLIENT_AUDIO_DIAGNOSTIC_UNIT).
positive_stage_redaction_test() ->
    Secret = <<"synthetic-ticket-header-body-audio">>,
    {error, error, Safe, _} = capture(fun() ->
        positive_stage(head_length, fun() -> error({unsafe_payload, Secret}) end)
    end),
    ?assertMatch({audio_positive_stage, head_length, _}, Safe),
    ?assertEqual(Safe, safe_reason(Safe)),
    ?assertEqual(nomatch, binary:match(term_to_binary(Safe), Secret)),
    lists:foreach(
        fun(Bad) ->
            ?assertMatch({redacted_failure_sha256, _}, safe_reason(Bad))
        end,
        [
            {audio_positive_stage, arbitrary_stage, binary:copy(<<"a">>, 64)},
            {audio_positive_stage, Secret, binary:copy(<<"a">>, 64)},
            {audio_positive_stage, head_length, Secret},
            {audio_positive_stage, head_length, binary:copy(<<"a">>, 64), Secret}
        ]
    ).

-endif.

denied(F, Path) ->
    lists:foreach(fun(M) -> {403, _, <<>>} = request(F, M, Path, [], <<>>) end, [get, head]).

expired_ticket(F, Source) ->
    #{query := Q} = uri_string:parse(list_to_binary(Source)),
    [{<<"ticket">>, T}] = uri_string:dissect_query(Q),
    Key = crypto:mac(
        hmac,
        sha256,
        maps:get(signing_key, F),
        <<"imboy:talk-record-client-audio:v1">>
    ),
    {ok, Claims} = imboy_jwt:verify(T, Key),
    Now = erlang:system_time(second),
    imboy_jwt:sign(
        Claims#{
            <<"iat">> => Now - 301,
            <<"exp">> => Now - 1,
            <<"source_issued_at">> => Now - 301,
            <<"source_expires_at">> => Now - 1
        },
        Key
    ).
source_path(URL) ->
    #{
        scheme := <<"https">>,
        path := <<"/api/v1/talk-records/client-audio-source">>,
        query := Q
    } = uri_string:parse(URL),
    %% Listener-only replay of path/query; never contact configured HTTPS origin.
    "/api/v1/talk-records/client-audio-source?" ++ binary_to_list(Q).
post(F, Path, Auth, ETag, Idem, Body) ->
    {202, _, Bytes} = request(
        F,
        post,
        Path,
        [
            {"if-match", binary_to_list(talk_record_http:quote_etag(ETag))},
            {"idempotency-key", Idem}
            | Auth
        ],
        jsone:encode(Body)
    ),
    maps:get(<<"payload">>, jsone:decode(Bytes)).
request(F, Method, Path, Headers, Body) ->
    true = lists:prefix("/api/v1/talk-records", Path),
    URL = "http://127.0.0.1:" ++ integer_to_list(maps:get(port, F)) ++ Path,
    Req =
        case Method of
            post -> {URL, Headers, "application/json", Body};
            _ -> {URL, Headers}
        end,
    {ok, {{_, Code, _}, H, B}} = httpc:request(
        Method,
        Req,
        [{autoredirect, false}, {timeout, 15000}],
        [{body_format, binary}]
    ),
    {Code, H, B}.

%% Cleanup uses durable owned-record identity even if available_source raised
%% before returning F. It never guesses a prior artifact count or deletes files.
cleanup_source(S) ->
    C = maps:get(conn, S),
    Uid = maps:get(u1, talk_record_t07_support:ids()),
    {ok, Rows} = elib_pg:query(
        C,
        <<
            "SELECT id,revision FROM talk_record WHERE owner_user_id=$1"
            " AND scope='personal' AND client_recording_id=$2"
        >>,
        [Uid, <<"00000000-0000-0000-0000-000000090001">>]
    ),
    DiscoveryResults = [
        capture(fun() -> discover_pending(S, Rid) end)
     || #{<<"id">> := Rid} <- Rows
    ],
    DeleteResults = [
        capture(fun() ->
            #{<<"id">> := Rid, <<"revision">> := Rev} = Row,
            {ok, _, _} = talk_record_delete_logic:delete(Uid, Rid, integer_to_binary(Rev))
        end)
     || Row <- Rows
    ],
    Identities = [I || {_, I} <- ets:tab2list(maps:get(pending_ledger, S))],
    %% Phase 1 waits on original PG expiry; phase 2 has its own drain budget.
    %% No record drain may consume an orphan's wait/retirement opportunity.
    PendingResults = [
        capture(fun() ->
            await_original_expiry(S, I),
            case pending_only(S, I) of
                true -> retire_pending(S, I, erlang:monotonic_time(millisecond) + 120000);
                false -> ok
            end
        end)
     || I <- Identities
    ],
    RecordResults = [
        capture(fun() ->
            drain_cleanup(C, Rid, erlang:monotonic_time(millisecond) + 120000)
        end)
     || #{<<"id">> := Rid} <- Rows
    ],
    ReceiptResults = [
        capture(fun() ->
            retire_pending(S, I, erlang:monotonic_time(millisecond) + 120000)
        end)
     || I <- Identities
    ],
    case
        [
            E
         || {error, _, _, _} = E <-
                DiscoveryResults ++ DeleteResults ++
                    PendingResults ++ RecordResults ++ ReceiptResults
        ]
    of
        [] -> ok;
        Errors -> error({cleanup_failures, Errors})
    end.

drain_cleanup(C, Rid, Deadline) ->
    true = erlang:monotonic_time(millisecond) < Deadline,
    {ok, _} = talk_record_job_worker:run_once(),
    {ok, _} = attachment_delete_worker:run_batch(
        C,
        fun(_, _, _, Identity) -> attachment_storage:delete_snapshot(Identity) end
    ),
    {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
    {ok, [#{<<"cleanup_status">> := Status}]} = elib_pg:query(
        C,
        <<"SELECT cleanup_status FROM talk_record WHERE id=$1">>,
        [Rid]
    ),
    case Status of
        <<"complete">> ->
            verify_objects_gone(C, Rid);
        <<"failed">> ->
            error({source_cleanup_failed, Rid});
        _ ->
            timer:sleep(1000),
            drain_cleanup(C, Rid, Deadline)
    end.
verify_objects_gone(C, Rid) ->
    {ok, Rows} = elib_pg:query(
        C,
        <<
            "SELECT DISTINCT a.path,a.storage_namespace,a.storage_container"
            " FROM attachment a LEFT JOIN talk_record_attachment_ref r ON r.attachment_id=a.id"
            " WHERE r.record_id=$1 OR (a.scope='talk_record' AND a.scope_ref=$1::text)"
        >>,
        [Rid]
    ),
    lists:foreach(
        fun(Row) ->
            Ns = maps:get(<<"storage_namespace">>, Row),
            {ok, Root} = attachment_storage:resolve(Ns),
            Driver = attachment_storage:driver_module(maps:get(driver, Root)),
            {error, not_found} = Driver:read(
                Ns,
                maps:get(<<"storage_container">>, Row),
                maps:get(<<"path">>, Row)
            )
        end,
        Rows
    ).
capture(Fun) ->
    try
        {ok, Fun()}
    catch
        Class:Reason:Stack ->
            {error, Class, safe_reason(Reason), safe_stack(Stack)}
    end.

%% Never copy arbitrary reason terms, arguments, file paths or metadata.
safe_reason(Reason) ->
    case safe_internal_reason(Reason, 0) of
        true ->
            Reason;
        false ->
            {redacted_failure_sha256,
                binary:encode_hex(
                    crypto:hash(sha256, term_to_binary(Reason)), lowercase
                )}
    end.

%% Structural whitelist, not tag recognition: no arbitrary payload is retained.
%% Bound recursion; every leaf must be a 64-byte lowercase SHA256 hex digest.
safe_internal_reason(_, Depth) when Depth > 16 -> false;
safe_internal_reason({redacted_failure_sha256, Hash}, _) ->
    safe_hash(Hash);
safe_internal_reason({audio_positive_stage, Stage, Hash}, _) ->
    safe_positive_stage(Stage) andalso safe_hash(Hash);
safe_internal_reason({Tag, Errors}, Depth) when
    Tag =:= cleanup_failures;
    Tag =:= source_fixture_cleanup_errors;
    Tag =:= setup_cleanup_failures
->
    safe_errors(Errors, Depth + 1);
safe_internal_reason({Tag, Original, Errors}, Depth) when
    Tag =:= gateway_fixture_failure; Tag =:= gateway_state_unavailable
->
    (Original =:= {ok, ok} orelse safe_error(Original, Depth + 1)) andalso
        (Errors =:= [] orelse safe_errors(Errors, Depth + 1));
safe_internal_reason({setup_state_unavailable, Original}, Depth) ->
    safe_error(Original, Depth + 1);
safe_internal_reason({test_and_cleanup_failed, Original, Cleanup}, Depth) ->
    safe_error(Original, Depth + 1) andalso safe_error(Cleanup, Depth + 1);
safe_internal_reason(_, _) ->
    false.

safe_errors([Error | Rest], Depth) ->
    safe_error(Error, Depth) andalso safe_errors_tail(Rest, Depth);
safe_errors(_, _) ->
    false.
safe_errors_tail([], _) ->
    true;
safe_errors_tail([Error | Rest], Depth) ->
    safe_error(Error, Depth) andalso safe_errors_tail(Rest, Depth);
safe_errors_tail(_, _) ->
    false.
safe_error({error, Class, Reason, Stack}, Depth) when
    Class =:= error; Class =:= exit; Class =:= throw
->
    safe_internal_reason(Reason, Depth) andalso safe_frames(Stack);
safe_error(_, _) ->
    false.

safe_hash(Hash) when is_binary(Hash), byte_size(Hash) =:= 64 ->
    lists:all(
        fun(Byte) ->
            (Byte >= $0 andalso Byte =< $9) orelse
                (Byte >= $a andalso Byte =< $f)
        end,
        binary_to_list(Hash)
    );
safe_hash(_) ->
    false.
safe_frames([]) ->
    true;
safe_frames([{Module, Function, Arity, Position} | Rest]) when
    is_atom(Module), is_atom(Function), is_integer(Arity), Arity >= 0
->
    safe_line(Position) andalso safe_frames(Rest);
safe_frames(_) ->
    false.
safe_line([]) -> true;
safe_line([{line, Line}]) when is_integer(Line), Line > 0 -> true;
safe_line(_) -> false.
safe_stack(Stack) ->
    [
        {Module, Function, safe_arity(Args), safe_position(Info)}
     || {Module, Function, Args, Info} <- Stack,
        is_atom(Module),
        is_atom(Function)
    ].
safe_arity(Arity) when is_integer(Arity), Arity >= 0 -> Arity;
safe_arity(Args) when is_list(Args) -> length(Args);
safe_arity(_) -> 0.
safe_position(Info) when is_list(Info) ->
    case proplists:get_value(line, Info) of
        Line when is_integer(Line), Line > 0 -> [{line, Line}];
        _ -> []
    end;
safe_position(_) ->
    [].

finish({ok, Value}, {ok, _}) -> Value;
finish({error, Class, Reason, Stack}, {ok, _}) -> erlang:raise(Class, Reason, Stack);
finish({ok, _}, {error, Class, Reason, Stack}) -> erlang:raise(Class, Reason, Stack);
finish(Original, CleanupError) -> error({test_and_cleanup_failed, Original, CleanupError}).
cleanup_all(S, Cleanup, Prev) ->
    finish({ok, ok}, release_result(S, Cleanup, Prev)).
release_result(S, Cleanup, Prev) ->
    %% Always attempt every layer; delete ETS only after all lifecycle reads.
    Source =
        case maps:is_key(pending_ledger, S) of
            true -> [capture(fun() -> cleanup_source(S) end)];
            false -> []
        end,
    Results =
        Source ++
            [
                capture(fun() -> Cleanup(S) end),
                capture(fun() -> stop_owned_cache(S) end)
            ] ++
            [capture(fun() -> restore([E]) end) || E <- Prev] ++
            [capture(fun() -> delete_ledger(S) end)],
    case [E || {error, _, _, _} = E <- Results] of
        [] -> {ok, ok};
        Errors -> {error, error, {source_fixture_cleanup_errors, Errors}, []}
    end.
delete_ledger(S) ->
    case maps:find(pending_ledger, S) of
        error ->
            ok;
        {ok, Table} ->
            case ets:info(Table, owner) of
                undefined ->
                    ok;
                Owner when Owner =:= self() ->
                    true = ets:delete(Table),
                    ok;
                _ ->
                    error(ledger_owner_mismatch)
            end
    end.

%% Freeze identity immediately after registration, before PUT/confirm.
observe_pending(S, Rid, Cid, Ref) ->
    [I] = pending_rows(S, Rid, maps:get(<<"object_key">>, Ref)),
    ?assertEqual(Cid, capture_for_pending(S, Rid, I)),
    remember_pending(S, I#{record_id => Rid, capture_id => Cid}).

discover_pending(S, Rid) ->
    %% Response-loss recovery is restricted by owned marker record + real owner
    %% and registration subject/declaration; no global pending sweep.
    lists:foreach(
        fun(I) ->
            remember_pending(
                S,
                I#{record_id => Rid, capture_id => capture_for_pending(S, Rid, I)}
            )
        end,
        pending_rows(S, Rid, null)
    ).

capture_for_pending(S, Rid, I) ->
    {ok, Parts} = elib_pg:query(
        maps:get(conn, S),
        <<"SELECT id,capture_id FROM talk_record_audio_part WHERE record_id=$1">>,
        [Rid]
    ),
    [Cid] = [
        C
     || #{<<"id">> := Pid, <<"capture_id">> := C} <- Parts,
        talk_record_part_upload:client_upload_id(Pid) =:= maps:get(<<"client_upload_id">>, I)
    ],
    Cid.

pending_rows(S, Rid, Key) ->
    Uid = maps:get(u1, talk_record_t07_support:ids()),
    {ok, Rows} = elib_pg:query(
        maps:get(conn, S),
        <<
            "SELECT object_key,creator_user_id,scope,client_upload_id,upload_expires_at,"
            "storage_driver,storage_namespace,storage_container,storage_locator_sha256,"
            "encode(digest(upload_registration::text,'sha256'),'hex') AS registration_hash,"
            "ceil(extract(epoch FROM (upload_expires_at-clock_timestamp())))::bigint AS remaining "
            "FROM attach_pending WHERE creator_user_id=$1 AND scope='talk_record' "
            "AND upload_registration->'subject'->>'principal'=$1::text "
            "AND upload_registration->'declaration'->>'scope_ref'=$2::text "
            "AND ($3::text IS NULL OR object_key=$3)"
        >>,
        [Uid, Rid, Key]
    ),
    Rows.

remember_pending(S, I0) ->
    Remaining = maps:get(<<"remaining">>, I0),
    true = is_integer(Remaining),
    true = Remaining =< 3700,
    lists:foreach(
        fun(K) ->
            V = maps:get(K, I0),
            true = is_binary(V),
            true = byte_size(V) > 0
        end,
        [
            <<"storage_driver">>,
            <<"storage_namespace">>,
            <<"storage_container">>,
            <<"storage_locator_sha256">>,
            <<"registration_hash">>
        ]
    ),
    I = maps:remove(<<"remaining">>, I0),
    Key = maps:get(<<"object_key">>, I),
    T = maps:get(pending_ledger, S),
    case ets:lookup(T, Key) of
        [] ->
            ets:insert(
                T,
                {Key, I#{
                    deadline => erlang:monotonic_time(millisecond) +
                        (max(0, Remaining) + 90) * 1000
                }}
            );
        [{Key, Frozen}] ->
            ?assertEqual(maps:remove(deadline, Frozen), I)
    end,
    ok.

retire_pending(S, I, DrainDeadline) ->
    C = maps:get(conn, S),
    Key = maps:get(<<"object_key">>, I),
    true = erlang:monotonic_time(millisecond) < DrainDeadline,
    [Current0] = pending_rows(S, maps:get(record_id, I), Key),
    ?assertEqual(
        maps:without([deadline, capture_id, record_id], I),
        maps:remove(<<"remaining">>, Current0)
    ),
    {ok, Assets} = elib_pg:query(C, <<"SELECT id FROM attachment WHERE path=$1">>, [Key]),
    {ok, Queue} = elib_pg:query(
        C,
        <<
            "SELECT job_state,completed_at,storage_driver,storage_namespace,storage_container,"
            "storage_locator_sha256 FROM attachment_object_deletion WHERE path=$1"
        >>,
        [Key]
    ),
    case Queue of
        [] when Assets =:= [] ->
            case maps:get(<<"remaining">>, Current0) < 0 of
                true ->
                    ok = attach_pending_repo:intent_pending_delete(
                        Key,
                        <<"sweep">>,
                        <<"owned_client_audio_fixture_cleanup">>
                    );
                false ->
                    ok
            end;
        %% Confirmed attachments use record delete/purge lifecycle.
        [] ->
            ok;
        [Receipt] ->
            Fields = [
                <<"storage_driver">>,
                <<"storage_namespace">>,
                <<"storage_container">>,
                <<"storage_locator_sha256">>
            ],
            ?assertEqual(maps:with(Fields, I), maps:with(Fields, Receipt))
    end,
    {ok, _} = attachment_delete_worker:run_batch(
        C,
        fun(_, _, _, Identity) -> attachment_storage:delete_snapshot(Identity) end
    ),
    {ok, Done} = elib_pg:query(
        C,
        <<
            "SELECT job_state,completed_at,storage_driver,storage_namespace,storage_container,"
            "storage_locator_sha256 FROM attachment_object_deletion WHERE path=$1"
        >>,
        [Key]
    ),
    case Done of
        [#{<<"job_state">> := <<"done">>, <<"completed_at">> := At} = Receipt2] when
            At =/= null
        ->
            Fields2 = [
                <<"storage_driver">>,
                <<"storage_namespace">>,
                <<"storage_container">>,
                <<"storage_locator_sha256">>
            ],
            ?assertEqual(maps:with(Fields2, I), maps:with(Fields2, Receipt2)),
            Ns = maps:get(<<"storage_namespace">>, I),
            {ok, Root} = attachment_storage:resolve(Ns),
            Driver = attachment_storage:driver_module(maps:get(driver, Root)),
            {error, not_found} = Driver:read(Ns, maps:get(<<"storage_container">>, I), Key);
        _ ->
            timer:sleep(1000),
            retire_pending(S, I, DrainDeadline)
    end.

pending_only(S, I) ->
    {ok, Assets} = elib_pg:query(
        maps:get(conn, S),
        <<"SELECT id FROM attachment WHERE path=$1">>,
        [maps:get(<<"object_key">>, I)]
    ),
    Assets =:= [].

await_original_expiry(S, I) ->
    [Current0] = pending_rows(S, maps:get(record_id, I), maps:get(<<"object_key">>, I)),
    ?assertEqual(
        maps:without([deadline, capture_id, record_id], I),
        maps:remove(<<"remaining">>, Current0)
    ),
    Remaining = maps:get(<<"remaining">>, Current0),
    true = is_integer(Remaining),
    case Remaining < 0 of
        %% Already naturally expired: never reject retirement on an old wait budget.
        true ->
            ok;
        false ->
            true = erlang:monotonic_time(millisecond) < maps:get(deadline, I),
            timer:sleep(1000),
            await_original_expiry(S, I)
    end.
