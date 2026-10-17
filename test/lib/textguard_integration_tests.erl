-module(textguard_integration_tests).

-moduledoc "T19 runs in its own VM with task-owned PostgreSQL and loopback Rust ports. "
"The real channel is configured through grpcbox client.channels; grpc_transport is omitted "
"so all RPCs use the production default bridge. Publication has its own HTTP base URL. "
"The supervised restart case checks a real Info RPC after restart. "
"T23 remains open for grpcbox wire buffering: a 256 KiB pre-decode check is not a receive-buffer cap.".

-include_lib("eunit/include/eunit.hrl").

-export([manifest/0]).

manifest() ->
    Core = [
        entry(<<"AC-060/", (maps:get(id, Case))/binary>>, core, Case, [<<"AC-060">>, <<"AC-077">>])
     || Case <- textguard_golden:cases()
    ],
    [
        entry(
            <<"AC-077/supervised-channel-restart">>,
            supervised_channel,
            none,
            [<<"AC-060">>, <<"AC-077">>]
        )
    ] ++ Core ++ fault_manifest(http) ++ fault_manifest(grpc) ++
        [
            entry(<<"AC-061/http-wrong-token">>, auth, {http, wrong}, [<<"AC-061">>]),
            entry(<<"AC-061/grpc-wrong-token">>, auth, {grpc, wrong}, [<<"AC-061">>]),
            entry(<<"AC-061/http-wrong-scope">>, auth, {http, publish}, [<<"AC-061">>]),
            entry(<<"AC-061/grpc-wrong-scope">>, auth, {grpc, publish}, [<<"AC-061">>]),
            entry(<<"AC-061/manage-wrong-token">>, manage_auth, wrong, [<<"AC-061">>]),
            entry(<<"AC-061/manage-wrong-scope">>, manage_auth, check, [<<"AC-061">>]),
            entry(<<"AC-077/empty-state-contract">>, empty_state, none, [<<"AC-077">>]),
            entry(<<"AC-077/info-dual-protocol">>, info, none, [<<"AC-060">>, <<"AC-077">>]),
            entry(<<"AC-077/stale-etag-rejected">>, stale_etag, none, [<<"AC-077">>]),
            entry(<<"AC-077/new-pg-revision">>, new_revision, none, [<<"AC-077">>]),
            entry(<<"AC-076/saved-pending">>, saved_pending, none, [<<"AC-076">>]),
            entry(<<"AC-077/independent-publication-endpoint">>, independent_endpoint, none, [
                <<"AC-077">>
            ])
        ].

entry(Id, Kind, Data, Acceptance) ->
    #{id => Id, kind => Kind, data => Data, acceptance => Acceptance}.

fault_manifest(Protocol) ->
    Name = atom_to_binary(Protocol),
    Missing = [
        request_id,
        ruleset_revision,
        generation,
        normalization_id,
        t2s_table_id,
        semantics_id,
        base_rules_digest,
        decision,
        scan_complete,
        hits_truncated,
        fields_checked,
        hit_semantics,
        build_id
    ],
    Modes = [
        {<<"missing-", (atom_to_binary(Key))/binary>>, {missing, Key}, protocol_error}
     || Key <- Missing
    ],
    Common = [
        {<<"unknown-decision">>, {mutate, decision, unknown}, protocol_error},
        {<<"unknown-severity">>, {hit_enum, severity}, protocol_error},
        {<<"unknown-variant">>, {hit_enum, variant_type}, protocol_error},
        {<<"unknown-method">>, {hit_enum, match_methods}, protocol_error},
        {<<"false-scan">>, {mutate, scan_complete, false}, protocol_error},
        {<<"invalid-generation">>, {mutate, generation, <<"01">>}, protocol_error},
        {<<"short-revision">>, {mutate, ruleset_revision, <<"abc">>}, protocol_error},
        {<<"bad-offset">>, bad_offset, protocol_error},
        {<<"review-hits-block-truncated">>, false_block, protocol_error}
    ],
    Errors = [
        {<<"protocol-internal">>, {remote, 500, 13, <<"PROTOCOL_ERROR">>, valid}, protocol_error},
        {<<"protocol-data-loss">>, {remote, 502, 15, <<"PROTOCOL_ERROR">>, valid}, protocol_error},
        {<<"protocol-invalid-argument-rejected">>, {remote, 400, 3, <<"PROTOCOL_ERROR">>, valid},
            protocol_error},
        {<<"overloaded">>, {remote, 429, 8, <<"OVERLOADED">>, valid}, overloaded},
        {<<"normalization-budget">>, {remote, 422, 8, <<"NORMALIZATION_LIMIT">>, valid},
            normalization_limit},
        {<<"match-budget">>, {remote, 422, 8, <<"MATCH_BUDGET_EXCEEDED">>, valid},
            match_budget_exceeded},
        {<<"unsupported-correct-status">>, {remote, 503, 9, <<"UNSUPPORTED_RULESET">>, valid},
            unsupported_ruleset},
        {<<"unsupported-wrong-status">>, {remote, 500, 5, <<"UNSUPPORTED_RULESET">>, valid},
            protocol_error},
        {<<"missing-metadata">>, {remote, 429, 8, <<"OVERLOADED">>, missing}, protocol_error},
        {<<"wrong-request-id">>, {remote, 429, 8, <<"OVERLOADED">>, wrong_id}, protocol_error},
        {<<"unknown-code">>, {remote, 500, 13, <<"UNKNOWN_CODE">>, valid}, protocol_error}
    ],
    Extra =
        case Protocol of
            http ->
                [
                    {<<"numeric-generation">>, {mutate, generation, 1}, protocol_error},
                    {<<"two-key-success">>, two_key, protocol_error},
                    {<<"malformed-json">>, malformed_json, protocol_error},
                    {<<"flat-error-rejected">>, flat_error, protocol_error},
                    {<<"401-overloaded-rejected">>, {remote, 401, 8, <<"OVERLOADED">>, valid},
                        protocol_error},
                    {<<"500-not-ready-rejected">>, {remote, 500, 14, <<"NOT_READY">>, valid},
                        protocol_error}
                ];
            grpc ->
                []
        end,
    [
        entry(<<"AC-061/", Name/binary, "-", Id/binary>>, fault, {Protocol, Mode, Expected}, [
            <<"AC-061">>
        ])
     || {Id, Mode, Expected} <-
            Modes ++ Common ++ Errors ++ Extra ++
                [
                    {<<"hang-deadline-single-attempt">>, hang, timeout}
                ]
    ].

textguard_integration_test_() ->
    {timeout, 180,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            [
                {
                    binary_to_list(maps:get(id, Case)),
                    {timeout, 15, fun() -> execute(Case, State) end}
                }
             || Case <- manifest()
            ]
        end}}.

setup() ->
    logger:set_primary_config(level, emergency),
    {ok, _} = application:ensure_all_started(gun),
    ok = textguard_export_tests:pg_setup(),
    State0 = #{
        check_url => env_binary("TG19_HTTP_URL"),
        manage_url => env_binary("TG19_MANAGE_URL"),
        grpc_port => list_to_integer(os:getenv("TG19_GRPC_PORT")),
        check_token => read_secret("TEXTGUARD_CHECK_SECRET_FILE"),
        publish_token => read_secret("TEXTGUARD_PUBLISH_SECRET_FILE")
    },
    set_config(State0, check),
    FaultPid = start_channels(State0),
    set_config(State0, manage),
    {ok, Initial} = moderation_detector_http:get_active_ruleset(ctx()),
    lists:foreach(
        fun({Id, Word, Category, Severity}) ->
            ok = textguard_export_tests:insert_word(Id, Word, Category, Severity)
        end,
        textguard_golden:rows()
    ),
    EnhancementPath = os:getenv("TG19_ENH_FILE"),
    ok = file:write_file(EnhancementPath, jsone:encode(textguard_golden:enhancement())),
    application:set_env(imboy, textguard_export, [
        {t2s_table_id, <<"test-t2s-v1">>}, {enhancement_file, list_to_binary(EnhancementPath)}
    ]),
    Published = publish(State0),
    {ok, Export} = textguard_export:export_package(),
    Active = maps:get(last_published, Published),
    true = maps:get(ruleset_revision, Active) =:= maps:get(package_sha256, Export),
    set_config(State0, check),
    Canonical = find_case(<<"canonical">>),
    Detection = textguard_golden:detection(Canonical),
    {ok, RawGrpc, _} = textguard_grpcbox_transport:raw(
        check_text, textguard_golden:request(Detection), 1000
    ),
    ReviewDetection = Detection#{fields => [{<<"body">>, binary:copy(<<"demo ">>, 101)}]},
    {ok, RawReview, _} = textguard_grpcbox_transport:raw(
        check_text, textguard_golden:request(ReviewDetection), 1000
    ),
    State = State0#{
        initial => Initial,
        active => Active,
        export => Export,
        raw_grpc => RawGrpc,
        raw_review => RawReview,
        fault_pid => FaultPid
    },
    write_receipt(<<"bootstrap">>, #{
        initial => Initial,
        active => Active,
        pg_rows => length(textguard_golden:rows()),
        base_digest => maps:get(base_digest, Export),
        package_sha256 => maps:get(package_sha256, Export)
    }),
    State.

start_channels(State) ->
    false = lists:keymember(grpcbox, 1, application:which_applications()),
    ok = application:load(grpcbox),
    ok = application:set_env(grpcbox, client, #{
        channels => [
            {tg19_real, [{http, "127.0.0.1", maps:get(grpc_port, State), []}], #{
                sync_start => true
            }}
        ]
    }),
    {ok, _} = application:ensure_all_started(grpcbox),
    _ = supervised_channel(tg19_real),
    true = grpcbox_channel:is_ready(tg19_real),
    FaultPort = list_to_integer(os:getenv("TG19_FAULT_GRPC_PORT")),
    {ok, FaultPid} = textguard_fault_service:start(FaultPort),
    {ok, _} = textguard_grpcbox_transport:start(tg19_fault_channel, <<"127.0.0.1">>, FaultPort),
    _ = supervised_channel(tg19_fault_channel),
    true = grpcbox_channel:is_ready(tg19_fault_channel),
    FaultPid.

supervised_channel(Name) ->
    Channel = gproc:where({n, l, {grpcbox_channel, Name}}),
    ?assert(is_pid(Channel)),
    ?assert(lists:keymember(Channel, 2, supervisor:which_children(grpcbox_channel_sup))),
    ?assert(
        lists:keymember(whereis(grpcbox_channel_sup), 2, supervisor:which_children(grpcbox_sup))
    ),
    Channel.

cleanup(State) ->
    catch textguard_publisher:stop(),
    textguard_grpcbox_transport:stop(tg19_fault_channel),
    textguard_grpcbox_transport:stop(tg19_real),
    textguard_fault_service:stop(maps:get(fault_pid, State)),
    application:stop(grpcbox),
    application:unset_env(grpcbox, client),
    textguard_export_tests:pg_teardown(ok),
    application:unset_env(imboy, moderation_detector),
    application:unset_env(imboy, textguard_export),
    ok.

execute(Case, State) ->
    Id = maps:get(id, Case),
    try run(maps:get(kind, Case), maps:get(data, Case), State) of
        Detail -> append_result(Case, passed, Detail)
    catch
        Class:Reason:Stack ->
            append_result(Case, failed, #{exception_class => Class}),
            erlang:raise(Class, {tg19_case_failed, Id, reason_tag(Reason)}, Stack)
    after
        set_config(State, check)
    end.

reason_tag({assert, _}) -> assertion;
reason_tag({assertEqual, _}) -> equality;
reason_tag({badmatch, _}) -> badmatch;
reason_tag(_) -> failure.

run(supervised_channel, _, State) ->
    set_config(State, check),
    Old = supervised_channel(tg19_real),
    Monitor = erlang:monitor(process, Old),
    ok = gen_statem:stop(Old, tg19_channel_restart, 1000),
    receive
        {'DOWN', Monitor, process, Old, tg19_channel_restart} -> ok
    after 1000 -> error(channel_did_not_stop)
    end,
    New = await_restarted_channel(Old, erlang:monotonic_time(millisecond) + 2000),
    ?assertEqual(New, supervised_channel(tg19_real)),
    {ok, Info} = moderation_detector_grpc:info(ctx()),
    assert_version(Info, maps:get(active, State)),
    #{
        application_client_config => true,
        supervisor => grpcbox_channel_sup,
        old_pid => list_to_binary(pid_to_list(Old)),
        new_pid => list_to_binary(pid_to_list(New)),
        production_bridge => moderation_detector_grpcbox,
        post_restart_real_rpc => true
    };
run(core, Case, State) ->
    set_config(State, check),
    Detection = textguard_golden:detection(Case),
    {ok, Http} = moderation_detector_http:check(Detection, ctx()),
    {ok, Grpc} = moderation_detector_grpc:check(Detection, ctx()),
    Expected = textguard_golden:expected(Case),
    ?assert(maps:with(maps:keys(Expected), Http) =:= Expected),
    ?assert(maps:with(maps:keys(Expected), Grpc) =:= Expected),
    ?assert(Http =:= Grpc),
    assert_version(Http, maps:get(active, State)),
    #{
        protocols => [http, grpc],
        decision => maps:get(decision, Http),
        hit_count => length(maps:get(hits, Http)),
        ruleset_revision => maps:get(ruleset_revision, Http)
    };
run(fault, {Protocol, Mode, Expected}, State) ->
    fault(Protocol, Mode, Expected, State);
run(auth, {Protocol, TokenKind}, State) ->
    set_config(State, check),
    Token = token(TokenKind, State),
    update_config([{token, Token}]),
    Expected =
        case TokenKind of
            wrong -> unauthenticated;
            publish -> permission_denied
        end,
    Module = transport(Protocol),
    ?assertEqual(
        {error, Expected},
        Module:check(textguard_golden:detection(find_case(<<"canonical">>)), ctx())
    ),
    #{protocol => Protocol, error => Expected};
run(manage_auth, TokenKind, State) ->
    set_config(State, manage),
    update_config([{publish_token, token(TokenKind, State)}]),
    Expected =
        case TokenKind of
            wrong -> unauthenticated;
            check -> permission_denied
        end,
    ?assertEqual({error, Expected}, moderation_detector_http:get_active_ruleset(ctx())),
    #{error => Expected};
run(empty_state, _, State) ->
    Initial = maps:get(initial, State),
    ?assertEqual(false, maps:get(ready, Initial)),
    ?assertEqual(<<"0">>, maps:get(generation, Initial)),
    ?assertEqual(<<>>, maps:get(ruleset_revision, Initial)),
    ?assertEqual(<<"uninitialized">>, maps:get(status, Initial)),
    ?assertEqual(64, byte_size(maps:get(state_etag, Initial))),
    #{generation => <<"0">>, state_etag_present => true};
run(info, _, State) ->
    set_config(State, check),
    {ok, Http} = moderation_detector_http:info(ctx()),
    {ok, Grpc} = moderation_detector_grpc:info(ctx()),
    ?assert(Http =:= Grpc),
    ?assertEqual(true, maps:get(ready, Http)),
    assert_version(Http, maps:get(active, State)),
    #{protocols => [http, grpc], ready => true, generation => maps:get(generation, Http)};
run(stale_etag, _, State) ->
    set_config(State, manage),
    {ok, Export} = textguard_export:export_package(),
    {ok, Before} = moderation_detector_http:get_active_ruleset(ctx()),
    Options = #{if_match => maps:get(state_etag, maps:get(initial, State)), ctx => ctx()},
    {ok, Same} = moderation_detector_http:publish_package(maps:get(package, Export), Options),
    ?assertEqual(<<"already_active">>, maps:get(status, Same)),
    assert_version(Same, Before),
    ?assertEqual(maps:get(state_etag, Before), maps:get(state_etag, Same)),
    ok = textguard_export_tests:insert_word(1097, <<"casprobeword">>, <<"test">>, <<"high">>),
    try
        {ok, Different} = textguard_export:export_package(),
        ?assert(maps:get(package, Export) =/= maps:get(package, Different)),
        ?assertEqual(
            {error, cas_conflict},
            moderation_detector_http:publish_package(
                maps:get(package, Different), Options
            )
        ),
        {ok, After} = moderation_detector_http:get_active_ruleset(ctx()),
        ?assertEqual(Before, After),
        #{error => cas_conflict, different_package => true, same_package_idempotent => true}
    after
        textguard_export_tests:delete_word(1097)
    end;
run(new_revision, _, State) ->
    set_config(State, check),
    {ok, Before} = moderation_detector_http:info(ctx()),
    ok = textguard_export_tests:insert_word(1099, <<"freshword">>, <<"test">>, <<"high">>),
    Published = publish(State),
    Active = maps:get(last_published, Published),
    ?assert(maps:get(ruleset_revision, Active) =/= maps:get(ruleset_revision, Before)),
    ?assert(
        binary_to_integer(maps:get(generation, Active)) >
            binary_to_integer(maps:get(generation, Before))
    ),
    set_config(State, check),
    Detection = #{
        request_id => <<"tg19-new-revision">>,
        scene => channel_message,
        fields => [{<<"body">>, <<"freshword">>}]
    },
    {ok, Http} = moderation_detector_http:check(Detection, ctx()),
    {ok, Grpc} = moderation_detector_grpc:check(Detection, ctx()),
    ?assert(Http =:= Grpc),
    ?assertEqual(block, maps:get(decision, Http)),
    ?assertEqual([<<"1099">>], [maps:get(rule_id, Hit) || Hit <- maps:get(hits, Http)]),
    assert_version(Http, Active),
    #{
        protocols => [http, grpc],
        ruleset_revision => maps:get(ruleset_revision, Active),
        generation => maps:get(generation, Active),
        pg_commit_visible => true
    };
run(saved_pending, _, State) ->
    saved_pending(State);
run(independent_endpoint, _, State) ->
    set_config(State, check),
    update_config([{publish_http_base_url, maps:get(manage_url, State)}]),
    {ok, Active} = moderation_detector_http:get_active_ruleset(ctx()),
    {ok, Info} = moderation_detector_http:info(ctx()),
    assert_version(Info, Active),
    #{independent_urls => true}.

saved_pending(State) ->
    set_config(State, check),
    {ok, Before} = moderation_detector_http:info(ctx()),
    ok = textguard_export_tests:insert_word(1098, <<"pendingword">>, <<"test">>, <<"high">>),
    {ok, [Saved]} = elib_pg:query(<<"SELECT id FROM public.sensitive_word WHERE id = $1">>, [1098]),
    ?assertEqual(1098, maps:get(<<"id">>, Saved)),
    set_config(State, manage),
    {ok, Active} = moderation_detector_http:get_active_ruleset(ctx()),
    Failed = ewh_http_fixture:start(fun
        (#{method := <<"GET">>}) ->
            {reply, 200, [], jsone:encode(Active)};
        (#{method := <<"PUT">>}) ->
            {reply, 400, [],
                jsone:encode(#{
                    error => #{
                        code => <<"INVALID_RULESET">>, request_id => <<>>, retryable => false
                    }
                })}
    end),
    try
        set_config(State, manage),
        update_config([
            {http_base_url, fixture_url(Failed)}, {publish_http_base_url, fixture_url(Failed)}
        ]),
        {ok, _} = textguard_publisher:start_link(publisher_options()),
        {ok, Status} = textguard_publisher:reconcile(),
        ?assertEqual(true, maps:get(pending, Status)),
        ?assertEqual(failed, maps:get(state, Status)),
        ?assertEqual({publish, invalid_ruleset}, maps:get(last_error, Status)),
        ?assertEqual(2, ewh_http_fixture:request_count(Failed)),
        ok = textguard_publisher:stop(),
        set_config(State, check),
        {ok, After} = moderation_detector_http:info(ctx()),
        ?assertEqual(maps:get(ruleset_revision, Before), maps:get(ruleset_revision, After)),
        Detection = #{
            request_id => <<"tg19-saved-pending">>,
            scene => channel_message,
            fields => [{<<"body">>, <<"pendingword">>}]
        },
        lists:foreach(
            fun(Module) ->
                {ok, Response} = Module:check(Detection, ctx()),
                ?assertEqual(allow, maps:get(decision, Response))
            end,
            [moderation_detector_http, moderation_detector_grpc]
        ),
        #{db_saved => true, pending => true, publisher_state => failed, active_unchanged => true}
    after
        catch textguard_publisher:stop(),
        ewh_http_fixture:stop(Failed),
        textguard_export_tests:delete_word(1098)
    end.

fault(Protocol, Mode, Expected, State) ->
    set_config(State, check),
    {Detection, Response} = fault_input(Mode, State),
    Budget =
        case Mode of
            hang -> 120;
            _ -> 1000
        end,
    Started = erlang:monotonic_time(millisecond),
    {Actual, Count} =
        case Protocol of
            grpc ->
                update_config([{grpc_channel, tg19_fault_channel}]),
                textguard_fault_service:set(grpc_reply(Mode, Response)),
                Reply = moderation_detector_grpc:check(Detection, #{deadline_ms => Budget}),
                {Reply, textguard_fault_service:count()};
            http ->
                Fixture = ewh_http_fixture:start(fun(_) -> http_reply(Mode, Response, Detection) end),
                try
                    update_config([{http_base_url, fixture_url(Fixture)}]),
                    Reply = moderation_detector_http:check(Detection, #{deadline_ms => Budget}),
                    {Reply, ewh_http_fixture:request_count(Fixture)}
                after
                    ewh_http_fixture:stop(Fixture)
                end
        end,
    Elapsed = erlang:monotonic_time(millisecond) - Started,
    ?assertEqual({error, Expected}, Actual),
    ?assertEqual(1, Count),
    ?assert(Mode =/= hang orelse Elapsed < Budget + 300),
    #{
        protocol => Protocol,
        expected_error => Expected,
        wire_requests => Count,
        elapsed_ms => Elapsed,
        budget_ms => Budget
    }.

fault_input(false_block, State) ->
    Detection = (textguard_golden:detection(find_case(<<"canonical">>)))#{
        fields => [{<<"body">>, binary:copy(<<"demo ">>, 101)}]
    },
    {Detection, (maps:get(raw_review, State))#{decision => 'BLOCK'}};
fault_input(Mode, State) ->
    {
        textguard_golden:detection(find_case(<<"canonical">>)),
        mutate(Mode, maps:get(raw_grpc, State))
    }.

mutate({missing, Key}, Response) ->
    maps:remove(Key, Response);
mutate({mutate, decision, unknown}, Response) ->
    Response#{decision => 99};
mutate({mutate, Key, Value}, Response) ->
    Response#{Key => Value};
mutate({hit_enum, Key}, #{hits := [Hit | Rest]} = Response) ->
    Value =
        case Key of
            match_methods -> [99];
            _ -> 99
        end,
    Response#{hits => [Hit#{Key => Value} | Rest]};
mutate(bad_offset, #{hits := [Hit | Rest]} = Response) ->
    Response#{hits => [Hit#{end_byte => 1} | Rest]};
mutate(_, Response) ->
    Response.

grpc_reply(hang, _) -> hang;
grpc_reply({remote, _, Status, Code, Mode}, _) -> {error, Status, Code, Mode};
grpc_reply(_, Response) -> {success, Response}.

http_reply(hang, _, _) ->
    receive
        tg19_stop -> hang
    after 5000 -> hang
    end;
http_reply(malformed_json, _, _) ->
    {reply, 200, [], <<"{">>};
http_reply(two_key, _, _) ->
    {reply, 200, [], <<"{\"decision\":\"allow\",\"scan_complete\":true}">>};
http_reply(flat_error, _, _) ->
    {reply, 429, [],
        jsone:encode(#{code => <<"OVERLOADED">>, request_id => <<>>, retryable => true})};
http_reply({remote, Status, _, Code, Mode}, _, Detection) ->
    RequestId =
        case Mode of
            wrong_id -> <<"tg19-wrong-id">>;
            _ -> maps:get(request_id, Detection)
        end,
    Body =
        case Mode of
            missing -> #{error => #{retryable => true}};
            _ -> #{error => #{code => Code, request_id => RequestId, retryable => true}}
        end,
    {reply, Status, [], jsone:encode(Body)};
http_reply(_, Response, _) ->
    {reply, 200, [], jsone:encode(http_response(Response))}.

http_response(Response) ->
    maps:map(
        fun
            (hits, Hits) -> [maps:map(fun enum_json/2, Hit) || Hit <- Hits];
            (Key, Value) -> enum_json(Key, Value)
        end,
        Response
    ).

enum_json(match_methods, Values) ->
    [enum_json(match_method, Value) || Value <- Values];
enum_json(_Key, Value) when is_atom(Value), Value =/= true, Value =/= false ->
    list_to_binary(string:lowercase(atom_to_list(Value)));
enum_json(_Key, Value) ->
    Value.

publish(State) ->
    set_config(State, manage),
    {ok, _} = textguard_publisher:start_link(publisher_options()),
    try
        {ok, Status} = textguard_publisher:reconcile(),
        case maps:get(last_error, Status) of
            {Phase, Reason} when is_atom(Phase) ->
                Class =
                    case Reason of
                        Value when is_atom(Value) -> Value;
                        {Value, _} when is_atom(Value) -> Value;
                        _ -> redacted
                    end,
                ok = write_receipt(<<"publication-failure">>, #{
                    phase => Phase, error_class => Class
                });
            _ ->
                ok
        end,
        published = maps:get(state, Status),
        false = maps:get(pending, Status),
        Status
    after
        textguard_publisher:stop()
    end.

publisher_options() ->
    #{reconcile_interval_ms => 3600000, cas_max_retries => 0, publish_timeout_ms => 1000}.

await_restarted_channel(Old, Deadline) ->
    Candidate = gproc:where({n, l, {grpcbox_channel, tg19_real}}),
    Ready =
        try
            grpcbox_channel:is_ready(tg19_real)
        catch
            _:_ -> false
        end,
    case is_pid(Candidate) andalso Candidate =/= Old andalso Ready of
        true ->
            Candidate;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true -> receive
                    after 10 -> await_restarted_channel(Old, Deadline)
                    end;
                false -> error(channel_did_not_restart)
            end
    end.

set_config(State, Target) ->
    Url =
        case Target of
            check -> maps:get(check_url, State);
            manage -> maps:get(manage_url, State)
        end,
    application:set_env(imboy, moderation_detector, [
        {http_base_url, Url},
        {publish_http_base_url, maps:get(manage_url, State)},
        {token, maps:get(check_token, State)},
        {publish_token, maps:get(publish_token, State)},
        {grpc_host, <<"127.0.0.1">>},
        {grpc_port, maps:get(grpc_port, State)},
        {grpc_channel, tg19_real},
        {timeout_ms, 1000},
        {max_response_bytes, 262144}
    ]).

update_config(Changes) ->
    Conf = config_ds:env(moderation_detector, []),
    application:set_env(
        imboy,
        moderation_detector,
        Changes ++ [Entry || {Key, _} = Entry <- Conf, not lists:keymember(Key, 1, Changes)]
    ).

ctx() -> #{deadline_ms => 1000}.
transport(http) -> moderation_detector_http;
transport(grpc) -> moderation_detector_grpc.
token(wrong, _) -> <<"tg19-invalid-credential-not-a-secret">>;
token(check, State) -> maps:get(check_token, State);
token(publish, State) -> maps:get(publish_token, State).

assert_version(Response, Active) ->
    ?assertEqual(maps:get(ruleset_revision, Active), maps:get(ruleset_revision, Response)),
    ?assertEqual(maps:get(generation, Active), maps:get(generation, Response)).

find_case(Id) ->
    hd([Case || Case <- textguard_golden:cases(), maps:get(id, Case) =:= Id]).

fixture_url(Fixture) ->
    <<"http://127.0.0.1:", (integer_to_binary(ewh_http_fixture:port(Fixture)))/binary>>.

env_binary(Name) -> list_to_binary(os:getenv(Name)).
read_secret(Name) ->
    {ok, Value} = file:read_file(os:getenv(Name)),
    string:trim(Value).

write_receipt(Name, Value) ->
    file:write_file(
        filename:join(os:getenv("TG19_EVIDENCE"), binary_to_list(Name) ++ ".json"),
        jsone:encode(Value)
    ).

append_result(Case, Outcome, Detail) ->
    Value = #{
        id => maps:get(id, Case),
        acceptance => maps:get(acceptance, Case),
        outcome => Outcome,
        detail => Detail
    },
    ok = file:write_file(
        filename:join(os:getenv("TG19_EVIDENCE"), "case-results.jsonl"),
        [jsone:encode(Value), <<"\n">>],
        [append]
    ).
