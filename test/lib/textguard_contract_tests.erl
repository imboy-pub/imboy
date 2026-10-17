-module(textguard_contract_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CONTRACT, moderation_detector_transport).
-export([check_response/1, expected_check/1, info_response/0]).

check_response(Decision) ->
    Hits =
        case Decision of
            <<"allow">> ->
                [];
            _ ->
                [
                    #{
                        field_id => <<"body">>,
                        rule_id => <<"1001">>,
                        variant_id => <<"canonical">>,
                        variant_type => <<"canonical">>,
                        word => <<"你"/utf8>>,
                        category => <<"test">>,
                        severity => <<"high">>,
                        effective_action => Decision,
                        start_byte => 0,
                        end_byte => 3,
                        match_methods => [<<"literal">>]
                    }
                ]
        end,
    #{
        request_id => <<"req-t15-0001">>,
        ruleset_revision => binary:copy(<<"a">>, 64),
        generation => <<"9007199254740993">>,
        normalization_id => <<"imboy-base-v1">>,
        t2s_table_id => <<"opencc-1.1.9-single-v1">>,
        semantics_id => <<"enhanced-v1">>,
        base_rules_digest => binary:copy(<<"b">>, 64),
        decision => Decision,
        scan_complete => true,
        hits_truncated => false,
        hits => Hits,
        fields_checked => 2,
        hit_semantics => <<"literal-all-regex-first-v1">>,
        build_id => <<"test-build">>
    }.

expected_check(Decision) ->
    {ok, Atom} = ?CONTRACT:decision_to_atom(Decision),
    {ok, (check_response(Decision))#{decision => Atom}}.

info_response() ->
    #{
        instance_id => <<"persistent-test-instance">>,
        ready => true,
        build_id => <<"test-build">>,
        ruleset_revision => binary:copy(<<"a">>, 64),
        generation => <<"1">>,
        normalization_id => <<"imboy-base-v1">>,
        t2s_table_id => <<"opencc-1.1.9-single-v1">>,
        base_rules_digest => binary:copy(<<"b">>, 64),
        canonical_count => 1,
        alias_count => 0,
        regex_count => 0,
        supported_semantics => [<<"compat-v1">>, <<"enhanced-v1">>]
    }.

detection() ->
    #{
        request_id => <<"req-t15-0001">>,
        scene => channel_message,
        fields => [{<<"body">>, <<"你好 textguard"/utf8>>}, {<<"nickname">>, <<"bob">>}]
    }.

http_map(Map) -> jsone:decode(jsone:encode(Map)).

validate(Map, http) -> ?CONTRACT:validate_check(http_map(Map), detection(), http);
validate(Map, grpc) -> ?CONTRACT:validate_check(Map, detection(), grpc).

success_contract_test_() ->
    [
        {atom_to_list(Protocol) ++ binary_to_list(Decision), fun() ->
            ?assertEqual(
                expected_check(Decision),
                validate(check_response(Decision), Protocol)
            )
        end}
     || Protocol <- [http, grpc], Decision <- [<<"allow">>, <<"review">>, <<"block">>]
    ].

check_presence_test_() ->
    Response = check_response(<<"block">>),
    [
        {atom_to_list(Protocol) ++ " missing " ++ atom_to_list(Key), fun() ->
            ?assertEqual({error, protocol_error}, validate(maps:remove(Key, Response), Protocol))
        end}
     || Protocol <- [http, grpc], Key <- maps:keys(Response)
    ].

check_values_test_() ->
    Good = check_response(<<"block">>),
    Cases = [
        {scan_complete, false},
        {scan_complete, undefined},
        {decision, 0},
        {decision, 99},
        {decision, <<"ALLOW">>},
        {request_id, <<"wrong-request">>},
        {ruleset_revision, binary:copy(<<"a">>, 16)},
        {ruleset_revision, binary:copy(<<"G">>, 64)},
        {generation, 9007199254740993},
        {generation, <<"01">>},
        {generation, <<"-1">>},
        {generation, <<"18446744073709551616">>},
        {generation, <<"1\n">>},
        {normalization_id, <<"unknown-v9">>},
        {semantics_id, <<"unknown-v9">>},
        {t2s_table_id, <<>>},
        {base_rules_digest, <<>>},
        {build_id, <<>>},
        {hits_truncated, undefined},
        {hits_truncated, <<"false">>},
        {hits, #{}},
        {hits, []},
        {fields_checked, 1},
        {fields_checked, 2.0},
        {fields_checked, 0},
        {hit_semantics, <<"unknown-v2">>}
    ],
    [
        {
            atom_to_list(Protocol) ++ " invalid " ++ atom_to_list(Key) ++ " " ++
                integer_to_list(Index),
            fun() ->
                ?assertEqual({error, protocol_error}, validate(Good#{Key => Value}, Protocol))
            end
        }
     || Protocol <- [http, grpc],
        {{Key, Value}, Index} <- lists:zip(Cases, lists:seq(1, length(Cases)))
    ].

hit_presence_test_() ->
    Response = check_response(<<"block">>),
    [Hit] = maps:get(hits, Response),
    [
        {atom_to_list(Protocol) ++ " hit missing " ++ atom_to_list(Key), fun() ->
            ?assertEqual(
                {error, protocol_error},
                validate(Response#{hits => [maps:remove(Key, Hit)]}, Protocol)
            )
        end}
     || Protocol <- [http, grpc], Key <- maps:keys(Hit)
    ].

hit_values_test_() ->
    Response = check_response(<<"block">>),
    [Hit] = maps:get(hits, Response),
    Cases = [
        {field_id, <<"foreign">>},
        {rule_id, <<>>},
        {variant_id, <<"not-canonical">>},
        {variant_type, 0},
        {variant_type, 99},
        {word, <<>>},
        {category, <<>>},
        {severity, 0},
        {severity, 99},
        {effective_action, <<"allow">>},
        {start_byte, undefined},
        {start_byte, -1},
        {start_byte, 1},
        {start_byte, 0.0},
        {end_byte, 0},
        {end_byte, 2},
        {end_byte, 9999},
        {match_methods, []},
        {match_methods, [0]},
        {match_methods, [<<"bogus">>]},
        {match_methods, [<<"literal">>, <<"literal">>]},
        {match_methods, [<<"regex">>]}
    ],
    [
        {
            atom_to_list(Protocol) ++ " hit invalid " ++ atom_to_list(Key) ++ " " ++
                integer_to_list(Index),
            fun() ->
                ?assertEqual(
                    {error, protocol_error},
                    validate(Response#{hits => [Hit#{Key => Value}]}, Protocol)
                )
            end
        }
     || Protocol <- [http, grpc],
        {{Key, Value}, Index} <- lists:zip(Cases, lists:seq(1, length(Cases)))
    ].

grpc_enums_and_evolution_test() ->
    Response = check_response(<<"block">>),
    [Hit] = maps:get(hits, Response),
    Wire = Response#{
        decision => 'BLOCK',
        unknown_wire_field => 123,
        hits => [
            Hit#{
                variant_type => 'CANONICAL',
                severity => 3,
                effective_action => 3,
                match_methods => ['LITERAL'],
                unknown_hit_field => true
            }
        ]
    },
    ?assertEqual(expected_check(<<"block">>), validate(Wire, grpc)),
    ?assertEqual({error, protocol_error}, validate(Wire, http)).

review_fix1_test_() ->
    [
        {"reject 401 OVERLOADED", http_bad_status_case(401, <<"OVERLOADED">>)},
        {"reject 500 NOT_READY", http_bad_status_case(500, <<"NOT_READY">>)}
    ] ++ http_error_pairing_cases() ++
        [
            truncated_witness_case(Protocol, HasBlock)
         || Protocol <- [http, grpc], HasBlock <- [false, true]
        ].

http_bad_status_case(Status, Code) ->
    Body = jsone:encode(#{error => #{code => Code, request_id => <<>>, retryable => false}}),
    http_contract_case(
        fun(_) -> {reply, Status, [], Body} end,
        fun(_) -> ?assertEqual({error, protocol_error}, call_http_endpoint(check)) end
    ).

http_error_pairing_cases() ->
    All = [check, info, get_active, put_active],
    Cases = [
        {<<"INVALID_ARGUMENT">>, [400], invalid_argument, [check, put_active]},
        {<<"INVALID_UTF8">>, [400], invalid_utf8, [check, put_active]},
        {<<"TEXT_TOO_LARGE">>, [413], text_too_large, [check, put_active]},
        {<<"NORMALIZATION_LIMIT">>, [422], normalization_limit, [check]},
        {<<"MATCH_BUDGET_EXCEEDED">>, [422], match_budget_exceeded, [check]},
        {<<"OVERLOADED">>, [429], overloaded, [check]},
        {<<"NOT_READY">>, [503], not_ready, [check, put_active]},
        {<<"CONNECTION_FAILED">>, [503], connection_failed, [check]},
        {<<"CHECK_DEADLINE">>, [504], timeout, [check]},
        {<<"UNAUTHENTICATED">>, [401], unauthenticated, All},
        {<<"PERMISSION_DENIED">>, [403], permission_denied, All},
        {<<"PROTOCOL_ERROR">>, [500, 502], protocol_error, [check, put_active]},
        {<<"NORMALIZER_INCOMPATIBLE">>, [500, 502], normalizer_incompatible, [check, put_active]},
        {<<"INTERNAL_ERROR">>, [500, 502], internal, All},
        {<<"UNSUPPORTED_RULESET">>, [503], unsupported_ruleset, [check, put_active]},
        {<<"CAS_CONFLICT">>, [412], cas_conflict, [put_active]},
        {<<"INVALID_RULESET">>, [400], invalid_ruleset, [put_active]},
        {<<"PUBLISH_DEADLINE">>, [504], publish_deadline, [put_active]},
        {<<"PUBLISH_STATE_UNKNOWN">>, [500], publish_state_unknown, [get_active, put_active]},
        {<<"PUBLISH_BUSY">>, [409], publish_busy, [put_active]},
        {<<"RETIRED_SNAPSHOT_BUSY">>, [409], retired_snapshot_busy, [put_active]}
    ],
    [
        http_error_pairing_case(Endpoint, Code, ExpectedStatuses, Reason, Allowed)
     || Endpoint <- All, {Code, ExpectedStatuses, Reason, Allowed} <- Cases
    ].

http_error_pairing_case(Endpoint, Code, ExpectedStatuses, Reason, Allowed) ->
    Body = jsone:encode(#{error => #{code => Code, request_id => <<>>, retryable => false}}),
    StatusCell = atomics:new(1, []),
    Responder = fun(_) -> {reply, atomics:get(StatusCell, 1), [], Body} end,
    Test = http_contract_case(Responder, fun(Handle) ->
        Statuses = [202, 302, 400, 401, 403, 404, 409, 412, 413, 422, 429, 500, 502, 503, 504],
        lists:foreach(
            fun(Status) ->
                atomics:put(StatusCell, 1, Status),
                Expected =
                    case
                        lists:member(Status, ExpectedStatuses) andalso
                            lists:member(Endpoint, Allowed)
                    of
                        true -> Reason;
                        false -> protocol_error
                    end,
                ?assertEqual(
                    {Endpoint, Code, Status, {error, Expected}},
                    {Endpoint, Code, Status, call_http_endpoint(Endpoint)}
                )
            end,
            Statuses
        ),
        ?assertEqual(length(Statuses), ewh_http_fixture:request_count(Handle))
    end),
    {atom_to_list(Endpoint) ++ " status/code " ++ binary_to_list(Code), Test}.

http_contract_case(Responder, Test) ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(gun),
            Handle = ewh_http_fixture:start(Responder),
            BaseUrl =
                <<"http://127.0.0.1:", (integer_to_binary(ewh_http_fixture:port(Handle)))/binary>>,
            application:set_env(imboy, moderation_detector, [
                {http_base_url, BaseUrl},
                {token, <<"check-placeholder">>},
                {publish_token, <<"publish-placeholder">>}
            ]),
            Handle
        end,
        fun(Handle) ->
            ewh_http_fixture:stop(Handle),
            application:unset_env(imboy, moderation_detector)
        end,
        fun(Handle) -> fun() -> Test(Handle) end end}.

call_http_endpoint(Endpoint) ->
    Ctx = #{deadline_ms => 2000, request_id => <<"req-t15-0001">>},
    case Endpoint of
        check ->
            moderation_detector_http:check(detection(), Ctx);
        info ->
            moderation_detector_http:info(Ctx);
        get_active ->
            moderation_detector_http:get_active_ruleset(Ctx);
        put_active ->
            moderation_detector_http:publish_package(<<"{}">>, #{
                if_match => binary:copy(<<"a">>, 64), ctx => Ctx
            })
    end.

truncated_witness_case(Protocol, HasBlock) ->
    Response = check_response(<<"block">>),
    [Hit] = maps:get(hits, Response),
    Reviews = [
        Hit#{rule_id => integer_to_binary(1000 + Index), effective_action => <<"review">>}
     || Index <- lists:seq(1, 100)
    ],
    Hits =
        case HasBlock of
            true -> [Hit | tl(Reviews)];
            false -> Reviews
        end,
    Truncated = Response#{hits => Hits, hits_truncated => true},
    Expected =
        case HasBlock of
            true -> {ok, Truncated#{decision => block}};
            false -> {error, protocol_error}
        end,
    Test =
        case Protocol of
            http ->
                http_contract_case(
                    fun(_) -> {reply, 200, [], jsone:encode(Truncated)} end,
                    fun(_) -> ?assertEqual(Expected, call_http_endpoint(check)) end
                );
            grpc ->
                fun() ->
                    application:set_env(imboy, moderation_detector, [
                        {grpc_host, <<"127.0.0.1">>},
                        {grpc_port, 7464},
                        {grpc_transport, fun(_, _, _, _) -> {ok, Truncated} end}
                    ]),
                    try
                        ?assertEqual(Expected, moderation_detector_grpc:check(detection(), #{}))
                    after
                        application:unset_env(imboy, moderation_detector)
                    end
                end
        end,
    {atom_to_list(Protocol) ++ " truncated block witness " ++ atom_to_list(HasBlock), Test}.

hit_aggregation_contract_test_() ->
    Response = check_response(<<"block">>),
    [Hit] = maps:get(hits, Response),
    Review = Hit#{rule_id => <<"1002">>, effective_action => <<"review">>},
    Hundred = [Hit#{rule_id => integer_to_binary(1000 + Index)} || Index <- lists:seq(1, 100)],
    [
        fun() ->
            ?assertMatch({ok, _}, validate(Response#{hits => [Hit, Review]}, Protocol)),
            ?assertMatch(
                {ok, _}, validate(Response#{hits => Hundred, hits_truncated => true}, Protocol)
            ),
            ?assertEqual(
                {error, protocol_error}, validate(Response#{hits => [Review, Hit]}, Protocol)
            ),
            ?assertEqual(
                {error, protocol_error}, validate(Response#{hits => [Hit, Hit]}, Protocol)
            ),
            ?assertEqual(
                {error, protocol_error}, validate(Response#{hits_truncated => true}, Protocol)
            ),
            ?assertEqual(
                {error, protocol_error}, validate(Response#{decision => <<"allow">>}, Protocol)
            )
        end
     || Protocol <- [http, grpc]
    ].

json_duplicates_test() ->
    Body = jsone:encode(check_response(<<"allow">>)),
    <<"{", Rest/binary>> = Body,
    ?assertEqual(
        {error, protocol_error},
        ?CONTRACT:decode_json(
            <<"{\"scan_complete\":true,", Rest/binary>>
        )
    ),
    ?assertEqual(
        {error, protocol_error},
        ?CONTRACT:decode_json(
            <<"{\"hits\":[{\"start_byte\":0,\"start_byte\":1}]}">>
        )
    ),
    ?assertEqual({error, protocol_error}, ?CONTRACT:decode_json(<<"{\"id\":\"", 255, "\"}">>)).

info_contract_test_() ->
    Info = info_response(),
    NotReady = maps:with([instance_id, build_id, supported_semantics], Info),
    Missing = [maps:remove(Key, Info) || Key <- maps:keys(Info)],
    Bad = [
        Info#{canonical_count => -1},
        Info#{regex_count => 33},
        Info#{generation => 1},
        Info#{ruleset_revision => <<>>},
        Info#{supported_semantics => []},
        Info#{supported_semantics => [<<"new-v2">>]},
        Info#{supported_semantics => [<<"compat-v1">>, <<"compat-v1">>]}
    ],
    [
        fun() ->
            ?assertEqual({ok, Info}, validate_info(Info, Protocol)),
            ?assertEqual(
                {ok, NotReady#{ready => false}}, validate_info(NotReady#{ready => false}, Protocol)
            ),
            Partial = NotReady#{ready => false, generation => <<"0">>},
            ?assertEqual({ok, Partial}, validate_info(Partial, Protocol))
        end
     || Protocol <- [http, grpc]
    ] ++
        [
            fun() -> ?assertEqual({error, protocol_error}, validate_info(Response, Protocol)) end
         || Protocol <- [http, grpc], Response <- Missing ++ Bad
        ].

validate_info(Info, http) -> ?CONTRACT:validate_info(http_map(Info), http);
validate_info(Info, grpc) -> ?CONTRACT:validate_info(Info, grpc).

publication(Generation, Revision, Status) ->
    Instance = <<"persisted-data-directory-id">>,
    ETag = binary:encode_hex(
        crypto:hash(
            sha256,
            <<Instance/binary, "\n", Generation/binary, "\n", Revision/binary>>
        ),
        lowercase
    ),
    #{
        instance_id => Instance,
        ready => Revision =/= <<>>,
        generation => Generation,
        ruleset_revision => Revision,
        state_etag => ETag,
        status => Status,
        scene_counts => #{channel_message => 0, moment_post => 0, profile_field => 0}
    }.

publication_contract_test_() ->
    Initial = publication(<<"0">>, <<>>, <<"uninitialized">>),
    Active = publication(<<"1">>, binary:copy(<<"a">>, 64), <<"active">>),
    Bad =
        [
            Active#{generation => 1},
            Active#{instance_id => <<"other-directory">>},
            Active#{state_etag => maps:get(ruleset_revision, Active)},
            Active#{ready => false},
            Active#{scene_counts => #{channel_message => 1}},
            Active#{scene_counts => #{channel_message => -1, moment_post => 0, profile_field => 0}},
            Initial#{ruleset_revision => binary:copy(<<"b">>, 64)},
            Initial#{status => <<"active">>}
        ] ++
            [maps:remove(Key, Active) || Key <- maps:keys(Active)],
    [
        fun() ->
            ?assertEqual(
                {ok, State},
                ?CONTRACT:validate_publication(http_map(State), Method)
            )
        end
     || {Method, State} <- [
            {get, Initial},
            {get, Active},
            {put, Active#{status => <<"committed">>}},
            {put, Active#{status => <<"already_active">>}}
        ]
    ] ++
        [
            fun() ->
                ?assertEqual(
                    {error, protocol_error},
                    ?CONTRACT:validate_publication(http_map(State), get)
                )
            end
         || State <- Bad
        ].

publication_etag_identity_test() ->
    Initial = publication(<<"0">>, <<>>, <<"uninitialized">>),
    ?assertEqual(
        <<"57beb2b20870c7ff70309892ce55a0a410bd61d20e3759f1fef5459d5d7de31b">>,
        maps:get(state_etag, Initial)
    ),
    First = publication(<<"1">>, binary:copy(<<"a">>, 64), <<"active">>),
    Repeated = publication(<<"3">>, binary:copy(<<"a">>, 64), <<"active">>),
    ?assertNotEqual(maps:get(state_etag, First), maps:get(state_etag, Repeated)).

grpc_error_status_pairing_test_() ->
    Cases = [
        {<<"INVALID_ARGUMENT">>, [3], invalid_argument},
        {<<"INVALID_UTF8">>, [3], invalid_utf8},
        {<<"TEXT_TOO_LARGE">>, [8], text_too_large},
        {<<"NORMALIZATION_LIMIT">>, [8], normalization_limit},
        {<<"MATCH_BUDGET_EXCEEDED">>, [8], match_budget_exceeded},
        {<<"OVERLOADED">>, [8], overloaded},
        {<<"NOT_READY">>, [14], not_ready},
        {<<"CONNECTION_FAILED">>, [14], connection_failed},
        {<<"CHECK_DEADLINE">>, [4], timeout},
        {<<"UNAUTHENTICATED">>, [16], unauthenticated},
        {<<"PERMISSION_DENIED">>, [7], permission_denied},
        {<<"PROTOCOL_ERROR">>, [13, 15], protocol_error},
        {<<"NORMALIZER_INCOMPATIBLE">>, [13, 15], normalizer_incompatible},
        {<<"INTERNAL_ERROR">>, [13, 15], internal},
        {<<"UNSUPPORTED_RULESET">>, [9], unsupported_ruleset}
    ],
    [grpc_error_status_pairing_case(Code, Statuses, Reason) || {Code, Statuses, Reason} <- Cases].

grpc_default_transport_missing_credential_test() ->
    application:set_env(imboy, moderation_detector, [
        {grpc_host, <<"127.0.0.1">>}, {grpc_port, 8788}
    ]),
    try
        ?assertEqual({error, not_ready}, moderation_detector_grpc:check(detection(), #{})),
        ?assertEqual({error, not_ready}, moderation_detector_grpc:info(#{}))
    after
        application:unset_env(imboy, moderation_detector)
    end.

grpc_error_status_pairing_case(Code, Statuses, Reason) ->
    {"grpc status/code " ++ binary_to_list(Code), fun() ->
        StatusCell = atomics:new(2, []),
        lists:foreach(
            fun(RequestId) ->
                Transport = fun(_, _, _, _) ->
                    atomics:add(StatusCell, 2, 1),
                    {error, #{
                        grpc_status => atomics:get(StatusCell, 1),
                        metadata => #{
                            <<"x-textguard-error-code">> =>
                                case is_list(RequestId) of
                                    true -> [Code, Code];
                                    false -> Code
                                end,
                            <<"x-textguard-request-id">> => RequestId
                        }
                    }}
                end,
                application:set_env(imboy, moderation_detector, [
                    {grpc_host, <<"127.0.0.1">>},
                    {grpc_port, 7464},
                    {grpc_transport, Transport}
                ]),
                try
                    lists:foreach(
                        fun(Status) ->
                            atomics:put(StatusCell, 1, Status),
                            Expected =
                                case lists:member(Status, Statuses) of
                                    true -> Reason;
                                    false -> protocol_error
                                end,
                            ?assertEqual(
                                {Code, Status, {error, Expected}},
                                {Code, Status, moderation_detector_grpc:check(detection(), #{})}
                            )
                        end,
                        lists:seq(0, 16)
                    )
                after
                    application:unset_env(imboy, moderation_detector)
                end
            end,
            [<<"req-t15-0001">>, <<>>, [<<"req-t15-0001">>, <<"req-t15-0001">>], [<<>>, <<>>]]
        ),
        ?assertEqual(68, atomics:get(StatusCell, 2))
    end}.

grpc_error_metadata_test_() ->
    Good = #{
        <<"x-textguard-error-code">> => <<"OVERLOADED">>,
        <<"x-textguard-request-id">> => <<"req-t15-0001">>
    },
    Cases = [
        maps:remove(<<"x-textguard-error-code">>, Good),
        maps:remove(<<"x-textguard-request-id">>, Good),
        Good#{<<"x-textguard-request-id">> => <<"wrong">>},
        Good#{<<"x-textguard-error-code">> => <<"NEW_UNKNOWN_ERROR">>},
        Good#{<<"x-textguard-error-code">> => [<<"OVERLOADED">>, <<"TEXT_TOO_LARGE">>]},
        Good#{<<"x-textguard-request-id">> => [<<"req-t15-0001">>, <<"wrong">>]},
        #{<<"textguard-error-code">> => <<"OVERLOADED">>}
    ],
    [
        fun() ->
            application:set_env(
                imboy,
                moderation_detector,
                [
                    {grpc_host, <<"127.0.0.1">>},
                    {grpc_port, 7464},
                    {grpc_transport, fun(_, _, _, _) ->
                        {error, #{grpc_status => 8, metadata => Meta}}
                    end}
                ]
            ),
            try
                ?assertEqual(
                    {error, protocol_error}, moderation_detector_grpc:check(detection(), #{})
                )
            after
                application:unset_env(imboy, moderation_detector)
            end
        end
     || Meta <- Cases
    ].
