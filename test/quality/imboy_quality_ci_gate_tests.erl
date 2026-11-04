-module(imboy_quality_ci_gate_tests).
-include_lib("eunit/include/eunit.hrl").

ops() -> [<<"eunit_full">>, <<"pbt_quick">>, <<"pbt_nightly">>, <<"critical_ct">>, <<"guard">>].
properties() ->
    #{
        <<"f01_frame_roundtrip_wire">> => ordinary,
        <<"f02_frame_fragment_recovery">> => ordinary,
        <<"d01_same_instant_offsets">> => ordinary,
        <<"d02_precision_scaling">> => ordinary,
        <<"c01_canonical_reference">> => ordinary,
        <<"c02_illegal_injection">> => ordinary,
        <<"s01_tsid_bit_layout">> => ordinary,
        <<"s02_tsid_interval_reservation">> => ordinary,
        <<"q01_tsid_reserve_sequences">> => sequence,
        <<"m01_full_manifest_model">> => ordinary,
        <<"m02_entry_idempotence_page_integrity">> => ordinary,
        <<"m03_revision_conflict_compat">> => ordinary,
        <<"oracle01_frame_corruption">> => ordinary,
        <<"oracle02_offset_reversal">> => ordinary,
        <<"oracle03_tsid_slot_reuse">> => ordinary,
        <<"oracle04_stale_live_over_delete">> => ordinary
    }.
context() ->
    #{
        head => <<"candidate">>,
        input_fingerprint => <<"input">>,
        u_fingerprint => <<"universe">>,
        artifacts => [{Op, true} || Op <- ops()],
        expected_properties => properties(),
        expected_full_roots => [<<"root1">>, <<"root2">>],
        expected_leaf_ids => #{
            <<"root1">> => [<<"leaf1">>, <<"leaf2">>], <<"root2">> => [<<"leaf3">>]
        },
        expected_critical_suites => [
            <<"msg_delivery_SUITE">>,
            <<"msg_ack_logic_SUITE">>,
            <<"imboy_quality_ct02_org_boundary_SUITE">>,
            <<"imboy_quality_ct03_migration_SUITE">>
        ],
        expected_guard_checks => [<<"guard1">>]
    }.
item(Id) -> #{id => Id, executed => 1, passed => 1, fail => 0, skip => 0, cancel => 0}.
pbt_item(Id, Kind, Profile) ->
    N =
        case {Profile, Kind} of
            {quick, ordinary} -> 200;
            {quick, sequence} -> 100;
            {nightly, ordinary} -> 2000;
            {nightly, sequence} -> 500
        end,
    (item(Id))#{
        requested => N,
        performed => N,
        completion => 1,
        random_started => 1,
        witnesses_passed_count => 1
    }.
receipt(Op) ->
    C = context(),
    Items =
        case Op of
            <<"eunit_full">> ->
                [
                    begin
                        L = maps:get(I, maps:get(expected_leaf_ids, C)),
                        (item(I))#{leaf_ids => L, executed := length(L), passed := length(L)}
                    end
                 || I <- maps:get(expected_full_roots, C)
                ];
            <<"critical_ct">> ->
                [item(I) || I <- maps:get(expected_critical_suites, C)];
            <<"guard">> ->
                [item(I) || I <- maps:get(expected_guard_checks, C)];
            <<"pbt_quick">> ->
                [pbt_item(I, K, quick) || {I, K} <- maps:to_list(properties())];
            <<"pbt_nightly">> ->
                [pbt_item(I, K, nightly) || {I, K} <- maps:to_list(properties())]
        end,
    #{
        op => Op,
        candidate_sha => <<"candidate">>,
        input_fingerprint => <<"input">>,
        u_fingerprint => <<"universe">>,
        evidence_ref => <<"raw-reference">>,
        verdict => <<"all_passed">>,
        total => length(Items),
        passed => length(Items),
        fail => 0,
        skip => 0,
        cancel => 0,
        roots_missing => 0,
        cleanup_ok => true,
        assertions_ok => true,
        results => Items
    }.
receipts() -> [receipt(Op) || Op <- ops()].
complete_test() -> ?assertEqual(pass, imboy_quality_ci:gate(receipts(), context())).
ordinary_cannot_use_sequence_budget_test() ->
    Id = <<"f01_frame_roundtrip_wire">>,
    BadContext = (context())#{expected_properties := (properties())#{Id := sequence}},
    BadReceipts = [
        case maps:get(op, R) of
            <<"pbt_quick">> ->
                R#{
                    results := [
                        case maps:get(id, I) of
                            Id -> I#{requested := 100, performed := 100};
                            _ -> I
                        end
                     || I <- maps:get(results, R)
                    ]
                };
            <<"pbt_nightly">> ->
                R#{
                    results := [
                        case maps:get(id, I) of
                            Id -> I#{requested := 500, performed := 500};
                            _ -> I
                        end
                     || I <- maps:get(results, R)
                    ]
                };
            _ ->
                R
        end
     || R <- receipts()
    ],
    ?assertMatch({fail, _}, imboy_quality_ci:gate(BadReceipts, BadContext)).
missing_required_test() ->
    [Full, Quick, Night, Ct, Guard] = receipts(),
    ?assertMatch({fail, _}, imboy_quality_ci:gate([Quick, Night, Ct, Guard], context())),
    ?assertMatch({fail, _}, imboy_quality_ci:gate([Full, Quick, Ct, Guard], context())),
    ?assertMatch(
        {fail, _}, imboy_quality_ci:gate([Full, Quick, Night, Ct, Guard, Guard], context())
    ),
    ?assertMatch(
        {fail, _}, imboy_quality_ci:gate(receipts(), maps:remove(expected_properties, context()))
    ),
    ?assertMatch(
        {fail, _}, imboy_quality_ci:gate(receipts(), (context())#{expected_properties := #{}})
    ),
    ?assertMatch(
        {fail, _},
        imboy_quality_ci:gate(receipts(), (context())#{
            expected_properties := maps:remove(<<"oracle01_frame_corruption">>, properties())
        })
    ),
    ?assertMatch(
        {fail, _},
        imboy_quality_ci:gate(receipts(), (context())#{
            expected_critical_suites := [<<"msg_delivery_SUITE">>]
        })
    ).
mixed_identity_test() ->
    [Full | Rest] = receipts(),
    lists:foreach(
        fun(Bad) -> ?assertMatch({fail, _}, imboy_quality_ci:gate([Bad | Rest], context())) end,
        [
            Full#{candidate_sha := <<"other">>},
            Full#{input_fingerprint := <<"other">>},
            Full#{u_fingerprint := <<"other">>},
            maps:remove(u_fingerprint, Full)
        ]
    ).
failed_evidence_test() ->
    [Full, Quick, Night, Ct, Guard] = receipts(),
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {fail, _}, imboy_quality_ci:gate([Bad, Quick, Night, Ct, Guard], context())
            )
        end,
        [
            Full#{fail := 1},
            Full#{skip := 1},
            Full#{cancel := 1},
            Full#{cleanup_ok := false},
            Full#{assertions_ok := false},
            maps:remove(results, Full),
            Full#{results := [item(<<"root1">>)]},
            begin
                [Root | Roots] = maps:get(results, Full),
                Full#{results := [Root#{leaf_ids := [<<"leaf1">>]} | Roots]}
            end
        ]
    ),
    [Q | Qs] = maps:get(results, Quick),
    lists:foreach(
        fun(BadItem) ->
            ?assertMatch(
                {fail, _},
                imboy_quality_ci:gate(
                    [Full, Quick#{results := [BadItem | Qs]}, Night, Ct, Guard], context()
                )
            )
        end,
        [
            Q#{performed := 1},
            Q#{completion := 0},
            Q#{random_started := 0},
            Q#{skip := 1},
            Q#{witnesses_passed_count := 0}
        ]
    ),
    [Suite | Suites] = maps:get(results, Ct),
    ?assertMatch(
        {fail, _},
        imboy_quality_ci:gate(
            [
                Full,
                Quick,
                Night,
                Ct#{results := [Suite#{executed := 0, passed := 0} | Suites]},
                Guard
            ],
            context()
        )
    ),
    ?assertMatch({fail, _}, imboy_quality_ci:gate(receipts(), (context())#{artifacts := []})).
