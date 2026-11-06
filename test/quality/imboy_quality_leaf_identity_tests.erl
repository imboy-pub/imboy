%% Synthetic evidence only. Never discovers or executes the product universe.
-module(imboy_quality_leaf_identity_tests).
-include_lib("eunit/include/eunit.hrl").

context() ->
    #{
        candidate => <<"candidate-digest">>,
        build => <<"beam-digest">>,
        provision => <<"provision-digest">>,
        otp => list_to_binary(erlang:system_info(otp_release)),
        erts => list_to_binary(erlang:system_info(version)),
        root_contract => <<"synthetic-root-generator-fixture-contract-digest">>
    }.
metadata() -> #{source => {fixture, leaf, 0}, line => 7, desc => <<"leaf">>}.
entry() ->
    #{
        exit => ok,
        counts => #{pass => 1, fail => 0, skip => 0, cancel => 0},
        events => [
            {test_begin, [1, 1]},
            {leaf_metadata, [1, 1], metadata()},
            {test_end, [1, 1], ok, 0},
            {group_end, [1], 0},
            {group_end, [], 0},
            {result, {ok, [{pass, 1}, {fail, 0}, {skip, 0}, {cancel, 0}]}}
        ]
    }.
observed(Entries) ->
    imboy_quality_leaf_identity:observed_manifest(context(), maps:keys(Entries), Entries).

namespaced_stable_test() ->
    {ok, A} = observed(#{root_a => entry(), root_b => entry()}),
    {ok, B} = observed(maps:from_list([{root_b, entry()}, {root_a, entry()}])),
    ?assertEqual(A, B),
    Ids = [maps:get(id, L) || L <- maps:get(leaves, A)],
    ?assertEqual(2, length(lists:usort(Ids))),
    ?assertEqual(<<"6:root_a:1.1">>, imboy_quality_leaf_identity:global_id(root_a, [1, 1])).

exact_comparison_test() ->
    {ok, O} = observed(#{root_a => entry()}),
    E = O#{kind := expected},
    ?assertEqual(ok, imboy_quality_leaf_identity:compare(E, O)),
    ?assertEqual({error, invalid_manifest_kind}, imboy_quality_leaf_identity:compare(O, O)),
    ?assertEqual(
        {error, expected_manifest_unavailable},
        imboy_quality_leaf_identity:compare(undefined, O)
    ).

same_count_different_leaf_test() ->
    {ok, O} = observed(#{root_a => entry()}),
    [L] = maps:get(leaves, O),
    Other = L#{path := [2], id := imboy_quality_leaf_identity:global_id(root_a, [2])},
    ?assertMatch(
        {error, #{missing := [_], unexpected := [_]}},
        imboy_quality_leaf_identity:compare(O#{kind := expected, leaves := [Other]}, O)
    ).

metadata_change_rejected_test() ->
    {ok, O} = observed(#{root_a => entry()}),
    [L] = maps:get(leaves, O),
    Other = L#{metadata := (metadata())#{line := 8}},
    ?assertMatch(
        {error, #{missing := [_], unexpected := [_]}},
        imboy_quality_leaf_identity:compare(O#{kind := expected, leaves := [Other]}, O)
    ).

context_binding_test() ->
    {ok, O} = observed(#{root_a => entry()}),
    lists:foreach(
        fun(K) ->
            E = O#{kind := expected, context := (context())#{K := <<"different">>}},
            ?assertMatch({error, _}, imboy_quality_leaf_identity:compare(E, O)),
            lists:foreach(
                fun(C) ->
                    ?assertMatch(
                        {error, _},
                        imboy_quality_leaf_identity:observed_manifest(
                            C, [root_a], #{root_a => entry()}
                        )
                    ),
                    ?assertMatch(
                        {error, _},
                        imboy_quality_leaf_identity:compare(
                            O#{kind := expected, context := C}, O
                        )
                    )
                end,
                [maps:remove(K, context()), (context())#{K := <<>>}]
            )
        end,
        [candidate, build, provision, otp, erts, root_contract]
    ).

missing_or_zero_obligation_test() ->
    ?assertMatch(
        {error, _},
        imboy_quality_leaf_identity:observed_manifest(
            context(), [root_a, root_b], #{root_a => entry()}
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_leaf_identity:observed_manifest(
            context(), [root_a, root_a], #{root_a => entry()}
        )
    ),
    ?assertMatch({error, _}, observed(#{})),
    ?assertMatch(
        {error, _},
        observed(#{
            root_a => (entry())#{
                counts := #{pass => 0, fail => 0, skip => 0, cancel => 0}
            }
        })
    ).

invalid_event_streams_test() ->
    E = entry(),
    Events = maps:get(events, E),
    [Begin, Meta, End | Tail] = Events,
    Bad = [
        [Begin, End | Tail],
        [Begin, Meta, Meta, End | Tail],
        [Begin, Meta, End, Begin, Meta, End | Tail],
        [Begin, Meta, {test_cancel, test, [1, 1], timeout} | Tail],
        [Begin, Meta, {test_end, [1, 1], {skipped, skip}, 0} | Tail],
        [Begin, Meta | Tail],
        lists:droplast(Events),
        Events ++ [Begin],
        [Begin, {leaf_metadata, [2], metadata()}, End | Tail]
    ],
    lists:foreach(
        fun(X) ->
            ?assertMatch({error, _}, observed(#{root_a => E#{events := X}}))
        end,
        Bad
    ).

duplicate_manifest_identity_test() ->
    {ok, O} = observed(#{root_a => entry()}),
    [L] = maps:get(leaves, O),
    ?assertMatch(
        {error, _},
        imboy_quality_leaf_identity:compare(
            O#{kind := expected, leaves := [L, L]}, O
        )
    ).

real_listener_capture_test() ->
    {ok, Counts, Events} = imboy_quality_runner:worker_main(quality_fixture_pass_tests),
    {ok, O} = observed(#{
        quality_fixture_pass_tests =>
            #{exit => ok, counts => Counts, events => Events}
    }),
    ?assertEqual(1, maps:get(pass, Counts)),
    [#{id := Id, path := Path, metadata := M}] = maps:get(leaves, O),
    ?assertEqual({quality_fixture_pass_tests, ok_test, 0}, maps:get(source, M)),
    ?assert(is_integer(maps:get(line, M))),
    ?assert(maps:is_key(desc, M)),
    ?assertEqual(imboy_quality_leaf_identity:global_id(quality_fixture_pass_tests, Path), Id),
    ?assert(lists:member({leaf_metadata, Path, M}, Events)),
    ?assert(lists:member({test_begin, Path}, Events)).
