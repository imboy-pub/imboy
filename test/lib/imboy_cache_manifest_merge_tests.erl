-module(imboy_cache_manifest_merge_tests).
-include_lib("eunit/include/eunit.hrl").
-include("imboy_cache_sync_state.hrl").

base_and_journal_merge_to_final_delete_test() ->
    {Key, Exp, Rev} = sample(),
    Live = row({Key, old_value, Exp, Rev}),
    Delete = row({deleted, Key, undefined, Exp, {2, node()}}),
    ?assertEqual({ok, #{Key => Delete}}, final([[Live], [Delete]])),
    ?assertEqual({ok, #{Key => Delete}}, final([[Delete], [Live]])).

same_revision_payload_and_expiry_conflicts_are_rejected_test() ->
    {Key, Exp, Rev} = sample(),
    A = row({Key, value_a, Exp, Rev}),
    B = row({Key, value_b, Exp, Rev}),
    C = row({Key, value_a, Exp + 1, Rev}),
    [?assertMatch({error, _}, final(Pages)) || Pages <- [[[A], [B]], [[B], [A]], [[A], [C]]]].

wrapper_difference_does_not_manufacture_payload_conflict_test() ->
    {Key, Exp, Rev} = sample(),
    Body = {Key, value, Exp, Rev},
    Plain = row(Body),
    Known = row({with_origin, {node(), <<1:128>>, 1}, Body}),
    ?assert(element(6, Plain) =/= element(6, Known)),
    ?assert(element(7, Plain) =:= element(7, Known)),
    ?assertMatch({ok, _}, final([[Plain], [Known]])),
    ?assertMatch({ok, _}, final([[Known], [Plain]])),
    Wrong = row({with_origin, {node(), <<2:128>>, 1}, Body}),
    ?assertMatch({error, _}, final([[Known], [Wrong]])).

unavailable_same_revision_keeps_live_and_delete_cannot_change_kind_test() ->
    {Key, Exp, Rev} = sample(),
    Live = row({Key, value, Exp, Rev}),
    Floor = row({unavailable, Key, undefined, Exp, Rev}),
    Delete = row({deleted, Key, undefined, Exp, Rev}),
    ?assertEqual({ok, #{Key => Live}}, final([[Live], [Floor]])),
    ?assertEqual({ok, #{Key => Live}}, final([[Floor], [Live]])),
    ?assertMatch({error, _}, final([[Delete], [Floor]])),
    ?assertMatch({error, _}, final([[Floor], [Delete]])).

missing_or_duplicate_page_set_cannot_be_final_manifest_test() ->
    ?assertMatch(
        {error, _}, imboy_cache_policy:final_snapshot_manifest(#{pages => 2, manifest => [{1, []}]})
    ),
    ?assertMatch(
        {error, _},
        imboy_cache_policy:final_snapshot_manifest(#{pages => 2, manifest => [{1, []}, {1, []}]})
    ),
    ?assertEqual({ok, #{}}, final([])).

cross_source_same_revision_conflict_cannot_be_merged_test() ->
    {Key, Exp, Rev} = sample(),
    A = #{authority => true, pages => 1, manifest => [{1, [row({Key, a, Exp, Rev})]}]},
    B = #{authority => true, pages => 1, manifest => [{1, [row({Key, b, Exp, Rev})]}]},
    ?assertMatch(
        {error, _},
        imboy_cache_policy:final_snapshot_manifest(#{
            deadline_ms => Exp, peers => #{a => A, b => B}
        })
    ),
    ?assertMatch(
        {error, _},
        imboy_cache_policy:final_snapshot_manifest(#{
            deadline_ms => Exp, peers => #{b => A, a => B}
        })
    ),
    ?assertEqual(
        {ok, #{Key => row({Key, a, Exp, Rev})}},
        imboy_cache_policy:final_snapshot_manifest(#{deadline_ms => Exp, peers => #{a => A, b => A}})
    ).

normalization_has_its_own_bounded_workspace_test() ->
    Original = application:get_env(imboy, cache_replication),
    {Key, Exp, Rev} = sample(),
    try
        application:set_env(imboy, cache_replication, #{b_max_bytes => 1}),
        ?assertMatch({error, _}, final([[row({Key, value, Exp, Rev})]]))
    after
        case Original of
            undefined -> application:unset_env(imboy, cache_replication);
            {ok, C} -> application:set_env(imboy, cache_replication, C)
        end
    end.

conflicting_final_content_cannot_ack_done_test() ->
    {Key, Exp, Rev} = sample(),
    Token = <<1:96>>,
    Peer = #{
        authority => true,
        reported => true,
        requested => true,
        done => false,
        watermark => 0,
        pages => 2,
        expected_more => false,
        max_revision_counter => 1,
        source_cut => diagnostic_cut,
        manifest => [{1, [row({Key, a, Exp, Rev})]}, {2, [row({Key, b, Exp, Rev})]}]
    },
    Round = #{round => Token, deadline_ms => Exp, peers => #{node() => Peer}},
    ?assertEqual(
        {abort, snapshot_content_conflict, Round},
        imboy_cache_sync_snapshot:handle_done(node(), Token, 1, 2, Round)
    ),
    Valid = Peer#{manifest := [{1, [row({Key, a, Exp, Rev})]}, {2, []}]},
    ?assertMatch(
        {ok, _},
        imboy_cache_sync_snapshot:handle_done(
            node(),
            Token,
            1,
            2,
            Round#{peers := #{node() => Valid}}
        )
    ).

covered_origin_absence_is_rejected_in_both_source_orders_test() ->
    {Key, Exp, Rev} = sample(),
    Boot = <<1:128>>,
    Known = row({with_origin, {a, Boot, 7}, {Key, value, Exp, Rev}}),
    A = source(a, Boot, 7, [Known], [], Exp),
    B = source(b, <<2:128>>, 1, [], [{a, Boot, 0, 7}], Exp),
    [
        ?assertEqual({error, snapshot_absence_unproven}, verify(Peers, Exp))
     || Peers <- [
            #{a => A, b => B},
            #{a => A, '0' => source('0', <<2:128>>, 1, [], [{a, Boot, 0, 7}], Exp)}
        ]
    ].

empty_source_cannot_manufacture_deletion_test() ->
    {Key, Exp, Rev} = sample(),
    Boot = <<1:128>>,
    Known = row({with_origin, {a, Boot, 7}, {Key, value, Exp, Rev}}),
    A = source(a, Boot, 7, [Known], [], Exp),
    B = source(b, <<2:128>>, 0, [], [], Exp),
    ?assertEqual({ok, #{Key => Known}}, verify(#{a => A, b => B}, Exp)).

covered_content_regression_and_missing_target_are_rejected_test() ->
    {Key, Exp, Rev} = sample(),
    Boot = <<1:128>>,
    Old = row({with_origin, {a, Boot, 6}, {Key, old, Exp, Rev}}),
    New = row({with_origin, {a, Boot, 7}, {Key, new, Exp, {2, node()}}}),
    A = source(a, Boot, 7, [New], [], Exp),
    B = source(b, <<2:128>>, 1, [Old], [{a, Boot, 0, 7}], Exp),
    ?assertEqual({error, snapshot_covered_content_regressed}, verify(#{a => A, b => B}, Exp)),
    Target = #{authority => false, origin_target => {c, <<3:128>>, 0, 1}},
    ?assertMatch({error, _}, verify(#{a => A, c => Target}, Exp)).

unknown_provenance_and_failed_coverage_cannot_certify_ready_test() ->
    {Key, Exp, Rev} = sample(),
    A = source(a, <<1:128>>, 1, [row({Key, value, Exp, Rev})], [], Exp),
    ?assertEqual({error, snapshot_provenance_unproven}, verify(#{a => A}, Exp)),
    Cut = maps:get(source_cut, A),
    Failed = A#{source_cut := setelement(9, Cut, {error, incomplete_application_coverage})},
    ?assertMatch({error, _}, verify(#{a => Failed}, Exp)).

compatible_plain_row_never_discards_known_provenance_test() ->
    {Key, Exp, Rev} = sample(),
    Body = {Key, value, Exp, Rev},
    Known = row({with_origin, {a, <<1:128>>, 1}, Body}),
    Plain = row(Body),
    ?assertEqual({ok, #{Key => Known}}, final([[Plain], [Known]])),
    ?assertEqual({ok, #{Key => Known}}, final([[Known], [Plain]])).

late_delta_final_effect_is_not_lost_to_old_snapshot_test() ->
    {Key, Exp, Rev} = sample(),
    Boot = <<1:128>>,
    Old = row({with_origin, {a, Boot, 7}, {Key, old, Exp, Rev}}),
    A = source(a, Boot, 7, [Old], [], Exp),
    Round = #{
        deadline_ms => Exp, cohort_epoch => 0, peers => #{a => A}, retained_obligations => []
    },
    Event = #{
        operation => delete,
        key => Key,
        revision => {2, node()},
        expires_at_ms => Exp,
        origin_node => a,
        origin_boot_id => Boot,
        epoch => 0,
        origin_seq => 8
    },
    {ok, S} = imboy_cache_sync_coverage:record_delta(
        Event,
        #state{snapshot_round = Round},
        {1000000, 100},
        2
    ),
    {ok, Final} = imboy_cache_sync_coverage:verify(
        S#state.snapshot_round,
        {1000000, 100},
        erlang:system_time(millisecond)
    ),
    ?assertEqual(deleted, element(1, maps:get(Key, Final))),
    ?assertEqual({a, Boot, 8}, element(5, maps:get(Key, Final))),
    ?assertEqual(
        #{{a, Boot} => 8}, imboy_cache_sync_coverage:certified_prefixes(S#state.snapshot_round)
    ),
    ?assertEqual(
        #{{a, Boot} => 7},
        imboy_cache_sync_coverage:certified_prefixes(
            (S#state.snapshot_round)#{delta_receipts := #{{a, Boot, 0, 9} => true}}
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_cache_sync_coverage:record_delta(
            Event#{key := another_key}, S, {1000000, 100}, 1
        )
    ),
    ?assertEqual(
        {error, snapshot_invalidated},
        imboy_cache_sync_coverage:record_delta(
            #{operation => invalidate}, S, {1000000, 100}, 2
        )
    ).

retained_receiver_key_cannot_disappear_behind_empty_source_cut_test() ->
    {Key, Exp, Rev} = sample(),
    Boot = <<1:128>>,
    A = source(a, Boot, 7, [], [], Exp),
    Round = #{
        deadline_ms => Exp,
        cohort_epoch => 0,
        peers => #{a => A},
        retained_obligations => [{{rev, Key}, {Rev, {Boot, 7}, Exp, live, 0}}]
    },
    ?assertEqual(
        {error, retained_content_missing},
        imboy_cache_sync_coverage:verify(
            Round, {1000000, 100}, erlang:system_time(millisecond)
        )
    ).

non_authority_zero_sequence_is_not_an_empty_content_census_test() ->
    {_, Exp, _} = sample(),
    A = source(a, <<1:128>>, 0, [], [], Exp),
    C = #{authority => false, origin_target => {c, <<3:128>>, 0, 0}},
    A0 = A#{source_cut := setelement(9, maps:get(source_cut, A), {ok, [{c, <<3:128>>, 0, 0}]})},
    ?assertEqual({error, peer_content_unproven}, verify(#{a => A0, c => C}, Exp)).

source(Node, Boot, Seq, Rows, Vector, _Exp) ->
    #{
        authority => true,
        pages => 1,
        manifest => [{1, Rows}],
        origin_target => {Node, Boot, 0, Seq},
        source_cut =>
            {cache_snapshot_cut, Node, <<1:96>>, Boot, 0, Seq, <<0:128>>, <<0:128>>, {ok, Vector},
                imboy_cache_sync_coverage:manifest_certificate(
                    maps:from_list([{element(2, R), R} || R <- Rows])
                )}
    }.
verify(Peers, Exp) ->
    imboy_cache_sync_coverage:verify(
        #{deadline_ms => Exp, cohort_epoch => 0, peers => Peers, retained_obligations => []},
        {1000000, 100},
        erlang:system_time(millisecond)
    ).

omitted_source_content_cannot_pass_with_empty_receiver_and_high_cut_test() ->
    {Key, Exp, _} = sample(),
    R = row({with_origin, {a, <<1:128>>, 7}, {Key, retained, Exp, {7, a}}}),
    A = source(a, <<1:128>>, 7, [R], [], Exp),
    ?assertMatch({ok, _}, verify(#{a => A}, Exp)),
    ?assertMatch({error, _}, verify(#{a => A#{manifest := [{1, []}]}}, Exp)),
    Cut = maps:get(source_cut, A),
    ?assertEqual(
        {error, source_content_certificate_missing},
        verify(#{a => A#{source_cut := erlang:delete_element(10, Cut)}}, Exp)
    ).

page_digest_binds_payload_expiry_and_origin_test() ->
    {Key, Exp, Rev} = sample(),
    Round = <<1:96>>,
    Entry = {Key, old, Exp, Rev},
    Digest = imboy_cache_policy:snapshot_page_digest(Round, 1, [Entry]),
    ?assertNotEqual(
        Digest, imboy_cache_policy:snapshot_page_digest(Round, 1, [{Key, new, Exp, Rev}])
    ),
    ?assertNotEqual(
        Digest, imboy_cache_policy:snapshot_page_digest(Round, 1, [{Key, old, Exp + 1, Rev}])
    ),
    ?assertNotEqual(
        Digest,
        imboy_cache_policy:snapshot_page_digest(Round, 1, [
            {with_origin, {node(), <<1:128>>, 1}, Entry}
        ])
    ).

sample() -> {{manifest_merge, key}, erlang:system_time(millisecond) + 60000, {1, node()}}.
row(Wire) -> imboy_cache_policy:snapshot_manifest_entry(Wire).
final(Pages) ->
    imboy_cache_policy:final_snapshot_manifest(#{
        pages => length(Pages),
        manifest => lists:zip(lists:seq(1, length(Pages)), Pages)
    }).
