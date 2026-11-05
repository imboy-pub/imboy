-module(forward_attachment_context_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

context_cleanup_and_commit_test() ->
    Ctx = #{target_type => <<"c2c">>, target_id => 9, sender => 7, keys => [<<"owned/key">>]},
    ?assertEqual(
        {error, forward_reference_not_committed},
        forward_attachment_repo:with_context(Ctx, fun() -> {ok, <<"msg">>} end)
    ),
    Result = forward_attachment_repo:with_context(Ctx, fun() ->
        Nested = forward_attachment_repo:with_context(Ctx, fun() -> ok end),
        ?assertEqual({error, nested_forward_context}, Nested),
        ok = forward_attachment_repo:mark_committed(<<"c2c">>, <<"msg">>),
        {ok, <<"msg">>}
    end),
    ?assertEqual({ok, <<"msg">>}, Result),
    ?assertException(
        error,
        synthetic_failure,
        forward_attachment_repo:with_context(Ctx, fun() -> error(synthetic_failure) end)
    ),
    ?assertEqual(ok, forward_attachment_repo:accept_tx(undefined, <<"c2c">>, <<"m">>, 7, 9, 1)),
    ?assertEqual(
        {error, forward_reference_not_committed},
        forward_attachment_repo:with_context(Ctx, fun() ->
            ok = forward_attachment_repo:mark_committed(<<"c2g">>, <<"msg">>),
            {ok, <<"msg">>}
        end)
    ).

context_target_mismatch_test() ->
    Ctx = #{target_type => <<"c2c">>, target_id => 9, sender => 7, keys => [<<"owned/key">>]},
    ?assertThrow(
        {rollback, forward_context_mismatch},
        forward_attachment_repo:with_context(Ctx, fun() ->
            forward_attachment_repo:accept_tx(undefined, <<"c2c">>, <<"m">>, 7, 10, 1)
        end)
    ).

record_failure_aborts_target_transaction_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_, _, _) ->
                    {ok, [#{<<"allowed">> => true, <<"accepted">> => true}]}
                end}
            ]},
            {attachment_repo, [
                {'bind_reference_lock_tx', 2, fun(_, _) -> ok end},
                {'find_by_path_for_update_tx', 2, fun(_, _) ->
                    {ok, #{
                        <<"status">> => 1,
                        <<"lifecycle_state">> => <<"live">>,
                        <<"scope">> => <<"private">>,
                        <<"creator_user_id">> => 7
                    }}
                end},
                {'verify_direct_winner_tx', 2, fun(_, _) -> ok end}
            ]},
            {msg_forward_repo, [
                {'insert_tx', 2, fun(_, _) -> {error, synthetic_write_failure} end}
            ]}
        ],
        fun() ->
            Ctx = #{
                source_type => <<"c2c">>,
                source_id => <<"source">>,
                source_from => 7,
                source_target => 8,
                source_msg_type => <<"image">>,
                source_payload => #{<<"uri">> => <<"owned/key">>},
                target_type => <<"c2c">>,
                target_id => 9,
                sender => 7,
                keys => [<<"owned/key">>]
            },
            ?assertThrow(
                {rollback, {forward_attachment, synthetic_write_failure}},
                forward_attachment_repo:with_context(Ctx, fun() ->
                    forward_attachment_repo:accept_tx(conn, <<"c2c">>, <<"target">>, 7, 9, 1)
                end)
            )
        end
    ).
