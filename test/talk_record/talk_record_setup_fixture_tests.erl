-module(talk_record_setup_fixture_tests).
-include_lib("eunit/include/eunit.hrl").

success_preserves_result_without_cleanup_test() ->
    Ref = make_ref(),
    Result = talk_record_setup_fixture:prepare(
        original,
        fun() -> {enriched, Ref} end,
        fun(_) -> self() ! unexpected_cleanup end
    ),
    ?assertEqual({enriched, Ref}, Result),
    receive
        unexpected_cleanup -> error(cleaned_live_fixture)
    after 0 -> ok
    end.

failure_classes_preserve_original_and_cleanup_once_test_() ->
    [?_test(check_failure(Class)) || Class <- [error, exit, throw]].

check_failure(Class) ->
    Ref = make_ref(),
    try
        talk_record_setup_fixture:prepare(
            Ref,
            fun() -> erlang:raise(Class, injected_seed_failure, []) end,
            fun(State) -> self() ! {cleaned, State} end
        ),
        error(failure_swallowed)
    catch
        Class:injected_seed_failure -> ok
    end,
    receive
        {cleaned, Ref} -> ok
    after 0 -> error(cleanup_missing)
    end,
    receive
        {cleaned, Ref} -> error(double_cleanup)
    after 0 -> ok
    end.

cleanup_failure_reports_both_errors_test() ->
    ?assertException(
        error,
        {fixture_cleanup_failed, {error, seed_failed, _}, {exit, cleanup_failed, _}},
        talk_record_setup_fixture:prepare(
            state,
            fun() -> error(seed_failed) end,
            fun(state) -> exit(cleanup_failed) end
        )
    ).

partial_acquisition_preserves_foreign_mock_test() ->
    pure_mock_fixture:run(fun() ->
        ok = meck:new(elib_param, [non_strict, no_link]),
        Foreign = whereis(elib_param_meck),
        try
            ?assertException(
                error,
                {already_started, _},
                talk_record_setup_fixture:mocks([elib_response, elib_param], fun() -> ok end)
            ),
            ?assertNot(lists:member(elib_response, meck:mocked())),
            ?assertEqual(Foreign, whereis(elib_param_meck)),
            ?assert(is_process_alive(Foreign))
        after
            meck:unload(elib_param)
        end
    end).

configure_failure_releases_all_owned_mocks_test() ->
    pure_mock_fixture:run(fun() ->
        ?assertException(
            error,
            configure_failed,
            talk_record_setup_fixture:mocks(
                [elib_response, elib_param],
                fun() -> error(configure_failed) end
            )
        ),
        ?assertNot(lists:member(elib_response, meck:mocked())),
        ?assertNot(lists:member(elib_param, meck:mocked()))
    end).

successful_acquisition_has_idempotent_owned_cleanup_test() ->
    pure_mock_fixture:run(fun() ->
        Owners = talk_record_setup_fixture:mocks([elib_response, elib_param], fun() -> ok end),
        ?assertEqual(2, map_size(Owners)),
        ok = talk_record_setup_fixture:unload(Owners),
        ok = talk_record_setup_fixture:unload(Owners),
        ?assertNot(lists:member(elib_response, meck:mocked())),
        ?assertNot(lists:member(elib_param, meck:mocked()))
    end).

replacement_mock_is_never_unloaded_test() ->
    pure_mock_fixture:run(fun() ->
        Owners = talk_record_setup_fixture:mocks([elib_response], fun() -> ok end),
        ok = meck:unload(elib_response),
        ok = meck:new(elib_response, [non_strict, no_link]),
        Replacement = whereis(elib_response_meck),
        try
            ?assertException(
                error,
                {handler_mock_ownership_lost, elib_response},
                talk_record_setup_fixture:unload(Owners)
            ),
            ?assertEqual(Replacement, whereis(elib_response_meck)),
            ?assert(is_process_alive(Replacement))
        after
            meck:unload(elib_response)
        end
    end).
