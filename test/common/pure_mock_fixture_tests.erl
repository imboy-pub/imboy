-module(pure_mock_fixture_tests).
-include_lib("eunit/include/eunit.hrl").

declared_calls_and_environment_restore_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            application:set_env(imboy, pure_mock_probe, original),
            Before = lists:sort(application:get_all_env(imboy)),
            ok = pure_mock_fixture:with_mocks(
                [{elib_pg, [{query, 2, fun(_, _) -> declared_result end}]}],
                fun() ->
                    ?assertEqual(declared_result, elib_pg:query(<<"synthetic">>, [])),
                    application:set_env(imboy, pure_mock_probe, changed),
                    ok
                end
            ),
            ?assertEqual(Before, lists:sort(application:get_all_env(imboy))),
            ?assertEqual([], meck:mocked())
        end)
    end}.

swallowed_undeclared_call_still_fails_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            Before = lists:sort(application:get_all_env(imboy)),
            ?assertError(
                {badmatch, true},
                pure_mock_fixture:with_mocks(
                    [{elib_pg, [{query, 2, fun(_, _) -> declared_result end}]}],
                    fun() ->
                        try elib_pg:execute(<<"must never reach PG">>, []) of
                            _ -> error(undeclared_call_was_allowed)
                        catch
                            error:Reason -> put(pure_mock_guard_reason, Reason)
                        end,
                        ok
                    end
                )
            ),
            ?assertEqual(
                {undeclared_mock_call, elib_pg, execute, 2},
                erase(pure_mock_guard_reason)
            ),
            ?assertEqual(Before, lists:sort(application:get_all_env(imboy))),
            ?assertEqual([], meck:mocked())
        end)
    end}.

dynamic_declared_export_remains_supported_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            {module, elib_pg} = code:ensure_loaded(elib_pg),
            Original = lists:sort(elib_pg:module_info(exports)),
            ?assertNot(lists:member({pure_mock_contract_probe, 0}, Original)),
            ok = pure_mock_fixture:with_mocks(
                [{elib_pg, [{pure_mock_contract_probe, 0, fun() -> dynamic_result end}]}],
                fun() ->
                    ?assertEqual(dynamic_result, elib_pg:pure_mock_contract_probe()),
                    ok
                end
            ),
            ?assertEqual(Original, lists:sort(elib_pg:module_info(exports))),
            ?assertEqual([], meck:mocked())
        end)
    end}.

body_exception_preserves_cleanup_test_() ->
    {timeout, 30, fun() ->
        pure_mock_fixture:run(fun() ->
            Before = lists:sort(application:get_all_env(imboy)),
            ?assertThrow(
                synthetic_body_failure,
                pure_mock_fixture:with_mocks(
                    [{elib_pg, [{query, 2, fun(_, _) -> declared_result end}]}],
                    fun() ->
                        application:set_env(imboy, pure_mock_probe, changed),
                        throw(synthetic_body_failure)
                    end
                )
            ),
            ?assertEqual(Before, lists:sort(application:get_all_env(imboy))),
            ?assertEqual([], meck:mocked())
        end)
    end}.
