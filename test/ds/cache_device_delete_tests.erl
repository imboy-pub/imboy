-module(cache_device_delete_tests).
-include_lib("eunit/include/eunit.hrl").

device_delete_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(failed_delete_does_not_revoke_or_purge()) end,
        fun(_) -> ?_test(committed_delete_reaches_invalidation()) end,
        fun(_) -> ?_test(failed_rename_does_not_invalidate()) end,
        fun(_) -> ?_test(committed_rename_invalidates_after_write()) end
    ]}.
setup() ->
    Old = application:get_env(imboy, sql_driver),
    application:set_env(imboy, sql_driver, pgsql),
    meck:new(elib_pg, [no_link, passthrough]),
    meck:expect(elib_pg, execute, fun(_, _) -> {error, unavailable} end),
    meck:new(olm_identity_repo, [no_link, passthrough]),
    meck:expect(olm_identity_repo, delete_by_device, fun(_, _) -> {ok, 0} end),
    meck:new(imboy_cache, [no_link, passthrough]),
    meck:expect(imboy_cache, invalidate, fun(_) -> ok end),
    meck:new(imboy_syn, [no_link, passthrough]),
    meck:expect(imboy_syn, list_by_uid, fun(_) -> [] end),
    meck:expect(elib_pg, execute_count, fun(_, _) -> {error, unavailable} end),
    Old.
cleanup(Old) ->
    try
        [meck:unload(M) || M <- [imboy_syn, imboy_cache, olm_identity_repo, elib_pg]]
    after
        case Old of
            undefined -> application:unset_env(imboy, sql_driver);
            {ok, V} -> application:set_env(imboy, sql_driver, V)
        end
    end.
failed_delete_does_not_revoke_or_purge() ->
    ?assertError(device_delete_failed, user_device_logic:delete(1, <<"fixture_device">>)),
    ?assertEqual(0, meck:num_calls(olm_identity_repo, delete_by_device, 2)),
    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)),
    ?assertEqual(0, meck:num_calls(imboy_syn, list_by_uid, 1)).
committed_delete_reaches_invalidation() ->
    meck:expect(elib_pg, execute, fun(_, _) -> {ok, 1} end),
    ?assertEqual(ok, user_device_logic:delete(1, <<"fixture_device">>)),
    ?assertEqual(1, meck:num_calls(olm_identity_repo, delete_by_device, 2)),
    ?assertEqual(2, meck:num_calls(imboy_cache, invalidate, 1)),
    ?assertEqual(1, meck:num_calls(imboy_syn, list_by_uid, 1)).

failed_rename_does_not_invalidate() ->
    ?assertError(
        device_rename_failed,
        user_device_logic:change_name(1, <<"fixture_device">>, <<"fixture_name">>)
    ),
    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)).
committed_rename_invalidates_after_write() ->
    meck:expect(elib_pg, execute_count, fun(_, _) -> {ok, 1} end),
    meck:expect(imboy_cache, invalidate, fun(Key) ->
        ?assertEqual({user_device_name, 2, 1, <<"fixture_device">>}, Key),
        ?assertEqual(1, meck:num_calls(elib_pg, execute_count, 2)),
        ok
    end),
    ?assertEqual(
        ok,
        user_device_logic:change_name(1, <<"fixture_device">>, <<"fixture_name">>)
    ),
    ?assertEqual(1, meck:num_calls(imboy_cache, invalidate, 1)).
