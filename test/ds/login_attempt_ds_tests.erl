-module(login_attempt_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

ensure_server() ->
    case whereis(login_attempt_ds) of
        undefined ->
            {ok, _Pid} = login_attempt_ds:start_link(),
            ok;
        _Pid ->
            ok
    end,
    ets:delete_all_objects(login_attempt_ets),
    application:set_env(imboy, login_max_attempts, 5),
    application:set_env(imboy, login_lock_duration_minutes, 30),
    ok.

record_failure_and_reset_flow_test_() ->
    ?WITH_MECK(elib_log, [
        {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) ->
            ok
        end}
    ], fun() ->
        ok = ensure_server(),
        ?assertEqual({ok, 1}, login_attempt_ds:record_failure(<<"user@test.com">>, <<"127.0.0.1">>)),
        ?assertEqual({ok, 1}, login_attempt_ds:get_attempts(<<"user@test.com">>, <<"127.0.0.1">>)),
        ?assertEqual({ok, 4}, login_attempt_ds:get_remaining_attempts(<<"user@test.com">>, <<"127.0.0.1">>)),
        ?assertEqual(ok, login_attempt_ds:reset(<<"user@test.com">>, <<"127.0.0.1">>)),
        ?assertEqual({ok, 0}, login_attempt_ds:get_attempts(<<"user@test.com">>, <<"127.0.0.1">>))
    end).

record_failure_invalid_input_returns_error_test_() ->
    ?WITH_MECK(elib_log, [
        {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) ->
            ok
        end}
    ], fun() ->
        ?assertEqual({error, invalid_input}, login_attempt_ds:record_failure(<<>>, <<"127.0.0.1">>)),
        ?assertEqual({error, invalid_input}, login_attempt_ds:record_failure(<<"user@test.com">>, <<>>))
    end).

is_locked_after_reaching_limit_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) ->
                ok
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() ->
                1000
            end},
            {'add', 2, fun(1000, {30, minute}) ->
                2000
            end}
        ]}
    ], fun() ->
        ok = ensure_server(),
        application:set_env(imboy, login_max_attempts, 2),
        ?assertEqual({ok, 1}, login_attempt_ds:record_failure(<<"user@test.com">>, <<"127.0.0.1">>)),
        ?assertEqual({ok, 2}, login_attempt_ds:record_failure(<<"user@test.com">>, <<"127.0.0.1">>)),
        ?assertEqual(true, login_attempt_ds:is_locked(<<"user@test.com">>, <<"127.0.0.1">>))
    end).

cache_key_returns_tuple_test() ->
    ?assertEqual({login_attempt, <<"user@test.com">>, <<"127.0.0.1">>},
                 login_attempt_ds:cache_key(<<"user@test.com">>, <<"127.0.0.1">>)).

check_ip_rate_limit_success_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get', 1, fun({login_attempt_ip_limit, <<"127.0.0.1">>}) ->
            undefined
        end},
        {'set', 3, fun({login_attempt_ip_limit, <<"127.0.0.1">>}, 1, 3600) ->
            ok
        end}
    ], fun() ->
        ?assertEqual({ok, 1}, login_attempt_ds:check_ip_rate_limit(<<"127.0.0.1">>))
    end).
