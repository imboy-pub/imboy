-module(msg_c2g_read_failure_tests).
-include_lib("eunit/include/eunit.hrl").

read_storage_failure_is_retryable_test() ->
    with_read_result({error, unavailable}, fun() ->
        {reply, Reply} = msg_c2g_logic:c2g_read(<<"receipt">>, 1001, read_request()),
        Payload = maps:get(<<"payload">>, Reply),
        ?assertEqual(<<"storage_unavailable">>, maps:get(<<"error">>, Payload)),
        ?assertEqual(true, maps:get(<<"retryable">>, Payload)),
        ?assertEqual(false, maps:is_key(<<"read_seq">>, Payload)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, '_'))
    end).

read_noop_is_success_without_broadcast_test() ->
    with_read_result({ok, []}, fun() ->
        {reply, Reply} = msg_c2g_logic:c2g_read(<<"receipt">>, 1001, read_request()),
        Payload = maps:get(<<"payload">>, Reply),
        ?assertEqual(false, maps:is_key(<<"error">>, Payload)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, '_'))
    end).

read_request() ->
    #{<<"to">> => <<"100">>, <<"payload">> => #{<<"msg_ids">> => [<<"m1">>]}}.

with_read_result(Result, Test) ->
    Modules = [group_ds, msg_s2c_ds, elib_dt],
    try
        lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
        meck:expect(group_ds, is_member, fun(1001, 100) -> true end),
        meck:expect(group_ds, mark_read, fun(1001, 100, [<<"m1">>]) -> Result end),
        meck:expect(msg_s2c_ds, send, fun(_, _, _, _, _, _, _) -> ok end),
        meck:expect(elib_dt, millisecond, fun() -> 1000 end),
        Test()
    after
        lists:foreach(
            fun(M) ->
                try
                    meck:unload(M)
                catch
                    error:_ -> ok
                end
            end,
            Modules
        )
    end.
