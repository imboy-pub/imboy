-module(account_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

init_returns_ok_when_execute_succeeds_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, []) ->
            ok
        end}
    ], fun() ->
        ?assertEqual(ok, account_ds:init())
    end).

init_returns_ok_when_execute_throws_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, []) ->
            erlang:error(db_down)
        end}
    ], fun() ->
        ?assertEqual(ok, account_ds:init())
    end).

allocate_returns_integer_id_on_cache_miss_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get_wait', 1, fun({local_cache, account_list}) ->
                undefined
            end},
            {'set', 4, fun({local_cache, account_list}, [], _Ttl, []) ->
                ok
            end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, []) ->
                ok
            end},
            {'query', 2, fun(_Sql, []) ->
                {ok, [#{<<"nextval">> => 1001}]}
            end}
        ]}
    ], fun() ->
        ?assertEqual(1001, account_ds:allocate())
    end).

allocate_uses_cached_ids_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get_wait', 1, fun({local_cache, account_list}) ->
            {ok, [50001, 50002]}
        end},
        {'set', 4, fun({local_cache, account_list}, [50002], _Ttl, []) ->
            ok
        end}
    ], fun() ->
        ?assertEqual(50001, account_ds:allocate())
    end).

allocate_returns_no_ids_error_when_source_empty_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get_wait', 1, fun({local_cache, account_list}) ->
                undefined
            end},
            {'set', 4, fun({local_cache, account_list}, [], 3, []) ->
                ok
            end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, []) ->
                ok
            end},
            {'query', 2, fun(_Sql, []) ->
                {error, no_connection}
            end}
        ]}
    ], fun() ->
        ?assertEqual({error, no_ids}, account_ds:allocate())
    end).
