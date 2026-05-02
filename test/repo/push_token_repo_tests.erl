-module(push_token_repo_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try Fun()
        after meck:unload(Modules)
        end
    end)()
).

%% ===================================================================
%% Tests
%% ===================================================================

tablename_test() ->
    ?WITH_MECKS([elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) -> <<"public.push_token">> end),
        ?assertEqual(<<"public.push_token">>, push_token_repo:tablename())
    end).

upsert_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql, elib_tsid], fun() ->
        Now = <<"2026-04-04T00:00:00Z">>,
        meck:expect(elib_dt, now, fun() -> Now end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) -> <<"public.push_token">> end),
        meck:expect(elib_tsid, generate, fun(push_token) -> 9001 end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 1} end),
        meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
        ?assertEqual({ok, 9001}, push_token_repo:upsert(1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token123">>)),
        %% 验证先 deactivate (execute) 再 insert (query)
        ?assert(meck:num_calls(elib_pg, execute, '_') >= 1),
        ?assert(meck:num_calls(elib_pg, query, '_') >= 1)
    end).

deactivate_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql], fun() ->
        meck:expect(elib_dt, now, fun() -> <<"2026-04-04T00:00:00Z">> end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) -> <<"public.push_token">> end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 1} end),
        ?assertEqual({ok, 1}, push_token_repo:deactivate(1, <<"did1">>))
    end).

deactivate_by_token_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql], fun() ->
        meck:expect(elib_dt, now, fun() -> <<"2026-04-04T00:00:00Z">> end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) -> <<"public.push_token">> end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 1} end),
        ?assertEqual({ok, 1}, push_token_repo:deactivate_by_token(<<"token123">>))
    end).

list_by_uid_test() ->
    ?WITH_MECKS([elib_pg, elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) -> <<"public.push_token">> end),
        Expected = {ok, [], [{<<"did1">>, <<"android">>, <<"fcm">>, <<"token123">>}]},
        meck:expect(elib_pg, query, fun(_Sql, [1]) -> Expected end),
        ?assertEqual(Expected, push_token_repo:list_by_uid(1))
    end).

list_by_uids_empty_test() ->
    ?assertEqual({ok, []}, push_token_repo:list_by_uids([])).

list_by_uids_test() ->
    ?WITH_MECKS([elib_pg, elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) -> <<"public.push_token">> end),
        Expected = {ok, [], [{1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>}]},
        meck:expect(elib_pg, query, fun(_Sql, [[1, 2]]) -> Expected end),
        ?assertEqual(Expected, push_token_repo:list_by_uids([1, 2]))
    end).

delete_by_uid_test() ->
    ?WITH_MECKS([elib_pg, elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) -> <<"public.push_token">> end),
        meck:expect(elib_pg, execute, fun(_Sql, [1]) -> {ok, 3} end),
        ?assertEqual({ok, 3}, push_token_repo:delete_by_uid(1))
    end).
