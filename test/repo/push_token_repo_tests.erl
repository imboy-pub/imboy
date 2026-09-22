-module(push_token_repo_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(Modules)
        end
    end)()
).

%% 从进程邮箱取回 mock 捕获的参数（mock 内 self() ! Msg）
recv_captured(Tag) ->
    receive
        {Tag, Value} -> Value
    after 2000 ->
        erlang:error({capture_timeout, Tag})
    end.

%% ===================================================================
%% Tests
%% ===================================================================

tablename_test() ->
    ?WITH_MECKS([elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        ?assertEqual(<<"public.push_token">>, push_token_repo:tablename())
    end).

upsert_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql, elib_tsid], fun() ->
        Now = <<"2026-04-04T00:00:00Z">>,
        meck:expect(elib_dt, now, fun() -> Now end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        meck:expect(elib_tsid, generate, fun(push_token) -> 9001 end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 1} end),
        meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
        ?assertEqual(
            {ok, 9001},
            push_token_repo:upsert(1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token123">>)
        ),
        %% 验证先 deactivate (execute) 再 insert (query)
        ?assert(meck:num_calls(elib_pg, execute, '_') >= 1),
        ?assert(meck:num_calls(elib_pg, query, '_') >= 1)
    end).

deactivate_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql], fun() ->
        meck:expect(elib_dt, now, fun() -> <<"2026-04-04T00:00:00Z">> end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 1} end),
        ?assertEqual({ok, 1}, push_token_repo:deactivate(1, <<"did1">>))
    end).

deactivate_by_token_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql], fun() ->
        meck:expect(elib_dt, now, fun() -> <<"2026-04-04T00:00:00Z">> end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 1} end),
        ?assertEqual({ok, 1}, push_token_repo:deactivate_by_token(<<"token123">>))
    end).

deactivate_inactive_uses_rfc3339_timestamps_test() ->
    ?WITH_MECKS([elib_pg, elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        meck:expect(elib_pg, execute, fun(_Sql, [Now, Cutoff]) ->
            ?assertMatch(<<_:10/binary, "T", _/binary>>, Now),
            ?assertMatch(<<_:10/binary, "T", _/binary>>, Cutoff),
            {ok, 1}
        end),
        ?assertEqual({ok, 1}, push_token_repo:deactivate_inactive(30))
    end).

list_by_uid_test() ->
    ?WITH_MECKS([elib_pg, elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        Expected = {ok, [], [{<<"did1">>, <<"android">>, <<"fcm">>, <<"token123">>}]},
        meck:expect(elib_pg, query, fun(_Sql, [1]) -> Expected end),
        ?assertEqual(Expected, push_token_repo:list_by_uid(1))
    end).

list_by_uids_empty_test() ->
    ?assertEqual({ok, []}, push_token_repo:list_by_uids([])).

list_by_uids_test() ->
    ?WITH_MECKS([elib_pg, elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        Expected = {ok, [], [{1, <<"did1">>, <<"android">>, <<"fcm">>, <<"token1">>}]},
        meck:expect(elib_pg, query, fun(_Sql, [[1, 2]]) -> Expected end),
        ?assertEqual(Expected, push_token_repo:list_by_uids([1, 2]))
    end).

%% FULL-06：断电条件必须**含 token 维度**，且参数顺序为
%% [Now, Token, Uid, DeviceId]。只按 (user_id, device_id) 断开会漏掉
%% 「同 token 换主人/换设备」（plan-full §7 token 跨用户/设备不可复用）；
%% 语义由 test/repo/push_token_contract_pg_tests.erl 的真库 oracle 证明，
%% 本用例钉住 SQL 文本与参数形状（防止回归时静默把 token 谓词删掉）。
upsert_deactivates_by_token_and_by_user_device_test() ->
    ?WITH_MECKS([elib_pg, elib_dt, elib_pg_sql, elib_tsid], fun() ->
        Now = <<"2026-09-22T00:00:00Z">>,
        meck:expect(elib_dt, now, fun() -> Now end),
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        meck:expect(elib_tsid, generate, fun(push_token) -> 9002 end),
        meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
        meck:expect(elib_pg, execute, fun(Sql, Params) ->
            self() ! {deactivate, {iolist_to_binary(Sql), Params}},
            {ok, 1}
        end),
        ?assertEqual(
            {ok, 9002},
            push_token_repo:upsert(7, <<"did7">>, <<"android">>, <<"jpush">>, <<"rid7">>)
        ),
        {Sql, Params} = recv_captured(deactivate),
        %% token 维度在位（换主人/换设备都能断电）
        ?assertNotEqual(nomatch, binary:match(Sql, <<"token = $2">>)),
        %% 既有 (user_id, device_id) 维度保留（刷新路径不变）
        ?assertNotEqual(nomatch, binary:match(Sql, <<"user_id = $3">>)),
        ?assertNotEqual(nomatch, binary:match(Sql, <<"device_id = $4">>)),
        %% 只断活跃行（部分索引 / 历史行不动）
        ?assertNotEqual(nomatch, binary:match(Sql, <<"status = 1">>)),
        %% 参数顺序与占位符一一对应
        ?assertEqual([Now, <<"rid7">>, 7, <<"did7">>], Params)
    end).

delete_by_uid_test() ->
    ?WITH_MECKS([elib_pg, elib_pg_sql], fun() ->
        meck:expect(elib_pg_sql, public_tablename, fun(<<"push_token">>) ->
            <<"public.push_token">>
        end),
        meck:expect(elib_pg, execute, fun(_Sql, [1]) -> {ok, 3} end),
        ?assertEqual({ok, 3}, push_token_repo:delete_by_uid(1))
    end).
