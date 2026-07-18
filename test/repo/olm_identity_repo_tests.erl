-module(olm_identity_repo_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 表名
%% ===================================================================

tablename_identity_test() ->
    ?assert(is_binary(olm_identity_repo:tablename_identity())).

tablename_one_time_key_test() ->
    ?assert(is_binary(olm_identity_repo:tablename_one_time_key())).

tablename_fallback_key_test() ->
    ?assert(is_binary(olm_identity_repo:tablename_fallback_key())).

%% ===================================================================
%% upsert_identity：构造 SQL 应含 ON CONFLICT 子句（幂等更新）
%% ===================================================================

upsert_identity_builds_upsert_sql_test() ->
    _ = catch meck:unload([elib_pg, elib_tsid]),
    ok = meck:new(elib_tsid, [no_link]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_tsid, generate, 1, fun(olm_identity) -> 7001 end),
        Captured = atomics:new(1, [{signed, true}]),
        meck:expect(elib_pg, query, 2, fun(Sql, _Params) ->
            SqlStr = binary_to_list(iolist_to_binary(Sql)),
            HasUpsert = string:str(SqlStr, "ON CONFLICT (user_id, device_id) DO UPDATE") > 0,
            atomics:put(
                Captured,
                1,
                case HasUpsert of
                    true -> 1;
                    false -> 0
                end
            ),
            {ok, 1}
        end),
        {ok, _} = olm_identity_repo:upsert_identity(
            100, <<"dev-A">>, <<"ed25519">>, <<"curve25519">>, <<"sig">>, <<"ios">>
        ),
        ?assertEqual(1, atomics:get(Captured, 1))
    after
        meck:unload([elib_pg, elib_tsid])
    end.

%% ===================================================================
%% claim_one_time_key：耗尽时返回 {error, exhausted}
%% ===================================================================

claim_one_time_key_exhausted_test() ->
    _ = catch meck:unload([elib_pg]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_pg, query, 2, fun(_Sql, _Params) -> {ok, []} end),
        ?assertEqual(
            {error, exhausted}, olm_identity_repo:claim_one_time_key(100, <<"dev-A">>, 200)
        )
    after
        meck:unload([elib_pg])
    end.

%% claim_one_time_key：命中时返回 {ok, Row}，且 claimed_by 作为 $3 写入审计列
claim_one_time_key_hit_test() ->
    _ = catch meck:unload([elib_pg]),
    ok = meck:new(elib_pg, [no_link]),
    try
        Row = #{<<"key_id">> => <<"otk-1">>, <<"key_base64">> => <<"AAAA">>},
        meck:expect(elib_pg, query, 2, fun(Sql, Params) ->
            SqlStr = binary_to_list(iolist_to_binary(Sql)),
            ?assert(string:str(SqlStr, "claimed_by = $3") > 0),
            %% 参数顺序：[OwnerUid, DeviceId, ClaimedBy]
            ?assertEqual([100, <<"dev-A">>, 200], Params),
            {ok, [Row]}
        end),
        ?assertEqual({ok, Row}, olm_identity_repo:claim_one_time_key(100, <<"dev-A">>, 200))
    after
        meck:unload([elib_pg])
    end.

%% ===================================================================
%% upsert_one_time_keys：先删后插，返回插入条数
%% ===================================================================

upsert_one_time_keys_returns_count_test() ->
    _ = catch meck:unload([elib_pg, elib_tsid]),
    ok = meck:new(elib_tsid, [no_link]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_tsid, generate, 1, fun(olm_one_time_key) -> 8001 end),
        %% DELETE 与每条 INSERT 都成功
        meck:expect(elib_pg, execute, 2, fun(_Sql, _Params) -> {ok, 1} end),
        Keys = [{<<"k1">>, <<"v1">>}, {<<"k2">>, <<"v2">>}],
        ?assertEqual({ok, 2}, olm_identity_repo:upsert_one_time_keys(100, <<"dev-A">>, Keys, 100))
    after
        meck:unload([elib_pg, elib_tsid])
    end.

%% ===================================================================
%% count_one_time_keys：解析 COUNT(*) AS n
%% ===================================================================

count_one_time_keys_test() ->
    _ = catch meck:unload([elib_pg]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_pg, query, 2, fun(_Sql, _Params) -> {ok, [#{<<"n">> => 42}]} end),
        ?assertEqual({ok, 42}, olm_identity_repo:count_one_time_keys(100, <<"dev-A">>))
    after
        meck:unload([elib_pg])
    end.

%% ===================================================================
%% upsert_fallback_key / claim_fallback_key
%% ===================================================================

upsert_fallback_key_uses_upsert_test() ->
    _ = catch meck:unload([elib_pg, elib_tsid]),
    ok = meck:new(elib_tsid, [no_link]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_tsid, generate, 1, fun(olm_fallback_key) -> 9001 end),
        Captured = atomics:new(1, [{signed, true}]),
        meck:expect(elib_pg, query, 2, fun(Sql, _Params) ->
            SqlStr = binary_to_list(iolist_to_binary(Sql)),
            HasUpsert = string:str(SqlStr, "ON CONFLICT (user_id, device_id) DO UPDATE") > 0,
            atomics:put(
                Captured,
                1,
                case HasUpsert of
                    true -> 1;
                    false -> 0
                end
            ),
            {ok, 1}
        end),
        {ok, _} = olm_identity_repo:upsert_fallback_key(100, <<"dev-A">>, <<"fb-1">>, <<"BBBB">>),
        ?assertEqual(1, atomics:get(Captured, 1))
    after
        meck:unload([elib_pg, elib_tsid])
    end.

claim_fallback_key_exhausted_test() ->
    _ = catch meck:unload([elib_pg]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_pg, query, 2, fun(_Sql, _Params) -> {ok, []} end),
        ?assertEqual({error, exhausted}, olm_identity_repo:claim_fallback_key(100, <<"dev-A">>))
    after
        meck:unload([elib_pg])
    end.
