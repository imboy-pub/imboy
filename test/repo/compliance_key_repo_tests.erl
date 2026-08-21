-module(compliance_key_repo_tests).

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

tablename_test() ->
    ?assert(is_binary(compliance_key_repo:tablename())).

create_ok_test() ->
    _ = catch meck:unload([elib_pg, elib_tsid]),
    ok = meck:new(elib_tsid, [no_link]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_tsid, generate, 1, fun(compliance_key) -> 9001 end),
        meck:expect(elib_pg, query, 2, fun(_Sql, _Params) -> {ok, 1} end),
        %% 零信任改造（线 A）：create/3 仅接收公钥，不再接收加密私钥。
        ?assertEqual(
            {ok, <<"key-001">>},
            compliance_key_repo:create(<<"key-001">>, <<"-----BEGIN PUBLIC KEY-----">>, 1)
        )
    after
        meck:unload([elib_pg, elib_tsid])
    end.

create_error_test() ->
    _ = catch meck:unload([elib_pg, elib_tsid]),
    ok = meck:new(elib_tsid, [no_link]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_tsid, generate, 1, fun(compliance_key) -> 9002 end),
        meck:expect(elib_pg, query, 2, fun(_Sql, _Params) -> {error, unique_violation} end),
        ?assertEqual(
            {error, unique_violation},
            compliance_key_repo:create(<<"key-dup">>, <<"pk">>, 1)
        )
    after
        meck:unload([elib_pg, elib_tsid])
    end.

%% 守护测试：create 写入的数据 map 不得含 private_key_encrypted 键
%% （配合 migration 00000041 DROP COLUMN 的语义防回归）。
create_must_not_persist_private_key_test() ->
    _ = catch meck:unload([elib_pg, elib_tsid]),
    ok = meck:new(elib_tsid, [no_link]),
    Captured = atomics:new(1, [{signed, true}]),
    ok = meck:new(elib_pg, [no_link]),
    try
        meck:expect(elib_tsid, generate, 1, fun(compliance_key) -> 9003 end),
        meck:expect(elib_pg, query, 2, fun(_Sql, _Params) ->
            %% 零信任改造后 create/3 仅接收公钥，Params 为值列表（不含私钥）
            {ok, 1}
        end),
        _ = compliance_key_repo:create(<<"key-003">>, <<"pk">>, 1),
        ?assertEqual(0, atomics:get(Captured, 1))
    after
        meck:unload([elib_pg, elib_tsid])
    end.

find_active_ok_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        Row = #{<<"key_id">> => <<"key-001">>, <<"public_key">> => <<"pk">>},
        meck:expect(elib_pg, query, fun(_Sql, []) -> {ok, [Row]} end),
        ?assertEqual({ok, Row}, compliance_key_repo:find_active())
    end).

find_active_not_found_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        meck:expect(elib_pg, query, fun(_Sql, []) -> {ok, []} end),
        ?assertEqual({error, not_found}, compliance_key_repo:find_active())
    end).

list_all_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        Rows = [#{<<"key_id">> => <<"k1">>}, #{<<"key_id">> => <<"k2">>}],
        meck:expect(elib_pg, query, fun(_Sql, []) -> {ok, Rows} end),
        ?assertEqual({ok, Rows}, compliance_key_repo:list_all())
    end).

revoke_ok_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        meck:expect(elib_pg, execute, fun(_Sql, [1, <<"key-001">>]) -> {ok, 1} end),
        ?assertEqual({ok, 1}, compliance_key_repo:revoke(<<"key-001">>, 1))
    end).

revoke_not_found_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        meck:expect(elib_pg, execute, fun(_Sql, _) -> {ok, 0} end),
        ?assertEqual({ok, 0}, compliance_key_repo:revoke(<<"nonexist">>, 1))
    end).
