-module(compliance_key_repo_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try Fun()
        after meck:unload(Modules)
        end
    end)()
).

tablename_test() ->
    ?assert(is_binary(compliance_key_repo:tablename())).

create_ok_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        meck:expect(elib_pg, insert, fun(_Tb, _Data, _Opts) -> {ok, 1} end),
        ?assertEqual(
            {ok, <<"key-001">>},
            compliance_key_repo:create(<<"key-001">>, <<"-----BEGIN PUBLIC KEY-----">>, <<"encrypted">>, 1)
        )
    end).

create_error_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        meck:expect(elib_pg, insert, fun(_Tb, _Data, _Opts) -> {error, unique_violation} end),
        ?assertEqual(
            {error, unique_violation},
            compliance_key_repo:create(<<"key-dup">>, <<"pk">>, <<"enc">>, 1)
        )
    end).

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

find_by_key_id_ok_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        Row = #{<<"key_id">> => <<"key-001">>, <<"status">> => 1},
        meck:expect(elib_pg, query, fun(_Sql, [<<"key-001">>]) -> {ok, [Row]} end),
        ?assertEqual({ok, Row}, compliance_key_repo:find_by_key_id(<<"key-001">>))
    end).

find_by_key_id_not_found_test() ->
    ?WITH_MECKS([elib_pg], fun() ->
        meck:expect(elib_pg, query, fun(_Sql, [_]) -> {ok, []} end),
        ?assertEqual({error, not_found}, compliance_key_repo:find_by_key_id(<<"nonexist">>))
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
