-module(app_version_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_version_repo 模块的 EUnit 测试
%%%
%%% 目标：验证应用版本数据仓库层的语义正确性（纯 mock，不需要真实数据库）
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.app_version">> end}
    ], fun() ->
        Result = app_version_repo:tablename(),
        ?assertEqual(<<"public.app_version">>, Result)
    end).

%% ===================================================================
%% find/2 测试
%% ===================================================================

find_by_type_with_region_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"region_code">> => <<"cn">>,
                   <<"type">> => <<"ios">>,
                   <<"vsn">> => <<"1.0.0">>}}
        end}
    ], fun() ->
        Result = app_version_repo:find(<<"ios">>, <<"cn">>),
        ?assertMatch(#{<<"type">> := <<"ios">>, <<"vsn">> := <<"1.0.0">>}, Result)
    end).

find_by_type_without_region_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"region_code">> => <<>>,
                   <<"type">> => <<"android">>,
                   <<"vsn">> => <<"2.0.0">>}}
        end}
    ], fun() ->
        Result = app_version_repo:find(<<"android">>, <<>>),
        ?assertMatch(#{<<"type">> := <<"android">>}, Result)
    end).

find_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
    ], fun() ->
        Result = app_version_repo:find(<<"nonexistent">>, <<"xx">>),
        ?assertEqual(#{}, Result)
    end).

find_database_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {error, connection_lost} end}
    ], fun() ->
        Result = app_version_repo:find(<<"ios">>, <<"cn">>),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% add/1 测试
%% ===================================================================

add_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end},
            {'insert', 2, fun(_Tb, _Data) -> {<<"INSERT INTO test VALUES ($1)">>, [42]} end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(app_version) -> 900001 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Data = #{
            <<"region_code">> => <<"cn">>,
            <<"type">> => <<"ios">>,
            <<"package_name">> => <<"com.test.app">>,
            <<"app_name">> => <<"TestApp">>,
            <<"vsn">> => <<"1.0.0">>,
            <<"download_url">> => <<"https://test.com/download">>,
            <<"description">> => <<"Test app">>,
            <<"force_update">> => 1,
            <<"created_at">> => 1714521600,
            <<"sign_key">> => <<"test_key">>
        },
        Result = app_version_repo:add(Data),
        ?assertEqual({ok, 900001}, Result)
    end).

add_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end},
            {'insert', 2, fun(_Tb, _Data) -> {<<"INSERT INTO test VALUES ($1)">>, [42]} end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(app_version) -> 900002 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {error, unique_violation} end}
        ]}
    ], fun() ->
        Data = #{
            <<"type">> => <<"ios">>,
            <<"vsn">> => <<"1.0.0">>,
            <<"created_at">> => 1714521600
        },
        Result = app_version_repo:add(Data),
        ?assertEqual({error, unique_violation}, Result)
    end).

%% ===================================================================
%% delete_by_id/1 测试
%% ===================================================================

delete_by_id_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = app_version_repo:delete_by_id(12345),
        ?assertEqual({ok, 1}, Result)
    end).

delete_by_id_not_found_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        Result = app_version_repo:delete_by_id(99999),
        ?assertEqual({ok, 0}, Result)
    end).

delete_by_id_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {error, connection_lost} end}
        ]}
    ], fun() ->
        Result = app_version_repo:delete_by_id(12345),
        ?assertEqual({error, connection_lost}, Result)
    end).
