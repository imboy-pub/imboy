-module(conversation_delete_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% conversation_delete_repo 模块的 EUnit 测试
%%%
%%% 目标：验证会话删除操作的语义正确性（纯 mock，不需要真实数据库）
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.conversation_delete">> end}
    ], fun() ->
        Result = conversation_delete_repo:tablename(),
        ?assertEqual(<<"public.conversation_delete">>, Result)
    end).

%% ===================================================================
%% mark_deleted/3 测试
%% ===================================================================

mark_deleted_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1714521600 end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(conversation_delete) -> 90001 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = conversation_delete_repo:mark_deleted(1001, <<"gdwqa5">>, <<"c2c">>),
        ?assertEqual({ok, 1}, Result)
    end).

mark_deleted_idempotent_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1714521600 end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(conversation_delete) -> 90002 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        %% ON CONFLICT DO NOTHING 返回 0 行受影响
        Result = conversation_delete_repo:mark_deleted(1001, <<"gdwqa5">>, <<"c2c">>),
        ?assertEqual({ok, 0}, Result)
    end).

mark_deleted_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1714521600 end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(conversation_delete) -> 90003 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {error, connection_lost} end}
        ]}
    ], fun() ->
        Result = conversation_delete_repo:mark_deleted(1001, <<"conv1">>, <<"c2c">>),
        ?assertEqual({error, connection_lost}, Result)
    end).

%% ===================================================================
%% is_deleted/3 测试
%% ===================================================================

is_deleted_true_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"count">> => 1}]}
            end}
        ]}
    ], fun() ->
        ?assertEqual(true, conversation_delete_repo:is_deleted(1002, <<"p25vd5">>, <<"c2c">>))
    end).

is_deleted_false_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"count">> => 0}]}
            end}
        ]}
    ], fun() ->
        ?assertEqual(false, conversation_delete_repo:is_deleted(1002, <<"p25vd5">>, <<"c2c">>))
    end).

is_deleted_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {error, timeout}
            end}
        ]}
    ], fun() ->
        ?assertEqual(false, conversation_delete_repo:is_deleted(1002, <<"p25vd5">>, <<"c2c">>))
    end).

%% ===================================================================
%% list/1 测试
%% ===================================================================

list_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{<<"conversation_id">> => <<"group1">>, <<"conversation_type">> => <<"c2g">>, <<"deleted_at">> => 1714521602},
                    #{<<"conversation_id">> => <<"conv2">>, <<"conversation_type">> => <<"c2c">>, <<"deleted_at">> => 1714521601},
                    #{<<"conversation_id">> => <<"conv1">>, <<"conversation_type">> => <<"c2c">>, <<"deleted_at">> => 1714521600}
                ]}
            end}
        ]}
    ], fun() ->
        {ok, List} = conversation_delete_repo:list(1003),
        ?assertEqual(3, length(List)),
        [First | _] = List,
        ?assertEqual(<<"group1">>, maps:get(<<"conversation_id">>, First))
    end).

list_empty_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ]}
    ], fun() ->
        {ok, List} = conversation_delete_repo:list(1003),
        ?assertEqual(0, length(List))
    end).

list_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {error, connection_lost}
            end}
        ]}
    ], fun() ->
        Result = conversation_delete_repo:list(1003),
        ?assertEqual({error, connection_lost}, Result)
    end).

%% ===================================================================
%% restore/3 测试
%% ===================================================================

restore_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = conversation_delete_repo:restore(1004, <<"conv1">>, <<"c2c">>),
        ?assertEqual({ok, 1}, Result)
    end).

restore_not_found_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        Result = conversation_delete_repo:restore(1004, <<"nonexistent">>, <<"c2c">>),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% delete_by_user/1 测试
%% ===================================================================

delete_by_user_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 5} end}
        ]}
    ], fun() ->
        Result = conversation_delete_repo:delete_by_user(1001),
        ?assertEqual({ok, 5}, Result)
    end).

delete_by_user_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tb) -> <<"public.", Tb/binary>> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {error, timeout} end}
        ]}
    ], fun() ->
        Result = conversation_delete_repo:delete_by_user(1001),
        ?assertEqual({error, timeout}, Result)
    end).
