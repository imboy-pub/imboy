-module(verification_code_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% verification_code_repo 模块的 EUnit 测试
%%%
%%% 目标：验证验证码数据访问层功能
%%% 覆盖：验证码查询、保存（upsert）
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.verification_code">> end}
    ], fun() ->
        Result = verification_code_repo:tablename(),
        ?assertEqual(<<"public.verification_code">>, Result)
    end).

%% ===================================================================
%% find_by_id/1 测试
%% ===================================================================

find_by_id_existing_code_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"id">> => <<"test@example.com">>,
                   <<"code">> => <<"123456">>,
                   <<"validity_at">> => 1640995200,
                   <<"created_at">> => 1640991600}}
        end}
    ], fun() ->
        Id = <<"test@example.com">>,
        Result = verification_code_repo:find_by_id(Id),
        ?assertMatch(#{<<"id">> := _, <<"code">> := _, <<"validity_at">> := _, <<"created_at">> := _}, Result),
        ?assertEqual(<<"test@example.com">>, maps:get(<<"id">>, Result)),
        ?assertEqual(<<"123456">>, maps:get(<<"code">>, Result))
    end).

find_by_id_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
    ], fun() ->
        Id = <<"nonexistent@example.com">>,
        Result = verification_code_repo:find_by_id(Id),
        ?assertEqual(undefined, Result)
    end).

find_by_id_database_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) -> {error, connection_failed} end}
    ], fun() ->
        Id = <<"test@example.com">>,
        Result = verification_code_repo:find_by_id(Id),
        ?assertEqual(undefined, Result)
    end).

%% ===================================================================
%% save/4 测试
%% ===================================================================

save_new_code_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        ToEmail = <<"test@example.com">>,
        VerifyCode = <<"123456">>,
        ValidityAt = 1640995200,
        Now = <<"2021-12-31 16:00:00">>,

        Result = verification_code_repo:save(ToEmail, VerifyCode, ValidityAt, Now),
        ?assertEqual({ok, 1}, Result)
    end).

save_update_existing_code_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        % 使用相同的邮箱更新验证码（测试 ON CONFLICT UPDATE 分支）
        ToEmail = <<"test@example.com">>,
        VerifyCode = <<"654321">>,
        ValidityAt = 1640999000,
        Now = <<"2021-12-31 17:00:00">>,

        Result = verification_code_repo:save(ToEmail, VerifyCode, ValidityAt, Now),
        ?assertEqual({ok, 1}, Result)
    end).

save_with_different_params_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        % 测试不同的邮箱和验证码
        ToEmail = <<"another@example.com">>,
        VerifyCode = <<"999999">>,
        ValidityAt = 1641000000,
        Now = <<"2021-12-31 18:00:00">>,

        Result = verification_code_repo:save(ToEmail, VerifyCode, ValidityAt, Now),
        ?assertEqual({ok, 1}, Result)
    end).

save_database_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {error, database_constraint} end}
    ], fun() ->
        ToEmail = <<"test@example.com">>,
        VerifyCode = <<"123456">>,
        ValidityAt = 1640995200,
        Now = <<"2021-12-31 16:00:00">>,

        Result = verification_code_repo:save(ToEmail, VerifyCode, ValidityAt, Now),
        ?assertEqual({error, database_constraint}, Result)
    end).

%% ===================================================================
%% 集成测试
%% ===================================================================

save_and_find_code_flow_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end},
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"id">> => <<"test@example.com">>,
                   <<"code">> => <<"123456">>,
                   <<"validity_at">> => 1640995200,
                   <<"created_at">> => 1640991600}}
        end}
    ], fun() ->
        % 1. 保存验证码
        ToEmail = <<"test@example.com">>,
        VerifyCode = <<"123456">>,
        ValidityAt = 1640995200,
        Now = <<"2021-12-31 16:00:00">>,

        ?assertEqual({ok, 1}, verification_code_repo:save(ToEmail, VerifyCode, ValidityAt, Now)),

        % 2. 查询验证码
        Result = verification_code_repo:find_by_id(ToEmail),
        ?assertMatch(#{<<"code">> := <<"123456">>}, Result),
        ?assertEqual(<<"123456">>, maps:get(<<"code">>, Result))
    end).

save_update_and_find_code_flow_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end},
        {'one', 2, fun(_Sql, _Params) ->
            % 返回更新后的验证码
            {ok, #{<<"id">> => <<"test@example.com">>,
                   <<"code">> => <<"654321">>,
                   <<"validity_at">> => 1640999000,
                   <<"created_at">> => 1640995400}}
        end}
    ], fun() ->
        ToEmail = <<"test@example.com">>,

        % 1. 第一次保存
        ?assertEqual({ok, 1}, verification_code_repo:save(ToEmail, <<"123456">>, 1640995200, <<"2021-12-31 16:00:00">>)),

        % 2. 更新验证码（测试 UPSERT）
        ?assertEqual({ok, 1}, verification_code_repo:save(ToEmail, <<"654321">>, 1640999000, <<"2021-12-31 17:00:00">>)),

        % 3. 查询验证码
        Result = verification_code_repo:find_by_id(ToEmail),
        ?assertMatch(#{<<"code">> := <<"654321">>}, Result)
    end).
