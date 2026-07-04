-module(adm_user_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_user_repo 模块的 EUnit 测试
%%%
%%% 目标：验证管理员用户数据仓库操作
%%% 覆盖：表名获取、查询操作、保存、更新、删除
%%%
%%% 规则：不 mock elib_pg_sql；elib_pg 和 elib_tsid 使用 [no_link]
%%%===================================================================

%% ===================================================================
%% Helper: meck setup/cleanup for elib_pg (no_link, no passthrough)
%% ===================================================================

setup_elib_pg_mocks(Expectations) ->
    catch meck:unload(elib_pg),
    meck:new(elib_pg, [no_link]),
    lists:foreach(
        fun({Func, Arity, Fun}) ->
            meck:expect(elib_pg, Func, Arity, Fun)
        end,
        Expectations
    ).

setup_elib_pg_tsid_mocks(PgExpectations, TsidExpectations) ->
    catch meck:unload(elib_pg),
    catch meck:unload(elib_tsid),
    meck:new(elib_pg, [no_link]),
    meck:new(elib_tsid, [no_link]),
    lists:foreach(
        fun({Func, Arity, Fun}) ->
            meck:expect(elib_pg, Func, Arity, Fun)
        end,
        PgExpectations
    ),
    lists:foreach(
        fun({Func, Arity, Fun}) ->
            meck:expect(elib_tsid, Func, Arity, Fun)
        end,
        TsidExpectations
    ).

cleanup_elib_pg_mocks() ->
    catch meck:unload(elib_pg),
    ok.

cleanup_elib_pg_tsid_mocks() ->
    catch meck:unload(elib_pg),
    catch meck:unload(elib_tsid),
    ok.

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_name_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = adm_user_repo:tablename(),
        ?assertEqual(<<"public.adm_user">>, Result)
    end).

%% ===================================================================
%% count_by_role_id/1 测试
%% ===================================================================

count_by_role_id_success_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'query', 2, fun(_Sql, [_RoleId]) ->
                    {ok, [#{<<"count">> => 5}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:count_by_role_id(1),
                ?assertEqual({ok, 5}, Result)
            end)
        end}.

count_by_role_id_zero_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'query', 2, fun(_Sql, [_RoleId]) ->
                    {ok, [#{<<"count">> => 0}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:count_by_role_id(999),
                ?assertEqual({ok, 0}, Result)
            end)
        end}.

count_by_role_id_error_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'query', 2, fun(_Sql, [_RoleId]) ->
                    {error, connection_failed}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:count_by_role_id(1),
                ?assertEqual({error, connection_failed}, Result)
            end)
        end}.

%% ===================================================================
%% find_by_email/2 测试
%% ===================================================================

find_by_email_success_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Email]) ->
                    {ok, #{<<"id">> => 1, <<"account">> => <<"admin">>}}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_email(<<"admin@example.com">>, <<"id,account">>),
                ?assertMatch(#{<<"id">> := 1}, Result)
            end)
        end}.

find_by_email_not_found_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Email]) ->
                    {ok, not_found}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_email(<<"notfound@example.com">>, <<"id">>),
                ?assertEqual(#{}, Result)
            end)
        end}.

%% ===================================================================
%% find_by_mobile/2 测试
%% ===================================================================

find_by_mobile_binary_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Mobile]) ->
                    {ok, #{<<"id">> => 2, <<"mobile">> => <<"13692177080">>}}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_mobile(<<"13692177080">>, <<"id,mobile">>),
                ?assertMatch(#{<<"id">> := 2}, Result)
            end)
        end}.

find_by_mobile_string_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Mobile]) ->
                    {ok, #{<<"id">> => 3}}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_mobile("13800138000", <<"id">>),
                ?assertMatch(#{<<"id">> := 3}, Result)
            end)
        end}.

%% ===================================================================
%% find_by_account/2 测试
%% ===================================================================

find_by_account_success_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Account]) ->
                    {ok, #{<<"id">> => 1, <<"account">> => <<"admin">>}}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_account(<<"admin">>, <<"id,account">>),
                ?assertMatch(#{<<"id">> := 1}, Result)
            end)
        end}.

find_by_account_string_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Account]) ->
                    {ok, #{<<"id">> => 5}}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_account("testuser", <<"id">>),
                ?assertMatch(#{<<"id">> := 5}, Result)
            end)
        end}.

%% ===================================================================
%% find_by_id/1 测试 - 使用默认列
%% ===================================================================

find_by_id_default_column_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Id]) ->
                    {ok, #{<<"id">> => 1, <<"account">> => <<"admin">>, <<"avatar">> => <<>>}}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_id(1),
                ?assertMatch(#{<<"id">> := 1}, Result)
            end)
        end}.

%% ===================================================================
%% find_by_id/2 测试 - 指定列
%% ===================================================================

find_by_id_with_custom_column_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Id]) ->
                    {ok, #{<<"id">> => 100, <<"nickname">> => <<"管理员"/utf8>>}}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_id(100, <<"id,nickname">>),
                ?assertMatch(#{<<"id">> := 100}, Result)
            end)
        end}.

find_by_id_with_asterisk_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Id]) ->
                    {ok, #{<<"id">> => 200, <<"account">> => <<"superadmin">>}}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_id(200, <<"*">>),
                ?assertMatch(#{<<"id">> := 200}, Result)
            end)
        end}.

find_by_id_not_found_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'one', 2, fun(_Sql, [_Id]) -> {ok, not_found} end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:find_by_id(999, <<"id">>),
                ?assertEqual(#{}, Result)
            end)
        end}.

%% ===================================================================
%% list_by_ids/2 测试
%% ===================================================================

list_by_ids_success_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'query', 2, fun(_Sql, [1, 2, 3]) ->
                    {ok, [
                        #{<<"id">> => 1, <<"account">> => <<"admin1">>},
                        #{<<"id">> => 2, <<"account">> => <<"admin2">>},
                        #{<<"id">> => 3, <<"account">> => <<"admin3">>}
                    ]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:list_by_ids([1, 2, 3], <<"id,account">>),
                ?assertMatch({ok, [_, _, _]}, Result)
            end)
        end}.

list_by_ids_empty_test_() ->
    ?_test(begin
        Result = adm_user_repo:list_by_ids([], <<"id">>),
        ?assertEqual({ok, []}, Result)
    end).

list_by_ids_binary_ids_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'query', 2, fun(_Sql, [<<"1">>, <<"2">>]) ->
                    {ok, [#{<<"id">> => 1}, #{<<"id">> => 2}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:list_by_ids([<<"1">>, <<"2">>], <<"id">>),
                ?assertMatch({ok, [_, _]}, Result)
            end)
        end}.

%% ===================================================================
%% select_by_where/4 测试 - binary Where
%% ===================================================================

select_by_where_binary_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'page', 6, fun(_Tb, _Col, _Where, _Order, _Page, _Limit) ->
                    {ok, [#{<<"id">> => 1}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:select_by_where(<<"status = 1">>, 10, 0, <<"id DESC">>),
                ?assertMatch({ok, [_]}, Result)
            end)
        end}.

select_by_where_binary_zero_limit_test_() ->
    ?_test(begin
        Result = adm_user_repo:select_by_where(<<"status = 1">>, 0, 0, <<"id DESC">>),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% select_by_where/4 测试 - map Where
%% ===================================================================

select_by_where_map_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'page', 6, fun(_Tb, _Col, _Where, _Order, _Page, _Limit) ->
                    {ok, [#{<<"id">> => 2}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:select_by_where(#{<<"status">> => 1}, 10, 0, <<"id ASC">>),
                ?assertMatch({ok, [_]}, Result)
            end)
        end}.

select_by_where_map_zero_limit_test_() ->
    ?_test(begin
        Result = adm_user_repo:select_by_where(#{<<"status">> => 1}, 0, 0, <<"id ASC">>),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% select_by_where/5 测试 - binary Where
%% ===================================================================

select_by_where_5_binary_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'page', 6, fun(_Tb, _Col, _Where, _Order, _Page, _Limit) ->
                    {ok, [#{<<"id">> => 3}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:select_by_where(
                    <<"id,account">>, <<"role_id = 1">>, 10, 0, <<"id DESC">>
                ),
                ?assertMatch({ok, [_]}, Result)
            end)
        end}.

%% ===================================================================
%% select_by_where/5 测试 - map Where
%% ===================================================================

select_by_where_5_map_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'page', 6, fun(_Tb, _Col, _Where, _Order, _Page, _Limit) ->
                    {ok, [#{<<"id">> => 4}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:select_by_where(
                    <<"id">>, #{<<"status">> => 1}, 10, 0, <<"id ASC">>
                ),
                ?assertMatch({ok, [_]}, Result)
            end)
        end}.

%% ===================================================================
%% select_by_where_safe/6 测试
%% ===================================================================

select_by_where_safe_success_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [#{<<"id">> => 1}, #{<<"id">> => 2}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:select_by_where_safe(
                    <<"id">>,
                    #{<<"status">> => 1},
                    10,
                    0,
                    [{<<"id">>, desc}],
                    [<<"id">>, <<"account">>]
                ),
                ?assertMatch({ok, [_, _]}, Result)
            end)
        end}.

select_by_where_safe_zero_limit_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = adm_user_repo:select_by_where_safe(
            <<"id">>,
            #{<<"status">> => 1},
            0,
            0,
            [{<<"id">>, desc}],
            [<<"id">>]
        ),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% save/1 测试
%% ===================================================================

save_success_test_() ->
    {setup,
        fun() ->
            catch meck:unload(elib_pg),
            catch meck:unload(elib_tsid),
            meck:new(elib_pg, [no_link]),
            meck:new(elib_tsid, [no_link]),
            meck:expect(elib_pg, query, 2, fun(_Sql, _Params) -> {ok, 1} end),
            meck:expect(elib_tsid, generate, 1, fun(adm_user) -> 12345 end),
            ok
        end,
        fun(_) -> cleanup_elib_pg_tsid_mocks() end, fun(_) ->
            ?_test(begin
                Data = #{
                    account => <<"test_admin"/utf8>>,
                    password => <<"hashed_password">>,
                    mobile => <<"13800138000">>,
                    role_id => 1,
                    status => 1
                },
                Result = adm_user_repo:save(Data),
                ?assertEqual({ok, 12345}, Result)
            end)
        end}.

save_error_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_tsid_mocks(
                [{'query', 2, fun(_Sql, _Params) -> {error, duplicate_key} end}],
                [{'generate', 1, fun(adm_user) -> 12346 end}]
            )
        end,
        fun(_) -> cleanup_elib_pg_tsid_mocks() end, fun(_) ->
            ?_test(begin
                Data = #{account => <<"admin">>, password => <<"pass">>},
                Result = adm_user_repo:save(Data),
                ?assertEqual({error, duplicate_key}, Result)
            end)
        end}.

%% ===================================================================
%% update/2 测试
%% ===================================================================

update_success_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 1} end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Data = #{nickname => <<"新昵称"/utf8>>},
                Result = adm_user_repo:update(1, Data),
                ?assertEqual({ok, 1}, Result)
            end)
        end}.

update_not_found_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 0} end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Data = #{nickname => <<"新昵称"/utf8>>},
                Result = adm_user_repo:update(999, Data),
                ?assertEqual({ok, 0}, Result)
            end)
        end}.

%% ===================================================================
%% delete/1 测试
%% ===================================================================

delete_success_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'update', 4, fun(_Tb, Data, _Where, _Params) ->
                    ?assertEqual(-1, maps:get(status, Data)),
                    {ok, 1}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:delete(1),
                ?assertEqual({ok, 1}, Result)
            end)
        end}.

delete_not_found_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 0} end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:delete(999),
                ?assertEqual({ok, 0}, Result)
            end)
        end}.

%% ===================================================================
%% count_by_role/1 测试（bigint[] 数组列须用 ANY 匹配）
%% ===================================================================

count_by_role_uses_any_operator_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'query', 2, fun(Sql, [7]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"$1 = ANY(role_id)">>)),
                    {ok, [#{<<"count">> => 3}]}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:count_by_role(7),
                ?assertEqual({ok, 3}, Result)
            end)
        end}.

count_by_role_error_test_() ->
    {setup,
        fun() ->
            setup_elib_pg_mocks([
                {'query', 2, fun(_Sql, [_RoleId]) ->
                    {error, connection_failed}
                end}
            ])
        end,
        fun(_) -> cleanup_elib_pg_mocks() end, fun(_) ->
            ?_test(begin
                Result = adm_user_repo:count_by_role(1),
                ?assertEqual({error, connection_failed}, Result)
            end)
        end}.
