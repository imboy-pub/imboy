-module(adm_user_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_user_repo 模块的 EUnit 测试
%%%
%%% 目标：验证管理员用户数据仓库操作
%%% 覆盖：表名获取、查询操作、保存、更新、删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_name_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(<<"adm_user">>) ->
            <<"public.adm_user">>
        end}
    ], fun() ->
        Result = adm_user_repo:tablename(),
        ?assertEqual(<<"public.adm_user">>, Result)
    end).

%% ===================================================================
%% count_by_role_id/1 测试
%% ===================================================================

count_by_role_id_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [_RoleId]) ->
                {ok, [#{<<"count">> => 5}]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:count_by_role_id(1),
        ?assertEqual({ok, 5}, Result)
    end).

count_by_role_id_zero_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [_RoleId]) ->
                {ok, [#{<<"count">> => 0}]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:count_by_role_id(999),
        ?assertEqual({ok, 0}, Result)
    end).

count_by_role_id_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [_RoleId]) ->
                {error, connection_failed}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:count_by_role_id(1),
        ?assertEqual({error, connection_failed}, Result)
    end).

%% ===================================================================
%% find_by_email/2 测试
%% ===================================================================

find_by_email_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Email]) ->
                {ok, #{<<"id">> => 1, <<"account">> => <<"admin">>}}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, User}) -> User end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_email(<<"admin@example.com">>, <<"id,account">>),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

find_by_email_not_found_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Email]) ->
                {ok, not_found}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, _}) -> #{} end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_email(<<"notfound@example.com">>, <<"id">>),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% find_by_mobile/2 测试
%% ===================================================================

find_by_mobile_binary_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Mobile]) ->
                {ok, #{<<"id">> => 2, <<"mobile">> => <<"13692177080">>}}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, User}) -> User end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_mobile(<<"13692177080">>, <<"id,mobile">>),
        ?assertMatch(#{<<"id">> := 2}, Result)
    end).

find_by_mobile_string_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Mobile]) ->
                {ok, #{<<"id">> => 3}}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, User}) -> User end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_mobile("13800138000", <<"id">>),
        ?assertMatch(#{<<"id">> := 3}, Result)
    end).

%% ===================================================================
%% find_by_account/2 测试
%% ===================================================================

find_by_account_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Account]) ->
                {ok, #{<<"id">> => 1, <<"account">> => <<"admin">>}}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, User}) -> User end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_account(<<"admin">>, <<"id,account">>),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

find_by_account_string_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Account]) ->
                {ok, #{<<"id">> => 5}}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, User}) -> User end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_account("testuser", <<"id">>),
        ?assertMatch(#{<<"id">> := 5}, Result)
    end).

%% ===================================================================
%% find_by_id/1 测试 - 使用默认列
%% ===================================================================

find_by_id_default_column_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Id]) ->
                {ok, #{<<"id">> => 1, <<"account">> => <<"admin">>, <<"avatar">> => <<>>}}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, User}) -> User end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_id(1),
        ?assertMatch(#{<<"id">> := 1}, Result)
    end).

%% ===================================================================
%% find_by_id/2 测试 - 指定列
%% ===================================================================

find_by_id_with_custom_column_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Id]) ->
                {ok, #{<<"id">> => 100, <<"nickname">> => <<"管理员"/utf8>>}}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, User}) -> User end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_id(100, <<"id,nickname">>),
        ?assertMatch(#{<<"id">> := 100}, Result)
    end).

find_by_id_with_asterisk_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Id]) ->
                {ok, #{<<"id">> => 200, <<"account">> => <<"superadmin">>}}
            end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, User}) -> User end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_id(200, <<"*">>),
        ?assertMatch(#{<<"id">> := 200}, Result)
    end).

find_by_id_not_found_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [_Id]) -> {ok, not_found} end}
        ]},
        {elib_pg_sql, [
            {'value_or_empty', 1, fun({ok, _}) -> #{} end}
        ]}
    ], fun() ->
        Result = adm_user_repo:find_by_id(999, <<"id">>),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% list_by_ids/2 测试
%% ===================================================================

list_by_ids_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [1, 2, 3]) ->
                {ok, [[
                    #{<<"id">> => 1, <<"account">> => <<"admin1">>},
                    #{<<"id">> => 2, <<"account">> => <<"admin2">>},
                    #{<<"id">> => 3, <<"account">> => <<"admin3">>}
                ]]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:list_by_ids([1, 2, 3], <<"id,account">>),
        ?assertMatch({ok, [_, _, _]}, Result)
    end).

list_by_ids_empty_test_() ->
    Result = adm_user_repo:list_by_ids([], <<"id">>),
    ?assertEqual({ok, []}, Result).

list_by_ids_binary_ids_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [<<"1">>, <<"2">>]) ->
                {ok, [[#{<<"id">> => 1}, #{<<"id">> => 2}]]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:list_by_ids([<<"1">>, <<"2">>], <<"id">>),
        ?assertMatch({ok, [_, _]}, Result)
    end).

%% ===================================================================
%% select_by_where/4 测试 - binary Where
%% ===================================================================

select_by_where_binary_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'page', 7, fun(_Tb, _Col, _Where, _Order, _Page, _Limit) ->
                {ok, [#{<<"id">> => 1}]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:select_by_where(<<"status = 1">>, 10, 0, <<"id DESC">>),
        ?assertMatch({ok, [_]}, Result)
    end).

select_by_where_binary_zero_limit_test_() ->
    Result = adm_user_repo:select_by_where(<<"status = 1">>, 0, 0, <<"id DESC">>),
    ?assertEqual({ok, []}, Result).

%% ===================================================================
%% select_by_where/4 测试 - map Where
%% ===================================================================

select_by_where_map_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'page', 7, fun(_Tb, _Col, _Where, _Order, _Page, _Limit) ->
                {ok, [#{<<"id">> => 2}]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:select_by_where(#{<<"status">> => 1}, 10, 0, <<"id ASC">>),
        ?assertMatch({ok, [_]}, Result)
    end).

select_by_where_map_zero_limit_test_() ->
    Result = adm_user_repo:select_by_where(#{<<"status">> => 1}, 0, 0, <<"id ASC">>),
    ?assertEqual({ok, []}, Result).

%% ===================================================================
%% select_by_where/5 测试 - binary Where
%% ===================================================================

select_by_where_5_binary_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'page', 7, fun(_Tb, _Col, _Where, _Order, _Page, _Limit) ->
                {ok, [#{<<"id">> => 3}]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:select_by_where(<<"id,account">>, <<"role_id = 1">>, 10, 0, <<"id DESC">>),
        ?assertMatch({ok, [_]}, Result)
    end).

%% ===================================================================
%% select_by_where/5 测试 - map Where
%% ===================================================================

select_by_where_5_map_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'page', 7, fun(_Tb, _Col, _Where, _Order, _Page, _Limit) ->
                {ok, [#{<<"id">> => 4}]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:select_by_where(<<"id">>, #{<<"status">> => 1}, 10, 0, <<"id ASC">>),
        ?assertMatch({ok, [_]}, Result)
    end).

%% ===================================================================
%% select_by_where_safe/6 测试
%% ===================================================================

select_by_where_safe_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg_sql, [
            {'build_select_safe', 6, fun(_Tb, _Col, _Where, _Order, _Fields, _Opts) ->
                {<<"SELECT id FROM adm_user WHERE status = $1 ORDER BY id DESC LIMIT 10">>, [1]}
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [1]) ->
                {ok, [[#{<<"id">> => 1}, #{<<"id">> => 2}]]}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:select_by_where_safe(
            <<"id">>,
            #{<<"status">> => 1},
            10,
            0,
            [{<<"id">>, desc}],
            [<<"id">>, <<"account">>]
        ),
        ?assertMatch({ok, [_, _]}, Result)
    end).

select_by_where_safe_zero_limit_test_() ->
    Result = adm_user_repo:select_by_where_safe(
        <<"id">>,
        #{<<"status">> => 1},
        0,
        0,
        [{<<"id">>, desc}],
        [<<"id">>]
    ),
    ?assertEqual({ok, []}, Result).

%% ===================================================================
%% save/1 测试
%% ===================================================================

save_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, _Data) -> {ok, 1} end}
        ]}
    ], fun() ->
        Data = #{
            account => <<"test_admin"/utf8>>,
            password => <<"hashed_password">>,
            mobile => <<"13800138000">>,
            role_id => 1,
            status => 1
        },
        Result = adm_user_repo:save(Data),
        ?assertEqual({ok, 1}, Result)
    end).

save_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, _Data) -> {error, duplicate_key} end}
        ]}
    ], fun() ->
        Data = #{account => <<"admin">>, password => <<"pass">>},
        Result = adm_user_repo:save(Data),
        ?assertEqual({error, duplicate_key}, Result)
    end).

%% ===================================================================
%% update/2 测试
%% ===================================================================

update_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Data = #{nickname => <<"新昵称"/utf8>>},
        Result = adm_user_repo:update(1, Data),
        ?assertEqual({ok, 1}, Result)
    end).

update_not_found_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        Data = #{nickname => <<"新昵称"/utf8>>},
        Result = adm_user_repo:update(999, Data),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% delete/1 测试
%% ===================================================================

delete_success_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Tb, Data, _Where, _Params) ->
                ?assertEqual(-1, maps:get(status, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = adm_user_repo:delete(1),
        ?assertEqual({ok, 1}, Result)
    end).

delete_not_found_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"adm_user">> end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        Result = adm_user_repo:delete(999),
        ?assertEqual({ok, 0}, Result)
    end).
