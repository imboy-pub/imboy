-module(group_notice_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_notice_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组公告数据访问层功能
%%% 覆盖：增删改查、分页查询、置顶、软删除、已读统计
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.group_notice">> end}
    ], fun() ->
        Result = group_notice_repo:tablename(),
        ?assertEqual(<<"public.group_notice">>, Result)
    end).

%% ===================================================================
%% insert/1 测试 - 插入公告
%% ===================================================================

insert_success_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 1001 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
    Data = #{
        group_id => 123,
        user_id => 456,
        title => <<"公告标题"/utf8>>,
        body => <<"公告内容"/utf8>>,
        status => 1,
        pinned => false
    },
    Result = group_notice_repo:insert(Data),
    meck:unload(elib_pg),
    meck:unload(elib_tsid),
    ?assertMatch({ok, 1001}, Result).

insert_with_missing_required_field_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 3, fun(_Table, _Data, _Returning) ->
            {error, {missing_field, group_id}}
        end}
    ], fun() ->
        Data = #{body => <<"公告内容"/utf8>>},
        Result = group_notice_repo:insert(Data),
        ?assertMatch({error, {missing_field, _}}, Result)
    end).

%% ===================================================================
%% update/2 测试 - 更新公告
%% ===================================================================

update_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 1}
        end}
    ], fun() ->
        Data = #{title => <<"新标题"/utf8>>, body => <<"新内容"/utf8>>},
        Result = group_notice_repo:update(1001, Data),
        ?assertEqual({ok, 1}, Result)
    end).

update_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 0}
        end}
    ], fun() ->
        Data = #{title => <<"新标题"/utf8>>},
        Result = group_notice_repo:update(999999, Data),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% soft_delete/1 测试 - 软删除
%% ===================================================================

soft_delete_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_notice_repo:soft_delete(1001),
        ?assertEqual({ok, 1}, Result)
    end).

soft_delete_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 0}
        end}
    ], fun() ->
        Result = group_notice_repo:soft_delete(999999),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% pin/1 测试 - 置顶公告
%% ===================================================================

pin_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_notice_repo:pin(1001),
        ?assertEqual({ok, 1}, Result)
    end).

pin_already_pinned_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_notice_repo:pin(1001),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% unpin/1 测试 - 取消置顶
%% ===================================================================

unpin_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) ->
            {ok, 1}
        end}
    ], fun() ->
        Result = group_notice_repo:unpin(1001),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% find_by_id/1 测试 - 查询单个公告
%% ===================================================================

find_by_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 1001},
                    {<<"group_id">>, 123},
                    {<<"title">>, <<"公告标题"/utf8>>},
                    {<<"body">>, <<"公告内容"/utf8>>}]}]}
        end}
    ], fun() ->
        Result = group_notice_repo:find_by_id(1001),
        ?assertMatch({ok, _}, Result)
    end).

find_by_id_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_notice_repo:find_by_id(999999),
        ?assertEqual({error, not_found}, Result)
    end).

%% ===================================================================
%% list_by_group_id/3 测试 - 分页查询公告列表
%% ===================================================================

list_by_group_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 1001},
                    {<<"group_id">>, 123},
                    {<<"title">>, <<"置顶公告"/utf8>>},
                    {<<"pinned">>, true},
                    {<<"created_at">>, 1705334400000}]},
                  {[{<<"id">>, 1002},
                    {<<"group_id">>, 123},
                    {<<"title">>, <<"普通公告"/utf8>>},
                    {<<"pinned">>, false},
                    {<<"created_at">>, 1705334300000}]}]}
        end}
    ], fun() ->
        Result = group_notice_repo:list_by_group_id(123, 1, 20),
        ?assertMatch({ok, _}, Result)
    end).

list_by_group_id_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_notice_repo:list_by_group_id(999, 1, 20),
        ?assertEqual({ok, []}, Result)
    end).

list_by_group_id_pinned_first_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            % 验证 SQL 包含 ORDER BY pinned DESC, id DESC
            ?assert(binary:match(_Sql, <<"ORDER BY pinned DESC, id DESC">>) =/= nomatch),
            {ok, [{[{<<"id">>, 1001}, {<<"pinned">>, true}]}]}
        end}
    ], fun() ->
        Result = group_notice_repo:list_by_group_id(123, 1, 20),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% count_by_group_id/1 测试 - 统计公告数量
%% ===================================================================

count_by_group_id_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 15}]}
        end}
    ], fun() ->
        Result = group_notice_repo:count_by_group_id(123),
        ?assertEqual({ok, 15}, Result)
    end).

count_by_group_id_zero_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 0}]}
        end}
    ], fun() ->
        Result = group_notice_repo:count_by_group_id(999),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% increment_read_count/1 测试 - 增加已读数
%% ===================================================================

increment_read_count_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 1001}, {<<"read_count">>, 51}]}]}
        end}
    ], fun() ->
        Result = group_notice_repo:increment_read_count(1001),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% get_pinned_notices/1 测试 - 获取置顶公告
%% ===================================================================

get_pinned_notices_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{[{<<"id">>, 1001},
                    {<<"group_id">>, 123},
                    {<<"title">>, <<"置顶公告"/utf8>>},
                    {<<"pinned">>, true}]}]}
        end}
    ], fun() ->
        Result = group_notice_repo:get_pinned_notices(123),
        ?assertMatch({ok, _}, Result)
    end).

get_pinned_notices_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        Result = group_notice_repo:get_pinned_notices(999),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

insert_with_long_title_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 1002 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) ->
        {error, {string_too_long, title}}
    end),
    LongTitle = binary:copy(<<"测试"/utf8>>, 100),
    Data = #{
        group_id => 123,
        user_id => 456,
        title => LongTitle,
        body => <<"内容"/utf8>>
    },
    Result = group_notice_repo:insert(Data),
    meck:unload(elib_pg),
    meck:unload(elib_tsid),
    ?assertMatch({error, {string_too_long, _}}, Result).

insert_with_long_body_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 1003 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) ->
        {error, {string_too_long, body}}
    end),
    LongBody = binary:copy(<<"测试"/utf8>>, 3000),
    Data = #{
        group_id => 123,
        user_id => 456,
        body => LongBody
    },
    Result = group_notice_repo:insert(Data),
    meck:unload(elib_pg),
    meck:unload(elib_tsid),
    ?assertMatch({error, {string_too_long, _}}, Result).

%% ===================================================================
%% 注意
%% ===================================================================
%% 测试使用 meck 模拟 elib_pg 模块
%% 实际使用时请确保数据库连接正常
%%===================================================================
