-module(group_vote_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_vote_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群投票数据访问层功能
%%% 覆盖：投票CRUD、选项管理、投票记录、统计查询
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(
        elib_pg_sql,
        [
            {'public_tablename', 1, fun(_Table) -> <<"public.group_vote">> end}
        ],
        fun() ->
            Result = group_vote_repo:tablename(),
            ?assertEqual(<<"public.group_vote">>, Result)
        end
    ).

%% ===================================================================
%% insert_vote/1 测试 - 插入投票
%% ===================================================================

insert_vote_success_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 1001 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
    Data = #{
        group_id => 123,
        vote_id => <<"vote123">>,
        title => <<"今天吃什么？"/utf8>>,
        description => <<"投票选择今天午餐"/utf8>>,
        creator_id => 456,
        vote_type => 1,
        is_anonymous => false,
        status => 1
    },
    Result = group_vote_repo:insert_vote(Data),
    meck:unload(elib_pg),
    meck:unload(elib_tsid),
    ?assertMatch({ok, 1001, _}, Result).

insert_vote_with_missing_required_field_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'insert', 3, fun(_Table, _Data, _Returning) ->
                {error, {missing_field, group_id}}
            end}
        ],
        fun() ->
            Data = #{title => <<"标题"/utf8>>},
            Result = group_vote_repo:insert_vote(Data),
            ?assertMatch({error, {missing_field, _}}, Result)
        end
    ).

%% ===================================================================
%% find_by_vote_id/1 测试 - 根据vote_id查询投票
%% ===================================================================

find_by_vote_id_success_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{
                        <<"id">> => 1001,
                        <<"vote_id">> => <<"vote123">>,
                        <<"title">> => <<"今天吃什么？"/utf8>>
                    }
                ]}
            end}
        ],
        fun() ->
            Result = group_vote_repo:find_by_vote_id(<<"vote123">>),
            ?assertMatch({ok, _}, Result),
            {ok, Vote} = Result,
            ?assertEqual(<<"vote123">>, maps:get(<<"vote_id">>, Vote))
        end
    ).

find_by_vote_id_not_found_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ],
        fun() ->
            Result = group_vote_repo:find_by_vote_id(<<"notexist">>),
            ?assertEqual({error, not_found}, Result)
        end
    ).

%% ===================================================================
%% insert_option/1 测试 - 插入投票选项
%% ===================================================================

insert_option_success_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 2001 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
    Data = #{
        vote_id => <<"vote123">>,
        option_id => <<"opt1">>,
        option_text => <<"火锅"/utf8>>,
        sort_order => 1
    },
    Result = group_vote_repo:insert_option(Data),
    meck:unload(elib_pg),
    meck:unload(elib_tsid),
    ?assertMatch({ok, 2001, _}, Result).

insert_option_batch_success_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, 2}
            end}
        ],
        fun() ->
            % 批量插入现为每行生成 id TSID，测试环境无 generator 注册，需 mock。
            _ = catch meck:unload(elib_tsid),
            meck:new(elib_tsid, [passthrough, no_link]),
            meck:expect(elib_tsid, generate, fun(group_vote_option) -> 3001 end),
            Options = [
                #{
                    vote_id => <<"vote123">>,
                    option_id => <<"opt1">>,
                    option_text => <<"火锅"/utf8>>,
                    sort_order => 1
                },
                #{
                    vote_id => <<"vote123">>,
                    option_id => <<"opt2">>,
                    option_text => <<"烧烤"/utf8>>,
                    sort_order => 2
                }
            ],
            Result = group_vote_repo:insert_options_batch(Options),
            meck:unload(elib_tsid),
            ?assertMatch({ok, 2}, Result)
        end
    ).

%% 回归：批量插入必须显式提供 id（表 id NOT NULL 无默认值）。
%% 此前 SQL 漏 id 列在生产触发 23502 not_null_violation（2026-07-12）。
insert_option_batch_includes_id_column_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(group_vote_option) -> 3001 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    Self = self(),
    meck:expect(elib_pg, query, fun(Sql, Params) ->
        Self ! {captured, iolist_to_binary(Sql), Params},
        {ok, 2}
    end),
    Options = [
        #{vote_id => <<"v1">>, option_id => <<"o1">>, option_text => <<"A">>, sort_order => 1},
        #{vote_id => <<"v1">>, option_id => <<"o2">>, option_text => <<"B">>, sort_order => 2}
    ],
    ?assertMatch({ok, 2}, group_vote_repo:insert_options_batch(Options)),
    receive
        {captured, Sql, Params} ->
            ?assertMatch({_, _}, binary:match(Sql, <<"(id, vote_id, option_id">>)),
            % 每行 6 参数（id 领头），2 行共 12 个
            ?assertEqual(12, length(Params)),
            ?assertEqual(3001, hd(Params))
    after 1000 ->
        ?assert(false)
    end,
    meck:unload(elib_pg),
    meck:unload(elib_tsid).

%% ===================================================================
%% list_options_by_vote_id/1 测试 - 查询投票选项列表
%% ===================================================================

list_options_by_vote_id_success_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{
                        <<"option_id">> => <<"opt1">>,
                        <<"option_text">> => <<"火锅"/utf8>>,
                        <<"sort_order">> => 1
                    },
                    #{
                        <<"option_id">> => <<"opt2">>,
                        <<"option_text">> => <<"烧烤"/utf8>>,
                        <<"sort_order">> => 2
                    }
                ]}
            end}
        ],
        fun() ->
            Result = group_vote_repo:list_options_by_vote_id(<<"vote123">>),
            ?assertMatch({ok, [_ | _]}, Result),
            {ok, Options} = Result,
            ?assertEqual(2, length(Options))
        end
    ).

%% ===================================================================
%% insert_record/1 测试 - 插入投票记录
%% ===================================================================

insert_record_success_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 3001 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
    Data = #{
        vote_id => <<"vote123">>,
        user_id => 789,
        option_ids => <<"[\"opt1\",\"opt2\"]">>
    },
    Result = group_vote_repo:insert_record(Data),
    meck:unload(elib_pg),
    meck:unload(elib_tsid),
    ?assertMatch({ok, 3001, _}, Result).

%% ===================================================================
%% find_record_by_vote_and_user/2 测试 - 查询用户投票记录
%% ===================================================================

find_record_by_vote_and_user_found_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{
                        <<"id">> => 3001,
                        <<"vote_id">> => <<"vote123">>,
                        <<"user_id">> => 789,
                        <<"option_ids">> => <<"[\"opt1\"]">>
                    }
                ]}
            end}
        ],
        fun() ->
            Result = group_vote_repo:find_record_by_vote_and_user(<<"vote123">>, 789),
            ?assertMatch({ok, _}, Result)
        end
    ).

find_record_by_vote_and_user_not_found_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ],
        fun() ->
            Result = group_vote_repo:find_record_by_vote_and_user(<<"vote123">>, 999),
            ?assertEqual({error, not_found}, Result)
        end
    ).

%% ===================================================================
%% update_record/2 测试 - 更新投票记录
%% ===================================================================

update_record_success_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'update', 4, fun(_Table, _Data, _Where, _Params) ->
                {ok, 1}
            end}
        ],
        fun() ->
            Data = #{option_ids => <<"[\"opt2\"]">>},
            Result = group_vote_repo:update_record(3001, Data),
            ?assertEqual({ok, 1}, Result)
        end
    ).

%% ===================================================================
%% list_votes_by_group_id/4 测试 - 分页查询群投票列表
%% ===================================================================

list_votes_by_group_id_success_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{<<"id">> => 1001, <<"title">> => <<"投票1"/utf8>>},
                    #{<<"id">> => 1002, <<"title">> => <<"投票2"/utf8>>}
                ]}
            end}
        ],
        fun() ->
            Result = group_vote_repo:list_votes_by_group_id(123, 1, 10),
            ?assertMatch({ok, [_ | _]}, Result),
            {ok, Votes} = Result,
            ?assertEqual(2, length(Votes))
        end
    ).

%% ===================================================================
%% count_votes_by_group_id/1 测试 - 统计群投票数量
%% ===================================================================

count_votes_by_group_id_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"count">> => 5}]}
            end}
        ],
        fun() ->
            Result = group_vote_repo:count_votes_by_group_id(123),
            ?assertEqual({ok, 5}, Result)
        end
    ).

%% ===================================================================
%% update_vote_status/2 测试 - 更新投票状态
%% ===================================================================

update_vote_status_success_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'update', 4, fun(_Table, _Data, _Where, _Params) ->
                {ok, 1}
            end}
        ],
        fun() ->
            Result = group_vote_repo:update_vote_status(<<"vote123">>, 2),
            ?assertEqual({ok, 1}, Result)
        end
    ).

%% ===================================================================
%% count_votes_by_option_id/1 测试 - 统计选项得票数
%% ===================================================================

count_votes_by_option_id_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"count">> => 10}]}
            end}
        ],
        fun() ->
            Result = group_vote_repo:count_votes_by_option_id(<<"opt1">>),
            ?assertEqual({ok, 10}, Result)
        end
    ).

%% ===================================================================
%% count_total_votes_by_vote_id/1 测试 - 统计投票总人数
%% ===================================================================

count_total_votes_by_vote_id_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"count">> => 15}]}
            end}
        ],
        fun() ->
            Result = group_vote_repo:count_total_votes_by_vote_id(<<"vote123">>),
            ?assertEqual({ok, 15}, Result)
        end
    ).

%% ===================================================================
%% count_votes_grouped_by_vote_id/1 测试 - 一次性聚合统计各选项得票数
%% ===================================================================

count_votes_grouped_by_vote_id_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [
                    #{<<"option_id">> => <<"opt1">>, <<"vote_count">> => 6},
                    #{<<"option_id">> => <<"opt2">>, <<"vote_count">> => 0}
                ]}
            end}
        ],
        fun() ->
            Result = group_vote_repo:count_votes_grouped_by_vote_id(<<"vote123">>),
            ?assertEqual(
                {ok, [
                    #{<<"option_id">> => <<"opt1">>, <<"vote_count">> => 6},
                    #{<<"option_id">> => <<"opt2">>, <<"vote_count">> => 0}
                ]},
                Result
            )
        end
    ).

count_votes_grouped_by_vote_id_invalid_param_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_vote_repo:count_votes_grouped_by_vote_id(<<>>),
        ?assertEqual({error, invalid_param}, Result)
    end).

%% ===================================================================
%% delete_vote_option_by_option_id/1 测试 - 删除投票选项
%% ===================================================================

delete_vote_option_by_option_id_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, 1}
            end}
        ],
        fun() ->
            Result = group_vote_repo:delete_vote_option_by_option_id(<<"opt1">>),
            ?assertEqual({ok, 1}, Result)
        end
    ).
