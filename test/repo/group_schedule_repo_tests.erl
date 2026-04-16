-module(group_schedule_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

%%%===================================================================
%%% @doc
%%% group_schedule_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组日程数据访问层功能
%%% 覆盖：增删改查、分页查询、状态更新、参与人管理
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test() ->
    _ = catch meck:unload(elib_pg_sql),
    Result = group_schedule_repo:tablename(),
    ?assertEqual(<<"public.group_schedule">>, Result).

%% ===================================================================
%% insert/1 测试 - 插入日程
%% ===================================================================

insert_success_test() ->
    _ = catch meck:unload(elib_pg_sql),
    meck:new(elib_pg_sql, [passthrough, no_link]),
    meck:expect(elib_pg_sql, public_tablename, fun(_Table) -> <<"public.group_schedule">> end),
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 1001 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
    Data = #{
        group_id => 123,
        schedule_id => <<"sched_123">>,
        title => <<"团队会议"/utf8>>,
        description => <<"讨论下周工作计划"/utf8>>,
        location => <<"会议室A"/utf8>>,
        creator_id => 456,
        start_at => <<"2026-02-20T10:00:00Z">>,
        end_at => <<"2026-02-20T11:00:00Z">>,
        remind_before => 15,
        status => 1
    },
    Result = group_schedule_repo:insert(Data),
    ?assertMatch({ok, 1001, _}, Result),
    meck:unload(elib_pg),
    meck:unload(elib_tsid),
    meck:unload(elib_pg_sql).

insert_with_missing_required_field_test() ->
    _ = catch meck:unload(elib_pg_sql),
    meck:new(elib_pg_sql, [passthrough, no_link]),
    meck:expect(elib_pg_sql, public_tablename, fun(_Table) -> <<"public.group_schedule">> end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, insert, fun(_Table, _Data, _Returning) ->
        {error, {missing_field, group_id}}
    end),
    Data = #{title => <<"日程标题"/utf8>>},
    Result = group_schedule_repo:insert(Data),
    ?assertMatch({error, {missing_field, _}}, Result),
    meck:unload(elib_pg),
    meck:unload(elib_pg_sql).

%% ===================================================================
%% update/2 测试 - 更新日程
%% ===================================================================

update_success_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, update, fun(_Table, _Data, _Where, _Params) ->
        {ok, 1}
    end),
    Data = #{title => <<"新标题"/utf8>>, description => <<"更新内容"/utf8>>},
    Result = group_schedule_repo:update(1001, Data),
    ?assertEqual({ok, 1}, Result),
    meck:unload(elib_pg).

update_not_found_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, update, fun(_Table, _Data, _Where, _Params) ->
        {ok, 0}
    end),
    Data = #{title => <<"新标题"/utf8>>},
    Result = group_schedule_repo:update(999999, Data),
    ?assertEqual({ok, 0}, Result),
    meck:unload(elib_pg).

%% ===================================================================
%% find_by_id/2 测试 - 查询单个日程
%% ===================================================================

find_by_id_success_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, one, fun(_Sql, _Params) ->
        {ok, #{<<"id">> => 1001,
                <<"group_id">> => 123,
                <<"title">> => <<"会议"/utf8>>}}
    end),
    Result = group_schedule_repo:find_by_id(1001),
    ?assertMatch(#{<<"id">> := 1001, <<"title">> := <<"会议"/utf8>>}, Result),
    meck:unload(elib_pg).

find_by_id_not_found_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, one, fun(_Sql, _Params) ->
        {error, not_found}
    end),
    Result = group_schedule_repo:find_by_id(999999),
    ?assertEqual({error, not_found}, Result),
    meck:unload(elib_pg).

%% ===================================================================
%% find_by_schedule_id/2 测试 - 通过 schedule_id 查询
%% ===================================================================

find_by_schedule_id_success_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, one, fun(_Sql, _Params) ->
        {ok, #{<<"id">> => 1001,
                <<"schedule_id">> => <<"sched_123">>,
                <<"title">> => <<"会议"/utf8>>}}
    end),
    Result = group_schedule_repo:find_by_schedule_id(<<"sched_123">>),
    ?assertMatch(#{<<"schedule_id">> := <<"sched_123">>}, Result),
    meck:unload(elib_pg).

find_by_schedule_id_not_found_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, one, fun(_Sql, _Params) ->
        {error, not_found}
    end),
    Result = group_schedule_repo:find_by_schedule_id(<<"sched_not_exist">>),
    ?assertEqual({error, not_found}, Result),
    meck:unload(elib_pg).

%% ===================================================================
%% list_by_group_id/4 测试 - 分页查询群组日程列表
%% ===================================================================

list_by_group_id_success_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) ->
        {ok, [#{<<"id">> => 1001, <<"title">> => <<"会议1"/utf8>>},
               #{<<"id">> => 1002, <<"title">> => <<"会议2"/utf8>>}]}
    end),
    Result = group_schedule_repo:list_by_group_id(123, 1, 20),
    ?assertMatch({ok, [_, _]}, Result),
    meck:unload(elib_pg).

list_by_group_id_empty_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) ->
        {ok, []}
    end),
    Result = group_schedule_repo:list_by_group_id(999, 1, 20),
    ?assertEqual({ok, []}, Result),
    meck:unload(elib_pg).

list_by_group_id_with_time_filter_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(Sql, [123, <<"2026-02-22T00:00:00Z">>, <<"2026-02-23T00:00:00Z">>]) ->
        ?assertNotEqual(nomatch, binary:match(Sql, <<"end_at >= $2">>)),
        ?assertNotEqual(nomatch, binary:match(Sql, <<"start_at <= $3">>)),
        {ok, []}
    end),
    Result = group_schedule_repo:list_by_group_id(
        123,
        <<"2026-02-22T00:00:00Z">>,
        <<"2026-02-23T00:00:00Z">>,
        1,
        20
    ),
    ?assertEqual({ok, []}, Result),
    meck:unload(elib_pg).

%% ===================================================================
%% list_by_user_id/4 测试 - 查询用户参与的日程
%% ===================================================================

list_by_user_id_success_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(Sql, [456]) ->
        ?assertNotEqual(nomatch, binary:match(Sql, <<"INNER JOIN">>)),
        ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE p.user_id = $1">>)),
        {JoinPos, _} = binary:match(Sql, <<"INNER JOIN">>),
        {WherePos, _} = binary:match(Sql, <<"WHERE p.user_id = $1">>),
        ?assert(JoinPos < WherePos),
        {ok, [#{<<"id">> => 1001, <<"title">> => <<"会议1"/utf8>>},
               #{<<"id">> => 1002, <<"title">> => <<"会议2"/utf8>>}]}
    end),
    Result = group_schedule_repo:list_by_user_id(456, 1, 20),
    ?assertMatch({ok, [_, _]}, Result),
    meck:unload(elib_pg).

list_by_user_id_with_time_filter_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(Sql, [456, <<"2026-02-22T00:00:00Z">>, <<"2026-02-23T00:00:00Z">>]) ->
        ?assertNotEqual(nomatch, binary:match(Sql, <<"gs.end_at >= $2">>)),
        ?assertNotEqual(nomatch, binary:match(Sql, <<"gs.start_at <= $3">>)),
        {ok, []}
    end),
    Result = group_schedule_repo:list_by_user_id(
        456,
        <<"2026-02-22T00:00:00Z">>,
        <<"2026-02-23T00:00:00Z">>,
        1,
        20
    ),
    ?assertEqual({ok, []}, Result),
    meck:unload(elib_pg).

%% ===================================================================
%% update_status/2 测试 - 更新日程状态
%% ===================================================================

update_status_success_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, update, fun(_Table, _Data, _Where, _Params) ->
        {ok, 1}
    end),
    Result = group_schedule_repo:update_status(1001, 2),
    ?assertEqual({ok, 1}, Result),
    meck:unload(elib_pg).

count_by_group_id_with_time_filter_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, one, fun(Sql, [123, <<"2026-02-22T00:00:00Z">>, <<"2026-02-23T00:00:00Z">>]) ->
        ?assertNotEqual(nomatch, binary:match(Sql, <<"end_at >= $2">>)),
        ?assertNotEqual(nomatch, binary:match(Sql, <<"start_at <= $3">>)),
        {ok, #{<<"count">> => 2}}
    end),
    Result = group_schedule_repo:count_by_group_id(
        123,
        <<"2026-02-22T00:00:00Z">>,
        <<"2026-02-23T00:00:00Z">>
    ),
    ?assertEqual({ok, 2}, Result),
    meck:unload(elib_pg).

%% ===================================================================
%% Participant 表测试
%% ===================================================================

participant_tablename_test() ->
    _ = catch meck:unload(elib_pg_sql),
    Result = group_schedule_repo:participant_tablename(),
    ?assertEqual(<<"public.group_schedule_participant">>, Result).

insert_participant_success_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 2001 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
    Data = #{
        schedule_id => <<"sched_123">>,
        user_id => 789,
        status => 0
    },
    Result = group_schedule_repo:insert_participant(Data),
    ?assertMatch({ok, 2001, _}, Result),
    meck:unload(elib_pg),
    meck:unload(elib_tsid).

update_participant_status_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, update, fun(_Table, _Data, _Where, _Params) ->
        {ok, 1}
    end),
    Result = group_schedule_repo:update_participant_status(<<"sched_123">>, 789, 1),
    ?assertEqual({ok, 1}, Result),
    meck:unload(elib_pg).

list_participants_success_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) ->
        {ok, [#{<<"user_id">> => 789, <<"status">> => 1},
               #{<<"user_id">> => 790, <<"status">> => 0}]}
    end),
    Result = group_schedule_repo:list_participants(<<"sched_123">>),
    ?assertMatch({ok, [_, _]}, Result),
    meck:unload(elib_pg).

%% ===================================================================
%% Remind 表测试
%% ===================================================================

remind_tablename_test() ->
    _ = catch meck:unload(elib_pg_sql),
    Result = group_schedule_repo:remind_tablename(),
    ?assertEqual(<<"public.group_schedule_remind">>, Result).

insert_remind_success_test() ->
    _ = catch meck:unload(elib_tsid),
    meck:new(elib_tsid, [passthrough, no_link]),
    meck:expect(elib_tsid, generate, fun(_Table) -> 3001 end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end),
    Data = #{
        schedule_id => <<"sched_123">>,
        user_id => 789,
        remind_at => <<"2026-02-20T09:45:00Z">>,
        is_sent => false
    },
    Result = group_schedule_repo:insert_remind(Data),
    ?assertMatch({ok, 3001, _}, Result),
    meck:unload(elib_pg),
    meck:unload(elib_tsid).

list_pending_reminds_success_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, query, fun(_Sql, _Params) ->
        {ok, [#{<<"id">> => 3001, <<"schedule_id">> => <<"sched_123">>}]}
    end),
    Result = group_schedule_repo:list_pending_reminds(),
    ?assertMatch({ok, [_]}, Result),
    meck:unload(elib_pg).

update_remind_sent_test() ->
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, update, fun(_Table, _Data, _Where, _Params) ->
        {ok, 1}
    end),
    Result = group_schedule_repo:update_remind_sent(3001),
    ?assertEqual({ok, 1}, Result),
    meck:unload(elib_pg).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

insert_with_empty_title_test() ->
    _ = catch meck:unload(elib_pg_sql),
    meck:new(elib_pg_sql, [passthrough, no_link]),
    meck:expect(elib_pg_sql, public_tablename, fun(_Table) -> <<"public.group_schedule">> end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, insert, fun(_Table, _Data, _Returning) ->
        {error, {invalid_field, title}}
    end),
    Data = #{
        group_id => 123,
        title => <<>>
    },
    Result = group_schedule_repo:insert(Data),
    ?assertMatch({error, {missing_field, title}}, Result),
    meck:unload(elib_pg),
    meck:unload(elib_pg_sql).

insert_with_invalid_time_range_test() ->
    _ = catch meck:unload(elib_pg_sql),
    meck:new(elib_pg_sql, [passthrough, no_link]),
    meck:expect(elib_pg_sql, public_tablename, fun(_Table) -> <<"public.group_schedule">> end),
    _ = catch meck:unload(elib_pg),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, insert, fun(_Table, _Data, _Returning) ->
        {error, {invalid_time_range, start_at, end_at}}
    end),
    Data = #{
        group_id => 123,
        title => <<"会议"/utf8>>,
        start_at => <<"2026-02-20T11:00:00Z">>,
        end_at => <<"2026-02-20T10:00:00Z">>
    },
    Result = group_schedule_repo:insert(Data),
    ?assertMatch({error, {invalid_time_range, _, _}}, Result),
    meck:unload(elib_pg),
    meck:unload(elib_pg_sql).
