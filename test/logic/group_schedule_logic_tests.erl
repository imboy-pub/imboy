-module(group_schedule_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

%%%===================================================================
%%% @doc
%%% group_schedule_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群组日程业务逻辑功能
%%% 覆盖：创建日程、修改日程、取消日程、参与人确认、提醒处理
%%%===================================================================

%% ===================================================================
%% create_schedule/5 测试 - 创建日程
%% ===================================================================

create_schedule_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, insert, fun(_Data) ->
        {ok, 1001, #{<<"id">> => 1001, <<"schedule_id">> => <<"sched_123">>}}
    end),
    meck:expect(group_schedule_repo, insert_participant, fun(_Data) ->
        {ok, 2001, #{<<"id">> => 2001}}
    end),
    meck:expect(group_schedule_repo, insert_remind, fun(_Data) ->
        {ok, 3001, #{<<"id">> => 3001}}
    end),
    _ = catch meck:unload(elib_dt),
    meck:new(elib_dt, [passthrough, no_link]),
    meck:expect(elib_dt, now, fun() -> 1640995200000 end),
    meck:expect(elib_dt, timestamp, fun() -> 1640995200000 end),
    _ = catch meck:unload(group_ds),
    meck:new(group_ds, [passthrough, no_link]),
    meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> true end),

    Result = group_schedule_logic:create_schedule(
        % GroupId
        123,
        % CreatorId
        456,
        <<"团队会议"/utf8>>,
        <<"讨论项目进展"/utf8>>,
        <<"会议室A"/utf8>>,
        <<"2026-02-20T10:00:00Z">>,
        <<"2026-02-20T11:00:00Z">>,
        % RemindBefore
        15,
        % ParticipantIds
        [789, 790]
    ),

    ?assertMatch({ok, #{schedule_id := <<"sched_", _/binary>>}}, Result),
    meck:unload(elib_dt),
    meck:unload(group_ds),
    meck:unload(group_schedule_repo).

create_schedule_without_participants_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, insert, fun(_Data) ->
        {ok, 1001, #{<<"id">> => 1001, <<"schedule_id">> => <<"sched_123">>}}
    end),
    meck:expect(group_schedule_repo, insert_participant, fun(_Data) ->
        {ok, 2001, #{<<"id">> => 2001}}
    end),
    meck:expect(group_schedule_repo, insert_remind, fun(_Data) ->
        {ok, 3001, #{<<"id">> => 3001}}
    end),
    _ = catch meck:unload(elib_dt),
    meck:new(elib_dt, [passthrough, no_link]),
    meck:expect(elib_dt, now, fun() -> 1640995200000 end),
    meck:expect(elib_dt, timestamp, fun() -> 1640995200 end),
    _ = catch meck:unload(group_ds),
    meck:new(group_ds, [passthrough, no_link]),
    meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> true end),

    Result = group_schedule_logic:create_schedule(
        % GroupId
        123,
        % CreatorId
        456,
        <<"团队会议"/utf8>>,
        <<"讨论项目进展"/utf8>>,
        <<"会议室A"/utf8>>,
        <<"2026-02-20T10:00:00Z">>,
        <<"2026-02-20T11:00:00Z">>,
        % RemindBefore
        15,
        % ParticipantIds
        []
    ),

    ?assertMatch({ok, _}, Result),
    meck:unload(elib_dt),
    meck:unload(group_ds),
    meck:unload(group_schedule_repo).

create_schedule_with_invalid_time_range_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, insert, fun(_Data) ->
        {error, {invalid_time_range, start_at, end_at}}
    end),
    _ = catch meck:unload(elib_dt),
    meck:new(elib_dt, [passthrough, no_link]),
    meck:expect(elib_dt, now, fun() -> 1640995200000 end),
    meck:expect(elib_dt, timestamp, fun() -> 1640995200000 end),
    _ = catch meck:unload(group_ds),
    meck:new(group_ds, [passthrough, no_link]),
    meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> true end),

    Result = group_schedule_logic:create_schedule(
        % GroupId
        123,
        % CreatorId
        456,
        <<"团队会议"/utf8>>,
        <<"讨论项目进展"/utf8>>,
        <<"会议室A"/utf8>>,
        % 结束时间早于开始时间
        <<"2026-02-20T11:00:00Z">>,
        <<"2026-02-20T10:00:00Z">>,
        15,
        []
    ),

    ?assertMatch({error, {invalid_time_range, _, _}}, Result),
    meck:unload(elib_dt),
    meck:unload(group_ds),
    meck:unload(group_schedule_repo).

%% ===================================================================
%% update_schedule/6 测试 - 修改日程
%% ===================================================================

update_schedule_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{
            <<"id">> => 1001,
            <<"schedule_id">> => <<"sched_123">>,
            <<"creator_id">> => 456,
            <<"status">> => 1
        }
    end),
    meck:expect(group_schedule_repo, update, fun(_Id, _Data) ->
        {ok, 1}
    end),

    Result = group_schedule_logic:update_schedule(
        <<"sched_123">>,
        % CreatorId
        456,
        <<"更新后的会议"/utf8>>,
        <<"新的描述"/utf8>>,
        <<"会议室B"/utf8>>,
        <<"2026-02-20T10:30:00Z">>,
        <<"2026-02-20T11:30:00Z">>
    ),

    ?assertEqual(ok, Result),
    meck:unload(group_schedule_repo).

update_schedule_unauthorized_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{
            <<"id">> => 1001,
            <<"schedule_id">> => <<"sched_123">>,
            <<"creator_id">> => 789,
            <<"status">> => 1
        }
    end),

    Result = group_schedule_logic:update_schedule(
        <<"sched_123">>,
        % 不是创建者
        456,
        <<"更新后的会议"/utf8>>,
        <<"新的描述"/utf8>>,
        <<"会议室B"/utf8>>,
        <<"2026-02-20T10:30:00Z">>,
        <<"2026-02-20T11:30:00Z">>
    ),

    ?assertEqual({error, unauthorized}, Result),
    meck:unload(group_schedule_repo).

update_schedule_not_found_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        {error, not_found}
    end),

    Result = group_schedule_logic:update_schedule(
        <<"sched_not_exist">>,
        456,
        <<"更新后的会议"/utf8>>,
        <<"新的描述"/utf8>>,
        <<"会议室B"/utf8>>,
        <<"2026-02-20T10:30:00Z">>,
        <<"2026-02-20T11:30:00Z">>
    ),

    ?assertEqual({error, not_found}, Result),
    meck:unload(group_schedule_repo).

%% ===================================================================
%% cancel_schedule/2 测试 - 取消日程
%% ===================================================================

cancel_schedule_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{
            <<"id">> => 1001,
            <<"schedule_id">> => <<"sched_123">>,
            <<"creator_id">> => 456,
            <<"status">> => 1
        }
    end),
    meck:expect(group_schedule_repo, update_status, fun(_Id, _Status) ->
        {ok, 1}
    end),

    Result = group_schedule_logic:cancel_schedule(<<"sched_123">>, 456),

    ?assertEqual(ok, Result),
    meck:unload(group_schedule_repo).

cancel_schedule_already_cancelled_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{
            <<"id">> => 1001,
            <<"schedule_id">> => <<"sched_123">>,
            <<"creator_id">> => 456,
            <<"status">> => 4
        }
    end),

    Result = group_schedule_logic:cancel_schedule(<<"sched_123">>, 456),

    ?assertEqual({error, already_cancelled}, Result),
    meck:unload(group_schedule_repo).

cancel_schedule_unauthorized_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{
            <<"id">> => 1001,
            <<"schedule_id">> => <<"sched_123">>,
            <<"creator_id">> => 789,
            <<"status">> => 1
        }
    end),

    Result = group_schedule_logic:cancel_schedule(<<"sched_123">>, 456),

    ?assertEqual({error, unauthorized}, Result),
    meck:unload(group_schedule_repo).

%% ===================================================================
%% confirm_participation/3 测试 - 确认参与
%% ===================================================================

confirm_participation_accept_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{<<"id">> => 1001, <<"schedule_id">> => <<"sched_123">>, status => 1}
    end),
    meck:expect(group_schedule_repo, update_participant_status, fun(_ScheduleId, _UserId, _Status) ->
        {ok, 1}
    end),

    Result = group_schedule_logic:confirm_participation(<<"sched_123">>, 789, true),

    ?assertEqual(ok, Result),
    meck:unload(group_schedule_repo).

confirm_participation_decline_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{<<"id">> => 1001, <<"schedule_id">> => <<"sched_123">>, status => 1}
    end),
    meck:expect(group_schedule_repo, update_participant_status, fun(_ScheduleId, _UserId, _Status) ->
        {ok, 1}
    end),

    Result = group_schedule_logic:confirm_participation(<<"sched_123">>, 789, false),

    ?assertEqual(ok, Result),
    meck:unload(group_schedule_repo).

confirm_participation_schedule_not_found_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        {error, not_found}
    end),

    Result = group_schedule_logic:confirm_participation(<<"sched_not_exist">>, 789, true),

    ?assertEqual({error, schedule_not_found}, Result),
    meck:unload(group_schedule_repo).

%% ===================================================================
%% get_schedule_detail/1 测试 - 获取日程详情
%% ===================================================================

get_schedule_detail_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{<<"id">> => 1001, <<"schedule_id">> => <<"sched_123">>, <<"title">> => <<"会议"/utf8>>}
    end),
    meck:expect(group_schedule_repo, list_participants, fun(_ScheduleId) ->
        {ok, [#{<<"user_id">> => 789, <<"status">> => 1}]}
    end),
    meck:expect(group_schedule_repo, count_participants, fun(_ScheduleId) ->
        {ok, 5}
    end),

    Result = group_schedule_logic:get_schedule_detail(<<"sched_123">>),

    ?assertMatch({ok, #{schedule := _, participants := _}}, Result),
    meck:unload(group_schedule_repo).

get_schedule_detail_not_found_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        {error, not_found}
    end),

    Result = group_schedule_logic:get_schedule_detail(<<"sched_not_exist">>),

    ?assertEqual({error, schedule_not_found}, Result),
    meck:unload(group_schedule_repo).

%% ===================================================================
%% list_group_schedules/3 测试 - 查询群组日程
%% ===================================================================

list_group_schedules_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, list_by_group_id, fun(
        _GroupId, undefined, undefined, _Page, _Size
    ) ->
        {ok, [
            #{<<"id">> => 1001, <<"title">> => <<"会议1"/utf8>>},
            #{<<"id">> => 1002, <<"title">> => <<"会议2"/utf8>>}
        ]}
    end),
    meck:expect(group_schedule_repo, count_by_group_id, fun(_GroupId, undefined, undefined) ->
        {ok, 10}
    end),

    Result = group_schedule_logic:list_group_schedules(123, 1, 20),

    ?assertMatch({ok, #{list := [_, _], total := 10}}, Result),
    meck:unload(group_schedule_repo).

list_group_schedules_with_time_filter_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, list_by_group_id, fun(
        123, <<"2026-02-22T00:00:00Z">>, <<"2026-02-23T00:00:00Z">>, 1, 20
    ) ->
        {ok, [#{<<"id">> => 1001, <<"title">> => <<"会议1"/utf8>>}]}
    end),
    meck:expect(group_schedule_repo, count_by_group_id, fun(
        123, <<"2026-02-22T00:00:00Z">>, <<"2026-02-23T00:00:00Z">>
    ) ->
        {ok, 1}
    end),

    Result = group_schedule_logic:list_group_schedules(
        123,
        <<"2026-02-22T00:00:00Z">>,
        <<"2026-02-23T00:00:00Z">>,
        1,
        20
    ),

    ?assertMatch({ok, #{list := [_], total := 1}}, Result),
    meck:unload(group_schedule_repo).

%% ===================================================================
%% list_my_schedules/3 测试 - 查询我的日程
%% ===================================================================

list_my_schedules_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, list_by_user_id, fun(
        _UserId, undefined, undefined, _Page, _Size
    ) ->
        {ok, [
            #{<<"id">> => 1001, <<"title">> => <<"会议1"/utf8>>},
            #{<<"id">> => 1002, <<"title">> => <<"会议2"/utf8>>}
        ]}
    end),

    Result = group_schedule_logic:list_my_schedules(456, 1, 20),

    ?assertMatch({ok, [_, _]}, Result),
    meck:unload(group_schedule_repo).

list_my_schedules_with_time_filter_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, list_by_user_id, fun(
        456, <<"2026-02-22T00:00:00Z">>, <<"2026-02-23T00:00:00Z">>, 1, 20
    ) ->
        {ok, [#{<<"id">> => 1001, <<"title">> => <<"会议1"/utf8>>}]}
    end),

    Result = group_schedule_logic:list_my_schedules(
        456,
        <<"2026-02-22T00:00:00Z">>,
        <<"2026-02-23T00:00:00Z">>,
        1,
        20
    ),

    ?assertMatch({ok, [_]}, Result),
    meck:unload(group_schedule_repo).

%% ===================================================================
%% process_reminders/0 测试 - 处理提醒
%% ===================================================================

process_reminders_success_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, list_pending_reminds, fun() ->
        {ok, [#{<<"id">> => 3001, <<"schedule_id">> => <<"sched_123">>, <<"user_id">> => 789}]}
    end),
    meck:expect(group_schedule_repo, update_remind_sent, fun(_Id) ->
        {ok, 1}
    end),
    meck:expect(group_schedule_repo, find_by_schedule_id, fun(_ScheduleId) ->
        #{<<"title">> => <<"会议提醒"/utf8>>, <<"start_at">> => <<"2026-02-20T10:00:00Z">>}
    end),
    _ = catch meck:unload(msg_s2c_ds),
    meck:new(msg_s2c_ds, [passthrough, no_link]),
    meck:expect(msg_s2c_ds, send, fun(_From, _To, _Action, _MsgId, _Body, _Payload, _Save) ->
        ok
    end),

    Result = group_schedule_logic:process_reminders(),

    ?assertEqual({ok, 1}, Result),
    meck:unload(msg_s2c_ds),
    meck:unload(group_schedule_repo).

process_reminders_empty_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, list_pending_reminds, fun() ->
        {ok, []}
    end),

    Result = group_schedule_logic:process_reminders(),

    ?assertEqual({ok, 0}, Result),
    meck:unload(group_schedule_repo).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

create_schedule_with_empty_title_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, insert, fun(_Data) ->
        {error, {missing_field, title}}
    end),
    _ = catch meck:unload(elib_dt),
    meck:new(elib_dt, [passthrough, no_link]),
    meck:expect(elib_dt, now, fun() -> 1640995200000 end),
    meck:expect(elib_dt, timestamp, fun() -> 1640995200000 end),
    _ = catch meck:unload(group_ds),
    meck:new(group_ds, [passthrough, no_link]),
    meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> true end),

    Result = group_schedule_logic:create_schedule(
        123,
        456,
        <<>>,
        <<>>,
        <<>>,
        <<"2026-02-20T10:00:00Z">>,
        <<"2026-02-20T11:00:00Z">>,
        15,
        []
    ),

    ?assertMatch({error, {missing_field, title}}, Result),
    meck:unload(elib_dt),
    meck:unload(group_ds),
    meck:unload(group_schedule_repo).

create_schedule_with_too_many_participants_test() ->
    _ = catch meck:unload(group_schedule_repo),
    meck:new(group_schedule_repo, [passthrough, no_link]),
    meck:expect(group_schedule_repo, insert, fun(_Data) ->
        {ok, 1001, #{<<"id">> => 1001, <<"schedule_id">> => <<"sched_123">>}}
    end),
    _ = catch meck:unload(group_ds),
    meck:new(group_ds, [passthrough, no_link]),
    meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> true end),

    % 生成超过限制的参与人列表
    ParticipantIds = lists:seq(1, 101),

    Result = group_schedule_logic:create_schedule(
        123,
        456,
        <<"会议"/utf8>>,
        <<>>,
        <<>>,
        <<"2026-02-20T10:00:00Z">>,
        <<"2026-02-20T11:00:00Z">>,
        15,
        ParticipantIds
    ),

    ?assertMatch({error, too_many_participants}, Result),
    meck:unload(group_ds),
    meck:unload(group_schedule_repo).

create_schedule_not_group_member_test() ->
    _ = catch meck:unload(group_ds),
    meck:new(group_ds, [passthrough, no_link]),
    meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> false end),

    Result = group_schedule_logic:create_schedule(
        123,
        456,
        <<"会议"/utf8>>,
        <<>>,
        <<>>,
        <<"2026-02-20T10:00:00Z">>,
        <<"2026-02-20T11:00:00Z">>,
        15,
        []
    ),

    ?assertMatch({error, not_group_member}, Result),
    meck:unload(group_ds).
