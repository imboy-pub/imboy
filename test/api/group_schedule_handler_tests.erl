-module(group_schedule_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

%%%===================================================================
%%% @doc
%%% group_schedule_handler 模块的 EUnit 测试
%%%
%%% 目标：验证群组日程 API 处理器功能
%%% 覆盖：创建、修改、取消、查询日程、参与确认
%%%===================================================================

%% 导入 cowboy_req_h 用于测试

%% ===================================================================
%% create/2 测试 - 创建日程
%% ===================================================================

create_success_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, create_schedule, fun(_GroupId, _CreatorId, _Title, _Desc, _Loc, _Start, _End, _Remind, _Participants) ->
        {ok, #{schedule_id => <<"sched_123">>}}
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"group_id">> => <<"M123">>,
          <<"title">> => <<"团队会议"/utf8>>,
          <<"description">> => <<"讨论项目"/utf8>>,
          <<"location">> => <<"会议室A"/utf8>>,
          <<"start_at">> => <<"2026-02-20T10:00:00Z">>,
          <<"end_at">> => <<"2026-02-20T11:00:00Z">>,
          <<"remind_before">> => 15,
          <<"participant_ids">> => [<<"M456">>, <<"M789">>]}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, success, fun(_Req, _Data, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:create(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

create_missing_required_field_test() ->
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"title">> => <<"会议"/utf8>>}
        % 缺少 group_id
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, error, fun(_Req, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:create(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param).

create_invalid_time_range_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, create_schedule, fun(_GroupId, _CreatorId, _Title, _Desc, _Loc, _Start, _End, _Remind, _Participants) ->
        {error, {invalid_time_range, start_at, end_at}}
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"group_id">> => <<"M123">>,
          <<"title">> => <<"会议"/utf8>>,
          <<"start_at">> => <<"2026-02-20T11:00:00Z">>,
          <<"end_at">> => <<"2026-02-20T10:00:00Z">>}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, error, fun(_Req, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:create(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

%% ===================================================================
%% update/2 测试 - 修改日程
%% ===================================================================

update_success_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, update_schedule, fun(_ScheduleId, _CreatorId, _Title, _Desc, _Loc, _Start, _End) ->
        ok
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"schedule_id">> => <<"sched_123">>,
          <<"title">> => <<"更新后的会议"/utf8>>,
          <<"description">> => <<"新描述"/utf8>>,
          <<"location">> => <<"会议室B"/utf8>>,
          <<"start_at">> => <<"2026-02-20T10:30:00Z">>,
          <<"end_at">> => <<"2026-02-20T11:30:00Z">>}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, success, fun(_Req, _Data, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:update(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

update_unauthorized_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, update_schedule, fun(_ScheduleId, _CreatorId, _Title, _Desc, _Loc, _Start, _End) ->
        {error, unauthorized}
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"schedule_id">> => <<"sched_123">>,
          <<"title">> => <<"更新后的会议"/utf8>>}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, error, fun(_Req, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:update(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

%% ===================================================================
%% cancel/2 测试 - 取消日程
%% ===================================================================

cancel_success_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, cancel_schedule, fun(_ScheduleId, _CreatorId) ->
        ok
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"schedule_id">> => <<"sched_123">>}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, success, fun(_Req, _Data, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:cancel(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

cancel_already_cancelled_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, cancel_schedule, fun(_ScheduleId, _CreatorId) ->
        {error, already_cancelled}
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"schedule_id">> => <<"sched_123">>}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, error, fun(_Req, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:cancel(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

%% ===================================================================
%% detail/2 测试 - 查询日程详情
%% ===================================================================

detail_success_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, get_schedule_detail, fun(_ScheduleId) ->
        {ok, #{
            schedule => #{schedule_id => <<"sched_123">>, title => <<"会议"/utf8>>},
            participants => [#{user_id => 456, status => 1}],
            participant_count => 1
        }}
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, query, fun(_Req) ->
        #{schedule_id => <<"sched_123">>}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, success, fun(_Req, _Data, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"GET">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:detail(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

detail_not_found_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, get_schedule_detail, fun(_ScheduleId) ->
        {error, schedule_not_found}
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, query, fun(_Req) ->
        #{schedule_id => <<"sched_not_exist">>}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, error, fun(_Req, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"GET">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:detail(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

%% ===================================================================
%% list/2 测试 - 查询群组日程列表
%% ===================================================================

list_success_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, list_group_schedules, fun(_GroupId, _Page, _Size) ->
        {ok, #{
            list => [#{schedule_id => <<"sched_123">>, title => <<"会议1"/utf8>>}],
            total => 10,
            page => 1,
            size => 20
        }}
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, query, fun(_Req) ->
        #{group_id => <<"M123">>, page => 1, size => 20}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, success, fun(_Req, _Data, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"GET">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:list(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

%% ===================================================================
%% my_list/2 测试 - 查询我的日程
%% ===================================================================

my_list_success_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, list_my_schedules, fun(_UserId, _Page, _Size) ->
        {ok, [#{schedule_id => <<"sched_123">>, title => <<"会议1"/utf8>>}]}
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, page, fun(_Req) ->
        {1, 20}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, success, fun(_Req, _Data, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"GET">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:my_list(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

%% ===================================================================
%% confirm/2 测试 - 确认参与
%% ===================================================================

confirm_accept_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, confirm_participation, fun(_ScheduleId, _UserId, _Accept) ->
        ok
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"schedule_id">> => <<"sched_123">>, <<"accept">> => true}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, success, fun(_Req, _Data, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:confirm(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

confirm_decline_test() ->
    meck:new(group_schedule_logic, [passthrough, no_link]),
    meck:expect(group_schedule_logic, confirm_participation, fun(_ScheduleId, _UserId, _Accept) ->
        ok
    end),
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        #{<<"schedule_id">> => <<"sched_123">>, <<"accept">> => false}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, success, fun(_Req, _Data, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:confirm(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param),
    meck:unload(group_schedule_logic).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

create_with_too_long_title_test() ->
    meck:new(elib_param, [passthrough, no_link]),
    meck:expect(elib_param, post, fun(_Req) ->
        LongTitle = binary:copy(<<"测"/utf8>>, 100),
        #{<<"group_id">> => <<"M123">>,
          <<"title">> => LongTitle}
    end),
    meck:new(elib_response, [passthrough, no_link]),
    meck:expect(elib_response, error, fun(_Req, _Msg) -> cowboy_req_h:new() end),

    Req = cowboy_req_h:new(#{method => <<"POST">>}),
    State = #{current_uid => 123},

    Result = group_schedule_handler:create(Req, State),

    ?assert(cowboy_req_h:is_valid(Result)),
    meck:unload(elib_response),
    meck:unload(elib_param).
