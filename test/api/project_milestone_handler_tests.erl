-module(project_milestone_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% ZC-03 W2 Milestone — project_milestone_handler 直调测试（不起 HTTP，
%%% 路由由 ZC-05 统一注册后走 contract/consistency 套件）
%%%
%%% 覆盖：action 分派与参数解析（binding 缺省回退 post 字段、非法 id 400）、
%%% 字段白名单语义（create/update 携带 status 字段 400 且不触达 logic；
%%% 未知字段忽略）、错误码透传（403/980）、reach 幂等标志透出。

-define(UID, 930001).
-define(PROJECT_ID, 730001).
-define(MS_ID, 630001).

%%% ===================================================================
%%% create
%%% ===================================================================

create_success_test_() ->
    ?WITH_MECKS(
        create_mocks(#{<<"name">> => <<"M1">>, <<"due_date">> => <<"2026-09-30">>}), fun() ->
            Result = project_milestone_handler:handle_action(
                create, req_mock(), #{current_uid => ?UID}
            ),
            ?assertEqual({resp, success, ms_row()}, Result),
            receive
                {logic_create, Uid, Pid, Name, DueDate} ->
                    ?assertEqual(?UID, Uid),
                    ?assertEqual(?PROJECT_ID, Pid),
                    ?assertEqual(<<"M1">>, Name),
                    ?assertEqual(<<"2026-09-30">>, DueDate)
            after 500 ->
                ?assert(false, "logic create not called")
            end
        end
    ).

create_status_param_rejected_test_() ->
    ?WITH_MECKS(create_mocks(#{<<"name">> => <<"M1">>, <<"status">> => <<"reached">>}), fun() ->
        Result = project_milestone_handler:handle_action(
            create, req_mock(), #{current_uid => ?UID}
        ),
        ?assertEqual({resp, error, 400}, Result),
        ?assertEqual(0, meck:num_calls(project_milestone_logic, create, 4))
    end).

create_invalid_project_id_test_() ->
    ?WITH_MECKS(
        create_mocks(#{<<"name">> => <<"M1">>, <<"project_id">> => <<>>}),
        fun() ->
            Result = project_milestone_handler:handle_action(
                create, req_mock(), #{current_uid => ?UID}
            ),
            ?assertEqual({resp, error, 400}, Result),
            ?assertEqual(0, meck:num_calls(project_milestone_logic, create, 4))
        end
    ).

create_unknown_fields_ignored_test_() ->
    ?WITH_MECKS(
        create_mocks(#{
            <<"name">> => <<"M1">>,
            <<"gantt">> => [1, 2, 3],
            <<"progress">> => 99,
            <<"assignee_id">> => 1
        }),
        fun() ->
            ?assertEqual(
                {resp, success, ms_row()},
                project_milestone_handler:handle_action(create, req_mock(), #{current_uid => ?UID})
            )
        end
    ).

create_logic_error_passthrough_test_() ->
    ?WITH_MECKS(
        [
            {project_milestone_logic, [
                {'create', 4, fun(_Uid, _Pid, _Name, _Due) ->
                    {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}}
                end}
            ]}
        ] ++ base_mocks(#{<<"name">> => <<"M1">>}) ++ [resp_mocks(), auth_mock()],
        fun() ->
            Result = project_milestone_handler:handle_action(
                create, req_mock(), #{current_uid => ?UID}
            ),
            ?assertEqual({resp, error, 980}, Result)
        end
    ).

%%% ===================================================================
%%% list
%%% ===================================================================

list_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun
                    (project_id, _Req) -> ?PROJECT_ID;
                    (_, _Req) -> undefined
                end},
                {'parse_qs', 1, fun(_Req) -> [{<<"status">>, <<"planned">>}] end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"project_id">> => ?PROJECT_ID} end},
                {'page', 1, fun(_Req) -> {2, 20} end}
            ]},
            {project_milestone_logic, [
                {'list', 5, fun(Uid, Pid, Status, Page, Size) ->
                    self() ! {logic_list, Uid, Pid, Status, Page, Size},
                    {ok, #{list => [], page => Page, size => Size}}
                end}
            ]}
        ] ++ resp_mocks_list() ++ [auth_mock()],
        fun() ->
            Result = project_milestone_handler:handle_action(
                list, req_mock(), #{current_uid => ?UID}
            ),
            ?assertEqual(
                {resp, success, #{list => [], page => 2, size => 20}},
                Result
            ),
            receive
                {logic_list, Uid, Pid, Status, Page, Size} ->
                    ?assertEqual(?UID, Uid),
                    ?assertEqual(?PROJECT_ID, Pid),
                    ?assertEqual(<<"planned">>, Status),
                    ?assertEqual(2, Page),
                    ?assertEqual(20, Size)
            after 500 ->
                ?assert(false, "logic list not called")
            end
        end
    ).

%%% ===================================================================
%%% update / reach
%%% ===================================================================

update_success_test_() ->
    ?WITH_MECKS(update_mocks(#{<<"name">> => <<"M2">>, <<"due_date">> => null}), fun() ->
        Result = project_milestone_handler:handle_action(
            update, req_mock(), #{current_uid => ?UID}
        ),
        ?assertEqual({resp, success, ms_row()}, Result),
        receive
            {logic_update, Uid, MsId, Name, DueDate} ->
                ?assertEqual(?UID, Uid),
                ?assertEqual(?MS_ID, MsId),
                ?assertEqual(<<"M2">>, Name),
                ?assertEqual(null, DueDate)
        after 500 ->
            ?assert(false, "logic update not called")
        end
    end).

update_status_param_rejected_test_() ->
    ?WITH_MECKS(update_mocks(#{<<"name">> => <<"M2">>, <<"status">> => <<"planned">>}), fun() ->
        Result = project_milestone_handler:handle_action(
            update, req_mock(), #{current_uid => ?UID}
        ),
        ?assertEqual({resp, error, 400}, Result),
        ?assertEqual(0, meck:num_calls(project_milestone_logic, update, 4))
    end).

reach_success_test_() ->
    ?WITH_MECKS(reach_mocks(reached), fun() ->
        Result = project_milestone_handler:handle_action(
            reach, req_mock(), #{current_uid => ?UID}
        ),
        ?assertEqual({resp, success, ms_row(#{status_flag => reached})}, Result),
        receive
            {logic_reach, ?UID, ?MS_ID} -> ok
        after 500 ->
            ?assert(false, "logic reach not called")
        end
    end).

reach_error_passthrough_test_() ->
    ?WITH_MECKS(
        [
            {project_milestone_logic, [
                {'reach', 2, fun(_Uid, _MsId) ->
                    {error, {403, <<"仅项目成员可写里程碑"/utf8>>}}
                end}
            ]}
        ] ++ base_mocks(#{}) ++ resp_mocks_list() ++ [auth_mock()],
        fun() ->
            Result = project_milestone_handler:handle_action(
                reach, req_mock(), #{current_uid => ?UID}
            ),
            ?assertEqual({resp, error, 403}, Result)
        end
    ).

%%% ===================================================================
%%% mock 基建
%%% ===================================================================

req_mock() ->
    #{mock_req => true}.

ms_row() ->
    ms_row(#{}).

ms_row(Extra) ->
    maps:merge(
        #{<<"id">> => ?MS_ID, <<"name">> => <<"M1">>, <<"status">> => <<"planned">>},
        Extra
    ).

auth_mock() ->
    {auth_ds, [{'current_uid', 1, fun(_State) -> ?UID end}]}.

resp_mocks() ->
    {elib_response, [
        {'success', 1, fun(_Req) -> {resp, success, #{}} end},
        {'success', 2, fun(_Req, Payload) -> {resp, success, Payload} end},
        {'success', 3, fun(_Req, _Payload, _Msg) -> {resp, success, #{}} end},
        {'success', 4, fun(_Req, _Payload, _Msg, _Opts) -> {resp, success, #{}} end},
        {'error', 1, fun(_Req) -> {resp, error, 1} end},
        {'error', 2, fun(_Req, _Msg) -> {resp, error, 400} end},
        {'error', 3, fun(_Req, _Msg, Code) -> {resp, error, Code} end},
        {'error', 4, fun(_Req, _Msg, _Code, _Opts) -> {resp, error, 400} end}
    ]}.

resp_mocks_list() ->
    [resp_mocks()].

%% base：binding 恒 undefined（回退 post 字段）、post 参数可注入
base_mocks(PostVals) ->
    [
        {cowboy_req, [
            {'binding', 2, fun(_Name, _Req) -> undefined end},
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                maps:merge(
                    #{<<"milestone_id">> => ?MS_ID, <<"project_id">> => ?PROJECT_ID}, PostVals
                )
            end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {elib_cnv, [
            {'safe_to_integer', 1, fun
                (V) when is_integer(V) -> V;
                (V) when is_binary(V), V =/= <<>> ->
                    try
                        binary_to_integer(V)
                    catch
                        _:_ -> 0
                    end;
                (_) ->
                    0
            end}
        ]}
    ].

create_mocks(PostVals) ->
    logic_create_mocks() ++ base_mocks(PostVals) ++ [resp_mocks(), auth_mock()].

logic_create_mocks() ->
    [
        {project_milestone_logic, [
            {'create', 4, fun(Uid, Pid, Name, DueDate) ->
                self() ! {logic_create, Uid, Pid, Name, DueDate},
                {ok, ms_row()}
            end}
        ]}
    ].

update_mocks(PostVals) ->
    [
        {project_milestone_logic, [
            {'update', 4, fun(Uid, MsId, Name, DueDate) ->
                self() ! {logic_update, Uid, MsId, Name, DueDate},
                {ok, ms_row()}
            end}
        ]}
    ] ++ base_mocks(PostVals) ++ [resp_mocks(), auth_mock()].

reach_mocks(Flag) ->
    [
        {project_milestone_logic, [
            {'reach', 2, fun(Uid, MsId) ->
                self() ! {logic_reach, Uid, MsId},
                {ok, ms_row(#{status_flag => Flag}), Flag}
            end}
        ]}
    ] ++ base_mocks(#{}) ++ [resp_mocks(), auth_mock()].
