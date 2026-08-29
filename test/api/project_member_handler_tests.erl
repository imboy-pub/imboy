-module(project_member_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% W2 ZC-02 — project_member_handler 契约测试（直接模块调用，不起 HTTP）
%%% 覆盖：四个动作的参数解析（binding 优先/post 兜底）、成功/错误 envelope 映射、
%%% 非法 project id / 缺 user_id 400。路由未注册（ZC-05 统一注册），
%%% 本套件为 handler 单体契约，State 携带 current_uid 与线上 auth_middleware 一致。
%%%
%%% 结构说明：?WITH_MECK_TESTS 是本文件本地宏（{setup, S, C, [用例]} 规范
%%% context 结构）——"{Desc, fun() -> fixture end}" 包装式会让 EUnit 空转判 ok。

-define(PROJECT_ID, 730001).
-define(TARGET, 930002).
-define(UID, 930001).

%% 本地宏：一次 mock 安装 + 多个真实执行的内层用例（EUnit 规范 context）
-define(WITH_MECK_TESTS(MockConfigs, Tests),
    {setup,
        fun() ->
            lists:foreach(
                fun({Module, Expectations}) ->
                    case meck_helper:setup_mock(Module, Expectations) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            ?debugFmt("Mock setup failed for ~p: ~p", [Module, Reason])
                    end
                end,
                MockConfigs
            )
        end,
        fun(_) ->
            lists:foreach(
                fun({Module, _Expectations}) -> meck_helper:cleanup_mock(Module) end,
                MockConfigs
            )
        end,
        Tests}
).

%%% ===================================================================
%%% Mock 基建
%%% ===================================================================

%% BindingPid = ?PROJECT_ID（binding 命中）| undefined（走 post 兜底）
handler_mocks(BindingPid, PostVals) ->
    [
        {cowboy_req, [
            {'binding', 2, fun
                (project_id, _Req) -> BindingPid;
                (_, _) -> undefined
            end},
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> PostVals end},
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                #{response_status => 200, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) ->
                #{response_status => Code}
            end}
        ]},
        {project_member_logic, [
            {'list', 4, fun(Uid, Pid, Page, Size) ->
                self() ! {logic_list, Uid, Pid, Page, Size},
                {ok, #{list => [], page => Page, size => Size, total => 0, total_page => 0}}
            end},
            {'invite', 3, fun(Uid, Pid, Target) ->
                self() ! {logic_invite, Uid, Pid, Target},
                {ok, #{<<"user_id">> => Target}, created}
            end},
            {'remove', 3, fun(Uid, Pid, Target) ->
                self() ! {logic_remove, Uid, Pid, Target},
                {ok, #{user_id => Target, status => <<"removed">>}, removed}
            end},
            {'transfer_owner', 3, fun(Uid, Pid, Target) ->
                self() ! {logic_transfer, Uid, Pid, Target},
                {ok, #{owner_id => Target, previous_owner_id => Uid}}
            end}
        ]}
    ].

post_with_project_and_target() ->
    #{<<"project_id">> => integer_to_binary(?PROJECT_ID), <<"user_id">> => ?TARGET}.

post_with_target() ->
    #{<<"user_id">> => ?TARGET}.

post_empty() ->
    #{}.

%%% ===================================================================
%%% GET /api/v1/projects/:project_id/members → members
%%% ===================================================================

members_action_test_() ->
    ?WITH_MECK_TESTS(handler_mocks(?PROJECT_ID, post_empty()), [
        {"members list parses binding and delegates to logic", fun() ->
            Req = project_member_handler:handle_action(members, req0, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Req)),
            receive
                {logic_list, ?UID, ?PROJECT_ID, 1, 10} -> ok
            after 0 -> ?assert(false, "logic list not called")
            end
        end},
        {"members falls back to post project_id when binding missing", fun() ->
            %% 独立重装 binding=undefined 的 mock 组
            ok = meck_helper:cleanup_mock(cowboy_req),
            {ok, _} = meck_helper:setup_mock(cowboy_req, [
                {'binding', 2, fun(_, _) -> undefined end},
                {'parse_qs', 1, fun(_) -> [] end}
            ]),
            ok = meck_helper:cleanup_mock(elib_param),
            {ok, _} = meck_helper:setup_mock(elib_param, [
                {'post', 1, fun(_) -> post_with_project_and_target() end},
                {'page', 1, fun(_) -> {1, 10} end}
            ]),
            Req = project_member_handler:handle_action(members, req0, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Req)),
            receive
                {logic_list, ?UID, ?PROJECT_ID, 1, 10} -> ok
            after 0 -> ?assert(false, "logic list not called via post fallback")
            end
        end},
        {"members with invalid project id responds 400", fun() ->
            ok = meck_helper:cleanup_mock(cowboy_req),
            {ok, _} = meck_helper:setup_mock(cowboy_req, [
                {'binding', 2, fun(_, _) -> undefined end},
                {'parse_qs', 1, fun(_) -> [] end}
            ]),
            ok = meck_helper:cleanup_mock(elib_param),
            {ok, _} = meck_helper:setup_mock(elib_param, [
                {'post', 1, fun(_) -> post_empty() end},
                {'page', 1, fun(_) -> {1, 10} end}
            ]),
            Req = project_member_handler:handle_action(members, req0, #{current_uid => ?UID}),
            ?assertEqual(400, maps:get(response_status, Req)),
            receive
                {logic_list, _, _, _, _} -> ?assert(false, "must not reach logic")
            after 0 -> ok
            end
        end}
    ]).

%%% ===================================================================
%%% POST /api/v1/projects/:project_id/members/invite → invite
%%% ===================================================================

invite_action_test_() ->
    ?WITH_MECK_TESTS(handler_mocks(?PROJECT_ID, post_with_target()), [
        {"invite delegates target uid to logic", fun() ->
            Req = project_member_handler:handle_action(invite, req0, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Req)),
            receive
                {logic_invite, ?UID, ?PROJECT_ID, ?TARGET} -> ok
            after 0 -> ?assert(false, "logic invite not called")
            end
        end},
        {"invite maps logic 403 to error envelope", fun() ->
            ok = meck_helper:cleanup_mock(project_member_logic),
            {ok, _} = meck_helper:setup_mock(project_member_logic, [
                {'invite', 3, fun(_, _, _) -> {error, {403, <<"仅项目 Owner 可执行该操作"/utf8>>}} end}
            ]),
            Req = project_member_handler:handle_action(invite, req0, #{current_uid => ?UID}),
            ?assertEqual(403, maps:get(response_status, Req))
        end},
        {"invite without user_id responds 400", fun() ->
            ok = meck_helper:cleanup_mock(elib_param),
            {ok, _} = meck_helper:setup_mock(elib_param, [
                {'post', 1, fun(_) -> post_empty() end},
                {'page', 1, fun(_) -> {1, 10} end}
            ]),
            Req = project_member_handler:handle_action(invite, req0, #{current_uid => ?UID}),
            ?assertEqual(400, maps:get(response_status, Req)),
            receive
                {logic_invite, _, _, _} -> ?assert(false, "must not reach logic")
            after 0 -> ok
            end
        end}
    ]).

%%% ===================================================================
%%% POST /api/v1/projects/:project_id/members/remove → remove
%%% POST /api/v1/projects/:project_id/members/transfer_owner → transfer_owner
%%% ===================================================================

remove_and_transfer_action_test_() ->
    ?WITH_MECK_TESTS(handler_mocks(?PROJECT_ID, post_with_target()), [
        {"remove delegates target uid to logic", fun() ->
            Req = project_member_handler:handle_action(remove, req0, #{current_uid => ?UID}),
            ?assertEqual(200, maps:get(response_status, Req)),
            receive
                {logic_remove, ?UID, ?PROJECT_ID, ?TARGET} -> ok
            after 0 -> ?assert(false, "logic remove not called")
            end
        end},
        {"transfer_owner delegates target uid to logic", fun() ->
            Req = project_member_handler:handle_action(transfer_owner, req0, #{
                current_uid => ?UID
            }),
            ?assertEqual(200, maps:get(response_status, Req)),
            receive
                {logic_transfer, ?UID, ?PROJECT_ID, ?TARGET} -> ok
            after 0 -> ?assert(false, "logic transfer not called")
            end
        end},
        {"remove without user_id responds 400", fun() ->
            ok = meck_helper:cleanup_mock(elib_param),
            {ok, _} = meck_helper:setup_mock(elib_param, [
                {'post', 1, fun(_) -> post_empty() end},
                {'page', 1, fun(_) -> {1, 10} end}
            ]),
            Req = project_member_handler:handle_action(remove, req0, #{current_uid => ?UID}),
            ?assertEqual(400, maps:get(response_status, Req))
        end}
    ]).
