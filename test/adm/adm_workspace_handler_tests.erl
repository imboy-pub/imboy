-module(adm_workspace_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% 双体验 v2.5.2 WP7/T11b — adm_workspace_handler EUnit 测试
%%%
%%% 重点：admin 端点 fail-open 是已知事故模式——本文件显式证明：
%%%   * 无 workspaces:read/update 权限的 admin 访问列表/详情/归档/恢复 → 403
%%%   * State 无 adm_user_id → 403
%%%   * 业务 logic 在拒绝路径零调用（meck:num_calls = 0）
%%%   * 授权路径调用透传（AdmUserId/WsId 正确下传，TSID 转 string）

-define(ADM_UID, 1001).
-define(NO_PERM_UID, 2001).
-define(WS_ID, 800001).
-define(PROJECT_ID, 700001).

%% 有权限角色（镜像 adm_acl_tests 的 MOCK_ROLE_ACL 模式）
-define(MOCK_ROLE_ACL,
    {adm_index_handler, [
        {'role_acl', 1, fun
            (1) -> {<<"super_admin">>, [<<"workspaces:read">>, <<"workspaces:update">>], []};
            (3) -> {<<"audit_admin">>, [], []};
            (_) -> {<<"none">>, [], []}
        end}
    ]}
).

-define(MOCK_PERM_FIND,
    {adm_user_logic, [
        {'find', 3, fun
            (?ADM_UID, <<"id,role_id">>, _Key) ->
                #{<<"id">> => ?ADM_UID, <<"role_id">> => [1]};
            (?NO_PERM_UID, <<"id,role_id">>, _Key) ->
                #{<<"id">> => ?NO_PERM_UID, <<"role_id">> => [3]}
        end}
    ]}
).

%% 跳过插件 feature gate（无插件注册时本就 undefined）
-define(MOCK_NO_PLUGIN,
    {imboy_plugin_registry, [
        {'required_feature', 3, fun(_Type, _Handler, _Action) -> undefined end}
    ]}
).

-define(MOCK_METHOD(M),
    {cowboy_req, [
        {'method', 1, fun(_Req) -> M end}
    ]}
).

-define(MOCK_RESP,
    {elib_response, [
        {'success', 2, fun(Req, Payload) -> Req#{response_status => 200, payload => Payload} end},
        {'success', 3, fun(Req, Payload, _Msg) ->
            Req#{response_status => 200, payload => Payload}
        end},
        {'error', 3, fun(Req, Msg, Code) -> Req#{response_status => Code, error_msg => Msg} end}
    ]}
).

%%% ===================================================================
%%% 鉴权（fail-closed：显式 403）
%%% ===================================================================

list_forbidden_without_permission_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_METHOD(<<"GET">>),
            ?MOCK_PERM_FIND,
            ?MOCK_ROLE_ACL,
            ?MOCK_NO_PLUGIN,
            ?MOCK_RESP,
            {workspace_logic, [
                {'admin_page', 4, fun(_P, _S, _St, _K) -> erlang:error(should_not_query) end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _} = adm_workspace_handler:init(
                #{}, #{action => list, adm_user_id => ?NO_PERM_UID}
            ),
            ?assertEqual(403, maps:get(response_status, RespReq)),
            ?assertEqual(0, meck:num_calls(workspace_logic, admin_page, 4))
        end
    ).

list_forbidden_without_adm_user_id_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_METHOD(<<"GET">>),
            ?MOCK_ROLE_ACL,
            ?MOCK_NO_PLUGIN,
            ?MOCK_RESP,
            {workspace_logic, [
                {'admin_page', 4, fun(_P, _S, _St, _K) -> erlang:error(should_not_query) end}
            ]}
        ],
        fun() ->
            %% State 无 adm_user_id（adm_acl 默认 0 → 无权限）
            {ok, RespReq, _} = adm_workspace_handler:init(#{}, #{action => list}),
            ?assertEqual(403, maps:get(response_status, RespReq))
        end
    ).

detail_forbidden_without_permission_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_METHOD(<<"GET">>),
            ?MOCK_PERM_FIND,
            ?MOCK_ROLE_ACL,
            ?MOCK_NO_PLUGIN,
            ?MOCK_RESP,
            {workspace_logic, [
                {'admin_detail', 1, fun(_Ws) -> erlang:error(should_not_query) end}
            ]},
            {elib_param, [
                {'binary', 3, fun(workspace_id, _Req, _D) -> {ok, <<"800001">>} end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _} = adm_workspace_handler:init(
                #{}, #{action => detail, adm_user_id => ?NO_PERM_UID}
            ),
            ?assertEqual(403, maps:get(response_status, RespReq)),
            ?assertEqual(0, meck:num_calls(workspace_logic, admin_detail, 1))
        end
    ).

archive_forbidden_without_update_permission_test_() ->
    %% 只读权限（workspaces:read）不能归档（需要 workspaces:update）
    ?WITH_MECKS(
        [
            ?MOCK_METHOD(<<"POST">>),
            {adm_user_logic, [
                {'find', 3, fun(?NO_PERM_UID, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => ?NO_PERM_UID, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"reader">>, [<<"workspaces:read">>], []}
                end}
            ]},
            ?MOCK_NO_PLUGIN,
            ?MOCK_RESP,
            {workspace_logic, [
                {'admin_archive', 2, fun(_A, _W) -> erlang:error(should_not_archive) end}
            ]},
            {elib_param, [
                {'binary', 3, fun(workspace_id, _Req, _D) -> {ok, <<"800001">>} end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _} = adm_workspace_handler:init(
                #{}, #{action => archive, adm_user_id => ?NO_PERM_UID}
            ),
            ?assertEqual(403, maps:get(response_status, RespReq)),
            ?assertEqual(0, meck:num_calls(workspace_logic, admin_archive, 2))
        end
    ).

%%% ===================================================================
%%% 授权路径
%%% ===================================================================

list_success_with_tsid_string_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_METHOD(<<"GET">>),
            ?MOCK_PERM_FIND,
            ?MOCK_ROLE_ACL,
            ?MOCK_NO_PLUGIN,
            ?MOCK_RESP,
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 10} end},
                {'binary', 3, fun
                    (status, _Req, _D) -> {ok, <<"all">>};
                    (keyword, _Req, _D) -> {ok, <<>>}
                end}
            ]},
            {workspace_logic, [
                {'admin_page', 4, fun(1, 10, <<"all">>, <<>>) ->
                    {ok, #{
                        list => [
                            #{
                                <<"id">> => ?WS_ID,
                                <<"name">> => <<"ops-ws">>,
                                <<"owner_id">> => 900001,
                                <<"status">> => <<"active">>,
                                <<"project_count">> => 3
                            }
                        ],
                        page => 1,
                        size => 10,
                        total => 1,
                        total_page => 1
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _} = adm_workspace_handler:init(
                #{}, #{action => list, adm_user_id => ?ADM_UID}
            ),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            [Row | _] = maps:get(list, Payload),
            %% TSID int → string（防 JS 精度丢失）
            ?assertEqual(<<"800001">>, maps:get(<<"id">>, Row)),
            ?assertEqual(<<"900001">>, maps:get(<<"owner_id">>, Row))
        end
    ).

archive_success_passes_adm_uid_and_audits_test_() ->
    [
        {"archive success passes adm uid and audits", fun() ->
            Self = self(),
            ?WITH_MECKS(
                [
                    ?MOCK_METHOD(<<"POST">>),
                    ?MOCK_PERM_FIND,
                    ?MOCK_ROLE_ACL,
                    ?MOCK_NO_PLUGIN,
                    ?MOCK_RESP,
                    {elib_param, [
                        {'binary', 3, fun(workspace_id, _Req, _D) -> {ok, <<"800001">>} end}
                    ]},
                    {workspace_logic, [
                        {'admin_archive', 2, fun(?ADM_UID, ?WS_ID) ->
                            Self ! {archive_called, ?ADM_UID, ?WS_ID},
                            {ok, #{
                                workspace_id => ?WS_ID,
                                status => <<"archived">>,
                                archived_by => ?ADM_UID
                            }}
                        end}
                    ]},
                    {adm_operation_log_ds, [
                        {'insert', 6, fun(AdmUid, Action, WsId, <<"workspace">>, _Detail, _Ip) ->
                            Self ! {audit, AdmUid, Action, WsId},
                            ok
                        end}
                    ]},
                    {elib_req, [
                        {'peer_ip', 1, fun(_Req) -> <<"127.0.0.1">> end}
                    ]}
                ],
                fun() ->
                    {ok, RespReq, _} = adm_workspace_handler:init(
                        #{}, #{action => archive, adm_user_id => ?ADM_UID}
                    ),
                    ?assertEqual(200, maps:get(response_status, RespReq)),
                    receive
                        {archive_called, ?ADM_UID, ?WS_ID} -> ok
                    after 500 -> ?assert(false, "admin_archive not called with adm uid")
                    end,
                    receive
                        {audit, ?ADM_UID, <<"workspace_archive">>, ?WS_ID} -> ok
                    after 500 -> ?assert(false, "governance audit not written")
                    end
                end
            )
        end}
    ].

detail_not_found_passthrough_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_METHOD(<<"GET">>),
            ?MOCK_PERM_FIND,
            ?MOCK_ROLE_ACL,
            ?MOCK_NO_PLUGIN,
            ?MOCK_RESP,
            {elib_param, [
                {'binary', 3, fun(workspace_id, _Req, _D) -> {ok, <<"800001">>} end}
            ]},
            {workspace_logic, [
                {'admin_detail', 1, fun(?WS_ID) -> {error, {404, <<"工作区不存在"/utf8>>}} end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _} = adm_workspace_handler:init(
                #{}, #{action => detail, adm_user_id => ?ADM_UID}
            ),
            ?assertEqual(404, maps:get(response_status, RespReq))
        end
    ).

project_detail_readonly_payload_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_METHOD(<<"GET">>),
            ?MOCK_PERM_FIND,
            ?MOCK_ROLE_ACL,
            ?MOCK_NO_PLUGIN,
            ?MOCK_RESP,
            {elib_param, [
                {'binary', 3, fun(project_id, _Req, _D) -> {ok, <<"700001">>} end}
            ]},
            {project_logic, [
                {'admin_detail', 1, fun(?PROJECT_ID) ->
                    {ok, #{
                        <<"id">> => ?PROJECT_ID,
                        <<"workspace_id">> => ?WS_ID,
                        <<"owner_id">> => 900001,
                        <<"name">> => <<"p1">>,
                        <<"status">> => <<"active">>,
                        workspace => #{<<"id">> => ?WS_ID, <<"name">> => <<"ops-ws">>},
                        owner => #{<<"id">> => 900001, <<"nickname">> => <<"alice">>},
                        task_stats => #{<<"todo">> => 2, <<"done">> => 1},
                        assignees => [
                            #{<<"assignee_id">> => 900001, <<"total">> => 3, <<"done">> => 1}
                        ]
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _} = adm_workspace_handler:init(
                #{}, #{action => project_detail, adm_user_id => ?ADM_UID}
            ),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(<<"700001">>, maps:get(<<"id">>, Payload)),
            ?assertEqual(<<"800001">>, maps:get(<<"workspace_id">>, Payload)),
            ?assertEqual(#{<<"todo">> => 2, <<"done">> => 1}, maps:get(task_stats, Payload)),
            %% assignee 概览的 TSID 同样转 string
            [A | _] = maps:get(assignees, Payload),
            ?assertEqual(<<"900001">>, maps:get(<<"assignee_id">>, A))
        end
    ).

invalid_workspace_id_rejected_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_METHOD(<<"POST">>),
            ?MOCK_PERM_FIND,
            ?MOCK_ROLE_ACL,
            ?MOCK_NO_PLUGIN,
            ?MOCK_RESP,
            {elib_param, [
                {'binary', 3, fun(workspace_id, _Req, _D) -> {ok, <<"abc">>} end}
            ]},
            {workspace_logic, [
                {'admin_archive', 2, fun(_A, _W) -> erlang:error(should_not_archive) end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _} = adm_workspace_handler:init(
                #{}, #{action => archive, adm_user_id => ?ADM_UID}
            ),
            ?assertEqual(?ERR_BAD_REQUEST, maps:get(response_status, RespReq))
        end
    ).
