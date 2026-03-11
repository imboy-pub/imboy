-module(adm_index_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_index_handler 模块的 EUnit 测试
%%%
%%% 目标：验证管理后台首页处理器的路由分发和关键响应语义
%%% 策略：避免 meck 并发冲突，使用真实 cowboy_req 消息回传进行断言
%%%===================================================================

adm_index_handler_test_() ->
    {inorder, [
        module_loaded_case(),
        current_handler_requires_login_case(),
        current_handler_method_not_allowed_case(),
        index_handler_method_not_allowed_case(),
        welcome_handler_method_not_allowed_case(),
        rbac_handler_supports_multi_role_payload_case(),
        role_acl_contains_group_enhancement_permissions_case(),
        invalid_action_case()
    ]}.

%% ===================================================================
%% Internal helpers
%% ===================================================================

make_req(Method) ->
    StreamId = erlang:unique_integer([positive]),
    {StreamId, #{
        method => Method,
        pid => self(),
        streamid => StreamId
    }}.

flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 ->
        ok
    end.

recv_response(StreamId) ->
    receive
        {{_Pid, StreamId}, {response, Status, Headers, Body}} ->
            {Status, Headers, Body}
    after 1000 ->
        timeout
    end.

%% ===================================================================
%% Cases
%% ===================================================================

module_loaded_case() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(adm_index_handler),
        ?assertMatch({file, _}, code:is_loaded(adm_index_handler))
    end).

%% @doc current GET 未登录返回业务错误 706
current_handler_requires_login_case() ->
    ?TEST_SIMPLE(fun() ->
        flush_mailbox(),
        {StreamId, Req0} = make_req(<<"GET">>),
        {ok, _Req, _State} = adm_index_handler:init(Req0, #{action => current}),

        Resp = recv_response(StreamId),
        ?assertNotEqual(timeout, Resp),
        {StatusCode, _Headers, Body} = Resp,
        ?ASSERT_EQUAL(200, StatusCode),
        Decoded = jsone:decode(Body, [{object_format, map}]),
        ?ASSERT_EQUAL(706, maps:get(<<"code">>, Decoded))
    end).

%% @doc current 非 GET 返回 405
current_handler_method_not_allowed_case() ->
    ?TEST_SIMPLE(fun() ->
        flush_mailbox(),
        {StreamId, Req0} = make_req(<<"POST">>),
        {ok, _Req, _State} = adm_index_handler:init(Req0, #{action => current, adm_user_id => 7}),

        {StatusCode, _Headers, _Body} = recv_response(StreamId),
        ?ASSERT_EQUAL(405, StatusCode)
    end).

%% @doc index 非 GET 返回 405
index_handler_method_not_allowed_case() ->
    ?TEST_SIMPLE(fun() ->
        flush_mailbox(),
        {StreamId, Req0} = make_req(<<"POST">>),
        {ok, _Req, _State} = adm_index_handler:init(Req0, #{action => index}),

        {StatusCode, _Headers, _Body} = recv_response(StreamId),
        ?ASSERT_EQUAL(405, StatusCode)
    end).

%% @doc welcome 非 GET 返回 405
welcome_handler_method_not_allowed_case() ->
    ?TEST_SIMPLE(fun() ->
        flush_mailbox(),
        {StreamId, Req0} = make_req(<<"POST">>),
        {ok, _Req, _State} = adm_index_handler:init(Req0, #{action => welcome}),

        {StatusCode, _Headers, _Body} = recv_response(StreamId),
        ?ASSERT_EQUAL(405, StatusCode)
    end).

%% @doc rbac GET 在 role_id 为列表时应返回聚合权限
rbac_handler_supports_multi_role_payload_case() ->
    ?WITH_MECKS([
        {adm_user_logic, [
            {'find', 3, fun(7, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 7, <<"role_id">> => [2, 1]}
            end}
        ]}
    ], fun() ->
        flush_mailbox(),
        {StreamId, Req0} = make_req(<<"GET">>),
        {ok, _Req, _State} = adm_index_handler:init(Req0, #{action => rbac, adm_user_id => 7}),

        Resp = recv_response(StreamId),
        ?assertNotEqual(timeout, Resp),
        {StatusCode, _Headers, Body} = Resp,
        ?ASSERT_EQUAL(200, StatusCode),
        Decoded = jsone:decode(Body, [{object_format, map}]),
        Payload = maps:get(<<"payload">>, Decoded),

        RoleIds = maps:get(<<"role_ids">>, Payload, []),
        ?assert(lists:member(1, RoleIds)),
        ?assert(lists:member(2, RoleIds)),

        Permissions = maps:get(<<"permissions">>, Payload, []),
        ?assert(lists:member(<<"groups:notice:read">>, Permissions)),
        ?assert(lists:member(<<"groups:notice:delete">>, Permissions)),
        ?assert(lists:member(<<"groups:category:read">>, Permissions)),
        ?assert(lists:member(<<"groups:category:delete">>, Permissions)),
        ?assert(lists:member(<<"groups:tag:read">>, Permissions)),
        ?assert(lists:member(<<"groups:tag:delete">>, Permissions)),
        ?assert(lists:member(<<"groups:file:read">>, Permissions)),
        ?assert(lists:member(<<"groups:file:delete">>, Permissions)),
        ?assert(lists:member(<<"groups:album:read">>, Permissions)),
        ?assert(lists:member(<<"groups:album:delete">>, Permissions)),
        ?assert(lists:member(<<"groups:schedule:restore">>, Permissions)),
        ?assert(lists:member(<<"groups:task:restore">>, Permissions)),
        ?assert(lists:member(<<"groups:task:review">>, Permissions)),
        ?assert(lists:member(<<"groups:task:close">>, Permissions)),
        ?assert(lists:member(<<"groups:task:delete">>, Permissions)),
        ?assert(lists:member(<<"settings:ddl:delete">>, Permissions))
    end).

%% @doc role ACL 包含群增强治理权限键（role 1/2）
role_acl_contains_group_enhancement_permissions_case() ->
    ?TEST_SIMPLE(fun() ->
        {_RoleName1, Perms1, _MenuPaths1} = adm_index_handler:role_acl(1),
        {_RoleName2, Perms2, _MenuPaths2} = adm_index_handler:role_acl(2),
        Required = [
            <<"groups:vote:read">>,
            <<"groups:vote:close">>,
            <<"groups:notice:read">>,
            <<"groups:notice:delete">>,
            <<"groups:category:read">>,
            <<"groups:category:delete">>,
            <<"groups:tag:read">>,
            <<"groups:tag:delete">>,
            <<"groups:file:read">>,
            <<"groups:file:delete">>,
            <<"groups:album:read">>,
            <<"groups:album:delete">>,
            <<"groups:schedule:read">>,
            <<"groups:schedule:restore">>,
            <<"groups:schedule:cancel">>,
            <<"groups:task:read">>,
            <<"groups:task:restore">>,
            <<"groups:task:review">>,
            <<"groups:task:close">>,
            <<"groups:task:delete">>
        ],
        lists:foreach(fun(Permission) ->
            ?assert(lists:member(Permission, Perms1)),
            ?assert(lists:member(Permission, Perms2))
        end, Required)
    end).

%% @doc 无效 action 保持现有行为：抛出 case_clause
invalid_action_case() ->
    ?TEST_SIMPLE(fun() ->
        {_StreamId, Req0} = make_req(<<"GET">>),
        ?assertError({case_clause, invalid}, adm_index_handler:init(Req0, #{action => invalid}))
    end).
