-module(enterprise_message_id_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% Guard undeclared DB/entity calls in a fresh peer without application startup.
-define(PURE_MOCKS(Configs, Test),
    {timeout, 45, fun() ->
        pure_mock_fixture:run(fun() -> pure_mock_fixture:with_mocks(Configs, Test) end)
    end}
).

%% OA 业务 msg_id 与消息行 ID 分离，origin/审计/响应不得重新发号。
%% OA business IDs are UUIDv7; origin, audit, and response share the accepted ID.
oa_direct_and_group_preserve_business_id_test_() ->
    ?PURE_MOCKS(
        [
            {enterprise_message_repo, [
                {'find_active_principal_tx', 2, fun(_, 10) -> {ok, 10} end},
                {'insert_direct_tx', 6, fun(_, _, 10, 20, <<"text">>, _) -> {ok, 71} end},
                {'insert_group_tx', 6, fun(_, _, 10, 30, <<"text">>, _) -> {ok, 72} end},
                {'insert_audit_tx', 4, fun(_, _, _, _) -> {ok, 81} end}
            ]},
            {enterprise_external_identity_repo, [
                {'resolve_tx', 4, fun(_, 1, 2, [<<"recipient">>]) ->
                    {ok, [#{<<"user_id">> => 20}]}
                end}
            ]},
            {enterprise_org_member_repo, [
                {'find_membership_with_user_tx', 3, fun(_, 1, 20) ->
                    {ok, #{
                        <<"member_status">> => <<"active">>,
                        <<"account_type">> => 0,
                        <<"user_status">> => 1
                    }}
                end}
            ]},
            {enterprise_group_repo, [
                {'find_group_in_org_tx', 3, fun(_, 1, 30) ->
                    {ok, #{<<"workspace_id">> => 40}}
                end},
                {'find_workspace_in_org_tx', 3, fun(_, 1, 40) ->
                    {ok, #{<<"status">> => <<"active">>}}
                end}
            ]},
            {enterprise_message_origin_repo, [
                {'insert_tx', 8, fun(_, _, _, _, _, _, _, _) -> {ok, #{}} end}
            ]},
            {enterprise_webhook_logic, [
                {'emit_event_tx', 4, fun(_, _, _, _) -> ok end}
            ]},
            {enterprise_application_usage_repo, [
                {'bump_tx', 4, fun(_, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            Ctx = #{organization_id => 1, application_id => 2, principal_user_id => 10},
            Input = #{
                sender_mode => <<"application">>,
                msg_type => <<"text">>,
                content => <<"hello">>,
                recipient_user_id => <<"recipient">>
            },
            {ok, Direct} = enterprise_message_logic:direct_tx(test_conn, Ctx, Input),
            {ok, Group} = enterprise_message_logic:group_tx(test_conn, Ctx, 30, Input),
            DirectId = maps:get(<<"msg_id">>, Direct),
            GroupId = maps:get(<<"msg_id">>, Group),
            ?assertNotEqual(DirectId, GroupId),
            lists:foreach(
                fun({Result, Kind, RowId}) ->
                    Id = maps:get(<<"msg_id">>, Result),
                    ?assertEqual(36, byte_size(Id)),
                    ?assertMatch(
                        {match, _},
                        re:run(
                            Id,
                            <<"^[0-9a-f]{8}-[0-9a-f]{4}-7[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>
                        )
                    ),
                    ?assertEqual(RowId, maps:get(<<"webhook_resource_id">>, Result)),
                    ?assert(
                        meck:called(
                            enterprise_message_origin_repo,
                            insert_tx,
                            [test_conn, Kind, 1, 2, application, undefined, RowId, Id]
                        )
                    )
                end,
                [{Direct, direct, 71}, {Group, group, 72}]
            ),
            ?assert(
                meck:called(
                    enterprise_message_repo,
                    insert_direct_tx,
                    [test_conn, DirectId, 10, 20, <<"text">>, '_']
                )
            ),
            ?assert(
                meck:called(
                    enterprise_message_repo,
                    insert_group_tx,
                    [test_conn, GroupId, 10, 30, <<"text">>, '_']
                )
            ),
            AuditIds = [
                maps:get(<<"msg_id">>, Detail)
             || {_, {_, insert_audit_tx, [_, _, _, Detail]}, _} <-
                    meck:history(enterprise_message_repo)
            ],
            ?assertEqual([DirectId, GroupId], AuditIds)
        end
    ).

%% §3c 辅助函数下沉最小断言：Conn 透传 + 宽松降级 + null 语义
%%（handler 改道后原链路等价；mock 未命中子句兜底 {error, not_found}）。
with_principal_tx_passthrough_test_() ->
    ?PURE_MOCKS(
        [
            {enterprise_application_repo, [
                {'find_tx', 3, fun
                    (test_conn, 1, 2) -> {ok, #{<<"principal_user_id">> => 10}};
                    (_, _, _) -> {error, not_found}
                end}
            ]}
        ],
        fun() ->
            Ctx = #{organization_id => 1, application_id => 2},
            %% 命中：principal 注入
            ?assertEqual(
                #{organization_id => 1, application_id => 2, principal_user_id => 10},
                enterprise_message_logic:with_principal_tx(test_conn, Ctx)
            ),
            ?assert(meck:called(enterprise_application_repo, find_tx, [test_conn, 1, 2])),
            %% 失败宽松降级：返回原 Ctx 不注入
            ?assertEqual(
                #{organization_id => 9, application_id => 8},
                enterprise_message_logic:with_principal_tx(
                    test_conn, #{organization_id => 9, application_id => 8}
                )
            )
        end
    ).

fetch_row_id_tx_semantics_test_() ->
    ?PURE_MOCKS(
        [
            {enterprise_message_repo, [
                {'find_direct_tx', 2, fun
                    (test_conn, <<"m1">>) -> {ok, #{<<"id">> => 71}};
                    (_, _) -> {error, not_found}
                end},
                {'find_group_tx', 2, fun
                    (test_conn, <<"m2">>) -> {ok, #{<<"id">> => 72}};
                    (_, _) -> {error, not_found}
                end}
            ]}
        ],
        fun() ->
            %% c2c/c2g 命中 → 行 id；未命中 → null（不致命）
            ?assertEqual(
                71, enterprise_message_logic:fetch_row_id_tx(test_conn, <<"msg_c2c">>, <<"m1">>)
            ),
            ?assertEqual(
                72, enterprise_message_logic:fetch_row_id_tx(test_conn, <<"msg_c2g">>, <<"m2">>)
            ),
            ?assertEqual(
                null, enterprise_message_logic:fetch_row_id_tx(test_conn, <<"msg_c2c">>, <<"none">>)
            )
        end
    ).
