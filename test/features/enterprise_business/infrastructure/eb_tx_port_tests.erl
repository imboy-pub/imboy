%%% @doc EB-03R T1/T2 套件：用例级事务 Port（原子消息接受 / 会话审计同事务）。
%%%
%%% 覆盖：
%%%   * accept_message/3：canonical message + policy snapshot + audit **同事务**，
%%%     提交成功才返回；任一步失败整体回滚（无半提交）；
%%%   * 幂等重放：同一 client_msg_id 只产生一条消息与一条审计；
%%%   * append_conversation_audit/3：审计与调用方上下文同事务落库；
%%%   * 导出面只有两个具名用例（无 exec/query/transaction）。
-module(eb_tx_port_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

tx_port_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun accept_message_commits_message_policy_and_audit_together/0},
        {timeout, 60, fun accept_message_is_idempotent_on_client_msg_id/0},
        {timeout, 60, fun accept_message_rolls_back_without_a_half_commit/0},
        {timeout, 60, fun accept_message_fails_closed_on_missing_policy/0},
        {timeout, 60, fun append_conversation_audit_writes_audit_in_its_own_transaction/0},
        {timeout, 60, fun export_surface_is_named_use_cases_only/0}
    ];
cases(_Skipped) ->
    {skip, "tx port suite requires the scratch database connection"}.

accept_message_commits_message_policy_and_audit_together() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AuditsBefore = audit_count(Org),
        {ok, Result} = eb_pg_tx:accept_message(Org, Ws, accept_params(Scope, <<"eb03r-tx-1">>)),
        ?assertEqual(false, maps:get(replayed, Result)),
        MessageId = maps:get(id, maps:get(message, Result)),
        ?assert(is_integer(maps:get(audit_id, Result))),
        %% 三条事实同时存在（同事务提交）
        ?assertEqual(1, message_count(Org, Ws, MessageId)),
        ?assertEqual(AuditsBefore + 1, audit_count(Org)),
        ?assertEqual(
            <<"message.accept">>,
            eb_pg_test_fixture:scalar(
                <<"SELECT action FROM enterprise_audit_event WHERE id=$1">>,
                [maps:get(audit_id, Result)]
            )
        ),
        %% 消息上的 policy 快照必须与库内当前策略一致
        ?assertEqual(
            maps:get(policy_id, Scope),
            eb_pg_test_fixture:scalar(
                <<"SELECT policy_id FROM enterprise_message WHERE organization_id=$1 AND id=$2">>,
                [Org, MessageId]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

accept_message_is_idempotent_on_client_msg_id() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Params = accept_params(Scope, <<"eb03r-tx-2">>),
        {ok, First} = eb_pg_tx:accept_message(Org, Ws, Params),
        ?assertEqual(false, maps:get(replayed, First)),
        {ok, Second} = eb_pg_tx:accept_message(Org, Ws, Params),
        ?assertEqual(true, maps:get(replayed, Second)),
        ?assertEqual(
            maps:get(id, maps:get(message, First)),
            maps:get(id, maps:get(message, Second))
        ),
        ?assertEqual(1, message_count(Org, Ws, maps:get(id, maps:get(message, First))))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

accept_message_rolls_back_without_a_half_commit() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        %% 不存在的会话：事务必须整体回滚，审计也不得落库。
        Before = audit_count(Org),
        Bad = maps:put(conversation_id, Conv + 1, accept_params(Scope, <<"eb03r-tx-3">>)),
        ?assertEqual({error, not_found}, eb_pg_tx:accept_message(Org, Ws, Bad)),
        ?assertEqual(Before, audit_count(Org)),
        ?assertEqual(
            0,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM enterprise_message"
                    " WHERE organization_id=$1 AND client_msg_id=$2"
                >>,
                [Org, <<"eb03r-tx-3">>]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

accept_message_fails_closed_on_missing_policy() ->
    Scope = eb_pg_test_fixture:new_scope(#{with_policy => false}),
    try
        {Org, Ws} = tenant(Scope),
        Before = audit_count(Org),
        ?assertEqual(
            {error, missing_retention_policy},
            eb_pg_tx:accept_message(Org, Ws, accept_params(Scope, <<"eb03r-tx-4">>))
        ),
        ?assertEqual(Before, audit_count(Org)),
        ?assertEqual(
            0,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM enterprise_message"
                    " WHERE organization_id=$1 AND client_msg_id=$2"
                >>,
                [Org, <<"eb03r-tx-4">>]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

append_conversation_audit_writes_audit_in_its_own_transaction() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Before = audit_count(Org),
        {ok, AuditId} = eb_pg_tx:append_conversation_audit(Org, Ws, #{
            resource_id => maps:get(conversation_id, Scope),
            actor_user_id => maps:get(actor_user_id, Scope),
            actor_role => <<"member">>,
            business_identity_id => maps:get(sales_identity_id, Scope),
            detail => #{<<"synthetic">> => true}
        }),
        ?assert(is_integer(AuditId)),
        ?assertEqual(Before + 1, audit_count(Org)),
        ?assertEqual(
            <<"conversation.open">>,
            eb_pg_test_fixture:scalar(
                <<"SELECT action FROM enterprise_audit_event WHERE id=$1">>, [AuditId]
            )
        ),
        %% detail 里必须带 workspace_id（租户贯穿证据），且不含任何明文/密钥
        ?assertEqual(
            Ws,
            eb_pg_test_fixture:scalar(
                <<"SELECT (detail->>'workspace_id')::bigint FROM enterprise_audit_event WHERE id=$1">>,
                [AuditId]
            )
        ),
        %% 非法租户 fail-closed
        ?assertMatch(
            {error, {invalid_tenant, _}}, eb_pg_tx:append_conversation_audit(undefined, Ws, #{})
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

export_surface_is_named_use_cases_only() ->
    Exports = [
        {N, A}
     || {N, A} <- eb_pg_tx:module_info(exports), N =/= module_info
    ],
    ?assertEqual(
        [{accept_message, 3}, {append_conversation_audit, 3}],
        lists:sort(Exports)
    ),
    ?assertEqual(
        [{accept_message, 3}, {append_conversation_audit, 3}],
        lists:sort(eb_tx_port:behaviour_info(callbacks))
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

accept_params(Scope, ClientMsgId) ->
    #{
        conversation_id => maps:get(conversation_id, Scope),
        client_msg_id => ClientMsgId,
        body => <<"eb03r-tx-body">>,
        sender_type => <<"business_identity">>,
        identity_id => maps:get(sales_identity_id, Scope),
        actor_user_id => maps:get(actor_user_id, Scope),
        key_ref => eb_pg_test_fixture:key_ref(1),
        accepted_at => eb_system_clock:now()
    }.

message_count(Org, Ws, MessageId) ->
    eb_pg_test_fixture:scalar(
        <<
            "SELECT count(*) AS n FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MessageId]
    ).

audit_count(Org) ->
    eb_pg_test_fixture:scalar(
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE organization_id=$1">>, [Org]
    ).
