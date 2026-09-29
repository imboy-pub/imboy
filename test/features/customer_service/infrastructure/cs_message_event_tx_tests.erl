%%% @doc REVIEW-3 F-2 真库套件：`message.appended` 事件行并入 enterprise
%%% canonical 事务（persist_hook 并轨写）。
%%%
%%% 覆盖三面：
%%%   * 正向：消息 + 事件行同事务落库，提交即同时可见（detail.message_id
%%%     与 canonical 消息 id 逐字一致）；
%%%   * 故障注入（meck 事件写失败）：消息/审计/事件**整体回滚零残留**，
%%%     client_msg_id 幂等键不消耗——同一键重试安全、无半态；
%%%   * 重放：同 client_msg_id 幂等重放不产生第二条事件行（与"重放不追加
%%%     第二条接受审计"同一裁决口径）。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID scope，零 PII。
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）：环境问题不得被当成 PASS。
-module(cs_message_event_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_message_event_tx_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun f2_message_and_event_commit_together/0},
        {timeout, 60, fun f2_event_write_failure_rolls_back_everything/0},
        {timeout, 60, fun f2_replay_does_not_duplicate_event/0}
    ];
cases({error, Reason}) ->
    erlang:error({cs_f2_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% 正向：消息与事件行同事务提交
%% ===================================================================

f2_message_and_event_commit_together() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = ws(Scope),
    try
        SessionId = open_claimed_session(Scope),
        MsgsBefore = ?FIX:count(Org, messages),
        EventsBefore = message_events(Org),
        ?assertEqual(0, MsgsBefore),
        ?assertEqual(0, EventsBefore),
        {ok, Accepted} = cs_session_app:append_session_message(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => maps:get(service_identity_id, Scope),
            actor_user_id => maps:get(actor_user_id, Scope),
            client_msg_id => <<"f2-ok-1">>,
            body => <<"seat outbound with event">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => 1700000001,
            notify => fun(_) -> ok end
        }),
        MessageId = maps:get(message_id, Accepted),
        %% 消息与事件行**同时**可见（同事务提交）。
        ?assertEqual(1, ?FIX:count(Org, messages)),
        ?assertEqual(1, message_events(Org)),
        %% 事件行携带 canonical 消息 id（同事务内拿到的 StoredMessage）。
        ?assertEqual(
            1,
            events_with_message_id(Org, <<"message.appended">>, MessageId)
        ),
        %% 接受审计照旧同事务（既有 A03 合同不回归）。
        ?assertEqual(1, accept_audits(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 故障注入：事件写失败 ⇒ 消息一并回滚（客户端重试安全）
%% ===================================================================

f2_event_write_failure_rolls_back_everything() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = ws(Scope),
    try
        SessionId = open_claimed_session(Scope),
        MsgsBefore = ?FIX:count(Org, messages),
        AuditsBefore = accept_audits(Org),
        EventsBefore = message_events(Org),
        %% 故障注入：store 端口注入确定性失败双（append_event_in 恒失败，
        %% 其余逐字委派真实现）——模拟 canonical 事务内事件行写失败。
        Result = cs_session_app:append_session_message(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => maps:get(service_identity_id, Scope),
            actor_user_id => maps:get(actor_user_id, Scope),
            client_msg_id => <<"f2-fail-1">>,
            body => <<"must roll back">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => 1700000002,
            notify => fun(_) -> ok end,
            store => cs_f2_fault_store
        }),
        %% 外部合同保持 audit_append_failed（cs_http 500 面）。
        ?assertEqual(
            {error, {audit_append_failed, {simulated, event_write_down}}},
            Result
        ),
        %% 零半态：消息 / 接受审计 / 事件行全部回滚。
        ?assertEqual(MsgsBefore, ?FIX:count(Org, messages)),
        ?assertEqual(AuditsBefore, accept_audits(Org)),
        ?assertEqual(EventsBefore, message_events(Org)),
        %% 幂等键未被失败消耗：同一 client_msg_id 换真 store 重试即成功，
        %% 且消息与事件同事务一并落库（无半态可遗留）。
        {ok, Accepted} = cs_session_app:append_session_message(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => maps:get(service_identity_id, Scope),
            actor_user_id => maps:get(actor_user_id, Scope),
            client_msg_id => <<"f2-fail-1">>,
            body => <<"must roll back">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => 1700000003,
            notify => fun(_) -> ok end
        }),
        ?assertEqual(false, maps:get(replayed, Accepted)),
        ?assertEqual(MsgsBefore + 1, ?FIX:count(Org, messages)),
        ?assertEqual(EventsBefore + 1, message_events(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 重放：不产生第二条事件行
%% ===================================================================

f2_replay_does_not_duplicate_event() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = ws(Scope),
    try
        SessionId = open_claimed_session(Scope),
        Params = #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => maps:get(service_identity_id, Scope),
            actor_user_id => maps:get(actor_user_id, Scope),
            client_msg_id => <<"f2-replay-1">>,
            body => <<"replay keeps one event">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => 1700000004,
            notify => fun(_) -> ok end
        },
        {ok, First} = cs_session_app:append_session_message(Org, Params),
        ?assertEqual(false, maps:get(replayed, First)),
        ?assertEqual(1, ?FIX:count(Org, messages)),
        ?assertEqual(1, message_events(Org)),
        {ok, Second} = cs_session_app:append_session_message(Org, Params),
        ?assertEqual(true, maps:get(replayed, Second)),
        %% 重放不增任何行：消息 / 审计 / 事件行（事件由原事务原子携带，
        %% persist_hook 在重放分支不触发）。
        ?assertEqual(1, ?FIX:count(Org, messages)),
        ?assertEqual(1, message_events(Org)),
        ?assertEqual(1, accept_audits(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

%% 开会话并 claim 成 active（坐席出站消息的前提，同 cs_pg_tests A04 口径）。
open_claimed_session(Scope) ->
    Org = org(Scope),
    Ws = ws(Scope),
    {ok, S} = cs_session_app:open_session(Org, #{
        workspace_id => Ws,
        contact_id => maps:get(contact_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        at => 1700000000,
        created_by_user_id => maps:get(peer_user_id, Scope)
    }),
    SessionId = maps:get(id, S),
    {ok, _} = cs_session_app:claim(Org, #{
        workspace_id => Ws,
        session_id => SessionId,
        business_identity_id => maps:get(service_identity_id, Scope),
        expected_version => 1,
        at => 1700000000
    }),
    SessionId.

message_events(Org) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) AS n FROM customer_service_event"
            " WHERE organization_id=$1 AND action='message.appended'"
        >>,
        [Org],
        -1
    ).

events_with_message_id(Org, Action, MessageId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) AS n FROM customer_service_event"
            " WHERE organization_id=$1 AND action=$2"
            "   AND detail->>'message_id' = $3::text"
        >>,
        [Org, Action, integer_to_binary(MessageId)],
        -1
    ).

accept_audits(Org) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) AS n FROM enterprise_audit_event"
            " WHERE organization_id=$1 AND action='message.accept'"
        >>,
        [Org],
        -1
    ).

org(Scope) -> maps:get(org_id, Scope).

ws(Scope) -> maps:get(workspace_id, Scope).
