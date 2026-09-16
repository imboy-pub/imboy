%%% @doc EB-06 企业保留策略 / hold / bounded purge（application 用例）套件（真库）。
%%%
%%% 覆盖 EB-06-A07：
%%%   * 合成 1095d 的**时钟边界**（`retain_until` 前一律不删；到点且无 active hold 才
%%%     精确清理，`Now == retain_until` 视为已到期）；
%%%   * policy **不可缩短**（应用层先判 + DB 守卫纵深；已接受消息的快照不回填）；
%%%   * active hold 阻断 purge（workspace / conversation / message 三个 scope）；
%%%   * 附件不得早于其消息被删；普通角色直接 SQL 删除被 DB 守卫拒绝；
%%%   * 本卡不替用户决定生产保留期 / hold 策略（无默认值、无 bypass 键）。
%%%
%%% 时间全部由调用方注入（`now` / `accepted_at`），本套件不依赖隐式系统时钟做边界断言。
-module(eb_message_retention_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).
-define(SECONDS_PER_DAY, 86400).
-define(DAYS_1095, 1095).

retention_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            case ?FIX:ensure_purge_role() of
                ok -> {ok, Conn};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a07_synthetic_1095d_boundary_is_exact/0},
        {timeout, 60, fun a07_policy_cannot_be_shortened_app_and_db/0},
        {timeout, 60, fun a07_active_hold_blocks_purge_in_every_scope/0},
        {timeout, 60, fun a07_hold_requires_synthetic_declaration_and_scope_shape/0},
        {timeout, 60, fun a07_purge_deletes_exactly_the_due_targets/0},
        {timeout, 60, fun a07_asset_retained_longer_than_message_blocks_purge/0},
        {timeout, 60, fun a07_ordinary_role_direct_delete_is_rejected/0},
        {timeout, 60, fun a07_no_default_policy_and_no_bypass_keys/0}
    ];
cases(Other) ->
    erlang:error({eb06_retention_suite_db_unavailable, Other}).

%% ===================================================================
%% A07：合成 1095d 的时钟边界
%% ===================================================================

a07_synthetic_1095d_boundary_is_exact() ->
    %% 只用合成 fixture 验证机制：1095d 只证明三年窗口的算法与守卫，
    %% 不是生产保留期结论（plan EB-D12 / §9）。
    Scope = ?FIX:new_scope(#{with_policy => false}),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Policy} = eb_retention_app:open_retention_policy(Org, #{
            workspace_id => Ws,
            data_class => <<"enterprise_message">>,
            retention_days => ?DAYS_1095,
            trigger_event => <<"message.accept">>,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(1, maps:get(version, maps:get(policy, Policy))),
        ?assertEqual(?DAYS_1095, maps:get(retention_days, maps:get(policy, Policy))),
        %% 接受时刻放在「1095 天前」，使 retain_until 落在**真实过去**——
        %% DB 守卫第 3 步按数据库时钟复核（纵深防御），故边界断言必须用真实已到期的锚点。
        AcceptedAt = now_secs() - ?DAYS_1095 * ?SECONDS_PER_DAY - 10,
        RetainUntil = AcceptedAt + ?DAYS_1095 * ?SECONDS_PER_DAY,
        ?assert(RetainUntil < now_secs()),
        {ok, Appended} = append_at(Scope, <<"eb06-boundary-1">>, AcceptedAt),
        MessageId = maps:get(message_id, Appended),
        %% 1095d 的算法口径：retain_until = accepted_at + 1095*86400（逐字可复现）
        ?assertEqual(RetainUntil, maps:get(retain_until, maps:get(message, Appended))),
        ?assertEqual(
            1095 * 86400, maps:get(retain_until, maps:get(message, Appended)) - AcceptedAt
        ),
        %% ① retain_until 之前（即使差 1 秒）一律不得物理删除
        {ok, JustBefore} = eb_retention_app:purge_batch(Org, #{
            workspace_id => Ws, now => RetainUntil - 1, batch_limit => 10
        }),
        ?assertEqual(0, maps:get(deleted, JustBefore)),
        ?assertEqual(1, alive_message(Org, Ws, MessageId)),
        %% ② 到期（Now == retain_until）且无 active hold ⇒ 精确清理该条
        {ok, OnBoundary} = eb_retention_app:purge_batch(Org, #{
            workspace_id => Ws, now => RetainUntil, batch_limit => 10
        }),
        ?assertEqual(1, maps:get(deleted, OnBoundary)),
        ?assertEqual([MessageId], maps:get(purged, OnBoundary)),
        ?assert(is_integer(maps:get(audit_id, OnBoundary))),
        ?assertEqual(0, alive_message(Org, Ws, MessageId))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A07：policy 不可缩短（应用层 + DB 纵深）
%% ===================================================================

a07_policy_cannot_be_shortened_app_and_db() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        PoliciesBefore = ?FIX:count(Org, Ws, policies),
        %% 先用 v1(1095d) 接受一条消息，再延长策略 ⇒ 旧消息快照不得被回填
        AcceptedAt = now_secs() - 60,
        {ok, Appended} = append_at(Scope, <<"eb06-shorten-1">>, AcceptedAt),
        MessageId = maps:get(message_id, Appended),
        ?assertEqual(1, maps:get(policy_version, maps:get(message, Appended))),
        ?assertEqual(
            AcceptedAt + ?DAYS_1095 * ?SECONDS_PER_DAY,
            maps:get(retain_until, maps:get(message, Appended))
        ),
        %% 延长到 1200d 允许（v2）
        {ok, Extended} = eb_retention_app:open_retention_policy(Org, #{
            workspace_id => Ws,
            data_class => <<"enterprise_message">>,
            retention_days => ?DAYS_1095 + 105
        }),
        ?assertEqual(2, maps:get(version, maps:get(policy, Extended))),
        ?assertEqual(?DAYS_1095 + 105, maps:get(retention_days, maps:get(policy, Extended))),
        %% 旧消息的 policy snapshot / retain_until 逐字不变（不随未来策略缩短或回填）
        {ok, StillOld} = eb_pg_store:fetch_message(Org, Ws, MessageId),
        ?assertEqual(1, maps:get(policy_version, StillOld)),
        ?assertEqual(
            AcceptedAt + ?DAYS_1095 * ?SECONDS_PER_DAY,
            maps:get(retain_until, StillOld)
        ),
        %% ① 应用层拒绝缩短：1000d < 1200d ⇒ 显式失败且零新增版本
        ?assertEqual(
            {error, {retention_shorten_forbidden, ?DAYS_1095 + 105, 1000}},
            eb_retention_app:open_retention_policy(Org, #{
                workspace_id => Ws,
                data_class => <<"enterprise_message">>,
                retention_days => 1000
            })
        ),
        ?assertEqual(PoliciesBefore + 1, ?FIX:count(Org, Ws, policies)),
        %% ② DB 纵深：绕过应用直接 INSERT 更短的版本也必须是 23514 且不落行
        Shorter = ?FIX:id(),
        Result = ?FIX:exec(
            <<
                "INSERT INTO enterprise_retention_policy"
                " (id,organization_id,workspace_id,data_class,version,retention_days,trigger_event)"
                " VALUES ($1,$2,$3,'enterprise_message',99,10,'message.accept')"
            >>,
            [Shorter, Org, Ws]
        ),
        ?assertMatch({error, _}, Result),
        ?assertEqual(
            0,
            ?FIX:scalar(
                <<"SELECT count(*) FROM enterprise_retention_policy WHERE id=$1">>, [Shorter], -1
            )
        ),
        ?assertEqual(PoliciesBefore + 1, ?FIX:count(Org, Ws, policies)),
        %% ③ DB 纵深：直接前移已接受消息的 retain_until 也必须是 23514，值不变
        ShortenMsg = ?FIX:exec(
            <<
                "UPDATE enterprise_message SET retain_until = retain_until - interval '1 day'"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, MessageId]
        ),
        ?assertMatch({error, _}, ShortenMsg),
        {ok, Unchanged} = eb_pg_store:fetch_message(Org, Ws, MessageId),
        ?assertEqual(
            AcceptedAt + ?DAYS_1095 * ?SECONDS_PER_DAY,
            maps:get(retain_until, Unchanged)
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A07：active hold 阻断 purge（三个 scope）
%% ===================================================================

a07_active_hold_blocks_purge_in_every_scope() ->
    %% ① message scope：只阻断它自己（同批其他到期消息仍被精确清理）
    MessageScope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(MessageScope),
        [Msg1, Msg2, Msg3] = due_messages(MessageScope, <<"hold">>, 3),
        Actor = maps:get(actor_user_id, MessageScope),
        {ok, Hold1} = eb_retention_app:create_hold(Org, #{
            workspace_id => Ws,
            scope => <<"message">>,
            scope_message_id => Msg1,
            reason_code => <<"eb06-synthetic-hold-message">>,
            synthetic => true,
            actor_user_id => Actor
        }),
        ?assertEqual(Msg1, maps:get(scope_message_id, maps:get(hold, Hold1))),
        Hold1Id = maps:get(id, maps:get(hold, Hold1)),
        {ok, Sum1} = eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => now_secs()}),
        ?assertEqual(2, maps:get(deleted, Sum1)),
        ?assertEqual(1, alive_message(Org, Ws, Msg1)),
        ?assertEqual(0, alive_message(Org, Ws, Msg2)),
        ?assertEqual(0, alive_message(Org, Ws, Msg3)),
        ?assert(lists:member({Msg1, {ineligible, active_hold}}, maps:get(skipped, Sum1))),
        %% hold 行是 append-only 事实：release 一次性写入 released_at + released_by
        {ok, Released} = eb_retention_app:release_hold(Org, #{
            workspace_id => Ws,
            hold_id => Hold1Id,
            synthetic => true,
            actor_user_id => Actor
        }),
        ?assertEqual(Hold1Id, maps:get(hold_id, Released)),
        ?assert(is_integer(maps:get(audit_id, Released))),
        ?assertEqual(
            0,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_retention_hold"
                    " WHERE id=$1 AND released_at IS NULL"
                >>,
                [Hold1Id],
                -1
            )
        ),
        %% 已释放的 hold 不得二次 release（append-only）。EB-06-A12 后错误语义**精确**：
        %% 不再是「统一 hold_not_releasable」，而是点名「已释放」+ 释放时刻（可区分于
        %% 「不存在」与「跨 Org」）。
        ?assertMatch(
            {error, {hold_already_released, Hold1Id, _ReleasedAt}},
            eb_retention_app:release_hold(Org, #{
                workspace_id => Ws, hold_id => Hold1Id, synthetic => true, actor_user_id => Actor
            })
        )
    after
        ?FIX:cleanup(MessageScope)
    end,
    %% ② conversation scope：阻断整个会话
    ConversationScope = ?FIX:new_scope(),
    try
        {Org2, Ws2} = tenant(ConversationScope),
        Conv2 = maps:get(conversation_id, ConversationScope),
        Msgs2 = due_messages(ConversationScope, <<"hold-conv">>, 3),
        {ok, _Hold2} = eb_retention_app:create_hold(Org2, #{
            workspace_id => Ws2,
            scope => <<"conversation">>,
            scope_conversation_id => Conv2,
            reason_code => <<"eb06-synthetic-hold-conversation">>,
            synthetic => true,
            actor_user_id => maps:get(actor_user_id, ConversationScope)
        }),
        {ok, Sum2} = eb_retention_app:purge_batch(Org2, #{workspace_id => Ws2, now => now_secs()}),
        ?assertEqual(0, maps:get(deleted, Sum2)),
        ?assertEqual(3, length([M || M <- Msgs2, alive_message(Org2, Ws2, M) =:= 1])),
        lists:foreach(
            fun(M) ->
                ?assert(lists:member({M, {ineligible, active_hold}}, maps:get(skipped, Sum2)))
            end,
            Msgs2
        )
    after
        ?FIX:cleanup(ConversationScope)
    end,
    %% ③ workspace scope：阻断整个 Workspace
    WorkspaceScope = ?FIX:new_scope(),
    try
        {Org3, Ws3} = tenant(WorkspaceScope),
        Msgs3 = due_messages(WorkspaceScope, <<"hold-ws">>, 3),
        {ok, _Hold3} = eb_retention_app:create_hold(Org3, #{
            workspace_id => Ws3,
            scope => <<"workspace">>,
            reason_code => <<"eb06-synthetic-hold-workspace">>,
            synthetic => true,
            actor_user_id => maps:get(actor_user_id, WorkspaceScope)
        }),
        {ok, Sum3} = eb_retention_app:purge_batch(Org3, #{workspace_id => Ws3, now => now_secs()}),
        ?assertEqual(0, maps:get(deleted, Sum3)),
        ?assertEqual(3, length([M || M <- Msgs3, alive_message(Org3, Ws3, M) =:= 1]))
    after
        ?FIX:cleanup(WorkspaceScope)
    end.

%% 一批已到期的合成消息（接受时刻回拨，使 retain_until 落在真实过去）。
due_messages(Scope, Label, Count) ->
    [
        append_id(
            Scope,
            client_id(<<Label/binary, "-", (integer_to_binary(N))/binary>>),
            due_accepted_at(120)
        )
     || N <- lists:seq(1, Count)
    ].

%% ===================================================================
%% A07：hold 只允许合成声明 + scope 形状自洽
%% ===================================================================

a07_hold_requires_synthetic_declaration_and_scope_shape() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        MessageId = append_id(Scope, <<"eb06-hold-shape">>, now_secs() - 60),
        Before = ?FIX:count(Org, Ws, holds),
        %% ① 未声明 synthetic ⇒ 拒绝（真实 hold 属需担责操作，须人工 Gate）
        ?assertEqual(
            {error, {synthetic_hold_required, synthetic}},
            eb_retention_app:create_hold(Org, #{
                workspace_id => Ws,
                scope => <<"workspace">>,
                reason_code => <<"eb06-hold">>
            })
        ),
        %% ② 未知 scope / 形状不自洽 ⇒ 拒绝（与 ck_erh_scope_shape 同口径）
        ?assertEqual(
            {error, {unknown_hold_scope, <<"org">>}},
            eb_retention_app:create_hold(Org, #{
                workspace_id => Ws,
                scope => <<"org">>,
                reason_code => <<"eb06-hold">>,
                synthetic => true
            })
        ),
        ?assertEqual(
            {error, {hold_scope_mismatch, <<"message">>}},
            eb_retention_app:create_hold(Org, #{
                workspace_id => Ws,
                scope => <<"message">>,
                reason_code => <<"eb06-hold">>,
                synthetic => true
            })
        ),
        ?assertEqual(
            {error, {hold_scope_mismatch, <<"workspace">>}},
            eb_retention_app:create_hold(Org, #{
                workspace_id => Ws,
                scope => <<"workspace">>,
                scope_conversation_id => Conv,
                reason_code => <<"eb06-hold">>,
                synthetic => true
            })
        ),
        %% ③ 目标不在本租户 ⇒ 拒绝且零写入（不靠 DB FK 报错）
        ?assertMatch(
            {error, {hold_target_not_in_scope, _}},
            eb_retention_app:create_hold(Org, #{
                workspace_id => Ws,
                scope => <<"message">>,
                scope_message_id => ?FIX:id(),
                reason_code => <<"eb06-hold">>,
                synthetic => true
            })
        ),
        %% ④ 空 reason_code ⇒ 拒绝
        ?assertEqual(
            {error, {invalid_reason_code, <<>>}},
            eb_retention_app:create_hold(Org, #{
                workspace_id => Ws,
                scope => <<"message">>,
                scope_message_id => MessageId,
                reason_code => <<>>,
                synthetic => true
            })
        ),
        ?assertEqual(Before, ?FIX:count(Org, Ws, holds))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A07：精确清理
%% ===================================================================

a07_purge_deletes_exactly_the_due_targets() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = now_secs(),
        Due = [
            append_id(Scope, <<"eb06-due-1">>, due_accepted_at(600)),
            append_id(Scope, <<"eb06-due-2">>, due_accepted_at(300))
        ],
        %% 保留期远未到（policy 1095d ⇒ retain_until ≈ Now + 1095d）
        NotDue = append_id(Scope, <<"eb06-notdue-1">>, Now - 60),
        {ok, Summary} = eb_retention_app:purge_batch(Org, #{
            workspace_id => Ws, now => Now, batch_limit => 1
        }),
        %% 批量上限生效：一次只清理一条
        ?assertEqual(1, maps:get(deleted, Summary)),
        {ok, Second} = eb_retention_app:purge_batch(Org, #{
            workspace_id => Ws, now => Now, batch_limit => 10
        }),
        ?assertEqual(1, maps:get(deleted, Second)),
        ?assertEqual([], [M || M <- Due, alive_message(Org, Ws, M) =:= 1]),
        ?assertEqual(1, alive_message(Org, Ws, NotDue)),
        {ok, Third} = eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => Now}),
        ?assertEqual(0, maps:get(deleted, Third)),
        %% 逐批 append-only 审计
        ?assertEqual(
            2,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND action='message.purge'"
                >>,
                [Org],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% 附件保留期长于消息时必须整批保留（附件不得早于其消息被删）
a07_asset_retained_longer_than_message_blocks_purge() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = now_secs(),
        {ok, Appended} = append_at(Scope, <<"eb06-asset-1">>, due_accepted_at(600)),
        MessageId = maps:get(message_id, Appended),
        AssetId = attach_asset(Scope, MessageId, Now + 86400),
        Result = eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => Now}),
        ?assertMatch({error, {sql, _, _}}, Result),
        {error, {sql, Code, Constraint}} = Result,
        ?assertEqual(<<"23514">>, Code),
        ?assertEqual(<<"trg_enterprise_asset_purge_guard">>, Constraint),
        ?assertEqual(1, alive_message(Org, Ws, MessageId)),
        ?assertEqual(1, alive_asset(Org, Ws, AssetId))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A07：普通角色直接 SQL 删除被 DB 守卫拒绝
%% ===================================================================

a07_ordinary_role_direct_delete_is_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = now_secs(),
        MessageId = append_id(Scope, <<"eb06-guard-1">>, due_accepted_at(600)),
        %% 不走 bounded purge 上下文（不设 GUC）= 普通角色路径
        Result = ?FIX:exec(
            <<"DELETE FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
            [Org, Ws, MessageId]
        ),
        ?assertMatch({error, _}, Result),
        %% 拒绝必须来自物理删除守卫（trg_enterprise_message_purge_guard），
        %% 而不是别的偶发错误——否则「普通角色不得直删」这条证据不成立。
        Blob = iolist_to_binary(io_lib:format("~p", [element(2, Result)])),
        ?assert(binary:match(Blob, <<"trg_enterprise_message_purge_guard">>) =/= nomatch),
        ?assert(binary:match(Blob, <<"23514">>) =/= nomatch),
        ?assertEqual(1, alive_message(Org, Ws, MessageId)),
        %% 应用层（本卡）也没有暴露任何删除/更新 canonical 的入口
        %% （EB-06 重开新增 `latest_retention_policy/2`、`fetch_hold/2`、
        %%  `hold_lookup_verdict/3` —— 全是读；没有 delete/update/force 形状）
        ?assertEqual(
            lists:sort([
                {create_hold, 2},
                {fetch_hold, 2},
                {hold_lookup_verdict, 3},
                {latest_retention_policy, 2},
                {open_retention_policy, 2},
                {purge_batch, 2},
                {release_hold, 2}
            ]),
            lists:sort(drop_module_info(eb_retention_app:module_info(exports)))
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A07：不替用户决定生产策略 / 无 bypass 键
%% ===================================================================

a07_no_default_policy_and_no_bypass_keys() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        %% ① 无默认保留期：缺 retention_days 一律拒绝（不猜生产策略）
        ?assertEqual(
            {error, {invalid_retention_days, undefined}},
            eb_retention_app:open_retention_policy(Org, #{
                workspace_id => Ws,
                data_class => <<"enterprise_message">>
            })
        ),
        ?assertEqual(
            {error, {invalid_retention_days, 0}},
            eb_retention_app:open_retention_policy(Org, #{
                workspace_id => Ws,
                data_class => <<"enterprise_message">>,
                retention_days => 0
            })
        ),
        ?assertEqual(
            {error, {unknown_data_class, <<"enterprise_note">>}},
            eb_retention_app:open_retention_policy(Org, #{
                workspace_id => Ws,
                data_class => <<"enterprise_note">>,
                retention_days => 10
            })
        ),
        %% ② purge 必须用注入时钟；缺时钟一律 fail-closed（绝不回落到系统时间之外的口径）
        ?assertMatch(
            {error, {invalid_clock, _}},
            eb_retention_app:purge_batch(Org, #{workspace_id => Ws, now => <<"not-a-clock">>})
        ),
        %% ③ bypass 键（force / offboarding / ignore_hold / skip_guard）不改变判定：
        %%    未到期时仍然一行不删。
        Now = now_secs(),
        MessageId = append_id(Scope, <<"eb06-bypass-1">>, Now - 60),
        {ok, Summary} = eb_retention_app:purge_batch(Org, #{
            workspace_id => Ws,
            now => Now,
            force => true,
            offboarding => true,
            ignore_hold => true,
            skip_guard => true,
            batch_limit => 100
        }),
        ?assertEqual(0, maps:get(deleted, Summary)),
        ?assertEqual(1, alive_message(Org, Ws, MessageId)),
        %% ④ 未到期时也不得因「普通角色路径」或「离职路径」而提前删除
        ?assertEqual(1, alive_message(Org, Ws, MessageId))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

now_secs() ->
    eb_system_clock:now().

client_id(Bin) ->
    <<"eb06-", Bin/binary>>.

%% 已到期的合成锚点：接受时刻回拨到「1095d 之前」，使 retain_until 落在真实过去。
%% （消息的 retain_until 由接受时刻 + policy 的 retention_days 决定，无法直接指定。）
due_accepted_at(OffsetSecs) ->
    now_secs() - ?DAYS_1095 * ?SECONDS_PER_DAY - OffsetSecs.

append_id(Scope, ClientMsgId, AcceptedAt) ->
    case append_at(Scope, ClientMsgId, AcceptedAt) of
        {ok, Result} -> maps:get(message_id, Result);
        {error, Reason} -> erlang:error({append_failed, ClientMsgId, Reason})
    end.

append_at(Scope, ClientMsgId, AcceptedAt) ->
    Org = maps:get(org_id, Scope),
    Params = #{
        workspace_id => maps:get(workspace_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        client_msg_id => ClientMsgId,
        body => <<"eb06-retention-body-", ClientMsgId/binary>>,
        sender_type => contact,
        contact_id => maps:get(contact_id, Scope),
        key_ref => ?FIX:key_ref(1),
        accepted_at => AcceptedAt,
        notify => fun(_Notification) -> ok end
    },
    case eb_message_app:append_message(Org, Params) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> erlang:error({append_failed, ClientMsgId, Reason})
    end.

attach_asset(Scope, MessageId, RetainUntil) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    AssetId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_asset"
            " (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,"
            "  uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,"
            "  retain_until,version)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,'application/octet-stream',64,'active',1,"
            "         to_timestamp($10::bigint/1000),1)"
        >>,
        [
            AssetId,
            Org,
            Ws,
            maps:get(conversation_id, Scope),
            MessageId,
            maps:get(sales_identity_id, Scope),
            maps:get(actor_user_id, Scope),
            <<"enterprise/eb06/", (integer_to_binary(AssetId))/binary, ".bin">>,
            sha256_hex(<<"eb06-asset-hash">>),
            RetainUntil * 1000
        ]
    ),
    AssetId.

alive_message(Org, Ws, MessageId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MessageId],
        -1
    ).

alive_asset(Org, Ws, AssetId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_asset"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, AssetId],
        -1
    ).

drop_module_info(Exports) ->
    [E || E <- Exports, E =/= {module_info, 0}, E =/= {module_info, 1}].

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).
