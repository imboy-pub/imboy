%%% @doc EB-06 企业消息（application 用例）套件（真库）。
%%%
%%% 覆盖：
%%%   EB-06-A01 无 consent 不写消息（负例断 canonical 行数增量 0，且**忽略调用方的
%%%             enforce_consent=false 旁路**）；
%%%   EB-06-A03 同 client_msg_id 重放不增行、不增审计（且不重复通知）；
%%%   EB-06-A04 两类 sender XOR/FK + 历史 actor 可追溯；
%%%   EB-06-A05 realtime 失败**不回滚**已提交真源，且通知只含 resource id；
%%%   EB-06-A06 delivery ACK 幂等，ACK/隐藏/授权事件前后 canonical 行数与关键字段
%%%             hash 不变；**禁止复用个人 CLIENT_ACK / msg_archive 清理链**；
%%%   EB-06-A08 canonical/audit 提交失败时无 accepted、无 realtime、无输入丢失，
%%%             恢复重试**恰好一条**。
-module(eb_message_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).
-define(CANONICAL_HASH_FIELDS, [
    organization_id,
    workspace_id,
    conversation_id,
    sender_type,
    sender_contact_id,
    sender_business_identity_id,
    actor_user_id,
    content_hash,
    policy_id,
    policy_version,
    retain_until
]).

%% 断言「发布器不该被调用时一次也没被调用」（realtime 不得在失败/重放路径上发出）。
-define(assertNoPublish(),
    receive
        {published, N} -> erlang:error({unexpected_publish, N});
        {unexpected_publish, N} -> erlang:error({unexpected_publish, N})
    after 300 -> ok
    end
).

message_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_no_consent_writes_no_message_even_with_bypass_flag/0},
        {timeout, 60, fun a03_replay_adds_no_row_and_no_audit/0},
        {timeout, 60, fun a04_sender_xor_and_composite_fk_are_enforced/0},
        {timeout, 60, fun a04_history_actor_is_traceable/0},
        {timeout, 60, fun a05_realtime_failure_does_not_roll_back_committed_truth/0},
        {timeout, 60, fun a05_notification_carries_only_resource_ids_after_commit/0},
        {timeout, 60, fun a06_ack_is_idempotent_and_leaves_canonical_untouched/0},
        {timeout, 60, fun a06_hide_and_authorization_events_do_not_change_canonical/0},
        {timeout, 60, fun a06_no_personal_ack_or_archive_chain_is_used/0},
        {timeout, 60, fun a08_commit_failure_yields_no_acceptance_and_retry_is_exactly_one/0},
        {timeout, 60, fun csb02s_d5_list_decrypts_with_keyring/0},
        {timeout, 60, fun csb02s_d5_list_without_keyring_keeps_cipher_projection/0}
    ];
cases(Other) ->
    erlang:error({eb06_message_suite_db_unavailable, Other}).

%% ===================================================================
%% A01：无 consent 不写消息（含旁路尝试）
%% ===================================================================

a01_no_consent_writes_no_message_even_with_bypass_flag() ->
    NoConsent = ?FIX:new_scope(#{with_consent => false}),
    try
        {Org, Ws} = tenant(NoConsent),
        Conv = maps:get(conversation_id, NoConsent),
        Contact = maps:get(contact_id, NoConsent),
        MessagesBefore = ?FIX:count(Org, Ws, messages),
        AuditsBefore = ?FIX:count(Org, Ws, audits),
        Base = #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"eb06-no-consent-1">>,
            body => <<"eb06-no-consent-body">>,
            sender_type => contact,
            contact_id => Contact,
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs(),
            notify => unexpected_publisher(self())
        },
        %% ① 正常路径：consent 缺失 → consent_required
        ?assertEqual(
            {error, consent_required},
            eb_message_app:append_message(Org, Base)
        ),
        %% ② 旁路尝试：调用方自报 enforce_consent=false —— 本卡的应用层**不得**转发该键
        ?assertEqual(
            {error, consent_required},
            eb_message_app:append_message(Org, Base#{
                client_msg_id => <<"eb06-no-consent-2">>,
                enforce_consent => false
            })
        ),
        %% ③ canonical 行数与审计数增量必须为 0（不是只看错误码）
        ?assertEqual(MessagesBefore, ?FIX:count(Org, Ws, messages)),
        ?assertEqual(AuditsBefore, ?FIX:count(Org, Ws, audits)),
        ?assertNoPublish()
    after
        ?FIX:cleanup(NoConsent)
    end,
    %% ④ 无 retention policy 同样 fail-closed，零写入
    NoPolicy = ?FIX:new_scope(#{with_policy => false}),
    try
        {Org2, Ws2} = tenant(NoPolicy),
        ?assertEqual(
            {error, missing_retention_policy},
            eb_message_app:append_message(Org2, #{
                workspace_id => Ws2,
                conversation_id => maps:get(conversation_id, NoPolicy),
                client_msg_id => <<"eb06-no-policy-1">>,
                body => <<"eb06-no-policy-body">>,
                sender_type => contact,
                contact_id => maps:get(contact_id, NoPolicy),
                key_ref => ?FIX:key_ref(1),
                accepted_at => now_secs()
            })
        ),
        ?assertEqual(0, ?FIX:count(Org2, Ws2, messages)),
        ?assertEqual(0, ?FIX:count(Org2, Ws2, audits))
    after
        ?FIX:cleanup(NoPolicy)
    end.

%% ===================================================================
%% A03：重放不增行、不增审计
%% ===================================================================

a03_replay_adds_no_row_and_no_audit() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Capturer = capturing_publisher(self()),
        Params = (base_params(Scope, <<"eb06-replay-1">>))#{notify => Capturer},
        {ok, First} = eb_message_app:append_message(Org, Params),
        ?assertEqual(true, maps:get(accepted, First)),
        ?assertEqual(false, maps:get(replayed, First)),
        ?assert(is_integer(maps:get(audit_id, First))),
        MessageId = maps:get(message_id, First),
        MessagesAfterFirst = ?FIX:count(Org, Ws, messages),
        AuditsAfterFirst = ?FIX:count(Org, Ws, audits),
        ?assertEqual(1, MessagesAfterFirst),
        ?assertEqual(1, AuditsAfterFirst),
        _ =
            receive
                {published, N1} -> ?assertEqual(MessageId, maps:get(resource_id, N1))
            after 2000 -> erlang:error(never_published)
            end,
        %% 重放：同样 client_msg_id，第二次
        {ok, Second} = eb_message_app:append_message(Org, Params),
        ?assertEqual(true, maps:get(accepted, Second)),
        ?assertEqual(true, maps:get(replayed, Second)),
        ?assertEqual(undefined, maps:get(audit_id, Second)),
        ?assertEqual(MessageId, maps:get(message_id, Second)),
        ?assertEqual({skipped, replay}, maps:get(publisher, Second)),
        %% 行数与审计数不变；重放不重复通知
        ?assertEqual(MessagesAfterFirst, ?FIX:count(Org, Ws, messages)),
        ?assertEqual(AuditsAfterFirst, ?FIX:count(Org, Ws, audits)),
        ?assertNoPublish()
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A04：sender XOR / 复合 FK
%% ===================================================================

a04_sender_xor_and_composite_fk_are_enforced() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        %% ① 客户入站：显式 contact sender，identity/actor 均为空
        {ok, Inbound} = eb_message_app:append_message(Org, base_params(Scope, <<"eb06-in-1">>)),
        InRow = maps:get(message, Inbound),
        ?assertEqual(contact, maps:get(sender_type, InRow)),
        ?assertEqual(Contact, maps:get(sender_contact_id, InRow)),
        ?assertEqual(undefined, maps:get(sender_business_identity_id, InRow)),
        ?assertEqual(undefined, maps:get(actor_user_id, InRow)),
        ?assertEqual(ok, eb_message:validate_sender(InRow)),
        %% ② 员工出站：显式 identity + actor
        {ok, Outbound} = eb_message_app:append_message(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"eb06-out-1">>,
            body => <<"eb06-out-body-1">>,
            sender_type => <<"business_identity">>,
            identity_id => Sales,
            actor_user_id => Actor,
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs()
        }),
        OutRow = maps:get(message, Outbound),
        ?assertEqual(business_identity, maps:get(sender_type, OutRow)),
        ?assertEqual(undefined, maps:get(sender_contact_id, OutRow)),
        ?assertEqual(Sales, maps:get(sender_business_identity_id, OutRow)),
        ?assertEqual(Actor, maps:get(actor_user_id, OutRow)),
        ?assertEqual(ok, eb_message:validate_sender(OutRow)),
        RowsBefore = ?FIX:count(Org, Ws, messages),
        %% ③ XOR 违约逐类可区分，且一律零新增行
        Violations = [
            {outbound_missing_actor,
                #{
                    sender_type => business_identity,
                    identity_id => Sales,
                    contact_id => undefined
                },
                actor_required},
            {outbound_missing_identity,
                #{
                    sender_type => business_identity,
                    actor_user_id => Actor,
                    contact_id => undefined
                },
                identity_required},
            {inbound_with_identity,
                #{
                    sender_type => contact, contact_id => Contact, identity_id => Sales
                },
                identity_not_allowed_for_contact},
            {inbound_with_actor,
                #{
                    sender_type => contact, contact_id => Contact, actor_user_id => Actor
                },
                actor_not_allowed_for_contact},
            {inbound_missing_contact,
                #{
                    sender_type => contact, contact_id => undefined
                },
                contact_required},
            {outbound_with_contact,
                #{
                    sender_type => business_identity,
                    identity_id => Sales,
                    actor_user_id => Actor,
                    contact_id => Contact
                },
                contact_not_allowed_for_identity},
            {unknown_sender_type, #{sender_type => system}, {unknown_sender_type, system}}
        ],
        lists:foreach(
            fun({Label, Sender, Expected}) ->
                Params = maps:merge(base_params(Scope, client_id(Label)), Sender),
                ?assertEqual(
                    {error, Expected},
                    eb_message_app:append_message(Org, Params)
                )
            end,
            Violations
        ),
        ?assertEqual(RowsBefore, ?FIX:count(Org, Ws, messages)),
        %% ④a 跨 Org contact 声明在 canonical tx 归属门即被拒（F-SEC-01：
        %% contact 必须等于会话绑定 contact，早于 DB FK 拦截）
        ForeignContact = foreign_contact(Scope),
        Result = eb_message_app:append_message(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"eb06-cross-org-sender">>,
            body => <<"eb06-cross-org-body">>,
            sender_type => contact,
            contact_id => ForeignContact,
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs()
        }),
        ?assertMatch(
            {error, {sender_contact_mismatch, ForeignContact, _}}, Result
        ),
        ?assertEqual(RowsBefore, ?FIX:count(Org, Ws, messages)),
        %% ④b 跨 Org business_identity：归属门不覆盖内部注入合同（无
        %% caller_identity_id），仍由 DB 复合 FK 23503 兜底
        ForeignIdentity = foreign_identity(Scope),
        Result2 = eb_message_app:append_message(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"eb06-cross-org-identity">>,
            body => <<"eb06-cross-org-body-2">>,
            sender_type => business_identity,
            identity_id => ForeignIdentity,
            actor_user_id => Actor,
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs()
        }),
        ?assertMatch({error, {sql, _, _}}, Result2),
        {error, {sql, Code2, Constraint2}} = Result2,
        ?assertEqual(<<"23503">>, Code2),
        ?assertEqual(<<"fk_em_sender_identity">>, Constraint2),
        ?assertEqual(RowsBefore, ?FIX:count(Org, Ws, messages)),
        %% ⑤ 非法参数在触库前就被拒（零副作用）
        ?assertEqual(
            {error, {invalid_client_msg_id, <<>>}},
            eb_message_app:append_message(Org, (base_params(Scope, <<"x">>))#{
                client_msg_id => <<>>
            })
        ),
        ?assertEqual(
            {error, {unknown_sender_type, <<"robot">>}},
            eb_message_app:append_message(Org, #{
                workspace_id => Ws,
                conversation_id => Conv,
                client_msg_id => <<"eb06-bad-sender">>,
                sender_type => <<"robot">>,
                body => <<"eb06-bad-sender-body">>
            })
        ),
        %% 租户参数先于一切业务校验（没有 workspace 就不触库）
        ?assertMatch(
            {error, {invalid_workspace_id, _}},
            eb_message_app:append_message(Org, #{
                conversation_id => Conv,
                client_msg_id => <<"eb06-no-ws">>,
                sender_type => contact,
                contact_id => Contact,
                body => <<"eb06-no-ws-body">>
            })
        ),
        ?assertMatch(
            {error, {invalid_conversation_id, _}},
            eb_message_app:append_message(Org, (base_params(Scope, <<"y">>))#{
                conversation_id => undefined
            })
        ),
        ?assertEqual(RowsBefore, ?FIX:count(Org, Ws, messages))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A04：历史 actor 可追溯
%% ===================================================================

a04_history_actor_is_traceable() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        {ok, Outbound} = eb_message_app:append_message(Org, #{
            workspace_id => Ws,
            conversation_id => maps:get(conversation_id, Scope),
            client_msg_id => <<"eb06-actor-1">>,
            body => <<"eb06-actor-body">>,
            sender_type => business_identity,
            identity_id => Sales,
            actor_user_id => Actor,
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs()
        }),
        MessageId = maps:get(message_id, Outbound),
        %% ① canonical 行上仍带 actor（历史可追溯）
        {ok, Row} = eb_pg_store:fetch_message(Org, Ws, MessageId),
        ?assertEqual(Actor, maps:get(actor_user_id, Row)),
        ?assertEqual(Sales, maps:get(sender_business_identity_id, Row)),
        %% ② 接受审计同时记录 identity/actor（资源级 + 审计级双向可追溯）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_id=$2 AND action='message.accept'"
                    "   AND actor_user_id=$3 AND business_identity_id=$4"
                >>,
                [Org, MessageId, Actor, Sales],
                0
            )
        ),
        %% ③ 入站消息的审计不携带 actor/identity（不被自动填充）
        {ok, Inbound} = eb_message_app:append_message(
            Org, base_params(Scope, <<"eb06-actor-in">>)
        ),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_id=$2 AND action='message.accept'"
                    "   AND actor_user_id IS NULL AND business_identity_id IS NULL"
                >>,
                [Org, maps:get(message_id, Inbound)],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A05：realtime 失败不回滚已提交真源
%% ===================================================================

a05_realtime_failure_does_not_roll_back_committed_truth() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        % ① 发布器返回错误
        {ok, Returned} = eb_message_app:append_message(Org, (base_params(Scope, <<"eb06-rt-1">>))#{
            notify => fun(_N) -> {error, publisher_down} end
        }),
        ?assertEqual(true, maps:get(accepted, Returned)),
        ?assertMatch({failed, publisher_down}, maps:get(publisher, Returned)),
        MessageId = maps:get(message_id, Returned),
        %% 真源已提交且字段 hash 与返回体一致（失败没有把它回滚掉）
        {ok, Row} = eb_pg_store:fetch_message(Org, Ws, MessageId),
        ?assertEqual(
            canonical_hash(Row),
            canonical_hash(maps:get(message, Returned))
        ),
        %% ② 发布器直接崩溃（throw）：同样不得影响 accepted 与真源
        {ok, Crashed} = eb_message_app:append_message(Org, (base_params(Scope, <<"eb06-rt-2">>))#{
            notify => fun(_N) -> erlang:error(publisher_crashed) end
        }),
        ?assertEqual(true, maps:get(accepted, Crashed)),
        ?assertMatch({failed, {publisher_crashed, _}}, maps:get(publisher, Crashed)),
        {ok, CrashRow} = eb_pg_store:fetch_message(Org, Ws, maps:get(message_id, Crashed)),
        ?assertEqual(
            canonical_hash(CrashRow),
            canonical_hash(maps:get(message, Crashed))
        ),
        %% ③ 两次失败之后，canonical 行数仍精确为 2（无回滚、无重复）
        ?assertEqual(2, ?FIX:count(Org, Ws, messages)),
        ?assertEqual(2, ?FIX:count(Org, Ws, audits))
    after
        ?FIX:cleanup(Scope)
    end.

%% 通知只在**提交之后**发出，且只含 resource id（无正文/密文/摘要/密钥）。
a05_notification_carries_only_resource_ids_after_commit() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Parent = self(),
        Canary = ?FIX:canary(),
        %% 发布器在**自己的回调里回读 DB**：必须已经能看到该 canonical 行
        %% （证明 accepted/通知都晚于事务提交），同时把通知内容交给测试进程。
        Publisher = fun(Notification) ->
            MessageId = maps:get(resource_id, Notification),
            Probe = eb_pg_store:fetch_message(Org, Ws, MessageId),
            Parent ! {published, Notification, Probe},
            ok
        end,
        {ok, Result} = eb_message_app:append_message(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"eb06-notify-1">>,
            body => Canary,
            sender_type => contact,
            contact_id => maps:get(contact_id, Scope),
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs(),
            notify => Publisher
        }),
        MessageId = maps:get(message_id, Result),
        Notification =
            receive
                {published, N, Probe} ->
                    ?assertMatch({ok, _}, Probe),
                    {ok, Committed} = Probe,
                    ?assertEqual(MessageId, maps:get(id, Committed)),
                    N
            after 2000 ->
                erlang:error(never_published)
            end,
        %% 落库密文非明文
        {ok, Stored} = eb_pg_store:fetch_message(Org, Ws, MessageId),
        ?assertEqual(nomatch, binary:match(maps:get(body_cipher, Stored), Canary)),
        %% 通知只含 resource id：键白名单 + 值里不含任何 payload
        Allowed = eb_message_app:notification_keys(),
        lists:foreach(
            fun(Key) ->
                ?assert(lists:member(Key, Allowed)),
                ?assertEqual(
                    nomatch,
                    re:run(atom_to_binary(Key, utf8), <<"body|cipher|hash|plaintext|key_ref">>, [
                        {capture, none}
                    ])
                )
            end,
            maps:keys(Notification)
        ),
        ?assertEqual(MessageId, maps:get(resource_id, Notification)),
        Blob = iolist_to_binary(io_lib:format("~p", [Notification])),
        ?assertEqual(nomatch, binary:match(Blob, Canary)),
        ?assertEqual(nomatch, binary:match(Blob, maps:get(body_cipher, Stored))),
        ?assert(maps:size(Notification) =< length(Allowed))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A06：ACK 幂等 + canonical 不变
%% ===================================================================

a06_ack_is_idempotent_and_leaves_canonical_untouched() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        {ok, First} = eb_message_app:append_message(Org, base_params(Scope, <<"eb06-ack-1">>)),
        MessageId = maps:get(message_id, First),
        {ok, Keep} = eb_message_app:append_message(Org, base_params(Scope, <<"eb06-ack-keep">>)),
        KeepId = maps:get(message_id, Keep),
        BeforeCount = ?FIX:count(Org, Ws, messages),
        BeforeHashes = canonical_hashes(Org, Ws, Conv),
        RecipientRef = <<"contact:", (integer_to_binary(maps:get(contact_id, Scope)))/binary>>,
        AckBase = #{
            workspace_id => Ws,
            message_id => MessageId,
            recipient_ref => RecipientRef,
            device_id => <<"eb06-device-1">>,
            acked_at => now_secs()
        },
        {ok, Acked} = eb_message_app:ack_delivery(Org, AckBase),
        Delivery = maps:get(delivery, Acked),
        ?assertEqual(delivered, maps:get(status, Delivery)),
        ?assertEqual(MessageId, maps:get(message_id, Delivery)),
        ?assertEqual(true, maps:get(canonical_unchanged, Acked)),
        %% 幂等：同一 (message, recipient, device) 重复 ACK 不增行、返回同一行
        {ok, Again} = eb_message_app:ack_delivery(Org, AckBase),
        ?assertEqual(maps:get(id, Delivery), maps:get(id, maps:get(delivery, Again))),
        ?assertEqual(1, ?FIX:count(Org, Ws, deliveries)),
        %% ACK 前后 canonical 行数与关键字段 hash 逐字不变
        ?assertEqual(BeforeCount, ?FIX:count(Org, Ws, messages)),
        ?assertEqual(BeforeHashes, canonical_hashes(Org, Ws, Conv)),
        %% 另一台设备是一条独立的投递状态（允许），但 canonical 仍不变
        {ok, OtherDevice} = eb_message_app:ack_delivery(
            Org, AckBase#{device_id => <<"eb06-device-2">>}
        ),
        ?assertNotEqual(maps:get(id, Delivery), maps:get(id, maps:get(delivery, OtherDevice))),
        ?assertEqual(2, ?FIX:count(Org, Ws, deliveries)),
        ?assertEqual(BeforeHashes, canonical_hashes(Org, Ws, Conv)),
        ?assertEqual(BeforeCount, ?FIX:count(Org, Ws, messages)),
        %% 未受影响的消息仍在（误删控制）
        {ok, _} = eb_pg_store:fetch_message(Org, Ws, KeepId),
        %% 负例：消息不在本租户 ⇒ 独立投递状态不落行
        ?assertMatch(
            {error, {message_not_in_scope, _}},
            eb_message_app:ack_delivery(Org, AckBase#{message_id => eb_pg_test_fixture:id()})
        ),
        %% 负例：recipient_ref 形状不符（DB CHECK 白名单之外）⇒ 触库前拒绝
        ?assertEqual(
            {error, {invalid_recipient_ref, <<"someone">>}},
            eb_message_app:ack_delivery(Org, AckBase#{recipient_ref => <<"someone">>})
        ),
        ?assertEqual(2, ?FIX:count(Org, Ws, deliveries))
    after
        ?FIX:cleanup(Scope)
    end.

%% 客户端隐藏（visibility）与授权类事件都不得改变 canonical 关键字段。
a06_hide_and_authorization_events_do_not_change_canonical() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        {ok, Appended} = eb_message_app:append_message(Org, base_params(Scope, <<"eb06-hide-1">>)),
        MessageId = maps:get(message_id, Appended),
        Before = canonical_hashes(Org, Ws, Conv),
        BeforeCount = ?FIX:count(Org, Ws, messages),
        %% visibility 不在 canonical 字段集合内（隐藏只改可见性）
        ?assertNot(lists:member(visibility, eb_message:canonical_fields())),
        %% ① 客户端隐藏：visibility → hidden，canonical 关键字段 hash 不变
        ok = ?FIX:exec(
            <<
                "UPDATE enterprise_message SET visibility='hidden'"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, MessageId]
        ),
        {ok, Hidden} = eb_pg_store:fetch_message(Org, Ws, MessageId),
        ?assertEqual(hidden, maps:get(visibility, Hidden)),
        ?assertEqual(Before, canonical_hashes(Org, Ws, Conv)),
        ?assertEqual(BeforeCount, ?FIX:count(Org, Ws, messages)),
        ?assertEqual(
            ok,
            eb_message:ack_preserves_canonical(
                eb_message_app:canonical_view([maps:get(message, Appended)]),
                eb_message_app:canonical_view([Hidden])
            )
        ),
        %% ② 授权类事件（suspend / offboarding 语义）：本卡的 application 面**没有任何**
        %%    可改变 canonical 消息的入口——导出白名单锁死，只有 append / ack / 只读历史。
        %%    （EB-06 重开新增 `list_messages/2` 键集分页与 `fetch_message/2` 只读读取；
        %%    两者也落在下面的「零写」静态断言的覆盖范围内。）
        ?assertEqual(
            lists:sort([
                {ack_delivery, 2},
                {append_message, 2},
                {canonical_view, 1},
                {fetch_message, 2},
                {list_messages, 2},
                {notification_keys, 0}
            ]),
            lists:sort(drop_module_info(eb_message_app:module_info(exports)))
        ),
        %% ③ 越权密钥（force/offboarding/suspend 之类的旁路）不改变 append 语义：
        %%    无 consent 时依旧 fail-closed（零写入）。
        Source = module_code(eb_message_app),
        ?assertEqual(nomatch, re:run(Source, <<"elib_pg">>, [{capture, none}])),
        ?assertEqual(nomatch, re:run(Source, <<"DELETE FROM">>, [{capture, none}])),
        ?assertEqual(nomatch, re:run(Source, <<"UPDATE">>, [{capture, none}]))
    after
        ?FIX:cleanup(Scope)
    end.

%% 禁止复用个人 CLIENT_ACK / msg_archive 清理链（计划点名）
a06_no_personal_ack_or_archive_chain_is_used() ->
    Modules = [eb_message_app, eb_conversation_app, eb_retention_app, eb_consent_app],
    Forbidden = [
        <<"msg_c2c">>,
        <<"msg_store">>,
        <<"msg_archive">>,
        <<"CLIENT_ACK">>,
        <<"client_ack">>,
        <<"conversation_table">>,
        <<"user_friend">>
    ],
    lists:foreach(
        fun(Mod) ->
            Source = module_code(Mod),
            lists:foreach(
                fun(Token) ->
                    ?assertEqual(
                        {Mod, Token, nomatch},
                        {Mod, Token, re:run(Source, Token, [{capture, none}])}
                    )
                end,
                Forbidden
            )
        end,
        Modules
    ),
    %% 行为侧（F-EB06-1，2026-09-15 重做）：**meck passthrough 调用跟踪**证明
    %% 本次 ACK 未进入个人链路。取代旧的全表计数断言 —— 计数证明力弱（全表
    %% 没变≠本调用没写；且对并发写入者敏感），passthrough 包装恰好逐条回答
    %% 「本 ACK 的调用序列里有没有触碰个人模块」（原行为透传，行为零改变）。
    %% 口径与上面静态 Forbidden 同源：不复用个人消息/会话/好友/附件链路的
    %% **任何**调用（比「只禁写函数」更严）。
    %% （备选的 erlang:trace call 跟踪在本机 OTP29 裸节点实测静默失效——
    %%   pattern 匹配 28/29 但零事件，send trace 同样零事件；meck 是仓内
    %%   EUnit 常态等价机制。）
    Scope = ?FIX:new_scope(),
    PersonalMods = personal_touch_modules(),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Appended} = eb_message_app:append_message(
            Org, base_params(Scope, <<"eb06-ack-personal">>)
        ),
        %% 负例校准：故意调用一个个人模块的入口，跟踪必须抓得到 ——
        %% 保证「零命中」不是包装没生效的假绿。
        ok = begin_personal_trace(PersonalMods),
        _ = msg_c2c_repo:tablename(),
        Negative = collect_personal_trace_hits(PersonalMods),
        stop_personal_trace(PersonalMods),
        ?assertMatch([{msg_c2c_repo, tablename, 0} | _], Negative),
        ok = begin_personal_trace(PersonalMods),
        {ok, _} = eb_message_app:ack_delivery(Org, #{
            workspace_id => Ws,
            message_id => maps:get(message_id, Appended),
            recipient_ref => recipient_ref(Scope),
            device_id => <<"eb06-device-personal">>,
            acked_at => now_secs()
        }),
        Hits = collect_personal_trace_hits(PersonalMods),
        stop_personal_trace(PersonalMods),
        ?assertEqual([], Hits)
    after
        stop_personal_trace(PersonalMods),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A08：提交失败无 accepted，恢复重试恰好一条
%% ===================================================================

a08_commit_failure_yields_no_acceptance_and_retry_is_exactly_one() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        MessagesBefore = ?FIX:count(Org, Ws, messages),
        AuditsBefore = ?FIX:count(Org, Ws, audits),
        Capturer = capturing_publisher(self()),
        ClientMsgId = <<"eb06-atomic-1">>,
        %% 制造**真实**的提交失败：审计 action 违反 ck_eae_action ⇒ 事务整体回滚
        Result = eb_message_app:append_message(Org, (base_params(Scope, ClientMsgId))#{
            audit_action => <<>>,
            notify => Capturer
        }),
        ?assertMatch({error, {sql, _, _}}, Result),
        {error, {sql, Code, Constraint}} = Result,
        ?assertEqual(<<"23514">>, Code),
        ?assertEqual(<<"ck_eae_action">>, Constraint),
        %% ① 无 half-commit（消息与审计都不留）
        ?assertEqual(MessagesBefore, ?FIX:count(Org, Ws, messages)),
        ?assertEqual(AuditsBefore, ?FIX:count(Org, Ws, audits)),
        %% ② 无 accepted（返回的是错误元组，不含 accepted 标志）
        ?assertEqual(error, element(1, Result)),
        ?assertEqual(2, tuple_size(Result)),
        %% ③ 无 realtime（发布器一次都没被调用）
        ?assertNoPublish(),
        %% ④ 无输入丢失：同一幂等键 + 同一正文重试 ⇒ 恰好一条消息、一条接受审计
        {ok, Retried} = eb_message_app:append_message(Org, base_params(Scope, ClientMsgId)),
        ?assertEqual(true, maps:get(accepted, Retried)),
        ?assertEqual(false, maps:get(replayed, Retried)),
        ?assertEqual(MessagesBefore + 1, ?FIX:count(Org, Ws, messages)),
        ?assertEqual(AuditsBefore + 1, ?FIX:count(Org, Ws, audits)),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_message"
                    " WHERE organization_id=$1 AND conversation_id=$2 AND client_msg_id=$3"
                >>,
                [Org, Conv, ClientMsgId],
                0
            )
        ),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_id=$2 AND action='message.accept'"
                >>,
                [Org, maps:get(message_id, Retried)],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

%% ===================================================================
%% CSB-02S D5：读面解密（keyring 可用 → 明文体；缺失 → 密文投影维持）
%% ===================================================================

%% @doc 有 key_ref：append（服务端封口）→ list_messages 服务端解密 →
%% `body` 为原明文、密文材料列（body_cipher/aad_hash）不出站。
csb02s_d5_list_decrypts_with_keyring() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Plain = <<"d5-plaintext-canary-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        %% key_ref 绑定一次复用：key_ref/1 每次生成新随机密钥，封口与解密
        %% 必须同一把（否则 GCM authentication_failed）。
        KeyRef = ?FIX:key_ref(1),
        {ok, _} = eb_message_app:append_message(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"d5-decrypt-1">>,
            body => Plain,
            sender_type => contact,
            contact_id => Contact,
            key_ref => KeyRef,
            accepted_at => now_secs()
        }),
        %% 读回密文列的保真性自证：content_hash 是入库时对密文摘要的锚。
        {ok, Raw} = ?FIX:store():list_messages_after(Org, Ws, #{
            conversation_id => Conv, after_id => 0, limit => 200
        }),
        RawMine = hd([R || R <- Raw, maps:get(client_msg_id, R, undefined) =:= <<"d5-decrypt-1">>]),
        ?assertEqual(
            maps:get(content_hash, RawMine),
            binary:encode_hex(crypto:hash(sha256, maps:get(body_cipher, RawMine)), lowercase)
        ),
        {ok, Rows} = eb_message_app:list_messages(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            key_ref => KeyRef
        }),
        ?assert(length(Rows) >= 1),
        Mine = hd([R || R <- Rows, maps:get(client_msg_id, R, undefined) =:= <<"d5-decrypt-1">>]),
        ?assertEqual(Plain, maps:get(body, Mine)),
        %% 密文材料不与明文并存出站。
        ?assertNot(is_map_key(body_cipher, Mine)),
        ?assertNot(is_map_key(aad_hash, Mine))
    after
        ?FIX:cleanup(Scope)
    end.

%% @doc 无 keyring：list_messages 维持既有**密文投影**（body_cipher/aad_hash
%% 在、无 body 明文键）——不报错、不半解密。
csb02s_d5_list_without_keyring_keeps_cipher_projection() ->
    Scope = ?FIX:new_scope(),
    OldKeyring = application:get_env(imboy, eb_enterprise_keyring),
    try
        %% env 临时清空（F6：无 keyring = 装配缺省不可用）。
        application:unset_env(imboy, eb_enterprise_keyring),
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        {ok, _} = eb_message_app:append_message(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => <<"d5-cipher-1">>,
            body => <<"d5-cipher-body">>,
            sender_type => contact,
            contact_id => Contact,
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs()
        }),
        {ok, Rows} = eb_message_app:list_messages(Org, #{
            workspace_id => Ws,
            conversation_id => Conv
        }),
        ?assert(length(Rows) >= 1),
        Mine = hd([R || R <- Rows, maps:get(client_msg_id, R, undefined) =:= <<"d5-cipher-1">>]),
        ?assert(is_binary(maps:get(body_cipher, Mine))),
        ?assert(is_binary(maps:get(aad_hash, Mine))),
        ?assertNot(is_map_key(body, Mine))
    after
        case OldKeyring of
            undefined -> application:unset_env(imboy, eb_enterprise_keyring);
            {ok, V} -> application:set_env(imboy, eb_enterprise_keyring, V)
        end,
        ?FIX:cleanup(Scope)
    end.

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

base_params(Scope, ClientMsgId) ->
    #{
        workspace_id => maps:get(workspace_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        client_msg_id => ClientMsgId,
        body => <<"eb06-body-", ClientMsgId/binary>>,
        sender_type => contact,
        contact_id => maps:get(contact_id, Scope),
        key_ref => ?FIX:key_ref(1),
        accepted_at => now_secs(),
        notify => fun(_Notification) -> ok end
    }.

client_id(Label) ->
    <<"eb06-", (atom_to_binary(Label, utf8))/binary>>.

recipient_ref(Scope) ->
    <<"contact:", (integer_to_binary(maps:get(contact_id, Scope)))/binary>>.

now_secs() ->
    eb_system_clock:now().

capturing_publisher(Parent) ->
    fun(Notification) ->
        Parent ! {published, Notification},
        ok
    end.

unexpected_publisher(Parent) ->
    fun(Notification) ->
        Parent ! {unexpected_publish, Notification},
        ok
    end.

canonical_hashes(Org, Ws, Conv) ->
    {ok, Rows} = eb_pg_store:list_messages(Org, Ws, Conv),
    maps:from_list([{maps:get(id, Row), canonical_hash(Row)} || Row <- Rows]).

canonical_hash(Row) ->
    Fields = [{K, maps:get(K, Row, undefined)} || K <- ?CANONICAL_HASH_FIELDS],
    binary:encode_hex(crypto:hash(sha256, term_to_binary(Fields)), lowercase).

foreign_identity(Scope) ->
    %% 造一个**不存在**的他 Org business_identity id：FK 23503（无匹配行）
    %% 与「行属于他 Org」对复合 FK (organization_id, id) 是同一拒绝。
    ?FIX:id().

foreign_contact(Scope) ->
    OtherOrg = maps:get(other_org_id, Scope),
    ContactId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_contact(id,organization_id,status,display_name,version)"
            " VALUES ($1,$2,'active',$3,1)"
        >>,
        [ContactId, OtherOrg, <<"eb06-foreign-contact-", (integer_to_binary(ContactId))/binary>>]
    ),
    ContactId.

%% F-EB06-1：个人链路触碰面（与 A06 静态 Forbidden 同口径的消息/会话/好友/
%% 附件 ds+repo+logic 模块）。meck passthrough 包装必须成功，失败即测试红
%% （不允许「包装失败静默跳过」的假绿）。
personal_touch_modules() ->
    [
        msg_c2c_ds,
        msg_c2c_repo,
        msg_store_ds,
        msg_store_repo,
        msg_store_worker,
        friend_ds,
        friend_repo,
        friend_category_ds,
        friend_category_repo,
        conversation_logic,
        conversation_pin_ds,
        conversation_pin_repo,
        conversation_delete_ds,
        conversation_delete_repo,
        attach_logic,
        attachment_ds,
        attachment_repo,
        attach_pending_repo
    ].

begin_personal_trace(Modules) ->
    lists:foreach(
        fun(M) ->
            case code:ensure_loaded(M) of
                {module, M} ->
                    case catch meck:new(M, [passthrough]) of
                        ok -> ok;
                        E1 -> erlang:error({personal_trace_mock_failed, M, E1})
                    end;
                E2 ->
                    erlang:error({personal_trace_load_failed, M, E2})
            end
        end,
        Modules
    ),
    ok.

collect_personal_trace_hits(Modules) ->
    %% 只统计**测试自身进程**的调用：「本 ACK 的调用链」＝测试进程内同步
    %% 发出的调用。后台 msg_store_worker 是独立 gen_statem 进程，每秒 tick
    %% 调 msg_store_repo:claim_pending/2（并把 staging 存量写 msg_c2c_ds/
    %% msg_c2c_repo 正式表），其调用与本测试判定无关，却会按发生顺序污染
    %% meck history——曾把负例校准的 [{msg_c2c_repo,tablename,0}] 顶出列表
    %% 头部导致校准必红（全量/单跑结果随机）。按 CallerPid 过滤使判定
    %% 确定化；断言语义（ACK 链路对个人模块零触碰）不变。
    Self = self(),
    lists:append(
        lists:map(
            fun(M) ->
                case catch meck:history(M) of
                    L when is_list(L) ->
                        %% meck>=0.9 形状：[{CallerPid, {Mod, Fun, Args}, Result}]
                        [
                            {M, F, length(Args)}
                         || {Caller, {M, F, Args}, _Result} <- L, Caller =:= Self
                        ];
                    _Other ->
                        erlang:error({personal_trace_history_failed, M})
                end
            end,
            Modules
        )
    ).

stop_personal_trace(Modules) ->
    lists:foreach(
        fun(M) -> catch meck:unload(M) end,
        Modules
    ).

drop_module_info(Exports) ->
    [E || E <- Exports, E =/= {module_info, 0}, E =/= {module_info, 1}].

module_code(Mod) ->
    {ok, Bin} = file:read_file(source_path(Mod)),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    iolist_to_binary([
        [re:replace(Line, <<"%.*$">>, <<>>, [{return, binary}]), <<"\n">>]
     || Line <- Lines
    ]).

source_path(Mod) ->
    Rel = "src/features/enterprise_business/application/" ++ source_rel(Mod),
    FromBeam =
        case code:which(Mod) of
            BeamPath when is_list(BeamPath) ->
                [filename:dirname(filename:dirname(BeamPath))];
            _ ->
                []
        end,
    FromLib =
        try
            [code:lib_dir(imboy)]
        catch
            _:_ -> []
        end,
    Candidates = FromBeam ++ FromLib ++ [element(2, file:get_cwd())],
    case [P || P <- [filename:join(Root, Rel) || Root <- Candidates], filelib:is_file(P)] of
        [Found | _] -> Found;
        [] -> erlang:error({eb06_source_missing, Mod, Rel, Candidates})
    end.

source_rel(eb_consent_app) -> "consent/eb_consent_app.erl";
source_rel(eb_conversation_app) -> "conversation/eb_conversation_app.erl";
source_rel(eb_message_app) -> "message/eb_message_app.erl";
source_rel(eb_retention_app) -> "retention/eb_retention_app.erl".
