%%% @doc EB-06 企业会话（application 用例）套件（真库）。
%%%
%%% 覆盖：
%%%   EB-06-A02 企业会话**显式绑定同 Org 默认 Workspace**（服务端解析，跨 Org 拒绝），
%%%             且**不进入个人表**（`conversation` / `msg_c2c` / `msg_store` 行数增量 0）；
%%%   EB-06-A04 的 handover 侧：handover **不重写历史 message** 的 sender/actor
%%%             （逐条 hash 比对 + domain 判据 + 反例控制），且 handover 路径不触消息表。
%%%
%%% 隔离与命名同 EB-03/EB-05 的触库套件（随机 TSID 合成租户、`eb06-` 前缀）。
-module(eb_conversation_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).

conversation_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a02_conversation_binds_org_default_workspace/0},
        {timeout, 60, fun a02_non_default_and_cross_org_workspaces_are_rejected/0},
        {timeout, 60, fun a02_missing_default_workspace_source_fails_closed/0},
        {timeout, 60, fun a02_enterprise_conversation_writes_no_personal_tables/0},
        {timeout, 60, fun a02_conversation_row_carries_synthetic_consent/0},
        {timeout, 60, fun a04_handover_does_not_rewrite_history/0},
        {timeout, 60, fun a11_handover_audit_failure_rolls_back_assignee/0},
        {timeout, 60, fun a11_handover_audit_uses_usecase_tx_port_not_plain_audit_port/0},
        {timeout, 60, fun a15_default_workspace_comes_from_member_fact_port/0},
        {timeout, 60, fun a15_cross_org_default_workspace_is_rejected/0},
        {timeout, 60, fun a15_resolver_reuse_fails_closed_on_unresolvable_workspace/0}
    ];
cases(Other) ->
    erlang:error({eb06_conversation_suite_db_unavailable, Other}).

%% ===================================================================
%% A02：显式绑定同 Org 默认 Workspace（服务端解析）
%% ===================================================================

a02_conversation_binds_org_default_workspace() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Before = eb_pg_test_fixture:count(Org, Ws, conversations),
        {ok, Opened} = open(Scope, Org, Ws),
        Conversation = maps:get(conversation, Opened),
        %% ① 行里的 Workspace 就是服务端解析出的 Org 默认 Workspace 本身
        ?assertEqual(Ws, maps:get(workspace_id, Opened)),
        ?assertEqual(Ws, maps:get(workspace_id, Conversation)),
        ?assertEqual(Org, maps:get(organization_id, Conversation)),
        %% ② DB 回读：workspace 归属确实等于该 Org（复合 FK + 同语句租户自检）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_conversation c"
                    "  JOIN workspace w ON w.id = c.workspace_id"
                    "    AND w.organization_id = c.organization_id"
                    " WHERE c.organization_id=$1 AND c.workspace_id=$2 AND c.id=$3"
                >>,
                [Org, Ws, maps:get(conversation_id, Opened)],
                0
            )
        ),
        ?assertEqual(Before + 1, eb_pg_test_fixture:count(Org, Ws, conversations)),
        %% ③ 会话的 consent 是合成件，且只能声明状态机
        ?assert(eb_consent_app:is_synthetic(Conversation)),
        ?assertEqual(
            synthetic_state_machine_only,
            maps:get(status, maps:get(evidence, Opened))
        )
    after
        ?FIX:cleanup(Scope)
    end.

a02_non_default_and_cross_org_workspaces_are_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        SecondWs = second_workspace(Scope),
        OtherIdentity = other_org_identity(Scope),
        Before = conversations_all(Org),
        OtherBefore = conversations_all(OtherOrg),
        %% ① 同 Org 但不是默认 Workspace ⇒ 服务端直接拒绝，零写入（不触库）
        ?assertEqual(
            {error, {not_default_workspace, SecondWs, Ws}},
            eb_conversation_app:open_conversation(Org, #{
                workspace_id => SecondWs,
                contact_id => maps:get(contact_id, Scope),
                business_identity_id => maps:get(sales_identity_id, Scope),
                default_workspace => default_ws(Ws)
            })
        ),
        %% ② 默认 Workspace 解析到**另一个 Org** ⇒ 同语句租户自检拒绝，零写入
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_conversation_app:open_conversation(Org, #{
                workspace_id => OtherWs,
                contact_id => maps:get(contact_id, Scope),
                business_identity_id => maps:get(sales_identity_id, Scope),
                default_workspace => default_ws(OtherWs)
            })
        ),
        %% ③ 调用方自报「默认」但默认解析器给出的不同 ⇒ 拒绝（不接受调用方指定/切换）
        ?assertEqual(
            {error, {not_default_workspace, OtherWs, Ws}},
            eb_conversation_app:open_conversation(Org, #{
                workspace_id => OtherWs,
                contact_id => maps:get(contact_id, Scope),
                business_identity_id => maps:get(sales_identity_id, Scope),
                default_workspace => default_ws(Ws)
            })
        ),
        %% ④ 跨 Org 的 contact / identity 也必须零写入
        ?assertEqual(
            {error, {identity_not_in_org, OtherIdentity}},
            eb_conversation_app:open_conversation(Org, #{
                workspace_id => Ws,
                contact_id => maps:get(contact_id, Scope),
                business_identity_id => OtherIdentity,
                default_workspace => default_ws(Ws)
            })
        ),
        ?assertEqual(Before, conversations_all(Org)),
        %% ⑤ 另一 Org 的计数不受影响（跨 Org 不越权）
        ?assertEqual(OtherBefore, conversations_all(OtherOrg))
    after
        ?FIX:cleanup(Scope)
    end.

a02_missing_default_workspace_source_fails_closed() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Before = conversations_all(Org),
        ?assertEqual(
            {error, {default_workspace_source_missing, organization}},
            eb_conversation_app:open_conversation(Org, #{
                workspace_id => Ws,
                contact_id => maps:get(contact_id, Scope),
                business_identity_id => maps:get(sales_identity_id, Scope)
            })
        ),
        ?assertEqual(Before, conversations_all(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A02：企业会话不进入个人表
%% ===================================================================

a02_enterprise_conversation_writes_no_personal_tables() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        PersonalBefore = personal_table_counts(),
        {ok, _Opened} = open(Scope, Org, Ws),
        %% ① 企业侧确实写了（正控制）
        ?assert(eb_pg_test_fixture:count(Org, Ws, conversations) >= 1),
        %% ② 个人表行数增量必须为 0（不是「没调用」的静态证据）
        ?assertEqual(PersonalBefore, personal_table_counts())
    after
        ?FIX:cleanup(Scope)
    end.

a02_conversation_row_carries_synthetic_consent() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Opened} = open(Scope, Org, Ws),
        ConversationId = maps:get(conversation_id, Opened),
        {ok, Row} = eb_pg_store:fetch_conversation(Org, Ws, ConversationId),
        ?assertEqual(eb_consent_app:synthetic_notice_version(), maps:get(notice_version, Row)),
        ?assertEqual(
            eb_consent_app:synthetic_subject(ConversationId),
            maps:get(consent_subject, Row)
        ),
        ?assert(is_integer(maps:get(consent_at, Row))),
        ?assertEqual(synthetic, maps:get(consent_evidence_kind, Row)),
        ?assertEqual(ok, eb_consent:gate(eb_consent_app:consent_of(Row), any))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A04：handover 不重写历史
%% ===================================================================

a04_handover_does_not_rewrite_history() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        %% 历史消息：一条客户入站 + 一条员工出站（带 actor）
        ok = append(Org, Ws, Conv, <<"eb06-ho-in">>, #{
            sender_type => contact, contact_id => Contact
        }),
        ok = append(Org, Ws, Conv, <<"eb06-ho-out">>, #{
            sender_type => business_identity, identity_id => Sales, actor_user_id => Actor
        }),
        Before = canonical_hashes(Org, Ws, Conv),
        ?assertEqual(2, maps:size(Before)),
        {ok, PreConv} = eb_pg_store:fetch_conversation(Org, Ws, Conv),
        ?assertEqual(Sales, maps:get(business_identity_id, PreConv)),
        AuditsBefore = ?FIX:count(Org, Ws, audits),
        %% EB-06 重开：交接**真做**（走 `update_conversation_assignee/4` 契约回调），
        %% 且**必须留审计**（action = conversation.handover）。
        {ok, Handover} = eb_conversation_app:handover_identity(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            to_identity_id => Service,
            actor_user_id => Actor
        }),
        ?assertEqual(Conv, maps:get(conversation_id, Handover)),
        ?assertEqual(Sales, maps:get(from_identity_id, Handover)),
        ?assertEqual(Service, maps:get(to_identity_id, Handover)),
        ?assert(is_integer(maps:get(audit_id, Handover))),
        ?assertEqual(false, maps:get(history_rewritten, Handover)),
        %% ① 当前经办确实换了（owner / resource id 不变）
        {ok, PostConv} = eb_pg_store:fetch_conversation(Org, Ws, Conv),
        ?assertEqual(Service, maps:get(business_identity_id, PostConv)),
        ?assertEqual(Org, maps:get(organization_id, PostConv)),
        ?assertEqual(Conv, maps:get(id, PostConv)),
        %% ② 审计**必须**存在（同事务通道；行数增量 ≥ 1）
        ?assert(?FIX:count(Org, Ws, audits) > AuditsBefore),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1"
                    "   AND resource_type='enterprise_conversation' AND resource_id=$2"
                    "   AND action='conversation.handover'"
                >>,
                [Org, Conv]
            )
        ),
        %% ③ 逐条比对历史 sender/actor 的 hash：必须逐字不变（不是「看起来没变」）
        After = canonical_hashes(Org, Ws, Conv),
        ?assertEqual(Before, After),
        lists:foreach(
            fun({MessageId, Hash}) ->
                ?assertEqual(Hash, maps:get(MessageId, After))
            end,
            maps:to_list(Before)
        ),
        %% domain 冻结婚自判据（集合语义）
        {ok, MsgRows} = eb_pg_store:list_messages(Org, Ws, Conv),
        ?assertEqual(ok, eb_message:handover_preserves_sender(MsgRows, MsgRows)),
        %% 反例控制：若有人把出站消息改写成新 identity（典型的「用改写历史伪装交接」），
        %% 判据必须抓住它——证明这条不变量有牙齿，而不是恒真。
        Rewritten = [
            case maps:get(client_msg_id, M) of
                <<"eb06-ho-out">> ->
                    M#{
                        sender_business_identity_id => Service,
                        actor_user_id => Actor
                    };
                _ ->
                    M
            end
         || M <- MsgRows
        ],
        ?assertEqual(
            {error, sender_rewritten_by_handover},
            eb_message:handover_preserves_sender(MsgRows, Rewritten)
        ),
        %% 同类判据对「丢消息」也成立
        ?assertEqual(
            {error, message_set_changed_by_handover},
            eb_message:handover_preserves_sender(MsgRows, tl(MsgRows))
        ),
        %% 静态：handover 路径不触消息表（本卡无任何 canonical message 的 UPDATE/DELETE）
        Source = module_code(eb_conversation_app),
        ?assertEqual(nomatch, re:run(Source, <<"elib_pg">>, [{capture, none}])),
        ?assertEqual(nomatch, re:run(Source, <<"DELETE FROM">>, [{capture, none}])),
        ?assertEqual(nomatch, re:run(Source, <<"UPDATE">>, [{capture, none}])),
        ?assertEqual(nomatch, re:run(Source, <<"append_message">>, [{capture, none}]))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A11（EB-06 重开新增）：交接与其审计**互相蕴含**
%% ===================================================================

%% ① 审计端口失败 ⇒ 交接**不得**生效（补偿回原经办），且不留下半条事实。
a11_handover_audit_failure_rolls_back_assignee() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        {ok, Pre} = eb_pg_store:fetch_conversation(Org, Ws, Conv),
        ?assertEqual(Sales, maps:get(business_identity_id, Pre)),
        ok = eb06_port_probe:reset(#{tx_mode => fail}),
        Result = eb_conversation_app:handover_identity(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            to_identity_id => Service,
            actor_user_id => Actor,
            tx => eb06_port_probe
        }),
        ?assertMatch({error, {handover_audit_failed, audit_down, compensated}}, Result),
        %% 交接**没有**生效：经办回到原值
        {ok, Post} = eb_pg_store:fetch_conversation(Org, Ws, Conv),
        ?assertEqual(Sales, maps:get(business_identity_id, Post)),
        %% 也没有 handover 审计
        ?assertEqual(
            0,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_id=$2 AND action='conversation.handover'"
                >>,
                [Org, Conv]
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ② 交接的审计必须走**用例级事务端口**（`eb_tx_port:append_conversation_audit/3`），
%%    不得绕到普通审计端口的独立写 —— 绕开即红。
a11_handover_audit_uses_usecase_tx_port_not_plain_audit_port() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ok = eb06_port_probe:reset(#{tx_mode => ok}),
        {ok, Handover} = eb_conversation_app:handover_identity(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            to_identity_id => Service,
            actor_user_id => Actor,
            audit => eb06_port_probe,
            tx => eb06_port_probe
        }),
        ?assert(is_integer(maps:get(audit_id, Handover))),
        %% 普通审计端口的独立写**一次都没有发生**；审计只经用例级事务端口。
        ?assertEqual(0, eb06_port_probe:count(plain_audit_append)),
        ?assertEqual(1, eb06_port_probe:count(tx_conversation_audit)),
        %% 负例（load-bearing）：把 tx 端口切到失败模式 ⇒ 交接必须失败，
        %% 证明上面的成功**依赖**于那次 tx 调用，而不是恒真断言。
        ok = eb06_port_probe:set_tx_mode(fail),
        ?assertMatch(
            {error, {handover_audit_failed, _, _}},
            eb_conversation_app:handover_identity(Org, #{
                workspace_id => Ws,
                conversation_id => Conv,
                to_identity_id => Sales,
                actor_user_id => Actor,
                audit => eb06_port_probe,
                tx => eb06_port_probe
            })
        ),
        ?assertEqual(0, eb06_port_probe:count(plain_audit_append))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A15（EB-06 重开新增）：默认 Workspace 消费只读事实 Port + 复用 BC-19 + 校验同 Org
%% ===================================================================

%% ① 事实源 = `eb_member_fact_port:default_workspace/2`（不再是「注入式伪造 fun」）：
%%    不给 `default_workspace`、只给成员身份 ⇒ 仍能解析出该 Org 的默认 Workspace。
a15_default_workspace_comes_from_member_fact_port() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Before = conversations_all(Org),
        %% 只经事实 Port 解析（**不注入** default_workspace）
        {ok, Opened} = eb_conversation_app:open_conversation(Org, #{
            workspace_id => Ws,
            contact_id => maps:get(contact_id, Scope),
            business_identity_id => maps:get(sales_identity_id, Scope),
            member_user_id => maps:get(actor_user_id, Scope),
            consent_at => now_secs(),
            actor_user_id => maps:get(actor_user_id, Scope)
        }),
        ?assertEqual(Ws, maps:get(workspace_id, Opened)),
        ?assertEqual(Before + 1, conversations_all(Org)),
        %% 事实 Port 的读数与解析结果一致（同一次解析的口径）
        {ok, FactsWs} = eb_member_fact_pg:default_workspace(Org, maps:get(actor_user_id, Scope)),
        ?assertEqual(Ws, FactsWs),
        %% BC-19：该 Workspace 能被 workspace_resolver 认成一个真实 workspace 资源
        ?assertEqual({ok, Ws}, workspace_resolver:resolve_workspace({workspace, Ws})),
        %% 静态：本模块确实引用 BC-19（复用而不是另造隐式推断）
        ?assertNotEqual(
            nomatch,
            re:run(module_code(eb_conversation_app), <<"workspace_resolver">>, [{capture, none}])
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ② 跨 Org 的默认 Workspace ⇒ 必须拒绝，且两个 Org 都不增行。
a15_cross_org_default_workspace_is_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Before = conversations_all(Org),
        OtherBefore = conversations_all(OtherOrg),
        ?assertNotEqual(Ws, OtherWs),
        Result = eb_conversation_app:open_conversation(Org, #{
            workspace_id => OtherWs,
            contact_id => maps:get(contact_id, Scope),
            business_identity_id => maps:get(sales_identity_id, Scope),
            member_user_id => maps:get(actor_user_id, Scope),
            consent_at => now_secs(),
            actor_user_id => maps:get(actor_user_id, Scope)
        }),
        %% 拒绝理由必须点名「不是默认 Workspace」（不得静默落到别的 Workspace）
        ?assertMatch({error, {not_default_workspace, OtherWs, _ServerDefault}}, Result),
        ?assertEqual(Before, conversations_all(Org)),
        ?assertEqual(OtherBefore, conversations_all(OtherOrg))
    after
        ?FIX:cleanup(Scope)
    end.

%% ③ BC-19 复用是**在路径里**的（不是装饰）：事实源给出的 Workspace 若无法被
%%    `workspace_resolver` 解析（不存在 / 不可归属）⇒ fail-closed，零写入。
a15_resolver_reuse_fails_closed_on_unresolvable_workspace() ->
    Scope = ?FIX:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Before = conversations_all(Org),
        NonExistent = ?FIX:id(),
        ?assertEqual(
            {error, not_found}, workspace_resolver:resolve_workspace({workspace, NonExistent})
        ),
        Result = eb_conversation_app:open_conversation(Org, #{
            workspace_id => NonExistent,
            contact_id => maps:get(contact_id, Scope),
            business_identity_id => maps:get(sales_identity_id, Scope),
            %% 事实源「恰好」解析到一个不存在的 Workspace（模拟脏事实/越权事实）
            default_workspace => fun(_OrgId) -> {ok, NonExistent} end,
            consent_at => now_secs(),
            actor_user_id => maps:get(actor_user_id, Scope)
        }),
        ?assertMatch({error, {default_workspace_resolver_failed, _}}, Result),
        ?assertEqual(Before, conversations_all(Org)),
        %% 负例对照：把同一个 id 换成真实存在的 Ws ⇒ 同一路径放行（证明判定不是恒假）
        {ok, _} = eb_conversation_app:open_conversation(Org, #{
            workspace_id => Ws,
            contact_id => maps:get(contact_id, Scope),
            business_identity_id => maps:get(sales_identity_id, Scope),
            default_workspace => fun(_OrgId) -> {ok, Ws} end,
            consent_at => now_secs(),
            actor_user_id => maps:get(actor_user_id, Scope)
        }),
        ?assertEqual(Before + 1, conversations_all(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

default_ws(Ws) ->
    fun(_OrgId) -> {ok, Ws} end.

now_secs() ->
    eb_system_clock:now().

open(Scope, Org, Ws) ->
    eb_conversation_app:open_conversation(Org, #{
        workspace_id => Ws,
        contact_id => maps:get(contact_id, Scope),
        business_identity_id => maps:get(sales_identity_id, Scope),
        default_workspace => default_ws(Ws),
        consent_at => now_secs(),
        actor_user_id => maps:get(actor_user_id, Scope)
    }).

append(Org, Ws, Conv, ClientMsgId, Sender) ->
    Params = maps:merge(
        #{
            workspace_id => Ws,
            conversation_id => Conv,
            client_msg_id => ClientMsgId,
            body => <<"eb06-handover-body-", ClientMsgId/binary>>,
            key_ref => ?FIX:key_ref(1),
            accepted_at => now_secs(),
            notify => fun(_Notification) -> ok end
        },
        Sender
    ),
    case eb_message_app:append_message(Org, Params) of
        {ok, Result} ->
            ?assertEqual(true, maps:get(accepted, Result)),
            ok;
        {error, Reason} ->
            erlang:error({append_failed, ClientMsgId, Reason})
    end.

%% 每条 canonical message 的 sender/actor 关键字段 hash（org/ws/sender/actor/retain_until）。
canonical_hashes(Org, Ws, Conv) ->
    {ok, Rows} = eb_pg_store:list_messages(Org, Ws, Conv),
    maps:from_list([
        {maps:get(id, Row), canonical_hash(Row)}
     || Row <- Rows
    ]).

canonical_hash(Row) ->
    Fields = [
        {K, maps:get(K, Row, undefined)}
     || K <- [
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
        ]
    ],
    binary:encode_hex(crypto:hash(sha256, term_to_binary(Fields)), lowercase).

second_workspace(Scope) ->
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Ws = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
            " VALUES ($1,$2,$3,'active','project',$4)"
        >>,
        [Ws, <<"eb06-ws-second-", (integer_to_binary(Ws))/binary>>, Owner, Org]
    ),
    Ws.

conversations_all(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM enterprise_conversation WHERE organization_id=$1">>,
        [Org],
        -1
    ).

%% 另一个 Org 的 active business identity（跨 Org 负例用；只有 id 与 Org 归属有意义）。
other_org_identity(Scope) ->
    OtherOrg = maps:get(other_org_id, Scope),
    IdentityId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id,organization_id,function_key,display_name,status,version)"
            " VALUES ($1,$2,'sales',$3,'active',1)"
        >>,
        [IdentityId, OtherOrg, <<"eb06-other-identity-", (integer_to_binary(IdentityId))/binary>>]
    ),
    IdentityId.

%% 个人世界的行数：企业动作前后必须逐字相等（增量 0）。
personal_table_counts() ->
    lists:foldl(
        fun(Table, Acc) ->
            Acc#{Table => ?FIX:scalar(<<"SELECT count(*) FROM ", Table/binary>>, [], -1)}
        end,
        #{},
        [
            <<"conversation">>,
            <<"msg_c2c">>,
            <<"msg_store">>,
            <<"user_friend">>,
            <<"attachment">>
        ]
    ).

module_code(Mod) ->
    {ok, Bin} = file:read_file(source_path(Mod)),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    iolist_to_binary([
        [re:replace(Line, <<"%.*$">>, <<>>, [{return, binary}]), <<"\n">>]
     || Line <- Lines
    ]).

source_path(Mod) ->
    Rel =
        "src/features/enterprise_business/application/conversation/" ++
            atom_to_list(Mod) ++ ".erl",
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
