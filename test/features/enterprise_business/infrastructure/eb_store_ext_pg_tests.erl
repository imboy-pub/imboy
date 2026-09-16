%%% @doc EB-03R P1..P9 + M2 套件：新并入契约的 store 能力与同意证据写入路径。
%%%
%%% 覆盖（**正向**：真实 PG 往返，写入后可读回）：
%%%   P1 insert_assignment   P2 list_identities     P3 insert_note
%%%   P4 insert_contact_assignment                   P5 update_conversation_assignee
%%%   P7 list_contacts / update_contact              P8 list_conversations
%%%   P9 list_messages_after（**键集**分页，非 offset）
%%%   M2 consent_evidence_kind（只写 'synthetic'；'real' 由 DB 拒绝）
-module(eb_store_ext_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

store_ext_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun p1_insert_assignment_round_trips/0},
        {timeout, 60, fun p1_second_active_assignment_conflicts/0},
        {timeout, 60, fun p2_list_identities_returns_org_identities/0},
        {timeout, 60, fun p3_insert_note_round_trips/0},
        {timeout, 60, fun p4_insert_contact_assignment_round_trips/0},
        {timeout, 60, fun p5_update_conversation_assignee_round_trips/0},
        {timeout, 60, fun p7_list_and_patch_contact/0},
        {timeout, 60, fun p8_list_conversations_returns_org_workspace_rows/0},
        {timeout, 60, fun p9_list_messages_after_is_keyset_pagination/0},
        {timeout, 60, fun p9_statement_never_uses_offset/0},
        {timeout, 60, fun m2_consent_evidence_kind_is_synthetic_only/0},
        {timeout, 60, fun m2_real_consent_value_is_rejected_by_db/0}
    ];
cases(_Skipped) ->
    {skip, "store ext suite requires the scratch database connection"}.

%% P1
p1_insert_assignment_round_trips() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AssignmentId = eb_pg_test_fixture:id(),
        {ok, Row} = eb_pg_store:insert_assignment(Org, Ws, #{
            id => AssignmentId,
            business_identity_id => maps:get(service_identity_id, Scope),
            function_key => <<"customer_service">>,
            user_id => maps:get(owner_user_id, Scope),
            assigned_by => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(AssignmentId, maps:get(id, Row)),
        ?assertEqual(active, maps:get(status, Row)),
        ?assertEqual(1, maps:get(version, Row)),
        ?assertEqual(<<"customer_service">>, maps:get(function_key, Row))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

p1_second_active_assignment_conflicts() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Params = #{
            id => eb_pg_test_fixture:id(),
            business_identity_id => maps:get(service_identity_id, Scope),
            function_key => <<"customer_service">>,
            user_id => maps:get(owner_user_id, Scope)
        },
        {ok, _} = eb_pg_store:insert_assignment(Org, Ws, Params),
        ?assertEqual(
            {error, conflict},
            eb_pg_store:insert_assignment(Org, Ws, Params#{id := eb_pg_test_fixture:id()})
        ),
        %% fixture 里 sales identity 已有 active 经办 ⇒ 不得再绑一个 active
        ?assertEqual(
            {error, conflict},
            eb_pg_store:insert_assignment(Org, Ws, #{
                id => eb_pg_test_fixture:id(),
                business_identity_id => maps:get(sales_identity_id, Scope),
                function_key => <<"sales">>,
                user_id => maps:get(peer_user_id, Scope)
            })
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% P2
p2_list_identities_returns_org_identities() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Identities} = eb_pg_store:list_identities(Org, Ws),
        Keys = lists:sort([maps:get(function_key, I) || I <- Identities]),
        ?assertEqual([<<"customer_service">>, <<"sales">>], Keys),
        lists:foreach(
            fun(I) ->
                ?assertEqual(Org, maps:get(organization_id, I)),
                ?assertEqual(Ws, maps:get(workspace_id, I))
            end,
            Identities
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% P3
p3_insert_note_round_trips() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        NoteId = eb_pg_test_fixture:id(),
        {ok, Row} = eb_pg_store:insert_note(Org, Ws, #{
            id => NoteId,
            contact_id => maps:get(contact_id, Scope),
            business_identity_id => maps:get(sales_identity_id, Scope),
            actor_user_id => maps:get(actor_user_id, Scope),
            body_cipher => <<"eb03r-note-cipher">>,
            body_key_version => 1
        }),
        ?assertEqual(NoteId, maps:get(id, Row)),
        ?assertEqual(active, maps:get(status, Row)),
        ?assertEqual(<<"eb03r-note-cipher">>, maps:get(body_cipher, Row)),
        %% 密文与 key_version 成对（DB CHECK 同口径）：只给密文不给版本必须失败
        ?assertMatch(
            {error, _},
            eb_pg_store:insert_note(Org, Ws, #{
                id => eb_pg_test_fixture:id(),
                contact_id => maps:get(contact_id, Scope),
                body_cipher => <<"cipher-without-version">>
            })
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% P4
p4_insert_contact_assignment_round_trips() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Id = eb_pg_test_fixture:id(),
        {ok, Row} = eb_pg_store:insert_contact_assignment(Org, Ws, #{
            id => Id,
            contact_id => maps:get(contact_id, Scope),
            business_identity_id => maps:get(sales_identity_id, Scope),
            role => <<"primary">>,
            assigned_by => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(Id, maps:get(id, Row)),
        ?assertEqual(active, maps:get(status, Row)),
        ?assertEqual(<<"primary">>, maps:get(role, Row))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% P5
p5_update_conversation_assignee_round_trips() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        NewIdentity = maps:get(service_identity_id, Scope),
        {ok, Before} = eb_pg_store:fetch_conversation(Org, Ws, Conv),
        {ok, After} = eb_pg_store:update_conversation_assignee(Org, Ws, Conv, NewIdentity),
        ?assertEqual(NewIdentity, maps:get(business_identity_id, After)),
        ?assertEqual(maps:get(version, Before) + 1, maps:get(version, After)),
        %% 不存在的 identity ⇒ conflict（不得静默保留旧 identity）
        ?assertEqual(
            {error, conflict},
            eb_pg_store:update_conversation_assignee(Org, Ws, Conv, maps:get(peer_user_id, Scope))
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% P7
p7_list_and_patch_contact() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Contact = maps:get(contact_id, Scope),
        {ok, Contacts} = eb_pg_store:list_contacts(Org, Ws),
        ?assertEqual(1, length(Contacts)),
        ?assertEqual(Contact, maps:get(id, hd(Contacts))),
        %% PATCH：只改白名单字段，版本推进；不存在的行 not_found
        {ok, Patched} = eb_pg_store:update_contact(Org, Ws, #{
            id => Contact,
            display_name => <<"eb03r-renamed-contact">>
        }),
        ?assertEqual(<<"eb03r-renamed-contact">>, maps:get(display_name, Patched)),
        ?assertEqual(2, maps:get(version, Patched)),
        %% 不可变字段（id / organization_id）不可能被 PATCH 改动
        {ok, Stable} = eb_pg_store:update_contact(Org, Ws, #{
            id => Contact,
            organization_id => maps:get(other_org_id, Scope)
        }),
        ?assertEqual(Org, maps:get(organization_id, Stable)),
        ?assertEqual(
            {error, not_found},
            eb_pg_store:update_contact(Org, Ws, #{id => Contact + 777, display_name => <<"x">>})
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% P8
p8_list_conversations_returns_org_workspace_rows() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        {ok, Conversations} = eb_pg_store:list_conversations(Org, Ws),
        ?assertEqual(1, length(Conversations)),
        ?assertEqual(maps:get(conversation_id, Scope), maps:get(id, hd(Conversations))),
        %% 跨租户（同 Org 配另一个 Workspace）必须为空
        {ok, Empty} = eb_pg_store:list_conversations(Org, maps:get(other_workspace_id, Scope)),
        ?assertEqual([], Empty)
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% P9：键集分页（after_id 严格大于）
p9_list_messages_after_is_keyset_pagination() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Now = eb_system_clock:now(),
        [M1, M2, M3] = [
            insert_message(Scope, <<"eb03r-page-", (integer_to_binary(N))/binary>>, Now + 86400)
         || N <- [1, 2, 3]
        ],
        Sorted = lists:sort([M1, M2, M3]),
        Conv = maps:get(conversation_id, Scope),
        {ok, Page1} = eb_pg_store:list_messages_after(Org, Ws, #{
            conversation_id => Conv, limit => 2
        }),
        ?assertEqual(2, length(Page1)),
        ?assertEqual(lists:sublist(Sorted, 2), [maps:get(id, M) || M <- Page1]),
        Cursor = maps:get(id, lists:last(Page1)),
        {ok, Page2} = eb_pg_store:list_messages_after(Org, Ws, #{
            conversation_id => Conv, limit => 2, after_id => Cursor
        }),
        ?assertEqual(1, length(Page2)),
        ?assertEqual(lists:last(Sorted), maps:get(id, hd(Page2))),
        %% 游标语义是**严格大于**：把游标推到最大 id 之后必须为空
        {ok, Empty} = eb_pg_store:list_messages_after(Org, Ws, #{
            conversation_id => Conv, after_id => lists:last(Sorted)
        }),
        ?assertEqual([], Empty),
        %% 键集分页的韧性（与 offset 的差别）：同一游标下**追加**新行不产生跳行/重复，
        %% 也不改变已返回页的内容（offset 分页会因总行数变化而漂移）。
        Appended = insert_message(Scope, <<"eb03r-page-4">>, Now + 86400),
        ?assert(lists:member(Appended, Sorted) =:= false),
        {ok, Page2Again} = eb_pg_store:list_messages_after(Org, Ws, #{
            conversation_id => Conv, limit => 2, after_id => Cursor
        }),
        ?assertEqual(
            lists:sort([lists:last(Sorted), Appended]),
            [maps:get(id, M) || M <- Page2Again]
        ),
        {ok, Page1Again} = eb_pg_store:list_messages_after(Org, Ws, #{
            conversation_id => Conv, limit => 2
        }),
        ?assertEqual(lists:sublist(Sorted, 2), [maps:get(id, M) || M <- Page1Again]),
        %% limit 越界 fail-closed
        ?assertMatch(
            {error, {invalid_limit, _}},
            eb_pg_store:list_messages_after(Org, Ws, #{conversation_id => Conv, limit => 0})
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

p9_statement_never_uses_offset() ->
    Statements = eb_pg_message_ext:sql_statements(),
    ?assert(length(Statements) >= 1),
    %% 分页语句 = 命中 enterprise_message 的那条；其余是租户自检语句。
    PageStatements = [
        Sql
     || Sql <- Statements,
        binary:match(Sql, <<"FROM enterprise_message">>) =/= nomatch
    ],
    ?assertEqual(1, length(PageStatements)),
    lists:foreach(
        fun(Sql) ->
            Upper = string:uppercase(binary_to_list(Sql)),
            ?assertEqual(nomatch, string:find(Upper, "OFFSET")),
            ?assertNotEqual(nomatch, string:find(Upper, "ID > $4")),
            ?assertNotEqual(nomatch, string:find(Upper, "LIMIT $5")),
            ?assertNotEqual(nomatch, string:find(Upper, "ORDER BY ID"))
        end,
        PageStatements
    ),
    %% 每条语句都带双租户键（本 Feature 的 store 纪律）
    lists:foreach(
        fun(Sql) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"organization_id">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"workspace_id">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"$1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"$2">>))
        end,
        Statements
    ).

%% M2：同意证据类别（只写 'synthetic'；无 consent 不得伪装）
m2_consent_evidence_kind_is_synthetic_only() ->
    Scope = eb_pg_test_fixture:new_scope(),
    NoConsent = eb_pg_test_fixture:new_scope(#{with_consent => false}),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        ok = eb_pg_consent_evidence:record_synthetic_consent(Org, Ws, Conv),
        {ok, Row} = eb_pg_consent_evidence:fetch_consent_evidence_kind(Org, Ws, Conv),
        ?assertEqual(synthetic, maps:get(consent_evidence_kind, Row)),
        %% 幂等：再次记录不改变事实
        ?assertEqual(ok, eb_pg_consent_evidence:record_synthetic_consent(Org, Ws, Conv)),
        %% 无 consent 的会话：必须失败（不得把「无 consent」伪装成「有证据」）
        ?assertEqual(
            {error, no_consent},
            eb_pg_consent_evidence:record_synthetic_consent(
                maps:get(org_id, NoConsent),
                maps:get(workspace_id, NoConsent),
                maps:get(conversation_id, NoConsent)
            )
        ),
        %% 无 consent 会话该列必须是 NULL（M2-a）
        ?assertEqual(
            0,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM enterprise_conversation"
                    " WHERE organization_id=$1 AND consent_at IS NULL"
                    "   AND consent_evidence_kind IS NOT NULL"
                >>,
                [maps:get(org_id, NoConsent)]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope),
        eb_pg_test_fixture:cleanup(NoConsent)
    end.

%% 'real' 由 DB 拒绝（不是应用层拒绝）——A07 的机械判定之一。
m2_real_consent_value_is_rejected_by_db() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Result = elib_pg:execute(
            <<
                "UPDATE enterprise_conversation SET consent_evidence_kind='real'"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, Conv]
        ),
        ?assertMatch({error, _}, Result),
        %% CHECK 定义里非空分支只有 'synthetic'
        Def = eb_pg_test_fixture:scalar(
            <<
                "SELECT pg_get_constraintdef(oid) FROM pg_constraint"
                " WHERE conname='ck_ec_consent_evidence_kind'"
            >>,
            []
        ),
        ?assertNotEqual(undefined, Def),
        ?assertNotEqual(nomatch, binary:match(Def, <<"'synthetic'">>)),
        ?assertEqual(nomatch, binary:match(Def, <<"'real'">>)),
        ?assertEqual(nomatch, binary:match(Def, <<"verified_real">>))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

insert_message(Scope, ClientMsgId, RetainUntil) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    MsgId = eb_pg_test_fixture:id(),
    Aad = #{
        organization_id => Org, workspace_id => Ws, conversation_id => Conv, message_id => MsgId
    },
    {ok, Sealed} = eb_managed_crypto:seal(
        Aad, <<"eb03r-page-body">>, eb_pg_test_fixture:key_ref(1)
    ),
    {ok, _Row} = eb_pg_store:append_message(Org, Ws, #{
        id => MsgId,
        conversation_id => Conv,
        client_msg_id => ClientMsgId,
        sender_type => <<"contact">>,
        sender_contact_id => maps:get(contact_id, Scope),
        sender_business_identity_id => null,
        actor_user_id => null,
        body_cipher => maps:get(cipher, Sealed),
        key_version => maps:get(key_version, Sealed),
        aad_hash => maps:get(aad_hash, Sealed),
        content_hash => binary:encode_hex(crypto:hash(sha256, maps:get(cipher, Sealed)), lowercase),
        policy_id => maps:get(policy_id, Scope),
        policy_version => 1,
        retention_days => 1095,
        retain_until => RetainUntil
    }),
    MsgId.
