%%% @doc CS-BE-02（队列摘要与等待时长）真库 focused 套件：坐席队列视图的
%%% `waiting_seconds` 权威值、`last_message.preview` 服务端解密截断、占位
%%% 语义与密文零出站——SQL LATERAL（cs_pg_session:SQL_SEAT_SESSION_PAGE）、
%%% 解密（cs_message_preview）与投影（cs_session_app）在真实 PG 上全链验证。
%%%
%%% 数据经真实写入路径落库（cs_session_app:open_session / append_session_message，
%%% 密钥用夹具合成 key_ref 显式注入），撤回/篡改行用夹具 SQL 直改（对应
%%% enterprise 侧 hide 与外部损坏的最小模拟）。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID scope；无真实数据。
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）：环境问题不得被当成 PASS。
%%% 本套件由 A0 在 scratch PG 队列独占运行；本地门禁只做编译自检。
-module(cs_queue_summary_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_queue_summary_pg_test_() ->
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
        {timeout, 60, fun waiting_seconds_authoritative_value/0},
        {timeout, 60, fun preview_truncated_from_last_text_message/0},
        {timeout, 60, fun attachment_only_and_no_message_placeholders/0},
        {timeout, 60, fun hidden_last_message_has_no_preview/0},
        {timeout, 60, fun tampered_cipher_degrades_preview_to_null/0},
        {timeout, 60, fun keyring_unavailable_degrades_preview_to_null/0},
        {timeout, 60, fun row_projection_never_carries_cipher_material/0}
    ];
cases({error, Reason}) ->
    erlang:error({csbe02_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% 用例
%% ===================================================================

%% waiting_seconds = at − queued_at（epoch 秒）：真库行（open_session 的
%% queued_at 落库回读）+ 服务端注入时钟；下限 0。
waiting_seconds_authoritative_value() ->
    Scope = ?FIX:new_scope(),
    try
        KeyRef = ?FIX:key_ref(),
        SessionId = open_queued_session(Scope, first_conversation(Scope), 1700000000),
        {ok, #{sessions := [Row]}} = seat_page(Scope, #{
            at => 1700000125, key_ref => KeyRef
        }),
        ?assertEqual(SessionId, maps:get(id, Row)),
        ?assertEqual(125, maps:get(waiting_seconds, Row)),
        %% 回拨/同秒竞争：不出现负等待。
        {ok, #{sessions := [Clamped]}} = seat_page(Scope, #{
            at => 1700000000, key_ref => KeyRef
        }),
        ?assertEqual(0, maps:get(waiting_seconds, Clamped)),
        %% 缺 at（非 HTTP 直驱）：键不出，页不失败。
        {ok, #{sessions := [NoClock]}} = seat_page(Scope, #{key_ref => KeyRef}),
        ?assertNot(is_map_key(waiting_seconds, NoClock))
    after
        ?FIX:cleanup(Scope)
    end.

%% preview 取末条**可见**消息解密后的前 64 个 Unicode 码点（UTF-8 安全）。
%% 期望值在测试内独立构造（3 ASCII + 61 CJK），不用被测 truncate 反推；
%% 末条语义：preview 始终跟随最新一条消息。
preview_truncated_from_last_text_message() ->
    Scope = ?FIX:new_scope(),
    try
        KeyRef = ?FIX:key_ref(),
        Chars = "abc" ++ lists:duplicate(70, $\x{5BA2}),
        Plain = unicode:characters_to_binary(Chars, utf8),
        Expected = unicode:characters_to_binary(lists:sublist(Chars, 64), utf8),
        SessionId = open_queued_session(Scope, first_conversation(Scope), 1700000000),
        append_visitor_text(Scope, SessionId, <<"csbe02-prev-1">>, Plain, KeyRef),
        {ok, #{sessions := [Row]}} = seat_page(Scope, #{
            at => 1700000100, key_ref => KeyRef
        }),
        LM = maps:get(last_message, Row),
        ?assertEqual(Expected, maps:get(preview, LM)),
        ?assertEqual(64, string:length(unicode:characters_to_list(maps:get(preview, LM), utf8))),
        %% 再发一条更短的：末条才是 preview 来源。
        append_visitor_text(Scope, SessionId, <<"csbe02-prev-2">>, <<"short tail">>, KeyRef),
        {ok, #{sessions := [Row2]}} = seat_page(Scope, #{
            at => 1700000100, key_ref => KeyRef
        }),
        ?assertEqual(<<"short tail">>, maps:get(preview, maps:get(last_message, Row2)))
    after
        ?FIX:cleanup(Scope)
    end.

%% 附件-only（空正文密文——BE-PATCH-01 附件消息的落库形态，直插行避免
%% 资产绑定链）：last_message 骨架在、preview 占位 null；无消息会话：
%% last_message 整体 undefined。
attachment_only_and_no_message_placeholders() ->
    Scope = ?FIX:new_scope(),
    try
        KeyRef = ?FIX:key_ref(),
        Conv2 = second_conversation(Scope),
        Conv3 = second_conversation(Scope),
        _AttachmentSession = open_queued_session(Scope, Conv2, 1700000000),
        _NoMessageSession = open_queued_session(Scope, Conv3, 1700000030),
        MessageId = insert_contact_message(Scope, Conv2, <<"csbe02-att-1">>, <<>>, KeyRef),
        {ok, #{sessions := Rows}} = seat_page(Scope, #{at => 1700000100, key_ref => KeyRef}),
        ByConv =
            maps:from_list([{maps:get(conversation_id, R), R} || R <- Rows]),
        AttRow = maps:get(Conv2, ByConv),
        AttLM = maps:get(last_message, AttRow),
        ?assertEqual(MessageId, maps:get(id, AttLM)),
        ?assertEqual(undefined, maps:get(preview, AttLM)),
        NoMsgRow = maps:get(Conv3, ByConv),
        ?assertEqual(undefined, maps:get(last_message, NoMsgRow))
    after
        ?FIX:cleanup(Scope)
    end.

%% 撤回（visibility='hidden'）：末条摘要骨架（id/sender_type/created_at）保留，
%% 密文三列在 SQL 侧已置 NULL——preview 零透出。
hidden_last_message_has_no_preview() ->
    Scope = ?FIX:new_scope(),
    try
        KeyRef = ?FIX:key_ref(),
        SessionId = open_queued_session(Scope, first_conversation(Scope), 1700000000),
        append_visitor_text(
            Scope, SessionId, <<"csbe02-hide-1">>, <<"will be hidden"/utf8>>, KeyRef
        ),
        MessageId =
            ?FIX:scalar(
                <<
                    "SELECT id FROM enterprise_message"
                    " WHERE organization_id = $1 AND client_msg_id = 'csbe02-hide-1'"
                >>,
                [org(Scope)]
            ),
        ok = ?FIX:exec(
            <<"UPDATE enterprise_message SET visibility = 'hidden' WHERE id = $1">>,
            [MessageId]
        ),
        {ok, #{sessions := [Row]}} = seat_page(Scope, #{at => 1700000100, key_ref => KeyRef}),
        LM = maps:get(last_message, Row),
        ?assertEqual(MessageId, maps:get(id, LM)),
        ?assertEqual(undefined, maps:get(preview, LM))
    after
        ?FIX:cleanup(Scope)
    end.

%% 密文被篡改/解不开（F-R5）：该行 preview 降级 null（骨架 id/sender_type/
%% created_at 保留），**页不失败**（队列 200 语义——单条旧密钥遗留消息不得
%% 拖垮整页坐席队列）；同页好密文行 preview 不受影响（混合页各归各位）。
tampered_cipher_degrades_preview_to_null() ->
    Scope = ?FIX:new_scope(),
    try
        KeyRef = ?FIX:key_ref(),
        BadConv = second_conversation(Scope),
        GoodConv = second_conversation(Scope),
        _BadSession = open_queued_session(Scope, BadConv, 1700000000),
        GoodSessionId = open_queued_session(Scope, GoodConv, 1700000010),
        BadMessageId =
            insert_contact_message(
                Scope, BadConv, <<"csbe02-tamper-1">>, <<"tamper me"/utf8>>, KeyRef
            ),
        ok = ?FIX:exec(
            <<
                "UPDATE enterprise_message SET body_cipher = 'deadbeef-not-a-cipher'"
                " WHERE organization_id = $1 AND client_msg_id = 'csbe02-tamper-1'"
            >>,
            [org(Scope)]
        ),
        append_visitor_text(Scope, GoodSessionId, <<"csbe02-tamper-2">>, <<"good tail">>, KeyRef),
        {ok, #{sessions := Rows}} = seat_page(Scope, #{at => 1700000100, key_ref => KeyRef}),
        ByConv = maps:from_list([{maps:get(conversation_id, R), R} || R <- Rows]),
        %% 坏密文行：页不失败，preview null，末条骨架保留。
        BadLM = maps:get(last_message, maps:get(BadConv, ByConv)),
        ?assertEqual(BadMessageId, maps:get(id, BadLM)),
        ?assertEqual(undefined, maps:get(preview, BadLM)),
        %% 同页好密文行：preview 正常出站。
        GoodLM = maps:get(last_message, maps:get(GoodConv, ByConv)),
        ?assertEqual(<<"good tail">>, maps:get(preview, GoodLM))
    after
        ?FIX:cleanup(Scope)
    end.

%% keyring 未装配（无显式 key_ref 且 env 无 keyring）：整页成功、preview
%% null 降级（不吐密文、不报错——D5 的降级口径）。
keyring_unavailable_degrades_preview_to_null() ->
    Scope = ?FIX:new_scope(),
    OldKeyring = application:get_env(imboy, eb_enterprise_keyring),
    try
        application:unset_env(imboy, eb_enterprise_keyring),
        KeyRef = ?FIX:key_ref(),
        SessionId = open_queued_session(Scope, first_conversation(Scope), 1700000000),
        append_visitor_text(
            Scope, SessionId, <<"csbe02-nokey-1">>, <<"invisible without keyring"/utf8>>, KeyRef
        ),
        {ok, #{sessions := [Row]}} = seat_page(Scope, #{at => 1700000100}),
        LM = maps:get(last_message, Row),
        ?assert(is_integer(maps:get(id, LM))),
        ?assertEqual(undefined, maps:get(preview, LM)),
        ?assertEqual(100, maps:get(waiting_seconds, Row))
    after
        case OldKeyring of
            undefined -> application:unset_env(imboy, eb_enterprise_keyring);
            {ok, V} -> application:set_env(imboy, eb_enterprise_keyring, V)
        end,
        ?FIX:cleanup(Scope)
    end.

%% 出站行白名单逐字：密文绝不出站——密文材料只在 enterprise 侧中转
%% （preview 经 enterprise_business_facade:list_messages 读面解密），客服
%% 侧 store 行与出站视图行都不携带任何密文列。
row_projection_never_carries_cipher_material() ->
    Scope = ?FIX:new_scope(),
    try
        KeyRef = ?FIX:key_ref(),
        SessionId = open_queued_session(Scope, first_conversation(Scope), 1700000000),
        append_visitor_text(Scope, SessionId, <<"csbe02-wl-1">>, <<"whitelist probe">>, KeyRef),
        %% store 原始行带末条摘要原料（id/sender_type/created_at），无密文列。
        {ok, #{rows := [StoreRow]}} =
            cs_pg_store:seat_session_page(org(Scope), <<"queued">>, 0, 10, ws(Scope)),
        ?assert(is_integer(maps:get(last_message_id, StoreRow))),
        ?assertEqual(<<"contact">>, maps:get(last_message_sender_type, StoreRow)),
        %% 出站视图：行键 = 白名单 + source/contact/last_message/waiting_seconds。
        {ok, #{sessions := [Row]}} = seat_page(Scope, #{at => 1700000100, key_ref => KeyRef}),
        ExpectedKeys =
            [
                id,
                organization_id,
                workspace_id,
                contact_id,
                conversation_id,
                business_identity_id,
                status,
                version,
                queued_at,
                claimed_at,
                closed_at,
                source,
                contact,
                last_message,
                waiting_seconds
            ],
        ?assertEqual(lists:sort(ExpectedKeys), lists:sort(maps:keys(Row))),
        LM = maps:get(last_message, Row),
        ?assertEqual([created_at, id, preview, sender_type], lists:sort(maps:keys(LM))),
        RedLines = [
            last_message_body_cipher,
            last_message_key_version,
            last_message_aad_hash,
            body_cipher,
            key_version,
            aad_hash,
            client_msg_id,
            key_ref,
            secret,
            visit_token_id,
            close_reason
        ],
        lists:foreach(fun(K) -> ?assertNot(is_map_key(K, Row)) end, RedLines),
        lists:foreach(fun(K) -> ?assertNot(is_map_key(K, LM)) end, RedLines),
        %% store 行允许携带服务端内部列（visit_token_id 等），但密文材料
        %% 一概不得出现（本设计里密文只在 enterprise 侧中转）。
        lists:foreach(
            fun(K) -> ?assertNot(is_map_key(K, StoreRow)) end,
            [last_message_body_cipher, last_message_key_version, last_message_aad_hash, body_cipher]
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 夹具辅助
%% ===================================================================

org(Scope) ->
    maps:get(org_id, Scope).

ws(Scope) ->
    maps:get(workspace_id, Scope).

first_conversation(Scope) ->
    maps:get(conversation_id, Scope).

seat_page(Scope, Opts) ->
    cs_session_app:seat_session_page(
        org(Scope),
        maps:merge(
            #{
                workspace_id => ws(Scope),
                status => <<"queued">>,
                limit => 10
            },
            Opts
        )
    ).

open_queued_session(Scope, ConversationId, QueuedAt) ->
    {ok, Session} = cs_session_app:open_session(org(Scope), #{
        workspace_id => ws(Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => ConversationId,
        at => QueuedAt
    }),
    maps:get(id, Session).

%% 访客入站文本（sender=contact，与队列场景同型；密钥显式注入）。
append_visitor_text(Scope, SessionId, ClientMsgId, Body, KeyRef) ->
    {ok, _} = cs_session_app:append_session_message(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => SessionId,
        contact_id => maps:get(contact_id, Scope),
        client_msg_id => ClientMsgId,
        body => Body,
        key_ref => KeyRef,
        accepted_at => 1700000050,
        notify => fun(_) -> ok end
    }),
    ok.

%% 直插一条 contact 入站消息行（canonical 事务的最小同构：密文三列由
%% eb_managed_crypto:seal 按同口径构造；retention 快照取夹具同值）。
%% 返回消息 id。附件-only 用 Body = <<>>（空正文密文）。
insert_contact_message(Scope, ConversationId, ClientMsgId, Body, KeyRef) ->
    MessageId = ?FIX:id(),
    Aad = #{
        organization_id => org(Scope),
        workspace_id => ws(Scope),
        conversation_id => ConversationId,
        message_id => MessageId
    },
    {ok, Sealed} = eb_managed_crypto:seal(Aad, Body, KeyRef),
    Cipher = maps:get(cipher, Sealed),
    ContentHash = binary:encode_hex(crypto:hash(sha256, Cipher), lowercase),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_message"
            " (id, organization_id, workspace_id, conversation_id, sender_type,"
            "  sender_contact_id, client_msg_id, body_cipher, key_version, aad_hash,"
            "  content_hash, retention_days, retain_until)"
            " VALUES ($1,$2,$3,$4,'contact',$5,$6,$7,$8,$9,$10,1095,CURRENT_TIMESTAMP + interval '1095 days')"
        >>,
        [
            MessageId,
            org(Scope),
            ws(Scope),
            ConversationId,
            maps:get(contact_id, Scope),
            ClientMsgId,
            Cipher,
            maps:get(key_version, Sealed),
            maps:get(aad_hash, Sealed),
            ContentHash
        ]
    ),
    MessageId.

second_conversation(Scope) ->
    ConversationId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_conversation"
            " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
            "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
            " VALUES ($1,$2,$3,$4,$5,'active',1,'cs01-notice-v1',CURRENT_TIMESTAMP,$6,'synthetic')"
        >>,
        [
            ConversationId,
            org(Scope),
            ws(Scope),
            maps:get(contact_id, Scope),
            maps:get(service_identity_id, Scope),
            <<"csbe02-consent-", (integer_to_binary(ConversationId))/binary>>
        ]
    ),
    ConversationId.
