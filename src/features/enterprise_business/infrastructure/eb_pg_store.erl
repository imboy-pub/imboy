%%% @doc 企业业务持久化的 PostgreSQL store（`eb_store_port` 的真实现）。
%%%
%%% 依据：plan v4.1 §4.1 数据合同、§4.3 数据不变量、EB-03 §3.1/§3.3（A01/A02/A03）。
%%% 冻结契约：EB-02 的 `application/eb_store_port.erl`（本模块声明同一 behaviour）。
%%%
%%% 铁律 6（租户作用域显式贯穿）在本模块的落地方式：
%%%   * 每个资源级函数的**前两个业务参数**都是 `OrgId` / `WorkspaceId`；两者必须是
%%%     整数，否则立即 `{error, {invalid_tenant, ...}}`（不触库）。
%%%   * **每一条 SQL 都同时带 `organization_id` 与 `workspace_id`**（见
%%%     `sql_statements/0`；写入类语句把「Workspace 必须属于该 Org」写进同一条
%%%     语句的 WHERE 子句，而不是先查后拼）。
%%%   * 部分企业表（`enterprise_contact` / `enterprise_contact_identity` /
%%%     `organization_business_identity` / `..._assignment`）按 EB-01 的 schema 没有
%%%     workspace 列，因此用 `workspace` 表在同一语句里做归属校验，绝不退化成
%%%     「只按 organization_id 查」。
%%%
%%% 铁律 7（并发状态迁移）：`advance_assignment/5` 是 CAS——`UPDATE ... WHERE status = $4`
%%% 且仅当影响行数为 1 时返回 `ok`，否则 `{error, conflict}`（由 DB 行锁裁决，见
%%% 套件的 8 进程真并发用例）。
%%%
%%% 返回形态：面向调用方（application/domain）统一为**原子键 map**，
%%% timestamptz 一律归一为 **Unix 秒整数**，NULL 一律归一为 `undefined`——
%%% 与 EB-02 的 domain 纯函数（`eb_message` / `eb_retention` / `eb_identity`）的
%%% 输入约定逐字对齐。
-module(eb_pg_store).

-behaviour(eb_store_port).

-include_lib("epgsql/include/epgsql.hrl").

-export([
    %% eb_store_port
    fetch_identity/3,
    insert_identity/3,
    fetch_conversation/3,
    insert_conversation/3,
    conversation_handler_identity/2,
    append_message/3,
    advance_assignment/5,
    list_assignments/2,
    %% 同一 store 的其余资源写读（canonical 事务 / EB-05+ 复用；同样租户贯穿）
    insert_contact/3,
    fetch_contact/3,
    insert_contact_identity/3,
    fetch_contact_identity/3,
    insert_policy/3,
    latest_policy/3,
    insert_hold/3,
    release_hold/4,
    list_active_holds/2,
    %% EB-03R P6：`fetch_hold/3` 此前**已定义未导出**（R0 的 C2），四件齐后补齐导出。
    fetch_hold/3,
    fetch_message/3,
    list_messages/3,
    ack_delivery/3,
    append_message_in/4,
    fetch_conversation_in/4,
    latest_policy_in/4,
    %% EB-03R：契约面补齐后的新增能力（实现下沉到各 eb_pg_*_ext 模块，本模块只做
    %% Port 实现的唯一入口——`eb_infra_ports:resolve(store)` 返回的模块必须实现
    %% `eb_store_port` 的全部 callback）。
    list_messages_after/3,
    insert_assignment/3,
    list_identities/2,
    list_identities_page/3,
    insert_note/3,
    insert_contact_assignment/3,
    update_conversation_assignee/4,
    list_contacts/2,
    update_contact/3,
    list_conversations/2,
    insert_offboarding_case/3,
    fetch_offboarding_case/3,
    list_offboarding_cases/2,
    insert_offboarding_item/3,
    list_offboarding_items/3,
    update_offboarding_item/3,
    advance_offboarding_case/6,
    update_offboarding_case_counts/6,
    sql_statements/0
]).

%% SQL 语句与行归一化下沉到 eb_pg_store_sql（F1/EB-03R：让本文件 < 800 行）。
%% 行为保持：语句文本与 normalize/字段规格逐字相同，仅由宏改为函数调用。

%% ===================================================================
%% eb_store_port：identity
%% ===================================================================

-spec fetch_identity(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_identity(OrgId, WorkspaceId, IdentityId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_one(
            eb_pg_store_sql:sql(fetch_identity),
            [OrgId, WorkspaceId, IdentityId],
            eb_pg_store_sql:identity_fields()
        )
    end).

-spec insert_identity(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_identity(OrgId, WorkspaceId, Identity) when is_map(Identity) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Identity, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            Id,
            maps:get(function_key, Identity, undefined),
            maps:get(display_name, Identity, undefined),
            eb_pg_store_sql:nullify(maps:get(created_by_user_id, Identity, undefined))
        ],
        insert_then_fetch(
            eb_pg_store_sql:sql(insert_identity),
            Params,
            fun() ->
                conflict_or(eb_pg_store_sql:sql(scope_ok), [OrgId, WorkspaceId], WorkspaceId)
            end,
            fun() -> fetch_identity(OrgId, WorkspaceId, Id) end
        )
    end);
insert_identity(_OrgId, _WorkspaceId, _Identity) ->
    {error, invalid_identity}.

%% ===================================================================
%% eb_store_port：conversation
%% ===================================================================

-spec fetch_conversation(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_conversation(OrgId, WorkspaceId, ConversationId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_one(
            eb_pg_store_sql:sql(fetch_conversation),
            [OrgId, WorkspaceId, ConversationId],
            eb_pg_store_sql:conversation_fields()
        )
    end).

%% @doc 会话经办 `business_identity_id` 的轻量只读（EB-01 / A01.36 方案 a）。
%% 授权层 hint 预取专用：无 Workspace 语义（会话 id 为 TSID 主键 + Org 双键
%% 防跨租户），主键索引一次取行；**零写入**，不参与任何事务提交路径。
-spec conversation_handler_identity(integer(), integer()) ->
    {ok, #{business_identity_id => integer()}} | {error, term()}.
conversation_handler_identity(OrgId, ConversationId) when
    is_integer(OrgId), is_integer(ConversationId)
->
    fetch_one(
        eb_pg_store_sql:sql(conversation_handler_identity),
        [OrgId, ConversationId],
        [{business_identity_id, <<"business_identity_id">>, int}]
    ).

%% @doc 在调用方事务内读取会话（canonical 事务用；同样租户贯穿）。
-spec fetch_conversation_in(term(), integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
fetch_conversation_in(Conn, OrgId, WorkspaceId, ConversationId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        case
            elib_pg:query(Conn, eb_pg_store_sql:sql(fetch_conversation), [
                OrgId, WorkspaceId, ConversationId
            ])
        of
            {ok, [Row | _]} ->
                {ok, eb_pg_store_sql:normalize(Row, eb_pg_store_sql:conversation_fields())};
            {ok, []} ->
                {error, not_found};
            {error, Reason} ->
                {error, eb_pg_store_sql:normalize_error(Reason)}
        end
    end).

%% @doc 在调用方事务内取当前生效的 retention policy 版本（canonical 事务用）。
-spec latest_policy_in(term(), integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
latest_policy_in(Conn, OrgId, WorkspaceId, DataClass) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        case
            elib_pg:query(Conn, eb_pg_store_sql:sql(latest_policy), [OrgId, WorkspaceId, DataClass])
        of
            {ok, [Row | _]} ->
                {ok, eb_pg_store_sql:normalize(Row, eb_pg_store_sql:policy_fields())};
            {ok, []} ->
                {error, not_found};
            {error, Reason} ->
                {error, eb_pg_store_sql:normalize_error(Reason)}
        end
    end).

-spec insert_conversation(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_conversation(OrgId, WorkspaceId, Conversation) when is_map(Conversation) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        do_insert_conversation(OrgId, WorkspaceId, Conversation)
    end);
insert_conversation(_OrgId, _WorkspaceId, _Conversation) ->
    {error, invalid_conversation}.

do_insert_conversation(OrgId, WorkspaceId, Conversation) ->
    Id = maps:get(id, Conversation, undefined),
    ContactId = maps:get(contact_id, Conversation, undefined),
    IdentityId = maps:get(business_identity_id, Conversation, undefined),
    ConsentAtMs = eb_pg_store_sql:ms_or_null(maps:get(consent_at, Conversation, undefined)),
    Params = [
        OrgId,
        WorkspaceId,
        Id,
        ContactId,
        IdentityId,
        eb_pg_store_sql:nullify(maps:get(notice_version, Conversation, undefined)),
        ConsentAtMs,
        eb_pg_store_sql:nullify(maps:get(consent_subject, Conversation, undefined)),
        eb_pg_store_sql:nullify(maps:get(consent_evidence_kind, Conversation, undefined))
    ],
    case execute_insert(eb_pg_store_sql:sql(insert_conversation), Params) of
        {ok, _Inserted} ->
            fetch_conversation(OrgId, WorkspaceId, Id);
        {error, no_row} ->
            diagnose_conversation_scope(OrgId, WorkspaceId, ContactId, IdentityId);
        {error, _} = Err ->
            Err
    end.

diagnose_conversation_scope(OrgId, WorkspaceId, ContactId, IdentityId) ->
    case scope_ok(OrgId, WorkspaceId) of
        false ->
            {error, {workspace_not_in_org, WorkspaceId}};
        true ->
            case
                exists_check(eb_pg_store_sql:sql(exists_contact), [OrgId, WorkspaceId, ContactId])
            of
                false ->
                    {error, {contact_not_in_org, ContactId}};
                true ->
                    case
                        exists_check(eb_pg_store_sql:sql(exists_identity), [
                            OrgId, WorkspaceId, IdentityId
                        ])
                    of
                        false -> {error, {identity_not_in_org, IdentityId}};
                        true -> {error, conflict}
                    end
            end
    end.

%% ===================================================================
%% eb_store_port：message
%% ===================================================================

%% 幂等：`(organization_id, conversation_id, client_msg_id)` 唯一；
%% 重放返回既有行且标记 `replayed => true`（不增行、不改写既有密文）。
-spec append_message(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
append_message(OrgId, WorkspaceId, Message) when is_map(Message) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        append_message_here(OrgId, WorkspaceId, Message)
    end);
append_message(_OrgId, _WorkspaceId, _Message) ->
    {error, invalid_message}.

%% @doc 在调用方事务内追加消息（canonical 事务用；同样租户贯穿 + 幂等）。
-spec append_message_in(term(), integer(), integer(), map()) -> {ok, map()} | {error, term()}.
append_message_in(Conn, OrgId, WorkspaceId, Message) when is_map(Message) ->
    case tenant_error(OrgId, WorkspaceId) of
        ok -> append_message_conn(Conn, OrgId, WorkspaceId, Message);
        {error, _} = Err -> Err
    end;
append_message_in(_Conn, _OrgId, _WorkspaceId, _Message) ->
    {error, invalid_message}.

append_message_here(OrgId, WorkspaceId, Message) ->
    append_message_conn(pool, OrgId, WorkspaceId, Message).

append_message_conn(Conn, OrgId, WorkspaceId, Message) ->
    case eb_pg_store_sql:ms_strict(maps:get(retain_until, Message, undefined)) of
        {ok, RetainUntilMs} ->
            append_message_validated(Conn, OrgId, WorkspaceId, Message, RetainUntilMs);
        {error, _} = Err ->
            Err
    end.

append_message_validated(Conn, OrgId, WorkspaceId, Message, RetainUntilMs) ->
    Id = maps:get(id, Message, undefined),
    ConversationId = maps:get(conversation_id, Message, undefined),
    ClientMsgId = maps:get(client_msg_id, Message, undefined),
    Params = [
        OrgId,
        WorkspaceId,
        Id,
        ConversationId,
        eb_pg_store_sql:nullify(
            eb_pg_store_sql:sender_type_bin(maps:get(sender_type, Message, undefined))
        ),
        eb_pg_store_sql:nullify(maps:get(sender_contact_id, Message, undefined)),
        eb_pg_store_sql:nullify(maps:get(sender_business_identity_id, Message, undefined)),
        eb_pg_store_sql:nullify(maps:get(actor_user_id, Message, undefined)),
        ClientMsgId,
        eb_pg_store_sql:nullify(maps:get(body_cipher, Message, undefined)),
        eb_pg_store_sql:nullify(maps:get(key_version, Message, undefined)),
        eb_pg_store_sql:nullify(maps:get(aad_hash, Message, undefined)),
        eb_pg_store_sql:nullify(maps:get(content_hash, Message, undefined)),
        eb_pg_store_sql:nullify(maps:get(policy_id, Message, undefined)),
        eb_pg_store_sql:nullify(maps:get(policy_version, Message, undefined)),
        maps:get(retention_days, Message, undefined),
        RetainUntilMs
    ],
    case run_execute(Conn, eb_pg_store_sql:sql(append_message), Params) of
        {ok, _Id} ->
            fetch_appended_message(Conn, OrgId, WorkspaceId, ConversationId, ClientMsgId, false);
        {error, no_row} ->
            fetch_appended_message(Conn, OrgId, WorkspaceId, ConversationId, ClientMsgId, true);
        {error, _} = Err ->
            Err
    end.

fetch_appended_message(Conn, OrgId, WorkspaceId, ConversationId, ClientMsgId, Replayed) ->
    %% run_query/3 已按 eb_pg_store_sql:message_fields() 归一化，这里不再二次归一（否则原子键会被
    %% 当作二进制键取不到值）。
    case
        run_query(Conn, eb_pg_store_sql:sql(fetch_message_by_client), [
            OrgId, WorkspaceId, ConversationId, ClientMsgId
        ])
    of
        {ok, [Row | _]} ->
            {ok, Row#{replayed => Replayed}};
        {ok, []} ->
            {error, not_found};
        {error, _} = Err ->
            Err
    end.

-spec fetch_message(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_message(OrgId, WorkspaceId, MessageId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_one(
            eb_pg_store_sql:sql(fetch_message),
            [OrgId, WorkspaceId, MessageId],
            eb_pg_store_sql:message_fields()
        )
    end).

-spec list_messages(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_messages(OrgId, WorkspaceId, ConversationId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_many(
            eb_pg_store_sql:sql(list_messages),
            [OrgId, WorkspaceId, ConversationId],
            eb_pg_store_sql:message_fields()
        )
    end).

%% ACK 只写投递状态表：canonical message 行数/密文 hash/scope/sender/actor/policy/retain_until
%% 一律不动（plan §2.1 #16）。
-spec ack_delivery(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
ack_delivery(OrgId, WorkspaceId, Ack) when is_map(Ack) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        Params = [
            OrgId,
            WorkspaceId,
            maps:get(id, Ack, eb_tsid:new_id(enterprise_message_delivery)),
            maps:get(message_id, Ack, undefined),
            maps:get(recipient_ref, Ack, undefined),
            eb_pg_store_sql:nullify(maps:get(device_id, Ack, undefined)),
            maps:get(acked_at, Ack, undefined) * 1000
        ],
        case execute_insert(eb_pg_store_sql:sql(ack_delivery), Params) of
            {ok, _Inserted} ->
                fetch_delivery(OrgId, WorkspaceId, Ack);
            {error, no_row} ->
                {error, {message_not_in_scope, maps:get(message_id, Ack, undefined)}};
            {error, _} = Err ->
                Err
        end
    end);
ack_delivery(_OrgId, _WorkspaceId, _Ack) ->
    {error, invalid_ack}.

%% ===================================================================
%% eb_store_port：assignment CAS
%% ===================================================================

-spec advance_assignment(integer(), integer(), integer(), atom(), atom()) ->
    ok | {error, term()}.
advance_assignment(OrgId, WorkspaceId, IdentityId, Expected, Next) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        case eb_identity:valid_transition(assignment, {Expected, Next}) of
            ok ->
                Params = [
                    OrgId,
                    WorkspaceId,
                    IdentityId,
                    eb_pg_store_sql:status_bin(Expected),
                    eb_pg_store_sql:status_bin(Next)
                ],
                case elib_pg:execute(eb_pg_store_sql:sql(advance_assignment), Params) of
                    {ok, 1} -> ok;
                    {ok, 0} -> {error, conflict};
                    {ok, 0, _Rows} -> {error, conflict};
                    {ok, _Other} -> {error, conflict};
                    {error, Reason} -> {error, eb_pg_store_sql:normalize_error(Reason)}
                end;
            {error, _} = Err ->
                %% domain 判定的非法迁移：不触库、不落行（铁律 7 的语义来自 domain）
                Err
        end
    end).

-spec list_assignments(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_assignments(OrgId, WorkspaceId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        case elib_pg:query(eb_pg_store_sql:sql(list_assignments), [OrgId, WorkspaceId]) of
            {ok, []} ->
                {ok, []};
            {ok, [First | _] = Rows} ->
                case truthy(maps:get(<<"scope_ok">>, First, false)) of
                    false ->
                        {error, {workspace_not_in_org, WorkspaceId}};
                    true ->
                        {ok, [
                            eb_pg_store_sql:normalize(Row, eb_pg_store_sql:assignment_fields())
                         || Row <- Rows, maps:get(<<"assignment_id">>, Row, undefined) =/= null
                        ]}
                end;
            {error, Reason} ->
                {error, eb_pg_store_sql:normalize_error(Reason)}
        end
    end).

%% ===================================================================
%% contact / contact_identity
%% ===================================================================

-spec insert_contact(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_contact(OrgId, WorkspaceId, Contact) when is_map(Contact) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Contact, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            Id,
            eb_pg_store_sql:nullify(maps:get(imboy_user_id, Contact, undefined)),
            maps:get(display_name, Contact, undefined),
            eb_pg_store_sql:nullify(maps:get(profile_cipher, Contact, undefined)),
            eb_pg_store_sql:nullify(maps:get(profile_key_version, Contact, undefined)),
            eb_pg_store_sql:nullify(maps:get(created_by_business_identity_id, Contact, undefined))
        ],
        insert_then_fetch(
            eb_pg_store_sql:sql(insert_contact),
            Params,
            fun() ->
                conflict_or(eb_pg_store_sql:sql(scope_ok), [OrgId, WorkspaceId], WorkspaceId)
            end,
            fun() -> fetch_contact(OrgId, WorkspaceId, Id) end
        )
    end);
insert_contact(_OrgId, _WorkspaceId, _Contact) ->
    {error, invalid_contact}.

-spec fetch_contact(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_contact(OrgId, WorkspaceId, ContactId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_one(
            eb_pg_store_sql:sql(fetch_contact),
            [OrgId, WorkspaceId, ContactId],
            eb_pg_store_sql:contact_fields()
        )
    end).

-spec insert_contact_identity(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_contact_identity(OrgId, WorkspaceId, Subject) when is_map(Subject) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        Params = [
            OrgId,
            WorkspaceId,
            maps:get(id, Subject, undefined),
            maps:get(contact_id, Subject, undefined),
            maps:get(channel, Subject, undefined),
            maps:get(subject_hmac, Subject, undefined),
            eb_pg_store_sql:nullify(maps:get(subject_mask, Subject, undefined))
        ],
        SubjectId = maps:get(id, Subject, undefined),
        case execute_insert(eb_pg_store_sql:sql(insert_contact_identity), Params) of
            {ok, _Inserted} ->
                fetch_contact_identity(OrgId, WorkspaceId, SubjectId);
            {error, no_row} ->
                diagnose_contact_identity_scope(
                    OrgId, WorkspaceId, maps:get(contact_id, Subject, undefined)
                );
            {error, _} = Err ->
                Err
        end
    end);
insert_contact_identity(_OrgId, _WorkspaceId, _Subject) ->
    {error, invalid_contact_identity}.

%% @doc 读取客户渠道标识（组织域 HMAC 与掩码；不含明文 subject）。
-spec fetch_contact_identity(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_contact_identity(OrgId, WorkspaceId, SubjectId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_one(
            eb_pg_store_sql:sql(fetch_contact_identity),
            [OrgId, WorkspaceId, SubjectId],
            eb_pg_store_sql:contact_identity_fields()
        )
    end).

%% @doc 读取单条 hold（append-only 事实；released_at 非空即已释放）。
-spec fetch_hold(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_hold(OrgId, WorkspaceId, HoldId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_one(
            eb_pg_store_sql:sql(fetch_hold),
            [OrgId, WorkspaceId, HoldId],
            eb_pg_store_sql:hold_fields()
        )
    end).

fetch_delivery(OrgId, WorkspaceId, Ack) ->
    fetch_one(
        eb_pg_store_sql:sql(fetch_delivery),
        [
            OrgId,
            WorkspaceId,
            maps:get(message_id, Ack, undefined),
            maps:get(recipient_ref, Ack, undefined),
            eb_pg_store_sql:nullify(maps:get(device_id, Ack, undefined))
        ],
        eb_pg_store_sql:delivery_fields()
    ).

diagnose_contact_identity_scope(OrgId, WorkspaceId, ContactId) ->
    case scope_ok(OrgId, WorkspaceId) of
        false ->
            {error, {workspace_not_in_org, WorkspaceId}};
        true ->
            case
                exists_check(eb_pg_store_sql:sql(exists_contact), [OrgId, WorkspaceId, ContactId])
            of
                false -> {error, {contact_not_in_org, ContactId}};
                true -> {error, conflict}
            end
    end.

%% ===================================================================
%% retention policy / hold
%% ===================================================================

-spec insert_policy(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_policy(OrgId, WorkspaceId, Policy) when is_map(Policy) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        Id = maps:get(id, Policy, undefined),
        Params = [
            OrgId,
            WorkspaceId,
            Id,
            maps:get(data_class, Policy, <<"enterprise_message">>),
            maps:get(version, Policy, 1),
            maps:get(retention_days, Policy, undefined),
            eb_pg_store_sql:nullify(maps:get(trigger_event, Policy, undefined)),
            eb_pg_store_sql:nullify(maps:get(created_by_user_id, Policy, undefined))
        ],
        insert_then_fetch(
            eb_pg_store_sql:sql(insert_policy),
            Params,
            fun() ->
                conflict_or(eb_pg_store_sql:sql(scope_ok), [OrgId, WorkspaceId], WorkspaceId)
            end,
            fun() -> latest_policy_by_id(OrgId, WorkspaceId, Id) end
        )
    end);
insert_policy(_OrgId, _WorkspaceId, _Policy) ->
    {error, invalid_policy}.

latest_policy_by_id(OrgId, WorkspaceId, Id) ->
    fetch_one(
        eb_pg_store_sql:sql(fetch_policy_by_id),
        [OrgId, WorkspaceId, Id],
        eb_pg_store_sql:policy_fields()
    ).

-spec latest_policy(integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
latest_policy(OrgId, WorkspaceId, DataClass) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_one(
            eb_pg_store_sql:sql(latest_policy),
            [OrgId, WorkspaceId, DataClass],
            eb_pg_store_sql:policy_fields()
        )
    end).

-spec insert_hold(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_hold(OrgId, WorkspaceId, Hold) when is_map(Hold) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        Params = [
            OrgId,
            WorkspaceId,
            maps:get(id, Hold, undefined),
            maps:get(scope_type, Hold, <<"workspace">>),
            eb_pg_store_sql:nullify(maps:get(scope_conversation_id, Hold, undefined)),
            eb_pg_store_sql:nullify(maps:get(scope_message_id, Hold, undefined)),
            maps:get(reason_code, Hold, undefined),
            eb_pg_store_sql:nullify(maps:get(actor_user_id, Hold, undefined))
        ],
        HoldId = maps:get(id, Hold, undefined),
        case execute_insert(eb_pg_store_sql:sql(insert_hold), Params) of
            {ok, _Inserted} ->
                fetch_hold(OrgId, WorkspaceId, HoldId);
            {error, no_row} ->
                conflict_or(eb_pg_store_sql:sql(scope_ok), [OrgId, WorkspaceId], WorkspaceId);
            {error, _} = Err ->
                Err
        end
    end);
insert_hold(_OrgId, _WorkspaceId, _Hold) ->
    {error, invalid_hold}.

%% append-only 事实的唯一可变动作：一次性写入 released_at + released_by_user_id。
-spec release_hold(integer(), integer(), integer(), integer() | undefined) ->
    ok | {error, term()}.
release_hold(OrgId, WorkspaceId, HoldId, ReleasedBy) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        Params = [
            OrgId,
            WorkspaceId,
            HoldId,
            eb_system_clock:now() * 1000,
            eb_pg_store_sql:nullify(ReleasedBy)
        ],
        case elib_pg:execute(eb_pg_store_sql:sql(release_hold), Params) of
            {ok, 1} -> ok;
            {ok, 0} -> {error, conflict};
            {ok, 0, _Rows} -> {error, conflict};
            {ok, _Other} -> {error, conflict};
            {error, Reason} -> {error, eb_pg_store_sql:normalize_error(Reason)}
        end
    end).

-spec list_active_holds(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_active_holds(OrgId, WorkspaceId) ->
    with_tenant(OrgId, WorkspaceId, fun() ->
        fetch_many(
            eb_pg_store_sql:sql(list_active_holds),
            [OrgId, WorkspaceId],
            eb_pg_store_sql:hold_fields()
        )
    end).

%% ===================================================================
%% EB-03R：契约面补齐后的能力（薄委托到各自的实现模块）
%% ===================================================================
%%
%% 为什么用委托而不是把实现搬进本文件：F1 要求 `eb_pg_store.erl` < 800 行，而
%% `eb_store_port` 的**所有** callback 必须由 `eb_infra_ports:resolve(store)`
%% 返回的同一个模块实现（`-behaviour` 的编译期检查）。因此本模块保持「唯一
%% Port 实现 + 租户贯穿」的门面，按能力域把实现下沉：
%%   * `eb_pg_identity_ext`   —— P1/P2/P5/P8
%%   * `eb_pg_contact_ext`    —— P3/P4/P7
%%   * `eb_pg_message_ext`    —— P9（键集分页）
%%   * `eb_pg_offboarding_ext`—— P11（case/item/CAS，EB-08 的预置能力）
%% 委托是**逐字**的：本层不追加、不削减任何语义（F4 行为保持）。

%% -- P9：只读历史（键集分页；after_id 严格大于，非 offset）------------------
-spec list_messages_after(integer(), integer(), map()) -> {ok, [map()]} | {error, term()}.
list_messages_after(OrgId, WorkspaceId, Query) ->
    eb_pg_message_ext:list_messages_after(OrgId, WorkspaceId, Query).

%% -- P1/P2/P5/P8 -----------------------------------------------------------
-spec insert_assignment(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_assignment(OrgId, WorkspaceId, Assignment) ->
    eb_pg_identity_ext:insert_assignment(OrgId, WorkspaceId, Assignment).

-spec list_identities(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_identities(OrgId, WorkspaceId) ->
    eb_pg_identity_ext:list_identities(OrgId, WorkspaceId).

%% -- C5：键集分页 + active assignment 投影（语句下推到 eb_pg_identity_ext）--
-spec list_identities_page(integer(), integer(), map()) -> {ok, [map()]} | {error, term()}.
list_identities_page(OrgId, WorkspaceId, Query) ->
    eb_pg_identity_ext:list_identities(OrgId, WorkspaceId, Query).

-spec update_conversation_assignee(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
update_conversation_assignee(OrgId, WorkspaceId, ConversationId, IdentityId) ->
    eb_pg_identity_ext:update_conversation_assignee(OrgId, WorkspaceId, ConversationId, IdentityId).

-spec list_conversations(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_conversations(OrgId, WorkspaceId) ->
    eb_pg_identity_ext:list_conversations(OrgId, WorkspaceId).

%% -- P3/P4/P7 --------------------------------------------------------------
-spec insert_note(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_note(OrgId, WorkspaceId, Note) ->
    eb_pg_contact_ext:insert_note(OrgId, WorkspaceId, Note).

-spec insert_contact_assignment(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_contact_assignment(OrgId, WorkspaceId, Assignment) ->
    eb_pg_contact_ext:insert_contact_assignment(OrgId, WorkspaceId, Assignment).

-spec list_contacts(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_contacts(OrgId, WorkspaceId) ->
    eb_pg_contact_ext:list_contacts(OrgId, WorkspaceId).

-spec update_contact(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
update_contact(OrgId, WorkspaceId, Patch) ->
    eb_pg_contact_ext:update_contact(OrgId, WorkspaceId, Patch).

%% -- P11：offboarding（EB-08 的预置持久化能力）-----------------------------
-spec insert_offboarding_case(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_offboarding_case(OrgId, WorkspaceId, Case) ->
    eb_pg_offboarding_ext:insert_case(OrgId, WorkspaceId, Case).

-spec fetch_offboarding_case(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_offboarding_case(OrgId, WorkspaceId, CaseId) ->
    eb_pg_offboarding_ext:fetch_case(OrgId, WorkspaceId, CaseId).

-spec list_offboarding_cases(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_offboarding_cases(OrgId, WorkspaceId) ->
    eb_pg_offboarding_ext:list_cases(OrgId, WorkspaceId).

-spec insert_offboarding_item(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
insert_offboarding_item(OrgId, WorkspaceId, Item) ->
    eb_pg_offboarding_ext:insert_item(OrgId, WorkspaceId, Item).

-spec list_offboarding_items(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_offboarding_items(OrgId, WorkspaceId, CaseId) ->
    eb_pg_offboarding_ext:list_items(OrgId, WorkspaceId, CaseId).

-spec update_offboarding_item(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
update_offboarding_item(OrgId, WorkspaceId, Item) ->
    eb_pg_offboarding_ext:update_item(OrgId, WorkspaceId, Item).

-spec advance_offboarding_case(integer(), integer(), integer(), integer(), atom(), map()) ->
    ok | {error, term()}.
advance_offboarding_case(OrgId, WorkspaceId, CaseId, ExpectedVersion, NextStatus, Counts) ->
    eb_pg_offboarding_ext:advance_case(
        OrgId, WorkspaceId, CaseId, ExpectedVersion, NextStatus, Counts
    ).

-spec update_offboarding_case_counts(integer(), integer(), integer(), integer(), atom(), map()) ->
    ok | {error, term()}.
update_offboarding_case_counts(OrgId, WorkspaceId, CaseId, ExpectedVersion, ExpectedStatus, Counts) ->
    eb_pg_offboarding_ext:update_case_counts(
        OrgId, WorkspaceId, CaseId, ExpectedVersion, ExpectedStatus, Counts
    ).

%% ===================================================================
%% 冻结语句清单（EB-03-A01 的机械判据）
%% ===================================================================

%% @doc 本 Feature 全部冻结 SQL（逐字迁到 `eb_pg_store_sql` 后仍由此转发，保持既有取证口径）。
%%
%% 每条都必须同时含 `organization_id`、`workspace_id` 与 `$1`/`$2`
%% （前两个业务参数 = OrgId/WorkspaceId 的机械证据）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    eb_pg_store_sql:statements().

%% ===================================================================
%% 租户门（铁律 6 的 Erlang 侧一半）
%% ===================================================================

tenant_error(OrgId, WorkspaceId) when is_integer(OrgId), is_integer(WorkspaceId) ->
    ok;
tenant_error(OrgId, WorkspaceId) ->
    {error, {invalid_tenant, {OrgId, WorkspaceId}}}.

with_tenant(OrgId, WorkspaceId, Fun) ->
    case tenant_error(OrgId, WorkspaceId) of
        ok -> Fun();
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% SQL 执行辅助
%% ===================================================================

fetch_one(Sql, Params, Fields) ->
    case fetch_many(Sql, Params, Fields) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} = Err -> Err
    end.

fetch_many(Sql, Params, Fields) ->
    case elib_pg:query(Sql, Params) of
        {ok, Rows} -> {ok, [eb_pg_store_sql:normalize(Row, Fields) || Row <- Rows]};
        {error, Reason} -> {error, eb_pg_store_sql:normalize_error(Reason)}
    end.

%% INSERT ... RETURNING：{ok, [Row]}；未写入 → {error, no_row}；DB 错 → {error, {sql, ...}}
execute_returning(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, 1, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, 1} -> {ok, []};
        {ok, 0, _Rows} -> {ok, []};
        {ok, 0} -> {ok, []};
        {ok, _Other, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, _Other} -> {ok, []};
        {error, Reason} -> {error, eb_pg_store_sql:normalize_error(Reason)}
    end.

execute_insert(Sql, Params) ->
    case execute_returning(Sql, Params) of
        {ok, []} -> {error, no_row};
        {ok, Rows} -> {ok, Rows};
        {error, _} = Err -> Err
    end.

%% 事务内执行（Conn 为 epgsql 连接；pool 表示走连接池独立语句）。
run_execute(pool, Sql, Params) ->
    execute_insert(Sql, Params);
run_execute(Conn, Sql, Params) ->
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, 1, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, 1} -> {ok, []};
        {ok, 0, _Rows} -> {error, no_row};
        {ok, 0} -> {error, no_row};
        {error, Reason} -> {error, eb_pg_store_sql:normalize_error(Reason)}
    end.

run_query(pool, Sql, Params) ->
    fetch_many(Sql, Params, eb_pg_store_sql:message_fields());
run_query(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} ->
            {ok, [eb_pg_store_sql:normalize(Row, eb_pg_store_sql:message_fields()) || Row <- Rows]};
        {error, Reason} ->
            {error, eb_pg_store_sql:normalize_error(Reason)}
    end.

insert_then_fetch(InsertSql, Params, OnNoRow, FetchFun) ->
    case execute_insert(InsertSql, Params) of
        {ok, _Inserted} -> FetchFun();
        {error, no_row} -> OnNoRow();
        {error, _} = Err -> Err
    end.

%% 0 行且 scope 正常 ⇒ 唯一键冲突（幂等/去重语义）。
conflict_or(ScopeSql, ScopeParams, WorkspaceId) ->
    case exists_check(ScopeSql, ScopeParams) of
        true -> {error, conflict};
        false -> {error, {workspace_not_in_org, WorkspaceId}}
    end.

scope_ok(OrgId, WorkspaceId) ->
    exists_check(eb_pg_store_sql:sql(scope_ok), [OrgId, WorkspaceId]).

%% 存在性判据：租户自检语句只在 (Org, Workspace) 同时成立时返回行。
exists_check(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [_ | _]} -> true;
        _ -> false
    end.

truthy(true) -> true;
truthy(<<"t">>) -> true;
truthy(<<"true">>) -> true;
truthy(_Other) -> false.
