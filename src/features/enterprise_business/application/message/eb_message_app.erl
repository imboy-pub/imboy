%%% @doc 企业消息（canonical message）的应用层用例：接受消息、投递 ACK 与只读历史。
%%%
%%% 依据：plan v4.1 EB-D05、§2.1 #9/#16/#18、§8 EB-06；作业书 §4 A01/A03/A05/A06/A08、§5。
%%%
%%% ## 接受语义（`append_message/2`）
%%%
%%% 服务端「接受」的语义固定为：**canonical message + policy snapshot + append-only
%%% audit 在同一事务里提交成功之后**才算 accepted，并且**提交之后**才异步通知
%%% **只含 resource id** 的载体（不含正文/密文/摘要/密钥）。这三条分别由
%%% 用例级事务扩展点 `eb_tx_port`（T1，同事务；实现经 `eb_infra_ports:resolve(tx)`
%%% 装配）与本模块的调用顺序保证。
%%%
%%%   * **consent fail-closed**：无 consent 一律 `{error, consent_required}`，零写入。
%%%     本模块**永不**转发调用方的 `enforce_consent`（旁路防护：即使调用方传
%%%     `enforce_consent => false`，仍走门禁）。
%%%   * **幂等**：同一 `client_msg_id` 重放不增行、不增审计，也**不重复通知**。
%%%   * **realtime 失败不回滚真源**：通知在提交之后发出；发布器报错或崩溃只记录在
%%%     返回体的 `publisher` 字段里，不改变 `accepted`、不回滚已提交的行、不吞异常。
%%%   * **失败即无 accepted**：canonical/audit 提交失败直接返回 `{error, Reason}`，
%%%     没有任何「服务端已接受」的表示，也不发通知；幂等键不被消耗，重试恰好一条。
%%%
%%% ## 投递 ACK（`ack_delivery/2`）
%%%
%%% ACK 只写独立的 `enterprise_message_delivery`（同 (message, recipient, device)
%%% 幂等）。写入前后各回读一次 canonical 行，并用 domain 判据
%%% `eb_message:ack_preserves_canonical/2` 自检；一旦发现 canonical 被改写即
%%% 返回 `{error, {canonical_mutated_by_ack, ...}}`。本模块**不复用**个人
%%% CLIENT_ACK / msg_archive 清理链，也不 DELETE/UPDATE canonical 行。
%%%
%%% ## 数据访问与装配（EB-06 重开：层间偏差收口）
%%%
%%% 事务编排走**用例级扩展点** `eb_tx_port:accept_message/3`（EB-03R T1 交付），
%%% 实现经 `eb_infra_ports:resolve(tx)` 装配。本模块因此**不再出现任何持久化实现
%%% 模块名**（无 `eb_pg_` 前缀）：application 层拿到的能力恰好是「接受一条企业消息」
%%% 这一个具名用例，而不是「开一个事务 + 发任意 SQL」。其余读写一律经扩展点
%%% （`eb_store_port` / `eb_clock_port` / `eb_id_port`）；本模块零 SQL、零 `elib_pg`、
%%% 不触 `*_repo` / `*_ds`。端口可在 `Params` 里用同键覆盖（`canonical_tx` /
%%% `store` / `clock` / `notify`，测试与装配用）。
%%%
%%% ## 只读历史（`list_messages/2` / `fetch_message/2`）
%%%
%%% 两者都是**只读**入口，经 `eb_store_port` 的 `list_messages_after/3`（键集分页）
%%% 与 `fetch_message/3` 读取 canonical 真源；本模块在此路径上**不写任何行**——
%%% 不改 `seen` / `status` / `version`，也不触发任何 ACK 或归档链。
-module(eb_message_app).

-export([
    append_message/2,
    ack_delivery/2,
    list_messages/2,
    fetch_message/2,
    canonical_view/1,
    notification_keys/0
]).

%% 键集分页的 limit 边界：与实现侧 `eb_pg_message_ext` 逐字同口径。
-define(DEFAULT_PAGE_LIMIT, 50).
-define(MAX_PAGE_LIMIT, 200).

%% 通知载体**只允许**出现这些键（全部是标识符；无正文/密文/摘要/密钥）。
-define(NOTIFICATION_KEYS, [
    resource_type,
    resource_id,
    organization_id,
    workspace_id,
    conversation_id
]).

-define(RESOURCE_TYPE, <<"enterprise_message">>).

%% ===================================================================
%% 接受企业消息
%% ===================================================================

%% @doc 接受一条企业消息（同事务 canonical + policy snapshot + audit；成功后通知）。
%%
%% Params：
%%   workspace_id      必填整数（租户操作范围）
%%   conversation_id   必填整数
%%   client_msg_id     必填非空二进制（幂等键）
%%   sender_type       必填：`contact`（客户端入站）| `business_identity`（员工出站）
%%   body              必填非空二进制（**明文**；由 canonical 事务做企业托管加密）
%%   contact_id        入站必填；出站必须缺省
%%   identity_id       出站必填；入站必须缺省
%%   actor_user_id     出站必填；入站必须缺省
%%   key_ref           可选（显式注入优先；缺省经 eb_env_keyring 服务端装配，F6）
%%   accepted_at       可选（Unix 秒；缺省经注入时钟端口）
%%   audit_action      可选（缺省 `message.accept`）
%%   notify            可选 fun/1（realtime 发布器；缺省显式报告未装配）
%%   canonical_tx / store / clock / id 可选端口覆盖
%%
%% 返回 `{ok, #{accepted, message, message_id, replayed, audit_id, notification,
%% publisher}}` 或 `{error, Reason}`。
-spec append_message(integer(), map()) -> {ok, map()} | {error, term()}.
append_message(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            append_args(OrgId, WorkspaceId, Params)
    end;
append_message(_OrgId, _Params) ->
    {error, {invalid_argument, append_message}}.

append_args(OrgId, WorkspaceId, Params) ->
    case validate_append(Params) of
        {error, _} = Err ->
            Err;
        {ok, SenderType, ClientMsgId} ->
            case accepted_at(Params) of
                {error, _} = Err ->
                    Err;
                {ok, AcceptedAt} ->
                    append_tx(OrgId, WorkspaceId, SenderType, ClientMsgId, AcceptedAt, Params)
            end
    end.

validate_append(Params) ->
    ConversationId = maps:get(conversation_id, Params, undefined),
    ClientMsgId = maps:get(client_msg_id, Params, undefined),
    case is_pos_int(ConversationId) of
        false ->
            {error, {invalid_conversation_id, ConversationId}};
        true ->
            case is_non_empty_binary(ClientMsgId) of
                false ->
                    {error, {invalid_client_msg_id, ClientMsgId}};
                true ->
                    validate_sender_type(Params, ClientMsgId)
            end
    end.

validate_sender_type(Params, ClientMsgId) ->
    case sender_type_atom(maps:get(sender_type, Params, undefined)) of
        {error, _} = Err ->
            Err;
        {ok, SenderType} ->
            case is_non_empty_binary(maps:get(body, Params, undefined)) of
                false ->
                    {error, {invalid_body, maps:get(body, Params, undefined)}};
                true ->
                    validate_sender_xor(Params, SenderType, ClientMsgId)
            end
    end.

%% 显式 sender XOR 合同（EB-D05）：调用方给的 sender 字段**原样**送 domain 判定，
%% 不做静默过滤或补齐——入站带 identity/actor、出站缺 actor 等一律在触库前拒绝。
validate_sender_xor(Params, SenderType, ClientMsgId) ->
    Candidate = #{
        sender_type => SenderType,
        sender_contact_id => maps:get(contact_id, Params, undefined),
        sender_business_identity_id => maps:get(identity_id, Params, undefined),
        actor_user_id => maps:get(actor_user_id, Params, undefined)
    },
    case eb_message:validate_sender(Candidate) of
        ok -> {ok, SenderType, ClientMsgId};
        {error, _} = Err -> Err
    end.

%% 只识别两个受控值；其余原样回传（不做 binary_to_atom，避免用外部输入造原子）。
sender_type_atom(contact) ->
    {ok, contact};
sender_type_atom(<<"contact">>) ->
    {ok, contact};
sender_type_atom(business_identity) ->
    {ok, business_identity};
sender_type_atom(<<"business_identity">>) ->
    {ok, business_identity};
sender_type_atom(Other) ->
    {error, {unknown_sender_type, Other}}.

append_tx(OrgId, WorkspaceId, SenderType, ClientMsgId, AcceptedAt, Params) ->
    TxParams = tx_params(Params, SenderType, ClientMsgId, AcceptedAt),
    case canonical_tx(Params) of
        {error, _} = Err ->
            Err;
        {ok, CanonicalTx} ->
            try CanonicalTx:accept_message(OrgId, WorkspaceId, TxParams) of
                {error, _} = Err ->
                    Err;
                {ok, TxResult} ->
                    accepted(OrgId, WorkspaceId, TxResult, Params)
            catch
                Class:Reason ->
                    {error, {canonical_tx_failed, {Class, Reason}}}
            end
    end.

%% 事务参数**显式白名单**构造：调用方给的 `enforce_consent` 等旁路键一律不转发。
%%
%% F6（RULING-2026-09-15 §七）主密钥装配点：显式注入（map 形态的测试/内部合同）
%% 原样优先；缺省经 `eb_env_keyring` 从服务端 env 解析 active key_ref。env 缺失
%% 时 resolve 返回 undefined，canonical tx 的 seal 照旧 `{error, missing_key}`
%% fail-closed（500 面），不降级、不造默认密钥。
tx_params(Params, SenderType, ClientMsgId, AcceptedAt) ->
    Base = #{
        conversation_id => maps:get(conversation_id, Params, undefined),
        client_msg_id => ClientMsgId,
        body => maps:get(body, Params, undefined),
        sender_type => SenderType,
        key_ref => eb_env_keyring:resolve_key_ref(maps:get(key_ref, Params, undefined)),
        accepted_at => AcceptedAt,
        enforce_consent => true
    },
    WithAudit =
        case maps:get(audit_action, Params, undefined) of
            undefined -> Base;
            Action -> Base#{audit_action => Action}
        end,
    %% sender 字段原样透传（XOR 合同已在 validate_sender_xor/3 判定；此处不补齐、不过滤）。
    maps:merge(WithAudit, #{
        contact_id => maps:get(contact_id, Params, undefined),
        identity_id => maps:get(identity_id, Params, undefined),
        actor_user_id => maps:get(actor_user_id, Params, undefined)
    }).

%% 提交成功之后：先构造（只含 id 的）通知，再尽力发布——发布失败不回滚、不吞掉。
accepted(OrgId, WorkspaceId, TxResult, Params) ->
    Message = maps:get(message, TxResult),
    Replayed = maps:get(replayed, TxResult, false),
    {Notification, Publisher} = publish(OrgId, WorkspaceId, Message, Replayed, Params),
    {ok, #{
        accepted => true,
        message => Message,
        message_id => maps:get(id, Message),
        replayed => Replayed,
        audit_id => maps:get(audit_id, TxResult, undefined),
        notification => Notification,
        publisher => Publisher
    }}.

%% 重放不重复通知（该资源早已被接受过）。
publish(_OrgId, _WorkspaceId, _Message, true, _Params) ->
    {undefined, {skipped, replay}};
publish(OrgId, WorkspaceId, Message, false, Params) ->
    Notification = notification(OrgId, WorkspaceId, Message),
    case notifier(Params) of
        {error, _} = Err ->
            {Notification, Err};
        {ok, Notify} ->
            {Notification, run_notifier(Notify, Notification)}
    end.

run_notifier(Notify, Notification) ->
    try Notify(Notification) of
        ok -> ok;
        {error, Reason} -> {failed, Reason};
        Other -> {failed, {unexpected_result, Other}}
    catch
        Class:Reason -> {failed, {Reason, Class}}
    end.

%% 通知载体：**只含 resource id**（无正文、无密文、无摘要、无密钥引用）。
notification(OrgId, WorkspaceId, Message) ->
    #{
        resource_type => ?RESOURCE_TYPE,
        resource_id => maps:get(id, Message),
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => maps:get(conversation_id, Message)
    }.

%% @doc 通知载体的键白名单（静态可核对；供测试断言「不泄露 payload」）。
-spec notification_keys() -> [atom()].
notification_keys() ->
    ?NOTIFICATION_KEYS.

notifier(Params) ->
    case maps:get(notify, Params, undefined) of
        Fun when is_function(Fun, 1) ->
            {ok, Fun};
        undefined ->
            %% V1 的企业 realtime 通道由上层（handler）装配；未装配时**显式报告未投递**，
            %% 不假装已通知，也不因此回滚已提交的真源（plan §2.1 #18）。
            {error, {realtime_publisher_missing, enterprise_message}};
        Other ->
            {error, {invalid_notify, Other}}
    end.

%% ===================================================================
%% 投递 ACK（只写独立状态）
%% ===================================================================

%% @doc 客户端投递 ACK：只写 `enterprise_message_delivery`，并对 canonical 真源自检。
%%
%% Params：
%%   workspace_id   必填整数
%%   message_id     必填整数
%%   recipient_ref  必填，形状 `contact:<id>` | `identity:<id>`（与 DB CHECK 同口径）
%%   device_id      可选
%%   acked_at       可选（Unix 秒；缺省经注入时钟端口）
%%   store / clock 可选端口覆盖
%%
%% 返回 `{ok, #{delivery, canonical_unchanged, message_id}}` 或 `{error, Reason}`。
-spec ack_delivery(integer(), map()) -> {ok, map()} | {error, term()}.
ack_delivery(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            ack_args(OrgId, WorkspaceId, Params)
    end;
ack_delivery(_OrgId, _Params) ->
    {error, {invalid_argument, ack_delivery}}.

ack_args(OrgId, WorkspaceId, Params) ->
    MessageId = maps:get(message_id, Params, undefined),
    RecipientRef = maps:get(recipient_ref, Params, undefined),
    case is_pos_int(MessageId) of
        false ->
            {error, {invalid_message_id, MessageId}};
        true ->
            case is_valid_recipient_ref(RecipientRef) of
                false ->
                    {error, {invalid_recipient_ref, RecipientRef}};
                true ->
                    case acked_at(Params) of
                        {error, _} = Err ->
                            Err;
                        {ok, AckedAt} ->
                            ack_write(
                                OrgId, WorkspaceId, MessageId, RecipientRef, AckedAt, Params
                            )
                    end
            end
    end.

ack_write(OrgId, WorkspaceId, MessageId, RecipientRef, AckedAt, Params) ->
    case fetch_canonical(OrgId, WorkspaceId, MessageId, Params) of
        {error, not_found} ->
            {error, {message_not_in_scope, MessageId}};
        {error, _} = Err ->
            Err;
        {ok, Before} ->
            Ack = #{
                message_id => MessageId,
                recipient_ref => RecipientRef,
                device_id => maps:get(device_id, Params, undefined),
                acked_at => AckedAt
            },
            case
                with_store(Params, fun(Store) ->
                    Store:ack_delivery(OrgId, WorkspaceId, Ack)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Delivery} ->
                    ack_verify(OrgId, WorkspaceId, MessageId, Before, Delivery, Params)
            end
    end.

%% ACK 之后复核 canonical 真源逐字不变（行数不由本路径改变；字段由 domain 判据裁决）。
ack_verify(OrgId, WorkspaceId, MessageId, Before, Delivery, Params) ->
    case fetch_canonical(OrgId, WorkspaceId, MessageId, Params) of
        {error, _} = Err ->
            Err;
        {ok, After} ->
            case
                eb_message:ack_preserves_canonical(
                    canonical_view([Before]), canonical_view([After])
                )
            of
                ok ->
                    {ok, #{
                        delivery => Delivery,
                        message_id => MessageId,
                        canonical_unchanged => true
                    }};
                {error, Reason} ->
                    {error, {canonical_mutated_by_ack, Reason}}
            end
    end.

fetch_canonical(OrgId, WorkspaceId, MessageId, Params) ->
    with_store(Params, fun(Store) -> Store:fetch_message(OrgId, WorkspaceId, MessageId) end).

%% @doc canonical 真源视图：把 store 的列名映射为 domain `canonical_fields/0` 的命名
%% （`id` → `message_id`，`content_hash` → `body_cipher_hash`），供不变量断言复用。
-spec canonical_view(map() | [map()]) -> map() | [map()].
canonical_view(Rows) when is_list(Rows) ->
    [canonical_row(Row) || Row <- Rows];
canonical_view(Row) when is_map(Row) ->
    canonical_row(Row).

canonical_row(Row) ->
    Row#{
        message_id => maps:get(id, Row, undefined),
        body_cipher_hash => maps:get(content_hash, Row, undefined)
    }.

%% recipient_ref 与迁移 116 的 ck_emd_recipient_ref 逐字同口径（早失败、零副作用）。
is_valid_recipient_ref(Ref) when is_binary(Ref) ->
    re:run(Ref, <<"^(contact|identity):[0-9]+$">>, [{capture, none}]) =:= match;
is_valid_recipient_ref(_Other) ->
    false.

%% ===================================================================
%% 只读历史（键集分页 + 单条读取；本段**零写入**）
%% ===================================================================

%% @doc 会话历史的**键集分页**读取（§5.1 `GET messages`，EB-06-A09）。
%%
%% Params：
%%   workspace_id     必填整数
%%   conversation_id  必填整数
%%   after_id         可选（缺省 0）：游标，语义是**严格 `id > after_id`**
%%   limit            可选（缺省 50，1..200）
%%   store            可选端口覆盖
%%
%% **为什么是键集而不是 offset**：企业消息 append-only 且会被 bounded purge 物理删除；
%% `OFFSET n` 在「读取期间有行被删/被追加」时会跳行或重复行。键集把游标钉在**已读到的
%% id** 上，因此翻页之间插入 `id < 游标` 的行不会让第 2 页漂移。
%%
%% **边界语义（明确，不是隐含）**：`after_id` 指向**不存在或已被物理删除**的消息时，
%% 结果与「该 id 从未存在」逐字相同 —— 返回 `id > after_id` 的那一页，既不报错也不
%% 从头开始。这是键集语义的直接推论（游标是**位置**，不是**引用**），也是它相对 offset
%% 的可依赖之处。
%%
%% 返回 `{ok, [Message]}`（按 `id` 升序）或 `{error, Reason}`。**只读**：不改任何行状态。
-spec list_messages(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_messages(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            list_args(OrgId, WorkspaceId, Params)
    end;
list_messages(_OrgId, _Params) ->
    {error, {invalid_argument, list_messages}}.

list_args(OrgId, WorkspaceId, Params) ->
    ConversationId = maps:get(conversation_id, Params, undefined),
    case is_pos_int(ConversationId) of
        false ->
            {error, {invalid_conversation_id, ConversationId}};
        true ->
            case page_cursor(Params) of
                {error, _} = Err ->
                    Err;
                {ok, AfterId} ->
                    case page_limit(Params) of
                        {error, _} = Err ->
                            Err;
                        {ok, Limit} ->
                            Query = #{
                                conversation_id => ConversationId,
                                after_id => AfterId,
                                limit => Limit
                            },
                            with_store(Params, fun(Store) ->
                                Store:list_messages_after(OrgId, WorkspaceId, Query)
                            end)
                    end
            end
    end.

%% 游标缺省 0（`id > 0` 等价首页）；显式负数/非整数一律 fail-closed（不静默当首页）。
page_cursor(Params) ->
    case maps:get(after_id, Params, 0) of
        undefined -> {ok, 0};
        null -> {ok, 0};
        Id when is_integer(Id), Id >= 0 -> {ok, Id};
        Other -> {error, {invalid_after_id, Other}}
    end.

page_limit(Params) ->
    case maps:get(limit, Params, ?DEFAULT_PAGE_LIMIT) of
        Limit when is_integer(Limit), Limit >= 1, Limit =< ?MAX_PAGE_LIMIT -> {ok, Limit};
        Other -> {error, {invalid_limit, Other}}
    end.

%% @doc 单条 canonical 消息的**只读**读取（EB-06-A10）。
%%
%% Params：`workspace_id` / `message_id` 必填；`store` 可选覆盖。
%%
%% 本函数只发一条 SELECT（经 `eb_store_port:fetch_message/3`）：**不**改 `seen`、
%% **不**改 `status`、**不**动 `version`，也不写 delivery/ACK —— 只读入口的语义就是
%% 只读，任何「顺手标记已读」都会让只读变成隐式写。
-spec fetch_message(integer(), map()) -> {ok, map()} | {error, term()}.
fetch_message(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case maps:get(message_id, Params, undefined) of
                MessageId when is_integer(MessageId), MessageId > 0 ->
                    with_store(Params, fun(Store) ->
                        Store:fetch_message(OrgId, WorkspaceId, MessageId)
                    end);
                Other ->
                    {error, {invalid_message_id, Other}}
            end
    end;
fetch_message(_OrgId, _Params) ->
    {error, {invalid_argument, fetch_message}}.

%% ===================================================================
%% 内部辅助：端口 / 时钟 / 参数
%% ===================================================================

port(Key, Params) ->
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(Key)
    end.

with_store(Params, Fun) ->
    case port(store, Params) of
        {ok, Store} -> Fun(Store);
        {error, _} = Err -> Err
    end.

%% canonical 事务编排的装配默认：经**用例级 Port** `eb_tx_port` 解析
%%（实现按装配选择，application 层不出现实现模块名）。
canonical_tx(Params) ->
    case maps:get(canonical_tx, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(tx)
    end.

clock_port(Params) ->
    case maps:get(clock, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(clock)
    end.

accepted_at(Params) ->
    injected_or_clock(accepted_at, Params).

acked_at(Params) ->
    injected_or_clock(acked_at, Params).

injected_or_clock(Key, Params) ->
    case maps:get(Key, Params, undefined) of
        Value when is_integer(Value) ->
            {ok, Value};
        undefined ->
            case clock_port(Params) of
                {ok, Clock} ->
                    try
                        {ok, Clock:now()}
                    catch
                        Class:Reason -> {error, {clock_unavailable, {Class, Reason}}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        Other ->
            {error, {invalid_injected_time, Key, Other}}
    end.

tenant(OrgId, Params) ->
    case is_integer(OrgId) andalso OrgId > 0 of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case maps:get(workspace_id, Params, undefined) of
                WorkspaceId when is_integer(WorkspaceId), WorkspaceId > 0 ->
                    {ok, WorkspaceId};
                Other ->
                    {error, {invalid_workspace_id, Other}}
            end
    end.

is_pos_int(Value) ->
    is_integer(Value) andalso Value > 0.

is_non_empty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.
