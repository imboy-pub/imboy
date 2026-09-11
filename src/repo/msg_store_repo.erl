-module(msg_store_repo).
%%%
% msg_store_repo 是消息写入队列备份表的仓库层
% 提供备份表的 CRUD 操作，保证消息零丢失
%%%

-include("log.hrl").

-define(MAX_C2G_RECIPIENTS, 5000).

%% ==================== API ====================

-export([tablename/0]).

-ifdef(TEST).
%% 仅测试导出：payload/e2ee 的 JSONB 规范化（数字开头密文误判回归）
-export([msg_store_payload_to_jsonb/1]).
-export([msg_store_e2ee_to_jsonb/1]).
-endif.

%% 表管理
-export([ensure_table_exists/0]).
-export([create_indexes/1]).

%% 写入操作
-export([stage/10]).
-export([stage/11]).
-export([stage/12]).
-export([stage_action/13]).
%% 真库回归直接执行生产 SQL，避免 mock 只验证字符串形状。
-export([action_recipient_sql/0]).

%% 删除操作
-export([unstage/2]).
-export([claim_pending/2]).
-export([mark_processed/1]).
-export([mark_failed/4]).
-export([mark_terminal/3]).
-export([delete_processed/1]).
-export([delete_expired_c2g_ledgers/2]).
-export([truncate_processed/0]).
-export([vacuum_table/0]).

%% 查询操作
-export([get_unstaged/1]).
-export([get_staging_stats/0]).
-export([find_by_msg_id/2]).

%% ==================== API Functions ====================

%% @doc 按消息类型和 ID 查仍待处理的 staging 行（秒撤兜底）。
-spec find_by_msg_id(binary(), binary()) -> {ok, map()} | {error, not_found} | {error, term()}.
find_by_msg_id(Type, MsgId) ->
    Tb = tablename(),
    Sql =
        <<"SELECT msg_id, from_id, to_id, created_at FROM ", Tb/binary,
            " WHERE type = $1 AND msg_id = $2 AND processed_at IS NULL LIMIT 1">>,
    case elib_pg:query(Sql, [Type, MsgId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc c2g staging 预分配的 seq 计数器原子 upsert（E2EE-2026-012 §7.2）。
%% 与 membership 转换对同一行加锁；与 staging INSERT 同事务执行。
-spec stage_conv_seq_sql() -> binary().
stage_conv_seq_sql() ->
    <<"INSERT INTO public.msg_store_seq (conv_key, seq) VALUES ($1, 1) ",
        "ON CONFLICT (conv_key) DO UPDATE SET seq = public.msg_store_seq.seq + 1 ",
        "RETURNING seq">>.

%% @doc 获取备份表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_store_staging">>).

%% @doc 写入备份表（v2.0）
%% @param Type 消息类别 (c2c/c2g/s2c/c2s)
%% @param MsgId 消息唯一ID
%% @param MsgType 消息子类型 (text/image/video/etc)
%% @param Action S2C 操作类型
%% @param E2EE 端到端加密元数据 (JSONB map 或 null)
%% @param Payload 消息内容 (JSON binary)
%% @param FromId 发送者用户ID
%% @param ToId 接收者用户ID (单聊) 或 ToIdList (群聊)
%% @param CreatedAt 消息创建时间 (RFC3339 binary)
%% @param ServerTs 服务器时间戳 (RFC3339 binary)
-spec stage(
    binary(),
    binary(),
    binary(),
    binary(),
    map(),
    binary(),
    integer(),
    integer() | [integer()],
    binary(),
    binary()
) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs) ->
    stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs, <<>>).

%% @doc 写入备份表（A2-a：带服务端验证过的发送者设备标识）
%%
%% 相比 stage/10 多一个 SenderDid：PFv3 接收侧 context binding 第 6 项拿
%% 信封顶层的 `sender_did` 与受认证的 `protected_header.sender_did` 硬比对
%% （ADR 15 §3.3）。实时投递靠 `message_ds:with_sender_device/2` 现场盖章，
%% 离线（decrypt-on-read）路径没有「现场」——必须在 staging 落库时就存下来，
%% 否则重连拉取的 v3 消息永久判 `context_mismatch_sender_did` 不可读。
%%
%% SenderDid 为 `<<>>` 时**不写该列**（保持 NULL）：空串不是设备标识，
%% 写空串会让接收侧把「服务端没提供」误判成「设备 ID 是空串」。
%%
%% @param SenderDid 发送者设备 ID（取自 WebSocket 连接认证态，客户端不可伪造）
-spec stage(
    binary(),
    binary(),
    binary(),
    binary(),
    map(),
    binary(),
    integer(),
    integer() | [integer()],
    binary(),
    binary(),
    binary()
) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
stage(
    Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs, SenderDid
) when
    is_integer(ToId), Type =:= <<"c2g">>
->
    stage(
        Type,
        MsgId,
        MsgType,
        Action,
        E2EE,
        Payload,
        FromId,
        ToId,
        CreatedAt,
        ServerTs,
        SenderDid,
        1
    );
stage(
    Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs, SenderDid
) when
    is_integer(ToId)
->
    Tb = tablename(),
    Data0 = #{
        type => Type,
        msg_id => MsgId,
        msg_type => MsgType,
        action => Action,
        e2ee => msg_store_e2ee_to_jsonb(E2EE),
        %% payload 列是 JSONB：
        %%  - 普通消息：Payload 已经是合法 JSON binary（如 {"text":"..."}）
        %%  - E2EE 消息：Payload 是裸 base64 密文 binary，必须包装为 JSON 字符串
        %%  - Map：编码为 JSON object
        payload => msg_store_payload_to_jsonb(Payload),
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTs,
        retry_count => 0
    },
    Data = put_sender_did(Data0, SenderDid),
    %% 预生成 TSID
    GenId = elib_tsid:generate(msg_store),
    Data2 = Data#{id => GenId},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    %% 【幂等性修复】捕获唯一约束错误
    case elib_pg:query(Sql, Params) of
        {ok, _} ->
            {ok, GenId};
        {error, {error, {error, <<"23505">>, unique_violation, _, _}}} ->
            %% PostgreSQL 唯一约束错误：消息已存在（幂等性）
            {error, {unique_violation, MsgId}};
        {error, {error, Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end;
stage(
    <<"c2g">>,
    _MsgId,
    _MsgType,
    _Action,
    _E2EE,
    _Payload,
    _FromId,
    ToIdList,
    _CreatedAt,
    _ServerTs,
    _SenderDid
) when is_list(ToIdList) ->
    %% C2G 必须携带 GID，才能在同一事务内重验群/发送者并固化收件人快照。
    {error, c2g_group_id_required};
stage(
    Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToIdList, CreatedAt, ServerTs, SenderDid
) when
    is_list(ToIdList)
->
    Tb = tablename(),
    Data0 = #{
        type => Type,
        msg_id => MsgId,
        msg_type => MsgType,
        action => Action,
        e2ee => msg_store_e2ee_to_jsonb(E2EE),
        %% payload 列是 JSONB：
        %%  - 普通消息：Payload 已经是合法 JSON binary（如 {"text":"..."}）
        %%  - E2EE 消息：Payload 是裸 base64 密文 binary，必须包装为 JSON 字符串
        %%  - Map：编码为 JSON object
        payload => msg_store_payload_to_jsonb(Payload),
        from_id => FromId,
        to_id_list => ToIdList,
        created_at => CreatedAt,
        server_ts => ServerTs,
        retry_count => 0
    },
    Data = put_sender_did(Data0, SenderDid),
    %% 预生成 TSID
    GenId2 = elib_tsid:generate(msg_store),
    Data3 = Data#{id => GenId2},
    {Sql2, Params2} = elib_pg_sql:insert(Tb, Data3),
    %% 【幂等性修复】捕获唯一约束错误
    case elib_pg:query(Sql2, Params2) of
        {ok, _} ->
            {ok, GenId2};
        {error, {error, {error, <<"23505">>, unique_violation, _, _}}} ->
            %% PostgreSQL 唯一约束错误：消息已存在（幂等性）
            {error, {unique_violation, MsgId}};
        {error, {error, Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc C2G 持久接受：在序列锁内重验 active sender、最低角色和收件人快照。
-spec stage(
    binary(),
    binary(),
    binary(),
    binary(),
    map(),
    binary(),
    integer(),
    integer(),
    binary(),
    binary(),
    binary(),
    1 | 3
) -> {ok, term(), [integer()]} | {error, term()}.
stage(
    <<"c2g">> = Type,
    MsgId,
    MsgType,
    Action,
    E2EE,
    Payload,
    FromId,
    ToId,
    CreatedAt,
    ServerTs,
    SenderDid,
    RequiredRole
) when is_integer(ToId), (RequiredRole =:= 1 orelse RequiredRole =:= 3) ->
    case valid_c2g_msg_id(MsgId) of
        true ->
            stage_c2g(
                Type,
                MsgId,
                MsgType,
                Action,
                E2EE,
                Payload,
                FromId,
                ToId,
                CreatedAt,
                ServerTs,
                SenderDid,
                RequiredRole,
                undefined
            );
        false ->
            {error, invalid_msgid}
    end.

%% @doc C2G 编辑/撤回持久接受。收件人取原消息已提交快照与当前可见世代的交集，
%% 防止后加入或退出后重入的成员收到旧消息正文变更。
-spec stage_action(
    binary(),
    binary(),
    binary(),
    binary(),
    map(),
    binary(),
    integer(),
    integer(),
    binary(),
    binary(),
    binary(),
    1 | 3,
    binary()
) -> {ok, term(), [integer()]} | {error, term()}.
stage_action(
    <<"c2g">> = Type,
    MsgId,
    MsgType,
    Action,
    E2EE,
    Payload,
    FromId,
    ToId,
    CreatedAt,
    ServerTs,
    SenderDid,
    RequiredRole,
    OriginalMsgId
) when
    is_integer(ToId),
    (RequiredRole =:= 1 orelse RequiredRole =:= 3),
    is_binary(OriginalMsgId),
    OriginalMsgId =/= <<>>
->
    case valid_c2g_msg_id(MsgId) of
        true ->
            stage_c2g(
                Type,
                MsgId,
                MsgType,
                Action,
                E2EE,
                Payload,
                FromId,
                ToId,
                CreatedAt,
                ServerTs,
                SenderDid,
                RequiredRole,
                OriginalMsgId
            );
        false ->
            {error, invalid_msgid}
    end.

-spec valid_c2g_msg_id(term()) -> boolean().
valid_c2g_msg_id(MsgId) when is_binary(MsgId) ->
    byte_size(MsgId) >= 1 andalso byte_size(MsgId) =< 40;
valid_c2g_msg_id(_) ->
    false.

-spec stage_c2g(
    binary(),
    binary(),
    binary(),
    binary(),
    map(),
    binary(),
    integer(),
    integer(),
    binary(),
    binary(),
    binary(),
    1 | 3,
    undefined | binary()
) -> {ok, term(), [integer()]} | {error, term()}.
stage_c2g(
    Type,
    MsgId,
    MsgType,
    Action,
    E2EE,
    Payload,
    FromId,
    ToId,
    CreatedAt,
    ServerTs,
    SenderDid,
    RequiredRole,
    OriginalMsgId
) ->
    %% Sequence、发送权限、recipient snapshot 与 staging INSERT 位于同一事务。
    %% 重复 msg_id 会整体回滚，不改变原行 conv_seq，也不制造序号 gap。
    ConvKey = msg_archive_ds:conv_key_c2g(ToId),
    Tb = tablename(),
    Data0 = #{
        type => Type,
        msg_id => MsgId,
        msg_type => MsgType,
        action => Action,
        e2ee => msg_store_e2ee_to_jsonb(E2EE),
        payload => msg_store_payload_to_jsonb(Payload),
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTs,
        retry_count => 0
    },
    Data = put_sender_did(Data0, SenderDid),
    GenId = elib_tsid:generate(msg_store),
    case
        elib_pg:with_tx(fun(Conn) ->
            %% 与 workspace 归档 UPDATE 锁同一行；先拿锁者决定消息是否被接受。
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {group, ToId})
            ),
            Seq = allocate_c2g_seq(Conn, ConvKey),
            MemberUids =
                case OriginalMsgId of
                    undefined ->
                        authorized_c2g_recipients(Conn, ToId, FromId, RequiredRole);
                    _ ->
                        authorized_c2g_action_recipients(
                            Conn, ToId, FromId, RequiredRole, OriginalMsgId
                        )
                end,
            ok = persist_c2g_request_identity(
                Conn,
                MsgId,
                FromId,
                ToId,
                Action,
                MsgType,
                maps:get(e2ee, Data),
                maps:get(payload, Data),
                maps:get(sender_did, Data, null),
                CreatedAt
            ),
            ok = persist_c2g_recipient_snapshot(
                Conn, MsgId, FromId, ToId, Seq, MemberUids, CreatedAt
            ),
            StagedData = Data#{id => GenId, conv_seq => Seq, to_id_list => MemberUids},
            {Sql, Params} = elib_pg_sql:insert(Tb, StagedData),
            %% 普通返回值会提交，所有失败都必须显式抛 rollback。
            case elib_pg:query(Conn, Sql, Params) of
                {ok, _} ->
                    case OriginalMsgId of
                        undefined ->
                            ok = bind_group_attachment_anchors(Conn, MsgId, FromId, ToId, Seq);
                        _ ->
                            ok
                    end,
                    {ok, MemberUids};
                {error, {error, error, <<"23505">>, unique_violation, _, _}} ->
                    throw({rollback, {unique_violation, MsgId}});
                {error, Reason} ->
                    throw({rollback, Reason})
            end
        end)
    of
        {ok, MemberUids} when is_list(MemberUids) ->
            {ok, GenId, MemberUids};
        {rollback, {unique_violation, MsgId}} ->
            {error, {unique_violation, MsgId}};
        {rollback, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason};
        Other ->
            Other
    end.

%% @private 请求账本覆盖正式消息生命周期；相同 msg_id 只有完整请求身份一致才是重试。
-spec persist_c2g_request_identity(
    pid(),
    binary(),
    integer(),
    integer(),
    binary(),
    binary(),
    binary(),
    binary(),
    null | binary(),
    binary()
) -> ok.
persist_c2g_request_identity(
    Conn, MsgId, FromId, Gid, Action, MsgType, E2EE, Payload, SenderDid, CreatedAt
) ->
    Sql =
        <<"INSERT INTO public.msg_c2g_request_ledger ",
            "(msg_id, from_id, to_gid, action, request_hash, created_at) ",
            "VALUES ($1, $2, $3, $4, ", "digest(convert_to(jsonb_build_array($5::text, $6::jsonb, ",
            "CASE WHEN jsonb_typeof($7::jsonb) = 'object' THEN ",
            "$7::jsonb - 'server_ts' - 'revoked_at' - 'edited_at' ",
            "#- '{payload,server_ts}' #- '{payload,revoked_at}' #- '{payload,edited_at}' ",
            "ELSE $7::jsonb END, ", "COALESCE($8::text, ''))::text, 'UTF8'), 'sha256'), $9) ",
            "ON CONFLICT (msg_id) DO NOTHING RETURNING msg_id">>,
    Params = [MsgId, FromId, Gid, Action, MsgType, E2EE, Payload, SenderDid, CreatedAt],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [_]} ->
            ok;
        {ok, []} ->
            verify_c2g_duplicate_identity(Conn, Params);
        {error, Reason} ->
            throw({rollback, {request_ledger_persist_failed, Reason}});
        Other ->
            throw({rollback, {unexpected_request_ledger_persist, Other}})
    end.

-spec verify_c2g_duplicate_identity(pid(), [term()]) -> no_return().
verify_c2g_duplicate_identity(
    Conn, [MsgId, FromId, Gid, Action, MsgType, E2EE, Payload, SenderDid, _CreatedAt]
) ->
    Sql =
        <<"SELECT from_id, to_gid, action IS NOT DISTINCT FROM $2::text AS same_action, ",
            "request_hash = digest(convert_to(jsonb_build_array($3::text, $4::jsonb, ",
            "CASE WHEN jsonb_typeof($5::jsonb) = 'object' THEN ",
            "$5::jsonb - 'server_ts' - 'revoked_at' - 'edited_at' ",
            "#- '{payload,server_ts}' #- '{payload,revoked_at}' #- '{payload,edited_at}' ",
            "ELSE $5::jsonb END, ",
            "COALESCE($6::text, ''))::text, 'UTF8'), 'sha256') AS same_request ",
            "FROM public.msg_c2g_request_ledger WHERE msg_id = $1">>,
    VerifyParams = [MsgId, Action, MsgType, E2EE, Payload, SenderDid],
    case elib_pg:query(Conn, Sql, VerifyParams) of
        {ok, [
            #{
                <<"from_id">> := FromId,
                <<"to_gid">> := Gid,
                <<"same_action">> := true,
                <<"same_request">> := true
            }
        ]} ->
            throw({rollback, {unique_violation, MsgId}});
        {ok, [_]} ->
            throw({rollback, msg_id_conflict});
        {ok, []} ->
            throw({rollback, {request_ledger_conflict_missing, MsgId}});
        {error, Reason} ->
            throw({rollback, {request_ledger_conflict_read_failed, Reason}});
        Other ->
            throw({rollback, {unexpected_request_ledger_conflict_read, Other}})
    end.

%% @private 不可变收件人快照不随离线 ACK、timeline retention 或 staging 清理删除。
-spec persist_c2g_recipient_snapshot(
    pid(), binary(), integer(), integer(), integer(), [integer()], binary()
) -> ok.
persist_c2g_recipient_snapshot(Conn, MsgId, FromId, Gid, Seq, RecipientUids, CreatedAt) ->
    Sql =
        <<"INSERT INTO public.msg_c2g_recipient_snapshot ",
            "(msg_id, from_id, to_gid, conv_seq, recipient_uids, created_at) ",
            "VALUES ($1, $2, $3, $4, $5, $6) ",
            "ON CONFLICT (msg_id) DO NOTHING RETURNING msg_id">>,
    case elib_pg:query(Conn, Sql, [MsgId, FromId, Gid, Seq, RecipientUids, CreatedAt]) of
        {ok, [_]} ->
            ok;
        {ok, []} ->
            throw({rollback, {recipient_snapshot_conflict, MsgId}});
        {error, Reason} ->
            throw({rollback, {recipient_snapshot_persist_failed, Reason}});
        Other ->
            throw({rollback, {unexpected_recipient_snapshot_persist, Other}})
    end.

-spec allocate_c2g_seq(pid(), binary()) -> pos_integer().
allocate_c2g_seq(Conn, ConvKey) ->
    try epgsql:equery(Conn, stage_conv_seq_sql(), [ConvKey]) of
        {ok, _, _, [{Seq}]} when is_integer(Seq), Seq >= 1 ->
            Seq;
        {error, Reason} ->
            throw({rollback, {conv_seq_allocate_failed, Reason}});
        Other ->
            throw({rollback, {conv_seq_allocate_failed, {unexpected_result, Other}}})
    catch
        throw:{rollback, _} = Rollback -> throw(Rollback);
        Class:Reason -> throw({rollback, {conv_seq_allocate_failed, {Class, Reason}}})
    end.

%% @private C2G staging INSERT 成功后、事务提交前，把同一发送者预先确认的群附件
%% 绑定到权威 conv_seq。普通消息无匹配行时是合法 no-op；未绑定附件下载侧仍拒绝。
-spec bind_group_attachment_anchors(pid(), binary(), integer(), integer(), integer()) -> ok.
bind_group_attachment_anchors(Conn, MsgId, FromId, Gid, Seq) ->
    Sql =
        <<"UPDATE public.attachment SET anchor_conv_seq = $4, updated_at = now() ",
            "WHERE anchor_msg_id = $1 AND creator_user_id = $2 ",
            "AND scope = 'group' AND scope_ref = $3::text ",
            "AND group_file_id IS NULL AND anchor_conv_seq IS NULL AND status >= 0">>,
    case elib_pg:query(Conn, Sql, [MsgId, FromId, Gid, Seq]) of
        {ok, _} -> ok;
        {error, Reason} -> throw({rollback, {attachment_anchor_bind_failed, Reason}});
        Other -> throw({rollback, {unexpected_attachment_anchor_bind, Other}})
    end.

%% @private sequence 行锁已由调用方持有；本查询在同一事务的新 READ COMMITTED
%% snapshot 中重新验证发送者，并固化所有下游必须复用的 active recipient 集合。
-spec authorized_c2g_recipients(pid(), integer(), integer(), 1 | 3) -> [integer()].
authorized_c2g_recipients(Conn, Gid, FromId, RequiredRole) ->
    ProbeLimit = ?MAX_C2G_RECIPIENTS + 1,
    Sql =
        <<"SELECT recipient.user_id FROM public.\"group\" grp ",
            "JOIN public.group_member caller ON caller.group_id = grp.id ",
            "AND caller.user_id = $2 AND caller.status = 1 AND caller.role >= $3 ",
            "JOIN public.group_member recipient ON recipient.group_id = grp.id ",
            "AND recipient.status = 1 ", "WHERE grp.id = $1 AND grp.status = 1 LIMIT $4">>,
    case elib_pg:query(Conn, Sql, [Gid, FromId, RequiredRole, ProbeLimit]) of
        {ok, []} ->
            throw({rollback, forbidden});
        {ok, Rows} when length(Rows) > ?MAX_C2G_RECIPIENTS ->
            throw({rollback, recipient_limit_exceeded});
        {ok, Rows} ->
            case [Uid || #{<<"user_id">> := Uid} <- Rows, is_integer(Uid)] of
                Uids when length(Uids) =:= length(Rows) -> Uids;
                _ -> throw({rollback, invalid_recipient_row})
            end;
        {error, Reason} ->
            throw({rollback, {recipient_snapshot_failed, Reason}});
        Other ->
            throw({rollback, {unexpected_recipient_snapshot, Other}})
    end.

%% @private 原消息的不可变 recipient snapshot 与 staging 在同一事务提交；离线 ACK、
%% timeline retention 和 staging 清理都不会改变它。当前 generation 仍必须覆盖原 seq。
-spec authorized_c2g_action_recipients(pid(), integer(), integer(), 1 | 3, binary()) ->
    [integer()].
authorized_c2g_action_recipients(Conn, Gid, FromId, RequiredRole, OriginalMsgId) ->
    ProbeLimit = ?MAX_C2G_RECIPIENTS + 1,
    Sql = action_recipient_sql(),
    case elib_pg:query(Conn, Sql, [Gid, FromId, RequiredRole, OriginalMsgId, ProbeLimit]) of
        {ok, []} ->
            throw({rollback, action_target_forbidden});
        {ok, Rows} when length(Rows) > ?MAX_C2G_RECIPIENTS ->
            throw({rollback, recipient_limit_exceeded});
        {ok, Rows} ->
            case [Uid || #{<<"user_id">> := Uid} <- Rows, is_integer(Uid)] of
                Uids when length(Uids) =:= length(Rows) -> Uids;
                _ -> throw({rollback, invalid_recipient_row})
            end;
        {error, Reason} ->
            throw({rollback, {recipient_snapshot_failed, Reason}});
        Other ->
            throw({rollback, {unexpected_recipient_snapshot, Other}})
    end.

-spec action_recipient_sql() -> binary().
action_recipient_sql() ->
    <<"SELECT recipient.user_id FROM public.\"group\" grp",
        " JOIN public.group_member caller ON caller.group_id = grp.id",
        " AND caller.user_id = $2 AND caller.status = 1 AND caller.role >= $3",
        " JOIN public.group_member_generation caller_gen",
        " ON caller_gen.group_id = caller.group_id AND caller_gen.user_id = caller.user_id",
        " AND caller_gen.end_seq IS NULL", " JOIN public.msg_c2g_recipient_snapshot target",
        " ON target.to_gid = grp.id AND target.from_id = caller.user_id", " AND target.msg_id = $4",
        " AND target.conv_seq >= caller_gen.start_seq",
        " CROSS JOIN LATERAL unnest(target.recipient_uids) original_recipient(user_id)",
        " JOIN public.group_member recipient ON recipient.group_id = grp.id",
        " AND recipient.user_id = original_recipient.user_id AND recipient.status = 1",
        " JOIN public.group_member_generation recipient_gen",
        " ON recipient_gen.group_id = recipient.group_id",
        " AND recipient_gen.user_id = recipient.user_id AND recipient_gen.end_seq IS NULL",
        " AND target.conv_seq >= recipient_gen.start_seq",
        " WHERE grp.id = $1 AND grp.status = 1 LIMIT $5">>.

%% @private
%% @doc 仅在设备标识非空时写该列；空值一律保持 NULL。
%% 与 message_ds:with_sender_device/2 的「缺字段时不补空值」同一语义。
-spec put_sender_did(map(), term()) -> map().
put_sender_did(Data, Did) when is_binary(Did), Did =/= <<>> ->
    Data#{sender_did => Did};
put_sender_did(Data, _Did) ->
    Data.

%% @doc 删除备份表记录（消息成功写入正式表后调用）
-spec unstage(binary(), binary()) -> {ok, integer()} | {error, any()}.
unstage(Type, MsgId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE type = $1 AND msg_id = $2">>,
    elib_pg:execute(Sql, [Type, MsgId]).

%% @doc 抢占未处理消息（FOR UPDATE SKIP LOCKED），并设置 lease（available_at）
-spec claim_pending(pos_integer(), pos_integer()) -> {ok, list(map())} | {error, term()}.
claim_pending(Limit, LeaseSeconds) ->
    Tb = tablename(),
    elib_pg:with_tx(fun(Conn) ->
        Sql = <<
            "SELECT id, type, msg_id, payload, from_id, to_id, to_id_list, created_at, server_ts, retry_count, "
            "msg_type, action, e2ee, sender_did, conv_seq "
            "FROM ",
            Tb/binary,
            " WHERE processed_at IS NULL ",
            " AND available_at <= NOW() ",
            " ORDER BY created_at ASC ",
            " LIMIT $1 ",
            " FOR UPDATE SKIP LOCKED"
        >>,
        case elib_pg:query(Conn, Sql, [Limit]) of
            {ok, Rows} ->
                case Rows of
                    [] ->
                        {ok, []};
                    _ ->
                        Ids = [maps:get(<<"id">>, Row) || Row <- Rows],
                        LeaseSql =
                            <<"UPDATE ", Tb/binary,
                                " SET available_at = NOW() + INTERVAL '1 second' * $1 ",
                                " WHERE id = ANY($2)">>,
                        _ = elib_pg:execute(Conn, LeaseSql, [LeaseSeconds, Ids]),
                        {ok, Rows}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    end).

%% @doc 标记消息已处理（不区分类型，一条 SQL 更新所有类型）
%% 优化版本：移除 type 条件，避免对每种类型都执行一次 UPDATE
%% @param MsgId 消息唯一ID
%% @return {ok, Count} | {error, any()}
-spec mark_processed(binary()) -> {ok, integer()} | {error, any()}.
mark_processed(MsgId) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET processed_at = NOW(), error_msg = NULL ",
            " WHERE msg_id = $1">>,
    elib_pg:execute(Sql, [MsgId]).

%% @doc 标记失败并设置下次重试时间
-spec mark_failed(binary(), binary(), binary(), pos_integer()) -> {ok, integer()} | {error, any()}.
mark_failed(Type, MsgId, ErrorMsg, DelaySeconds) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET retry_count = retry_count + 1, ", " error_msg = $3, ",
            " available_at = NOW() + INTERVAL '1 second' * $4 ",
            " WHERE type = $1 AND msg_id = $2 AND processed_at IS NULL">>,
    elib_pg:execute(Sql, [Type, MsgId, ErrorMsg, DelaySeconds]).

%% @doc 结构性不可恢复的 staging 失败进入终态，保留 error_msg 供审计。
-spec mark_terminal(binary(), binary(), binary()) -> {ok, integer()} | {error, any()}.
mark_terminal(Type, MsgId, ErrorMsg) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET processed_at = NOW(), error_msg = $3 ",
            " WHERE type = $1 AND msg_id = $2 AND processed_at IS NULL">>,
    elib_pg:execute(Sql, [Type, MsgId, ErrorMsg]).

%% @doc 主消息一年 retention 完成后再清理 C2G 请求账本与收件人快照。
-spec delete_expired_c2g_ledgers(pos_integer(), pos_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
delete_expired_c2g_ledgers(AgeSeconds, Limit) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            SelectSql =
                <<"SELECT ledger.msg_id FROM public.msg_c2g_request_ledger ledger ",
                    "WHERE ledger.created_at < NOW() - INTERVAL '1 second' * $1 ",
                    "AND NOT EXISTS (SELECT 1 FROM public.msg_c2g msg ",
                    "WHERE msg.msg_id = ledger.msg_id) ",
                    "AND NOT EXISTS (SELECT 1 FROM public.msg_store_staging staged ",
                    "WHERE staged.type = 'c2g' AND staged.msg_id = ledger.msg_id) ",
                    "ORDER BY ledger.created_at LIMIT $2 FOR UPDATE OF ledger SKIP LOCKED">>,
            case elib_pg:query(Conn, SelectSql, [AgeSeconds, Limit]) of
                {ok, []} ->
                    {ok, 0};
                {ok, Rows} ->
                    MsgIds = [maps:get(<<"msg_id">>, Row) || Row <- Rows],
                    delete_c2g_ledger_rows(Conn, MsgIds);
                {error, Reason} ->
                    throw({rollback, Reason});
                Other ->
                    throw({rollback, {unexpected_c2g_ledger_cleanup_select, Other}})
            end
        end)
    of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

-spec delete_c2g_ledger_rows(pid(), [binary()]) -> {ok, non_neg_integer()} | no_return().
delete_c2g_ledger_rows(Conn, MsgIds) ->
    case
        elib_pg:execute(
            Conn,
            <<"DELETE FROM public.msg_c2g_recipient_snapshot WHERE msg_id = ANY($1)">>,
            [MsgIds]
        )
    of
        {ok, _} -> ok;
        {error, Reason1} -> throw({rollback, {recipient_snapshot_cleanup_failed, Reason1}});
        Other1 -> throw({rollback, {unexpected_recipient_snapshot_cleanup, Other1}})
    end,
    case
        elib_pg:execute(
            Conn,
            <<"DELETE FROM public.msg_c2g_request_ledger WHERE msg_id = ANY($1)">>,
            [MsgIds]
        )
    of
        {ok, Count} -> {ok, Count};
        {error, Reason2} -> throw({rollback, {request_ledger_cleanup_failed, Reason2}});
        Other2 -> throw({rollback, {unexpected_request_ledger_cleanup, Other2}})
    end.

%% @doc 获取未处理的备份消息（用于启动时恢复）
-spec get_unstaged(integer()) -> {ok, list(map())} | {error, any()}.
get_unstaged(Limit) ->
    Tb = tablename(),
    Sql = <<
        "SELECT type, msg_type, msg_id, payload, from_id, to_id, to_id_list, created_at, "
        "server_ts, retry_count, action, e2ee, sender_did, conv_seq "
        "FROM ",
        Tb/binary,
        " WHERE processed_at IS NULL ",
        "ORDER BY created_at ASC ",
        "LIMIT $1"
    >>,
    elib_pg:query(Sql, [Limit]).

%% @doc 清理已处理的备份消息（定时任务调用）
-spec delete_processed(integer()) -> {ok, integer()} | {error, any()}.
delete_processed(Seconds) ->
    Tb = tablename(),
    Sql =
        <<"DELETE FROM ", Tb/binary, " WHERE processed_at IS NOT NULL ",
            " AND processed_at < NOW() - INTERVAL '1 second' * $1">>,
    elib_pg:execute(Sql, [Seconds]).

%% @doc 获取备份表的统计信息
-spec get_staging_stats() -> {ok, map()} | {error, any()}.
get_staging_stats() ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", "COUNT(*) FILTER (WHERE processed_at IS NULL) as pending, ",
            "COUNT(*) FILTER (WHERE processed_at IS NOT NULL) as processed, ",
            "COUNT(*) FILTER (WHERE error_msg IS NOT NULL) as failed, ", "COUNT(*) as total ",
            "FROM ", Tb/binary>>,
    elib_pg:query(Sql, []).

%% @doc 清空备份表（慎用！）
-spec truncate_processed() -> {ok, integer()} | {error, any()}.
truncate_processed() ->
    Tb = tablename(),
    elib_pg:query(<<"TRUNCATE TABLE ", Tb/binary>>, []).

%% @doc 清理备份表空间
-spec vacuum_table() -> {ok, term()} | {error, any()}.
vacuum_table() ->
    Tb = tablename(),
    elib_pg:query(<<"VACUUM ANALYZE ", Tb/binary>>, []).

%% @doc 确保备份表存在
%% 动态创建 msg_store_staging 表及其索引
-spec ensure_table_exists() -> ok | {error, any()}.
ensure_table_exists() ->
    Tb = tablename(),
    case
        elib_pg:execute(
            <<"CREATE TABLE IF NOT EXISTS ", Tb/binary,
                " (\n"
                "            id BIGINT PRIMARY KEY,\n"
                "            type VARCHAR(10) NOT NULL,\n"
                "            msg_id VARCHAR(50) NOT NULL,\n"
                "            msg_type VARCHAR(50),\n"
                "            action VARCHAR(50),\n"
                "            e2ee JSONB,\n"
                %% A2-a：发送者设备标识（PFv3 context binding #6）。本 DDL 只覆盖
                %% 全新安装；存量部署由 priv/migrations/00000048 的 ALTER 补列——
                %% 两处必须同步，漏一处即新老部署 schema 分叉。
                "            sender_did VARCHAR(128),\n"
                %% Task 8 / E2EE-2026-012：staging 预分配列（持久接受顺序）。
                %% 本 DDL 只覆盖全新安装；存量部署由 priv/migrations/00000101
                %% 的 ALTER 补列——两处必须同步（同 sender_did 的 00000048 惯例）。
                "            conv_seq BIGINT,\n"
                "            payload JSONB NOT NULL,\n"
                "            from_id BIGINT NOT NULL,\n"
                "            to_id BIGINT,\n"
                "            to_id_list BIGINT[],\n"
                "            created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\n"
                "            server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),\n"
                "            retry_count INTEGER NOT NULL DEFAULT 0,\n"
                "            processed_at TIMESTAMPTZ,\n"
                "            available_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\n"
                "            error_msg TEXT,\n"
                "            CONSTRAINT msg_store_staging_type_msg_id_key UNIQUE (type, msg_id)\n"
                "        )">>,
            []
        )
    of
        {ok, _} ->
            %% 创建索引
            create_indexes(Tb);
        {error, {error, Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建备份表的索引
-spec create_indexes(binary()) -> ok | {error, any()}.
create_indexes(Tb) ->
    %% 创建 processed_at 索引（用于清理已处理记录）
    _ = elib_pg:execute(
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary,
            "_processed_at_idx\n"
            "            ON ", Tb/binary, " (processed_at) WHERE processed_at IS NOT NULL">>,
        []
    ),
    %% 创建 available_at 索引（用于抢占待处理记录）
    _ = elib_pg:execute(
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary,
            "_available_at_idx\n"
            "            ON ", Tb/binary, " (available_at) WHERE processed_at IS NULL">>,
        []
    ),
    %% 创建 created_at 索引（用于按时间排序）
    _ = elib_pg:execute(
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary,
            "_created_at_idx\n"
            "            ON ", Tb/binary, " (created_at)">>,
        []
    ),
    ok.

%% @private
%% @doc 把传入的 payload 规范化为合法的 JSONB binary
%% 上游可能传：map（普通消息内容）、JSON 编码后的 binary（普通消息）、
%% 或裸 base64 密文 binary（E2EE 消息）。后者直接写入 JSONB 会触发
%% "invalid input syntax for type json"，需要包装为 JSON 字符串。
-spec msg_store_payload_to_jsonb(term()) -> binary().
msg_store_payload_to_jsonb(null) ->
    jsone:encode(null);
msg_store_payload_to_jsonb(Map) when is_map(Map) ->
    jsone:encode(Map, [native_utf8]);
msg_store_payload_to_jsonb(Bin) when is_binary(Bin) ->
    %% is_likely_json_binary 只看首字符，会把 "14bVk..." 这类以数字开头的
    %% 裸 E2EE 密文误判为 JSON 数字 → PG 22P02（真机实测，e2ee 消息 staging
    %% 全崩）。与 e2ee 字段同法：try-decode 真验证，不能解码则包装 JSON string。
    try jsone:decode(Bin, [{object_format, map}]) of
        _ -> Bin
    catch
        _:_ -> jsone:encode(Bin, [native_utf8])
    end;
msg_store_payload_to_jsonb(Other) ->
    jsone:encode(Other).

%% @private
%% @doc 把传入的 E2EE 元数据规范化为合法的 JSONB binary 或 null
%% 上游可能传：map（标准 E2EE 元数据）、JSON binary、空 binary、null、
%% 或者裸字符串（如某些上游路径只取了密文片段）。裸字符串必须包装为
%% JSON 字符串，否则触发 "invalid input syntax for type json"。
%% 与 payload 相同：try-decode 真验证，行为保持一致。
-spec msg_store_e2ee_to_jsonb(term()) -> binary() | null.
msg_store_e2ee_to_jsonb(null) ->
    null;
msg_store_e2ee_to_jsonb(<<>>) ->
    null;
msg_store_e2ee_to_jsonb(Map) when is_map(Map) ->
    jsone:encode(Map, [native_utf8]);
msg_store_e2ee_to_jsonb(Bin) when is_binary(Bin) ->
    %% is_likely_json_binary 只看首字符，会把 "4QuejM" 这类裸 base62 误判为 JSON 数字，
    %% 因此 e2ee 这里改用 try-decode 真验证：能解码才原样传，否则按 JSON string 包装。
    try jsone:decode(Bin, [{object_format, map}]) of
        _ -> Bin
    catch
        _:_ -> jsone:encode(Bin, [native_utf8])
    end;
msg_store_e2ee_to_jsonb(_) ->
    null.
