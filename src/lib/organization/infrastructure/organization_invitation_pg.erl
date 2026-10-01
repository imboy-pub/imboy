-module(organization_invitation_pg).

%% Organization Invitation 读写 SQL（infrastructure，事务内使用）。
%%
%% 只封装语句与行集；锁序与业务裁决在 organization_invitation_app。
%% 所有查询显式贯穿 organization_id（铁律 6：同语句 Org 作用域）；
%% accept 一次性消费 = 单条 CAS UPDATE（WHERE status='pending'），DB 行锁串行化，
%% 先提交者胜，后到者 WHERE 不命中（READ COMMITTED EvalPlanQual 重检）→ 影响 0 行。
%% token 只存 digest：任何语句都不存在明文 token 列，digest 查找按
%% (organization_id, target_user_id, token_digest) 同语句裁决，跨 Org 命中不了行。

-export([
    lock_organization_tx/2,
    lock_organization_for_share_tx/2,
    member_tx/3,
    target_user_tx/2,
    insert_tx/2,
    find_by_digest_tx/4,
    find_latest_active_for_target_tx/3,
    find_tx/4,
    expire_due_tx/2,
    consume_pending_tx/3,
    list_for_target_tx/4,
    list_for_org_tx/4
]).

-define(ROW_COLS,
    "id, organization_id, target_user_id, invited_by, token_digest, status,"
    "       extract(epoch from expires_at)::bigint AS expires_at,"
    "       extract(epoch from responded_at)::bigint AS responded_at,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
).

%% @doc 锁组织行（锁序与既有治理写一致：组织行先、成员/邀请行后）。
-spec lock_organization_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
lock_organization_tx(Conn, OrgId) ->
    lock_org_tx(Conn, OrgId, <<" FOR UPDATE">>).

lock_organization_for_share_tx(Conn, OrgId) ->
    lock_org_tx(Conn, OrgId, <<" FOR SHARE">>).

lock_org_tx(Conn, OrgId, Lock) ->
    Sql =
        <<"SELECT id, owner_id, status FROM ", (org_table())/binary, " WHERE id = $1",
            Lock/binary>>,
    one_tx(Conn, Sql, [OrgId]).

%% @doc 成员行读取（判定 inviter 治理资格 / target 是否已在 Org 内）。
-spec member_tx(any(), integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
member_tx(Conn, OrgId, Uid) ->
    Sql =
        <<"SELECT role, status FROM ", (member_table())/binary,
            " WHERE organization_id = $1 AND user_id = $2">>,
    one_tx(Conn, Sql, [OrgId, Uid]).

%% @doc 目标用户存在性读取（invite 仅限已注册用户；同事务只读裁决）。
%% 并发窗口兜底由 insert_tx 的 23503 映射承担：校验通过到插入之间
%% target 被删时不再让外键违规冒泡成 500。
-spec target_user_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
target_user_tx(Conn, Uid) ->
    Sql = <<"SELECT id FROM ", (user_table())/binary, " WHERE id = $1">>,
    one_tx(Conn, Sql, [Uid]).

%% @doc 插入邀请行（id 由应用层 elib_tsid 生成；token_digest 为唯一落库形态）。
%% 同 (org,target) 的第二条 pending 由部分唯一索引拒绝 → {error, pending_conflict}；
%% target 用户在校验后到插入之间被删（并发窗口）由外键拒绝 → {error, target_user_missing}。
-spec insert_tx(any(), map()) ->
    ok | {error, pending_conflict | target_user_missing | term()}.
insert_tx(Conn, Row) ->
    Sql =
        <<"INSERT INTO ", (invitation_table())/binary,
            " (id, organization_id, target_user_id, invited_by, token_digest, status, expires_at)"
            " VALUES ($1, $2, $3, $4, $5, 'pending', to_timestamp($6))">>,
    Params = [
        maps:get(id, Row),
        maps:get(organization_id, Row),
        maps:get(target_user_id, Row),
        maps:get(invited_by, Row),
        maps:get(token_digest, Row),
        maps:get(expires_at, Row)
    ],
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, 1} ->
            ok;
        {ok, _} ->
            {error, insert_affected_mismatch};
        {error, Reason} = Err ->
            case error_code(Reason) of
                <<"23505">> -> {error, pending_conflict};
                <<"23503">> -> {error, target_user_missing};
                _ -> Err
            end
    end.

%% @doc 按 (org, target, digest) 精确读行（不限 status——accept 幂等重放需要读终态）。
-spec find_by_digest_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_by_digest_tx(Conn, OrgId, TargetUid, Digest) ->
    Sql =
        <<"SELECT ", ?ROW_COLS, " FROM ", (invitation_table())/binary,
            " WHERE organization_id = $1 AND target_user_id = $2 AND token_digest = $3 FOR UPDATE">>,
    one_tx(Conn, Sql, [OrgId, TargetUid, Digest]).

%% @doc 按 (org, target) 读最新 pending/accepted 行（免口令 accept 路径）：
%% target 身份即凭据（JWT），digest 不可用；pending 行是消费对象，
%% accepted 终态用于幂等重放（重复点击收敛到同一视图）。
%% 部分唯一索引保证 (org,target) 至多一个 pending；rejected/revoked/expired
%% 不入选——终态非 accepted 的旧邀请不应被再次「接受成功」。
-spec find_latest_active_for_target_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_latest_active_for_target_tx(Conn, OrgId, TargetUid) ->
    Sql =
        <<"SELECT ", ?ROW_COLS, " FROM ", (invitation_table())/binary,
            " WHERE organization_id = $1 AND target_user_id = $2"
            "   AND status IN ('pending', 'accepted')"
            " ORDER BY id DESC LIMIT 1 FOR UPDATE">>,
    one_tx(Conn, Sql, [OrgId, TargetUid]).

%% @doc 按 (org, id) 读行；TargetUid = 0 时不限 target（治理路径），
%% 否则同语句锁 target 作用域（target-only 路径，无法读到他人邀请）。
-spec find_tx(any(), integer(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_tx(Conn, OrgId, InvitationId, TargetUid) ->
    {TargetCond, Params} =
        case TargetUid of
            0 -> {<<"">>, [OrgId, InvitationId]};
            _ -> {<<" AND target_user_id = $3">>, [OrgId, InvitationId, TargetUid]}
        end,
    Sql =
        <<"SELECT ", ?ROW_COLS, " FROM ", (invitation_table())/binary,
            " WHERE organization_id = $1 AND id = $2", TargetCond/binary, " FOR UPDATE">>,
    one_tx(Conn, Sql, Params).

%% @doc lazy expire：把本 Org 内已过期的 pending 行置为 expired（幂等，重复执行 0 行）。
%% OrgId = 0 时全域清扫（仅限后台/运维调用；命令路径一律 Org 作用域）。
-spec expire_due_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
expire_due_tx(Conn, OrgId) ->
    Scope =
        case OrgId of
            0 -> <<"">>;
            _ -> <<" AND organization_id = $1">>
        end,
    Sql = iolist_to_binary([
        <<"UPDATE ", (invitation_table())/binary,
            " SET status = 'expired', responded_at = CURRENT_TIMESTAMP,"
            "     updated_at = CURRENT_TIMESTAMP"
            " WHERE status = 'pending' AND expires_at <= clock_timestamp()">>,
        Scope
    ]),
    Params =
        case OrgId of
            0 -> [];
            _ -> [OrgId]
        end,
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, Count} when is_integer(Count) -> {ok, Count};
        {ok, _, _} -> {ok, 0};
        {error, _} = Err -> Err
    end.

%% @doc 一次性消费 CAS：仅 pending 行可进入终态，RETURNING 消费后的整行。
%% Accept also checks the actual database clock after row locking and classification.
-spec consume_pending_tx(any(), integer(), accept | reject | revoke) ->
    {ok, map()} | {error, not_pending | term()}.
consume_pending_tx(Conn, InvitationId, Kind) ->
    Status =
        case Kind of
            accept -> <<"accepted">>;
            reject -> <<"rejected">>;
            revoke -> <<"revoked">>
        end,
    Deadline =
        case Kind of
            accept -> <<" AND expires_at > clock_timestamp()">>;
            _ -> <<>>
        end,
    Sql =
        <<"UPDATE ", (invitation_table())/binary,
            " SET status = $2, responded_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP",
            " WHERE id = $1 AND status = 'pending'", Deadline/binary, " RETURNING ", ?ROW_COLS>>,
    case elib_pg:query(Conn, Sql, [InvitationId, Status]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_pending};
        {error, _} = Err -> Err
    end.

%% @doc target 视角邀请列表（跨 Org）。
-spec list_for_target_tx(any(), integer(), atom() | undefined, pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_for_target_tx(Conn, TargetUid, Status, Limit) ->
    {StatusCond, StatusParams} = status_cond(Status, 2),
    Sql = iolist_to_binary([
        <<"SELECT ", ?ROW_COLS, " FROM ", (invitation_table())/binary,
            " WHERE target_user_id = $1">>,
        StatusCond,
        <<" ORDER BY id DESC LIMIT $", (integer_to_binary(length(StatusParams) + 2))/binary>>
    ]),
    Params = [TargetUid] ++ StatusParams ++ [Limit],
    elib_pg:query(Conn, Sql, Params).

%% @doc Org 治理视角邀请列表（铁律 6：同语句 Org 作用域）。
-spec list_for_org_tx(any(), integer(), atom() | undefined, pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_for_org_tx(Conn, OrgId, Status, Limit) ->
    {StatusCond, StatusParams} = status_cond(Status, 2),
    Sql = iolist_to_binary([
        <<"SELECT ", ?ROW_COLS, " FROM ", (invitation_table())/binary,
            " WHERE organization_id = $1">>,
        StatusCond,
        <<" ORDER BY id DESC LIMIT $", (integer_to_binary(length(StatusParams) + 2))/binary>>
    ]),
    Params = [OrgId] ++ StatusParams ++ [Limit],
    elib_pg:query(Conn, Sql, Params).

%%--------------------------------------------------------------------
-spec status_cond(atom() | undefined, pos_integer()) -> {binary(), list()}.
status_cond(undefined, _N) ->
    {<<"">>, []};
status_cond(Status, N) when is_atom(Status) ->
    NBin = integer_to_binary(N),
    {<<" AND status = $", NBin/binary>>, [atom_to_binary(Status)]}.

-spec org_table() -> binary().
org_table() ->
    elib_pg_sql:public_tablename(<<"organization">>).

-spec member_table() -> binary().
member_table() ->
    elib_pg_sql:public_tablename(<<"organization_member">>).

-spec invitation_table() -> binary().
invitation_table() ->
    elib_pg_sql:public_tablename(<<"organization_invitation">>).

-spec user_table() -> binary().
user_table() ->
    elib_pg_sql:public_tablename(<<"user">>).

-spec one_tx(any(), binary(), list()) -> {ok, map()} | {error, not_found | term()}.
one_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 从 epgsql 错误元组提取 SQLSTATE（elib_pg 已收敛为 {error, Reason}）。
-spec error_code(term()) -> binary() | undefined.
error_code({error, _S, Code, _Cn, _Msg, _Extra}) when is_binary(Code) ->
    Code;
error_code(_) ->
    undefined.
