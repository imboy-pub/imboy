-module(talk_record_grant_repo).
-moduledoc "面聊记录显式授权仓储（T17）：grants 全量替换/撤销/候选目录。".
%%%
% talk_record_grant 数据仓库层（迁移 00000211 §3）。
%
% 语义（§2 grant 表 / §4 grants 端点 / §11.3 撤权边界）：
%   * 活跃集合 = revoked_at IS NULL（过期 expires_at 由读侧谓词过滤，
%     本层不做时间判断，与 find_active_grant 同口径）；
%   * 全量 PUT（replace_tx）：先撤销不在提交清单中的活跃行，再对提交
%     条目逐条 upsert——ON CONFLICT 命中部分唯一索引
%     uq_trg_active(record_id, grantee_user_id) WHERE revoked_at IS NULL，
%     原子完成“活跃行更新 / 无活跃行插入（历史 revoked 行保留，撤销后
%     可重新授权）”；调用方须已持 talk_record 行锁（lock_for_acl_tx）；
%   * 候选目录（page_candidates_tx，§8 share-candidates）：org 场景 =
%     同 org active 成员 + 账号有效（u.status=1 AND u.account_type=0，
%     与 organization_admin_logic valid_user 同判据）+（ws 绑定时）候选
%     具备 ws 资源授权（复用 organization_resource_access_repo 统一谓词，
%     UidExpr 内嵌候选行 om.user_id 列引用——表达式来自源码常量）；
%     personal 场景 = 有效本人或双向 status=1 好友，提交仍重验资格；
%   * 目录列（display_name/account/avatar）取 "user" 行最小信息（§11.3）。
%%%

-export([
    list_active_tx/2,
    list_active_tx/3,
    find_active_by_grantee_tx/3,
    replace_tx/3,
    revoke_tx/3,
    page_candidates_tx/2
]).

-define(GRANT_COLUMNS,
    <<"g.id,g.record_id,g.grantee_user_id,u.nickname AS grantee_display_name,",
        "g.role,g.allow_export,g.granted_by,g.revoked_at,g.expires_at">>
).

%% ===================================================================
%% 查询
%% ===================================================================

%% @doc 记录活跃授权全量（owner 视图；含 allow_export/expires_at；
%% revoked_at 恒 null——revoked 行属服务端历史，不进对外名单，§2 活跃
%% UNIQUE + T18 冻结“对外仅活跃名单”）。
-spec list_active_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
list_active_tx(Conn, RecordId) ->
    list_active_tx(Conn, RecordId, undefined).

%% @doc GranteeFilter 为 integer() 时仅取该被授权人的活跃行（非 owner
%% “只看自己的权限”，§11.3）；undefined 取该记录全量。
-spec list_active_tx(any(), integer(), integer() | undefined) ->
    {ok, [map()]} | {error, term()}.
list_active_tx(Conn, RecordId, GranteeFilter) ->
    {Frag, Params} =
        case GranteeFilter of
            undefined -> {<<>>, [RecordId]};
            Uid -> {<<" AND g.grantee_user_id = $2">>, [RecordId, Uid]}
        end,
    Sql =
        <<"SELECT ", ?GRANT_COLUMNS/binary, " FROM talk_record_grant g",
            " LEFT JOIN \"user\" u ON u.id = g.grantee_user_id",
            " WHERE g.record_id = $1 AND g.revoked_at IS NULL", Frag/binary,
            " ORDER BY g.created_at DESC, g.id DESC">>,
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 指定被授权人的活跃行（非 owner 视图/票据校验共用）。
-spec find_active_by_grantee_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_active_by_grantee_tx(Conn, RecordId, Uid) ->
    Sql =
        <<"SELECT ", ?GRANT_COLUMNS/binary, " FROM talk_record_grant g",
            " LEFT JOIN \"user\" u ON u.id = g.grantee_user_id",
            " WHERE g.record_id = $1 AND g.grantee_user_id = $2",
            " AND g.revoked_at IS NULL LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [RecordId, Uid]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 全量替换 / 撤销（调用方持 talk_record 行锁）
%% ===================================================================

%% @doc 全量 PUT（§4：owner 提交全量受权用户/角色/export/expiry，事务
%% 更新）。Entries 为已校验归一化条目
%% #{grantee_user_id, role, allow_export, expires_at}。
%% 返回 {ok, Revoked, Upserted}（计数，审计用）。
-spec replace_tx(any(), integer(), [map()]) ->
    {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
replace_tx(Conn, RecordId, Entries) ->
    GranteeIds = [maps:get(grantee_user_id, E) || E <- Entries],
    {ok, Revoked} = revoke_not_in_tx(Conn, RecordId, GranteeIds),
    Upserted = lists:foldl(
        fun(Entry, Acc) ->
            case upsert_one_tx(Conn, RecordId, Entry) of
                ok -> Acc + 1;
                {error, _} = E -> throw(E)
            end
        end,
        0,
        Entries
    ),
    {ok, Revoked, Upserted}.

%% @private 撤销不在提交清单中的活跃行（空清单 = 全撤，语义上 owner
%% 仍保留主表授权——不能移除最后 owner，owner 在主表）。
%% 参数序恒定：$1 = now、$2 = record_id、$3.. = 清单内 grantee ids。
-spec revoke_not_in_tx(any(), integer(), [integer()]) ->
    {ok, non_neg_integer()} | {error, term()}.
revoke_not_in_tx(Conn, RecordId, GranteeIds) ->
    Now = elib_dt:now(),
    NotInFrag =
        case GranteeIds of
            [] ->
                <<>>;
            _ ->
                Ph = [
                    <<"$", (integer_to_binary(N))/binary>>
                 || N <- lists:seq(3, length(GranteeIds) + 2)
                ],
                List = join_binary(Ph, <<", ">>),
                <<" AND g.grantee_user_id NOT IN (", List/binary, ")">>
        end,
    Sql =
        <<"UPDATE talk_record_grant g SET revoked_at = $1, updated_at = $1",
            " WHERE g.record_id = $2 AND g.revoked_at IS NULL", NotInFrag/binary>>,
    case elib_pg:execute(Conn, Sql, [Now, RecordId] ++ GranteeIds) of
        {ok, N} when is_integer(N) -> {ok, N};
        {ok, _} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

%% @private 单条 upsert：活跃行更新 / 无活跃行插入（部分唯一索引冲突
%% 目标；历史 revoked 行保留，重授权 = 新活跃行）。granted_by/updated_at
%% 随提交刷新（最后一次提交即当前事实）。
-spec upsert_one_tx(any(), integer(), map()) -> ok | {error, term()}.
upsert_one_tx(Conn, RecordId, #{
    grantee_user_id := GranteeId,
    role := Role,
    allow_export := AllowExport,
    expires_at := ExpiresAt,
    granted_by := ActorId
}) ->
    Id = elib_tsid:generate(talk_record),
    Now = elib_dt:now(),
    Sql =
        <<"INSERT INTO talk_record_grant",
            "(id, record_id, grantee_user_id, role, allow_export, granted_by, expires_at,",
            " created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $8)",
            " ON CONFLICT (record_id, grantee_user_id) WHERE revoked_at IS NULL",
            " DO UPDATE SET role = $4, allow_export = $5, granted_by = $6,",
            " expires_at = $7, updated_at = $8">>,
    case
        elib_pg:execute(Conn, Sql, [
            Id, RecordId, GranteeId, Role, AllowExport, ActorId, ExpiresAt, Now
        ])
    of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 单条撤销（§4 DELETE /:id/grants/:grant_id：立即生效——读侧活跃
%% 谓词与票据 acl_revision 校验同步拒绝）。仅活跃行可撤；0 行 →
%% grant_not_found（调用方映射 404）。
-spec revoke_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, grant_not_found | term()}.
revoke_tx(Conn, RecordId, GrantId) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE talk_record_grant g SET revoked_at = $1, updated_at = $1",
            " WHERE g.record_id = $2 AND g.id = $3 AND g.revoked_at IS NULL",
            " RETURNING grantee_user_id, role">>,
    case elib_pg:query(Conn, Sql, [Now, RecordId, GrantId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, grant_not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 候选目录（§8 share-candidates；q 限长由 logic 层保证）
%% ===================================================================

%% @doc 候选分页（keyset：u.id DESC 稳定倒序）。
%% Spec：
%%   scope         :: <<"organization">> | <<"personal">>
%%   actor_uid     :: integer()          -- 排除自己（generic 场景）
%%   organization_id :: integer()        -- org 场景必填（logic 已校验归属权）
%%   workspace_id  :: integer() | 0      -- ws 绑定（候选须有 ws 资源授权）
%%   exclude_uids  :: [integer()]        -- record 级排除（owner + 活跃被授权人）
%%   q             :: binary() | <<>>    -- nickname/account ILIKE
%%   cursor_id     :: integer() | undefined
%%   limit         :: pos_integer()
%% 返回 {ok, Rows}（user_id/display_name/account/avatar 最小列）。
-spec page_candidates_tx(any(), map()) -> {ok, [map()]} | {error, term()}.
page_candidates_tx(Conn, Spec) ->
    Limit = maps:get(limit, Spec),
    Q = maps:get(q, Spec, <<>>),
    CursorId = maps:get(cursor_id, Spec, undefined),
    {BaseFrag, BaseParams} = candidates_base(Spec),
    {QFrag, QParams} = candidates_q(Q, length(BaseParams) + 1),
    {CursorFrag, CursorParams} = candidates_cursor(
        CursorId, length(BaseParams) + length(QParams) + 1
    ),
    LimitPh = placeholder(Limit, length(BaseParams) + length(QParams) + length(CursorParams) + 1),
    Sql =
        <<BaseFrag/binary, QFrag/binary, CursorFrag/binary, " ORDER BY u.id DESC LIMIT ",
            LimitPh/binary>>,
    elib_pg:query(Conn, Sql, BaseParams ++ QParams ++ CursorParams ++ [Limit]).

%% @private FROM/WHERE 主体：org = 成员目录 + 账号有效 +（ws）资源授权；
%% personal = 有效本人或双向有效好友。
%% 参数序：$1 = actor_uid；org 场景 $2 = organization_id。
-spec candidates_base(map()) -> {binary(), [term()]}.
candidates_base(
    #{scope := <<"organization">>, actor_uid := ActorUid, organization_id := OrgId} = Spec
) ->
    Excluded = exclusion_frag(Spec),
    WsFrag = ws_candidate_frag(Spec),
    Sql =
        <<"SELECT u.id AS user_id, u.nickname AS display_name, u.account, u.avatar",
            " FROM organization_member om JOIN \"user\" u ON u.id = om.user_id",
            " WHERE om.organization_id = $2 AND om.status = 'active'",
            " AND u.status = 1 AND u.account_type = 0", " AND u.id <> $1", Excluded/binary,
            WsFrag/binary>>,
    {Sql, [ActorUid, OrgId]};
candidates_base(#{scope := <<"personal">>, actor_uid := ActorUid} = Spec) ->
    Excluded = exclusion_frag(Spec),
    Personal = talk_record_directory_repo:personal_predicate(),
    Sql =
        <<"SELECT u.id AS user_id, u.nickname AS display_name, u.account, u.avatar",
            " FROM \"user\" u WHERE u.status = 1 AND u.account_type = 0", " AND ", Personal/binary,
            Excluded/binary>>,
    {Sql, [ActorUid]}.

%% @private record 级排除（owner + 活跃被授权人不在候选中）：参数接在
%% 基础参数之后（N = 起始占位序号）。
-spec exclusion_frag(map()) -> binary().
exclusion_frag(#{record_id := RecordId, owner_uid := OwnerUid, exclude_uids := Excluded}) when
    is_integer(RecordId), is_integer(OwnerUid)
->
    ExList =
        case Excluded of
            [] ->
                <<>>;
            Ids ->
                Frag = join_binary([value_sql(I) || I <- Ids], <<", ">>),
                <<" AND u.id NOT IN (", Frag/binary, ")">>
        end,
    <<" AND u.id <> ", (integer_to_binary(OwnerUid))/binary,
        " AND NOT EXISTS (SELECT 1 FROM talk_record_grant gc", " WHERE gc.record_id = ",
        (integer_to_binary(RecordId))/binary,
        " AND gc.grantee_user_id = u.id AND gc.revoked_at IS NULL)", ExList/binary>>;
exclusion_frag(#{exclude_uids := []}) ->
    <<>>;
exclusion_frag(#{exclude_uids := Ids}) when is_list(Ids) ->
    Frag = join_binary([value_sql(I) || I <- Ids], <<", ">>),
    <<" AND u.id NOT IN (", Frag/binary, ")">>;
exclusion_frag(_) ->
    <<>>.

%% @private ws 绑定：候选须满足 ws 资源授权谓词（统一谓词内嵌候选
%% om.user_id 列引用；WsId 来自记录行/已校验参数，源码内 integer 安全）。
-spec ws_candidate_frag(map()) -> binary().
ws_candidate_frag(#{workspace_id := WsId}) when is_integer(WsId), WsId > 0 ->
    WsPred = organization_resource_access_repo:workspace_predicate_sql(
        integer_to_binary(WsId), <<"om.user_id">>
    ),
    <<" AND ", WsPred/binary>>;
ws_candidate_frag(_) ->
    <<>>.

%% @private q 过滤（nickname/account ILIKE，模式由 logic 层转义拼装）。
-spec candidates_q(binary(), pos_integer()) -> {binary(), [term()]}.
candidates_q(<<>>, _N) ->
    {<<>>, []};
candidates_q(Pattern, N) ->
    Ph = placeholder(Pattern, N),
    {<<" AND (u.nickname ILIKE ", Ph/binary, " OR u.account ILIKE ", Ph/binary, ")">>, [Pattern]}.

%% @private keyset 游标（u.id DESC → 下一页 id < cursor_id）。
-spec candidates_cursor(integer() | undefined, pos_integer()) -> {binary(), [term()]}.
candidates_cursor(undefined, _N) ->
    {<<>>, []};
candidates_cursor(CursorId, N) ->
    {<<" AND u.id < ", (placeholder(CursorId, N))/binary>>, [CursorId]}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec placeholder(term(), pos_integer()) -> binary().
placeholder(_V, N) ->
    <<"$", (integer_to_binary(N))/binary>>.

-spec value_sql(integer()) -> binary().
value_sql(V) when is_integer(V) ->
    integer_to_binary(V).

-spec join_binary([binary()], binary()) -> binary().
join_binary([], _Sep) ->
    <<>>;
join_binary([P], _Sep) ->
    P;
join_binary([P | Rest], Sep) ->
    <<P/binary, Sep/binary, (join_binary(Rest, Sep))/binary>>.
