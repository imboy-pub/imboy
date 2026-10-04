-module(enterprise_attachment_retention_repo).

-moduledoc "企业附件留存/法务 hold/purge 账本仓储层。".
%%%
% enterprise_attachment_retention_repo 是企业**附件**留存/法务 hold/purge 账本
% 仓储（FULL-02 / plan-full §3.1「企业附件 … retention/hold/purge 不变量」；
% 表由 migration 00000140 建立）。
%
%% 状态机（不变量在 DB 层声明式强制，应用层只是调用面）：
%   live  --hold-->  live(held)        （hold 生效期间 purge 被 23514 拒绝）
%   live  --purge--> purged            （仅当 hold_state='none' 且 retention 到期）
%   purged                             （终态：不可回退）
%   retention_until                    （只可延长，缩短被 23514 拒绝）
%   治理行禁止物理删除（BEFORE DELETE 23514）
%% 全部函数是事务内形态（Conn 直连）。purge/4 只改治理行状态，**不**删对象——
% S3/Garage 对象删除由 logic 层在同事务内显式执行（先 DB 守卫、后对象删除，
% 保证 hold 生效时绝不会先删掉对象）。
%%%

-export([
    tablename/0,
    view_purgeable/0,
    upsert_tx/4,
    find_tx/2,
    extend_tx/3,
    hold_tx/3,
    release_tx/2,
    purge_tx/2,
    is_purgeable_tx/2
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "attachment_id, organization_id, application_id, retention_until, hold_state,"
    " hold_reason, hold_set_at, purge_state, purged_at, created_at, updated_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_attachment_retention">>).

-spec view_purgeable() -> binary().
view_purgeable() ->
    elib_pg_sql:public_tablename(<<"v_efapi_attachment_purgeable">>).

%% @doc 事务内登记留存（confirm 转正时同事务写入；重复 confirm 同一对象为
%% 幂等 upsert：**只延长** retention_until，不缩短（缩短会被触发器 23514 拒绝）。
-spec upsert_tx(any(), pos_integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
upsert_tx(Conn, AttachmentId, OrgId, AppId) when
    is_integer(AttachmentId), AttachmentId > 0
->
    Sql =
        <<"INSERT INTO ", (tablename())/binary,
            " (attachment_id, organization_id, application_id, retention_until,"
            "  hold_state, purge_state, created_at, updated_at)",
            " VALUES ($1, $2, $3, CURRENT_TIMESTAMP + ($4 || ' days')::interval,"
            "  'none', 'live', NOW(), NOW())", " ON CONFLICT (attachment_id) DO UPDATE",
            "  SET retention_until = GREATEST(", (tablename())/binary,
            ".retention_until, EXCLUDED.retention_until), updated_at = NOW()", " RETURNING ",
            ?COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [AttachmentId, OrgId, AppId, retention_days()]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, upsert_empty_result};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内按 attachment_id 读治理行（Org/App 边界由复合 FK 保证）。
-spec find_tx(any(), pos_integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, AttachmentId) when is_integer(AttachmentId), AttachmentId > 0 ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE attachment_id = $1 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [AttachmentId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内延长留存到指定时间点（只延长：缩短被 DB 触发器 23514 拒绝，
%% 归一 {error, retention_shrink_rejected}）。返回新 retention_until。
-spec extend_tx(any(), pos_integer(), binary()) ->
    {ok, binary()} | {error, retention_shrink_rejected | not_found | term()}.
extend_tx(Conn, AttachmentId, RetentionUntil) when
    is_integer(AttachmentId), AttachmentId > 0, is_binary(RetentionUntil)
->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET retention_until = $2::timestamptz, updated_at = NOW()",
            " WHERE attachment_id = $1 RETURNING retention_until">>,
    case elib_pg:query(Conn, Sql, [AttachmentId, RetentionUntil]) of
        {ok, [#{<<"retention_until">> := New} | _]} ->
            {ok, New};
        {ok, []} ->
            {error, not_found};
        {error, #error{code = <<"23514">>}} ->
            {error, retention_shrink_rejected};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内设置法务 hold（held 需要非空 reason，CHECK 强制）。
%% 已 held 再 hold → 覆盖 reason（幂等语义由调用方按需判定）。
-spec hold_tx(any(), pos_integer(), binary()) -> ok | {error, not_found | term()}.
hold_tx(Conn, AttachmentId, Reason) when
    is_integer(AttachmentId), AttachmentId > 0, is_binary(Reason), Reason =/= <<>>
->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET hold_state = 'held', hold_reason = $2, hold_set_at = NOW(), updated_at = NOW()",
            " WHERE attachment_id = $1 AND purge_state = 'live'">>,
    case elib_pg:execute(Conn, Sql, [AttachmentId, Reason]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason2} -> {error, Reason2}
    end.

%% @doc 事务内释放法务 hold（held -> none；reason 随 CHECK 清空）。
%% 未持有时返回 {error, not_found}（调用方转 state 错误）。
-spec release_tx(any(), pos_integer()) -> ok | {error, not_found | term()}.
release_tx(Conn, AttachmentId) when is_integer(AttachmentId), AttachmentId > 0 ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET hold_state = 'none', hold_reason = NULL, hold_set_at = NULL,"
            " updated_at = NOW()",
            " WHERE attachment_id = $1 AND hold_state = 'held' AND purge_state = 'live'">>,
    case elib_pg:execute(Conn, Sql, [AttachmentId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内把治理行推到 purged（终态）。**不变量由 DB 触发器强制**：
%% hold 生效中 / retention 未到期 → 23514，归一 {error, purge_blocked}；
%% 已是 purged → 0 行归一 {error, already_purged}。
-spec purge_tx(any(), pos_integer()) ->
    {ok, binary()} | {error, purge_blocked | already_purged | not_found | term()}.
purge_tx(Conn, AttachmentId) when is_integer(AttachmentId), AttachmentId > 0 ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET purge_state = 'purged', purged_at = NOW(), updated_at = NOW()",
            " WHERE attachment_id = $1 AND purge_state = 'live' RETURNING purged_at">>,
    case elib_pg:query(Conn, Sql, [AttachmentId]) of
        {ok, [#{<<"purged_at">> := At} | _]} ->
            {ok, At};
        {ok, []} ->
            case find_tx(Conn, AttachmentId) of
                {ok, #{<<"purge_state">> := <<"purged">>}} -> {error, already_purged};
                {ok, _} -> {error, purge_blocked};
                {error, not_found} -> {error, not_found};
                {error, Reason} -> {error, Reason}
            end;
        {error, #error{code = <<"23514">>}} ->
            {error, purge_blocked};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 可 purge 判定（读时求值视图）：
%% purge_state='live' AND hold_state='none' AND CURRENT_TIMESTAMP >= retention_until。
-spec is_purgeable_tx(any(), pos_integer()) -> {ok, boolean()} | {error, term()}.
is_purgeable_tx(Conn, AttachmentId) when is_integer(AttachmentId), AttachmentId > 0 ->
    Sql =
        <<"SELECT EXISTS (SELECT 1 FROM ", (view_purgeable())/binary,
            " WHERE attachment_id = $1) AS purged_ok">>,
    case elib_pg:query(Conn, Sql, [AttachmentId]) of
        {ok, [Row | _]} -> {ok, maps:get(<<"purged_ok">>, Row)};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% 默认留存窗口（天）。企业托管附件的默认治理口径：一年。
-spec retention_days() -> pos_integer().
retention_days() ->
    case config_ds:env(imboy, enterprise_attachment_retention_days, 365) of
        N when is_integer(N), N > 0 -> N;
        _ -> 365
    end.
