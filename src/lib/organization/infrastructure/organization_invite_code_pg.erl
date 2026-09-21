-module(organization_invite_code_pg).

%% Organization Invite Code 读写 SQL（infrastructure，事务内使用；GZAPP-01）。
%%
%% 只封装语句与行集；锁序与业务裁决在 organization_invite_code_app。
%% 镜像 workspace_invite_repo（迁移 00000082 团队码）的 org 侧版本：
%%   * code 8 位 A-Z2-9（排除 0/O/1/I），全局唯一，撤销后作废不复用；
%%   * 过期判定由 SQL 同行计算 expired 布尔，避免 Erlang 侧解析时间格式；
%%   * 所有查找显式贯穿 organization_id（铁律 6：同语句 Org 作用域）——
%%     跨 Org 输码与码不存在同样命中不了行（not_found → 981，
%%     不泄露组织存在性）。

-export([
    generate_code/0,
    add_tx/5,
    find_active_by_code_tx/3,
    find_active_by_org_tx/2,
    revoke_active_by_org_tx/2
]).

%% 邀请码字符集：大写字母（排除 I/O）+ 数字 2-9（32 个；
%% 排除 0/O/1/I 手抄混淆字符——与 workspace_invite_repo 同一字符集）。
-define(INVITE_CHARSET, <<"ABCDEFGHJKLMNPQRSTUVWXYZ23456789">>).
-define(INVITE_CODE_LEN, 8).

-define(ROW_COLS,
    "id, organization_id, code, created_by,"
    "       extract(epoch from expires_at)::bigint AS expires_at,"
    "       (expires_at < CURRENT_TIMESTAMP) AS expired,"
    "       status,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
).

%%--------------------------------------------------------------------
%% 生成
%%--------------------------------------------------------------------

%% @doc 生成组织邀请码（8 位，A-Z + 2-9；rand:uniform 逐位抽取）
%% 镜像 workspace_invite_repo:generate_invite_code/0 的写法。
-spec generate_code() -> binary().
generate_code() ->
    generate_code_chars(?INVITE_CODE_LEN, <<>>).

%%--------------------------------------------------------------------
%% 写
%%--------------------------------------------------------------------

%% @doc 事务内插入邀请码行（id 由本层 elib_tsid 生成；expires_at epoch 秒）。
%% code 撞全局唯一约束或部分唯一索引 uk_organization_invite_code_org_active
%% （一组织至多一个 active 码）均归一 {error, code_conflict}，
%% 由调用方（app 层）重新生成码后重试。
-spec add_tx(any(), integer(), binary(), integer() | null, integer()) ->
    {ok, map()} | {error, code_conflict | term()}.
add_tx(Conn, OrgId, Code, CreatedBy, ExpiresAtEpoch) ->
    Tb = code_table(),
    Id = elib_tsid:generate(organization_invite_code),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, organization_id, code, created_by, expires_at, status, created_at, updated_at)",
            " VALUES ($1, $2, $3, $4, to_timestamp($5), 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)",
            " RETURNING ", ?ROW_COLS>>,
    case elib_pg:query(Conn, Sql, [Id, OrgId, Code, CreatedBy, ExpiresAtEpoch]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, insert_empty_result};
        {error, Reason} = Err ->
            case error_code(Reason) of
                <<"23505">> -> {error, code_conflict};
                _ -> Err
            end
    end.

%% @doc 事务内按组织撤销全部有效邀请码（一组织至多一个 active 码的维护
%% 口径：create 前置撤旧【重新生成=旧码失效】+ Owner/Admin 主动撤销共用）。
%% 幂等：无 active 码返回 {ok, 0}。
-spec revoke_active_by_org_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
revoke_active_by_org_tx(Conn, OrgId) ->
    Tb = code_table(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = 'revoked', updated_at = CURRENT_TIMESTAMP",
            " WHERE organization_id = $1 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [OrgId]) of
        {ok, Count} when is_integer(Count) -> {ok, Count};
        {ok, _, _} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% 读
%%--------------------------------------------------------------------

%% @doc 事务内按 (org, code) 查有效邀请码（status=active；过期与否由调用方判定）。
%% 同语句 org 作用域：跨 Org 输码命中不了行（not_found → 981，不泄露存在性）。
%% 含已撤销码 → not_found（981 口径）。
-spec find_active_by_code_tx(any(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_active_by_code_tx(Conn, OrgId, Code) ->
    Sql =
        <<"SELECT ", ?ROW_COLS, " FROM ", (code_table())/binary,
            " WHERE organization_id = $1 AND code = $2 AND status = 'active' LIMIT 1">>,
    one_tx(Conn, Sql, [OrgId, Code]).

%% @doc 事务内读组织当前 active 码（治理面 GET；无 active 码 → not_found）。
-spec find_active_by_org_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_active_by_org_tx(Conn, OrgId) ->
    Sql =
        <<"SELECT ", ?ROW_COLS, " FROM ", (code_table())/binary,
            " WHERE organization_id = $1 AND status = 'active'",
            " ORDER BY created_at DESC, id DESC LIMIT 1">>,
    one_tx(Conn, Sql, [OrgId]).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

-spec code_table() -> binary().
code_table() ->
    elib_pg_sql:public_tablename(<<"organization_invite_code">>).

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

%% @doc 逐位抽取邀请码（rand:uniform/1 返回 1..Len，转 0 起下标）
-spec generate_code_chars(non_neg_integer(), binary()) -> binary().
generate_code_chars(0, Acc) ->
    Acc;
generate_code_chars(N, Acc) ->
    Chars = ?INVITE_CHARSET,
    Pos = rand:uniform(byte_size(Chars)) - 1,
    <<_:Pos/binary, Char:1/binary, _/binary>> = Chars,
    generate_code_chars(N - 1, <<Acc/binary, Char/binary>>).
