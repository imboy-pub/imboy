-module(resource_create_receipt_repo).

-moduledoc "资源创建受理回执仓储层（channel-group-ux v2.2 W04.b，plan §6.4 C1）。".
%%%
% resource_create_receipt_repo 只服务本计划两类创建（create_group /
% create_channel）的受理回执：身份唯一键 (uid, operation, request_id)。
%
% 与 enterprise_internal_idempotency_repo 的关系：仅复用其
% INSERT ON CONFLICT 占位 + READ COMMITTED 行为的事务模式和摘要工具；
% **不照搬 TTL 过期重置语义**——本表无 expires_at，receipt 永不因过期
% 重置（§6.4：7d 只约束服务端完整结果的保留义务；receipt 是去重事实，
% 账户删除按既有删除合同清理）。
%
% 同事务生命周期（缺一不可，任何一步失败全部回滚）：
%   serialize_tx → placeholder_tx（占位/冲突探测）
%     → inserted：调用方创建资源 + 初始名 + 成员 + 持久通知入队
%     → accept_tx（回填 resource_id + server_accepted_at）
%   placeholder 命中 existing（同键并发前事务已提交）→ 调用方走重放，
%   不再创建资源、不再入队通知。
%
% 规范载荷摘要（canonical_group_digest）：固定字段序列 UTF-8 序列化后
% SHA256（64 小写 hex）。排除 request_id、recover_only 和两端时间
% （§6.4）。成员按十进制规范值去重、移除创建者、升序数字排序——
% 「顺序不同/重复成员」不得当作新意图（J24.c）。
%%%

-export([
    tablename/0,
    serialize_tx/4,
    placeholder_tx/5,
    accept_tx/5,
    find_tx/4,
    find/3,
    mark_deleted_by_resource_tx/3,
    canonical_group_digest/1,
    sha256_hex/1
]).

-define(COLUMNS, <<
    "uid, operation, request_id, request_digest, resource_id, "
    "server_accepted_at, resource_deleted, created_at"
>>).

%% 7d 完整结果保留窗口（J24.b：服务端受理年龄独立决定完整结果；
%% 过窗后原键只查/重放都不得创建，返回 create_recovery_expired + 最小元信息）。
-define(FULL_RESULT_RETENTION_SQL, <<"7 days">>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"resource_create_receipt">>).

%% @doc 事务域请求仲裁：对 (uid, operation, request_id) 取事务级 advisory
%% lock，让同键并发在触碰 receipt 行/资源锁之前先串行化（镜像
%% enterprise_internal_idempotency_repo:serialize_tx 模式；唯一索引本身
%% 仍是第二道防线）。
-spec serialize_tx(any(), integer(), binary(), binary()) -> ok | {error, term()}.
serialize_tx(Conn, Uid, Operation, RequestId) when
    is_integer(Uid), is_binary(Operation), is_binary(RequestId)
->
    LockKey = <<
        "imboy:rcr:",
        (integer_to_binary(Uid))/binary,
        ":",
        Operation/binary,
        ":",
        RequestId/binary
    >>,
    case
        elib_pg:query(
            Conn,
            <<"SELECT pg_advisory_xact_lock(hashtextextended($1, 4096))">>,
            [LockKey]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内占位（或探测既有受理）：
%%   INSERT ... ON CONFLICT DO NOTHING 首插成功 → {ok, inserted, Row}，
%%   调用方继续创建资源并在同事务回填（accept_tx）；
%%   命中已提交行 → {ok, existing, Row}（Row 含 expired 布尔），
%%   调用方必须走重放语义，不得再创建资源/入队通知；
%%   同键并发在途时唯一索引等待前事务结束后按上述两种结果归位。
%%   摘要不一致不会在这里报错——existing 分支由调用方比对并返回
%%   request_id_conflict（本函数保持探测纯度）。
-spec placeholder_tx(any(), integer(), binary(), binary(), binary()) ->
    {ok, inserted | existing, map()} | {error, term()}.
placeholder_tx(Conn, Uid, Operation, RequestId, RequestDigest) when
    is_integer(Uid), is_binary(Operation), is_binary(RequestId), is_binary(RequestDigest)
->
    Tb = tablename(),
    InsertSql =
        <<"INSERT INTO ", Tb/binary, " (uid, operation, request_id, request_digest)",
            " VALUES ($1, $2, $3, $4)", " ON CONFLICT (uid, operation, request_id) DO NOTHING">>,
    case elib_pg:execute(Conn, InsertSql, [Uid, Operation, RequestId, RequestDigest]) of
        {ok, 1} ->
            {ok, Row} = find_tx(Conn, Uid, Operation, RequestId),
            {ok, inserted, Row};
        {ok, 0} ->
            case find_tx(Conn, Uid, Operation, RequestId) of
                {ok, Row} -> {ok, existing, Row};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内回填受理结果：资源创建 + 初始名 + 成员 + 通知入队全部
%% 成功后，回填 resource_id 与 server_accepted_at。WHERE resource_id IS NULL
%% 保证只回填一次；占位与回填同事务提交，已提交行恒有二者（J23.a：
%% 提交前崩溃则整行随事务消失，同键可再次首次受理）。
-spec accept_tx(any(), integer(), binary(), binary(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
accept_tx(Conn, Uid, Operation, RequestId, ResourceId) when
    is_integer(Uid), is_binary(Operation), is_binary(RequestId), is_integer(ResourceId)
->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET resource_id = $4, server_accepted_at = CURRENT_TIMESTAMP",
            " WHERE uid = $1 AND operation = $2 AND request_id = $3", " AND resource_id IS NULL",
            " RETURNING ", ?COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [Uid, Operation, RequestId, ResourceId]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按身份读取回执行（附服务端 7d 年龄判定，避免 Erlang 侧
%% 解析时间）。服务端年龄独立于客户端时钟（J24.b 两个时钟分工）。
-spec find_tx(any(), integer(), binary(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, Uid, Operation, RequestId) when
    is_integer(Uid), is_binary(Operation), is_binary(RequestId)
->
    Sql = select_with_expired_sql(<<>>),
    case elib_pg:query(Conn, Sql, [Uid, Operation, RequestId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 连接池按身份读取回执行（重放快路径 / recover_only 只查路径）。
%% 只查操作本身不新增受理占位/资源/通知（§6.4 恢复状态表）。
-spec find(integer(), binary(), binary()) -> {ok, map()} | {error, not_found | term()}.
find(Uid, Operation, RequestId) when
    is_integer(Uid), is_binary(Operation), is_binary(RequestId)
->
    Sql = select_with_expired_sql(<<" LIMIT 1">>),
    case elib_pg:query(Sql, [Uid, Operation, RequestId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内按资源标记删除（群解散/频道删除同事务调用）：置位后同键
%% 重放不泄露内容、不复活对象，但 receipt 保留已受理事实、不把错误改为
%% 首次创建（J24.b 删除例外）。
-spec mark_deleted_by_resource_tx(any(), binary(), integer()) ->
    ok | {error, term()}.
mark_deleted_by_resource_tx(Conn, Operation, ResourceId) when
    is_binary(Operation), is_integer(ResourceId)
->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET resource_deleted = true",
            " WHERE operation = $1 AND resource_id = $2">>,
    case elib_pg:execute(Conn, Sql, [Operation, ResourceId]) of
        {ok, _Count} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 规范载荷摘要（§6.4 事务与摘要）
%% ===================================================================

%% @doc 群创建规范载荷摘要。输入必须是已规范化载荷：
%%   #{title => TrimmedBinary（Unicode 原值仅 trim，不做大小写/NFKC）,
%%     scope => <<"personal">> | <<"workspace">>,
%%     workspace_id => integer（scope=workspace 时合法正 ID；personal 为 0）,
%%     members => [integer()]（已验证 ID、十进制去重、移除创建者、升序）}
%% 固定字段序列（UTF-8，换行分隔）：
%%   create_group\nscope=<s>\nworkspace_id=<n|空>\ntype=2\ntitle=<t>\nmembers=<d1>,<d2>
%% type 固定 2（服务端实际默认，补齐进规范形式）；personal 的 workspace_id
%% 段为空。摘要排除 request_id、recover_only 与两端时间——同键不同规范
%% 载荷才是冲突，顺序不同/重复成员不是（J24.c）。
-spec canonical_group_digest(map()) -> binary().
canonical_group_digest(#{
    title := Title, scope := Scope, workspace_id := WorkspaceId, members := Members
}) when
    is_binary(Title), is_binary(Scope), is_integer(WorkspaceId), is_list(Members)
->
    WsSeg =
        case Scope of
            <<"workspace">> -> integer_to_binary(WorkspaceId);
            <<"personal">> -> <<>>
        end,
    MembersSeg = iolist_to_binary(lists:join(<<",">>, [integer_to_binary(M) || M <- Members])),
    Canonical = iolist_to_binary([
        <<"create_group">>,
        <<"\nscope=">>,
        Scope,
        <<"\nworkspace_id=">>,
        WsSeg,
        <<"\ntype=2">>,
        <<"\ntitle=">>,
        Title,
        <<"\nmembers=">>,
        MembersSeg
    ]),
    sha256_hex(Canonical).

%% @doc SHA256 十六进制（64 小写字符）。
-spec sha256_hex(binary()) -> binary().
sha256_hex(Bin) when is_binary(Bin) ->
    string:lowercase(binary:encode_hex(crypto:hash(sha256, Bin))).

%% ===================================================================
%% Internal
%% ===================================================================

select_with_expired_sql(Suffix) ->
    Tb = tablename(),
    <<
        "SELECT ",
        ?COLUMNS/binary,
        ", (server_accepted_at < CURRENT_TIMESTAMP - INTERVAL '",
        (?FULL_RESULT_RETENTION_SQL)/binary,
        "') AS expired FROM ",
        Tb/binary,
        " WHERE uid = $1 AND operation = $2 AND request_id = $3",
        Suffix/binary
    >>.
