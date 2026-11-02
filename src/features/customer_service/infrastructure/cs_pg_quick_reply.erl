%% CS Quick Reply 数据访问（合同 v3/specs/cs-quick-reply.md §1/§2/§4）。
%% 软删行对一切读不可见；排序：Admin `sort ASC, id ASC`、Seat `sort ASC, title ASC`。
%%
%% 架构（铁律5/6）：本模块是 `cs_quick_reply_app` 的持久化扩展点实现
%% （cs_pg_* 同族命名，镜像 cs_pg_widget/cs_pg_session）；全部 SQL 携带
%% organization_id 租户约束。例外是 find_tx：它是越权裁决的可见性探针——
%% org/workspace 随行**输出**（权威派生租户），谓词只按 id+未删，跨范围的
%% 403/404 区分由 application 同事务内裁决（形状与 cs_pg_widget 的全局反查
%% 同类：谓词零 Org 是语义所需，由 cs_quick_reply_tests 机械断言冻结）；
%% update/soft_delete 在探针之外再携带 org+ws 谓词（纵深防御）。
-module(cs_pg_quick_reply).

-export([
    find_tx/2,
    insert_tx/2,
    list_tx/3,
    page_tx/4,
    seat_assignment_facts/3,
    soft_delete_tx/4,
    update_tx/5,
    with_tx/1
]).

-define(COLS, <<
    "id, tenant_id, workspace_id, title, content, sort, created_by,"
    " extract(epoch from created_at)::bigint AS created_at,"
    " extract(epoch from updated_at)::bigint AS updated_at"
>>).

tablename() -> <<"cs_quick_reply">>.

%% @doc 事务组合入口：application 层在**同一事务**内做多步裁决
%% （find_tx → 越权判定 → update/soft_delete）时的唯一入口；连接句柄
%% 对本模块外不透明（application 不引用 elib_pg，铁律5）。
with_tx(Fun) when is_function(Fun, 1) -> elib_pg:with_tx(Fun).

%% @doc 创建。Caller 已完成字段校验（trim 长度界）；Id 由调用方发 TSID。
insert_tx(Conn, Row) when is_map(Row) ->
    Sql = [
        <<"INSERT INTO ">>,
        tablename(),
        <<
            " (id, tenant_id, workspace_id, title, content, sort, created_by)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7) RETURNING "
        >>,
        ?COLS
    ],
    case
        elib_pg:query(Conn, iolist_to_binary(Sql), [
            maps:get(id, Row),
            maps:get(organization_id, Row),
            maps:get(workspace_id, Row),
            maps:get(title, Row),
            maps:get(content, Row),
            maps:get(sort, Row, 0),
            maps:get(created_by, Row)
        ])
    of
        {ok, [Row0]} -> {ok, Row0};
        {ok, []} -> {error, insert_not_returned};
        {error, _} = E -> E
    end.

%% @doc 全量更新（合同 §2.3）。WHERE 携带 org+ws（纵深防御：application
%% 已先经 find_tx 裁决越权）。目标行软删或跨租户 → not_found（403/404
%% 由调用方按申报范围裁决，本层只给「不可见」事实）。
update_tx(Conn, Id, OrgId, WorkspaceId, Fields) when
    is_integer(Id), is_integer(OrgId), is_integer(WorkspaceId), is_map(Fields)
->
    %% 先锁读可见行（同事务；软删/不存在 → not_found），再走 execute 落库
    %% （UPDATE RETURNING 经 query/3 会被空集收敛——见 soft_delete_tx 注）。
    case find_tx(Conn, Id) of
        {error, not_found} ->
            {error, not_found};
        {error, _} = E ->
            E;
        {ok, _} ->
            case
                elib_pg:execute(
                    Conn,
                    iolist_to_binary([
                        <<"UPDATE ">>,
                        tablename(),
                        <<
                            " SET title=$4, content=$5, sort=$6, updated_at=now()"
                            " WHERE id=$1 AND tenant_id=$2 AND workspace_id=$3"
                            " AND deleted_at IS NULL"
                        >>
                    ]),
                    [
                        Id,
                        OrgId,
                        WorkspaceId,
                        maps:get(title, Fields),
                        maps:get(content, Fields),
                        maps:get(sort, Fields, 0)
                    ]
                )
            of
                {ok, N} when is_integer(N), N >= 1 ->
                    find_tx(Conn, Id);
                {ok, 0} ->
                    {error, not_found};
                {error, _} = E ->
                    E
            end
    end.

%% @doc 软删（合同 §2.4 幂等）：已删目标重复 DELETE 同样返回 ok。
%% WHERE 携带 org+ws（纵深防御，同 update_tx）。
soft_delete_tx(Conn, Id, OrgId, WorkspaceId) when
    is_integer(Id), is_integer(OrgId), is_integer(WorkspaceId)
->
    %% UPDATE 走 execute（elib_pg:query 是 SELECT 收口形态——会把非 SELECT
    %% 的 {ok, Count} 收敛为 {ok, []}，UPDATE 经它执行属调用方误用）。
    case
        elib_pg:execute(
            Conn,
            iolist_to_binary([
                <<"UPDATE ">>,
                tablename(),
                <<
                    " SET deleted_at=now(), updated_at=now()"
                    " WHERE id=$1 AND tenant_id=$2 AND workspace_id=$3"
                    " AND deleted_at IS NULL"
                >>
            ]),
            [Id, OrgId, WorkspaceId]
        )
    of
        {ok, N} when is_integer(N), N >= 1 -> {ok, deleted};
        {ok, 0} -> {ok, already_deleted};
        {error, _} = E -> E
    end.

%% @doc Admin 分页（显式租户+工作区条件；search=title ILIKE 包含匹配）。
page_tx(Conn, OrgId, WorkspaceId, Opts) when
    is_integer(OrgId), is_integer(WorkspaceId), is_map(Opts)
->
    Search = maps:get(search, Opts, <<>>),
    Offset = (maps:get(page, Opts, 1) - 1) * maps:get(size, Opts, 10),
    Limit = maps:get(size, Opts, 10),
    %% 全内插 squery（简单协议，无 prepared 复用形态耦合）：Org/W 是 cs_actions
    %% 类型门收敛的 integer；search 走单引号加倍转义（ILIKE 字面量；%/_ 通配
    %% 语义保留——合同 §2.1 允许包含匹配）。
    OrgBin = integer_to_binary(OrgId),
    WsBin = integer_to_binary(WorkspaceId),
    LimitBin = integer_to_binary(Limit),
    OffsetBin = integer_to_binary(Offset),
    Where =
        case is_binary(Search) andalso Search =/= <<>> of
            true ->
                Escaped = binary:replace(Search, <<"\'">>, <<"\'\'">>, [global]),
                [
                    <<" AND title ILIKE '%">>, Escaped, <<"%'">>
                ];
            false ->
                <<>>
        end,
    Base = [
        <<"FROM ">>,
        tablename(),
        <<" WHERE tenant_id=">>,
        OrgBin,
        <<" AND workspace_id=">>,
        WsBin,
        <<" AND deleted_at IS NULL">>
    ],
    CountSql = iolist_to_binary([<<"SELECT count(*)::int AS n ">>, Base, Where]),
    ListSql = iolist_to_binary([
        <<"SELECT ">>,
        ?COLS,
        <<" ">>,
        Base,
        Where,
        <<" ORDER BY sort ASC, id ASC LIMIT ">>,
        LimitBin,
        <<" OFFSET ">>,
        OffsetBin
    ]),
    case elib_pg:query(Conn, CountSql, []) of
        {ok, [#{<<"n">> := Total}]} ->
            case elib_pg:query(Conn, ListSql, []) of
                {ok, Rows} ->
                    Size = maps:get(size, Opts, 10),
                    {ok, Rows, #{
                        total => Total,
                        total_page => max(1, (Total + Size - 1) div Size)
                    }};
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end.

%% @doc Seat 只读全量（合同 §3.1；排序 sort ASC, title ASC）。
list_tx(Conn, OrgId, WorkspaceId) ->
    Sql = iolist_to_binary([
        <<"SELECT ">>,
        ?COLS,
        <<" FROM ">>,
        tablename(),
        <<
            " WHERE tenant_id=$1 AND workspace_id=$2 AND deleted_at IS NULL"
            " ORDER BY sort ASC, title ASC"
        >>
    ]),
    case elib_pg:query(Conn, Sql, [OrgId, WorkspaceId]) of
        {ok, Rows} -> {ok, Rows};
        {error, _} = E -> E
    end.

%% @doc 单行可见性探针（未删；谓词零 Org——org/workspace 随行输出，跨范围
%% 的 403/404 由 application 同事务内裁决，见模块头注释）。
find_tx(Conn, Id) when is_integer(Id) ->
    case
        elib_pg:query(
            Conn,
            iolist_to_binary([
                <<"SELECT ">>,
                ?COLS,
                <<" FROM ">>,
                tablename(),
                <<" WHERE id=$1 AND deleted_at IS NULL">>
            ]),
            [Id]
        )
    of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} = E -> E
    end.

%% @doc 坐席 assignment 事实（合同 §3.1：active org member + workspace 归属）。
%% 只回事实布尔对，不做裁决（ok/forbidden 由 application 判定）。
seat_assignment_facts(OrgId, WorkspaceId, UserId) when
    is_integer(OrgId), is_integer(WorkspaceId)
->
    with_tx(fun(C) ->
        Member = elib_pg:query(
            C,
            <<
                "SELECT 1 FROM organization_member WHERE organization_id=$1"
                " AND user_id=$2 AND status='active' LIMIT 1"
            >>,
            [OrgId, UserId]
        ),
        Ws = elib_pg:query(
            C,
            <<"SELECT 1 FROM workspace WHERE id=$1 AND organization_id=$2 LIMIT 1">>,
            [WorkspaceId, OrgId]
        ),
        {hit_any(Member), hit_any(Ws)}
    end).

hit_any({ok, [_ | _]}) -> true;
hit_any(_) -> false.
