-module(group_repo).
-export([workspace_groups/2, member_workspace_groups/4, member_workspace_groups/5]).
-export([mark_workspace_group_read/5]).
%%%
% group_repo 是 group repository 缩写
% 群组数据仓库层，提供群组数据的基础数据库操作
%%%

-export([tablename/0]).
-export([add/2]).
-export([create/1, find_by_gid/1]).
-export([find_by_id/2]).
-export([list_by_ids/2]).
-export([list_by_uid/2, list_by_uid/3]).
-export([page/2, page/4]).
-export([update/1]).
-export([update_by_id_tx/3]).
-export([update_owner_tx/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取群组表的表名
%% @return 返回群组表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group">>).

%% @doc 添加新群组
%% @doc 添加群组（使用连接）
%% @param Conn 数据库连接
%% @param Data 包含群组信息的map
%% @return {ok, GroupId} | {error, Reason} (返回插入的群组ID)
-spec add(any(), map()) -> {ok, integer()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    %% 尊重调用方显式传入的正整数 id/gid；缺省/0 则服务端生成 TSID。
    %% （此前无条件重生成，调用方 gid 被静默丢弃，成员行/后续引用挂在
    %% 真实群 id 上才能成立的场景全部踩坑。）
    %% 无论哪条路径都先剔除 id/<<"id">> 两种 key：normalize_legacy_create_data
    %% 用 atom `id` 承载调用方 id/gid，若再覆盖 binary <<"id">>，两个 key
    %% 共存使 elib_pg_sql:insert/2 拼出重复 "id" 列，PG 报 42701
    %% （真库集成测试实测复现：conversation_pin_delete_integration_tests）。
    RawId = maps:get(id, Data, maps:get(<<"id">>, Data, 0)),
    Id1 =
        try
            ec_cnv:to_integer(RawId)
        catch
            _:_ -> 0
        end,
    Data1 = maps:remove(id, maps:remove(<<"id">>, Data)),
    {Id, Data2} =
        case Id1 > 0 of
            true ->
                {Id1, Data1#{<<"id">> => Id1}};
            false ->
                IdGen = elib_tsid:generate(group_info),
                {IdGen, Data1#{<<"id">> => IdGen}}
        end,
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 兼容旧接口：创建群组
%% @return {ok, GroupId} 创建成功返回真实群 id | {error, Reason}
-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Data0) ->
    Data = normalize_legacy_create_data(Data0),
    elib_pg:with_tx(fun(Conn) -> add(Conn, Data) end).

%% @doc 兼容旧接口：按 gid 查询群组（排除 chat_aes_key 列）
-spec find_by_gid(integer() | binary()) -> {ok, map()} | {error, term()}.
find_by_gid(Gid) ->
    case
        find_by_id(
            Gid,
            <<"id,type,join_limit,content_limit,owner_uid,creator_uid,member_max,member_count,introduction,avatar,title,status,updated_at,created_at">>
        )
    of
        #{} = Row when map_size(Row) > 0 ->
            {ok, Row};
        #{} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 根据群组ID查找群组信息
%% @param Gid 群组ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return Row 查询成功返回行数据（map） | {error, Reason} 查询失败
%% @example group_repo:find_by_id(1, <<"*">>).
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, any()}.
find_by_id(Gid, Column) when is_list(Gid); is_binary(Gid) ->
    find_by_id(ec_cnv:to_integer(Gid), Column);
find_by_id(Gid, Column) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => Gid}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据群组ID列表批量查询群组信息
%% @param Ids 群组ID列表
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Rows} 查询成功返回 map 列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_ids([1,2], <<"*">>).
-spec list_by_ids(list(integer() | binary()), binary()) -> {ok, list(map())} | {error, any()}.
list_by_ids(Ids, Column) when length(Ids) > 0 ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => {in, Ids}}, #{}),
    case elib_pg:query(Sql, Params) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            {error, Reason}
    end;
list_by_ids([], _Column) ->
    {ok, []}.

%% @doc 查询用户创建的群组列表（使用默认限制10000）
%% @param Uid 用户ID（群组所有者）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_uid(1, <<"*">>).
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).

%% @doc 查询用户创建的群组列表（指定限制数量）
%% @param Uid 用户ID（群组所有者）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_uid(1, <<"*">>).
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{owner_uid => Uid, status => 1}, #{
        limit => Limit
    }),
    elib_pg:query(Sql, Params).

%% @doc 分页查询群组
-spec page(integer(), integer()) -> {ok, map()} | {error, any()}.
page(Page, Size) ->
    page(Page, Size, #{}, <<"created_at DESC">>).

%% @doc 分页查询群组（带条件）
-spec page(integer(), integer(), map(), binary()) -> {ok, map()} | {error, any()}.
page(Page, Size, Where, OrderBy) ->
    Tb = tablename(),
    Column =
        %% scope/workspace_id：双体验 v2.5.2（00000077）新列，admin 列表展示归属
        <<
            "id,title,avatar,owner_uid,creator_uid,type,join_limit,member_count,introduction,"
            "status,scope,workspace_id,created_at"
        >>,
    elib_pg:page_with_total(Tb, Column, Where, OrderBy, Page, Size).

%% @doc 更新群组信息
-spec update(map()) -> {ok, non_neg_integer()} | {error, any()}.
update(Data) ->
    Tb = tablename(),
    Id = maps:get(<<"id">>, Data),
    UpdateData = maps:without([<<"id">>], Data),
    elib_pg:update(Tb, UpdateData, <<"id = $1">>, [Id]).

%% @doc 事务内按 ID 更新群组（归档写守卫同事务，DS 层 write_tx 调用）
-spec update_by_id_tx(any(), integer(), map()) -> {ok, non_neg_integer()} | {error, any()}.
update_by_id_tx(Conn, Gid, Data) ->
    Tb = tablename(),
    UpdateData = maps:without([<<"id">>], Data),
    {Sql, Params} = elib_pg_sql:update(Tb, UpdateData, <<"id = $1">>, [Gid]),
    elib_pg:execute(Conn, Sql, Params).

%% @doc 在事务中更新群主
%% 更新指定群组的群主ID
%% @param Conn 数据库连接
%% @param Gid 群组ID
%% @param NewOwnerUid 新群主ID
%% @return ok | {error, Reason}
-spec update_owner_tx(any(), integer(), integer()) -> ok | {error, any()}.
update_owner_tx(Conn, Gid, NewOwnerUid) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql = <<"UPDATE ", Tb/binary, " SET owner_uid = $1, updated_at = $2 WHERE id = $3">>,
    case elib_pg:execute(Conn, Sql, [NewOwnerUid, Now, Gid]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, "群组不存在"};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec normalize_legacy_create_data(map()) -> map().
normalize_legacy_create_data(Data0) ->
    Gid = pick_value(Data0, [id, <<"id">>, gid, <<"gid">>], 0),
    OwnerUid = pick_value(Data0, [owner_uid, <<"owner_uid">>], 0),
    Name = pick_value(Data0, [title, <<"title">>, name, <<"name">>], <<"">>),
    #{
        id => ec_cnv:to_integer(Gid),
        owner_uid => ec_cnv:to_integer(OwnerUid),
        creator_uid => ec_cnv:to_integer(OwnerUid),
        title => ec_cnv:to_binary(Name),
        status => 1,
        created_at => elib_dt:now(),
        updated_at => elib_dt:now()
    }.

-spec pick_value(map(), [atom() | binary()], term()) -> term().
pick_value(_Map, [], Default) ->
    Default;
pick_value(Map, [Key | Rest], Default) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            pick_value(Map, Rest, Default)
    end.

%% 工作区资源目录与本人会话资源共用 DTO 列；后者按当前资格分页。
workspace_groups(WorkspaceId, Limit) ->
    Sql =
        <<"SELECT ", (workspace_group_columns())/binary, " FROM \"group\" g",
            " WHERE g.workspace_id = $1 AND g.scope = 'workspace' AND g.status = 1",
            " ORDER BY g.created_at DESC, g.id DESC LIMIT $2">>,
    elib_pg:query(Sql, [WorkspaceId, Limit]).

member_workspace_groups(WorkspaceId, Uid, AfterId, Limit) ->
    member_workspace_groups(WorkspaceId, Uid, AfterId, Limit, false).

member_workspace_groups(WorkspaceId, Uid, AfterId, Limit, Preview) ->
    Eligible =
        <<"SELECT ", (workspace_group_columns())/binary,
            ",gen.start_seq AS member_start_seq,gen.id AS member_generation_id",
            " FROM \"group\" g JOIN workspace w ON w.id=g.workspace_id",
            " LEFT JOIN organization o ON o.id=w.organization_id",
            " JOIN group_member_generation gen ON gen.group_id=g.id",
            " AND gen.user_id=$2 AND gen.end_seq IS NULL",
            " WHERE g.workspace_id=$1 AND g.scope='workspace' AND g.status=1 AND g.id>$3",
            " AND w.status IN ('active','archived')",
            " AND (w.organization_id IS NULL OR o.status='active')",
            " AND (w.organization_id IS NULL OR EXISTS (SELECT 1 FROM organization_member om",
            " WHERE om.organization_id=w.organization_id AND om.user_id=$2 AND om.status='active'))",
            " AND EXISTS (SELECT 1 FROM workspace_member wm WHERE wm.workspace_id=w.id",
            " AND wm.user_id=$2 AND wm.status='active')",
            " AND EXISTS (SELECT 1 FROM group_member gm WHERE gm.group_id=g.id",
            " AND gm.user_id=$2 AND gm.status=1)", " ORDER BY g.id ASC LIMIT $4">>,
    Sql =
        case Preview of
            false ->
                Eligible;
            true ->
                <<"WITH eligible AS (", Eligible/binary,
                    ") SELECT g.*,h.latest_message,r.read_seq,u.unread_count",
                    " FROM eligible g LEFT JOIN LATERAL (", (workspace_latest_message_sql())/binary,
                    ") h ON true LEFT JOIN workspace_group_read_cursor r",
                    " ON r.generation_id=g.member_generation_id LEFT JOIN LATERAL (",
                    (workspace_unread_sql())/binary, ") u ON true ORDER BY g.id ASC">>
        end,
    elib_pg:query(Sql, [WorkspaceId, Uid, AfterId, Limit]).

%% ACK is delivery, not read: include acknowledged timeline rows. The live table
%% carries edits/revokes/deletion; immutable archive payloads cannot replace it.
workspace_latest_message_sql() ->
    <<"SELECT jsonb_build_object('msg_id',m.msg_id,'conv_seq',tl.conv_seq,",
        "'msg_type',m.msg_type,'payload',m.payload,'e2ee',m.e2ee,",
        "'server_ts',m.server_ts,'expire_at',m.expire_at) AS latest_message",
        " FROM public.msg_c2g_timeline tl JOIN public.msg_c2g m",
        " ON m.msg_id=tl.msg_id AND m.created_at=tl.created_at AND m.to_id=tl.to_gid",
        " WHERE tl.to_uid=$2 AND tl.to_gid=g.id AND tl.conv_seq IS NOT NULL",
        " AND tl.conv_seq>=g.member_start_seq", " AND (m.expire_at IS NULL OR m.expire_at>NOW())",
        " ORDER BY tl.conv_seq DESC,tl.created_at DESC LIMIT 1">>.

workspace_unread_sql() ->
    <<"SELECT count(DISTINCT tl.msg_id)::integer AS unread_count",
        " FROM msg_c2g_timeline tl JOIN msg_c2g m",
        " ON m.msg_id=tl.msg_id AND m.created_at=tl.created_at AND m.to_id=tl.to_gid",
        " WHERE tl.to_uid=$2 AND tl.to_gid=g.id AND tl.conv_seq>=g.member_start_seq",
        " AND tl.conv_seq>COALESCE(r.read_seq,0) AND m.from_id<>$2",
        " AND (m.expire_at IS NULL OR m.expire_at>NOW())",
        " AND m.payload->>'action' IS DISTINCT FROM 'message_revoke_ack'">>.

%% Lock the exact active authorization/generation before writing a read fact.
%% Only actually delivered, live message IDs may advance it; never a guessed seq.
mark_workspace_group_read(Conn, WorkspaceId, Uid, GroupId, MsgIds) ->
    Sql =
        <<"WITH eligible AS MATERIALIZED (SELECT gen.id,gen.start_seq",
            " FROM \"group\" g JOIN workspace w ON w.id=g.workspace_id",
            " JOIN workspace_member wm ON wm.workspace_id=w.id AND wm.user_id=$2",
            " JOIN group_member gm ON gm.group_id=g.id AND gm.user_id=$2",
            " JOIN group_member_generation gen ON gen.group_id=g.id AND gen.user_id=$2",
            " JOIN \"user\" usr ON usr.id=$2",
            " WHERE w.id=$1 AND g.id=$3 AND g.scope='workspace' AND g.status=1",
            " AND w.status IN ('active','archived') AND wm.status='active'",
            " AND gm.status=1 AND gen.end_seq IS NULL AND usr.status=1",
            " AND (w.organization_id IS NULL OR EXISTS (SELECT 1 FROM organization o",
            " WHERE o.id=w.organization_id AND o.status='active' FOR SHARE))",
            " AND (w.organization_id IS NULL OR EXISTS (SELECT 1 FROM organization_member om",
            " WHERE om.organization_id=w.organization_id AND om.user_id=$2 AND om.status='active' FOR SHARE))",
            " FOR SHARE OF g,w,wm,gm,gen,usr), observed AS (",
            " SELECT e.id,max(tl.conv_seq) AS seq FROM eligible e",
            " JOIN msg_c2g_timeline tl ON tl.to_uid=$2 AND tl.to_gid=$3",
            " AND tl.conv_seq>=e.start_seq JOIN msg_c2g m",
            " ON m.msg_id=tl.msg_id AND m.created_at=tl.created_at AND m.to_id=tl.to_gid",
            " WHERE tl.msg_id=ANY($4::text[]) AND (m.expire_at IS NULL OR m.expire_at>NOW())",
            " AND m.payload->>'action' IS DISTINCT FROM 'message_revoke_ack'",
            " GROUP BY e.id HAVING count(DISTINCT tl.msg_id)=cardinality($4::text[]))",
            " INSERT INTO workspace_group_read_cursor (generation_id,read_seq)",
            " SELECT id,seq FROM observed ON CONFLICT (generation_id) DO UPDATE",
            " SET read_seq=GREATEST(workspace_group_read_cursor.read_seq,EXCLUDED.read_seq),",
            " updated_at=NOW() RETURNING read_seq">>,
    elib_pg:query(Conn, Sql, [WorkspaceId, Uid, GroupId, MsgIds]).

workspace_group_columns() ->
    <<"g.id,g.type,g.join_limit,g.content_limit,g.owner_uid,g.creator_uid,",
        "g.member_max,g.member_count,g.introduction,g.avatar,g.title,g.status,g.scope,g.workspace_id,",
        "g.updated_at,g.created_at">>.
