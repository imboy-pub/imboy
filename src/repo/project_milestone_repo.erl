-module(project_milestone_repo).
%%%
% project_milestone_repo 是 project_milestone repository 缩写
% 项目里程碑数据仓库层（迁移 00000081，Channel-first-class W2 / ZC-03）
%
% 表结构：project_milestone(id TSID, workspace_id 冗余, project_id 复合FK,
%   name varchar(200), due_date date 可空, status planned|reached,
%   reached_at timestamptz 可空, timestamps)
% DB 约束：chk_project_milestone_status（值域）、
%   chk_project_milestone_reached（status='reached' ⟺ reached_at IS NOT NULL）。
% 单向状态机 planned→reached 与重复 reach 幂等由应用层执行（DS 层）。
%
% due_date 参数约定：只接受 {Y,M,D} tuple 或 null（原子）——本连接注册了
% 自定义 timestamptz codec（epgsql_codec_rfc3339_bin），date 列走 epgsql
% 原生 codec，binary 参数会崩（ZC-01 已踩坑）；tuple 由 epgsql 原生 date
% codec 编码，INSERT/UPDATE 参数类型由服务端按目标列推断，无需显式 ::date。
% 读路径口径：find_by_id/list_by_project 将 due_date 归一为 ISO
% YYYY-MM-DD binary（normalize_row/1）——tuple 直出会被响应层格式化成
% "{2026,9,30}" 串（ZC-08 缺陷立项修复）；find_tx 不归一（内部窄列）。
%
% 事务写权限通过 find_project_member_tx/4 转发 project_member_repo，确保
% 与里程碑写入使用同一连接。
%%%

-export([tablename/0]).
-export([add_tx/2]).
-export([find_by_id/2]).
-export([find_tx/3]).
-export([find_by_id_tx/2]).
-export([full_columns/0]).
-export([list_by_project/4]).
-export([count_by_project/2]).
-export([update_fields_tx/3]).
-export([mark_reached_tx/3]).
-export([find_project_member_tx/4]).
-export([due_date_to_iso/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"project_milestone">>).

%% @doc 事务内插入里程碑（TSID 用无参 default 生成器——project_milestone 命名
%% 生成器未注册进 imboy_app:tsid_generator_names()，注册属 ZC-05 patch 清单项）
-spec add_tx(any(), map()) -> {ok, integer()} | {error, term()}.
add_tx(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 按 ID 查询里程碑（自动提交；空 map = 无记录；due_date 读路径
%% 归一为 ISO YYYY-MM-DD binary，见 normalize_row/1）
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, term()}.
find_by_id(MilestoneId, Column) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => MilestoneId}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> normalize_row(Row);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内查询里程碑行
-spec find_tx(any(), integer(), binary()) -> map().
find_tx(Conn, MilestoneId, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    case elib_pg:query(Conn, Sql, [MilestoneId]) of
        {ok, [Row | _]} ->
            Row;
        {error, Reason} ->
            _ = ?ERROR_LOG([project_milestone_find_tx_failed, MilestoneId, Reason]),
            #{};
        _ ->
            #{}
    end.

%% @doc 客户端可见全列（find_by_id 与事务内回读共用的单一来源）
-spec full_columns() -> binary().
full_columns() ->
    <<"id,workspace_id,project_id,name,due_date,status,reached_at,created_at,updated_at">>.

%% @doc 事务内按全列回读并归一（M-5：create/update/reach 改为提交前取数——
%% 提交后 find_by_id 回读遇连接池抖动会把已成功的事务报成失败，
%% create 非幂等，客户端重试将产生重复里程碑）
-spec find_by_id_tx(any(), integer() | binary()) -> map().
find_by_id_tx(Conn, MilestoneId) ->
    Tb = tablename(),
    Sql = <<"SELECT ", (full_columns())/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    case elib_pg:query(Conn, Sql, [MilestoneId]) of
        {ok, [Row | _]} -> normalize_row(Row);
        _ -> #{}
    end.

%% @doc 项目里程碑列表（status 可选过滤 all|planned|reached；稳定排序 id ASC，
%% 走 i_project_milestone_project_status；分页由调用方约束 Page/Size 为正整数）
-spec list_by_project(integer(), binary() | all, integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_by_project(ProjectId, Status, Page, Size) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    {Where, Params} =
        case Status of
            all ->
                {<<" WHERE project_id = $1">>, [ProjectId]};
            _ ->
                {<<" WHERE project_id = $1 AND status = $2">>, [ProjectId, Status]}
        end,
    Sql =
        <<"SELECT id,workspace_id,project_id,name,due_date,status,reached_at,",
            "created_at,updated_at FROM ", Tb/binary, Where/binary, " ORDER BY id ASC LIMIT ",
            (integer_to_binary(Size))/binary, " OFFSET ", (integer_to_binary(Offset))/binary>>,
    case elib_pg:query(Sql, Params) of
        {ok, Rows} -> {ok, [normalize_row(R) || R <- Rows]};
        {error, _} = Err -> Err
    end.

%% @doc 项目里程碑计数（admin_page 的独立 total 数据源，与数据页同 WHERE
%% 语义；status all|planned|reached。M-7：分页 total 不再用当前页行数近似）
-spec count_by_project(integer(), binary() | all) ->
    {ok, non_neg_integer()} | {error, term()}.
count_by_project(ProjectId, Status) ->
    Tb = tablename(),
    {Where, Params} =
        case Status of
            all ->
                {<<" WHERE project_id = $1">>, [ProjectId]};
            _ ->
                {<<" WHERE project_id = $1 AND status = $2">>, [ProjectId, Status]}
        end,
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, Where/binary>>,
    case elib_pg:one(Sql, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内更新里程碑字段（name/due_date/status/reached_at 白名单由
%% DS 层构造；归档守卫与权限校验由 DS 层同事务前置）
-spec update_fields_tx(any(), integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update_fields_tx(Conn, MilestoneId, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:update(Tb, Data, <<"id = $1">>, [MilestoneId]),
    elib_pg:execute(Conn, Sql, Params).

%% @doc 事务内达成里程碑（并发守卫更新：WHERE 带 status='planned' 谓词，
%% M-2——READ COMMITTED 下两连接并发 reach 时，后提交者的 UPDATE 重评估
%% 谓词命中 0 行，杜绝 reached_at 覆盖与重复事件）
-spec mark_reached_tx(any(), integer(), map()) ->
    {ok, non_neg_integer()} | {error, term()}.
mark_reached_tx(Conn, MilestoneId, Data) ->
    Tb = tablename(),
    {Sql, Params} =
        elib_pg_sql:update(Tb, Data, <<"id = $1 AND status = 'planned'">>, [MilestoneId]),
    elib_pg:execute(Conn, Sql, Params).

%% @doc 事务内查询 project_member 行（ZC-05 收敛：转发 project_member_repo
%% 单点实现；函数名保留以稳定既有调用点与 mock）
-spec find_project_member_tx(any(), integer(), integer(), binary()) -> map().
find_project_member_tx(Conn, ProjectId, Uid, Column) ->
    project_member_repo:find_tx(Conn, ProjectId, Uid, Column).

%% @doc 读路径归一：date 列 due_date 经 epgsql 原生 codec 回读为 {Y,M,D}
%% tuple，直出会被响应层格式化成 "{2026,9,30}" 串（ZC-08 缺陷立项），
%% 统一转 ISO YYYY-MM-DD binary（API/前端契约 due_date(YYYY-MM-DD|null)；
%% 格式与 project_milestone_ds update 事件 payload 的 due_date_to_binary
%% 保持一致，两侧均有测试钉住）。null/缺列原样透传。find_tx 不归一——
%% 其调用方只取 id/project_id/workspace_id/status 等内部窄列。
-spec normalize_row(map()) -> map().
normalize_row(Row) ->
    case maps:get(<<"due_date">>, Row, undefined) of
        {Y, M, D} when is_integer(Y), is_integer(M), is_integer(D) ->
            Row#{<<"due_date">> => due_date_to_iso({Y, M, D})};
        _ ->
            Row
    end.

%% @doc {Y,M,D} → ISO 8601 日期 binary（如 <<"2026-09-30">>）
-spec due_date_to_iso({integer(), integer(), integer()}) -> binary().
due_date_to_iso({Y, M, D}) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).
