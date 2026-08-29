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
%
% W2 过渡：find_project_member_tx/4、find_project_member/3 是 project_member
% 的只读查询（权限校验用，暂寄本模块）——project_member_repo 由 ZC-02 并行
% 产出，ZC-05 整合时迁往该模块并统一到 project_member_logic。
%%%

-export([tablename/0]).
-export([add_tx/2]).
-export([find_by_id/2]).
-export([find_tx/3]).
-export([list_by_project/4]).
-export([count_by_project/2]).
-export([update_fields_tx/3]).
-export([mark_reached_tx/3]).
-export([find_project_member_tx/4]).
-export([find_project_member/3]).

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

%% @doc 按 ID 查询里程碑（自动提交；空 map = 无记录）
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, term()}.
find_by_id(MilestoneId, Column) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => MilestoneId}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内查询里程碑行
-spec find_tx(any(), integer(), binary()) -> map().
find_tx(Conn, MilestoneId, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    case elib_pg:query(Conn, Sql, [MilestoneId]) of
        {ok, [Row | _]} -> Row;
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
    elib_pg:query(Sql, Params).

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

%% @doc 自动提交查询 project_member 行（ZC-05 收敛：转发 project_member_repo）
-spec find_project_member(integer(), integer(), binary()) -> map().
find_project_member(ProjectId, Uid, Column) ->
    project_member_repo:find(ProjectId, Uid, Column).
