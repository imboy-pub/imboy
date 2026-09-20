-module(moya_roster_repo).
%%%
% 墨芽教师侧只读班级学员名单仓库层（MN-ROSTER-01，P0-2）
% Teaching roster repository：本班 active enrollment learner 最小名单查询。
%
% 设计（P0-2）：
%   - 只返回 active enrollment 且 learner 机构 == 班机构 的学员
%     （removed enrollment / 跨班（无行）/ 跨机构一律不出现在名单）；
%   - submit_guardians = active 且 can_submit=true 的 guardian_learner 计数
%     （P0-3 同款语义：恰一 → assignment_ready，0/多 → setup-required）；
%   - SELECT 列仅 learner_id/display_name/submit_guardians——
%     监护人 UID、openid、联系方式、出生年份、关系详情在 SQL 层就不取
%     （P0-2：永不返回）；
%   - SQL 全参数化；行 map 键为 binary（epgsql column.name）。
%%%

-export([class_learners/2, class_learners_tx/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% ------------------------------------------------------------------
%% MN-ROSTER-01：本班 active learner 最小名单（含 can_submit 监护人计数）
%% GroupId 班级；OrgId 班机构（logic 已解析，跨机构 learner 过滤）
%% 行契约：#{<<"learner_id">> => integer(),
%%          <<"display_name">> => binary(),
%%          <<"submit_guardians">> => integer()}
%% ------------------------------------------------------------------

-spec class_learners(integer(), integer()) -> {ok, [map()]} | {error, term()}.
class_learners(GroupId, OrgId) ->
    class_learners_run(pool_exec(), GroupId, OrgId).

%% 事务/连接内版本（真库集成测试直连 scratch 用，与生产同 SQL 代码路径）
-spec class_learners_tx(any(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
class_learners_tx(Conn, GroupId, OrgId) ->
    class_learners_run(conn_exec(Conn), GroupId, OrgId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-type exec_fun() :: fun((binary(), [term()]) -> {ok, term()} | {error, term()}).

-spec pool_exec() -> exec_fun().
pool_exec() ->
    fun(Sql, Params) -> elib_pg:query(Sql, Params) end.

-spec conn_exec(any()) -> exec_fun().
conn_exec(Conn) ->
    fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end.

-spec class_learners_run(exec_fun(), integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
class_learners_run(Exec, GroupId, OrgId) when
    is_integer(GroupId), GroupId > 0, is_integer(OrgId), OrgId > 0
->
    Sql = <<
        "SELECT ce.learner_id, "
        "l.display_name, "
        "(SELECT count(*) FROM guardian_learner gl "
        " WHERE gl.learner_id = ce.learner_id AND gl.status = 'active' "
        "   AND gl.can_submit = true) AS submit_guardians "
        "FROM class_enrollment ce "
        "JOIN learner l ON l.id = ce.learner_id "
        "WHERE ce.group_id = $1 AND ce.status = 'active' "
        "AND l.organization_id = $2 "
        "ORDER BY ce.learner_id"
    >>,
    case Exec(Sql, [GroupId, OrgId]) of
        {ok, Rows} when is_list(Rows) ->
            {ok, [normalize_row(R) || R <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end;
class_learners_run(_Exec, _GroupId, _OrgId) ->
    {error, invalid_param}.

%% epgsql text 模式下 count 可能以 binary 返回（binary 模式为 integer）
-spec normalize_row(map()) -> map().
normalize_row(#{<<"submit_guardians">> := Count} = Row) when is_binary(Count) ->
    Row#{<<"submit_guardians">> => binary_to_integer(Count)};
normalize_row(Row) ->
    Row.
