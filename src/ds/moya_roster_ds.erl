-module(moya_roster_ds).
%%%
% 墨芽教师侧只读班级学员名单数据服务层（MN-ROSTER-01，P0-2）
% Teaching roster data service：名单读取编排（Logic 不直接触达 Repo）。
%
% 设计：
%   - list/2 生产入口（池连接）；list_tx/3 事务/连接内版本供真库集成
%     测试直调（与生产同 SQL 代码路径）；
%   - 只读无事务编排；行契约与隐私边界见 moya_roster_repo 模块头
%     （SQL 层不取监护人 UID/openid/联系方式/出生年份/关系详情）。
%%%

-export([list/2, list_tx/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 本班 active learner 名单行（生产入口）
%% 返回 {ok, Rows}，Row = #{learner_id, display_name, submit_guardians}
-spec list(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list(GroupId, OrgId) ->
    moya_roster_repo:class_learners(GroupId, OrgId).

%% @doc 事务/连接内版本（集成测试直连 scratch 用）
-spec list_tx(any(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_tx(Conn, GroupId, OrgId) ->
    moya_roster_repo:class_learners_tx(Conn, GroupId, OrgId).
