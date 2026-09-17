%%% @doc Organization Default Workspace 纯域逻辑（Core Contract C05 / 计划 ORG-05）。
%%%
%%% 职责：默认关系的合法性裁决（纯函数）、archive 交接（replace-or-clear）
%%% 决策、稳定错误码。本模块零 SQL、零 elib_pg、零应用启动依赖——可在
%%% 无 DB 的 EUnit 下独立验证。
%%%
%%% 铁律（C05 冻结语义，任何调用方不得绕过）：
%%%   * 默认关系是**指路事实**（每 Org 至多 1 条指向同 Org active Workspace），
%%%     **不是权限事实**：它不授予任何访问权；访问仍由 workspace_member 裁决
%%%     （C05：Org Role ≠ Workspace Role，Org owner 不自动成为 Workspace owner）；
%%%   * 每 Org 0 或 1 个默认；默认为空合法（Organization detail 可空）；
%%%   * set 的目标必须同 Org 且 active（组合 FK + DB 触发器是权威，
%%%     本模块提供快速失败预检）；
%%%   * set 同值幂等（unchanged）、clear 空值幂等（already_empty）；
%%%   * 变更必须先锁 organization 行（与 owner transfer 同锁序：组织行先）；
%%%   * 首个 Org Workspace 创建时同事务设默认；archive 交接策略见
%%%     `archive_decision/3`（replace-or-clear，与 legacy min-ID 读法过渡期等值）。
-module(organization_default_workspace).

-export([
    valid_id/1,
    ensure_settable_target/3,
    archive_decision/2,
    next_default_policy/0
]).

%% ===================================================================
%% 标识
%% ===================================================================

%% @doc Org/Workspace 标识：正整数（TSID）。
-spec valid_id(term()) -> ok | {error, {invalid_id, term()}}.
valid_id(Id) when is_integer(Id), Id > 0 ->
    ok;
valid_id(Id) ->
    {error, {invalid_id, Id}}.

%% @doc set 目标快速失败预检（DB 触发器是权威，此处仅为提前 4xx）：
%% 目标行必须同 Org（`WsOrg` =:= `ExpectedOrg`）且 status=active。
%% 返回 ok 或稳定错误码：not_found（404）/ not_active（409）/ cross_org（409）。
-spec ensure_settable_target(integer(), integer() | null, binary()) ->
    ok | {error, not_found | not_active | cross_org}.
ensure_settable_target(_ExpectedOrg, null, _Status) ->
    {error, not_found};
ensure_settable_target(ExpectedOrg, WsOrg, _Status) when WsOrg =/= ExpectedOrg ->
    {error, cross_org};
ensure_settable_target(_ExpectedOrg, _WsOrg, <<"active">>) ->
    ok;
ensure_settable_target(_ExpectedOrg, _WsOrg, _Status) ->
    {error, not_active}.

%% ===================================================================
%% archive 交接决策（replace-or-clear）
%% ===================================================================

%% @doc archive 时的默认交接策略标识（决策记录用，见 ORG-05 evidence）。
%% 选定 **replace_with_min_active** 的理由：legacy min-ID 读法在被替换前
%% 的过渡期内「归档默认后自然落到剩余最小 active id」——显式关系采用同一
%% 策略使两读法逐 Org 等值，装配层一次性切换零行为漂移，rollback 故事最强。
%% 无剩余 active Workspace 时 clear（legacy 读法此时同样返回
%% no_default_workspace，等值）。
-spec next_default_policy() -> replace_with_min_active.
next_default_policy() ->
    replace_with_min_active.

%% @doc 归档默认 Workspace 时的交接决策（纯函数）。
%% 输入：被归档的 workspace 是否当前默认（`IsCurrentDefault`）、
%% 同 Org 剩余 active workspace id 升序列表（不含被归档者）。
%% 输出：`clear` | `{replace, MinActiveId}`。
%% 剩余列表已按 id 升序给定时，头元素即 min（与 legacy 读法等值）。
%% ⚠️ 当前唯一生产调用方（organization_default_workspace_pg）恒传 true——
%% 其仅在「被归档者 = 当前默认」时才进入本决策；`false -> clear` 分支是
%% 防御臂，防止未来调用方误用造成误清空。
-spec archive_decision(boolean(), [integer()]) -> clear | {replace, integer()}.
archive_decision(false, _RemainingActiveIds) ->
    clear;
archive_decision(true, []) ->
    clear;
archive_decision(true, [MinId | _]) when is_integer(MinId), MinId > 0 ->
    {replace, MinId}.
