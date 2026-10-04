%%% @doc 扩展点：**最小只读事实**（成员状态 / 默认 Workspace 解析）。
%%%
%%% 依据：用户裁决 §三（原文）——
%%%   「成员状态用**最小只读事实 Port**，**不把 member 事实误当授权结论**；
%%%     **不要为了读事实伪造一个无职责的 eb_member_app**。」
%%% 与 plan v4.1 §2.1 / EB-05-A01 / EB-06-A02（会话创建需要默认 Workspace）。
%%%
%%% 为什么单独开这个扩展点（R0 的 B2 + 分类一）：
%%%   * 成员状态与默认 Workspace 是**事实读取**，不是授权判定——授权判定归
%%%     EB-04 的 `eb_auth_port`（逐请求加载授权事实，`load_request_facts/1`）；
%%%   * 此前两者只能靠调用方注入 `fun/2`，即「无权威事实源」；而
%%%     `eb_ports:facade_targets/0` 里登记的 `eb_member_app` 在计划里**没有任何
%%%     卡租约**（真无主），且 `suspend_member/2` 属 EB-08 的用例职责。
%%%     因此本卡只交付**只读事实**，不建 `eb_member_app`（A10 机械判定这一点）。
%%%
%%% 本扩展点是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% **零写能力（A10 的机械判据）**：本模块不声明任何
%%% `insert/update/delete/advance/append/purge` 形状的 callback。事实读取路径在
%%% 契约层就不可能产生副作用——「读事实」永远不会顺手改状态。
%%%
%%% **不是授权结论**：`member_status/2` 只回答「组织成员关系当前是什么状态」，
%%% 不回答「该请求是否有权做某事」。授权仍必须走 `eb_auth_port` 逐请求加载 +
%%% `eb_auth_app` 判定；把本扩展点的返回值当授权结论使用属于越权。
-module(eb_member_fact_port).

-moduledoc "扩展点：最小只读事实 —— 成员状态与默认 Workspace 解析。".
-export_type([member_status/0, user_id/0, workspace_id/0]).

%% 与 `organization_member.ck_organization_member_status` 逐字对齐（active | suspended | removed）。
-type member_status() :: active | suspended | removed.
-type user_id() :: integer().
-type workspace_id() :: integer().

%% @doc 读取成员在**该 Org** 内的关系状态。
%%
%% 逐请求读取、不缓存（与 `eb_auth_port:load_request_facts/1` 同规：授权相关事实
%% 的陈旧缓存会让 suspend / removal 失效）。无成员关系或解析失败必须
%% `{error, ...}`（fail-closed），**不得**返回默认 `active`。
-callback member_status(OrgId :: integer(), UserId :: user_id()) ->
    {ok, member_status()} | {error, no_member | term()}.

%% @doc 解析该成员在本 Org 的默认 Workspace（V1 只读解析，不接受调用方任意指定）。
%%
%% 返回的 `workspace_id` 必须同时满足「属于该 Org」与「该成员可访问」；不唯一或
%% 无法解析时 `{error, ...}`（fail-closed），调用方不得自行兜底挑一个 Workspace。
-callback default_workspace(OrgId :: integer(), UserId :: user_id()) ->
    {ok, workspace_id()} | {error, no_default_workspace | term()}.
