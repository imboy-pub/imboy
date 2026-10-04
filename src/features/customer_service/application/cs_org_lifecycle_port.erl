%%% @doc 扩展点：Organization 生命周期事实（ORG-08 / C16 / C08 adapter-only）。
%%%
%%% 真扩展点：只有 `-callback` 声明，零实现。CS 对 Organization 域只消费
%%% **只读生命周期事实**（active | archived），不写 organization 行、不自查
%%% org 表——archived Org 的「新 Session / 新 claim 稳定拒绝」由 application
%%% 层 `cs_org_lifecycle_gate` 经本端口裁决（compatibility doc §4.1）。
%%%
%%% 冻结语义（零重解释）：
%%%   * 状态枚举与 `organization.status` 逐字一致：`active | archived`
%%%     （C16 SOURCE OF TRUTH；00000095 CHECK）；
%%%   * `status/1` 是逐请求只读事实，无缓存（与 cs_auth 的逐请求 facts 同规）；
%%%   * 归档只禁**新写**（open_session/claim），授权只读与既有 Session 历史
%%%     不受影响（C16 INVARIANTS / compatibility matrix「历史不删除」）。
-module(cs_org_lifecycle_port).

-moduledoc "扩展点：Organization 生命周期事实（ORG-08 / C16 / C08，adapter-only）。".
-callback status(OrgId :: integer()) ->
    {ok, active | archived} | {error, not_found | term()}.
