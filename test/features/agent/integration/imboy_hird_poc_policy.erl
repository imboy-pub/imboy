%%% @doc PoC（IMBOY_HIRD_AUTH_REPLAY_POC_V1）专用资源策略——**非生产模块**。
%%%
%%% 真实静态实现（非 meck）：仅当 (capability, action, resource_type) 三元组
%%% 为 P1 唯一白名单工具 `workspace.detail/read/workspace` 且 Resource 归属
%%% org 与 RunCtx org 一致时 allow；其余（未注册工具、跨 org 资源）一律
%%% deny。经 app env `agent_resource_policy_module` 显式启用；生产默认仍是
%%% `agent_resource_policy`（R2 冻结：未配置策略恒 deny）。
%%%
%%% 注意：跨 org 拒绝在授权链步骤 1 已由 resource.organization_id ≠
%%% run.organization_id 给出（cross_org）；本策略是该语义的第二道显式
%%% 校验（org 一致性从 RunCtx 与 Resource 双侧核对），未覆盖资源仍拒绝。
-module(imboy_hird_poc_policy).

-export([evaluate/3]).

evaluate(RunCtx, Tool, Resource) when is_map(RunCtx), is_map(Tool), is_map(Resource) ->
    Triple = {
        maps:get(capability, Tool), maps:get(action, Tool), maps:get(resource_type, Resource)
    },
    case Triple of
        {<<"workspace.detail">>, <<"read">>, <<"workspace">>} ->
            SameOrg =
                maps:get(organization_id, Resource, undefined) =:=
                    maps:get(organization_id, RunCtx, undefined),
            case SameOrg of
                true -> allow;
                false -> {deny, poc_policy_cross_org}
            end;
        _ ->
            {deny, poc_policy_unknown_resource}
    end;
evaluate(_RunCtx, _Tool, _Resource) ->
    {deny, poc_policy_bad_input}.
