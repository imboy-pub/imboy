-module(talk_record_export_policy).
-moduledoc "面聊导出策略检查点（T19）：实例/企业两级开关——单一真源。".
%%%
% 导出策略检查点（§8 导出行：个人=owner 可导出受实例策略；企业=owner
% 与 grantee 都还必须通过企业 export capability，owner 不能绕过；
% §9 分享页导出开关在策略禁止时 disabled 并展示原因）。
%
% config：{imboy, talk_record_export_policy, #{
%   enabled => boolean()        %% 实例级总开关（缺省 true）
%   org_overrides => #{OrgId => boolean()}  %% 企业级覆写（仅 organization
%%                                   记录生效；键为整数 OrgId）}}
%
% **未配置默认允许**：V1 无企业策略真源（capabilities/creation-contexts
% 策略面尚未接入导出维度，§8"企业 export capability"真源待联调定稿——
% T21 报告缺口 2），未配置时放行是"未配置=不限制"的中立语义；部署关闭
% 即服务端拒绝（§11.4「关闭能力不能仅 UI 禁用」——config false 时
% capabilities_for_actor.export 与 POST /exports 两处同步拒）。
%
% 本模块无 DB 依赖（纯配置读取），供 logic（POST /exports 门禁）与
% ds（capabilities_for_actor 投影）共同引用——策略判定单一真源，两处
% 不漂移。
%%%

-export([allowed/2]).

%% @doc 策略检查点：scope ∈ personal|organization；OrgId 为记录归属
%% （personal 传 null/0——仅实例级开关生效）。
-spec allowed(binary(), integer() | null) -> boolean().
allowed(Scope, OrgId) ->
    Policy = config_ds:env(imboy, talk_record_export_policy, #{}),
    InstanceOk = maps:get(enabled, Policy, true) =:= true,
    OrgOk =
        case {Scope, maps:get(org_overrides, Policy, undefined)} of
            {<<"organization">>, Overrides} when is_map(Overrides), is_integer(OrgId) ->
                case maps:find(OrgId, Overrides) of
                    {ok, false} -> false;
                    _ -> true
                end;
            _ ->
                true
        end,
    InstanceOk andalso OrgOk.
