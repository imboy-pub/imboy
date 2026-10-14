%%% @doc PoC（IMBOY_HIRD_AUTH_REPLAY_POC_V1）native registry + 模拟 Provider
%%% + 业务执行观测——**非生产模块**。
%%%
%%% 三个角色（同模块减少 PoC 文件面）：
%%%
%%% 1. **native registry**（`agent_native_tool_adapter` 的 env seam 合同
%%%    `lookup/1`）：返回业务 handler。业务 handler 带 **effect 凭证防护**：
%%%    只接受同进程内 `set_effect/1` 签发的 {EffectId, RunId}——绕过授权
%%%    链直调 registry handler（无凭证）时拒绝执行、不读业务数据。
%%%
%%% 2. **业务执行观测**：真实执行边界（workspace_repo:internal_find_tx 调用
%%%    处）以 `atomics` 计数——跨进程可见（覆盖 dispatch worker / actor /
%%%    测试进程任意派生路径），非调用者进程字典。
%%%
%%% 3. **模拟 Provider**（`model_call/1`）：本地脚本化模型响应（查表），
%%%    **不调真实模型**；`provider_calls` 计数同样挂 atomics。P2 replay
%%%    期间以 `trap` 模式安装——任何调用即 error（"调用即失败"断言）。
%%%
%%% 上下文（conn/计数器/trap 开关）经 persistent_term 安装（`install/1` /
%%% `teardown/0`）：PoC 专用测试 VM 内串行使用，作用域由测试套件管理
%%% （P2 作用域纪律：节点全局状态 + 独立 VM 隔离）。
-module(imboy_hird_poc_registry).

-export([
    install/1,
    teardown/0,
    %% native registry 合同（agent_native_tool_adapter env seam）
    lookup/1,
    %% 桥/授权链调用面
    set_effect/1,
    clear_effect/0,
    run_ws_detail/1,
    %% 模拟 provider（hird 注册表 handler 直接绑定）
    model_handler/2,
    %% 观测读取
    biz_calls/0,
    provider_calls/0
]).

-define(CTX_KEY, {imboy_hird_poc_registry, ctx}).
-define(EFFECT_PD, imboy_hird_poc_effect).

-type ctx() :: #{
    conn := pid(),
    biz_counter := atomics:atomics_ref(),
    provider_counter := atomics:atomics_ref(),
    trap := boolean()
}.

%% @doc 安装 PoC 上下文（幂等覆盖；测试 setup 调用）。
install(Ctx) ->
    persistent_term:put(?CTX_KEY, Ctx),
    ok.

teardown() ->
    persistent_term:erase(?CTX_KEY),
    ok.

ctx() ->
    persistent_term:get(?CTX_KEY, undefined).

%% ===================================================================
%% native registry 合同（agent_native_tool_adapter）
%% ===================================================================

%% 仅注册唯一业务工具；未注册工具 → not_found（默认关闭语义）。
lookup(<<"tool.workspace.detail">>) ->
    {ok, fun(Resource) -> run_ws_detail(Resource) end};
lookup(_OtherToolId) ->
    {error, not_found}.

%% ===================================================================
%% effect 凭证 + 业务执行（真实只读边界）
%% ===================================================================

%% 桥在 authorize allow 后、调用业务 handler 前签发（同进程）。
%% 凭证绑定三元组（C3 CLOSEOUT）：授权决策（EffectId）+ 可信 Run（RunId）
%% + **目标资源**（organization_id + workspace_id）——凭证有效窗口内请求
%% 不相干资源同样拒绝，不能凭「存在一个 authorized effect」放行。
set_effect({EffectId, RunId, #{organization_id := OrgId, workspace_id := WsId}}) when
    is_integer(EffectId), is_integer(RunId)
->
    put(?EFFECT_PD, {EffectId, RunId, OrgId, WsId}),
    ok.

clear_effect() ->
    erase(?EFFECT_PD),
    ok.

%% 业务执行体：effect 凭证校验（存在 + 资源绑定一致）→ trap 断言 → 真实
%% 只读读取 → 计数。凭证缺失（绕过授权链直调）→ {error, no_effect_credential}；
%% 凭证与请求资源不匹配（窗口内跨资源）→ {error, effect_resource_mismatch}。
%% 两者都不发生业务读取与计数（fail closed）。
%% 输入合同：native adapter 的 ResourceContext（workspace_id/organization_id）。
run_ws_detail(#{workspace_id := WsId, organization_id := OrgId} = _Resource) ->
    case get(?EFFECT_PD) of
        {EffectId, RunId, CredOrg, CredWs} when
            EffectId > 0,
            RunId > 0,
            CredOrg =:= OrgId,
            CredWs =:= WsId
        ->
            case ctx() of
                #{conn := Conn, biz_counter := Counter, trap := Trap} ->
                    case Trap of
                        true ->
                            erlang:error(replay_must_not_execute_biz);
                        false ->
                            case workspace_repo:internal_find_tx(Conn, OrgId, WsId) of
                                {ok, Row} ->
                                    _ = atomics:add_get(Counter, 1, 1),
                                    {ok, encode_ws_row(Row)};
                                {error, Reason} ->
                                    {error, Reason}
                            end
                    end;
                undefined ->
                    {error, registry_not_installed}
            end;
        {_, _, _, _} ->
            {error, effect_resource_mismatch};
        undefined ->
            {error, no_effect_credential}
    end;
run_ws_detail(_BadResource) ->
    {error, bad_resource}.

encode_ws_row(Row) when is_map(Row) ->
    jsone:encode(#{
        <<"id">> => maps:get(<<"id">>, Row),
        <<"name">> => maps:get(<<"name">>, Row),
        <<"owner_id">> => maps:get(<<"owner_id">>, Row)
    }).

%% ===================================================================
%% 模拟 Provider（本地脚本化响应；不调真实模型）
%% ===================================================================

%% hird 注册表 handler 形状 fun(Args, Handlers)。
model_handler(#{prompt := Prompt}, _Handlers) ->
    case ctx() of
        #{provider_counter := Counter, trap := Trap} ->
            case Trap of
                true ->
                    erlang:error(replay_must_not_call_provider);
                false ->
                    _ = atomics:add_get(Counter, 1, 1),
                    scripted_reply(Prompt)
            end;
        undefined ->
            erlang:error(registry_not_installed)
    end;
model_handler(_Args, _Handlers) ->
    erlang:error(bad_model_args).

scripted_reply(Prompt) when is_binary(Prompt) ->
    <<"scripted-model-reply:", Prompt/binary>>;
scripted_reply(Prompt) ->
    iolist_to_binary(["scripted-model-reply:", io_lib:format("~0p", [Prompt])]).

%% ===================================================================
%% 观测读取
%% ===================================================================

biz_calls() ->
    read_counter(biz_counter).

provider_calls() ->
    read_counter(provider_counter).

read_counter(Key) ->
    case ctx() of
        #{Key := Counter} -> atomics:get(Counter, 1);
        undefined -> 0
    end.
