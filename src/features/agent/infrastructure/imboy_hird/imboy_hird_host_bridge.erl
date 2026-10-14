%%% @doc Hirð 生成程序 → IMBoy 授权链的宿主注册表桥（P1，IMBOY_HIRD_AUTH_REPLAY_POC_V1）。
%%%
%%% 背景：Hirð codegen 对 actor 回调体内的 tool 调用生成
%%% `hird_tool_dispatch:call(Tool, Caller, #{}, Args)`（无词法 handler map），
%%% dispatch resolve 落 `hird_handlers` 全局注册表——本模块在该注册表以
%%% `with_handlers/2` 作用域安装桥 handler，使**生成程序 actor 实际发出的
%%% 业务 tool 调用**进入 IMBoy 可信执行上下文与中央授权咽喉：
%%%
%%%   actor 内 ws_detail(Args)
%%%     → hird_tool_dispatch:call（外层，audit 记录）
%%%     → 注册表桥（本模块 bridge_handler/1）
%%%     → agent_tool_authorizer:authorize/3（十步 fail-closed，真实 PG）
%%%     → PoC 业务 handler（真实只读业务读取，effect 凭证防护）
%%%
%%% 纪律（§14 对齐 imboy_hird）：
%%%   * Run/Agent/Org/Grant 身份全部来自**可信宿主绑定的 RunCtx**（桥闭包
%%%     捕获），绝不来自 tool 参数或程序输出——程序无法自选身份；
%%%   * deny/approval_required 不触业务 handler（业务执行计数恒 0），
%%%     以域错误 `throw {hird_exn, {tool_denied, Reason}}`（ADR-016 形状）
%%%     回传生成程序；
%%%   * raw 返回面：桥把业务 handler 的原始结果回传给生成程序（程序效果
%%%     面需要真实数据继续执行；该值会经外层 dispatch 进入 hird audit 面
%%%     ——调用方负责只喂合成/低敏数据）。对外 HTTP/API 面的 digest-only
%%%     纪律不受影响（imboy_hird:run_tool/5 仍 sanitize）。
%%%   * 注册表是节点全局（persistent_term）——并发 Run 需调用方以独立 VM
%%%     或串行化保证作用域互斥（PoC 口径：专用测试 VM 内串行）。
%%%   * **回放护栏（C2#10 CLOSEOUT）**：`hird_replay:active/0` 是
%%%     `whereis` 检查——cursor 进程意外退出后 dispatch 立即回退 live 路径
%%%     并查询本注册表。回放编排器在 cursor 存活期以 `arm_replay_guard/0`
%%%     置护栏（`finish` 后 `disarm_replay_guard/0`）；护栏激活期间本桥
%%%     **在任何资源解析与授权之前**拒绝执行（deny 语义域错误回传），
%%%     保证「回放中 cursor 死亡」不可能转成真实业务执行。默认未装填
%%%     ——live 录制路径零影响。
-module(imboy_hird_host_bridge).

-export([
    run_program/2,
    bridge_handler/1,
    arm_replay_guard/0,
    disarm_replay_guard/0
]).

-define(REPLAY_GUARD_KEY, {imboy_hird_host_bridge, replay_guard}).

%% @doc 在注册表桥作用域内运行一个生成程序。
%%
%% Ctx 键（全部必填，缺键 fail closed）：
%%   program        :: module()             生成程序模块（须导出 main/1）
%%   registry       :: [{Key, Handler}]     注册表安装项（含桥在内的完整表）
%%   run_ctx        :: map()                可信宿主解析的 AgentRunContext
%%   tool           :: map()                ToolDescriptor（恒定，宿主签发）
%%   hird_tool_name :: atom()               注册表侧 tool 名（外层 dispatch 目标）
%%   resource_of    :: fun((Args) -> ResourceContext | {error, Reason})
%%                                           tool args → Resource（服务端解析，
%%                                           如查 workspace 真实归属 org）
%%   biz            :: fun((ResourceContext, EffectId) -> {ok, Raw} | {error, R})
%%                                           授权 allow 后的业务执行体
run_program(Ctx, MainArg) ->
    Required = [program, registry, run_ctx, tool, hird_tool_name, resource_of, biz],
    Missing = [K || K <- Required, not maps:is_key(K, Ctx)],
    case Missing of
        [] ->
            hird_handlers:with_handlers(
                maps:get(registry, Ctx),
                fun() -> (maps:get(program, Ctx)):main(MainArg) end
            );
        _ ->
            {error, {bridge_ctx_missing, Missing}}
    end.

%% @doc 桥 handler（注册表条目体）：回放护栏 → 授权 → （仅 allow）业务执行。
%% deny/approval 以 {hird_exn, _} 域错误回传（ADR-016：dispatch 记 {err,_}
%% 后原样 rethrow，生成程序侧按域错误传播）。
bridge_handler(Ctx) ->
    fun(Args, _ThreadedHandlers) ->
        case persistent_term:get(?REPLAY_GUARD_KEY, false) of
            true ->
                %% 回放护栏激活（cursor 存活期）：本调用点是回放中 cursor
                %% 死亡后 dispatch 回退 live 的落点——拒绝发生在资源解析
                %% 与授权之前，真实执行零可能。
                throw({hird_exn, {tool_denied, <<"replay_guard_active">>}});
            false ->
                RunCtx = maps:get(run_ctx, Ctx),
                Tool = maps:get(tool, Ctx),
                case (maps:get(resource_of, Ctx))(Args) of
                    {error, Reason} ->
                        throw({hird_exn, {tool_denied, reason_bin(Reason)}});
                    Resource ->
                        dispatch_authorized(Ctx, RunCtx, Tool, Resource)
                end
        end
    end.

%% @doc 装填回放护栏（回放编排器在 cursor 存活期调用；节点全局，PoC 口径
%% 为专用 VM 内串行）。
arm_replay_guard() ->
    persistent_term:put(?REPLAY_GUARD_KEY, true),
    ok.

%% @doc 解除回放护栏（cursor 正常 finish 或编排异常收尾时调用）。
disarm_replay_guard() ->
    persistent_term:erase(?REPLAY_GUARD_KEY),
    ok.

dispatch_authorized(Ctx, RunCtx, Tool, Resource) ->
    case agent_tool_authorizer:authorize(RunCtx, Tool, Resource) of
        {allow, Decision} ->
            EffectId = maps:get(effect_id, Decision, undefined),
            Biz = maps:get(biz, Ctx),
            case Biz(Resource, EffectId) of
                {ok, Raw} ->
                    Raw;
                {error, Reason} ->
                    throw({hird_exn, {tool_failed, reason_bin(Reason)}});
                Other ->
                    throw({hird_exn, {tool_bad_shape, reason_bin(Other)}})
            end;
        {deny, Reason} ->
            throw({hird_exn, {tool_denied, reason_bin(Reason)}});
        {approval_required, _AC} ->
            throw({hird_exn, {tool_approval_required, <<"approval_required">>}})
    end.

reason_bin(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
reason_bin(Reason) when is_binary(Reason) ->
    Reason;
reason_bin(Reason) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Reason])).
