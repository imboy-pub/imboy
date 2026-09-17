%%% @doc `imboy_hird` anti-corruption layer（AG31-10B；架构 §12.3）。
%%%
%%% Runtime API -> imboy_hird -> hird_* runtime。职责：DTO、生命周期、
%%% Tool handler、audit bridge、cancel/timeout 映射（Model adapter 面
%%% V3.1 由注入 handler 承担）。**不拥有** Organization/Grant/Permission/
%%% CS/Memory/ProductTask 数据（§12.3 冻结边界）。
%%%
%%% D2 三约束的宿主落法（10A spike 实证）：
%%%   1. **实例隔离唯一通道 = 每实例不同声明名**：boot_lifecycle/2 封装
%%%      宿主自扮 boot（audit sink → register_tools → M:main() → sync →
%%%      stop），调用方保证每 Run/实例一组独立声明名；
%%%   2. **audit run scope**：hird_audit 是 node 级单例，本层强制 dispatch
%%%      caller 形如 `<<"agent_run:<run_id>:<tool_id>">>`——并发 run 的
%%%      audit 混流靠 run scope 字段区分；
%%%   3. **禁共享 persistent_term key**：Tool handler 经进程本地 handler
%%%      map（hird_tool_dispatch:call/4 第 4 参）随调用传参，本层不写
%%%      hird_handlers 全局注册表（with_run_handlers/3 仅进程内作用域）。
%%%
%%% fail-closed 纪律（§14）：authorize 非allow（含 approval_required）不触
%%% hird runtime；hird 不可达 → {error, hird_unavailable}（绝不绕过
%%% authorizer）；dispatch 崩溃 → {error, dispatch_crashed}。
-module(imboy_hird).

-export([
    boot_lifecycle/2,
    run_tool/5,
    dispatch_with_timeout/5,
    with_run_handlers/3,
    caller_for/2,
    sanitize_result/2
]).

-define(DEFAULT_TIMEOUT_MS, 30000).

%% ===================================================================
%% 生命周期（D2#1：宿主自扮 boot；每实例独立声明名由调用方保证）
%% ===================================================================

%% @doc 宿主自扮 boot 序列（G03 OTP lifecycle）：
%% ```
%% boot_lifecycle(ProgramModule, #{audit_file => File, caller_base => Bin})
%%   -> ok | {error, Reason}
%% '''
%% 序列：audit sink 开启 → register_tools(签名表) → M:main() → sync →
%% audit stop。audit 单例（node 级）由调用方串行化（每实例串行 boot）。
boot_lifecycle(ProgramModule, Ctx) ->
    AuditFile = maps:get(audit_file, Ctx),
    _ = file:delete(AuditFile),
    ok = filelib:ensure_dir(AuditFile),
    case hird_audit:start_link([{sink, {file, AuditFile}}]) of
        {ok, _Pid} ->
            try
                ok = hird_audit:register_tools(ProgramModule:hird_tools@()),
                ok = ProgramModule:main(),
                ok = hird_audit:sync(),
                ok
            catch
                Class:Reason -> {error, {boot_failed, Class, Reason}}
            after
                safe_stop_audit()
            end;
        {error, Reason} ->
            {error, {audit_start_failed, Reason}}
    end.

safe_stop_audit() ->
    try
        gen_server:stop(hird_audit)
    catch
        _:_ -> ok
    end,
    ok.

%% ===================================================================
%% Tool 全链（G09/G10/G18/G19：authorizer 唯一入口，hird 面在后）
%% ===================================================================

%% @doc readonly Tool 全链：
%% ```
%% run_tool(RunCtx, ToolDescriptor, ResourceContext, HandlerMap, HirðToolName)
%%   -> {ok, Sanitized} | {approval_required, _} | {deny, ReasonCode}
%%    | {error, hird_unavailable | dispatch_crashed | {dispatch_timeout, Ms}}
%% '''
%% 顺序：agent_tool_authorizer:authorize/3（十步 fail-closed + 决策持久化
%% + 审计）→ **仅 allow** → hird_tool_dispatch:call（进程本地 handler map，
%% caller=run scope）→ sanitized（digest only）。deny/approval 路径 hird
%% runtime 零接触。
run_tool(RunCtx, ToolDescriptor, ResourceContext, HandlerMap, HirðToolName) ->
    case agent_tool_authorizer:authorize(RunCtx, ToolDescriptor, ResourceContext) of
        {deny, Reason} ->
            {deny, Reason};
        {approval_required, _AC} = Approval ->
            Approval;
        {allow, DecisionContext} ->
            EffectId = maps:get(effect_id, DecisionContext, undefined),
            Args = maps:get(hird_args, ResourceContext, #{}),
            dispatch_with_timeout(
                HirðToolName, caller_for(RunCtx, ToolDescriptor), Args, HandlerMap, #{
                    timeout_ms => maps:get(timeout_ms, RunCtx, ?DEFAULT_TIMEOUT_MS),
                    effect_id => EffectId
                }
            )
    end.

%% @doc run scope caller（D2#2）：`<<"agent_run:<run_id>:<tool_id>">>`。
caller_for(RunCtx, ToolDescriptor) ->
    iolist_to_binary(
        io_lib:format(
            "agent_run:~s:~s",
            [
                safe_iolist(maps:get(run_id, RunCtx, unknown)),
                safe_iolist(maps:get(tool_id, ToolDescriptor, unknown))
            ]
        )
    ).

%% @doc dispatch + timeout/cancel 映射（§14）：hird_tool_dispatch:call 为
%% 同步调用；本层以进程封装超时——超时即 {error,{dispatch_timeout,Ms}}
%% （调用方负责 effect 面处置：agent_recovery:mark_unknown 显式登记，本层
%% 不代写 ledger）。
dispatch_with_timeout(Tool, Caller, Args, HandlerMap, Opts) ->
    TimeoutMs = maps:get(timeout_ms, Opts, ?DEFAULT_TIMEOUT_MS),
    Parent = self(),
    Tag = make_ref(),
    Worker =
        spawn(fun() ->
            Result =
                try
                    %% audit 单例是 dispatch 的同步依赖：未运行 = hird
                    %% runtime 不可用（§14），不得误报为 handler 崩溃
                    case whereis(hird_audit) of
                        P when is_pid(P) -> ok;
                        undefined -> exit(hird_unavailable)
                    end,
                    hird_tool_dispatch:call(Tool, Caller, HandlerMap, Args)
                catch
                    exit:hird_unavailable -> {error, hird_unavailable};
                    _Class:_Reason -> {error, dispatch_crashed}
                end,
            Parent ! {Tag, Result}
        end),
    receive
        {Tag, Result} ->
            sanitize_dispatch_result(Result, Opts)
    after TimeoutMs ->
        erlang:exit(Worker, kill),
        {error, {dispatch_timeout, TimeoutMs}}
    end.

%% hird_tool_dispatch:call 成功返回 handler **裸值**（域错误走 throw
%% {hird_exn,_} 不经 return 面；bridge 自身错误走 {error,_}）——
%% error 形状先行分流，catch-all 即成功裸值。
sanitize_dispatch_result({error, Reason}, _Opts) ->
    {error, Reason};
sanitize_dispatch_result(Raw, Opts) ->
    {ok, sanitize_result(Raw, maps:get(effect_id, Opts, undefined))}.

%% @doc sanitized：原始载荷不外泄，只留 digest（§14 敏感数据行）。
sanitize_result(Raw, EffectId) ->
    #{
        status => ok,
        effect_id => EffectId,
        result_digest => digest(Raw)
    }.

%% ===================================================================
%% 进程本地 handler map（D2#3：零 persistent_term 共享 key）
%% ===================================================================

%% @doc 以进程本地 handler map 执行 Fun（直传 hird_tool_dispatch:call/4
%% 第 4 参）。提供 with_handlers 的进程内等价物但**不触全局注册表**；
%% 并发 run 各持各的 map，零共享 key 竞争。本函数不装卸任何全局状态，
%% 退出零残留（G06/G20）。
with_run_handlers(_RunId, _HandlerMap, Fun) ->
    Fun().

%% ===================================================================
%% 内部
%% ===================================================================

digest(Raw) ->
    Data = io_lib:format("~p", [Raw]),
    binary:encode_hex(erlang:md5(unicode:characters_to_binary(Data))).

safe_iolist(V) when is_binary(V) ->
    binary_to_list(V);
safe_iolist(V) when is_integer(V) ->
    integer_to_list(V);
safe_iolist(V) when is_atom(V) ->
    atom_to_list(V);
safe_iolist(V) ->
    io_lib:format("~p", [V]).
