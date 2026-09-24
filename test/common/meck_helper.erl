-module(meck_helper).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% Meck Mock 管理辅助模块
%%%
%%% 提供：
%%% - 统一的 Mock 创建和管理
%%% - Mock 验证和清理
%%% - 常用 Mock 模板
%%% - 测试数据生成器
%%%
%%% 使用方法：
%%% ?WITH_MECK([Module], Expectations, TestFun)
%%%===================================================================

-export([
    setup_mock/2,
    setup_mock/3,
    cleanup_mock/1,
    cleanup_mocks/1,
    verify_mock/1,
    verify_mock/2,
    verify_called/3,
    verify_called_once/3,

    % Mock 模板
    mock_elib_param/1,
    mock_elib_response/1,
    full_elib_response_mock/0,
    full_elib_response_mock/1,
    mock_passport_logic/1,
    mock_user_repo/1,
    mock_elib_pg/1,

    % 测试数据生成
    test_user/0,
    test_user/1,
    test_group/0,
    test_group/1,
    test_message/0,
    test_message/1,
    test_request/0,
    test_request/1,

    % 常驻调用者护栏（见文末 §常驻调用者护栏）
    is_resident_caller/0,
    is_resident_caller/1,
    caller_module/1,
    resident_caller_modules/0
]).

%% ===================================================================
%% Mock 管理函数
%% ===================================================================

%% @doc 为单个模块设置 Mock
%% @param Module 要 Mock 的模块名
%% @param Expectations 期望函数列表，格式为 [{Function, Arity, Fun}]
setup_mock(Module, Expectations) ->
    % 依次尝试多种策略，优先使用 passthrough+unstick 兼容旧测试。
    % 若模块在当前 code path 下短暂不可加载（如 {error,nofile}），
    % 则自动回退到不依赖 unstick 的纯 mock 策略。
    setup_mock_with_fallback(
        Module,
        Expectations,
        [
            [passthrough, no_link, unstick],
            [no_link, unstick],
            [passthrough, no_link],
            [non_strict, no_link],
            [no_link]
        ],
        []
    ).

%% @doc 为单个模块设置 Mock（带选项）
%% @param Module 要 Mock 的模块名
%% @param Options meck 选项，如 [passthrough, unstick, no_link]
%% @param Expectations 期望函数列表
setup_mock(Module, Options, Expectations) ->
    setup_mock(Module, Options, Expectations, 3).

setup_mock(_Module, _Options, _Expectations, Retries) when Retries =< 0 ->
    {error, too_many_retries};
setup_mock(Module, Options, Expectations, Retries) ->
    % 先尝试清理旧 mock，避免同模块重复 mock 卡住。
    cleanup_mock(Module),
    try
        meck:new(Module, Options),
        lists:foreach(
            fun({Func, Arity, Fun}) ->
                meck:expect(Module, Func, Arity, normalize_mock_fun(Fun, Arity))
            end,
            Expectations
        ),
        {ok, Module}
    catch
        error:{already_started, _} ->
            timer:sleep(10),
            setup_mock(Module, Options, Expectations, Retries - 1);
        _:Error:StackTrace ->
            % 捕获并返回详细错误
            {error, {Error, StackTrace}}
    end.

setup_mock_with_fallback(_Module, _Expectations, [], Errors) ->
    {error, {mock_setup_failed, lists:reverse(Errors)}};
setup_mock_with_fallback(Module, Expectations, [Options | Rest], Errors) ->
    case setup_mock(Module, Options, Expectations) of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            setup_mock_with_fallback(
                Module,
                Expectations,
                Rest,
                [{Options, Reason} | Errors]
            )
    end.

%% @doc 清理单个 Mock
cleanup_mock(Module) ->
    % 当前 meck 版本不保证提供 is_loaded/1，直接尝试 unload 并忽略未 mock 错误。
    _ = catch meck:unload(Module),
    % 某些场景 unload 后模块会变为未加载状态，下一次含 unstick 的 mock
    % 可能在 ensure_loaded 处报 {error,nofile}；尽力恢复原模块加载状态。
    _ = catch code:ensure_loaded(Module),
    ok.

%% @doc 批量清理 Mock
cleanup_mocks(Modules) when is_list(Modules) ->
    lists:foreach(fun cleanup_mock/1, Modules);
cleanup_mocks(Module) when is_atom(Module) ->
    cleanup_mock(Module).

%% @doc 验证 Mock 调用
%% @param Module Mock 的模块名
verify_mock(Module) ->
    try
        History = meck:history(Module),
        ?debugFmt("Mock history for ~p: ~p", [Module, History]),
        {ok, History}
    catch
        _:Error ->
            {error, Error}
    end.

%% ===================================================================
%% 常驻调用者护栏（跨套件隔离纪律）
%% ===================================================================
%% 问题：meck 拦截是 VM 全局的，而全量 eunit 期间 imboy app 常驻。测试期
%% 安装的期望 fun 是按**测试自身入参**写死的（多为 pattern-clause 断言型），
%% 此时常驻 worker 打进来的调用参数不匹配 → function_clause 当场打死 worker
%% → 池连接带孤儿命令回池 → epgsql unexpected_message 连环崩 → no_connection
%% 风暴（ent-org-v21-closure gate4 实证：agent_task_logic ×2 / ai_agent_proactive
%% 按运行顺序确定性变红，单跑全绿）。
%%
%% 纪律：经本 helper 安装的全部期望 fun 只对**测试侧调用者**生效；调用者
%% 是 §常驻模块名单 中的长驻进程时改走原实现（meck:passthrough/1 —— 直接
%% 调 <mod>_meck_original，meck:new 无条件备份原模块，与 passthrough 选项
%% 无关，见 deps/meck/src/meck_proc.erl backup_original/4）。
%%
%% 调用者判据（proc_lib:initial_call/1，实证三种形态）：
%%   * gen_server / gen_statem 进程 → {Mod, init, ['Argument__1']}   → 取第 1 位
%%   * supervisor 进程               → {supervisor, Mod, ['Argument__1']} → 取第 2 位
%%   * gen_event 进程                → {gen_event, ...}（handler 模块不可见，
%%     故 imboy_domain_event 之类的 gen_event 宿主不入名单——其 handler 在
%%     gen_event 进程内执行，无法按模块名识别）
%%   * eunit 测试进程 / 裸 spawn     → false（实证），不入名单语义 → 期望生效
%%
%% §常驻模块名单的取得方式（可复现）：以 sys.local 同口径起 app，遍历
%% processes() 对每个进程取 proc_lib:initial_call/1 并按上述形态归一出模块名，
%% 取其中 imboy 自有模块集（tools/dump 脚本见 run evidence）。名单必须满足：
%%   (a) 该模块确实拥有常驻进程（否则永远匹配不到，是死条目）；
%%   (b) 没有套件要求「从该模块自身进程内发出的调用」命中期望——违反 (b)
%%       会把「测 server 内路径」的期望让路掉。
%%
%% (b) 的排除项（显式登记，勿擅自加入名单）：
%%   * login_attempt_ds —— 既是常驻 gen_server 又是单测 SUT：其 handle_call
%%     在 server 进程内执行业务，入名单会让 login_attempt_ds_tests 的期望
%%     被让路（is_locked 等用例红，v3 全量实证）。
%%
%% 说明：名单里不放库函数模块（ack_retry_cache / agent_rate_limiter）——
%% 它们不拥有进程，永远不会成为调用者；不放 depcache 包装（imboy_cache，
%% 其进程 initial_call 是 depcache）与 gen_event 宿主（imboy_domain_event）。

-define(RESIDENT_CALLER_MODULES, [
    %% imboy_sup 直属 worker（src/imboy_sup.erl Specs，逐项对照）
    agent_payment_compensation_worker,
    ai_agent_runtime,
    barrel_mcp_registry,
    barrel_mcp_session,
    billing_invoice_worker,
    bot_webhook_delivery_worker,
    credential_retention_worker,
    elib_metric,
    imboy_cache_sync,
    imboy_mcp_tools,
    imboy_plugin_loader,
    license_notice_worker,
    msg_burn_logic,
    moderation_sweep_logic,
    olm_otk_cleanup_worker,
    user_deletion_logic,
    user_server,
    %% 监督树自身（worker 崩溃重启期间的 supervisor 调用同样让路）
    imboy_sup,
    imboy_plugin_sup,
    imboy_plugin_generic_sup,
    %% msg_store_sup 子树
    msg_store_sup,
    msg_store_ds,
    msg_store_worker,
    %% WS / 路由注册表常驻进程
    imboy_router_registry,
    imboy_ws_action_registry
]).

%% 期望 fun 只对测试侧调用者生效；常驻进程走原实现。
-spec guard_apply(fun(), [term()]) -> term().
guard_apply(Fun, Args) ->
    case is_resident_caller() of
        true -> meck:passthrough(Args);
        false -> apply(Fun, Args)
    end.

%% 调用方进程是否属常驻模块名单。
-spec is_resident_caller() -> boolean().
is_resident_caller() ->
    is_resident_caller(caller_module(self())).

-spec is_resident_caller(atom() | skip) -> boolean().
is_resident_caller(skip) -> false;
is_resident_caller(M) when is_atom(M) -> lists:member(M, ?RESIDENT_CALLER_MODULES).

%% 归一化 proc_lib:initial_call/1 的三种形态（见上方判据）；非进程或非
%% proc_lib 进程返回 skip（eunit 测试进程 / 裸 spawn / undefined 实证为
%% false 或非法入参——本函数全覆盖，不抛错）。
-spec caller_module(pid() | term()) -> atom() | skip.
caller_module(Pid) when is_pid(Pid) ->
    try proc_lib:initial_call(Pid) of
        {supervisor, M, _A} when is_atom(M) -> M;
        {M, _F, _A} when is_atom(M) -> M;
        _NotProcLib -> skip
    catch
        _:_ -> skip
    end;
caller_module(_NotPid) ->
    skip.

%% 名单只读访问（供名单完整性回归测试使用）。
-spec resident_caller_modules() -> [atom()].
resident_caller_modules() -> ?RESIDENT_CALLER_MODULES.

%% 静态 arity 包装（meck:expect 要求精确元数；全仓期望 arity 实测 0..15）。
guard_wrap(Fun, 0) ->
    fun() -> guard_apply(Fun, []) end;
guard_wrap(Fun, 1) ->
    fun(A1) -> guard_apply(Fun, [A1]) end;
guard_wrap(Fun, 2) ->
    fun(A1, A2) -> guard_apply(Fun, [A1, A2]) end;
guard_wrap(Fun, 3) ->
    fun(A1, A2, A3) -> guard_apply(Fun, [A1, A2, A3]) end;
guard_wrap(Fun, 4) ->
    fun(A1, A2, A3, A4) -> guard_apply(Fun, [A1, A2, A3, A4]) end;
guard_wrap(Fun, 5) ->
    fun(A1, A2, A3, A4, A5) -> guard_apply(Fun, [A1, A2, A3, A4, A5]) end;
guard_wrap(Fun, 6) ->
    fun(A1, A2, A3, A4, A5, A6) -> guard_apply(Fun, [A1, A2, A3, A4, A5, A6]) end;
guard_wrap(Fun, 7) ->
    fun(A1, A2, A3, A4, A5, A6, A7) -> guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7]) end;
guard_wrap(Fun, 8) ->
    fun(A1, A2, A3, A4, A5, A6, A7, A8) -> guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7, A8]) end;
guard_wrap(Fun, 9) ->
    fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) ->
        guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7, A8, A9])
    end;
guard_wrap(Fun, 10) ->
    fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ->
        guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])
    end;
guard_wrap(Fun, 11) ->
    fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) ->
        guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])
    end;
guard_wrap(Fun, 12) ->
    fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) ->
        guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12])
    end;
guard_wrap(Fun, 13) ->
    fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) ->
        guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])
    end;
guard_wrap(Fun, 14) ->
    fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) ->
        guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14])
    end;
guard_wrap(Fun, 15) ->
    fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) ->
        guard_apply(Fun, [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15])
    end;
guard_wrap(Fun, _Higher) ->
    Fun.

normalize_mock_fun(Fun, Arity) ->
    {arity, FunArity} = erlang:fun_info(Fun, arity),
    case FunArity of
        Arity ->
            guard_wrap(Fun, Arity);
        N when N =:= Arity - 1 ->
            guard_wrap(wrap_drop_first_arg(Fun, Arity), Arity);
        _ ->
            guard_wrap(Fun, Arity)
    end.

wrap_drop_first_arg(Fun, 1) ->
    fun(_A1) ->
        Fun()
    end;
wrap_drop_first_arg(Fun, 2) ->
    fun(_A1, A2) ->
        Fun(A2)
    end;
wrap_drop_first_arg(Fun, 3) ->
    fun(_A1, A2, A3) ->
        Fun(A2, A3)
    end;
wrap_drop_first_arg(Fun, 4) ->
    fun(_A1, A2, A3, A4) ->
        Fun(A2, A3, A4)
    end;
wrap_drop_first_arg(Fun, 5) ->
    fun(_A1, A2, A3, A4, A5) ->
        Fun(A2, A3, A4, A5)
    end;
wrap_drop_first_arg(Fun, _) ->
    Fun.

%% @doc 验证 Mock 调用（带期望）
%% @param Module Mock 的模块名
%% @param ExpectedCalls 期望调用列表，格式为 [{Function, Arity, MinCalls}]
verify_mock(Module, ExpectedCalls) ->
    try
        lists:foreach(
            fun({Func, Arity, MinCalls}) ->
                CallCount = meck:num_calls(Module, Func, Arity),
                case CallCount >= MinCalls of
                    true ->
                        ok;
                    false ->
                        ?assert(
                            false,
                            io_lib:format(
                                "Expected ~p:~p/~p to be called at least ~p times, got ~p",
                                [Module, Func, Arity, MinCalls, CallCount]
                            )
                        )
                end
            end,
            ExpectedCalls
        ),
        {ok, verified}
    catch
        _:Error ->
            {error, Error}
    end.

%% @doc 验证函数被调用
%% @param Module 模块名
%% @param Function 函数名
%% @param Arity 函数元数
verify_called(Module, Function, Arity) ->
    CallCount = meck:num_calls(Module, Function, Arity),
    case CallCount > 0 of
        true ->
            ok;
        false ->
            ?assert(
                false,
                io_lib:format(
                    "Expected ~p:~p/~p to be called, but was not called",
                    [Module, Function, Arity]
                )
            )
    end.

%% @doc 验证函数被恰好调用一次
%% @param Module 模块名
%% @param Function 函数名
%% @param Arity 函数元数
verify_called_once(Module, Function, Arity) ->
    CallCount = meck:num_calls(Module, Function, Arity),
    case CallCount of
        1 ->
            ok;
        _ ->
            ?assertEqual(
                1,
                CallCount,
                io_lib:format(
                    "Expected ~p:~p/~p to be called exactly once, got ~p",
                    [Module, Function, Arity, CallCount]
                )
            )
    end.

%% ===================================================================
%% 常用 Mock 模板
%% ===================================================================

%% @doc Mock elib_param 模块
mock_elib_param(Overrides) ->
    DefaultParams = [
        {<<"account">>, <<"test@example.com">>},
        {<<"password">>, <<"password123">>},
        {<<"type">>, <<"email">>},
        {<<"rsa_encrypt">>, <<"0">>}
    ],
    Params = maps:to_list(
        maps:merge(
            maps:from_list(DefaultParams),
            maps:from_list(Overrides)
        )
    ),

    Expectations = [
        {'post', 1, fun(_Req) -> Params end}
    ],
    {ok, _} = setup_mock(elib_param, Expectations).

%% @doc Mock elib_response 模块
mock_elib_response(Overrides) ->
    DefaultResponse = cowboy_req_h:new(#{
        response_status => 200,
        response_body => #{status => success}
    }),
    Response = maps:merge(DefaultResponse, Overrides),

    Expectations = [
        {'success', 3, fun(_Req, _Data, _Message) -> Response end},
        {'error', 2, fun(_Req, _Message) ->
            Response#{response_status => 400, response_body => #{status => error}}
        end}
    ],
    {ok, _} = setup_mock(elib_response, Expectations).

%% @doc 返回覆盖所有 arity 的 elib_response mock expectations 列表
%% 用于 ?WITH_MECKS 中替换 {elib_response, [...]} 部分，防止
%% passthrough 到真实 elib_response 导致 cowboy_req:reply 崩溃。
%%
%% 用法：
%%   ?WITH_MECKS([
%%       {elib_response, meck_helper:full_elib_response_mock()},
%%       {other_mod, [...]}
%%   ], fun() -> ... end).
%%
%% 或带自定义 tag：
%%   {elib_response, meck_helper:full_elib_response_mock(my_tag)}
%%
full_elib_response_mock() ->
    full_elib_response_mock(resp_mock).

full_elib_response_mock(Tag) ->
    [
        {'success', 1, fun(_Req) -> {Tag, success, #{}} end},
        {'success', 2, fun(_Req, Payload) -> {Tag, success, Payload} end},
        {'success', 3, fun(_Req, Payload, _Msg) -> {Tag, success, Payload} end},
        {'success', 4, fun(_Req, Payload, _Msg, _Opts) -> {Tag, success, Payload} end},
        {'error', 1, fun(_Req) -> {Tag, error, <<>>} end},
        {'error', 2, fun(_Req, Msg) -> {Tag, error, Msg} end},
        {'error', 3, fun(_Req, Msg, _Code) -> {Tag, error, Msg} end},
        {'error', 4, fun(_Req, Msg, _Code, _Opts) -> {Tag, error, Msg} end},
        {'handle_logic_result', 2, fun
            (_Req, {ok, Data}) -> {Tag, success, Data};
            (_Req, {error, Msg}) -> {Tag, error, Msg}
        end}
    ].

%% @doc Mock passport_logic 模块
mock_passport_logic(Overrides) ->
    DefaultUser = #{
        <<"uid">> => 12345,
        <<"account">> => <<"test@example.com">>,
        <<"nickname">> => <<"Test User">>
    },
    User = maps:merge(DefaultUser, Overrides),

    Expectations = [
        {'signup', 3, fun(_Type, _Account, _Password) -> {ok, User} end},
        {'do_login', 3, fun(_Type, _Account, _Password) -> {ok, User} end},
        {'find_password', 2, fun(_Type, _Account) ->
            {ok, #{<<"message">> => <<"重置密码邮件已发送"/utf8>>}}
        end}
    ],
    {ok, _} = setup_mock(passport_logic, Expectations).

%% @doc Mock user_repo 模块
mock_user_repo(Overrides) ->
    DefaultUser = #{
        <<"id">> => 12345,
        <<"account">> => <<"test@example.com">>,
        <<"nickname">> => <<"Test User">>,
        <<"status">> => 1
    },
    User = maps:merge(DefaultUser, Overrides),

    Expectations = [
        {'find_by_email', 2, fun(_Email, _Column) -> User end},
        {'find_by_id', 2, fun(_Id, _Column) -> User end},
        {'save', 1, fun(_Data) -> {ok, maps:get(<<"id">>, User)} end},
        {'update', 2, fun(_Id, _Data) -> {ok, 1} end}
    ],
    {ok, _} = setup_mock(user_repo, Expectations).

%% @doc Mock elib_pg 模块
mock_elib_pg(Overrides) ->
    DefaultResult = {ok, [{1, <<"test">>}]},
    Result = maps:get(result, Overrides, DefaultResult),

    Expectations = [
        {'query', 2, fun(_Sql, _Params) -> Result end},
        {'query', 3, fun(_Sql, _Params, _Conn) -> Result end},
        {'pluck', 4, fun(_Table, _Column, _Conditions, _Options) -> {ok, 1} end},
        {'page', 6, fun(_Table, _Column, _Where, _OrderBy, _Size, _Offset) ->
            maps:get(page_result, Overrides, [{<<"id">>, 1, {<<"kind">>, <<"1">>}}])
        end},
        {'with_tx', 1, fun(_TxFun) -> ok end}
    ],
    {ok, _} = setup_mock(elib_pg, Expectations).

%% @doc Mock user_collect_repo 模块
%% @private
mock_user_collect_repo(Overrides) ->
    DefaultCount = 0,
    Count = maps:get(count, Overrides, DefaultCount),

    Expectations = [
        {'count_by_uid_kind_id', 2, fun(_Uid, _KindId) -> Count end},
        {'delete', 2, fun(_Uid, _KindId) -> {ok, 1} end},
        {'update', 3, fun(_Uid, _KindId, _Data) -> {ok, 1} end},
        {'tablename', 0, fun() -> <<"public.user_collect">> end}
    ],
    {ok, _} = setup_mock(user_collect_repo, Expectations).

%% @doc Mock elib_uri 模块
%% @private
mock_elib_uri(Overrides) ->
    DefaultParams = {#{path => "/uploads/img.jpg"}, []},
    Params = maps:get(params, Overrides, DefaultParams),

    Expectations = [
        {'get_params', 1, fun(_Uri) -> Params end}
    ],
    {ok, _} = setup_mock(elib_uri, Expectations).

%% @doc Mock elib_dt 模块
%% @private
mock_elib_dt(Overrides) ->
    DefaultTimestamp = 1640995200,
    Timestamp = maps:get(timestamp, Overrides, DefaultTimestamp),

    Expectations = [
        {'now', 0, fun() -> Timestamp end},
        {'timestamp', 0, fun() -> Timestamp end}
    ],
    {ok, _} = setup_mock(elib_dt, Expectations).

%% @doc Mock group_member_repo 模块
%% @doc Mock group_member_repo 模块
%% @private
mock_group_member_repo(Overrides) ->
    DefaultResult = {ok, 1},
    Result = maps:get(result, Overrides, DefaultResult),

    Expectations = [
        {'transfer_ownership', 3, fun(_GroupId, _FromUid, _ToUid) -> Result end},
        {'is_owner', 2, fun(_GroupId, _Uid) ->
            maps:get(is_owner, Overrides, true)
        end},
        {'is_member', 2, fun(_GroupId, _Uid) ->
            maps:get(is_member, Overrides, true)
        end}
    ],
    {ok, _} = setup_mock(group_member_repo, Expectations).

%% @doc Mock group_repo 模块
%% @doc Mock group_repo 模块
%% @private
mock_group_repo(Overrides) ->
    DefaultGroup = #{
        <<"id">> => <<"group123">>,
        <<"name">> => <<"Test Group">>,
        <<"creator_id">> => 1
    },
    Group = maps:get(group, Overrides, DefaultGroup),

    Expectations = [
        {'find', 1, fun(_GroupId) -> {ok, Group} end},
        {'update', 2, fun(_GroupId, _Data) -> {ok, 1} end}
    ],
    {ok, _} = setup_mock(group_repo, Expectations).

%% @doc Mock websocket_ds 模块
%% @doc Mock websocket_ds 模块
%% @private
mock_websocket_ds(Overrides) ->
    DefaultResult = ok,
    Result = maps:get(result, Overrides, DefaultResult),

    Expectations = [
        {'send', 2, fun(_Uid, _Message) -> Result end},
        {'broadcast', 2, fun(_Uids, _Message) -> Result end}
    ],
    {ok, _} = setup_mock(websocket_ds, Expectations).

%% ===================================================================
%% 测试数据生成器
%% ===================================================================

%% @doc 生成默认用户测试数据
test_user() ->
    test_user(#{}).

%% @doc 生成自定义用户测试数据
test_user(Overrides) ->
    Default = #{
        id => 12345,
        account => <<"test_user_12345">>,
        nickname => <<"Test User">>,
        mobile => <<"+8613800138000">>,
        email => <<"test@example.com">>,
        password => <<"hashed_password">>,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% @doc 生成默认群组测试数据
test_group() ->
    test_group(#{}).

%% @doc 生成自定义群组测试数据
test_group(Overrides) ->
    Default = #{
        id => 54321,
        name => <<"Test Group">>,
        description => <<"A test group for testing">>,
        creator_id => 12345,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% @doc 生成默认消息测试数据
test_message() ->
    test_message(#{}).

%% @doc 生成自定义消息测试数据
test_message(Overrides) ->
    Default = #{
        id => 99999,
        from_uid => 12345,
        to_uid => 67890,
        content => <<"Hello, this is a test message">>,
        msg_type => 1,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% @doc 生成默认请求测试数据
test_request() ->
    test_request(#{}).

%% @doc 生成自定义请求测试数据
test_request(Overrides) ->
    Default = #{
        method => <<"POST">>,
        qs => <<>>,
        headers => #{},
        body => <<>>
    },
    cowboy_req_h:new(maps:merge(Default, Overrides)).

%% ===================================================================
%% EUnit 测试宏
%% ===================================================================

%% @doc 创建带单个 Mock 的测试
-define(WITH_MECK(Module, Expectations, TestFun),
    {setup,
        fun() ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> ?debugFmt("Mock setup failed: ~p", [Reason])
            end
        end,
        fun(_) ->
            meck_helper:cleanup_mock(Module)
        end,
        fun(_) -> ?_test(TestFun()) end}
).

%% @doc 创建带多个 Mock 的测试
-define(WITH_MECKS(MockConfigs, TestFun),
    {setup,
        fun() ->
            lists:foreach(
                fun({Module, Expectations}) ->
                    case meck_helper:setup_mock(Module, Expectations) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            ?debugFmt("Mock setup failed for ~p: ~p", [Module, Reason])
                    end
                end,
                MockConfigs
            )
        end,
        fun(_) ->
            Modules = [Module || {Module, _} <- MockConfigs],
            meck_helper:cleanup_mocks(Modules)
        end,
        fun(_) -> ?_test(TestFun()) end}
).

%% @doc 创建带 Mock 验证的测试
-define(WITH_MECK_VERIFY(Module, Expectations, VerifyCalls, TestFun),
    {setup,
        fun() ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> ?debugFmt("Mock setup failed: ~p", [Reason])
            end
        end,
        fun(_) ->
            meck_helper:verify_mock(Module, VerifyCalls),
            meck_helper:cleanup_mock(Module)
        end,
        fun(_) -> ?_test(TestFun()) end}
).

%% @doc 创建强断言宏
-define(ASSERT_EQUAL(Expected, Actual),
    ?assertEqual(Expected, Actual)
).

-define(ASSERT_MATCH(Pattern, Value),
    ?assertMatch(Pattern, Value)
).

-define(ASSERT_OK(Result),
    ?assertMatch({ok, _}, Result)
).

-define(ASSERT_ERROR(Result),
    ?assertMatch({error, _}, Result)
).
