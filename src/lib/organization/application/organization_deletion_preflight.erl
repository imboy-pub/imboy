-module(organization_deletion_preflight).

%% User deletion preflight 编排器（application 层）。
%%
%% Core Contract C17 + 计划 §1.6 DeletionPreflightFacts：
%%   * 调用全部「已注册」provider；任一 unavailable/timeout/inconsistent
%%     → 整体拒绝，稳定原因 `DEPENDENCY_FACTS_UNAVAILABLE`。
%%   * 冻结五域缺一不可：未注册域 = facts 不可得 = 拒；
%%     禁止给未实现域默认空 blocker；禁止缓存 allow（每次实时读）。
%%   * preflight 是解释性证据非 TOCTOU 保障；最终删除事务仍依赖
%%     DB RESTRICT/guard（00000126 已给 organization.owner_id RESTRICT）。
%%   * providers 只读、不做 handover/offboarding。
%%
%% provider 注册表：
%%   默认（代码内置）= organization + workspace 两域（org 侧只读代查 workspace
%%   owner 字段，ORG-02 任务卡冻结口径）；enterprise_business / customer_service /
%%   agent 三域的 provider 由各自域任务交付后注册。测试/集成可用
%%   application:set_env(imboy, deletion_preflight_providers, [{Domain, M, F}])
%%   覆盖注册表——但 required 域集合不可缩减：覆盖后仍缺域照拒。
%%
%% provider 回调约定：M:F(UserId) -> §1.6 冻结形状 | {error, unavailable}。

-export([run/1]).
-export([required_domains/0, registry/0, unavailable_code/0]).

-define(UNAVAILABLE_CODE, <<"DEPENDENCY_FACTS_UNAVAILABLE">>).
-define(DEFAULT_TIMEOUT_MS, 5000).

%% @doc 执行一次实时 preflight。
%%
%% 返回：
%%   {ok, #{subject_user_id => UserId, blockers => [BlockerMap()],
%%          facts => [FactMap()], observed_at => Ms}}
%%     —— 全部已注册域 facts 可得；blockers 为聚合后的冻结四字段 blocker。
%%     blockers = [] 只表示本次实时读取无 blocker。
%%   {error, #{code => <<"DEPENDENCY_FACTS_UNAVAILABLE">>, reason => atom(),
%%             domain => Domain | undefined, detail => term()}}
%%     —— 缺域 / provider unavailable / timeout / inconsistent（malformed
%%     返回按 inconsistent 归口）。整体拒绝，调用方不得删除。
-spec run(integer()) -> {ok, map()} | {error, map()}.
run(UserId) when is_integer(UserId), UserId > 0 ->
    Registry = registry(),
    case required_domains() -- [D || {D, _M, _F} <- Registry] of
        [] ->
            collect(UserId, Registry, [], [], os:system_time(millisecond));
        Missing ->
            {error, #{
                code => ?UNAVAILABLE_CODE,
                reason => provider_unregistered,
                domain => undefined,
                detail => Missing
            }}
    end;
run(_) ->
    {error, #{
        code => ?UNAVAILABLE_CODE,
        reason => bad_subject,
        domain => undefined,
        detail => subject_user_id_required
    }}.

%% @doc 冻结的五个依赖域（Core Contract C17 / 计划 §1.6，不可缩减）。
-spec required_domains() ->
    [organization | workspace | enterprise_business | customer_service | agent].
required_domains() ->
    [organization, workspace, enterprise_business, customer_service, agent].

%% @doc 当前生效的 provider 注册表 [{Domain, Module, Function}]。
%%
%% 禁止通过本表「跳过」preflight：env 覆盖只换 provider 实现，
%% required 域集合恒为 required_domains/0，缺域照拒（fail-closed）。
-spec registry() -> [{atom(), atom(), atom()}].
registry() ->
    case application:get_env(imboy, deletion_preflight_providers) of
        {ok, [{_, _, _} | _] = Registry} ->
            Registry;
        _ ->
            Default = organization_preflight_facts_pg,
            [
                {organization, Default, facts_organization},
                {workspace, Default, facts_workspace}
            ]
    end.

%% @doc 稳定拒绝原因（计划 §1.6 冻结字面量）。
-spec unavailable_code() -> binary().
unavailable_code() ->
    ?UNAVAILABLE_CODE.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

%% 逐域实时拉取；任一失败立即整体拒绝（fail-fast，不缓存）。
collect(UserId, [], Facts, Blockers, ObservedAt) ->
    {ok, #{
        subject_user_id => UserId,
        blockers => lists:append(lists:reverse(Blockers)),
        facts => lists:reverse(Facts),
        observed_at => ObservedAt
    }};
collect(UserId, [{Domain, M, F} | Rest], Facts, Blockers, ObservedAt) ->
    case call_provider(M, F, UserId) of
        {ok, Fact} ->
            case validate_fact(UserId, Domain, Fact) of
                ok ->
                    collect(
                        UserId,
                        Rest,
                        [Fact | Facts],
                        [maps:get(blockers, Fact) | Blockers],
                        ObservedAt
                    );
                {error, inconsistent} ->
                    reject(Domain, inconsistent, malformed_fact_shape)
            end;
        {error, Reason} when Reason =:= unavailable; Reason =:= timeout; Reason =:= inconsistent ->
            reject(Domain, Reason, Reason);
        {error, Reason} ->
            reject(Domain, inconsistent, Reason)
    end.

reject(Domain, Reason, Detail) ->
    {error, #{
        code => ?UNAVAILABLE_CODE,
        reason => Reason,
        domain => Domain,
        detail => Detail
    }}.

%% 独立进程内调用 provider：崩溃 → inconsistent；挂起 → timeout（kill）。
%% 编排器与删除事务同进程运行（user_deletion_executor 门），provider 用
%% 独立连接池连接做只读查询，不与事务共享连接/锁。
call_provider(M, F, UserId) ->
    Timeout = application:get_env(imboy, deletion_preflight_timeout_ms, ?DEFAULT_TIMEOUT_MS),
    Parent = self(),
    Ref = make_ref(),
    Pid =
        spawn(fun() ->
            Result =
                try M:F(UserId) of
                    Any -> Any
                catch
                    _:_ -> {error, inconsistent}
                end,
            Parent ! {Ref, Result}
        end),
    receive
        {Ref, Result} ->
            Result
    after Timeout ->
        exit(Pid, kill),
        {error, timeout}
    end.

%% 冻结形状校验（计划 §1.6）：subject/domain 必须与注册域一致，
%% observed_at/fact_version 为整数，blockers 为冻结四字段 map 列表。
validate_fact(UserId, Domain, #{
    subject_user_id := UserId,
    domain := Domain,
    observed_at := Ts,
    fact_version := V,
    blockers := Blockers
}) when
    is_integer(Ts),
    is_integer(V),
    V >= 1,
    is_list(Blockers)
->
    case lists:all(fun valid_blocker/1, Blockers) of
        true -> ok;
        false -> {error, inconsistent}
    end;
validate_fact(_UserId, _Domain, _Other) ->
    {error, inconsistent}.

valid_blocker(#{
    code := Code, resource_type := Type, resource_id := OpaqueId, organization_id := OrgId
}) when
    is_binary(Code),
    byte_size(Code) > 0,
    is_binary(Type),
    byte_size(Type) > 0,
    is_binary(OpaqueId),
    byte_size(OpaqueId) > 0,
    (OrgId =:= null orelse is_integer(OrgId))
->
    true;
valid_blocker(_) ->
    false.
