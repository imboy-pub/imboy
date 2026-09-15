%%% @doc EB-08 测试探针：把**只读事实**装配成 `eb_auth_app` 需要的形状（逐请求、不缓存）。
%%%
%%% 为什么需要它（如实登记）：`eb_auth_app:authorize_via_port/3` 的装配实现是
%%% `eb_pg_auth_facts`，但后者的 `SQL_MEMBER_ASSIGNMENTS` 只投影了
%%% `business_identity_id / function_key / status / version`，**没有** `user_id` 与
%%% `organization_id`；而 `eb_auth_app:active_assignments/3` 要求这两键。于是
%%% 「装配路径 + 真库」下 `enterprise_member` 恒被 `identity_assignment_missing`
%%% 拒绝（fail-closed，安全但无法区分原因）。该缺口属 infrastructure 租约
%%% （EB-08 不得改），登记为 findings[EB08-C1]。
%%%
%%% 本探针**不发明事实**：成员状态经最小只读事实 Port（`eb_member_fact_port` 的装配
%%% 实现）逐请求读取；经办关系经 store 端口（`list_assignments/2`，其行**自带**
%%% `user_id`/`organization_id`）读取；角色与权限集经 `eb_pg_auth_facts` 读取。
%%% 三者都是权威源的真实读数，本模块只做形状对齐（补 `organization_id` 到 assignment
%%% 行上，值取自 store 的同名字段，不构造新值）。
%%%
%%% 计数：每次 `load_request_facts/1` 都 +1（证明「不跨请求缓存」）。
-module(eb08_auth_probe).

-behaviour(eb_auth_port).

-export([load_request_facts/1, reset/0, count/0]).

-define(COUNTER_KEY, {?MODULE, invocations}).

%% @doc 清零调用计数。
-spec reset() -> ok.
reset() ->
    persistent_term:put(?COUNTER_KEY, counters:new(1, [write_concurrency])),
    ok.

%% @doc 已发生的逐请求装载次数（>1 证明没有复用上一次的结论）。
-spec count() -> non_neg_integer().
count() ->
    case persistent_term:get(?COUNTER_KEY, undefined) of
        undefined -> 0;
        Ref -> counters:get(Ref, 1)
    end.

%% @doc 逐请求装载授权事实（真读只读事实 Port + store + 授权事实源）。
%%
%% `Request` 必含 `organization_id` / `workspace_id` / `user_id`。
-spec load_request_facts(map()) -> {ok, map()} | {error, term()}.
load_request_facts(Request) when is_map(Request) ->
    bump(),
    OrgId = maps:get(organization_id, Request, undefined),
    WorkspaceId = maps:get(workspace_id, Request, undefined),
    UserId = maps:get(user_id, Request, undefined),
    case {is_integer(OrgId), is_integer(WorkspaceId), is_integer(UserId)} of
        {true, true, true} ->
            load(OrgId, WorkspaceId, UserId);
        _Missing ->
            {error, {missing_request_keys, Request}}
    end;
load_request_facts(_Request) ->
    {error, invalid_request}.

load(OrgId, WorkspaceId, UserId) ->
    case {eb_infra_ports:resolve(member_fact), eb_infra_ports:resolve(store)} of
        {{ok, MemberFact}, {ok, Store}} ->
            case MemberFact:member_status(OrgId, UserId) of
                {ok, Status} ->
                    case Store:list_assignments(OrgId, WorkspaceId) of
                        {ok, Assignments} ->
                            case
                                eb_pg_auth_facts:load_request_facts(#{
                                    organization_id => OrgId, user_id => UserId
                                })
                            of
                                {ok, AuthFacts} ->
                                    {ok, facts(OrgId, UserId, Status, Assignments, AuthFacts)};
                                {error, _} = Err ->
                                    Err
                            end;
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {{error, _} = Err, _} ->
            Err;
        {_, {error, _} = Err} ->
            Err
    end.

%% assignment 行补 `organization_id`：值来自 store 行自身的同名字段（缺失才补，
%% 不构造新值）；`user_id` 由 store 直接给出，无需补齐。
facts(OrgId, UserId, Status, Assignments, AuthFacts) ->
    Member = maps:get(member, AuthFacts, #{}),
    Owned = [
        normalize_assignment(A, OrgId, UserId)
     || A <- Assignments,
        maps:get(user_id, A, undefined) =:= UserId
    ],
    #{
        organization_id => OrgId,
        member => #{
            user_id => UserId,
            role => maps:get(role, Member, undefined),
            status => Status,
            governance_roles => []
        },
        assignments => Owned,
        permissions => maps:get(permissions, AuthFacts, [])
    }.

normalize_assignment(A, OrgId, UserId) ->
    Base = #{
        business_identity_id => maps:get(business_identity_id, A, undefined),
        function_key => maps:get(function_key, A, undefined),
        status => maps:get(status, A, undefined),
        version => maps:get(version, A, undefined),
        user_id => UserId
    },
    case maps:get(organization_id, A, undefined) of
        undefined -> Base#{organization_id => OrgId};
        Stored -> Base#{organization_id => Stored}
    end.

bump() ->
    case persistent_term:get(?COUNTER_KEY, undefined) of
        undefined ->
            reset(),
            bump();
        Ref ->
            _ = counters:add(Ref, 1, 1),
            ok
    end.
