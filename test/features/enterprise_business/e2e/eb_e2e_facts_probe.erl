%%% @doc EB-11 E2E 的**授权事实探针**（test-only）：只补两处装配缺口，不发明事实。
%%%
%%% 依据：`agents/a5/EB-11/ORDER.md` §4 的 F1 / F2 处置口径（「用测试侧注入，如实标注」）。
%%%
%%% ## 它补的是什么（两处，逐条可核对）
%%%
%%%   * **F2 — `governance_roles` 未投影**：生产装配 `eb_pg_auth_facts` 的 member 行
%%%     只投影 `user_id / role / status`，而 `eb_auth_app:governance_roles_of/1` 读
%%%     member 里的 `governance_roles`。于是 `enterprise_owner_admin` 在生产装配下
%%%     **恒不可达**（治理动作全 403）。本探针把**真实库里的 `role`**（owner / admin）
%%%     映射成治理角色值——映射是恒等投影（`owner`→`owner`、`admin`→`admin`），
%%%     **不新增**任何角色，也不放行 `member`。
%%%   * **F1 — 权限集只有 4 个值**：`eb_pg_auth_facts:permissions/1` 产出
%%%     `org.manage / member.manage / contact.read / conversation.read`，而动作表按
%%%     plan §5.4 词汇表声明了写侧权限。本探针把**本 E2E 显式授予的那一个权限**
%%%     追加进 `permissions`（`grant/1`），并在返回里单列
%%%     `granted_permissions` / `permissions_source`，使证据脚本能区分「真源权限」与
%%%     「测试补足的权限」。
%%%
%%% ## 它**不**做什么
%%%
%%% 不构造成员关系、不构造经办关系、不构造治理角色、不缓存任何事实（每次都现读
%%% `eb_pg_auth_facts`）。因此 `suspended` 的逐请求失效语义仍然是**真的**：成员状态
%%% 一变，下一个请求立刻被拒（EB-11-A01/A02 的该项断言不因本探针而放宽）。
-module(eb_e2e_facts_probe).

-behaviour(eb_auth_port).

-export([load_request_facts/1, grant/1, granted/0, clear/0]).

-define(KEY, {?MODULE, granted}).

%% @doc 设置本次请求要补足的权限（单进程/串行使用；E2E 逐请求设置）。
-spec grant([binary()]) -> ok.
grant(Permissions) when is_list(Permissions) ->
    persistent_term:put(?KEY, Permissions),
    ok.

-spec granted() -> [binary()].
granted() ->
    case persistent_term:get(?KEY, undefined) of
        undefined -> [];
        Permissions when is_list(Permissions) -> Permissions
    end.

-spec clear() -> ok.
clear() ->
    persistent_term:erase(?KEY),
    ok.

%% @doc 真事实（`eb_pg_auth_facts` 逐请求读库）+ 两处补偿。
-spec load_request_facts(map()) -> {ok, map()} | {error, term()}.
load_request_facts(Request) ->
    case eb_pg_auth_facts:load_request_facts(Request) of
        {ok, Facts} ->
            Real = maps:get(permissions, Facts, []),
            Member = maps:get(member, Facts, #{}),
            {ok, Facts#{
                permissions => lists:usort(Real ++ granted()),
                granted_permissions => granted(),
                permissions_source => {real, eb_pg_auth_facts},
                member => Member#{governance_roles => governance_roles(Member)}
            }};
        {error, _} = Err ->
            Err
    end.

%% 真实 role → 治理角色（恒等投影；`member` 不产生治理角色）。
governance_roles(#{role := owner}) -> [<<"owner">>];
governance_roles(#{role := admin}) -> [<<"admin">>];
governance_roles(_Member) -> [].
