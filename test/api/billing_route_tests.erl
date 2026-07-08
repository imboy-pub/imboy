%% @doc SEC-01 阶段1 路由固化测试
%%
%% 固化事实：管理端套餐 CRUD（plan_create/plan_update）已从 /v1/billing/*
%% 迁移至 /adm/finance/billing/*（走 adm RBAC）。租户端 /v1 仅保留 plan_list
%% （只读套餐目录）+ 9 个租户动作（subscribe/renew/cancel/...）。
%%
%% 防止回归：任何人不该在 /v1/billing/ 下重新暴露 plan_create/plan_update。
-module(billing_route_tests).

-include_lib("eunit/include/eunit.hrl").

%% /v1/billing/plan（POST 创建套餐）不得在 v1 路由表——管理动作走 adm RBAC
v1_billing_plan_create_route_removed_test() ->
    Paths = [unicode:characters_to_binary(P) || {P, _H, _S} <- all_routes()],
    ?assertNot(
        lists:member(<<"/api/v1/billing/plan">>, Paths),
        "管理动作 /v1/billing/plan 必须迁移至 /adm/finance/billing/*，不得留在 v1"
    ).

%% /v1/billing/plan/update（POST 改套餐）同样不得在 v1
v1_billing_plan_update_route_removed_test() ->
    Paths = [unicode:characters_to_binary(P) || {P, _H, _S} <- all_routes()],
    ?assertNot(
        lists:member(<<"/api/v1/billing/plan/update">>, Paths),
        "管理动作 /v1/billing/plan/update 必须迁移至 /adm/finance/billing/*，不得留在 v1"
    ).

%% /adm/finance/billing/plan（adm 侧 create）必须存在且 handler 是 adm_finance_handler
adm_billing_plan_create_route_exists_test() ->
    Handlers = [
        H
     || {P, H, _S} <- all_routes(),
        unicode:characters_to_binary(P) =:= <<"/api/adm/finance/billing/plan">>
    ],
    ?assert(
        lists:member(adm_finance_handler, Handlers),
        "adm 侧 /adm/finance/billing/plan 应存在且 handler 为 adm_finance_handler"
    ).

%% /v1/billing/plan/list（只读套餐目录）保留在 v1——对所有登录用户可见
v1_billing_plan_list_route_retained_test() ->
    BillingListHandlers = [
        H
     || {P, H, _S} <- all_routes(),
        unicode:characters_to_binary(P) =:= <<"/api/v1/billing/plan/list">>
    ],
    ?assert(
        lists:member(billing_handler, BillingListHandlers),
        "只读 /v1/billing/plan/list 应保留在 v1，handler 为 billing_handler"
    ).

%% billing_handler 不再 export plan_create/plan_update（死代码已删）
billing_handler_no_longer_exports_plan_crud_test() ->
    Exports = billing_handler:module_info(exports),
    ?assertNot(
        lists:keymember(plan_create, 1, Exports),
        "plan_create 已迁移至 adm_finance_handler，billing_handler 不该再 export"
    ),
    ?assertNot(
        lists:keymember(plan_update, 1, Exports),
        "plan_update 已迁移至 adm_finance_handler，billing_handler 不该再 export"
    ).

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

-spec all_routes() -> [tuple()].
all_routes() ->
    lists:flatmap(
        fun({_Host, Routes}) ->
            Routes
        end,
        imboy_router:get_routes()
    ).
