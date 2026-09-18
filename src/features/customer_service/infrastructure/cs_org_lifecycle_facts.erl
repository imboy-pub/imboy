%%% @doc `cs_org_lifecycle_port` 的唯一装配实现（ORG-08 adapter；只读）。
%%%
%%% 职责只有一件事：把 Organization 域的**既有读 API**
%%% （`organization_repo:find_by_id/1`）适配成 CS 生命周期事实
%%% `{ok, active | archived}`。本模块：
%%%   * **零 SQL**——org 表的读取真源是 Organization 域 repo（Feature 不自查
%%%     org 表，铁律 5：Feature → Core/legacy 单向消费既有读路径）；
%%%   * **零写**——C16：归档/恢复是 Organization 域 command
%%%     （organization_logic:archive/restore，ORG-02 交付），CS 侧只读消费；
%%%   * **逐请求直读、不缓存**——与 eb_pg_auth_facts 同规：一旦缓存，
%%%     restore 后的放行 / archive 后的拒绝都会退化为「等缓存过期」。
%%%
%%% 状态枚举与 organization.status CHECK（00000095）逐字一致：
%%% `active | archived`；其他历史值（理论不可达）按 fail-closed 归入 archived
%%% 语义之外的 `{error, {unknown_org_status, _}}`，不猜默认放行。
-module(cs_org_lifecycle_facts).

-behaviour(cs_org_lifecycle_port).

-export([status/1]).

%% @doc 读取组织生命周期状态（逐请求、只读）。
%%
%% 返回 `{ok, active} | {ok, archived} | {error, not_found} | {error, term()}`。
%% `not_found` 语义与 Organization 域既有读一致（organization_logic:detail
%% 同款：org 不存在 → not_found，由调用方译为 404 面）。
-spec status(integer()) ->
    {ok, active | archived} | {error, not_found | term()}.
status(OrgId) when is_integer(OrgId), OrgId > 0 ->
    case organization_repo:find_by_id(OrgId) of
        {ok, #{<<"status">> := <<"active">>}} ->
            {ok, active};
        {ok, #{<<"status">> := <<"archived">>}} ->
            {ok, archived};
        {ok, #{<<"status">> := Other}} ->
            {error, {unknown_org_status, Other}};
        {error, not_found} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end;
status(_) ->
    {error, invalid_org_id}.
