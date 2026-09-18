%%% @doc 客服基础设施的端口装配映射（Port → 具体实现；镜像 `eb_infra_ports`）。
%%%
%%% 只回答「这个端口当前由哪个模块实现」；不做 I/O，不含业务规则。
%%% application 层经 `resolve/1` 取默认实现；`Params` 同键注入可覆盖
%%% （`cs_app_support:port/2`）。
-module(cs_infra_ports).

-export([
    store/0,
    id/0,
    org_lifecycle/0,
    resolve/1,
    implementations/0
]).

-type implementation() :: module().

-export_type([implementation/0]).

%% @doc 持久化读写端口实现。
-spec store() -> implementation().
store() -> cs_pg_store.

%% @doc 注入 ID 端口实现（TSID）。
-spec id() -> implementation().
id() -> cs_tsid.

%% @doc Organization 生命周期事实端口实现（只读 adapter，ORG-08）。
-spec org_lifecycle() -> implementation().
org_lifecycle() -> cs_org_lifecycle_facts.

%% @doc 端口 → 已装配实现；未装配的端口显式失败（不返回空实现）。
-spec resolve(term()) -> {ok, implementation()} | {error, term()}.
resolve(Port) ->
    case lists:keyfind(Port, 1, by_key()) of
        {Port, Impl} ->
            {ok, Impl};
        false ->
            case lists:member(Port, cs_ports:all()) of
                true -> {error, {unimplemented_port, Port}};
                false -> {error, {unknown_port, Port}}
            end
    end.

%% @doc 已装配的 (契约模块, 实现模块) 列表（契约一致性测试用）。
-spec implementations() -> [{module(), implementation()}].
implementations() ->
    [
        {cs_ports:store(), store()},
        {cs_ports:id(), id()},
        {cs_ports:org_lifecycle(), org_lifecycle()}
    ].

by_key() ->
    [{store, store()}, {id, id()}, {org_lifecycle, org_lifecycle()}].
