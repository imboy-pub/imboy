%%% @doc 客服 application 共用的编排辅助（租户门 / 端口注入 / 事件追加）。
%%%
%%% 只做机械动作，不含业务规则：
%%%   * `tenant/2`：OrgId / workspace_id 形状门（不触库）；
%%%   * `with_store/2` / `new_id/2`：端口解析——`Params` 里同键（`store` / `id`）
%%%     可注入覆盖（测试用），未给则用 `cs_infra_ports` 装配默认；
%%%   * `append_event/3`：客服域 append-only 审计；失败显式返回
%%%     `{error, {audit_append_failed, _}}`（审计丢失不得静默）。
-module(cs_app_support).

-export([
    tenant/2,
    with_store/2,
    with_id/2,
    new_id/2,
    append_event/3,
    pos_int/1,
    non_empty_binary/1
]).

%% @doc 租户门：OrgId / workspace_id 必须都是正整数。
-spec tenant(term(), map()) -> {ok, integer()} | {error, term()}.
tenant(OrgId, Params) when is_map(Params) ->
    case pos_int(OrgId) of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case maps:get(workspace_id, Params, undefined) of
                Ws when is_integer(Ws), Ws > 0 -> {ok, Ws};
                Other -> {error, {invalid_workspace_id, Other}}
            end
    end;
tenant(OrgId, _Params) ->
    {error, {invalid_organization_id, OrgId}}.

%% @doc 解析 store 端口并执行；`Params` 里 `store` 键可注入覆盖。
-spec with_store(map(), fun((module()) -> T)) -> T | {error, term()}.
with_store(Params, Fun) ->
    case port(store, Params) of
        {ok, Store} -> Fun(Store);
        {error, _} = Err -> Err
    end.

%% @doc 解析 id 端口并执行；`Params` 里 `id` 键可注入覆盖。
-spec with_id(map(), fun((module()) -> T)) -> T | {error, term()}.
with_id(Params, Fun) ->
    case port(id, Params) of
        {ok, IdPort} -> Fun(IdPort);
        {error, _} = Err -> Err
    end.

%% @doc 生成一个 TSID（Kind 分域）；失败显式报错，不静默兜底。
-spec new_id(atom(), map()) -> {ok, integer()} | {error, term()}.
new_id(Kind, Params) ->
    with_id(Params, fun(IdPort) ->
        try
            {ok, IdPort:new_id(Kind)}
        catch
            Class:Reason -> {error, {id_generation_failed, Kind, {Class, Reason}}}
        end
    end).

%% @doc 追加客服状态审计事件（append-only）。
-spec append_event(map(), integer(), map()) -> ok | {error, term()}.
append_event(Params, OrgId, Event) when is_map(Event) ->
    case with_store(Params, fun(Store) -> Store:append_event(OrgId, Event) end) of
        {ok, _EventId} -> ok;
        {error, Reason} -> {error, {audit_append_failed, Reason}}
    end;
append_event(_Params, _OrgId, _Event) ->
    {error, {invalid_argument, append_event}}.

port(Key, Params) ->
    %% undefined 也是 atom：maps:get 的默认值不得被 is_atom 守卫当作注入模块放行。
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> cs_infra_ports:resolve(Key)
    end.

-spec pos_int(term()) -> boolean().
pos_int(V) when is_integer(V), V > 0 -> true;
pos_int(_) -> false.

-spec non_empty_binary(term()) -> boolean().
non_empty_binary(B) when is_binary(B), B =/= <<>> -> true;
non_empty_binary(_) -> false.
