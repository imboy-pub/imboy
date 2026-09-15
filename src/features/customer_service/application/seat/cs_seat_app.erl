%%% @doc 客服坐席（seat）的应用层用例：创建 / 开关（suspend）。
%%%
%%% 依据：plan v4.1 §4.2（customer_service_seat）、§5.2、CS-01-A01/A02、EB-D03。
%%%
%%% 职责边界：
%%%   * A01 双重校验的应用侧：创建前先经扩展点读取 identity 的 `function_key`，
%%%     非 `customer_service` 直接拒绝（数据库侧由复合 FK + CHECK 兜底——
%%%     绕过应用也进不来）；不采信调用方自报职能。
%%%   * seat 是 identity 的运营属性：**不含 owner user**；当前用户来自 active
%%%     identity assignment（EB 侧），不在本用例的读写面内（A04 的前提）。
%%%   * 授权（tenant admin / platform admin）由 CS-02 的认证分流判定；
%%%     本模块只判业务前提。
%%%
%%% 数据访问全部经 `cs_store_port`（Params 里 `store` / `id` 键可注入，
%%% 未给则用 `cs_infra_ports` 装配默认）；零 SQL、零 `elib_pg`。
-module(cs_seat_app).

-export([
    create_seat/2,
    suspend_seat/2,
    resume_seat/2,
    fetch_seat/2,
    list_dispatchable_seats/2
]).

%% ===================================================================
%% 创建坐席（A01）
%% ===================================================================

%% @doc 为 customer_service 业务身份开通坐席（PK = business_identity_id，重复开通 conflict）。
%%
%% Params：
%%   workspace_id         必填整数（租户操作范围）
%%   business_identity_id 必填整数；其 function_key 必须是 customer_service（应用侧
%%                        经 store 读取后判定，DB 侧复合 FK 兜底）
%%   max_concurrent       可选正整数（默认 1）
%%   enabled              可选布尔（默认 true）
%%   created_by_user_id   可选（审计快照）
%%   store / id           可选端口覆盖
-spec create_seat(integer(), map()) -> {ok, map()} | {error, term()}.
create_seat(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            create_seat_in(OrgId, WorkspaceId, Params)
    end;
create_seat(_OrgId, _Params) ->
    {error, {invalid_argument, create_seat}}.

create_seat_in(OrgId, WorkspaceId, Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    case pos_int(IdentityId) of
        false ->
            {error, {invalid_identity_id, IdentityId}};
        true ->
            case
                with_store(Params, fun(Store) ->
                    Store:fetch_identity_function(OrgId, IdentityId)
                end)
            of
                {error, not_found} ->
                    {error, {identity_not_found, IdentityId}};
                {error, _} = Err ->
                    Err;
                {ok, <<"customer_service">>} ->
                    insert_seat(OrgId, WorkspaceId, IdentityId, Params);
                {ok, OtherFunction} ->
                    {error, {identity_not_customer_service, IdentityId, OtherFunction}}
            end
    end.

insert_seat(OrgId, WorkspaceId, IdentityId, Params) ->
    Enabled = maps:get(enabled, Params, true),
    MaxConcurrent = maps:get(max_concurrent, Params, 1),
    Seat = #{
        organization_id => OrgId,
        business_identity_id => IdentityId,
        function_key => <<"customer_service">>,
        enabled => Enabled,
        max_concurrent => MaxConcurrent,
        created_by_user_id => maps:get(created_by_user_id, Params, undefined)
    },
    case with_store(Params, fun(Store) -> Store:insert_seat(OrgId, Seat) end) of
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            case
                append_event(Params, OrgId, WorkspaceId, #{
                    business_identity_id => IdentityId,
                    actor_user_id => maps:get(created_by_user_id, Params, undefined),
                    action => <<"seat.created">>,
                    detail => #{
                        <<"max_concurrent">> => MaxConcurrent,
                        <<"enabled">> => Enabled
                    }
                })
            of
                ok -> {ok, Stored#{workspace_id => WorkspaceId}};
                {error, _} = AuditErr -> AuditErr
            end
    end.

%% ===================================================================
%% 坐席开关（suspend / resume）
%% ===================================================================

%% @doc 停用坐席：`enabled=false` 后新 claim 即时被拒；
%% 既有 active 会话不自动迁移（显式 transfer/close 才变化）。
-spec suspend_seat(integer(), map()) -> {ok, map()} | {error, term()}.
suspend_seat(OrgId, Params) ->
    set_enabled(OrgId, Params, false, <<"seat.suspended">>).

%% @doc 恢复坐席。
-spec resume_seat(integer(), map()) -> {ok, map()} | {error, term()}.
resume_seat(OrgId, Params) ->
    set_enabled(OrgId, Params, true, <<"seat.resumed">>).

set_enabled(OrgId, Params, Enabled, Action) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            set_enabled_in(OrgId, WorkspaceId, Enabled, Action, Params)
    end;
set_enabled(_OrgId, _Params, _Enabled, _Action) ->
    {error, {invalid_argument, suspend_seat}}.

set_enabled_in(OrgId, WorkspaceId, Enabled, Action, Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    At = maps:get(at, Params, undefined),
    case pos_int(IdentityId) andalso pos_int(At) of
        false ->
            {error, {invalid_argument, set_seat_enabled}};
        true ->
            case
                with_store(Params, fun(Store) ->
                    Store:set_seat_enabled(OrgId, IdentityId, Enabled, At)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Seat} ->
                    case
                        append_event(Params, OrgId, WorkspaceId, #{
                            business_identity_id => IdentityId,
                            actor_user_id => maps:get(actor_user_id, Params, undefined),
                            action => Action,
                            detail => #{<<"reason">> => maps:get(reason, Params, undefined)}
                        })
                    of
                        ok -> {ok, Seat#{workspace_id => WorkspaceId}};
                        {error, _} = AuditErr -> AuditErr
                    end
            end
    end.

%% ===================================================================
%% 读取
%% ===================================================================

%% @doc 坐席详情。
-spec fetch_seat(integer(), map()) -> {ok, map()} | {error, term()}.
fetch_seat(OrgId, Params) when is_map(Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    case pos_int(IdentityId) of
        false ->
            {error, {invalid_identity_id, IdentityId}};
        true ->
            with_store(Params, fun(Store) -> Store:fetch_seat(OrgId, IdentityId) end)
    end;
fetch_seat(_OrgId, _Params) ->
    {error, {invalid_argument, fetch_seat}}.

%% @doc 派单快照（enabled 坐席 + active 计数；least-active 的输入）。
-spec list_dispatchable_seats(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_dispatchable_seats(OrgId, Params) when is_map(Params) ->
    with_store(Params, fun(Store) -> Store:list_dispatchable_seats(OrgId) end);
list_dispatchable_seats(_OrgId, _Params) ->
    {error, {invalid_argument, list_dispatchable_seats}}.

%% ===================================================================
%% 内部辅助
%% ===================================================================

append_event(Params, OrgId, WorkspaceId, Event) ->
    cs_app_support:append_event(Params, OrgId, Event#{workspace_id => WorkspaceId}).

with_store(Params, Fun) ->
    cs_app_support:with_store(Params, Fun).

pos_int(V) ->
    cs_app_support:pos_int(V).
