%%% @doc 客服**平台运营面**薄 Handler（CS-02，plan §5.3）。
%%%
%%% 依据：plan v4.1 §5.3、§5.4、CS-02-A02、`docs/architecture/feature-slice-rules.md`
%%% 铁律 6（租户作用域显式贯穿）。职责与租户面**完全同构**（解析 → 验证 → 认证 →
%%% 调用 → 映射），差异只在两处：
%%%
%%%   1. **principal 是 `platform_admin`**：凭证是 Admin session（`adm_user_id`，
%%%      adm_auth_middleware 注入），权限是 `customer_service:read|write`（由
%%%      route metadata 决定）；
%%%   2. **租户条件显式且强制**：Org 来自 `:org_id` 或冻结动作声明的显式参数，
%%%      `workspace_id` 是必填参数——不存在「不带 Org 的全局列举」；用例调用与
%%%      租户面共用同一 facade/application（CS-02-A02：**不复制业务逻辑**），SQL 侧仍是
%%%      「第一、二个业务参数 = OrgId/WorkspaceId」。
%%%
%%% **本模块不做**：不读库、不写 SQL、不做业务判定、不缓存事实、不签发任何 URL。
-module(cs_platform_handler).

-export([init/2, handle/3]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    Req = handle(Action, Req0, State0),
    {ok, Req, State0}.

%% @doc 单动作处理（导出以便契约测试直接驱动；生产路径由 init/2 调用）。
-spec handle(atom() | undefined, cowboy_req:req(), map()) -> cowboy_req:req().
handle(Action, Req0, State0) ->
    case cs_actions:platform(Action) of
        {error, {unknown_action, _}} ->
            cs_http:reply_error(Req0, {unknown_action, Action});
        {ok, Entry} ->
            case cs_actions:case_for(Entry, cowboy_req:method(Req0)) of
                {error, method_not_allowed} ->
                    cs_http:reply_error(Req0, method_not_allowed);
                {ok, Case} ->
                    dispatch(Entry, Case, Req0, State0)
            end
    end.

dispatch(Entry, Case, Req0, State0) ->
    case cs_http:read_body(Req0) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, Body} ->
            case cs_http:org_id(Entry, Req0, Body) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, OrgId} ->
                    State = State0#{organization_id => OrgId},
                    authorize(Entry, Case, Req0, Body, State, OrgId)
            end
    end.

authorize(Entry, Case, Req0, Body, State, OrgId) ->
    Metadata = authorize_metadata(Entry, Case, State),
    case cs_auth:authorize(Metadata, Req0, State) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, _AuthContext} ->
            invoke(Entry, Case, Req0, Body, OrgId)
    end.

invoke(Entry, Case, Req0, Body, OrgId) ->
    case cs_http:workspace_id(Req0, Body) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, WorkspaceId} ->
            Derived = #{workspace_id => WorkspaceId, at => now(Case)},
            case cs_http:build_params(Entry, Case, Req0, Body, Derived) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Params} ->
                    Result = cs_facade_call:call(maps:get(facade, Case), OrgId, Params),
                    cs_http:respond(Entry, Req0, Result)
            end
    end.

metadata(State) ->
    Keys = [auth_context, surface, required_function, required_permission, required_governance],
    maps:from_list([{K, maps:get(K, State, undefined)} || K <- Keys, maps:is_key(K, State)]).

authorize_metadata(Entry, Case, State) ->
    Base = metadata(State),
    case maps:get(case_auth, Entry, undefined) of
        CaseAuth when is_map(CaseAuth) ->
            maps:merge(Base, maps:get(maps:get(method, Case), CaseAuth, #{}));
        _ ->
            Base
    end.

now(Case) ->
    case maps:get(clock_unit, Case, millisecond) of
        second -> cs_http:now_sec();
        millisecond -> cs_http:now_ms()
    end.
