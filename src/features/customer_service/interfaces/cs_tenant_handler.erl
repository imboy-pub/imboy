%%% @doc 客服**租户面**薄 Handler（CS-02，plan §5.2）。
%%%
%%% 依据：plan v4.1 §5.2、§5.4、CS-02-A01/A02/A04、`docs/architecture/
%%% feature-slice-rules.md` 铁律 2/3/5。与 `eb_tenant_handler` 同职责同顺序：
%%%
%%%   1. **解析**：路径绑定 + JSON 正文 + OrgId（path 或申报参数，见
%%%      `cs_actions:org_source/1`）+ `workspace_id`；
%%%   2. **验证**：方法门（405）、动作表白名单投影（表外键忽略、服务端派生键
%%%      400、缺必填 422——`client_msg_id`/`key_ref` 等 application 的无默认
%%%      maps:get 键在到达 application 之前必被结构化校验，绝不 badarg 500）；
%%%   3. **认证**：`cs_auth:authorize/3`（五类身份由 route metadata 决定；
%%%      suspended seat actor 即时拒绝，CS-02-A04）；
%%%   4. **调用 + 映射**：`cs_facade_call:call/3`（只进 facade），结果 → HTTP
%%%      （200 / 400 / 401 / 403 / 404 / 405 / 409 / 422 / 500；offboarding 降级
%%%      = 409 + envelope `offboarding_required`）。
%%%
%%% 平台运营面不在本模块（见 `cs_platform_handler`）；两面共用同一 application，
%%% 接口层不复制业务逻辑（CS-02-A02）。
%%%
%%% **本模块不做**：不读库、不写 SQL、不做业务判定、不拼 SQL、不缓存事实。
-module(cs_tenant_handler).

-export([init/2, handle/3]).

%% cowboy 普通 handler：State = route Opts（含 route metadata + 中间件会话键）。
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    Req = handle(Action, Req0, State0),
    {ok, Req, State0}.

%% @doc 单动作处理（导出以便契约测试直接驱动；生产路径由 init/2 调用）。
-spec handle(atom() | undefined, cowboy_req:req(), map()) -> cowboy_req:req().
handle(Action, Req0, State0) ->
    case cs_actions:tenant(Action) of
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
    Metadata = metadata(State),
    case cs_auth:authorize(Metadata, Req0, State) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, AuthContext} ->
            invoke(Entry, Case, Req0, Body, OrgId, AuthContext)
    end.

invoke(Entry, Case, Req0, Body, OrgId, AuthContext) ->
    case cs_http:workspace_id(Req0, Body) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, WorkspaceId} ->
            Derived = derived_params(AuthContext, WorkspaceId),
            case cs_http:build_params(Entry, Case, Req0, Body, Derived) of
                {error, Reason} ->
                    cs_http:reply_error(Req0, Reason);
                {ok, Params} ->
                    Result = cs_facade_call:call(maps:get(facade, Case), OrgId, Params),
                    cs_http:respond(Entry, Req0, Result)
            end
    end.

%% route metadata：只取白名单键 + 认证需要的装配/会话键走 State 本体。
metadata(State) ->
    Keys = [auth_context, surface, required_function, required_permission, required_governance],
    maps:from_list([{K, maps:get(K, State, undefined)} || K <- Keys, maps:is_key(K, State)]).

%% 服务端派生参数：操作人/时钟/主体身份全部来自认证上下文与服务端时钟，
%% 客户端无法自报（动作表已把这些键列为 client_forbidden，400 兜底）。
derived_params(AuthContext, WorkspaceId) ->
    Base = #{
        workspace_id => WorkspaceId,
        at => cs_http:now_ms(),
        actor_user_id => actor_user_id(AuthContext)
    },
    maps:merge(Base, identity_derived(AuthContext)).

actor_user_id(#{user_id := Uid}) when is_integer(Uid) -> Uid;
actor_user_id(_Other) -> undefined.

identity_derived(#{auth_context := cs_seat, business_identity_id := Bid}) ->
    #{business_identity_id => Bid};
identity_derived(#{auth_context := cs_visit, contact_id := Cid}) ->
    #{contact_id => Cid};
identity_derived(#{auth_context := enterprise_owner_admin, user_id := Uid}) when
    is_integer(Uid)
->
    #{created_by_user_id => Uid};
identity_derived(_Other) ->
    #{}.
