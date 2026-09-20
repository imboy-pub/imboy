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
    Metadata = authorize_metadata(Entry, Case, State),
    case authorizer(Entry, Metadata, Req0, State) of
        {error, Reason} ->
            cs_http:reply_error(Req0, Reason);
        {ok, AuthContext} ->
            invoke(Entry, Case, Req0, Body, OrgId, AuthContext)
    end.

%% BE-S01a：`org_source = self` 的主体自身作用域用例（坐席上下文清单）。
%% 五类 principal 的凭证类别语义不变（cs_seat = IMBoy JWT），但**不**做
%% org 级 member/assignment/seat 判定——清单的意义正是枚举这些事实，各 Org
%% 的复核由 application 聚合时逐 Org 下推（store SQL 同语句过滤 active
%% member / active assignment / seat enabled）。缺 JWT 会话键即 401。
authorizer(Entry, Metadata, Req, State) ->
    case cs_actions:org_source(Entry) of
        self ->
            case maps:get(current_uid, State, 0) of
                Uid when is_integer(Uid), Uid > 0 ->
                    {ok, #{auth_context => cs_seat, user_id => Uid}};
                _ ->
                    {error, credential_missing}
            end;
        _ ->
            cs_auth:authorize(Metadata, Req, State)
    end.

%% route metadata（auth_context 等）+ 动作表 case_auth 覆盖：同一 cowboy 路径
%% 的 method+auth_context 分流（CSB-02R：GET /sessions/queue 的坐席语义）。
%% case_auth 只能**收窄**到五类 principal 内的声明（cs_route_contract_tests
%% 审计其方法/主体合法性）；未覆盖的方法沿用 route metadata 主体。
authorize_metadata(Entry, Case, State) ->
    Base = metadata(State),
    case maps:get(case_auth, Entry, undefined) of
        CaseAuth when is_map(CaseAuth) ->
            Override = maps:get(maps:get(method, Case), CaseAuth, #{}),
            maps:merge(Base, Override);
        _ ->
            Base
    end.

invoke(Entry, Case, Req0, Body, OrgId, AuthContext) ->
    case workspace_gate(Case, Req0, Body) of
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

%% workspace 门（CSB-02R）：缺省 required（既有口径不变）；动作表声明
%% `workspace => optional` 的用例缺失不 422（坐席 org-wide 列表的作用域由
%% Org/assignment 决定，显式给出才收窄）。
workspace_gate(Case, Req, Body) ->
    case maps:get(workspace, Case, required) of
        optional ->
            case cs_http:workspace_id(Req, Body) of
                {error, missing_workspace_id} -> {ok, undefined};
                Other -> Other
            end;
        required ->
            cs_http:workspace_id(Req, Body)
    end.

%% route metadata：只取白名单键 + 认证需要的装配/会话键走 State 本体。
metadata(State) ->
    Keys = [auth_context, surface, required_function, required_permission, required_governance],
    maps:from_list([{K, maps:get(K, State, undefined)} || K <- Keys, maps:is_key(K, State)]).

%% 服务端派生参数：操作人/时钟/主体身份全部来自认证上下文与服务端时钟，
%% 客户端无法自报（动作表已把这些键列为 client_forbidden，400 兜底）。
derived_params(AuthContext, WorkspaceId) ->
    %% CSB-02R：optional workspace **缺省时键不存在**（不是值为 undefined 的
    %% 键）——「未提供」与「提供了 undefined」是两种形状，facade/application
    %% 与消费方只见前者；此处是 optional 派生键的唯一归一点。
    Base0 = #{
        at => cs_http:now_ms(),
        actor_user_id => actor_user_id(AuthContext)
    },
    Base =
        case WorkspaceId of
            Ws when is_integer(Ws), Ws > 0 -> Base0#{workspace_id => Ws};
            _ -> Base0
        end,
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
