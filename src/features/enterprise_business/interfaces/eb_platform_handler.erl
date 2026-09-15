%%% @doc 企业**平台运营面**薄 Handler（EB-09，plan §5.3）。
%%%
%%% 依据：plan v4.1 §5.3、§5.4、EB-09-A01/A03/A04/A05、`docs/architecture/
%%% feature-slice-rules.md` 铁律 6（租户作用域显式贯穿）。
%%%
%%% 职责与租户面**完全同构**（解析 → 验证 → 授权 → 调用 → 映射），差异只在两处：
%%%
%%%   1. **principal 是 `platform_admin`**：凭证是 Admin session（`adm_user_id`），
%%%      权限是 `enterprise_business:read|write`（由 route metadata 决定）；企业身份
%%%      没有路径可以越权到 `/api/adm`（`eb_auth_principal` 的面相容矩阵 fail-closed）。
%%%   2. **租户条件显式且强制（A04）**：每个平台动作的 path 都带 `org_id`，并且
%%%      `workspace_id` 也是必填参数——不存在「不带 Org 的全局列举」，也**不**允许
%%%      「先查资源再比对归属」。用例调用与租户面共用同一 facade，因此 SQL 侧仍是
%%%      EB-03/EB-06 冻结的「第一、二个业务参数 = OrgId/WorkspaceId、同语句带两者」。
%%%
%%% 与租户面共用 application 层（**不复制业务逻辑**）：本模块只多一个显式参数
%%% 「被代理的组织责任人」`actor_user_id`（纠错写动作与取流的必填项）——因为平台
%%% 管理员不是 Org 成员，而 Core/asset 的 actor 契约要求 actor 归属本 Org。
%%%
%%% **本模块不做**：不读库、不写 SQL、不做业务判定、不缓存事实、不签发任何 URL。
-module(eb_platform_handler).

-export([init/2, handle/3]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, undefined),
    Req = handle(Action, Req0, State0),
    {ok, Req, State0}.

%% @doc 单动作处理（导出以便契约测试直接驱动；生产路径由 `init/2` 调用）。
-spec handle(atom() | undefined, cowboy_req:req(), map()) -> cowboy_req:req().
handle(Action, Req0, State0) ->
    case eb_enterprise_actions:platform(Action) of
        {error, {unknown_action, _}} ->
            eb_enterprise_http:reply_error(Req0, {unknown_action, Action});
        {ok, Entry} ->
            case eb_enterprise_actions:case_for(Entry, cowboy_req:method(Req0)) of
                {error, method_not_allowed} ->
                    eb_enterprise_http:reply_error(Req0, method_not_allowed);
                {ok, Case} ->
                    dispatch(Entry, Case, Req0, State0)
            end
    end.

dispatch(Entry, Case, Req0, State0) ->
    case eb_enterprise_http:org_id(Req0) of
        {error, Reason} ->
            eb_enterprise_http:reply_error(Req0, Reason);
        {ok, OrgId} ->
            State = State0#{organization_id => OrgId},
            case eb_enterprise_http:authorize(Entry, Req0, State) of
                {error, Reason} ->
                    eb_enterprise_http:reply_error(Req0, Reason);
                {ok, _AuthContext} ->
                    invoke(Entry, Case, Req0, State, OrgId)
            end
    end.

invoke(Entry, Case, Req0, _State, OrgId) ->
    case eb_enterprise_http:read_body(Req0) of
        {error, Reason} ->
            eb_enterprise_http:reply_error(Req0, Reason);
        {ok, Body} ->
            case eb_enterprise_http:workspace_id(Req0, Body) of
                {error, Reason} ->
                    eb_enterprise_http:reply_error(Req0, Reason);
                {ok, WorkspaceId} ->
                    case
                        eb_enterprise_http:build_params(Entry, Case, Req0, Body, #{
                            organization_id => OrgId,
                            workspace_id => WorkspaceId,
                            %% 平台面：actor 来自**显式请求参数**（被代理的组织责任人），
                            %% 由动作表登记为 required；只读动作不带 actor 也是合法的。
                            actor_user_id => undefined
                        })
                    of
                        {error, Reason} ->
                            eb_enterprise_http:reply_error(Req0, Reason);
                        {ok, Params} ->
                            Result = eb_enterprise_facade_call:call(
                                maps:get(facade, Case), OrgId, Params
                            ),
                            eb_enterprise_http:respond(Entry, Req0, Result)
                    end
            end
    end.
