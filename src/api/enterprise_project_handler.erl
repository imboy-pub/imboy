-module(enterprise_project_handler).

-moduledoc "V2.1 Internal 资源只读面 INT-28/29 的 HTTP 壳 —— Cowboy 路由由 A0 接线，不进 imboy_router。".
%%%
% enterprise_project_handler 是 V2.1 Internal 资源只读面 INT-28/29 的
% HTTP 壳（plan §6.1 冻结名 planned -> 落地；Cowboy 路由由 A0 接线，本
% 模块不进 imboy_router——见 SHARED_PATH_PROPOSAL）。
%
% 路由（A0 接线形态）：
%   GET /api/internal/v1/projects               -> #{action => projects}
%   GET /api/internal/v1/projects/:project_id   -> #{action => project}
% —— 必须经 enterprise_internal_middleware（scope projects:read →
%    rate internal_read fail-closed）。只读：无幂等键、无写入。
%
% INT-28 边界（kind=workspace，W 取自 query）：workspace_id **必填**——
% 缺失/非整数 400 invalid_request（fail-closed，不用缺省 W 放大可见域）；
% W 的 Grant 覆盖判定在参数校验之后、列表执行之前（enforce INT-28）。
% INT-29：project 行 Org 定位（404 同体）→ Grant 覆盖 project 所属 W（403）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            projects -> projects(Method, Req0, State);
            project -> project(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc INT-28：workspace_id 必填的 project keyset 列表。
%% query：workspace_id（必填）、status（active|done|all，缺省 all）、
%% limit（缺省 50，1..100，越界 400 不截断）、cursor（CURSOR-V2）。
-spec projects(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
projects(<<"GET">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Qs = maps:from_list(cowboy_req:parse_qs(Req0)),
    case list_opts(Qs) of
        {ok, Opts} ->
            Result =
                elib_pg:with_tx(fun(Conn) ->
                    %% kind=workspace：W 取自必填 query 过滤参数（§6.2/边界
                    %% moduledoc；缺 W 上下文对 workspace 类路由 fail-closed）。
                    with_boundary(Conn, Ctx, <<"INT-28">>, maps:get(workspace_id, Opts), fun() ->
                        enterprise_project_logic:list_projects_tx(Conn, Ctx, Opts)
                    end)
                end),
            reply_page_result(Req0, <<"INT-28">>, Ctx, Result);
        {error, invalid_request} ->
            enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
projects(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc INT-29：project 详情（project W 边界；deny precedence：404 先于 403）。
-spec project(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
project(<<"GET">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    ProjectId = binding_tsid(State, project_id),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            case enterprise_project_logic:locate_tx(Conn, Ctx, ProjectId) of
                {ok, Row} ->
                    WsId = maps:get(<<"workspace_id">>, Row),
                    with_boundary(Conn, Ctx, <<"INT-29">>, WsId, fun() ->
                        enterprise_project_logic:detail_tx(Conn, Ctx, Row)
                    end);
                {error, _} = Err ->
                    Err
            end
        end),
    reply_detail_result(Req0, Result);
project(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% Helpers
%% ===================================================================

%% @doc 列表参数校验：workspace_id 必填整数；status ∈ active|done|all；
%% limit 1..100（越界拒绝不截断）。
-spec list_opts(map()) -> {ok, map()} | {error, invalid_request}.
list_opts(Qs) ->
    WsId =
        case elib_tsid:from_binary(maps:get(<<"workspace_id">>, Qs, undefined)) of
            {ok, ParsedId} -> ParsedId;
            error -> invalid
        end,
    Status =
        case maps:get(<<"status">>, Qs, undefined) of
            undefined -> all;
            <<"active">> -> <<"active">>;
            <<"done">> -> <<"done">>;
            <<"all">> -> all;
            _OtherPresent -> invalid
        end,
    case {WsId, Status, enterprise_internal_read_page:parse_limit(Qs)} of
        {W, S, {ok, Limit}} when is_integer(W), W > 0, S =/= invalid ->
            {ok, #{
                workspace_id => W,
                status => S,
                limit => Limit,
                cursor => enterprise_internal_read_page:parse_cursor(Qs)
            }};
        _ ->
            {error, invalid_request}
    end.

-spec with_boundary(any(), map(), binary(), undefined | integer(), fun()) ->
    {ok, map()} | {error, {binary(), term()}}.
with_boundary(Conn, Ctx, RouteId, WorkspaceId, Fun) ->
    case enterprise_internal_boundary:enforce(Conn, Ctx, RouteId, WorkspaceId) of
        ok ->
            Fun();
        {error, Code} ->
            {error, {enterprise_internal_boundary:error_code(Code), grant_boundary}}
    end.

-spec binding_tsid(map(), atom()) -> integer().
binding_tsid(State, Key) ->
    case maps:get(Key, State, 0) of
        Id when is_integer(Id), Id > 0, Id =< 9223372036854775807 -> Id;
        Value ->
            case elib_tsid:from_binary(Value) of
                {ok, Id} -> Id;
                error -> 0
            end
    end.

-spec reply_page_result(cowboy_req:req(), binary(), map(), term()) -> cowboy_req:req().
reply_page_result(Req0, RouteId, Ctx, Result) ->
    case Result of
        {ok, #{<<"items">> := Items} = Page} ->
            ?INFO_LOG([
                enterprise_internal_read,
                #{
                    route => RouteId,
                    organization_id => maps:get(organization_id, Ctx, undefined),
                    application_id => maps:get(application_id, Ctx, undefined),
                    count => length(Items)
                }
            ]),
            reply_json(Req0, 200, Page);
        {error, {Code, _Detail}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_project_handler list rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_project_handler list error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

-spec reply_detail_result(cowboy_req:req(), term()) -> cowboy_req:req().
reply_detail_result(Req0, Result) ->
    case Result of
        {ok, Detail} ->
            reply_json(Req0, 200, Detail);
        {error, {Code, _Detail}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_project_handler detail rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_project_handler detail error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).
