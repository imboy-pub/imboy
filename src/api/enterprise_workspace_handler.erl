-module(enterprise_workspace_handler).

-moduledoc "V2.1 Internal 资源只读面 INT-24/25 的 HTTP 壳 —— Cowboy 路由由 A0 接线，不进 imboy_router。".

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
        case {Action, Method} of
            {workspaces, <<"POST">>} ->
                enterprise_internal_write_handler:write(
                    Req0,
                    maps:get(enterprise_internal, State),
                    create,
                    0,
                    enterprise_workspace_write_logic
                );
            {workspace, <<"PATCH">>} ->
                enterprise_internal_write_handler:write(
                    Req0,
                    maps:get(enterprise_internal, State),
                    update,
                    binding_tsid(State, workspace_id),
                    enterprise_workspace_write_logic
                );
            {workspace, <<"DELETE">>} ->
                enterprise_internal_write_handler:write(
                    Req0,
                    maps:get(enterprise_internal, State),
                    archive,
                    binding_tsid(State, workspace_id),
                    enterprise_workspace_write_logic
                );
            {workspaces, _} ->
                workspaces(Method, Req0, State);
            {workspace, _} ->
                workspace(Method, Req0, State);
            _ ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec workspaces(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
workspaces(<<"GET">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Qs = maps:from_list(cowboy_req:parse_qs(Req0)),
    case enterprise_internal_read_page:parse_limit(Qs) of
        {ok, Limit} ->
            Opts = #{
                limit => Limit,
                cursor => enterprise_internal_read_page:parse_cursor(Qs)
            },
            Result =
                elib_pg:with_tx(fun(Conn) ->
                    with_boundary(Conn, Ctx, <<"INT-24">>, undefined, fun() ->
                        enterprise_workspace_logic:list_workspaces_tx(Conn, Ctx, Opts)
                    end)
                end),
            reply_page_result(Req0, <<"INT-24">>, Ctx, Result);
        {error, invalid_request} ->
            enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
workspaces(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc INT-25：workspace 详情（path W 边界；deny precedence：404 先于 403）。
-spec workspace(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
workspace(<<"GET">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    WsId = binding_tsid(State, workspace_id),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            case enterprise_workspace_logic:locate_active_tx(Conn, Ctx, WsId) of
                {ok, Row} ->
                    with_boundary(Conn, Ctx, <<"INT-25">>, WsId, fun() ->
                        enterprise_workspace_logic:detail_tx(Conn, Ctx, Row)
                    end);
                {error, _} = Err ->
                    Err
            end
        end),
    reply_detail_result(Req0, Result);
workspace(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% Helpers
%% ===================================================================

-spec with_boundary(any(), map(), binary(), undefined | integer(), fun()) ->
    {ok, map()} | {error, {binary(), term()}}.
with_boundary(Conn, Ctx, RouteId, WorkspaceId, Fun) ->
    case enterprise_internal_boundary:enforce(Conn, Ctx, RouteId, WorkspaceId) of
        ok ->
            Fun();
        {error, Code} ->
            {error, {enterprise_internal_boundary:error_code(Code), resource_attribution}}
    end.

%% @doc TSID path 绑定段收敛（binary 数字串 → integer；非法 → 0 →
%% logic invalid_request fail-closed）。
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

%% @doc 列表应答：访问日志只记组织、应用、路由和数量，不记 PII 或响应体。
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
            ?ERROR_LOG("enterprise_workspace_handler list rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_workspace_handler list error: ~p~n", [Reason]),
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
            ?ERROR_LOG("enterprise_workspace_handler detail rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_workspace_handler detail error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).
