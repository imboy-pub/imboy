-module(enterprise_workspace_handler).

%%%
% enterprise_workspace_handler 是 V2.1 Internal 资源只读面 INT-24/25 的
% HTTP 壳（plan §6.1 冻结名 planned -> 落地； Cowboy 路由由 A0 接线，本
% 模块不进 imboy_router——见 SHARED_PATH_PROPOSAL）。
%
% 路由（A0 接线形态）：
%   GET /api/internal/v1/workspaces               -> #{action => workspaces}
%   GET /api/internal/v1/workspaces/:workspace_id -> #{action => workspace}
% —— 必须经 enterprise_internal_middleware（认证链：credential → active
%    → application active → organization active → scope workspaces:read →
%    rate internal_read fail-closed）。只读：无幂等键、无写入。
%
% 边界（§5.2/§6.2）：
%   * INT-24 kind=list：enforce 复验 scope 在生效集；行级「仅 Grant 覆盖 W」
%     收窄在 workspace_repo 的列表 SQL 内（SQL 义务，非内存过滤）；
%   * INT-25 kind=workspace：先 Org 边界定位（跨 O/不存在/归档同体 404，
%     不泄露存在性），再 Grant 覆盖判定（403 organization_boundary_violation）
%     ——deny precedence 第 5/6 步顺序不可倒置。
%
% path 绑定：{workspace_id} 段 cowboy 恒给 binary，本壳按 TSID 数字段
% 收敛为 integer（enterprise_internal_middleware 只白名单 group_id/
% delivery_id，本模块自收敛，不改共享中间件）。
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
            workspaces -> workspaces(Method, Req0, State);
            workspace -> workspace(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc INT-24：Grant 覆盖 W 集合的 active workspace keyset 列表。
%% query：limit（缺省 50，1..100，越界 400 不截断）、cursor（CURSOR-V2）。
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

%% @doc Grant 资源边界接线（唯一接线点 enterprise_internal_boundary:enforce/4；
%% INT-24 是 kind=list（无 W 上下文），INT-25 是 kind=workspace（path W））。
-spec with_boundary(any(), map(), binary(), undefined | integer(), fun()) ->
    {ok, map()} | {error, {binary(), term()}}.
with_boundary(Conn, Ctx, RouteId, WorkspaceId, Fun) ->
    case enterprise_internal_boundary:enforce(Conn, Ctx, RouteId, WorkspaceId) of
        ok ->
            Fun();
        {error, Code} ->
            {error, {enterprise_internal_boundary:error_code(Code), grant_boundary}}
    end.

%% @doc TSID path 绑定段收敛（binary 数字串 → integer；非法 → 0 →
%% logic invalid_request fail-closed）。
-spec binding_tsid(map(), atom()) -> integer().
binding_tsid(State, Key) ->
    case maps:get(Key, State, 0) of
        B when is_binary(B) ->
            case is_all_digits(B) of
                true ->
                    try
                        binary_to_integer(B)
                    catch
                        _:_ -> 0
                    end;
                false ->
                    0
            end;
        I when is_integer(I) ->
            I;
        _ ->
            0
    end.

-spec is_all_digits(binary()) -> boolean().
is_all_digits(<<>>) ->
    false;
is_all_digits(Bin) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Bin)).

%% @doc 列表应答（只读路径；A-R 结构化访问日志——只记 O/App/route/count，
%% 不记逐项 PII / 响应体 / filter 明细）。
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
