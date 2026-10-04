-module(enterprise_channel_handler).

-moduledoc "V2.1 Internal 资源只读面 INT-30/31 的 HTTP 壳 —— Cowboy 路由由 A0 接线，不进 imboy_router。".
%%%
% enterprise_channel_handler 是 V2.1 Internal 资源只读面 INT-30/31 的
% HTTP 壳（plan §6.1 冻结名 planned -> 落地；Cowboy 路由由 A0 接线，本
% 模块不进 imboy_router——见 SHARED_PATH_PROPOSAL）。
%
% 路由（A0 接线形态）：
%   GET /api/internal/v1/channels               -> #{action => channels}
%   GET /api/internal/v1/channels/:channel_id   -> #{action => channel}
% —— 必须经 enterprise_internal_middleware（scope channels:read →
%    rate internal_read fail-closed）。写面 INT-40..42 使用 channels:write、必需幂等键及同事务应用审计。
%
% INT-30 边界（kind=workspace，W 取自 query）：workspace_id **必填**；
% scope=workspace AND status=1 过滤在 repo SQL 内强制。
% INT-31：channel 行 Org 定位（404 同体，个人频道/已停用频道同不可见）
% → Grant 覆盖 channel 所属 W（403）。
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
        case {Action, Method} of
            {channels, <<"POST">>} ->
                enterprise_internal_write_handler:write(
                    Req0,
                    maps:get(enterprise_internal, State),
                    create,
                    0,
                    enterprise_channel_write_logic
                );
            {channel, <<"PATCH">>} ->
                enterprise_internal_write_handler:write(
                    Req0,
                    maps:get(enterprise_internal, State),
                    update,
                    binding_tsid(State, channel_id),
                    enterprise_channel_write_logic
                );
            {channel, <<"DELETE">>} ->
                enterprise_internal_write_handler:write(
                    Req0,
                    maps:get(enterprise_internal, State),
                    archive,
                    binding_tsid(State, channel_id),
                    enterprise_channel_write_logic
                );
            {channels, _} ->
                channels(Method, Req0, State);
            {channel, _} ->
                channel(Method, Req0, State);
            _ ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc INT-30：workspace_id 必填的 workspace 频道 keyset 列表。
%% query：workspace_id（必填）、limit（缺省 50，1..100，越界 400 不截断）、
%% cursor（CURSOR-V2）。
-spec channels(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
channels(<<"GET">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Qs = maps:from_list(cowboy_req:parse_qs(Req0)),
    case list_opts(Qs) of
        {ok, Opts} ->
            Result =
                elib_pg:with_tx(fun(Conn) ->
                    with_boundary(Conn, Ctx, <<"INT-30">>, maps:get(workspace_id, Opts), fun() ->
                        enterprise_channel_logic:list_channels_tx(Conn, Ctx, Opts)
                    end)
                end),
            reply_page_result(Req0, <<"INT-30">>, Ctx, Result);
        {error, invalid_request} ->
            enterprise_internal_error:reply(Req0, <<"invalid_request">>)
    end;
channels(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc INT-31：channel 详情（channel W 边界；deny precedence：404 先于 403）。
-spec channel(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
channel(<<"GET">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    ChannelId = binding_tsid(State, channel_id),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            case enterprise_channel_logic:locate_tx(Conn, Ctx, ChannelId) of
                {ok, Row} ->
                    WsId = maps:get(<<"workspace_id">>, Row),
                    with_boundary(Conn, Ctx, <<"INT-31">>, WsId, fun() ->
                        enterprise_channel_logic:detail_tx(Conn, Ctx, Row)
                    end);
                {error, _} = Err ->
                    Err
            end
        end),
    reply_detail_result(Req0, Result);
channel(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% Helpers
%% ===================================================================

%% @doc 列表参数校验：workspace_id 必填整数；limit 1..100（越界拒绝不截断）。
-spec list_opts(map()) -> {ok, map()} | {error, invalid_request}.
list_opts(Qs) ->
    WsId =
        case elib_tsid:from_binary(maps:get(<<"workspace_id">>, Qs, undefined)) of
            {ok, ParsedId} -> ParsedId;
            error -> invalid
        end,
    case {WsId, enterprise_internal_read_page:parse_limit(Qs)} of
        {W, {ok, Limit}} when is_integer(W), W > 0 ->
            {ok, #{
                workspace_id => W,
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
            ?ERROR_LOG("enterprise_channel_handler list rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_channel_handler list error: ~p~n", [Reason]),
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
            ?ERROR_LOG("enterprise_channel_handler detail rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_channel_handler detail error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).
