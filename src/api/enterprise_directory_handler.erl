-module(enterprise_directory_handler).

%%%
% enterprise_directory_handler 是**受限 cursor directory** 端点壳
% （FULL-02；待 A0 接线 Router/manifest）。
%
%% 路由（建议形态，见 checkpoint「A0 需要接线的路由清单」）：
%   POST /api/internal/v1/identity-mappings/directory -> #{action => mappings}
%   POST /api/internal/v1/directory/users             -> #{action => users}
%% —— 必须经 enterprise_internal_middleware（A2 认证链：credential →
%%    active/expiry → application active → organization active →
%%    scope identities:read → rate internal_read fail-closed）；
%%    ctx 注入 handler_opts.enterprise_internal（atom 键 map）。
%
%% 两条路由都是**只读**（manifest 口径 idempotency=not_required）：
%   无幂等键、无写入、无事务副作用（除聚合计量 directory.page）。
%   分页上限由 enterprise_directory_logic 强制（page_size ∈ [1,100]，缺省 50；
%%  超出即 invalid_request，绝不静默截断）；cursor 不透明、按页族校验。
%%  **不存在** list-all / export 形态：调用方只能按 cursor 逐页取。
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
            mappings -> mappings(Method, Req0, State);
            users -> users(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc INT-16 映射目录（等价于 enterprise_identity_handler 的 directory action；
%% 两条路由指向同一逻辑，Router 侧任选其一登记即可）。
-spec mappings(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
mappings(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    run(Req0, Ctx, <<"INT-16">>, fun(Conn) ->
        enterprise_directory_logic:page_mappings_tx(Conn, Ctx, page_opts(Params))
    end);
mappings(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc INT-17 成员目录（最小字段 + 可选 workspace 过滤）。
-spec users(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
users(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    run(Req0, Ctx, <<"INT-17">>, fun(Conn) ->
        enterprise_directory_logic:page_users_tx(Conn, Ctx, page_opts(Params))
    end);
users(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 只读路径：单事务；先做 Grant 资源边界（org 级；workspace 过滤由 logic
%% 在同一事务内追加判定），再执行分页查询。
-spec run(cowboy_req:req(), map(), binary(), fun((any()) -> term())) -> cowboy_req:req().
run(Req0, Ctx, RouteId, Fun) ->
    Opts = page_opts(elib_param:post(Req0)),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_boundary:enforce(
                    Conn, Ctx, RouteId, maps:get(workspace_id, Opts, undefined)
                )
            of
                ok ->
                    Fun(Conn);
                {error, Code} ->
                    {error, {enterprise_internal_boundary:error_code(Code), grant_boundary}}
            end
        end),
    case Result of
        {ok, Page} ->
            reply_json(Req0, 200, Page);
        {error, {Code, _Detail}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_directory_handler rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_directory_handler error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% @doc 分页入参（只取契约内已知键；未知键丢弃）。
-spec page_opts(map()) -> map().
page_opts(Params) when is_map(Params) ->
    Opts0 = #{
        cursor => maps:get(<<"cursor">>, Params, undefined),
        page_size => maps:get(<<"page_size">>, Params, undefined)
    },
    case maps:get(<<"workspace_id">>, Params, undefined) of
        undefined -> Opts0;
        WsId -> Opts0#{workspace_id => WsId}
    end;
page_opts(_Params) ->
    #{}.

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).
