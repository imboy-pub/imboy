-module(router_consistency_tests).

-include_lib("eunit/include/eunit.hrl").

route_handler_modules_exist_test() ->
    MissingModules = missing_route_modules(),
    ?assertEqual([], MissingModules).

%% PR-3β: SSE handler 路由必须注册到 v1 路由表
qr_login_subscribe_route_registered_test() ->
    Paths = [unicode:characters_to_binary(P) || {P, _H, _S} <- all_routes()],
    ?assert(
        lists:member(<<"/api/v1/passport/qr_login/subscribe">>, Paths),
        "SSE 路由 /v1/passport/qr_login/subscribe 未在 imboy_router 注册"
    ),
    %% 同时校验 handler 是 qr_login_sse_handler（不是误打到 qr_login_handler）
    [Handler] = [
        H
     || {P, H, _S} <- all_routes(),
        unicode:characters_to_binary(P) =:= <<"/api/v1/passport/qr_login/subscribe">>
    ],
    ?assertEqual(qr_login_sse_handler, Handler).

%% PR-3β: SSE 端点必须在 open 列表（无需登录态，否则 EventSource 跨域无法连）
qr_login_subscribe_route_in_open_list_test() ->
    ?assert(
        lists:member(<<"/api/v1/passport/qr_login/subscribe">>, imboy_router:open()),
        "SSE 路由 /v1/passport/qr_login/subscribe 必须在 open 列表，否则被认证中间件拒绝"
    ).

%% BUG#批次78-1：scan/confirm 是手机端已登录用户调用，handler 强制要求
%% current_uid != 0（qr_login_handler.handle_scan/handle_confirm 的 {0, _} 分支）。
%% 若误入 open 白名单，auth_middleware_api_v1 跳过 token 解析 → current_uid 恒为 0
%% → scan 必返回 401「未登录」→ 客户端 _checkAuthExpired 误判为会话失效
%% 触发 quitLogin + 删除本地数据库。此测试防止该回归。
qr_login_scan_confirm_not_in_open_list_test() ->
    Open = imboy_router:open(),
    MustAuthenticated = [
        <<"/api/v1/passport/qr_login/scan">>,
        <<"/api/v1/passport/qr_login/confirm">>
    ],
    Leakers = [P || P <- MustAuthenticated, lists:member(P, Open)],
    ?assertEqual(
        [],
        Leakers,
        "scan/confirm 不应在 open 白名单：handler 要求 current_uid != 0，"
        "白名单会让 token 解析被跳过导致 401 误判，触发客户端删库（BUG#批次78-1）"
    ).

%% 同批守护：scan/confirm 路由本身仍要注册（只是不进 open 白名单，未删路由）
qr_login_scan_confirm_route_registered_test() ->
    Paths = [unicode:characters_to_binary(P) || {P, _H, _S} <- all_routes()],
    [
        ?assert(
            lists:member(<<"/api/v1/passport/qr_login/scan">>, Paths),
            "scan 路由必须注册（只是不进 open 白名单，不是删除路由）"
        ),
        ?assert(
            lists:member(<<"/api/v1/passport/qr_login/confirm">>, Paths),
            "confirm 路由必须注册（只是不进 open 白名单，不是删除路由）"
        )
    ].

%% DF-20：channel/qrcode 必须注册为专属路由，且排在 /channel/:channel_id
%% 通配之前——否则 "qrcode" 被当作 channel_id 进入 show，
%% 退化为「频道不存在」的误导性业务错误（08-18 起的历史缺陷）。
channel_qrcode_route_registered_before_wildcard_test() ->
    Routes = all_routes(),
    Paths = [unicode:characters_to_binary(P) || {P, _H, _S} <- Routes],
    ?assert(
        lists:member(<<"/api/v1/channel/qrcode">>, Paths),
        "频道二维码路由 /v1/channel/qrcode 未注册（会被 :channel_id 通配捕获）"
    ),
    QrIdx = index_of(<<"/api/v1/channel/qrcode">>, Paths),
    WildIdx = index_of(<<"/api/v1/channel/:channel_id">>, Paths),
    ?assert(
        is_integer(QrIdx) andalso is_integer(WildIdx) andalso QrIdx < WildIdx,
        "channel/qrcode 必须排在 :channel_id 通配之前，否则通配先命中"
    ).

index_of(Value, List) ->
    index_of(Value, List, 1).

index_of(_Value, [], _Idx) ->
    not_found;
index_of(Value, [Value | _Rest], Idx) ->
    Idx;
index_of(Value, [_H | Rest], Idx) ->
    index_of(Value, Rest, Idx + 1).

open_routes_map_to_route_table_test() ->
    RoutePathSet = route_path_set(),
    MissingPaths = missing_paths(imboy_router:open(), RoutePathSet),
    ?assertEqual([], MissingPaths).

option_routes_map_to_route_table_test() ->
    RoutePathSet = route_path_set(),
    MissingPaths = missing_paths(imboy_router:option(), RoutePathSet),
    ?assertEqual([], MissingPaths).

-spec missing_route_modules() -> [atom()].
missing_route_modules() ->
    lists:usort([
        Handler
     || {_Path, Handler, _State} <- all_routes(),
        not handler_exists(Handler)
    ]).

-spec missing_paths([binary()], sets:set(binary())) -> [binary()].
missing_paths(Paths, RoutePathSet) ->
    [
        Path
     || Path <- Paths,
        not sets:is_element(Path, RoutePathSet)
    ].

-spec route_path_set() -> sets:set(binary()).
route_path_set() ->
    Paths = [
        unicode:characters_to_binary(Path)
     || {Path, _Handler, _State} <- all_routes()
    ],
    sets:from_list(Paths).

-spec all_routes() -> [tuple()].
all_routes() ->
    lists:flatmap(
        fun({_Host, Routes}) ->
            Routes
        end,
        imboy_router:get_routes()
    ).

-spec handler_exists(atom()) -> boolean().
handler_exists(cowboy_static) ->
    true;
handler_exists(Handler) when is_atom(Handler) ->
    case code:ensure_loaded(Handler) of
        {module, Handler} ->
            true;
        _ ->
            false
    end.
