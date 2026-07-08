-module(throttle_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc throttle_middleware 模块测试

%% @doc passport 路径命中专用宽松限流（非完全豁免，GAP-09），需要
%% mock elib_req:get_client_ip + throttle:check(passport_per_ip, _)，
%% 否则会真的调用 cowboy_req:header/3 在 fake_req 上崩溃。
whitelist_passport_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/passport/login">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"203.0.113.1">> end}
            ]},
            {throttle, [
                {'check', 2, fun(passport_per_ip, _Key) -> {ok, 9, 60000} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc /api/v1/init、/api/v1/ws 完全豁免限流（v0 裸路径已下架，不再测试）
whitelist_init_ws_test_() ->
    Paths = [<<"/api/v1/init">>, <<"/api/v1/ws">>],
    [
        ?WITH_MECKS(
            [
                {cowboy_req, [
                    {'path', 1, fun(_Req) -> Path end}
                ]}
            ],
            fun() ->
                Req = fake_req,
                Env = #{handler_opts => #{}},
                Result = throttle_middleware:execute(Req, Env),
                ?assertMatch({ok, _, _}, Result)
            end
        )
     || Path <- Paths
    ].

whitelist_health_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/health">> end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

whitelist_static_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/static/js/app.js">> end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc 正常请求通过（已认证用户，基于 UID 限流）
normal_uid_request_pass_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/user/info">> end}
            ]},
            {throttle, [
                {'check', 2, fun(api_per_user, _Key) -> {ok, 119, 60000} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{current_uid => 12345}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc 正常请求通过（未认证用户，基于 IP 限流）
normal_ip_request_pass_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/app_version/check">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"192.168.1.1">> end}
            ]},
            {throttle, [
                {'check', 2, fun(api_per_ip, _Key) -> {ok, 59, 60000} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc 超限返回 429（已认证用户）
uid_rate_limit_exceeded_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/user/info">> end},
                {'reply', 4, fun(429, _Headers, _Body, Req) -> Req end}
            ]},
            {throttle, [
                {'check', 2, fun(api_per_user, _Key) -> {limit_exceeded, 0, 5000} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{current_uid => 12345}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result)
        end
    ).

%% @doc 超限返回 429（未认证用户，基于 IP）
ip_rate_limit_exceeded_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/app_version/check">> end},
                {'reply', 4, fun(429, _Headers, _Body, Req) -> Req end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"10.0.0.1">> end}
            ]},
            {throttle, [
                {'check', 2, fun(api_per_ip, _Key) -> {limit_exceeded, 0, 5000} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result)
        end
    ).

%% @doc UID vs IP 限流选择 - 有 current_uid 走 UID 限流
uid_vs_ip_selection_uid_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/msg/send">> end}
            ]},
            {throttle, [
                {'check', 2, fun(Scope, _Key) ->
                    %% 确保使用的是 api_per_user scope
                    case Scope of
                        api_per_user -> {ok, 100, 60000};
                        api_per_ip -> {limit_exceeded, 0, 5000}
                    end
                end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{current_uid => 99}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc UID vs IP 限流选择 - 无 current_uid 走 IP 限流
uid_vs_ip_selection_ip_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/open/resource">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"172.16.0.1">> end}
            ]},
            {throttle, [
                {'check', 2, fun(Scope, _Key) ->
                    %% 确保使用的是 api_per_ip scope
                    case Scope of
                        api_per_ip -> {ok, 50, 60000};
                        api_per_user -> {limit_exceeded, 0, 5000}
                    end
                end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).
