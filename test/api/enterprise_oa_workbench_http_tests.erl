-module(enterprise_oa_workbench_http_tests).

%%%
% OA 工作台 entries 真链路接线测试（真 cowboy listener + 真中间件链 + 真
% handler，不 mock cowboy_req）。
%
% 背景（2026-10-12 prod 事故）：entries handler 曾调用不存在的
% cowboy_req:qs/2（undef），eunit mock 了 cowboy 层而全绿，prod 全量 entries
% 请求 500（客户端表现为 app_not_configured）。本套件把「请求到达 handler
% 并正常回包」钉进真 HTTP 轨道——handler 内任何 cowboy_req 调用面错误
% （undef/坏返回形状）都会以 500 暴露并被断言拦截。
%
% mock 面（刻意最小化，仅认证与数据源）：
%   - auth_ds（verify_sign/condition 直通、current_uid → 测试 uid）
%   - enterprise_oa_sso_logic:entries/1（返回合成条目；logic 自身语义由
%     enterprise_oa_sso_tests 覆盖）
% 其余全真：路由编译、中间件链顺序、handler 分派、信封构造
% （elib_response:success 的 {code,msg,sv_ts,payload} 协议）。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, enterprise_oa_workbench_http_test_listener).
-define(TEST_UID, 995017).

%% 与 imboy_app.erl 的同序中间件链（auth_middleware 必须在 cowboy_router 之后）
-define(MIDDLEWARES, [
    cowboy_router,
    cors_middleware,
    security_headers_middleware,
    auth_middleware,
    feature_gate_middleware,
    throttle_middleware,
    cowboy_handler
]).

http_entries_test_() ->
    {timeout, 60, fun http_entries/0}.

http_entries() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    %% passthrough：auth_ds 是真实模块，未 mock 的函数（如
    %% remove_last_forward_slash/1）走原实现——non_strict 会把它们变 undef，
    %% 中间件链直接崩（首跑实证）。
    meck:new(auth_ds, [passthrough, no_link]),
    meck:new(enterprise_oa_sso_logic, [non_strict, no_link]),
    %% throttle ETS 表由 throttle app 启动时创建；bare VM 未起 app，check
    %% 会在 ets:lookup 崩（首跑实证）。mock 成功形态，保持中间件链其余真实。
    meck:new(throttle, [passthrough, no_link]),
    meck:expect(throttle, check, fun(_Scope, _Key) -> {ok, 999999, 0} end),
    meck:expect(auth_ds, verify_sign, fun(Req, Env) -> {ok, Req, Env} end),
    meck:expect(auth_ds, condition, fun(_Opt, _Open, _Auth, Req, Env) ->
        {ok, Req, Env}
    end),
    meck:expect(auth_ds, current_uid, fun(_State) -> ?TEST_UID end),
    meck:expect(enterprise_oa_sso_logic, entries, fun(?TEST_UID) ->
        {ok, #{<<"entries">> => [synthetic_entry()]}}
    end),
    Dispatch = cowboy_router:compile(imboy_router:get_routes()),
    {ok, _Pid} = cowboy:start_clear(
        ?LISTENER,
        [{port, 0}],
        #{env => #{dispatch => Dispatch}, middlewares => ?MIDDLEWARES}
    ),
    try
        Port = ranch:get_port(?LISTENER),
        %% ① 新客户端形态：无参数。200 + 信封 payload.entries 含应用。
        NoParam = request(Port, <<"GET">>, <<"/api/v1/workbench/entries">>, []),
        assert_success_envelope(NoParam),
        %% ② 旧客户端兼容形态：携带 organization_id 参数。参数被忽略，
        %% 同样 200（qs/2 事故的原始复现路径——曾在此崩溃 500）。
        LegacyParam = request(
            Port,
            <<"GET">>,
            <<"/api/v1/workbench/entries?organization_id=115130666695526400">>,
            []
        ),
        assert_success_envelope(LegacyParam),
        %% handler 确实以测试 uid 调用了 logic（认证链注入贯通）
        ?assertEqual(2, meck:num_calls(enterprise_oa_sso_logic, entries, 1)),
        ?assert(meck:validate(auth_ds)),
        ?assert(meck:validate(enterprise_oa_sso_logic))
    after
        ok = cowboy:stop_listener(?LISTENER),
        meck:unload(auth_ds),
        meck:unload(enterprise_oa_sso_logic),
        meck:unload(throttle)
    end.

synthetic_entry() ->
    #{
        <<"kind">> => <<"oa">>,
        <<"organization_id">> => 114022088375011328,
        <<"application_id">> => 117647151070447616,
        <<"application_key">> => <<"default-oa">>,
        <<"label">> => <<"办公"/utf8>>,
        <<"redirect_uri">> => <<"https://www.imboy.pub/test_oa.html">>
    }.

%% 成功信封：HTTP 200 + code=0 + payload.entries 非空。任何 handler 层
%% 崩溃（undef 等）经 cowboy 兜底为 500，此断言即红。
assert_success_envelope(Response) ->
    ?assertMatch(<<"HTTP/1.1 200", _/binary>>, Response),
    ?assertNotEqual(nomatch, binary:match(Response, <<"\"code\":0">>)),
    ?assertNotEqual(nomatch, binary:match(Response, <<"\"entries\":">>)),
    ?assertNotEqual(nomatch, binary:match(Response, <<"default-oa">>)).

%%%===================================================================
%%% 真 HTTP helper（同 enterprise_internal_wiring_http_tests 形态）
%%%===================================================================

request(Port, Method, Path, Headers) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 5000),
    HdrLines = [
        [K, <<": ">>, V, <<"\r\n">>]
     || {K, V} <- Headers
    ],
    ok = gen_tcp:send(Socket, [
        Method,
        <<" ">>,
        Path,
        <<" HTTP/1.1\r\nHost: ">>,
        request_host(),
        <<"\r\nConnection: close\r\n">>,
        HdrLines,
        <<"\r\n">>
    ]),
    receive_all(Socket, []).

request_host() ->
    case config_ds:env(host, '_') of
        '_' -> <<"localhost">>;
        Host when is_binary(Host) -> Host;
        Host when is_list(Host) -> unicode:characters_to_binary(Host)
    end.

receive_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} -> receive_all(Socket, [Data | Acc]);
        {error, closed} -> iolist_to_binary(lists:reverse(Acc));
        {error, _} = E -> erlang:error(E)
    end.
