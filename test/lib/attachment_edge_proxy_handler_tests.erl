-module(attachment_edge_proxy_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-export([init/2]).

client_audio_real_http_test_() -> {timeout, 35, fun client_audio_real_http/0}.

client_audio_real_http() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    Table = ets:new(?MODULE, [public, set]),
    ets:insert(Table, {allowed, true}),
    Origin = {?MODULE, origin, make_ref()},
    Routes = cowboy_router:compile([
        {'_', [
            {"/auth", ?MODULE, {auth, Table}},
            {"/audio", ?MODULE, audio}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(Origin, [{ip, {127, 0, 0, 1}}, {port, 0}], #{
        env => #{dispatch => Routes}
    }),
    Port = ranch:get_port(Origin),
    ets:insert(
        Table, {origin, list_to_binary("http://127.0.0.1:" ++ integer_to_list(Port) ++ "/audio")}
    ),
    Edge = {?MODULE, edge, make_ref()},
    Auth = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/auth",
    Path = "/api/v1/talk-records/client-audio-source",
    Dispatch = cowboy_router:compile([
        {'_', [
            {Path, attachment_edge_proxy_handler, {Auth, "secret", client_audio}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(Edge, [{ip, {127, 0, 0, 1}}, {port, 0}], #{
        env => #{dispatch => Dispatch}
    }),
    try
        URL =
            "http://127.0.0.1:" ++ integer_to_list(ranch:get_port(Edge)) ++ Path ++ "?ticket=test",
        Opts = [{autoredirect, false}, {timeout, 5000}],
        {ok, {{_, 200, _}, H, <<"0123456789">>}} = httpc:request(get, {URL, []}, Opts, [
            {body_format, binary}
        ]),
        ?assertEqual(undefined, proplists:get_value("location", H)),
        {ok, {{_, 200, _}, HH, <<>>}} = httpc:request(head, {URL, []}, Opts, [{body_format, binary}]),
        ?assertEqual("10", proplists:get_value("content-length", HH)),
        {ok, {{_, 206, _}, RH, <<"2345">>}} = httpc:request(
            get, {URL, [{"range", "bytes=2-5"}]}, Opts, [{body_format, binary}]
        ),
        ?assertEqual("bytes 2-5/10", proplists:get_value("content-range", RH)),
        ets:insert(Table, {allowed, false}),
        {ok, {{_, 404, _}, _, <<>>}} = httpc:request(get, {URL, []}, Opts, [{body_format, binary}])
    after
        cowboy:stop_listener(Edge),
        cowboy:stop_listener(Origin),
        ets:delete(Table)
    end.

verified_part_real_http_test_() -> {timeout, 50, fun verified_part_real_http/0}.

verified_part_real_http() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    T = ets:new(?MODULE, [public, set]),
    ets:insert(T, [{allowed, true}, {bad_hash, false}, {revoke_on_fetch, false}]),
    Origin = {?MODULE, verified_origin, make_ref()},
    Routes = cowboy_router:compile([
        {'_', [
            {"/auth", ?MODULE, {part_auth, T}},
            {"/audio", ?MODULE, {part_audio, T}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(Origin, [{ip, {127, 0, 0, 1}}, {port, 0}], #{
        env => #{dispatch => Routes}
    }),
    Port = ranch:get_port(Origin),
    Base = "http://127.0.0.1:" ++ integer_to_list(Port),
    ets:insert(T, {origin, list_to_binary(Base ++ "/audio")}),
    Edge = {?MODULE, verified_edge, make_ref()},
    D = cowboy_router:compile([
        {'_', [{"/edge", attachment_edge_proxy_handler, {Base ++ "/auth", "secret"}}]}
    ]),
    {ok, _} = cowboy:start_clear(Edge, [{ip, {127, 0, 0, 1}}, {port, 0}], #{env => #{dispatch => D}}),
    try
        URL =
            "http://127.0.0.1:" ++ integer_to_list(ranch:get_port(Edge)) ++
                "/edge?ticket=parts:0:test",
        Opt = [{timeout, 5000}, {autoredirect, false}],
        Parent = self(),
        Holder = spawn(fun() ->
            Lock = {{attachment_edge_proxy_handler, verified_part}, self()},
            true = global:set_lock(Lock, [node()], 0),
            Parent ! {locked, self()},
            receive
                release -> ok
            end,
            global:del_lock(Lock, [node()]),
            Parent ! {released, self()}
        end),
        receive
            {locked, Holder} -> ok
        after 2000 -> error(lock_timeout)
        end,
        {ok, {{_, 429, _}, _, <<>>}} = httpc:request(get, {URL, []}, Opt, [{body_format, binary}]),
        Holder ! release,
        receive
            {released, Holder} -> ok
        after 2000 -> error(release_timeout)
        end,
        {ok, {{_, 200, _}, _, <<"0123456789">>}} = httpc:request(get, {URL, []}, Opt, [
            {body_format, binary}
        ]),
        {ok, {{_, 206, _}, H, <<"2345">>}} = httpc:request(
            get, {URL, [{"range", "bytes=2-5"}]}, Opt, [{body_format, binary}]
        ),
        ?assertEqual("bytes 2-5/10", proplists:get_value("content-range", H)),
        ets:insert(T, {slow, true}),
        Start = erlang:monotonic_time(millisecond),
        {ok, {{_, 404, _}, _, <<>>}} = httpc:request(
            get,
            {URL, []},
            [{timeout, 40000}, {autoredirect, false}],
            [{body_format, binary}]
        ),
        Elapsed = erlang:monotonic_time(millisecond) - Start,
        ?assert(Elapsed >= 29000 andalso Elapsed < 35000),
        ets:insert(T, {slow, false}),
        {ok, {{_, 416, _}, _, <<>>}} = httpc:request(get, {URL, [{"range", "bytes=99-"}]}, Opt, [
            {body_format, binary}
        ]),
        ets:insert(T, {bad_hash, true}),
        {ok, {{_, 404, _}, _, <<>>}} = httpc:request(get, {URL, []}, Opt, [{body_format, binary}]),
        ets:insert(T, [{bad_hash, false}, {revoke_on_fetch, true}]),
        {ok, {{_, 404, _}, _, <<>>}} = httpc:request(get, {URL, []}, Opt, [{body_format, binary}])
    after
        cowboy:stop_listener(Edge),
        cowboy:stop_listener(Origin),
        ets:delete(T)
    end.

verified_slow_client_test_() -> {timeout, 60, fun verified_slow_client/0}.

verified_slow_client() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    T = ets:new(?MODULE, [public, set]),
    Body = binary:copy(<<"x">>, 32 * 1024 * 1024),
    ets:insert(T, [{allowed, true}, {bad_hash, false}, {body, Body}]),
    Origin = {?MODULE, slow_origin, make_ref()},
    Routes = cowboy_router:compile([
        {'_', [{"/auth", ?MODULE, {part_auth, T}}, {"/audio", ?MODULE, {part_audio, T}}]}
    ]),
    {ok, _} = cowboy:start_clear(Origin, [{ip, {127, 0, 0, 1}}, {port, 0}], #{
        env => #{dispatch => Routes}
    }),
    Base = "http://127.0.0.1:" ++ integer_to_list(ranch:get_port(Origin)),
    ets:insert(T, {origin, list_to_binary(Base ++ "/audio")}),
    Edge = {?MODULE, slow_edge, make_ref()},
    D = cowboy_router:compile([
        {'_', [{"/edge", attachment_edge_proxy_handler, {Base ++ "/auth", "secret"}}]}
    ]),
    {ok, _} = cowboy:start_clear(Edge, [{ip, {127, 0, 0, 1}}, {port, 0}], #{env => #{dispatch => D}}),
    Port = ranch:get_port(Edge),
    try
        {ok, Sock} = gen_tcp:connect(
            {127, 0, 0, 1}, Port, [binary, {active, false}, {recbuf, 4096}], 5000
        ),
        ok = gen_tcp:send(
            Sock, <<"GET /edge?ticket=parts:0:test HTTP/1.1\r\nhost: localhost\r\n\r\n">>
        ),
        {ok, Headers} = gen_tcp:recv(Sock, 0, 30000),
        ?assertMatch({_, _}, binary:match(Headers, <<"200">>)),
        URL = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/edge?ticket=parts:0:test",
        {ok, {{_, 429, _}, _, <<>>}} = httpc:request(get, {URL, []}, [{timeout, 5000}], [
            {body_format, binary}
        ]),
        timer:sleep(31000),
        {ok, {{_, 200, _}, _, <<>>}} = httpc:request(head, {URL, []}, [{timeout, 10000}], [
            {body_format, binary}
        ]),
        ok = gen_tcp:close(Sock)
    after
        cowboy:stop_listener(Edge),
        cowboy:stop_listener(Origin),
        ets:delete(T)
    end.

verified_garage_test_() ->
    case os:getenv("A0_ENDPOINT") of
        false -> [];
        _ -> {timeout, 120, fun verified_garage/0}
    end.

verified_garage() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    Ep = list_to_binary(os:getenv("A0_ENDPOINT")),
    Ak = list_to_binary(os:getenv("A0_AK")),
    Sk = list_to_binary(os:getenv("A0_SK")),
    %% A0_BUCKET 未随 run 脚本导出时回退 fixture 桶名 imboy：与
    %% attachment_worker_port_tests 的 A0_BUCKET 默认值对齐。质量车道
    %% harness Garage 只建 imboy-w403-*/talk-record-relay/imboy 三桶
    %% （imboy_quality_env:garage/2 注明「真 Garage gate 链夹具固定
    %% bucket=imboy 语义」）；旧回退 imboy-closeout-v3 是 closeout 私有
    %% env 的桶名，该车道不存在——PUT 404 会经 uncertain_result 归并成
    %% worker_outcome_unknown 假失败。
    Bucket = list_to_binary(os:getenv("A0_BUCKET", "imboy")),
    Cfg = #{
        endpoint => Ep, bucket => Bucket, access_key => Ak, secret_key => Sk, region => <<"garage">>
    },
    Run = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    Key = <<"closeout-parts-edge/", Run/binary>>,
    Body = <<"0123456789">>,
    File = filename:join("/tmp", "closeout-parts-edge-" ++ binary_to_list(Run)),
    ok = file:write_file(File, Body),
    {ok, _} = attachment_worker_port:upload_file(#{
        src_path => File,
        endpoint => Ep,
        ak => Ak,
        sk => Sk,
        bucket => Bucket,
        region => <<"garage">>,
        object_key => Key,
        mime => <<"audio/aac">>
    }),
    Url = elib_s3_sign:presign_signed_url_at(
        <<"GET">>, Ep, Bucket, Key, [], calendar:universal_time(), 300, Cfg
    ),
    T = ets:new(?MODULE, [public, set]),
    ets:insert(T, [{allowed, true}, {bad_hash, false}, {origin, Url}]),
    Origin = {?MODULE, garage_auth, make_ref()},
    R = cowboy_router:compile([{'_', [{"/auth", ?MODULE, {part_auth, T}}]}]),
    {ok, _} = cowboy:start_clear(Origin, [{ip, {127, 0, 0, 1}}, {port, 0}], #{
        env => #{dispatch => R}
    }),
    Auth = "http://127.0.0.1:" ++ integer_to_list(ranch:get_port(Origin)) ++ "/auth",
    Edge = {?MODULE, garage_edge, make_ref()},
    D = cowboy_router:compile([{'_', [{"/edge", attachment_edge_proxy_handler, {Auth, "secret"}}]}]),
    {ok, _} = cowboy:start_clear(Edge, [{ip, {127, 0, 0, 1}}, {port, 0}], #{env => #{dispatch => D}}),
    try
        EdgeUrl =
            "http://127.0.0.1:" ++ integer_to_list(ranch:get_port(Edge)) ++
                "/edge?ticket=parts:0:test",
        Opt = [{timeout, 30000}, {autoredirect, false}],
        {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {EdgeUrl, []}, Opt, [
            {body_format, binary}
        ]),
        {ok, {{_, 206, _}, _, <<"2345">>}} = httpc:request(
            get, {EdgeUrl, [{"range", "bytes=2-5"}]}, Opt, [{body_format, binary}]
        ),
        ets:insert(T, {bad_hash, true}),
        {ok, {{_, 404, _}, _, <<>>}} = httpc:request(get, {EdgeUrl, []}, Opt, [
            {body_format, binary}
        ])
    after
        cowboy:stop_listener(Edge),
        cowboy:stop_listener(Origin),
        ets:delete(T)
    end.

init(Req, {part_auth, T} = State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Ticket = maps:get(<<"ticket">>, jsone:decode(Body)),
    Res =
        case {Ticket, ets:lookup(T, allowed)} of
            {<<"parts:0:test">>, [{allowed, true}]} ->
                [{origin, Url}] = ets:lookup(T, origin),
                TestBody =
                    case ets:lookup(T, body) of
                        [{body, B}] -> B;
                        _ -> <<"0123456789">>
                    end,
                Hash =
                    case ets:lookup(T, bad_hash) of
                        [{bad_hash, true}] -> binary:copy(<<"0">>, 64);
                        _ -> binary:encode_hex(crypto:hash(sha256, TestBody), lowercase)
                    end,
                cowboy_req:reply(
                    200,
                    #{},
                    jsone:encode(#{
                        <<"url">> => Url, <<"size">> => byte_size(TestBody), <<"sha256">> => Hash
                    }),
                    Req1
                );
            _ ->
                cowboy_req:reply(404, #{}, <<>>, Req1)
        end,
    {ok, Res, State};
init(Req, {part_audio, T} = State) ->
    case ets:lookup(T, slow) of
        [{slow, true}] ->
            SlowReq = cowboy_req:stream_reply(200, #{}, Req),
            slow_stream(SlowReq, 20),
            {ok, SlowReq, State};
        _ ->
            part_audio(Req, T, State)
    end;
init(Req, {auth, Table} = State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Ticket = maps:get(<<"ticket">>, jsone:decode(Body)),
    Response =
        case {Ticket, ets:lookup(Table, allowed)} of
            {<<"client_audio:test">>, [{allowed, true}]} ->
                [{origin, URL}] = ets:lookup(Table, origin),
                cowboy_req:reply(
                    200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsone:encode(#{<<"url">> => URL}),
                    Req1
                );
            _ ->
                cowboy_req:reply(404, #{}, <<>>, Req1)
        end,
    {ok, Response, State};
init(Req, audio) ->
    {Status, Headers, Body} =
        case cowboy_req:header(<<"range">>, Req) of
            <<"bytes=2-5">> -> {206, #{<<"content-range">> => <<"bytes 2-5/10">>}, <<"2345">>};
            _ -> {200, #{}, <<"0123456789">>}
        end,
    {ok, cowboy_req:reply(Status, Headers#{<<"content-type">> => <<"audio/aac">>}, Body, Req),
        audio}.

part_audio(Req, T, State) ->
    case ets:lookup(T, revoke_on_fetch) of
        [{revoke_on_fetch, true}] -> ets:insert(T, {allowed, false});
        _ -> ok
    end,
    Body =
        case ets:lookup(T, body) of
            [{body, B}] -> B;
            _ -> <<"0123456789">>
        end,
    {ok, cowboy_req:reply(200, #{}, Body, Req), State}.

slow_stream(_, 0) ->
    ok;
slow_stream(Req, N) ->
    timer:sleep(3000),
    cowboy_req:stream_body(<<"0">>, nofin, Req),
    slow_stream(Req, N - 1).

client_audio_duplicate_query_denied_test() ->
    pure_mock_fixture:run(fun() ->
        Req = #{method => <<"GET">>},
        Ctx = {"http://authorize.invalid", "owned-test-token", client_audio},
        pure_mock_fixture:with_mocks(
            [
                {cowboy_req, [
                    {method, 1, fun(_) -> <<"GET">> end},
                    {parse_qs, 1, fun(_) -> [{<<"ticket">>, <<"a">>}, {<<"ticket">>, <<"b">>}] end},
                    {reply, 4, fun(404, _, <<>>, _) -> Req#{replied => true} end}
                ]}
            ],
            fun() ->
                ?assertEqual(
                    {ok, Req#{replied => true}, Ctx},
                    attachment_edge_proxy_handler:init(Req, Ctx)
                )
            end
        )
    end).

unsatisfied_range_test_() ->
    [
        {timeout, 35, ?_test(pure_mock_fixture:run(fun() -> unsatisfied_range(Method) end))}
     || Method <- [<<"GET">>, <<"HEAD">>]
    ].

client_audio_unsatisfied_range_test_() ->
    {timeout, 35,
        ?_test(
            pure_mock_fixture:run(fun() ->
                unsatisfied_range(<<"GET">>, client_audio)
            end)
        )}.

unsatisfied_range(Method) ->
    unsatisfied_range(Method, generic).

unsatisfied_range(Method, Kind) ->
    Request = #{method => Method},
    Context =
        case Kind of
            generic -> {"http://authorize.invalid", "owned-test-token"};
            client_audio -> {"http://authorize.invalid", "owned-test-token", client_audio}
        end,
    Origin = <<"http://storage.invalid/private-key">>,
    RequestId = make_ref(),
    pure_mock_fixture:with_mocks(
        [
            {cowboy_req, [
                {method, 1, fun(_) -> Method end},
                {parse_qs, 1, fun(_) -> [{<<"ticket">>, <<"owned-ticket">>}] end},
                {header, 2, fun(<<"range">>, _) -> <<"bytes=99-">> end},
                {reply, 4, fun(416, Headers, <<>>, ActualRequest) ->
                    ?assertEqual(Request, ActualRequest),
                    ?assertEqual(<<"bytes */10">>, maps:get(<<"content-range">>, Headers)),
                    ?assertEqual(<<"private, no-store">>, maps:get(<<"cache-control">>, Headers)),
                    ?assertNot(maps:is_key(<<"content-length">>, Headers)),
                    ?assertNot(maps:is_key(<<"location">>, Headers)),
                    ?assertNot(maps:is_key(<<"set-cookie">>, Headers)),
                    Request#{replied => true}
                end}
            ]},
            {httpc, [
                {request, 4, fun
                    (post, {"http://authorize.invalid", _, "application/json", Body}, _, _) ->
                        Decoded = jsone:decode(list_to_binary(Body)),
                        ?assertEqual(
                            Method, maps:get(<<"method">>, Decoded)
                        ),
                        case Kind of
                            client_audio ->
                                ?assertEqual(
                                    <<"client_audio:owned-ticket">>,
                                    maps:get(<<"ticket">>, Decoded)
                                );
                            generic ->
                                ok
                        end,
                        {ok, {{"HTTP/1.1", 200, "OK"}, [], jsone:encode(#{<<"url">> => Origin})}};
                    (get, {Url, [{"range", "bytes=99-"}]}, Options, _) ->
                        ?assertEqual(binary_to_list(Origin), Url),
                        ?assertEqual(false, proplists:get_value(autoredirect, Options)),
                        self() !
                            {http,
                                {RequestId, {
                                    {"HTTP/1.1", 416, "Range Not Satisfiable"},
                                    [
                                        {"content-range", "bytes */10"},
                                        {"content-length", "27"},
                                        {"location", binary_to_list(Origin)},
                                        {"set-cookie", "origin-secret=1"}
                                    ],
                                    <<"private upstream error body">>
                                }}},
                        {ok, RequestId}
                end},
                {cancel_request, 1, fun(ActualId) ->
                    ?assertEqual(RequestId, ActualId),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, Request#{replied => true}, Context},
                attachment_edge_proxy_handler:init(Request, Context)
            )
        end
    ).
