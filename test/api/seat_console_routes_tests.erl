-module(seat_console_routes_tests).
-export([init/2]).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

seat_upload_target_preserves_credential_boundary_test() ->
    Previous = application:get_env(imboy, base_url),
    application:set_env(imboy, base_url, <<"https://api.example.test">>),
    try
        View = #{upload_ref => <<"unit-ref">>, upload => #{}},
        Req = #{
            path => <<"/api/v1/seat/enterprise/organizations/1/assets/presign">>,
            bindings => #{org_id => <<"1">>}
        },
        {ok, #{upload := #{url := SeatUrl, method := <<"PUT">>}}} =
            eb_enterprise_http:with_seat_upload_url(Req, 2, {ok, View}),
        ?assertEqual(
            <<"/api/v1/seat/enterprise/organizations/1/assets/presign?workspace_id=2&upload_ref=unit-ref">>,
            SeatUrl
        ),
        {ok, #{upload := #{url := HumanUrl}}} = eb_enterprise_http:with_seat_upload_url(
            Req#{path := <<"/api/v1/enterprise/organizations/1/assets/presign">>}, 2, {ok, View}
        ),
        ?assertEqual(
            <<"https://api.example.test/api/v1/enterprise/organizations/1/assets/presign?workspace_id=2&upload_ref=unit-ref">>,
            HumanUrl
        )
    after
        case Previous of
            {ok, Value} -> application:set_env(imboy, base_url, Value);
            undefined -> application:unset_env(imboy, base_url)
        end
    end.

%% 独立冻结 Console 路径，旧 Human 路由的完整冻结仍由 cs_route_contract_tests 覆盖。
seat_console_route_set_and_metadata_test() ->
    [{_, All}] = imboy_router:get_routes(),
    Seat = [{P, H, O} || {P, H, O} <- All, is_map(O), maps:get(jwt_purpose, O, human) =:= seat],
    ?assertEqual(lists:sort(expected_paths()), lists:sort([P || {P, _, _} <- Seat])),
    lists:foreach(
        fun({"/api/v1/seat/" ++ Tail, Handler, Opts}) ->
            [{_, Handler, HumanOpts}] = [R || {P, _, _} = R <- All, P =:= "/api/v1/" ++ Tail],
            Expected =
                case Handler of
                    eb_tenant_handler -> HumanOpts#{required_function => <<"customer_service">>};
                    _ -> HumanOpts
                end,
            ?assertEqual(Expected, maps:without([jwt_purpose, jwt_methods], Opts)),
            Methods = maps:get(jwt_methods, Opts),
            ?assert(Methods =/= []),
            ?assertEqual(
                {ok, tenant},
                eb_auth_principal:classify_surface(list_to_binary("/api/v1/seat/" ++ Tail))
            ),
            case Handler of
                cs_tenant_handler ->
                    {ok, Entry} = cs_actions:tenant(maps:get(action, Opts)),
                    lists:foreach(
                        fun(Method) ->
                            {ok, _} = cs_actions:case_for(Entry, Method),
                            Override = maps:get(Method, maps:get(case_auth, Entry, #{}), #{}),
                            Effective = maps:merge(Opts, Override),
                            ?assertEqual(cs_seat, maps:get(auth_context, Effective))
                        end,
                        Methods
                    );
                eb_tenant_handler ->
                    ?assertEqual(enterprise_member, maps:get(auth_context, Opts)),
                    ?assertEqual(<<"customer_service">>, maps:get(required_function, Opts))
            end
        end,
        Seat
    ),
    [{_, _, Queue}] = [
        R
     || {P, _, _} = R <- Seat, P =:= "/api/v1/seat/cs/organizations/:org_id/sessions/queue"
    ],
    ?assertEqual([<<"GET">>], maps:get(jwt_methods, Queue)).

expected_paths() ->
    C = "/api/v1/seat/cs/organizations/:org_id",
    E = "/api/v1/seat/enterprise/organizations/:org_id",
    [
        "/api/v1/seat/cs/me/seat-contexts",
        C ++ "/sessions/queue",
        C ++ "/sessions/:id/claim",
        C ++ "/sessions/:id/transfer",
        C ++ "/sessions/:id/close",
        C ++ "/sessions/:id",
        C ++ "/sessions/:id/context",
        C ++ "/sessions/:id/read-cursor",
        C ++ "/seats/me/heartbeat",
        C ++ "/seats/me/presence",
        C ++ "/seats/presence",
        C ++ "/seats/sessions",
        C ++ "/transfer-targets",
        C ++ "/seats/me/events",
        "/api/v1/seat/enterprise/conversations/:conversation_id/messages",
        E ++ "/conversations/:id/messages",
        E ++ "/assets/presign",
        E ++ "/assets/confirm",
        E ++ "/assets/:id/content"
    ].

seat_and_human_routes_enforce_opposite_purposes_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (jwt_key, _) -> <<"unit-console-purpose-key">>;
                    (api_auth_switch, _) -> <<"on">>
                end}
            ]},
            {user_device_ds, [{'is_active', 2, fun(123, <<"console-device">>) -> true end}]},
            {auth_session_ds, [
                {'current_epoch', 1, fun(123) -> {ok, 1} end},
                {'revoked', 2, fun(123, 1) -> false end}
            ]},
            {cowboy_req, [
                {'path', 1, fun(Req) -> maps:get(path, Req) end},
                {'method', 1, fun(Req) -> maps:get(method, Req) end},
                {'set_resp_header', 3, fun(Key, Value, Req) ->
                    Req#{response_headers => #{Key => Value}}
                end},
                {'header', 2, fun(<<"authorization">>, Req) -> maps:get(token, Req, undefined) end}
            ]},
            {elib_response, [
                {'error_with_status', 4, fun(Req, Status, _, _) -> Req#{http_status => Status} end}
            ]}
        ],
        fun() ->
            Human = token_ds:encrypt_token(123, <<"console-device">>),
            Seat = token_ds:encrypt_seat_token(123, <<"console-device">>),
            SeatOpts = #{handler_opts => #{jwt_purpose => seat, jwt_methods => [<<"GET">>]}},
            SReq = #{path => <<"/api/v1/seat/cs/me/seat-contexts">>, method => <<"GET">>},
            ?assertMatch(
                {ok, _, #{handler_opts := #{current_uid := 123}}},
                auth_middleware_api_v1:execute(SReq#{token => Seat}, SeatOpts)
            ),
            ?assertMatch(
                {stop, #{http_status := 401}},
                auth_middleware_api_v1:execute(SReq#{token => Human}, SeatOpts)
            ),
            ?assertMatch(
                {stop, #{http_status := 401}}, auth_middleware_api_v1:execute(SReq, SeatOpts)
            ),
            ?assertMatch(
                {stop, #{http_status := 405}},
                auth_middleware_api_v1:execute(SReq#{method => <<"POST">>, token => Seat}, SeatOpts)
            ),
            HReq = SReq#{path => <<"/api/v1/cs/me/seat-contexts">>},
            ?assertMatch(
                {ok, _, _},
                auth_middleware_api_v1:execute(HReq#{token => Human}, #{handler_opts => #{}})
            ),
            ?assertMatch(
                {stop, #{http_status := 401}},
                auth_middleware_api_v1:execute(HReq#{token => Seat}, #{handler_opts => #{}})
            )
        end
    ).

%% 真 Cowboy 监听器 + 生产 auth middleware；业务 handler 用计数探针替代，
%% 只证明用途拒绝发生在 handler 前，不计作真实 PG/客服完整旅程。
real_http_purpose_gate_before_handler_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (jwt_key, _) -> <<"unit-http-console-purpose-key">>;
                    (api_auth_switch, _) -> <<"on">>;
                    (_, Default) -> Default
                end}
            ]},
            {user_device_ds, [{'is_active', 2, fun(123, <<"http-console-device">>) -> true end}]},
            {auth_session_ds, [
                {'current_epoch', 1, fun(123) -> {ok, 1} end},
                {'revoked', 2, fun(123, 1) -> false end}
            ]}
        ],
        fun run_http_purpose_gate/0
    ).

run_http_purpose_gate() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    [{_, All}] = imboy_router:get_routes(),
    Calls = counters:new(1, []),
    HumanPath = "/api/v1/cs/me/seat-contexts",
    SeatPath = "/api/v1/seat/cs/me/seat-contexts",
    ProbeRoutes = [
        {P, ?MODULE, O#{test_calls => Calls}}
     || {P, _, O} <- All,
        P =:= HumanPath orelse P =:= SeatPath
    ],
    ?assertEqual(2, length(ProbeRoutes)),
    Dispatch = cowboy_router:compile([{'_', ProbeRoutes}]),
    Name = seat_console_purpose_http_test,
    {ok, _} = cowboy:start_clear(
        Name,
        [{ip, {127, 0, 0, 1}}, {port, 0}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [cowboy_router, auth_middleware, cowboy_handler]
        }
    ),
    Port = ranch:get_port(Name),
    try
        Human = token_ds:encrypt_token(123, <<"http-console-device">>),
        Seat = token_ds:encrypt_seat_token(123, <<"http-console-device">>),
        ?assertEqual(401, http_status(Port, SeatPath, Human)),
        ?assertEqual(401, http_status(Port, HumanPath, Seat)),
        ?assertEqual(401, http_status(Port, SeatPath, undefined)),
        {ok, {{_, 405, _}, Headers405, _}} = http_response(Port, SeatPath, Seat, post),
        ?assertEqual("GET", proplists:get_value("allow", Headers405)),
        ?assertEqual(0, counters:get(Calls, 1)),
        ?assertEqual(200, http_status(Port, SeatPath, Seat)),
        ?assertEqual(200, http_status(Port, HumanPath, Human)),
        ?assertEqual(2, counters:get(Calls, 1))
    after
        cowboy:stop_listener(Name)
    end.

http_status(Port, Path, Token) ->
    {ok, {{_, Status, _}, _, _}} = http_response(Port, Path, Token, get),
    Status.

http_response(Port, Path, Token, Method) ->
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ Path,
    Headers =
        case Token of
            undefined -> [];
            _ -> [{"authorization", "Bearer " ++ binary_to_list(Token)}]
        end,
    Request =
        case Method of
            get -> {Url, Headers};
            post -> {Url, Headers, "application/json", <<"{}">>}
        end,
    httpc:request(Method, Request, [{timeout, 3000}], [{body_format, binary}]).

init(Req, #{test_calls := Calls, current_uid := Uid} = State) ->
    ?assertEqual(123, Uid),
    counters:add(Calls, 1, 1),
    {ok,
        cowboy_req:reply(
            200,
            #{<<"content-type">> => <<"application/json">>},
            <<"{\"ok\":true}">>,
            Req
        ),
        State}.
