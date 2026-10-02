%% Disposable PG only; real Internal authentication, outbox, worker and sender.
%% The receiver is a synthetic loopback HTTP endpoint, never a transport mock.
-module(enterprise_webhook_delivery_http_checks).
-export([run/0, init/2]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    ?assertEqual(<<"test">>, imboy_env:current()),
    ?assertEqual(undefined, whereis(bot_webhook_delivery_worker)),
    ?assertEqual(undefined, application:get_env(imboy, bot_webhook_sender_mod)),
    Previous = application:get_env(imboy, bot_webhook_timeout_ms),
    S = intbe02_http_support:setup_all(),
    try
        application:set_env(imboy, bot_webhook_timeout_ms, 1000),
        lists:foreach(fun(Mode) -> scenario(S, Mode) end, [retry, dead, timeout]),
        io:format("ENTERPRISE_WEBHOOK_REAL_DELIVERY_RESULT=ok~n")
    after
        restore_timeout(Previous),
        intbe02_http_support:teardown_all(S),
        inttest_marker_db:release(S)
    end.

scenario(S, Mode) ->
    T = ets:new(?MODULE, [public, set]),
    Listener = enterprise_webhook_synthetic_receiver,
    try
        Dispatch = cowboy_router:compile([{'_', [{"/hook", ?MODULE, #{table => T}}]}]),
        {ok, _} = cowboy:start_clear(
            Listener,
            [{ip, {127, 0, 0, 1}}, {port, 0}],
            #{env => #{dispatch => Dispatch}}
        ),
        Port = ranch:get_port(Listener),
        Secret = configure(S, Port, Mode),
        ets:insert(T, [{secret, Secret}, {mode, Mode}, {count, 0}]),
        Did = emit(S, Mode),
        assert_pending(S, Did),
        with_worker(fun() -> check_delivery(S, T, Did, Mode) end),
        record(S, T, Did, Mode),
        io:format("ENTERPRISE_WEBHOOK_REAL_~s=PASS~n", [string:uppercase(atom_to_list(Mode))])
    after
        cowboy:stop_listener(Listener),
        ets:delete(T)
    end.

configure(S, Port, Mode) ->
    Url = iolist_to_binary(["http://127.0.0.1:", integer_to_binary(Port), "/hook"]),
    Input = #{<<"url">> => Url, <<"events">> => [<<"file.confirmed">>], <<"rotate">> => true},
    assert_production_rejects(S, Input, Mode),
    Json = request(S, <<"PUT">>, <<"/api/internal/v1/webhook">>, Input, key(Mode, <<"config">>)),
    Secret = maps:get(<<"secret">>, Json),
    ?assert(is_binary(Secret) andalso byte_size(Secret) > 0),
    Secret.

assert_production_rejects(S, Input, Mode) ->
    Conn = maps:get(conn, S),
    Before = enterprise_webhook_repo:find_bot_config_tx(Conn, 995014),
    Previous = os:getenv("IMBOYENV"),
    try
        os:putenv("IMBOYENV", "prod"),
        Headers = maps:merge(
            intbe02_http_support:auth(maps:get(cred_a, S)),
            intbe02_http_support:idem(key(Mode, <<"prod-deny">>))
        ),
        R = intbe02_http_support:http(
            maps:get(port, S), <<"PUT">>, <<"/api/internal/v1/webhook">>, Input, Headers
        ),
        ?assertEqual(400, maps:get(status, R)),
        Json = jsone:decode(maps:get(body, R)),
        ?assertEqual(<<"invalid_request">>, maps:get(<<"code">>, maps:get(<<"error">>, Json))),
        ?assertEqual(Before, enterprise_webhook_repo:find_bot_config_tx(Conn, 995014)),
        io:format("ENTERPRISE_WEBHOOK_PROD_LOOPBACK_DENIED=PASS~n")
    after
        case Previous of
            false -> os:unsetenv("IMBOYENV");
            _ -> os:putenv("IMBOYENV", Previous)
        end
    end.

emit(S, Mode) ->
    Json = request(
        S, <<"POST">>, <<"/api/internal/v1/webhook/test-delivery">>, #{}, key(Mode, <<"emit">>)
    ),
    ?assertEqual(true, maps:get(<<"enqueued">>, Json)),
    ?assertEqual(<<"webhook.ping">>, maps:get(<<"event_type">>, Json)),
    maps:get(<<"delivery_id">>, Json).

request(S, Method, Path, Body, Key) ->
    Headers = maps:merge(
        intbe02_http_support:auth(maps:get(cred_a, S)), intbe02_http_support:idem(Key)
    ),
    R = intbe02_http_support:http(maps:get(port, S), Method, Path, Body, Headers),
    ?assertEqual(200, maps:get(status, R)),
    jsone:decode(maps:get(body, R)).

key(Mode, Suffix) ->
    <<"real-delivery-", (atom_to_binary(Mode))/binary, "-", Suffix/binary>>.

assert_pending(S, Did) ->
    Row = delivery(S, Did),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Row)),
    ?assertEqual(0, maps:get(<<"attempt_count">>, Row)),
    ?assertEqual(<<"127.0.0.1">>, maps:get(<<"pinned_ip">>, Row)),
    ?assertEqual(<<"eapp:995014">>, maps:get(<<"bot_id">>, Row)),
    ?assertEqual([], attempts(S, Did)).

with_worker(Fun) ->
    ?assertEqual(undefined, whereis(bot_webhook_delivery_worker)),
    {ok, Pid} = bot_webhook_delivery_worker:start_link(),
    try
        Fun()
    after
        gen_server:stop(Pid, normal, 10000)
    end.

check_delivery(S, T, Did, retry) ->
    wait_status(S, Did, <<"retry">>, 4000),
    assert_retry_delay(S, Did),
    assert_attempt(S, Did, 1, <<"5xx">>, 500),
    wait_status(S, Did, <<"success">>, 10000),
    ?assertEqual(2, maps:get(<<"attempt_count">>, delivery(S, Did))),
    assert_attempt(S, Did, 2, <<"2xx">>, 200),
    ?assertEqual(2, length(attempts(S, Did))),
    [First, Second] = receptions(T, Did, 2),
    ?assert(maps:get(body, First) =:= maps:get(body, Second)),
    ?assert(maps:get(at, Second) - maps:get(at, First) >= 4900),
    ?assert(maps:get(body, First) =:= maps:get(<<"payload">>, delivery(S, Did)));
check_delivery(S, T, Did, dead) ->
    wait_status(S, Did, <<"dead">>, 4000),
    assert_attempt(S, Did, 1, <<"4xx">>, 410),
    timer:sleep(6500),
    ?assertEqual(<<"dead">>, maps:get(<<"status">>, delivery(S, Did))),
    ?assertEqual(1, maps:get(<<"attempt_count">>, delivery(S, Did))),
    ?assertEqual(1, length(attempts(S, Did))),
    [_] = receptions(T, Did, 1);
check_delivery(S, T, Did, timeout) ->
    wait_status(S, Did, <<"retry">>, 4000),
    assert_retry_delay(S, Did),
    A = assert_attempt(S, Did, 1, <<"error">>, null),
    ?assert(maps:get(<<"latency_ms">>, A) >= 900),
    ?assertEqual(<<"{recv,timeout}">>, maps:get(<<"error_trunc">>, A)),
    ?assertEqual(1, length(attempts(S, Did))),
    [_] = receptions(T, Did, 1).

assert_retry_delay(S, Did) ->
    Row = one(
        S,
        <<"SELECT extract(epoch FROM next_retry_at-updated_at)::float8 AS delay FROM bot_delivery WHERE delivery_id=$1">>,
        [Did]
    ),
    Delay = maps:get(<<"delay">>, Row),
    ?assert(Delay >= 4.9 andalso Delay =< 5.1).

assert_attempt(S, Did, N, Class, Code) ->
    [A] = [R || R <- attempts(S, Did), maps:get(<<"attempt_no">>, R) =:= N],
    ?assertEqual(Class, maps:get(<<"status_class">>, A)),
    ?assertEqual(Code, maps:get(<<"http_status">>, A)),
    A.

delivery(S, Did) ->
    one(
        S,
        <<"SELECT status,attempt_count,pinned_ip,bot_id,payload::text AS payload FROM bot_delivery WHERE delivery_id=$1">>,
        [Did]
    ).

attempts(S, Did) ->
    {ok, Rows} = elib_pg:query(
        maps:get(conn, S),
        <<"SELECT attempt_no,status_class,http_status,latency_ms,error_trunc FROM bot_delivery_attempt WHERE delivery_id=$1 ORDER BY attempt_no">>,
        [Did]
    ),
    Rows.

one(S, Sql, Params) ->
    intbe02_http_support:one(maps:get(conn, S), Sql, Params).

wait_status(S, Did, Status, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    wait_status_until(S, Did, Status, Deadline).

wait_status_until(S, Did, Status, Deadline) ->
    case maps:get(<<"status">>, delivery(S, Did)) of
        Status ->
            ok;
        _ ->
            ?assert(erlang:monotonic_time(millisecond) < Deadline),
            timer:sleep(20),
            wait_status_until(S, Did, Status, Deadline)
    end.

receptions(T, Did, N) ->
    ?assertEqual(N, ets:lookup_element(T, count, 2)),
    Rows = [ets:lookup_element(T, {request, I}, 2) || I <- lists:seq(1, N)],
    lists:foreach(
        fun(R) ->
            ?assertEqual(true, maps:get(valid, R)),
            ?assertEqual(Did, maps:get(delivery, R)),
            ?assertEqual(<<"webhook.ping">>, maps:get(event, R))
        end,
        Rows
    ),
    Rows.

%% Cowboy receiver independently computes HMAC; never writes bodies/secrets to logs.
init(Req0, #{table := T} = State) ->
    {Body, Req} = read_body(Req0, <<>>),
    N = ets:update_counter(T, count, 1),
    Ts = cowboy_req:header(<<"x-imboy-timestamp">>, Req, <<>>),
    Signature = cowboy_req:header(<<"x-imboy-signature">>, Req, <<>>),
    Secret = ets:lookup_element(T, secret, 2),
    Hex = binary:encode_hex(
        crypto:mac(hmac, sha256, Secret, <<Ts/binary, ".", Body/binary>>), lowercase
    ),
    Expected = <<"sha256=", Hex/binary>>,
    Valid = Signature =:= Expected andalso timestamp_valid(Ts),
    Row = #{
        body => Body,
        valid => Valid,
        at => erlang:monotonic_time(millisecond),
        delivery => cowboy_req:header(<<"x-imboy-delivery">>, Req),
        event => cowboy_req:header(<<"x-imboy-event">>, Req)
    },
    ets:insert(T, {{request, N}, Row}),
    Code = receiver_status(ets:lookup_element(T, mode, 2), N),
    {ok, cowboy_req:reply(Code, #{}, <<>>, Req), State}.

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Next} -> {<<Acc/binary, Body/binary>>, Next};
        {more, Body, Next} -> read_body(Next, <<Acc/binary, Body/binary>>)
    end.

timestamp_valid(Ts) ->
    try
        abs(os:system_time(second) - binary_to_integer(Ts)) =< 5
    catch
        error:badarg -> false
    end.

receiver_status(retry, 1) ->
    500;
receiver_status(retry, _) ->
    200;
receiver_status(dead, _) ->
    410;
receiver_status(timeout, _) ->
    timer:sleep(1500),
    200.

restore_timeout(undefined) -> application:unset_env(imboy, bot_webhook_timeout_ms);
restore_timeout({ok, Value}) -> application:set_env(imboy, bot_webhook_timeout_ms, Value).

record(S, T, Did, Mode) ->
    Row = delivery(S, Did),
    Safe = #{
        scenario => Mode,
        status => maps:get(<<"status">>, Row),
        attempt_count => maps:get(<<"attempt_count">>, Row),
        receiver_count => ets:lookup_element(T, count, 2),
        attempts => attempts(S, Did),
        hmac_verified => true
    },
    Path = filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "webhook-real-delivery.jsonl"),
    ok = file:write_file(Path, [jsone:encode(Safe), <<"\n">>], [append]).
