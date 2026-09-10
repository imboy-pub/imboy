-module(bot_webhook_delivery_worker_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% WH-01：交付 worker 行为（真库 + 真 guard/解析/sender + 本地 loopback TCP fixture）。
%%%   A03 2xx → success；5xx → retry（预算耗尽 → dead）；4xx → 不重试直接死信；
%%%   A05 SSRF 负例：私网/内网目标 guard 拒绝 → dead 且零外联；
%%%   A06 本地 loopback fixture（test profile 显式开放 http）走真 sender 全链。
%%% fixture：gen_tcp 监听 127.0.0.1 随机端口，按 ETS 配置返回状态码并计数连接。

-define(TEST_AES_KEY, <<"0123456789abcdef0123456789abcdef">>).

uid() ->
    erlang:unique_integer([positive]) +
        erlang:phash2(binary:encode_hex(crypto:strong_rand_bytes(8))).

did() -> iolist_to_binary(["dlv-", integer_to_binary(uid())]).

corr() ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"corr-", Bin/binary>>.

host_of(Url) ->
    case uri_string:parse(Url) of
        #{host := H} -> ec_cnv:to_binary(H);
        _ -> <<>>
    end.

%% 独立 user/bot fixture + AEAD verify_token，不覆盖共享库种子 Bot。
setup_bot(Url) ->
    {ok, _} = application:ensure_all_started(crypto),
    ok = application:set_env(imboy, postgre_aes_key, ?TEST_AES_KEY),
    BotId = 8000000000000000000 + uid(),
    Account = iolist_to_binary(["wh-worker-", integer_to_binary(BotId)]),
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO public.\"user\""
            " (id, account, nickname, password, status, account_type, reg_ip, reg_cosv)"
            " VALUES ($1, $2, 'webhook worker fixture', '', 1, 3, '127.0.0.1', 'eunit')"
        >>,
        [BotId, Account]
    ),
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO public.bot (user_id, owner_uid, name, webhook_url, verify_token,"
            " created_at, updated_at) VALUES ($1, $1, 'wt', $2, 'legacy', NOW(), NOW())"
        >>,
        [BotId, Url]
    ),
    {ok, _} = bot_repo:set_verify_token_enc(BotId, <<"wh-verify-secret">>),
    BotId.

%% 本地 TCP fixture：监听 127.0.0.1 随机端口，每个连接回固定状态码并计数
start_fixture(Status) ->
    {ok, _} = application:ensure_all_started(crypto),
    Tab = ets:new(wh_fixture, [public, named_table, set]),
    ets:insert(Tab, [{conn, 0}, {status, Status}]),
    {ok, Listen} = gen_tcp:listen(0, [
        binary,
        {active, false},
        {reuseaddr, true},
        {ip, {127, 0, 0, 1}}
    ]),
    {ok, Port} = inet:port(Listen),
    ets:insert(Tab, {port, Port}),
    Acceptor = spawn(fun() -> fixture_accept(Listen, Tab) end),
    ets:insert(Tab, {acceptor, Acceptor}),
    {Port, Tab}.

fixture_accept(Listen, Tab) ->
    case gen_tcp:accept(Listen, 10000) of
        {ok, Sock} ->
            ets:update_counter(Tab, conn, 1),
            Pid = spawn(fun() ->
                receive
                    go -> ok
                after 5000 -> ok
                end,
                {ok, _} = gen_tcp:recv(Sock, 0, 5000),
                [{_, Status}] = ets:lookup(Tab, status),
                gen_tcp:send(
                    Sock,
                    iolist_to_binary(
                        [
                            "HTTP/1.1 ",
                            integer_to_binary(Status),
                            " TT\r\ncontent-length: 0\r\n"
                            "connection: close\r\n\r\n"
                        ]
                    )
                ),
                gen_tcp:close(Sock)
            end),
            gen_tcp:controlling_process(Sock, Pid),
            Pid ! go,
            fixture_accept(Listen, Tab);
        _ ->
            ok
    end.

cleanup_fixture(Tab) ->
    [{_, Acceptor}] = ets:lookup(Tab, acceptor),
    try
        exit(Acceptor, kill)
    catch
        _:_ -> ok
    end,
    try
        ets:delete(Tab)
    catch
        _:_ -> ok
    end,
    ok.

wait_status(D, Status, Tries) ->
    case bot_webhook_delivery_repo:get_delivery(D) of
        {ok, #{<<"status">> := Status}} ->
            ok;
        {ok, _} when Tries > 0 ->
            timer:sleep(200),
            wait_status(D, Status, Tries - 1);
        {ok, Row} ->
            Row;
        Other ->
            Other
    end.

insert_delivery(D, BotId, Url, AttemptCount) ->
    Host = host_of(Url),
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO public.bot_delivery"
            " (delivery_id, bot_id, event_type, payload, correlation_id, idempotency_key,"
            "  webhook_url, webhook_host, pinned_ip, attempt_count, next_retry_at)"
            " VALUES ($1, $2, 'message', '{}'::jsonb, $3, $4, $5, $6, $6, $7,"
            "         NOW() + INTERVAL '1 hour')"
        >>,
        [D, integer_to_binary(BotId), corr(), <<"bwd:", D/binary>>, Url, Host, AttemptCount]
    ),
    {ok, Delivery} = bot_webhook_delivery_repo:get_delivery(D),
    Delivery.

attempts(D) ->
    {ok, Rows} = elib_pg:query(
        <<
            "SELECT attempt_no, status_class, http_status, error_trunc"
            " FROM public.bot_delivery_attempt WHERE delivery_id = $1"
            " ORDER BY created_at"
        >>,
        [D]
    ),
    Rows.

cleanup_delivery(D, BotId) ->
    {ok, _} = elib_pg:execute(
        <<"DELETE FROM public.bot_delivery WHERE delivery_id = $1">>, [D]
    ),
    {ok, _} = elib_pg:execute(<<"DELETE FROM public.bot WHERE user_id = $1">>, [BotId]),
    {ok, _} = elib_pg:execute(<<"DELETE FROM public.\"user\" WHERE id = $1">>, [BotId]),
    ok.

run_fixture_delivery(Status, AttemptCount, AssertFun) ->
    {Port, Tab} = start_fixture(Status),
    Url = iolist_to_binary(["http://127.0.0.1:", integer_to_binary(Port), "/hook"]),
    BotId = setup_bot(Url),
    D = did(),
    try
        Delivery = insert_delivery(D, BotId, Url, AttemptCount),
        _ = bot_webhook_delivery_worker:execute(Delivery),
        AssertFun(D, Tab)
    after
        cleanup_fixture(Tab),
        cleanup_delivery(D, BotId)
    end.

success_records_one_attempt_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        run_fixture_delivery(200, 0, fun(D, Tab) ->
            ok = wait_status(D, <<"success">>, 10),
            {ok, Row} = bot_webhook_delivery_repo:get_delivery(D),
            ?assertEqual(1, maps:get(<<"attempt_count">>, Row)),
            ?assertEqual(1, ets:lookup_element(Tab, conn, 2)),
            ?assertMatch(
                [
                    #{
                        <<"attempt_no">> := 1,
                        <<"status_class">> := <<"2xx">>,
                        <<"http_status">> := 200
                    }
                ],
                attempts(D)
            )
        end)
    end).

server_error_retries_with_one_attempt_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        run_fixture_delivery(503, 0, fun(D, Tab) ->
            ok = wait_status(D, <<"retry">>, 10),
            {ok, Row} = bot_webhook_delivery_repo:get_delivery(D),
            ?assertEqual(1, maps:get(<<"attempt_count">>, Row)),
            ?assertEqual(1, ets:lookup_element(Tab, conn, 2)),
            ?assertMatch(
                [
                    #{
                        <<"attempt_no">> := 1,
                        <<"status_class">> := <<"5xx">>,
                        <<"http_status">> := 503
                    }
                ],
                attempts(D)
            )
        end)
    end).

client_error_is_dead_after_one_attempt_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        run_fixture_delivery(400, 0, fun(D, Tab) ->
            ok = wait_status(D, <<"dead">>, 10),
            ?assertEqual(1, ets:lookup_element(Tab, conn, 2)),
            ?assertMatch(
                [
                    #{
                        <<"attempt_no">> := 1,
                        <<"status_class">> := <<"4xx">>,
                        <<"http_status">> := 400
                    }
                ],
                attempts(D)
            )
        end)
    end).

private_target_is_dead_without_connection_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        {Port, Tab} = start_fixture(200),
        Url = iolist_to_binary(["https://127.0.0.1:", integer_to_binary(Port), "/hook"]),
        BotId = setup_bot(Url),
        D = did(),
        try
            Delivery = insert_delivery(D, BotId, Url, 0),
            ok = bot_webhook_delivery_worker:execute(Delivery),
            ok = wait_status(D, <<"dead">>, 10),
            ?assertEqual(0, ets:lookup_element(Tab, conn, 2)),
            ?assertMatch(
                [#{<<"attempt_no">> := 1, <<"status_class">> := <<"forbidden_host">>}],
                attempts(D)
            )
        after
            cleanup_fixture(Tab),
            cleanup_delivery(D, BotId)
        end
    end).

credential_error_is_bounded_retry_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        {Port, Tab} = start_fixture(200),
        Url = iolist_to_binary(["http://127.0.0.1:", integer_to_binary(Port), "/hook"]),
        BotId = setup_bot(Url),
        D = did(),
        try
            Delivery = insert_delivery(D, BotId, Url, 3),
            ok = application:unset_env(imboy, postgre_aes_key),
            ok = bot_webhook_delivery_worker:execute(Delivery),
            ok = wait_status(D, <<"dead">>, 10),
            ?assertEqual(0, ets:lookup_element(Tab, conn, 2)),
            ?assertMatch(
                [#{<<"attempt_no">> := 4, <<"status_class">> := <<"credential_error">>}],
                attempts(D)
            )
        after
            ok = application:set_env(imboy, postgre_aes_key, ?TEST_AES_KEY),
            cleanup_fixture(Tab),
            cleanup_delivery(D, BotId)
        end
    end).
