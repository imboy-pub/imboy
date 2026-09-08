-module(bot_webhook_delivery_worker_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% WH-01：交付 worker 行为（真库 + 真 guard/解析/sender + 本地 loopback TCP fixture）。
%%%   A03 2xx → success；5xx → retry（预算耗尽 → dead）；4xx → 不重试直接死信；
%%%   A05 SSRF 负例：私网/内网目标 guard 拒绝 → dead 且零外联；
%%%   A06 本地 loopback fixture（test profile 显式开放 http）走真 sender 全链。
%%% fixture：gen_tcp 监听 127.0.0.1 随机端口，按 ETS 配置返回状态码并计数连接。

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

%% bot 行（user_id FK → 共享库种子用户）+ AEAD verify_token
setup_bot(Url) ->
    {ok, _} = application:ensure_all_started(crypto),
    {ok, [#{<<"id">> := SeedUid}]} = elib_pg:query(
        <<"SELECT id FROM public.\"user\" ORDER BY id LIMIT 1">>, []
    ),
    BotId = SeedUid,
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO public.bot (user_id, owner_uid, name, webhook_url, verify_token,"
            " created_at, updated_at) VALUES ($1, $1, 'wt', $2, 'legacy', NOW(), NOW())"
            " ON CONFLICT (user_id) DO UPDATE SET webhook_url = EXCLUDED.webhook_url"
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
