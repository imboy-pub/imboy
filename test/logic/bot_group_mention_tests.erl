-module(bot_group_mention_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% BOT-01：群 @Bot mention 分派（真库 + 本地 loopback fixture）。
%%%   A01 mention → 恰一次 delivery；普通消息零 delivery；
%%%   A03 防自环（bot 发的消息不触发）/ E2EE fail-closed / 非订阅 bot 跳过；
%%%   reply context：签名命中 / 篡改 invalid / 过期 expired / 重放 reused。

uid() ->
    erlang:unique_integer([positive]) +
        erlang:phash2(binary:encode_hex(crypto:strong_rand_bytes(8))).

corr() ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"corr-", Bin/binary>>.

%% A01：mention → 恰一次 delivery（幂等键去重，重复分派不重复）
mention_dispatch_once_test_() ->
    {timeout, 30,
        ?TEST_WITH_DB(fun() ->
            {Port, Tab, _Cleanup} = start_fixture(200),
            {BotUid, _} = setup_bot(
                iolist_to_binary([
                    "http://127.0.0.1:",
                    integer_to_binary(Port),
                    "/hook"
                ])
            ),
            FromUid = seed_from_user(),
            MsgId = iolist_to_binary(["m-", integer_to_binary(uid())]),
            Data = #{<<"id">> => MsgId, <<"e2ee">> => null},
            Payload = #{<<"mentions">> => [integer_to_binary(BotUid)]},
            ok = bot_webhook_logic:dispatch_group_mention(FromUid, 9001, Data, Payload, []),
            ok = bot_webhook_logic:dispatch_group_mention(FromUid, 9001, Data, Payload, []),
            Idem = iolist_to_binary(["bwd-mention:", BotUid, ":", MsgId]),
            {ok, Row} = delivery_by_idem(Idem),
            ?assertEqual(<<"message.c2g_mention">>, maps:get(<<"event_type">>, Row)),
            ?assertEqual(BotUid, to_int(maps:get(<<"bot_id">>, Row))),
            ok = cleanup_fixture(Tab)
        end)}.

%% A01 负例：无 mention → 零 delivery
no_mention_no_delivery_test_() ->
    {timeout, 30,
        ?TEST_WITH_DB(fun() ->
            Before = delivery_count(),
            FromUid = seed_from_user(),
            Data = #{
                <<"id">> => iolist_to_binary(["m-", integer_to_binary(uid())]),
                <<"e2ee">> => null
            },
            ok = bot_webhook_logic:dispatch_group_mention(
                FromUid,
                9001,
                Data,
                #{<<"text">> => <<"hi">>},
                []
            ),
            ?assertEqual(Before, delivery_count())
        end)}.

%% A03：bot 自身发送 → 零分派（防 Bot-to-Bot 环）
self_bot_sender_skipped_test_() ->
    {timeout, 30,
        ?TEST_WITH_DB(fun() ->
            {Port, Tab, _C} = start_fixture(200),
            {BotUid, _} = setup_bot(
                iolist_to_binary([
                    "http://127.0.0.1:",
                    integer_to_binary(Port),
                    "/hook"
                ])
            ),
            Before = delivery_count(),
            Data = #{
                <<"id">> => iolist_to_binary(["m-", integer_to_binary(uid())]),
                <<"e2ee">> => null
            },
            Payload = #{<<"mentions">> => [integer_to_binary(BotUid)]},
            %% 发送者就是 bot 自己
            ok = bot_webhook_logic:dispatch_group_mention(BotUid, 9001, Data, Payload, []),
            ?assertEqual(Before, delivery_count()),
            cleanup_fixture(Tab)
        end)}.

%% A03：E2EE fail-closed（e2ee 字段非 null → 零分派）
e2ee_message_skipped_test_() ->
    ?TEST_WITH_DB(fun() ->
        {Port, Tab, _C} = start_fixture(200),
        {BotUid, _} = setup_bot(
            iolist_to_binary([
                "http://127.0.0.1:",
                integer_to_binary(Port),
                "/hook"
            ])
        ),
        FromUid = seed_from_user(),
        Before = delivery_count(),
        Data = #{
            <<"id">> => iolist_to_binary(["m-", integer_to_binary(uid())]),
            <<"e2ee">> => 1
        },
        Payload = #{<<"mentions">> => [integer_to_binary(BotUid)]},
        ok = bot_webhook_logic:dispatch_group_mention(FromUid, 9001, Data, Payload, []),
        ?assertEqual(Before, delivery_count()),
        cleanup_fixture(Tab)
    end).

%% reply context：签名命中 / 过期 / 重放 / 篡改
reply_context_lifecycle_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, _} = application:ensure_all_started(crypto),
        {BotUid, _Seed} = setup_bot(<<"http://127.0.0.1:19301/hook">>),
        Secret = <<"wh-verify-secret">>,
        {ok, Token, _Jti} = bot_webhook_logic:make_reply_context(
            BotUid, 9001, <<"msg-1">>, corr()
        ),
        %% 篡改 payload → invalid（jti 未消耗）
        Tampered = iolist_to_binary(
            [binary:part(Token, 0, byte_size(Token) - 4), <<"AAAA">>]
        ),
        {error, invalid} = bot_webhook_logic:verify_reply_context(Tampered, Secret),
        %% 错误密钥 → invalid
        {error, invalid} = bot_webhook_logic:verify_reply_context(
            Token, <<"wrong-secret">>
        ),
        %% 首次合法验证 → ok（消耗 jti）
        {ok, Ctx} = bot_webhook_logic:verify_reply_context(Token, Secret),
        ?assertEqual(9001, maps:get(<<"group_id">>, Ctx)),
        %% 重放 → reused（jti 一次性）
        {error, reused} = bot_webhook_logic:verify_reply_context(Token, Secret)
    end).

%% ===================================================================
delivery_by_idem(Idem) ->
    case
        elib_pg:query(
            <<
                "SELECT delivery_id, bot_id, event_type, status FROM public.bot_delivery"
                " WHERE idempotency_key = $1"
            >>,
            [Idem]
        )
    of
        {ok, [Row]} -> {ok, Row};
        Other -> Other
    end.

delivery_count() ->
    case elib_pg:query(<<"SELECT count(*) AS n FROM public.bot_delivery">>, []) of
        {ok, [#{<<"n">> := N}]} -> N;
        _ -> -1
    end.

to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    try binary_to_integer(V) of
        I -> I
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.

%% bot 行 + AEAD verify token（user_id 锚定共享库种子用户）
setup_bot(Url) ->
    {ok, _} = application:ensure_all_started(crypto),
    {ok, [#{<<"id">> := SeedUid}]} = elib_pg:query(
        <<"SELECT id FROM public.\"user\" ORDER BY id LIMIT 1">>, []
    ),
    BotUid = SeedUid,
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO public.bot (user_id, owner_uid, name, webhook_url, verify_token,"
            " events, created_at, updated_at)"
            " VALUES ($1, $1, 'wt', $2, 'legacy',"
            " '[\"message.c2g_mention\"]'::jsonb, NOW(), NOW())"
            " ON CONFLICT (user_id) DO UPDATE SET webhook_url = EXCLUDED.webhook_url,"
            " events = EXCLUDED.events"
        >>,
        [BotUid, Url]
    ),
    {ok, _} = bot_repo:set_verify_token_enc(BotUid, <<"wh-verify-secret">>),
    {BotUid, SeedUid}.

%% 普通发送者：不能与 bot 的 user_id 相同（否则触发防自环）
seed_from_user() ->
    {ok, [#{<<"id">> := SeedUid}]} = elib_pg:query(
        <<"SELECT id FROM public.\"user\" ORDER BY id LIMIT 1">>, []
    ),
    SeedUid + 1000.

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
    Acceptor = spawn(fun() -> fixture_accept(Listen, Tab) end),
    {Port, Tab, {Listen, Acceptor}}.

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
    catch ets:delete(Tab),
    ok.
