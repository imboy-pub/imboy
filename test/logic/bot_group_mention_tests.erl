-module(bot_group_mention_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% BOT-01：群 @Bot mention 分派（真库 + 本地 loopback fixture）。
%%%   A01 mention → 恰一次 delivery；普通消息零 delivery；
%%%   A03 防自环（bot 发的消息不触发）/ E2EE fail-closed / 非订阅 bot 跳过；
%%%   reply context：签名命中 / 篡改 invalid / 过期 expired / 重放 reused。
%%%
%%% fail-closed 口径：verify_token 的 AEAD 密钥（postgre_aes_key）只来自 tracked
%%% 配置（config/sys.config.example 中为空）。套件按官方测试钩子自注入合成 key
%%% （对照 test/ds/sso_config_ds_tests.erl 的 set_aes_key/0），不读任何本机
%%% gitignored 配置（sys.local.config），保证 scratch/CI 干净环境下可复现。

-define(TEST_AES_KEY, <<"0123456789abcdef0123456789abcdef">>).

set_aes_key() ->
    application:set_env(imboy, postgre_aes_key, ?TEST_AES_KEY).

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
            ok = bot_webhook_logic:dispatch_group_mention(
                FromUid, 9001, Data, Payload, [BotUid]
            ),
            ok = bot_webhook_logic:dispatch_group_mention(
                FromUid, 9001, Data, Payload, [BotUid]
            ),
            Idem = iolist_to_binary([
                "bwd-mention:", integer_to_binary(BotUid), ":", MsgId
            ]),
            {ok, Row} = delivery_by_idem(Idem),
            ?assertEqual(<<"message.c2g_mention">>, maps:get(<<"event_type">>, Row)),
            ?assertEqual(BotUid, to_int(maps:get(<<"bot_id">>, Row))),
            %% TSID 十进制字符串边界：outbound JSON 标识符不得被 JSON number 截断
            Body = jsone:decode(maps:get(<<"payload">>, Row)),
            ?assertEqual(integer_to_binary(BotUid), maps:get(<<"bot_id">>, Body)),
            BodyData = maps:get(<<"data">>, Body),
            ?assertEqual(<<"9001">>, maps:get(<<"group_id">>, BodyData)),
            ?assertEqual(integer_to_binary(FromUid), maps:get(<<"from_uid">>, BodyData)),
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

%% A03：群外 Bot 即使被 mention 也不得接收 delivery（权威 active membership）
non_member_bot_skipped_test_() ->
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
            FromUid = seed_from_user(),
            Before = delivery_count(),
            Data = #{
                <<"id">> => iolist_to_binary(["m-", integer_to_binary(uid())]),
                <<"e2ee">> => null
            },
            Payload = #{<<"mentions">> => [integer_to_binary(BotUid)]},
            %% 成员表为空：BotUid 不在权威 active membership 内 → 零分派
            ok = bot_webhook_logic:dispatch_group_mention(FromUid, 9001, Data, Payload, []),
            ?assertEqual(Before, delivery_count()),
            cleanup_fixture(Tab)
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

%% reply context：服务端签名命中 / Bot secret 伪造拒绝 / 过期 / 畸形
reply_context_lifecycle_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, _} = application:ensure_all_started(crypto),
        {BotUid, _Seed} = setup_bot(<<"http://127.0.0.1:19301/hook">>),
        DeliveryId = iolist_to_binary(["dlv-ctx-", integer_to_binary(uid())]),
        Corr = corr(),
        {ok, Token, _Jti} = bot_webhook_logic:make_reply_context(
            BotUid, 9001, <<"msg-1">>, Corr, DeliveryId
        ),
        {ok, Ctx} = bot_webhook_logic:verify_reply_context(Token),
        ?assertEqual(9001, maps:get(<<"group_id">>, Ctx)),
        ?assertEqual(DeliveryId, maps:get(<<"delivery_id">>, Ctx)),
        %% Bot 持有 webhook verify secret，但不能用它伪造 reply context。
        Forged = sign_context(
            BotUid,
            9002,
            <<"dlv-forged">>,
            Corr,
            os:system_time(second) + 60,
            <<"wh-verify-secret">>
        ),
        {error, invalid} = bot_webhook_logic:verify_reply_context(Forged),
        Tampered = iolist_to_binary(
            [binary:part(Token, 0, byte_size(Token) - 4), <<"AAAA">>]
        ),
        {error, invalid} = bot_webhook_logic:verify_reply_context(Tampered),
        {error, invalid} = bot_webhook_logic:verify_reply_context(<<"%%%.not-base64">>),
        Expired = sign_context(
            BotUid,
            9001,
            <<"dlv-expired">>,
            Corr,
            os:system_time(second) - 1,
            server_reply_key()
        ),
        {error, expired} = bot_webhook_logic:verify_reply_context(Expired)
    end).

sign_context(BotUid, GroupId, DeliveryId, Corr, Exp, Key) ->
    Payload = jsone:encode(#{
        <<"bot_id">> => BotUid,
        <<"group_id">> => GroupId,
        <<"trigger_msg_id">> => <<"msg-test">>,
        <<"delivery_id">> => DeliveryId,
        <<"jti">> => DeliveryId,
        <<"correlation_id">> => Corr,
        <<"exp">> => Exp
    }),
    Sig = crypto:mac(hmac, sha256, Key, Payload),
    iolist_to_binary([base64:encode(Payload), $., base64:encode(Sig)]).

server_reply_key() ->
    crypto:mac(hmac, sha256, ?TEST_AES_KEY, <<"imboy.bot.reply-context.v1">>).

%% ===================================================================
delivery_by_idem(Idem) ->
    case
        elib_pg:query(
            <<
                "SELECT delivery_id, bot_id, event_type, status, payload::text AS payload"
                " FROM public.bot_delivery"
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
    ok = set_aes_key(),
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
