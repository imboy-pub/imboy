-module(bot_webhook_delivery_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% WH-01：outbox 仓库契约（真库）：幂等入队、认领、状态机、死信重放。

uid() ->
    erlang:unique_integer([positive]) +
        erlang:phash2(binary:encode_hex(crypto:strong_rand_bytes(8))).

did() ->
    iolist_to_binary(["dlv-", integer_to_binary(uid())]).

base(D) ->
    #{
        delivery_id => D,
        bot_id => 100,
        event_type => <<"message">>,
        payload => <<"{}">>,
        correlation_id => corr(),
        idempotency_key => <<"bwd:", D/binary>>,
        webhook_host => <<"example.com">>
    }.

corr() ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"corr-", Bin/binary>>.

insert_idempotent_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        {ok, inserted} = bot_webhook_delivery_repo:insert(base(D)),
        {ok, duplicate} = bot_webhook_delivery_repo:insert(base(D)),
        {ok, Row} = bot_webhook_delivery_repo:get_delivery(D),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Row)),
        ?assertMatch(<<"corr-", _/binary>>, maps:get(<<"correlation_id">>, Row))
    end).

claim_due_only_due_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        {ok, inserted} = bot_webhook_delivery_repo:insert(base(D)),
        {ok, Rows} = bot_webhook_delivery_repo:claim_due(50),
        ?assert(lists:any(fun(R) -> maps:get(<<"delivery_id">>, R) =:= D end, Rows)),
        %% 已投递行不再被认领
        {ok, _} = bot_webhook_delivery_repo:mark_success(D),
        {ok, Rows2} = bot_webhook_delivery_repo:claim_due(50),
        ?assertNot(lists:any(fun(R) -> maps:get(<<"delivery_id">>, R) =:= D end, Rows2))
    end).

mark_retry_and_dead_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        {ok, inserted} = bot_webhook_delivery_repo:insert(base(D)),
        {ok, _} = bot_webhook_delivery_repo:mark_retry(D, 30, 1, <<>>),
        {ok, Row} = bot_webhook_delivery_repo:get_delivery(D),
        ?assertEqual(<<"retry">>, maps:get(<<"status">>, Row)),
        ?assertEqual(1, maps:get(<<"attempt_count">>, Row)),
        {ok, _} = bot_webhook_delivery_repo:mark_dead(D, 4),
        {ok, Row2} = bot_webhook_delivery_repo:get_delivery(D),
        ?assertEqual(<<"dead">>, maps:get(<<"status">>, Row2)),
        ?assertEqual(4, maps:get(<<"attempt_count">>, Row2))
    end).

replay_dead_only_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        {ok, inserted} = bot_webhook_delivery_repo:insert(base(D)),
        %% 非 dead 不可重放
        {error, not_dead} = bot_webhook_delivery_repo:replay(D),
        {ok, _} = bot_webhook_delivery_repo:mark_dead(D, 4),
        {ok, reused_delivery} = bot_webhook_delivery_repo:replay(D),
        {ok, Row} = bot_webhook_delivery_repo:get_delivery(D),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Row)),
        %% delivery_id 不变（重放不换 ID）
        ?assertEqual(D, maps:get(<<"delivery_id">>, Row)),
        %% 不存在
        {error, notfound} = bot_webhook_delivery_repo:replay(did())
    end).

attempt_audit_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        {ok, inserted} = bot_webhook_delivery_repo:insert(base(D)),
        ok = bot_webhook_delivery_repo:insert_attempt(D, #{
            id => <<"bda-", D/binary>>,
            attempt_no => 1,
            status_class => <<"5xx">>,
            http_status => 502,
            latency_ms => 42,
            error_trunc => <<"bad gw">>
        }),
        ok
    end).
