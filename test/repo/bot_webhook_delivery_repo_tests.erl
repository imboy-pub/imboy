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
        webhook_url => <<"https://example.com/hook">>,
        webhook_host => <<"example.com">>,
        pinned_ip => <<"93.184.216.34">>
    }.

corr() ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"corr-", Bin/binary>>.

audit_id(Prefix) ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(12), lowercase),
    <<Prefix/binary, Bin/binary>>.

insert_idempotent_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        {ok, inserted} = bot_webhook_delivery_repo:insert(base(D)),
        ChangedTarget = (base(D))#{
            webhook_url => <<"https://changed.example/hook">>,
            webhook_host => <<"changed.example">>,
            pinned_ip => <<"203.0.113.10">>
        },
        {ok, duplicate} = bot_webhook_delivery_repo:insert(ChangedTarget),
        {ok, Row} = bot_webhook_delivery_repo:get_delivery(D),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Row)),
        ?assertMatch(<<"corr-", _/binary>>, maps:get(<<"correlation_id">>, Row)),
        ?assertEqual(<<"https://example.com/hook">>, maps:get(<<"webhook_url">>, Row)),
        ?assertEqual(<<"example.com">>, maps:get(<<"webhook_host">>, Row)),
        ?assertEqual(<<"93.184.216.34">>, maps:get(<<"pinned_ip">>, Row))
    end).

claim_due_only_due_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        {ok, inserted} = bot_webhook_delivery_repo:insert(base(D)),
        %% 共享库跨轮残留：历史轮认领行（60s 租约到期回 due）可堆积超过
        %% LIMIT 50，ORDER BY next_retry_at 下新插入的 D 排队尾认领不到
        %% （be-02 偶发失败实证）。同款 defensive UPDATE（见 exclusive 用例）
        %% 把 D 定为最老 due，断言只依赖本用例试数据。
        {ok, _} = elib_pg:execute(
            <<
                "UPDATE public.bot_delivery SET next_retry_at = NOW() - INTERVAL '1 day'"
                " WHERE delivery_id = $1"
            >>,
            [D]
        ),
        {ok, Rows} = bot_webhook_delivery_repo:claim_due(50),
        ?assert(lists:any(fun(R) -> maps:get(<<"delivery_id">>, R) =:= D end, Rows)),
        %% 已投递行不再被认领
        {ok, _} = bot_webhook_delivery_repo:mark_success(D),
        {ok, Rows2} = bot_webhook_delivery_repo:claim_due(50),
        ?assertNot(lists:any(fun(R) -> maps:get(<<"delivery_id">>, R) =:= D end, Rows2))
    end).

claim_due_is_exclusive_across_workers_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        {ok, inserted} = bot_webhook_delivery_repo:insert(base(D)),
        {ok, _} = elib_pg:execute(
            <<
                "UPDATE public.bot_delivery SET next_retry_at = NOW() - INTERVAL '1 day'"
                " WHERE delivery_id = $1"
            >>,
            [D]
        ),
        Parent = self(),
        Workers = [
            spawn(fun() -> Parent ! {claimed, bot_webhook_delivery_repo:claim_due(1)} end)
         || _ <- lists:seq(1, 16)
        ],
        Results = [
            receive
                {claimed, Result} -> Result
            after 10000 ->
                timeout
            end
         || _ <- Workers
        ],
        Claimed = [
            Row
         || {ok, Rows} <- Results,
            Row <- Rows,
            maps:get(<<"delivery_id">>, Row) =:= D
        ],
        ?assertEqual(1, length(Claimed)),
        ?assertNot(lists:member(timeout, Results))
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
        D = audit_id(<<"dlv-">>),
        Corr = corr(),
        TaskId = audit_id(<<"task-">>),
        EventId = audit_id(<<"event-">>),
        ok = elib_pg:with_tx(fun(Conn) ->
            ok = agent_hub_audit_repo:record_task_start_tx(Conn, Corr, TaskId),
            ok = agent_hub_audit_repo:record_transition_tx(
                Conn, Corr, TaskId, EventId, <<"working">>, <<"submitted">>
            )
        end),
        {ok, inserted} = bot_webhook_delivery_repo:insert(
            (base(D))#{correlation_id => Corr}
        ),
        %% 非 dead 不可重放
        {error, not_dead} = bot_webhook_delivery_repo:replay(D),
        {ok, _} = bot_webhook_delivery_repo:mark_dead(D, 4),
        {ok, reused_delivery} = bot_webhook_delivery_repo:replay(D),
        {ok, Row} = bot_webhook_delivery_repo:get_delivery(D),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Row)),
        %% delivery_id 不变（重放不换 ID）
        ?assertEqual(D, maps:get(<<"delivery_id">>, Row)),
        {ok, Audit} = agent_hub_audit_repo:list_by_correlation(Corr),
        [AuditDelivery] = [
            A
         || A = #{<<"entity_type">> := <<"delivery">>, <<"entity_id">> := AuditId} <- Audit,
            AuditId =:= D
        ],
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, AuditDelivery)),
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

reply_context_is_consumed_once_in_db_test_() ->
    ?TEST_WITH_DB(fun() ->
        D = did(),
        Corr = corr(),
        Token = <<"opaque-server-signed-reply-context">>,
        {ok, inserted} = bot_webhook_delivery_repo:insert(
            (base(D))#{reply_context => Token, correlation_id => Corr}
        ),
        {ok, consumed} = bot_webhook_delivery_repo:consume_reply_context(D, Token, 100, Corr),
        {error, notfound} = bot_webhook_delivery_repo:consume_reply_context(D, Token, 100, Corr),
        {ok, Row} = bot_webhook_delivery_repo:get_delivery(D),
        ?assertEqual(<<>>, maps:get(<<"reply_context">>, Row))
    end).
