-module(cache_mutation_hooks_tests).
-include_lib("eunit/include/eunit.hrl").

%% Repo 合同为不带 Conn 的单语句自动提交；只验证真实 DS 的返回后钩子。
mutation_hooks_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(billing_success(create)) end,
        fun(_) -> ?_test(billing_success(update)) end,
        fun(_) -> ?_test(billing_success(renew)) end,
        fun(_) -> ?_test(billing_errors_do_not_invalidate()) end,
        fun(_) -> ?_test(billing_noop_does_not_invalidate()) end,
        fun(_) -> ?_test(words_create_invalidates_after_write()) end,
        fun(_) -> ?_test(words_delete_invalidates_after_write()) end,
        fun(_) -> ?_test(words_error_and_noop_do_not_invalidate()) end
    ]}.

setup() ->
    meck:new(imboy_cache, [no_link, passthrough]),
    meck:expect(imboy_cache, invalidate, fun(Key) ->
        true = get(mutation_repo_returned),
        put(mutation_cache_key, Key),
        ok
    end),
    meck:new(billing_subscription_repo, [no_link, passthrough]),
    meck:new(sensitive_word_repo, [no_link, passthrough]),
    ok.
cleanup(_) ->
    [meck:unload(M) || M <- [sensitive_word_repo, billing_subscription_repo, imboy_cache]].

result(Value) ->
    put(mutation_repo_returned, true),
    Value.
mock_billing(Result) ->
    meck:expect(billing_subscription_repo, create, fun(_) -> result(Result) end),
    meck:expect(billing_subscription_repo, update, fun(_, _) -> result(Result) end),
    meck:expect(billing_subscription_repo, renew, fun(_, _, _) -> result(Result) end).
call(create) -> billing_subscription_ds:create(#{});
call(update) -> billing_subscription_ds:update(1, #{status => 3});
call(renew) -> billing_subscription_ds:renew(1, 1000, 2000).

billing_success(Op) ->
    mock_billing({ok, 1}),
    ?assertEqual({ok, 1}, call(Op)),
    ?assertEqual({billing_meter, active_sub, 0}, erase(mutation_cache_key)),
    ?assertEqual(1, meck:num_calls(imboy_cache, invalidate, 1)).
billing_errors_do_not_invalidate() ->
    mock_billing({error, unavailable}),
    [?assertEqual({error, unavailable}, call(Op)) || Op <- [create, update, renew]],
    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)).
billing_noop_does_not_invalidate() ->
    mock_billing({ok, 0}),
    [?assertEqual({ok, 0}, call(Op)) || Op <- [update, renew]],
    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)).

words_create_invalidates_after_write() ->
    meck:expect(sensitive_word_repo, insert, fun(_, _, _) -> result({ok, created, 7}) end),
    ?assertEqual(
        {ok, created, 7}, moderation_ds:sensitive_word_create(<<"word">>, <<>>, <<"high">>)
    ),
    ?assertEqual({moderation_sensitive_words}, erase(mutation_cache_key)).
words_delete_invalidates_after_write() ->
    meck:expect(sensitive_word_repo, delete_by_id, fun(_) -> result({ok, 1}) end),
    ?assertEqual({ok, 1}, moderation_ds:sensitive_word_delete(7)),
    ?assertEqual({moderation_sensitive_words}, erase(mutation_cache_key)).
words_error_and_noop_do_not_invalidate() ->
    meck:expect(sensitive_word_repo, insert, fun(_, _, _) -> result({error, failed}) end),
    ?assertEqual(
        {error, failed}, moderation_ds:sensitive_word_create(<<"word">>, <<>>, <<"high">>)
    ),
    meck:expect(sensitive_word_repo, insert, fun(_, _, _) -> result({ok, skipped}) end),
    ?assertEqual({ok, skipped}, moderation_ds:sensitive_word_create(<<"word">>, <<>>, <<"high">>)),
    meck:expect(sensitive_word_repo, delete_by_id, fun(_) -> result({ok, 0}) end),
    ?assertEqual({ok, 0}, moderation_ds:sensitive_word_delete(7)),
    meck:expect(sensitive_word_repo, delete_by_id, fun(_) -> result({error, failed}) end),
    ?assertEqual({error, failed}, moderation_ds:sensitive_word_delete(7)),
    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)).
