-module(cache_security_hooks_tests).
-include_lib("eunit/include/eunit.hrl").

%% 真提交动作队列 / 真 depcache；数据库驱动 mock，不代替真 PG 验收。
security_hooks_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(auth_commit_is_deferred()) end,
        fun(_) -> ?_test(admin_commit_is_deferred()) end,
        fun(_) -> ?_test(admin_rollback_does_not_evict()) end,
        fun(_) -> ?_test(auth_rollback_does_not_evict()) end,
        fun(_) -> ?_test(moment_commit_is_deferred()) end,
        fun(_) -> ?_test(moment_rollback_does_not_evict()) end,
        fun(_) -> ?_test(acl_failure_is_not_an_empty_denylist()) end,
        fun(_) -> ?_test(acl_known_empty_and_denylist_are_distinct()) end,
        fun(_) -> ?_test(malformed_acl_is_denied()) end,
        fun(_) -> ?_test(csrf_key_does_not_publish()) end
    ]}.

setup() ->
    PreviousSync = application:get_env(imboy, dsync_enabled),
    PreviousFenced = application:get_env(imboy, cache_fenced_namespaces),
    Saved = [{dsync_enabled, PreviousSync}, {cache_fenced_namespaces, PreviousFenced}],
    cache_fixture_helper:setup(Saved, fun() -> setup_components(Saved) end).
setup_components(Saved) ->
    application:set_env(imboy, cache_fenced_namespaces, []),
    application:set_env(imboy, dsync_enabled, false),
    D = cache_fixture_helper:acquire(imboy_cache, fun() ->
        depcache:start_link(imboy_cache, #{memory_max => 64})
    end),
    depcache:flush({moment_acl, 555}, imboy_cache),
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    cache_fixture_helper:on_failure(fun() -> Conn ! stop end),
    cache_fixture_helper:mock(pooler, [no_link, passthrough]),
    meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
    meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
    cache_fixture_helper:mock(config_ds, [no_link, passthrough]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    cache_fixture_helper:mock(epgsql, [no_link, passthrough]),
    meck:expect(epgsql, with_transaction, fun(C, F, _) -> F(C) end),
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, commit} end),
    meck:expect(epgsql, equery, fun(_, _, _) -> {ok, 1, [], [{2}]} end),
    cache_fixture_helper:mock(imboy_cache, [no_link, passthrough]),
    meck:expect(imboy_cache, invalidate, fun(Key) ->
        ?assertEqual(1, meck:num_calls(pooler, return_member, 2)),
        ?assertEqual(1, meck:num_calls(epgsql, get_cmd_status, 1)),
        put(security_evicted, Key),
        ok
    end),
    cache_fixture_helper:mock(elib_pg, [no_link, passthrough]),
    meck:expect(elib_pg, query, fun(_, _) -> {error, unavailable} end),
    cache_fixture_helper:mock(moment_post_repo, [no_link, passthrough]),
    meck:expect(moment_post_repo, add, fun(_, _) -> {ok, 555} end),
    cache_fixture_helper:mock(attachment_ds, [no_link, passthrough]),
    meck:expect(attachment_ds, bind_moment_scope_ref, fun(_, _, _, _) -> ok end),
    cache_fixture_helper:mock(moment_timeline_repo, [no_link, passthrough]),
    meck:expect(moment_timeline_repo, upsert_batch, fun(_, _, _, _, _) -> {ok, 1} end),
    cache_fixture_helper:mock(friend_ds, [no_link, passthrough]),
    meck:expect(friend_ds, list_by_uid, fun(_) -> [] end),
    meck:expect(friend_ds, is_friend, fun(_, _) -> true end),
    cache_fixture_helper:mock(user_denylist_repo, [no_link, passthrough]),
    meck:expect(user_denylist_repo, in_denylist, fun(_, _) -> 0 end),
    cache_fixture_helper:mock(moment_post_acl_repo, [no_link, passthrough]),
    meck:expect(moment_post_acl_repo, replace_for_post, fun(_, _, _, _) -> ok end),
    {Conn, D, Saved}.

cleanup({Conn, D, Saved}) ->
    Modules = [
        moment_post_acl_repo,
        user_denylist_repo,
        friend_ds,
        moment_timeline_repo,
        attachment_ds,
        moment_post_repo,
        elib_pg,
        imboy_cache,
        epgsql,
        config_ds,
        pooler
    ],
    Actions =
        [fun() -> meck:unload(Mod) end || Mod <- Modules] ++
            [
                fun() -> Conn ! stop end,
                fun() -> depcache:flush({moment_acl, 555}, imboy_cache) end,
                fun() -> cache_fixture_helper:release(D) end
            ],
    try
        cache_fixture_helper:cleanup(Actions)
    after
        cache_fixture_helper:restore_env(Saved)
    end.

call(auth) -> auth_session_ds:bump(1001);
call(admin) -> adm_session_ds:bump(1001);
call(moment) -> moment_ds:create_post(1001, #{content => <<"post">>}).

assert_deferred(Op, Expected, Key) ->
    erase(security_evicted),
    meck:expect(epgsql, with_transaction, fun(C, F, _) ->
        Result = F(C),
        ?assertEqual(undefined, get(security_evicted)),
        Result
    end),
    ?assertEqual(Expected, call(Op)),
    ?assertEqual(Key, erase(security_evicted)).

auth_commit_is_deferred() -> assert_deferred(auth, ok, {auth_session_epoch, 1001}).
moment_commit_is_deferred() -> assert_deferred(moment, {ok, 555}, {moment_acl, 555}).

assert_rollback(Op) ->
    erase(security_evicted),
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, rollback} end),
    ?assertMatch({error, _}, call(Op)),
    ?assertEqual(undefined, get(security_evicted)),
    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)).
auth_rollback_does_not_evict() -> assert_rollback(auth).
moment_rollback_does_not_evict() -> assert_rollback(moment).

post(Id, Visibility) ->
    #{
        <<"id">> => Id,
        <<"status">> => 1,
        <<"author_uid">> => 1001,
        <<"visibility">> => Visibility
    }.

acl_failure_is_not_an_empty_denylist() ->
    ?assertEqual({error, unavailable}, moment_post_acl_repo:list_for_post(777)),
    ?assertEqual(false, moment_ds:can_view_post(1002, post(777, 4))),
    ?assertEqual(false, moment_ds:can_view_post(1002, post(778, 3))),
    %% 零 TTL error 不驻留；PG 恢复后必须再读实际 ACL。
    meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
    ?assertEqual(true, moment_ds:can_view_post(1002, post(777, 4))).

acl_known_empty_and_denylist_are_distinct() ->
    meck:expect(elib_pg, query, fun(_, _) ->
        {ok, [#{<<"uid">> => 1002, <<"acl_type">> => 2}]}
    end),
    ?assertEqual(false, moment_ds:can_view_post(1002, post(779, 4))),
    ?assertEqual({[], [1002]}, moment_ds:list_post_acl(779)).

malformed_acl_is_denied() ->
    meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"uid">> => bad}]} end),
    ?assertEqual({error, malformed_acl_rows}, moment_post_acl_repo:list_for_post(780)),
    ?assertEqual(false, moment_ds:can_view_post(1002, post(780, 4))).

admin_commit_is_deferred() -> assert_deferred(admin, ok, {adm_session_epoch, 1001}).
admin_rollback_does_not_evict() -> assert_rollback(admin).

csrf_key_does_not_publish() ->
    application:set_env(imboy, dsync_enabled, true),
    cache_fixture_helper:mock(imboy_cache_sync, [no_link, passthrough]),
    try
        meck:expect(imboy_cache_sync, broadcast, fun(_) -> error(csrf_must_not_publish) end),
        Key = {local_cache, csrf, <<"synthetic_csrf_token">>},
        ?assertEqual(local, imboy_cache_policy:policy(Key)),
        ok = imboy_cache:set(Key, 1),
        ?assertEqual({ok, 1}, imboy_cache:get(Key)),
        ?assertEqual(0, meck:num_calls(imboy_cache_sync, broadcast, 1))
    after
        meck:unload(imboy_cache_sync)
    end.
