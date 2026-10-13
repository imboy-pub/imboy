-module(cache_channel_commit_tests).
-include_lib("eunit/include/eunit.hrl").

channel_commit_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(committed(subscribe)) end,
        fun(_) -> ?_test(committed(unsubscribe)) end,
        fun(_) -> ?_test(committed(remove)) end,
        fun(_) -> ?_test(rolled_back(subscribe)) end,
        fun(_) -> ?_test(rolled_back(unsubscribe)) end,
        fun(_) -> ?_test(rolled_back(remove)) end
    ]}.
setup() ->
    C = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    meck:new(pooler, [no_link, passthrough]),
    meck:expect(pooler, take_member, fun(pgsql) -> C end),
    meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
    meck:new(config_ds, [no_link, passthrough]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:new(epgsql, [no_link, passthrough]),
    meck:expect(epgsql, with_transaction, fun(Conn, F, _) ->
        Result = F(Conn),
        ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)),
        Result
    end),
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, commit} end),
    meck:new(imboy_cache, [no_link, passthrough]),
    meck:expect(imboy_cache, invalidate, fun(Key) ->
        ?assertEqual(1, meck:num_calls(epgsql, get_cmd_status, 1)),
        ?assertEqual(1, meck:num_calls(pooler, return_member, 2)),
        ?assert(lists:member(Key, [{channel_subs, 11}, {channel, 11}])),
        ok
    end),
    meck:new(workspace_guard, [no_link, passthrough]),
    meck:expect(workspace_guard, ensure_writable_tx, fun(_, {channel, 11}) -> ok end),
    meck:new(channel_subscription_repo, [no_link, passthrough]),
    meck:expect(channel_subscription_repo, upsert_active, fun(_, 11, 1001) -> {ok, true} end),
    meck:expect(channel_subscription_repo, delete, fun(_, 11, _) -> {ok, 1} end),
    meck:new(channel_repo, [no_link, passthrough]),
    meck:expect(channel_repo, increment_subscribers, fun(_, 11, _) -> {ok, 1} end),
    meck:new(micro_community_access_repo, [no_link, passthrough]),
    meck:expect(micro_community_access_repo, lock_channel_tx, fun(_, 11) -> locked end),
    meck:new(micro_community_access_ds, [no_link, passthrough]),
    meck:expect(micro_community_access_ds, revoke_channel_tx, fun(_, 11, _) -> ok end),
    meck:new(channel_logic_common, [no_link, passthrough]),
    meck:expect(channel_logic_common, get_user_role, fun
        (11, 1001) -> 2;
        (11, 2002) -> 1
    end),
    meck:expect(channel_logic_common, log_channel_action, fun(_, _, _, _, _, _) -> ok end),
    meck:new(channel_logic_notify, [no_link, passthrough]),
    meck:expect(channel_logic_notify, notify_channel_unsubscribed, fun(_, _) -> ok end),
    C.
cleanup(C) ->
    [
        meck:unload(M)
     || M <- [
            channel_logic_notify,
            channel_logic_common,
            micro_community_access_ds,
            micro_community_access_repo,
            channel_repo,
            channel_subscription_repo,
            workspace_guard,
            imboy_cache,
            epgsql,
            config_ds,
            pooler
        ]
    ],
    C ! stop.
call(subscribe) -> channel_ds:subscribe(11, 1001);
call(unsubscribe) -> channel_ds:unsubscribe(11, 1001);
call(remove) -> channel_logic_subscription:remove_subscriber(1001, 11, 2002).
committed(Op) ->
    ?assertEqual(ok, call(Op)),
    ?assertEqual(2, meck:num_calls(imboy_cache, invalidate, 1)).
rolled_back(Op) ->
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, rollback} end),
    ?assertMatch({error, _}, call(Op)),
    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)),
    ?assertEqual(0, meck:num_calls(channel_logic_notify, notify_channel_unsubscribed, 2)).
