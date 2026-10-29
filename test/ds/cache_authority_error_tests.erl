-module(cache_authority_error_tests).
-include_lib("eunit/include/eunit.hrl").

%% DB 失败必须在业务失效前终止，不能被 Repo 的 ok 包装掩盖。
authority_errors_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(friend_confirm_error_does_not_invalidate()) end,
        fun(_) -> ?_test(friend_pending_error_does_not_invalidate()) end,
        fun(_) -> ?_test(friend_delete_pending_error_does_not_invalidate()) end,
        fun(_) -> ?_test(dnd_save_error_does_not_invalidate()) end,
        fun(_) -> ?_test(dnd_delete_error_does_not_invalidate()) end,
        fun(_) -> ?_test(denylist_add_error_does_not_invalidate()) end,
        fun(_) -> ?_test(denylist_remove_error_does_not_invalidate()) end,
        fun(_) -> ?_test(conversation_restore_error_does_not_invalidate()) end,
        fun(_) -> ?_test(successful_writes_invalidate()) end,
        fun(_) -> ?_test(conversation_direct_caller_propagates_error()) end
    ]}.
setup() ->
    PreviousDriver = application:get_env(imboy, sql_driver),
    application:set_env(imboy, sql_driver, pgsql),
    meck:new(imboy_cache, [no_link, passthrough]),
    meck:expect(imboy_cache, invalidate, fun(_) -> error(uncommitted_write_must_not_invalidate) end),
    meck:new(elib_pg, [no_link, passthrough]),
    meck:expect(elib_pg, execute, fun(_, _) -> {error, unavailable} end),
    meck:expect(elib_pg, execute_count, fun(_, _) -> {error, unavailable} end),
    meck:expect(elib_pg, query, fun(_, _) -> {error, unavailable} end),
    meck:new(elib_tsid, [no_link, passthrough]),
    meck:expect(elib_tsid, generate, fun(_) -> 777 end),
    PreviousDriver.
cleanup(PreviousDriver) ->
    try
        [meck:unload(M) || M <- [elib_tsid, elib_pg, imboy_cache]]
    after
        case PreviousDriver of
            undefined -> application:unset_env(imboy, sql_driver);
            {ok, Value} -> application:set_env(imboy, sql_driver, Value)
        end
    end.

not_invalidated() -> ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)).
friend_confirm_error_does_not_invalidate() ->
    ?assertError(
        friend_write_failed, friend_ds:confirm_friend(false, 1, 2, <<>>, #{}, <<>>, elib_dt:now())
    ),
    not_invalidated().
friend_pending_error_does_not_invalidate() ->
    ?assertError(friend_write_failed, friend_ds:insert_pending(1, 2, #{}, elib_dt:now())),
    not_invalidated().
friend_delete_pending_error_does_not_invalidate() ->
    ?assertError(friend_write_failed, friend_ds:delete_pending(1, 2)),
    not_invalidated().
dnd_save_error_does_not_invalidate() ->
    %% save/1 已收敛为值语义 {ok} | {error, term()}（raise 只剩 delete_by_uid）；
    %% DB 失败经 repo 透传 {error, unavailable}，写失败仍不得失效缓存。
    ?assertMatch(
        {error, _},
        user_dnd_rule_ds:save(#{
            <<"user_id">> => 1, <<"start_min">> => 60, <<"end_min">> => 120, <<"status">> => 1
        })
    ),
    not_invalidated().
dnd_delete_error_does_not_invalidate() ->
    ?assertError(dnd_write_failed, user_dnd_rule_ds:delete(1)),
    not_invalidated().
denylist_add_error_does_not_invalidate() ->
    ?assertError(denylist_write_failed, user_denylist_logic:add(1, 2)),
    not_invalidated().
denylist_remove_error_does_not_invalidate() ->
    ?assertError(denylist_write_failed, user_denylist_logic:remove(1, 2)),
    not_invalidated().
conversation_restore_error_does_not_invalidate() ->
    ?assertError(
        conversation_restore_failed, conversation_delete_ds:restore_conversation(1, 2, <<"c2c">>)
    ),
    not_invalidated().

successful_writes_invalidate() ->
    meck:expect(elib_pg, execute, fun(_, _) ->
        put(authority_write_done, true),
        {ok, 1}
    end),
    meck:expect(elib_pg, execute_count, fun(_, _) ->
        put(authority_write_done, true),
        {ok, 1}
    end),
    meck:expect(elib_pg, query, fun(_, _) ->
        put(authority_write_done, true),
        {ok, 1}
    end),
    meck:expect(imboy_cache, invalidate, fun(_) ->
        ?assertEqual(true, get(authority_write_done)),
        put(authority_invalidation_seen, true),
        ok
    end),
    Calls = [
        fun() -> friend_ds:confirm_friend(false, 1, 2, <<>>, #{}, <<>>, elib_dt:now()) end,
        fun() -> friend_ds:insert_pending(1, 2, #{}, elib_dt:now()) end,
        fun() -> friend_ds:delete_pending(1, 2) end,
        fun() -> user_dnd_rule_ds:save(#{<<"user_id">> => 1, <<"status">> => 1}) end,
        fun() -> user_dnd_rule_ds:delete(1) end,
        fun() -> user_denylist_logic:add(1, 2) end,
        fun() -> user_denylist_logic:remove(1, 2) end,
        fun() -> conversation_delete_ds:restore_conversation(1, 2, <<"c2c">>) end
    ],
    lists:foreach(
        fun(Call) ->
            erase(authority_write_done),
            erase(authority_invalidation_seen),
            Call(),
            ?assertEqual(true, get(authority_invalidation_seen))
        end,
        Calls
    ).

conversation_direct_caller_propagates_error() ->
    %% W09.c 后 restore/3 走 with_tx + restore_conversation_tx，错误为值语义
    %% {error, binary()}（不再 raise conversation_restore_failed）；setup 只 mock
    %% 2 元 elib_pg，事务体会 passthrough 真连库并 DELETE 0 行返回 ok。注入
    %% 事务失败：with_tx/1 返回 {error, _} 属 elib_pg 公开契约，restore 的
    %% {error, Reason} 分支归一为 {error, <<"恢复会话失败">>}，且该路径不触达
    %% imboy_cache:invalidate——"错误传播且不失效缓存"意图保持等价。
    meck:expect(elib_pg, with_tx, fun(_TxFun) -> {error, unavailable} end),
    ?assertMatch({error, _}, conversation_logic:restore(1, 2, <<"c2c">>)),
    not_invalidated().
