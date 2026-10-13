-module(group_ds_fenced_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

stale_query_and_waiter_return_current_members_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912101,
            Key = {group, Gid},
            Parent = self(),
            Calls = atomics:new(1, []),
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                case atomics:add_get(Calls, 1, 1) of
                    1 ->
                        Parent ! {old_query, self()},
                        receive
                            resume -> rows([41])
                        after 3000 -> error(query_timeout)
                        end;
                    _ ->
                        rows([42])
                end
            end),
            Reader = spawn(fun() -> Parent ! {result, self(), group_ds:member_uids_strict(Gid)} end),
            receive
                {old_query, Reader} -> ok
            after 2000 -> error(reader_timeout)
            end,
            Waiter = spawn(fun() -> Parent ! {result, self(), group_ds:member_uids_strict(Gid)} end),
            try
                ok = await_cache_wait(Waiter, 100),
                ok = group_ds:leave(41, Gid),
                Reader ! resume,
                lists:foreach(
                    fun(P) ->
                        receive
                            {result, P, R} -> ?assertEqual({ok, [42]}, R)
                        after 2000 -> error(result_timeout)
                        end
                    end,
                    [Reader, Waiter]
                ),
                ?assertEqual(2, atomics:get(Calls, 1)),
                ?assertEqual({ok, [42]}, depcache:get(physical(Key), imboy_cache)),
                %% 从 legacy 遗留逻辑键中不能读取旧成员。
                ok = depcache:set(Key, [99], 60, [], imboy_cache),
                ?assertEqual({ok, [42]}, group_ds:member_uids_strict(Gid))
            after
                exit(Reader, kill),
                exit(Waiter, kill)
            end
        end)
    end).

empty_members_are_not_cached_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912102,
            Key = {group, Gid},
            Calls = atomics:new(1, []),
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                case atomics:add_get(Calls, 1, 1) of
                    1 -> rows([]);
                    _ -> rows([8])
                end
            end),
            ?assertEqual({ok, []}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(undefined, depcache:get(physical(Key), imboy_cache)),
            ?assertEqual({ok, [8]}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(2, atomics:get(Calls, 1))
        end)
    end).

query_errors_are_not_cached_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912103,
            Key = {group, Gid},
            Calls = atomics:new(1, []),
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                case atomics:add_get(Calls, 1, 1) of
                    1 -> {error, db_down};
                    _ -> rows([8])
                end
            end),
            ?assertEqual({error, db_down}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(undefined, depcache:get(physical(Key), imboy_cache)),
            ?assertEqual({ok, [8]}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(2, atomics:get(Calls, 1))
        end)
    end).

continuous_invalidation_unwraps_uncached_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912104,
            Key = {group, Gid},
            Calls = atomics:new(1, []),
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                atomics:add_get(Calls, 1, 1),
                ok = imboy_cache:invalidate(Key),
                {error, db_down}
            end),
            ?assertEqual({error, db_down}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(3, atomics:get(Calls, 1)),
            ?assertEqual(undefined, depcache:get(physical(Key), imboy_cache))
        end)
    end).

unexpected_query_shape_is_not_cached_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912105,
            Key = {group, Gid},
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                {ok, invalid_rows}
            end),
            ?assertEqual(
                {error, {unexpected_repo_result, {ok, invalid_rows}}},
                group_ds:member_uids_strict(Gid)
            ),
            ?assertEqual(undefined, depcache:get(physical(Key), imboy_cache))
        end)
    end).

with_fenced_group(Fun) ->
    Old = application:get_env(imboy, cache_fenced_namespaces),
    application:set_env(imboy, cache_fenced_namespaces, [group]),
    meck:new(group_member_repo, [unstick, passthrough, no_link]),
    meck:new(micro_community_access_repo, [unstick, passthrough, no_link]),
    %% 此套只验证缓存回填；真实 discussion ACL 的数据库验证独立执行。
    meck:expect(micro_community_access_repo, filter_uids, fun(_, Uids) -> {ok, Uids} end),
    try
        Fun()
    after
        meck:unload(micro_community_access_repo),
        meck:unload(group_member_repo),
        case Old of
            undefined -> application:unset_env(imboy, cache_fenced_namespaces);
            {ok, V} -> application:set_env(imboy, cache_fenced_namespaces, V)
        end
    end.
rows(Uids) -> {ok, [#{<<"user_id">> => U} || U <- Uids]}.
physical(Key) -> {cache_v2, Key, {imboy_cache_sync:node_boot(), imboy_cache_sync:generation(Key)}}.
await_cache_wait(_Pid, 0) ->
    error(waiter_not_blocked);
await_cache_wait(Pid, N) ->
    case process_info(Pid, current_function) of
        {current_function, {gen, do_call, 4}} ->
            ok;
        _ ->
            timer:sleep(5),
            await_cache_wait(Pid, N - 1)
    end.
