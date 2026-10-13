-module(user_tag_commit_tests).
-include_lib("eunit/include/eunit.hrl").

%% 真实 elib_pg 事务队列 + mocked PG driver：只证明业务钩子接线，不充当真 PG 验收。
commit_hooks_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(commit_then_invalidate(delete)) end,
        fun(_) -> ?_test(commit_then_invalidate(rename)) end,
        fun(_) -> ?_test(rollback_preserves_cache(delete)) end,
        fun(_) -> ?_test(rollback_preserves_cache(rename)) end,
        fun(_) -> ?_test(rename_error_aborts()) end,
        fun(_) -> ?_test(relation_read_error_prevents_transaction()) end,
        fun(_) -> ?_test(tag_lookup_error_prevents_delete()) end,
        fun(_) -> ?_test(delete_object_updates_are_owner_scoped(1)) end,
        fun(_) -> ?_test(delete_object_updates_are_owner_scoped(2)) end
    ]}.

setup() ->
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    meck:new(pooler, [no_link, passthrough]),
    meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
    meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
    meck:new(config_ds, [no_link, passthrough]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:new(epgsql, [no_link, passthrough]),
    meck:expect(epgsql, with_transaction, fun(C, F, _) -> F(C) end),
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, commit} end),
    meck:expect(epgsql, squery, fun(_, <<"ROLLBACK">>) -> {ok, [], []} end),
    meck:new(elib_pg, [no_link, passthrough]),
    meck:expect(elib_pg, pluck_value, fun(_, _, _, _, _) -> 5 end),
    meck:expect(elib_pg, pluck, fun(_, _, _, _, _) -> {ok, 5} end),
    meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
    meck:expect(elib_pg, execute, fun(_, _, _) -> {ok, 1} end),
    meck:new(user_tag_relation_repo, [no_link, passthrough]),
    meck:expect(user_tag_relation_repo, flush_subtitle, fun(5) ->
        ?assertEqual(1, meck:num_calls(pooler, return_member, 2)),
        ?assertEqual(1, meck:num_calls(epgsql, get_cmd_status, 1)),
        put(tag_commit_flushed, true),
        ok
    end),
    meck:expect(user_tag_relation_repo, update_tag, fun(_, 5, Name, _, _) -> {5, Name} end),
    Conn.

cleanup(Conn) ->
    [meck:unload(M) || M <- [user_tag_relation_repo, elib_pg, epgsql, config_ds, pooler]],
    Conn ! stop.

call(delete) -> user_tag_ds:delete(100, 1, <<"tag">>);
call(rename) -> user_tag_ds:change_name(0, 100, 1, 5, <<"new">>).

commit_then_invalidate(Op) ->
    erase(tag_commit_flushed),
    meck:expect(epgsql, with_transaction, fun(C, F, _) ->
        Result = F(C),
        ?assertEqual(undefined, get(tag_commit_flushed)),
        Result
    end),
    ?assertEqual(ok, call(Op)),
    ?assertEqual(true, erase(tag_commit_flushed)).

rollback_preserves_cache(Op) ->
    erase(tag_commit_flushed),
    %% PG 可把 COMMIT 实际返回为 ROLLBACK；业务体的 ok 不证明提交。
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, rollback} end),
    ?assertMatch({error, {commit_not_confirmed, ok, {ok, rollback}}}, call(Op)),
    ?assertEqual(undefined, get(tag_commit_flushed)),
    ?assertEqual(0, meck:num_calls(user_tag_relation_repo, flush_subtitle, 1)).

rename_error_aborts() ->
    erase(tag_commit_flushed),
    meck:expect(user_tag_relation_repo, update_tag, fun(_, _, _, _, _) -> {error, failed} end),
    ?assertEqual({error, failed}, call(rename)),
    ?assertEqual(undefined, get(tag_commit_flushed)),
    ?assertEqual(0, meck:num_calls(epgsql, get_cmd_status, 1)),
    ?assertEqual(0, meck:num_calls(user_tag_relation_repo, flush_subtitle, 1)).

relation_read_error_prevents_transaction() ->
    meck:expect(elib_pg, query, fun(_, _) -> {error, unavailable} end),
    ?assertEqual({error, unavailable}, call(rename)),
    ?assertEqual(0, meck:num_calls(epgsql, with_transaction, 3)),
    ?assertEqual(0, meck:num_calls(user_tag_relation_repo, update_tag, 5)),
    ?assertEqual(0, meck:num_calls(user_tag_relation_repo, flush_subtitle, 1)).

tag_lookup_error_prevents_delete() ->
    meck:expect(elib_pg, pluck_value, fun(_, _, _, _, _) -> 0 end),
    meck:expect(elib_pg, pluck, fun(_, _, _, _, _) -> {error, unavailable} end),
    ?assertEqual({error, unavailable}, call(delete)),
    ?assertEqual(0, meck:num_calls(epgsql, with_transaction, 3)),
    ?assertEqual(0, meck:num_calls(user_tag_relation_repo, flush_subtitle, 1)).
delete_object_updates_are_owner_scoped(Scene) ->
    meck:expect(elib_pg, execute, fun(_, Sql, Params) ->
        case binary:match(Sql, <<"UPDATE">>) of
            nomatch ->
                ok;
            _ ->
                Owner =
                    case Scene of
                        1 -> <<"user_id = $4">>;
                        2 -> <<"from_user_id = $4">>
                    end,
                ?assertMatch({_, _}, binary:match(Sql, Owner)),
                ?assertEqual(100, lists:nth(4, Params))
        end,
        {ok, 1}
    end),
    ?assertEqual(ok, user_tag_ds:delete(100, Scene, <<"tag">>)).
