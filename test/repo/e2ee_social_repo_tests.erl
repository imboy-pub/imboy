-module(e2ee_social_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc e2ee_social_repo 基础测试
%%% 旧的 generate_shard_id UUID v4 测试已删除：分片 ID 生成已迁移至
%%% logic 层 elib_tsid:generate()，repo 不再提供该函数。
%%%===================================================================

%% 一次性语义：mark_shard_used 必须带 status='active' CAS 条件
mark_shard_used_cas_on_active_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(Sql, [ShardId, ProxyUid]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"SET status = 'used'">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"used_at = NOW()">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"AND status = 'active'">>)),
                    ?assertEqual(<<"shard-1">>, ShardId),
                    ?assertEqual(1001, ProxyUid),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 1}, e2ee_social_repo:mark_shard_used(<<"shard-1">>, 1001))
        end
    ).

%% 撤销级联：revoke_shards_by_proxy 只失效 active 分片
revoke_shards_by_proxy_only_active_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(Sql, [Uid, ProxyUid]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"SET status = 'revoked'">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"AND status = 'active'">>)),
                    ?assertEqual(9999, Uid),
                    ?assertEqual(1001, ProxyUid),
                    {ok, 2}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 2}, e2ee_social_repo:revoke_shards_by_proxy(9999, 1001))
        end
    ).

%% 回归：can_recover 遍历所有恢复组，任一组满足门限即可恢复
%% （旧实现只取首行：首组不足、次组充足时会误判为不可恢复）。
can_recover_any_group_satisfies_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [Uid, KeyVersion]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"status = 'active'">>)),
                    ?assertEqual(9999, Uid),
                    ?assertEqual(<<"v1">>, KeyVersion),
                    %% 首组分片不足(1<2)，次组充足(3>=2)
                    {ok, [
                        #{<<"count">> => 1, <<"threshold">> => 2, <<"total_shards">> => 3},
                        #{<<"count">> => 3, <<"threshold">> => 2, <<"total_shards">> => 3}
                    ]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, true}, e2ee_social_repo:can_recover(9999, <<"v1">>))
        end
    ).

%% 回归：所有组均不足 → 不可恢复；空结果 → 不可恢复（不再依赖 GROUP BY 空行分支）
can_recover_all_groups_insufficient_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [#{<<"count">> => 1, <<"threshold">> => 2, <<"total_shards">> => 3}]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, false}, e2ee_social_repo:can_recover(9999, <<"v1">>))
        end
    ).
