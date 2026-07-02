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
