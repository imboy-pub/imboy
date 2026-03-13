-module(e2ee_social_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc e2ee_social_repo 基础测试
%%%===================================================================

generate_shard_id_is_uuid_v4_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        ShardId = e2ee_social_repo:generate_shard_id(),
        ?assert(is_binary(ShardId)),
        ?assertEqual(36, byte_size(ShardId)),
        ?assert(is_uuid_v4(ShardId))
    end).

generate_shard_id_is_unique_across_batch_test_() ->
    ?TEST_SIMPLE(fun() ->
        Ids = [e2ee_social_repo:generate_shard_id() || _ <- lists:seq(1, 100)],
        UniqueCount = length(lists:usort(Ids)),
        ?assertEqual(length(Ids), UniqueCount)
    end).

is_uuid_v4(Value) ->
    case re:run(Value, <<"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>) of
        {match, _} -> true;
        _ -> false
    end.

