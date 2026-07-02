-module(e2ee_transfer_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc e2ee_transfer_repo 基础测试
%%% 会话 ID 生成已迁移至 elib_uuid:gen_v7（repo 不再提供 generate_session_id），
%%% 保留 UUID v7 格式/唯一性/时序性质断言
%%%===================================================================

generate_session_id_is_uuid_v7_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        SessionId = elib_uuid:gen_v7(),
        ?assert(is_binary(SessionId)),
        ?assertEqual(36, byte_size(SessionId)),
        ?assert(is_uuid_v7(SessionId))
    end).

generate_session_id_is_unique_across_batch_test_() ->
    ?TEST_SIMPLE(fun() ->
        Ids = [elib_uuid:gen_v7() || _ <- lists:seq(1, 100)],
        UniqueCount = length(lists:usort(Ids)),
        ?assertEqual(length(Ids), UniqueCount)
    end).

generate_session_id_cross_ms_ordered_test_() ->
    ?TEST_SIMPLE(fun() ->
        Id1 = elib_uuid:gen_v7(),
        timer:sleep(10),
        Id2 = elib_uuid:gen_v7(),
        ?assert(Id1 < Id2)
    end).

is_uuid_v7(Value) ->
    case
        re:run(Value, <<"^[0-9a-f]{8}-[0-9a-f]{4}-7[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>)
    of
        {match, _} -> true;
        _ -> false
    end.
