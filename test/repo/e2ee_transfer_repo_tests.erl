-module(e2ee_transfer_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc e2ee_transfer_repo 基础测试
%%%===================================================================

generate_session_id_is_uuid_v7_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        SessionId = e2ee_transfer_repo:generate_session_id(),
        ?assert(is_binary(SessionId)),
        ?assertEqual(36, byte_size(SessionId)),
        ?assert(is_uuid_v7(SessionId))
    end).

generate_session_id_is_unique_across_batch_test_() ->
    ?TEST_SIMPLE(fun() ->
        Ids = [e2ee_transfer_repo:generate_session_id() || _ <- lists:seq(1, 100)],
        UniqueCount = length(lists:usort(Ids)),
        ?assertEqual(length(Ids), UniqueCount)
    end).

generate_session_id_cross_ms_ordered_test_() ->
    ?TEST_SIMPLE(fun() ->
        Id1 = e2ee_transfer_repo:generate_session_id(),
        timer:sleep(10),
        Id2 = e2ee_transfer_repo:generate_session_id(),
        ?assert(Id1 < Id2)
    end).

is_uuid_v7(Value) ->
    case
        re:run(Value, <<"^[0-9a-f]{8}-[0-9a-f]{4}-7[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>)
    of
        {match, _} -> true;
        _ -> false
    end.
