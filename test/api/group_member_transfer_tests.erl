-module(group_member_transfer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_member_transfer 模块的 EUnit 测试
%%%
%%% 目标：验证群成员列表处理功能
%%% 覆盖：成员ID替换、数据格式化
%%%===================================================================

%% ===================================================================
%% member_list/1 测试
%% ===================================================================

member_list_replaces_ids_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'replace_id', 2, fun(Map, _Field) ->
            Map2 = maps:put(<<"group_id">>, <<"encoded_group_123">>, Map),
            maps:put(<<"user_id">>, <<"encoded_user_456">>, Map2)
        end}
    ], fun() ->
        % 测试数据：包含群组和用户ID的成员列表
        MemberList = [
            #{<<"id">> => 1, <<"group_id">> => 123, <<"user_id">> => 456, <<"role">> => 1},
            #{<<"id">> => 2, <<"group_id">> => 123, <<"user_id">> => 789, <<"role">> => 2},
            #{<<"id">> => 3, <<"group_id">> => 123, <<"user_id">> => 101, <<"role">> => 1}
        ],
        
        Result = group_member_transfer:member_list(MemberList),
        
        % 验证结果是一个列表
        ?assertMatch([_|_], Result),
        ?assertEqual(length(MemberList), length(Result)),
        
        % 验证每个成员的ID都被替换了
        lists:foreach(fun(Member) ->
            ?assert(maps:is_key(<<"group_id">>, Member)),
            ?assert(maps:is_key(<<"user_id">>, Member)),
            ?assertEqual(<<"encoded_group_123">>, maps:get(<<"group_id">>, Member)),
            ?assertMatch(<<_/binary>>, maps:get(<<"user_id">>, Member))
        end, Result)
    end).

member_list_empty_list_test() ->
    EmptyList = [],
    Result = group_member_transfer:member_list(EmptyList),
    ?assertEqual([], Result).

member_list_single_member_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'replace_id', 2, fun(Map, Field) ->
            case Field of
                <<"group_id">> ->
                    maps:put(<<"group_id">>, <<"encoded_group_999">>, Map);
                <<"user_id">> ->
                    maps:put(<<"user_id">>, <<"encoded_user_888">>, Map);
                _ ->
                    Map
            end
        end}
    ], fun() ->
        % 测试单个成员
        SingleMember = #{<<"id">> => 42, <<"group_id">> => 999, <<"user_id">> => 888, <<"role">> => 4},
        
        Result = group_member_transfer:member_list([SingleMember]),
        
        ?assertEqual(1, length(Result)),
        [ProcessedMember] = Result,
        ?assertEqual(<<"encoded_group_999">>, maps:get(<<"group_id">>, ProcessedMember)),
        ?assertEqual(<<"encoded_user_888">>, maps:get(<<"user_id">>, ProcessedMember)),
        ?assertEqual(4, maps:get(<<"role">>, ProcessedMember))
    end).

member_list_preserves_other_fields_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'replace_id', 2, fun(Map, Field) ->
            case Field of
                <<"group_id">> ->
                    maps:put(<<"group_id">>, <<"encoded_group_777">>, Map);
                <<"user_id">> ->
                    maps:put(<<"user_id">>, <<"encoded_user_666">>, Map);
                _ ->
                    Map
            end
        end}
    ], fun() ->
        % 测试保留其他字段
        MemberWithExtraFields = [
            #{
                <<"id">> => 1,
                <<"group_id">> => 777,
                <<"user_id">> => 666,
                <<"role">> => 3,
                <<"nickname">> => <<"Test User">>,
                <<"avatar">> => <<"http://example.com/avatar.jpg">>,
                <<"join_time">> => 1640995200
            }
        ],
        
        Result = group_member_transfer:member_list(MemberWithExtraFields),
        
        [ProcessedMember] = Result,
        % 验证ID字段被替换
        ?assertEqual(<<"encoded_group_777">>, maps:get(<<"group_id">>, ProcessedMember)),
        ?assertEqual(<<"encoded_user_666">>, maps:get(<<"user_id">>, ProcessedMember)),
        
        % 验证其他字段保持不变
        ?assertEqual(3, maps:get(<<"role">>, ProcessedMember)),
        ?assertEqual(<<"Test User">>, maps:get(<<"nickname">>, ProcessedMember)),
        ?assertEqual(<<"http://example.com/avatar.jpg">>, maps:get(<<"avatar">>, ProcessedMember)),
        ?assertEqual(1640995200, maps:get(<<"join_time">>, ProcessedMember))
    end).
