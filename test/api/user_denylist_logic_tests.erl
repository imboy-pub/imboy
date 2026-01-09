-module(user_denylist_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_denylist_logic 模块的 EUnit 测试
%%%
%%% 目标：验证黑名单业务逻辑功能
%%% 覆盖：添加黑名单、移除黑名单、检查是否在黑名单、分页查询
%%%===================================================================

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_user_to_denylist_success_test_() ->
    ?WITH_MECK(user_denylist_repo, [
        {'add', 3, fun(_Uid, _DeniedUserId, _Now) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ], fun() ->
            ?WITH_MECK(imboy_dt, [
                {'now', 0, fun() -> 1640995200 end}
            ], fun() ->
                Uid = 12345,
                BlockedUid = 67890,
                
                Result = user_denylist_logic:add(Uid, BlockedUid),
                ?assertEqual(1640995200, Result)
            end)
        end)
    end).

add_user_to_denylist_with_same_user_test_() ->
    ?WITH_MECK(user_denylist_repo, [
        {'add', 3, fun(_Uid, _DeniedUserId, _Now) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ], fun() ->
            ?WITH_MECK(imboy_dt, [
                {'now', 0, fun() -> 1640995200 end}
            ], fun() ->
                Uid = 12345,
                BlockedUid = 12345,  % 同一个用户
                
                Result = user_denylist_logic:add(Uid, BlockedUid),
                ?assertEqual(1640995200, Result)
            end)
        end)
    end).

%% ===================================================================
%% remove/2 测试
%% ===================================================================

remove_user_from_denylist_success_test_() ->
    ?WITH_MECK(user_denylist_repo, [
        {'remove', 2, fun(_Uid, _DeniedUserId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ], fun() ->
            Uid = 12345,
            BlockedUid = 67890,
            
            Result = user_denylist_logic:remove(Uid, BlockedUid),
            ?assertEqual(ok, Result)
        end)
    end).

remove_nonexistent_user_test_() ->
    ?WITH_MECK(user_denylist_repo, [
        {'remove', 2, fun(_Uid, _DeniedUserId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ], fun() ->
            Uid = 12345,
            BlockedUid = 99999,  % 不存在的用户
            
            Result = user_denylist_logic:remove(Uid, BlockedUid),
            ?assertEqual(ok, Result)
        end)
    end).

%% ===================================================================
%% in_denylist/2 测试
%% ===================================================================

check_user_in_denylist_true_test_() ->
    ?WITH_MECK(user_denylist_repo, [
        {'in_denylist', 2, fun(_Uid, _DeniedUserId) -> 1 end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'memo', 3, fun(Function, _Key, _MaxAge) -> Function() end}
        ], fun() ->
            Uid = 12345,
            TargetUid = 67890,
            
            Result = user_denylist_logic:in_denylist(Uid, TargetUid),
            ?assertEqual(1, Result)
        end)
    end).

check_user_not_in_denylist_false_test_() ->
    ?WITH_MECK(user_denylist_repo, [
        {'in_denylist', 2, fun(_Uid, _DeniedUserId) -> 0 end}
    ], fun() ->
        ?WITH_MECK(imboy_cache, [
            {'memo', 3, fun(Function, _Key, _MaxAge) -> Function() end}
        ], fun() ->
            Uid = 12345,
            TargetUid = 67890,
            
            Result = user_denylist_logic:in_denylist(Uid, TargetUid),
            ?assertEqual(0, Result)
        end)
    end).

%% ===================================================================
%% page/3 测试
%% ===================================================================

page_denylist_with_results_test_() ->
    ?WITH_MECK(user_denylist_repo, [
        {'count_for_uid', 1, fun(_Uid) -> 5 end}
    ], fun() ->
        ?WITH_MECK(user_denylist_repo, [
            {'page_for_uid', 3, fun(_Uid, _Size, _Offset) ->
                {ok, [<<"id">>, <<"uid">>, <<"denied_uid">>, <<"created_at">>], [
                    {1, 12345, 67890, 1640995200},
                    {2, 12345, 67891, 1640995201}
                ]}
            end}
        ], fun() ->
            ?WITH_MECK(imboy_hashids, [
                {'encode', 1, fun(Id) -> <<"encoded_", (integer_to_binary(Id))/binary>> end}
            ], fun() ->
                ?WITH_MECK(imboy_response, [
                    {'page_payload', 4, fun(_Total, _Page, _Size, Items) -> Items end}
                ], fun() ->
                    Uid = 12345,
                    Page = 1,
                    Size = 10,
                    
                    Result = user_denylist_logic:page(Uid, Page, Size),
                    ?assertMatch([_|_], Result),
                    ?assert(length(Result) > 0)
                end)
            end)
        end)
    end).

page_denylist_empty_results_test_() ->
    ?WITH_MECK(user_denylist_repo, [
        {'count_for_uid', 1, fun(_Uid) -> 0 end}
    ], fun() ->
        ?WITH_MECK(user_denylist_repo, [
            {'page_for_uid', 3, fun(_Uid, _Size, _Offset) ->
                {ok, [<<"id">>, <<"uid">>, <<"denied_uid">>, <<"created_at">>], []}
            end}
        ], fun() ->
            ?WITH_MECK(imboy_response, [
                {'page_payload', 4, fun(_Total, _Page, _Size, Items) -> Items end}
            ], fun() ->
                Uid = 12345,
                Page = 1,
                Size = 10,
                
                Result = user_denylist_logic:page(Uid, Page, Size),
                ?assertEqual([], Result)
            end)
        end)
    end).

page_denylist_invalid_page_test_() ->
    Uid = 12345,
    Page = 0,  % 无效页码
    Size = 10,
    
    % 应该返回空列表，因为页码必须大于0
    Result = user_denylist_logic:page(Uid, Page, Size),
    ?assertEqual([], Result).
