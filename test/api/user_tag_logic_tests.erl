-module(user_tag_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_tag_logic 模块的 EUnit 测试
%%%
%%% 目标：验证用户标签业务逻辑功能
%%% 覆盖：标签添加、删除、修改、合并
%%%===================================================================

%% ===================================================================
%% page/5 测试
%% ===================================================================

page_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Scene = <<"1">>,
        Page = 1,
        Size = 10,
        Where = <<"creator_user_id = 1">>,
        Result = user_tag_logic:page(Scene, Page, Size, Where, <<"id DESC">>),
        ?assertMatch([_|_], Result)
    end).

%% ===================================================================
%% add/3 测试
%% ===================================================================

add_creates_new_tag_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Scene = <<"1">>,
        Tag = <<"Test Tag">>,
        Result = user_tag_logic:add(Uid, Scene, Tag),
        ?assertMatch({ok, _, _}, Result)
    end).

add_duplicate_tag_fails_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Scene = <<"1">>,
        Tag = <<"Duplicate Tag">>,
        user_tag_logic:add(Uid, Scene, Tag),
        Result2 = user_tag_logic:add(Uid, Scene, Tag),
        ?assertEqual(<<"标签名已存在"/utf8>>, Result2)
    end).

%% ===================================================================
%% delete/3 测试
%% ===================================================================

delete_removes_tag_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Scene = <<"1">>,
        Tag = <<"Tag To Delete">>,
        % 先添加标签
        user_tag_logic:add(Uid, Scene, Tag),
        % 再删除
        Result = user_tag_logic:delete(Uid, Scene, Tag),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% change_name/5 测试
%% ===================================================================

change_name_updates_tag_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Scene = <<"1">>,
        TagId = 1,
        NewTagName = <<"New Tag Name">>,
        Result = user_tag_logic:change_name(0, Uid, Scene, TagId, NewTagName),
        ?assertEqual(ok, Result)
    end).

change_name_duplicate_name_fails_test_() ->
    ?TEST_WITH_APP(fun() ->
        Uid = 1,
        Scene = <<"1">>,
        TagId = 1,
        NewTagName = <<"Duplicate Name">>,
        Result = user_tag_logic:change_name(1, Uid, Scene, TagId, NewTagName),
        ?assertEqual(<<"Duplicate Name 已存在"/utf8>>, Result)
    end).

%% ===================================================================
%% merge_tag/5 测试
%% ===================================================================

merge_tag_combines_tags_test_() ->
    ?WITH_MECKS([
        {user_tag_repo, [
            {'merge_tags', 3, fun(_Uid, _SourceTagId, _TargetTagId) ->
                {ok, #{
                    merged_count => 5,
                    source_tag_id => 123,
                    target_tag_id => 456,
                    combined_tag_name => <<"Combined Tag">>,
                    merged_at => elib_dt:timestamp()
                }}
            end}
        ]}
    ], fun() ->
        Uid = 12345,
        SourceTagId = 123,
        TargetTagId = 456,
        
        % 调用 merge_tag 函数
        Result = user_tag_logic:merge_tag(Uid, SourceTagId, TargetTagId),
        
        % 验证返回结果
        ?ASSERT_MATCH({ok, #{merged_count := 5}}, Result),
        {ok, MergeResult} = Result,
        ?ASSERT_EQUAL(123, maps:get(<<"source_tag_id">>, MergeResult)),
        ?ASSERT_EQUAL(456, maps:get(<<"target_tag_id">>, MergeResult)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_tag_repo, merge_tags, 3)
    end).
