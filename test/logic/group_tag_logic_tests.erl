-module(group_tag_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_tag_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群组标签业务逻辑功能
%%% 覆盖：添加、删除、查询、权限验证、热门标签
%%%===================================================================

%% ===================================================================
%% add/3 测试
%% ===================================================================

%% @doc 测试添加标签成功
add_success_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100, 101, 102] end}
        ]},
        {group_tag_ds, [
            {'add', 3, fun(_GroupId, _Uid, _TagName) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = group_tag_logic:add(1, 100, <<"技术交流"/utf8>>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试非群成员添加标签失败
add_by_non_member_fails_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [101, 102] end}
        ]}
    ], fun() ->
        Result = group_tag_logic:add(1, 100, <<"技术交流"/utf8>>),
        ?assertEqual({error, <<"只有群成员可以添加标签"/utf8>>}, Result)
    end).

%% @doc 测试添加标签名过长
add_with_too_long_name_fails_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100] end}
        ]}
    ], fun() ->
        LongTag = list_to_binary(lists:duplicate(100, $x)),
        Result = group_tag_logic:add(1, 100, LongTag),
        ?assertEqual({error, <<"标签名过长"/utf8>>}, Result)
    end).

%% @doc 测试添加空标签名
add_with_empty_name_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_logic:add(1, 100, <<>>),
        ?assertEqual({error, <<"标签名不能为空"/utf8>>}, Result)
    end).

%% ===================================================================
%% remove/3 测试
%% ===================================================================

%% @doc 测试删除标签成功
remove_success_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100, 101, 102] end}
        ]},
        {group_tag_ds, [
            {'remove', 3, fun(_GroupId, _Uid, _TagName) -> ok end}
        ]}
    ], fun() ->
        Result = group_tag_logic:remove(1, 100, <<"技术交流"/utf8>>),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试非群成员删除标签失败
remove_by_non_member_fails_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [101, 102] end}
        ]}
    ], fun() ->
        Result = group_tag_logic:remove(1, 100, <<"技术交流"/utf8>>),
        ?assertEqual({error, <<"只有群成员可以删除标签"/utf8>>}, Result)
    end).

%% @doc 测试删除空标签名
remove_with_empty_name_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_logic:remove(1, 100, <<>>),
        ?assertEqual({error, <<"标签名不能为空"/utf8>>}, Result)
    end).

%% ===================================================================
%% list/2 测试
%% ===================================================================

%% @doc 测试查询群组标签列表成功
list_returns_tags_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100, 101, 102] end}
        ]},
        {group_tag_ds, [
            {'list', 1, fun(_GroupId) ->
                {ok, [
                    #{<<"id">> => 1, <<"tag_name">> => <<"技术交流"/utf8>>},
                    #{<<"id">> => 2, <<"tag_name">> => <<"兴趣小组"/utf8>>}
                ]}
            end}
        ]}
    ], fun() ->
        Result = group_tag_logic:list(1, 100),
        ?assertMatch({ok, [_, _]}, Result)
    end).

%% @doc 测试非群成员查询标签失败
list_by_non_member_fails_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [101, 102] end}
        ]}
    ], fun() ->
        Result = group_tag_logic:list(1, 100),
        ?assertEqual({error, <<"只有群成员可以查看标签"/utf8>>}, Result)
    end).

%% @doc 测试查询空标签列表
list_with_empty_result_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100] end}
        ]},
        {group_tag_ds, [
            {'list', 1, fun(_GroupId) -> {ok, []} end}
        ]}
    ], fun() ->
        Result = group_tag_logic:list(1, 100),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% search/1 测试
%% ===================================================================

%% @doc 测试按标签搜索群组成功
search_returns_groups_test_() ->
    ?WITH_MECK(group_tag_ds, [
        {'search', 1, fun(_TagName) ->
            {ok, [
                #{<<"group_id">> => 1, <<"tag_name">> => <<"技术交流"/utf8>>},
                #{<<"group_id">> => 2, <<"tag_name">> => <<"技术交流"/utf8>>}
            ]}
        end}
    ], fun() ->
        Result = group_tag_logic:search(<<"技术交流"/utf8>>),
        ?assertMatch({ok, [_, _]}, Result)
    end).

%% @doc 测试搜索空标签名
search_with_empty_name_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_logic:search(<<>>),
        ?assertEqual({error, <<"标签名不能为空"/utf8>>}, Result)
    end).

%% @doc 测试搜索无结果
search_with_no_results_test_() ->
    ?WITH_MECK(group_tag_ds, [
        {'search', 1, fun(_TagName) -> {ok, []} end}
    ], fun() ->
        Result = group_tag_logic:search(<<"不存在的标签"/utf8>>),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% hot_tags/1 测试
%% ===================================================================

%% @doc 测试获取热门标签成功
hot_tags_returns_list_test_() ->
    ?WITH_MECK(group_tag_ds, [
        {'hot_tags', 1, fun(_Limit) ->
            {ok, [
                #{<<"tag_name">> => <<"技术交流"/utf8>>, <<"count">> => 100},
                #{<<"tag_name">> => <<"兴趣小组"/utf8>>, <<"count">> => 50}
            ]}
        end}
    ], fun() ->
        Result = group_tag_logic:hot_tags(10),
        ?assertMatch({ok, [_, _]}, Result)
    end).

%% @doc 测试热门标签空结果
hot_tags_with_empty_result_test_() ->
    ?WITH_MECK(group_tag_ds, [
        {'hot_tags', 1, fun(_Limit) -> {ok, []} end}
    ], fun() ->
        Result = group_tag_logic:hot_tags(10),
        ?assertEqual({ok, []}, Result)
    end).

%% @doc 测试热门标签限制
hot_tags_with_limit_test_() ->
    ?WITH_MECK(group_tag_ds, [
        {'hot_tags', 1, fun(Limit) ->
            {ok, lists:map(fun(I) ->
                #{<<"tag_name">> => <<"标签"/utf8, (integer_to_binary(I))/binary>>,
                  <<"count">> => 100 - I}
            end, lists:seq(1, Limit))}
        end}
    ], fun() ->
        Result = group_tag_logic:hot_tags(5),
        case Result of
            {ok, List} ->
                ?assertEqual(5, length(List));
            _ ->
                ?assert(false, "Expected {ok, List}")
        end
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试无效群组ID
add_with_invalid_group_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_logic:add(0, 100, <<"标签"/utf8>>),
        ?assertEqual({error, <<"无效的群组ID"/utf8>>}, Result)
    end).

%% @doc 测试负数群组ID
remove_with_negative_group_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_logic:remove(-1, 100, <<"标签"/utf8>>),
        ?assertEqual({error, <<"无效的群组ID"/utf8>>}, Result)
    end).

%% @doc 测试无效用户ID
list_with_invalid_uid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_logic:list(1, 0),
        ?assertEqual({error, <<"无效的用户ID"/utf8>>}, Result)
    end).

%% @doc 测试 UTF-8 标签名
add_with_utf8_tag_name_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100] end}
        ]},
        {group_tag_ds, [
            {'add', 3, fun(_GroupId, _Uid, TagName) ->
                ?assertEqual(<<"技术交流群"/utf8>>, TagName),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = group_tag_logic:add(1, 100, <<"技术交流群"/utf8>>),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 测试特殊字符标签名
add_with_special_chars_tag_name_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100] end}
        ]},
        {group_tag_ds, [
            {'add', 3, fun(_GroupId, _Uid, TagName) ->
                ?assertEqual(<<"技术-交流_(2024)">>, TagName),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = group_tag_logic:add(1, 100, <<"技术-交流_(2024)">>),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的标签生命周期
complete_tag_lifecycle_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100] end}
        ]},
        {group_tag_ds, [
            {'add', 3, fun(_GroupId, _Uid, _TagName) -> {ok, 1} end},
            {'list', 1, fun(_GroupId) ->
                {ok, [#{<<"id">> => 1, <<"tag_name">> => <<"测试标签"/utf8>>}]}
            end},
            {'remove', 3, fun(_GroupId, _Uid, _TagName) -> ok end}
        ]}
    ], fun() ->
        GroupId = 1,
        Uid = 100,
        TagName = <<"测试标签"/utf8>>,

        % 1. 添加标签
        AddResult = group_tag_logic:add(GroupId, Uid, TagName),
        ?assertMatch({ok, _}, AddResult),

        % 2. 查询标签列表
        ListResult = group_tag_logic:list(GroupId, Uid),
        ?assertMatch({ok, [_]}, ListResult),

        % 3. 删除标签
        RemoveResult = group_tag_logic:remove(GroupId, Uid, TagName),
        ?assertEqual(ok, RemoveResult)
    end).

%% @doc 测试同一群组多个标签管理
multiple_tags_management_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_GroupId) -> [100] end}
        ]},
        {group_tag_ds, [
            {'add', 3, fun(_GroupId, _Uid, TagName) ->
                case TagName of
                    <<"已存在"/utf8>> -> {error, <<"标签已存在"/utf8>>};
                    _ -> {ok, 1}
                end
            end},
            {'list', 1, fun(_GroupId) ->
                {ok, [
                    #{<<"id">> => 1, <<"tag_name">> => <<"标签1"/utf8>>},
                    #{<<"id">> => 2, <<"tag_name">> => <<"标签2"/utf8>>}
                ]}
            end}
        ]}
    ], fun() ->
        GroupId = 1,
        Uid = 100,
        Tags = [<<"标签1"/utf8>>, <<"标签2"/utf8>>, <<"已存在"/utf8>>],

        % 添加多个标签
        AddResults = [group_tag_logic:add(GroupId, Uid, Tag) || Tag <- Tags],
        ?assertMatch({ok, _}, lists:nth(1, AddResults)),
        ?assertMatch({ok, _}, lists:nth(2, AddResults)),
        ?assertEqual({error, <<"标签已存在"/utf8>>}, lists:nth(3, AddResults)),

        % 查询标签列表
        ListResult = group_tag_logic:list(GroupId, Uid),
        ?assertMatch({ok, [_, _]}, ListResult)
    end).

%% @doc 测试按标签搜索群组的完整流程
search_groups_flow_test_() ->
    ?WITH_MECKS([
        {group_tag_ds, [
            {'search', 1, fun(_TagName) ->
                {ok, [
                    #{<<"group_id">> => 1, <<"tag_name">> => <<"技术"/utf8>>},
                    #{<<"group_id">> => 2, <<"tag_name">> => <<"技术"/utf8>>}
                ]}
            end}
        ]},
        {group_repo, [
            {'find_by_id', 2, fun(_Id, _Column) ->
                #{<<"id">> => 1, <<"title">> => <<"技术群"/utf8>>}
            end}
        ]}
    ], fun() ->
        TagName = <<"技术"/utf8>>,

        % 搜索使用该标签的群组
        Result = group_tag_logic:search(TagName),
        ?assertMatch({ok, [_, _]}, Result)
    end).
