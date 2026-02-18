-module(group_tag_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_tag_ds 模块的 EUnit 测试
%%%
%%% 目标：验证群组标签数据服务功能
%%% 覆盖：添加、删除、查询、热门标签、边缘情况
%%%===================================================================

%% ===================================================================
%% add/3 测试
%% ===================================================================

%% @doc 测试添加标签成功
add_success_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_tag_repo, [
            {'exists', 2, fun(_GroupId, _TagName) -> false end},
            {'add', 2, fun(_Conn, Data) ->
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]}
    ], fun() ->
        Result = group_tag_ds:add(1, 100, <<"测试标签"/utf8>>),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 测试添加已存在的标签
add_existing_tag_returns_tag_id_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'exists', 2, fun(_GroupId, _TagName) -> true end}
    ], fun() ->
        Result = group_tag_ds:add(1, 100, <<"已存在标签"/utf8>>),
        ?assertEqual({error, <<"标签已存在"/utf8>>}, Result)
    end).

%% @doc 测试添加空标签名
add_with_empty_tag_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_ds:add(1, 100, <<>>),
        ?assertEqual({error, <<"标签名不能为空"/utf8>>}, Result)
    end).

%% @doc 测试无效的群组ID
add_with_invalid_group_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_ds:add(0, 100, <<"标签"/utf8>>),
        ?assertEqual({error, <<"无效的群组ID"/utf8>>}, Result)
    end).

%% ===================================================================
%% remove/3 测试
%% ===================================================================

%% @doc 测试删除标签成功
remove_success_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'delete', 2, fun(_GroupId, _TagName) -> {ok, 1} end}
    ], fun() ->
        Result = group_tag_ds:remove(1, 100, <<"测试标签"/utf8>>),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试删除空标签名
remove_with_empty_tag_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_ds:remove(1, 100, <<>>),
        ?assertEqual({error, <<"标签名不能为空"/utf8>>}, Result)
    end).

%% ===================================================================
%% list/2 测试
%% ===================================================================

%% @doc 测试查询群组标签列表成功
list_returns_tags_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'list_by_group', 2, fun(_GroupId, _Column) ->
            {ok, [
                #{<<"id">> => 1, <<"tag_name">> => <<"标签1"/utf8>>},
                #{<<"id">> => 2, <<"tag_name">> => <<"标签2"/utf8>>}
            ]}
        end}
    ], fun() ->
        Result = group_tag_ds:list(1),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% @doc 测试查询群组标签空结果
list_with_empty_result_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'list_by_group', 2, fun(_GroupId, _Column) -> {ok, []} end}
    ], fun() ->
        Result = group_tag_ds:list(1),
        ?assertEqual({ok, []}, Result)
    end).

%% @doc 测试查询标签错误
list_with_error_returns_empty_list_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'list_by_group', 2, fun(_GroupId, _Column) -> {error, db_error} end}
    ], fun() ->
        Result = group_tag_ds:list(1),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% search/2 测试
%% ===================================================================

%% @doc 测试按标签名搜索群组成功
search_returns_groups_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'list_by_tag_name', 2, fun(_TagName, _Column) ->
            {ok, [
                #{<<"group_id">> => 1, <<"tag_name">> => <<"技术交流"/utf8>>},
                #{<<"group_id">> => 2, <<"tag_name">> => <<"技术交流"/utf8>>}
            ]}
        end}
    ], fun() ->
        Result = group_tag_ds:search(<<"技术交流"/utf8>>),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% @doc 测试按标签名搜索空结果
search_with_empty_result_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'list_by_tag_name', 2, fun(_TagName, _Column) -> {ok, []} end}
    ], fun() ->
        Result = group_tag_ds:search(<<"不存在的标签"/utf8>>),
        ?assertEqual({ok, []}, Result)
    end).

%% @doc 测试搜索空标签名
search_with_empty_tag_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_ds:search(<<>>),
        ?assertEqual({error, <<"标签名不能为空"/utf8>>}, Result)
    end).

%% ===================================================================
%% hot_tags/1 测试
%% ===================================================================

%% @doc 测试获取热门标签成功
hot_tags_returns_list_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'hot_tags', 1, fun(_Limit) ->
            {ok, [
                #{<<"tag_name">> => <<"技术交流"/utf8>>, <<"count">> => 100},
                #{<<"tag_name">> => <<"兴趣爱好"/utf8>>, <<"count">> => 50}
            ]}
        end}
    ], fun() ->
        Result = group_tag_ds:hot_tags(10),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% @doc 测试热门标签空结果
hot_tags_with_empty_result_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'hot_tags', 1, fun(_Limit) -> {ok, []} end}
    ], fun() ->
        Result = group_tag_ds:hot_tags(10),
        ?assertEqual({ok, []}, Result)
    end).

%% @doc 测试热门标签限制数量
hot_tags_with_limit_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'hot_tags', 1, fun(Limit) ->
            {ok, lists:map(fun(I) ->
                #{<<"tag_name">> => <<"标签"/utf8, (integer_to_binary(I))/binary>>,
                  <<"count">> => 100 - I}
            end, lists:seq(1, Limit))}
        end}
    ], fun() ->
        Result = group_tag_ds:hot_tags(5),
        case Result of
            {ok, List} ->
                ?assertEqual(5, length(List));
            _ ->
                ?assert(false, "Expected {ok, List}")
        end
    end).

%% ===================================================================
%% count/1 测试
%% ===================================================================

%% @doc 测试统计群组标签数量
count_returns_count_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'count_by_group', 1, fun(_GroupId) -> {ok, 5} end}
    ], fun() ->
        Result = group_tag_ds:count(1),
        ?assertEqual({ok, 5}, Result)
    end).

%% @doc 测试统计不存在的群组
count_for_nonexistent_group_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'count_by_group', 1, fun(_GroupId) -> {ok, 0} end}
    ], fun() ->
        Result = group_tag_ds:count(999999),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试超长标签名
add_with_long_tag_name_test_() ->
    ?WITH_MECKS([
        {group_tag_repo, [
            {'exists', 2, fun(_GroupId, _TagName) -> false end},
            {'add', 2, fun(_Conn, Data) ->
                TagName = maps:get(<<"tag_name">>, Data),
                ?assert(byte_size(TagName) =< 50),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]}
    ], fun() ->
        LongTag = list_to_binary(lists:duplicate(50, $x)),
        Result = group_tag_ds:add(1, 100, LongTag),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 测试 UTF-8 标签名
add_with_utf8_tag_name_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_tag_repo, [
            {'exists', 2, fun(_GroupId, _TagName) -> false end},
            {'add', 2, fun(_Conn, Data) ->
                ?assertEqual(<<"技术交流群"/utf8>>, maps:get(<<"tag_name">>, Data)),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]}
    ], fun() ->
        Result = group_tag_ds:add(1, 100, <<"技术交流群"/utf8>>),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 测试特殊字符标签名
add_with_special_chars_tag_name_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_tag_repo, [
            {'exists', 2, fun(_GroupId, _TagName) -> false end},
            {'add', 2, fun(_Conn, Data) ->
                ?assertEqual(<<"技术-交流_(2024)">>, maps:get(<<"tag_name">>, Data)),
                {ok, 1, #{<<"id">> => 1}}
            end}
        ]}
    ], fun() ->
        Result = group_tag_ds:add(1, 100, <<"技术-交流_(2024)">>),
        ?assertMatch({ok, _}, Result)
    end).

%% @doc 测试负数群组ID
add_with_negative_group_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_tag_ds:add(-1, 100, <<"标签"/utf8>>),
        ?assertEqual({error, <<"无效的群组ID"/utf8>>}, Result)
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的标签生命周期
complete_tag_lifecycle_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_tag_repo, [
            {'exists', 2, fun(_GroupId, _TagName) -> false end},
            {'add', 2, fun(_Conn, _Data) -> {ok, 1, #{<<"id">> => 1}} end},
            {'list_by_group', 2, fun(_GroupId, _Column) ->
                {ok, [#{<<"id">> => 1, <<"tag_name">> => <<"测试标签"/utf8>>}]}
            end},
            {'delete', 2, fun(_GroupId, _TagName) -> {ok, 1} end}
        ]}
    ], fun() ->
        GroupId = 1,
        Uid = 100,
        TagName = <<"测试标签"/utf8>>,

        % 1. 添加标签
        AddResult = group_tag_ds:add(GroupId, Uid, TagName),
        ?assertMatch({ok, _}, AddResult),

        % 2. 查询标签列表
        ListResult = group_tag_ds:list(GroupId),
        ?assertMatch({ok, [_]}, ListResult),

        % 3. 统计标签数量
        CountResult = group_tag_ds:count(GroupId),
        ?assertEqual({ok, 1}, CountResult),

        % 4. 删除标签
        RemoveResult = group_tag_ds:remove(GroupId, Uid, TagName),
        ?assertEqual(ok, RemoveResult)
    end).

%% @doc 测试同一群组添加多个不同标签
multiple_tags_for_same_group_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_tag_repo, [
            {'exists', 2, fun(_GroupId, TagName) -> TagName =:= <<"已存在"/utf8>> end},
            {'add', 2, fun(_Conn, Data) ->
                case maps:get(<<"tag_name">>, Data) of
                    <<"已存在"/utf8>> -> {error, duplicate};
                    _ -> {ok, 1, #{<<"id">> => 1}}
                end
            end}
        ]}
    ], fun() ->
        GroupId = 1,
        Uid = 100,
        Tags = [<<"标签1"/utf8>>, <<"标签2"/utf8>>, <<"已存在"/utf8>>],

        Results = [group_tag_ds:add(GroupId, Uid, Tag) || Tag <- Tags],
        ?assertMatch({ok, _}, lists:nth(1, Results)),
        ?assertMatch({ok, _}, lists:nth(2, Results)),
        ?assertEqual({error, <<"标签已存在"/utf8>>}, lists:nth(3, Results))
    end).

%% @doc 测试按标签搜索群组的完整流程
search_groups_by_tag_flow_test_() ->
    ?WITH_MECKS([
        {group_tag_repo, [
            {'list_by_tag_name', 2, fun(_TagName, _Column) ->
                {ok, [
                    #{<<"group_id">> => 1, <<"tag_name">> => <<"技术"/utf8>>},
                    #{<<"group_id">> => 2, <<"tag_name">> => <<"技术"/utf8>>}
                ]}
            end}
        ]}
    ], fun() ->
        TagName = <<"技术"/utf8>>,

        % 搜索使用该标签的群组
        Result = group_tag_ds:search(TagName),
        ?assertMatch({ok, [_, _]}, Result),

        % 验证返回的群组ID
        {ok, Groups} = Result,
        GroupIds = [maps:get(<<"group_id">>, G) || G <- Groups],
        ?assert(lists:member(1, GroupIds)),
        ?assert(lists:member(2, GroupIds))
    end).
