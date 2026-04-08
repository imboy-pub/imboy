%% @doc 群分组和群标签功能集成测试
%% 测试范围：
%% - 创建群分组
%% - 群分组排序
%% - 将群加入分组
%% - 创建群标签
%% - 为群添加标签
%% - 按标签筛选群
-module(group_category_tag_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% 测试夹具
group_category_tag_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"创建群分组", fun test_create_category/0},
      {"更新群分组", fun test_update_category/0},
      {"删除群分组", fun test_delete_category/0},
      {"群分组排序", fun test_category_sort/0},
      {"将群加入分组", fun test_add_group_to_category/0},
      {"创建群标签", fun test_create_tag/0},
      {"为群添加标签", fun test_add_tag_to_group/0},
      {"按标签筛选群", fun test_filter_groups_by_tag/0},
      {"批量操作", fun test_batch_operations/0}
     ]
    }.

setup() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    % 创建测试用户
    {ok, User1} = create_test_user(<<"user1_category">>),
    % 创建测试群组
    {ok, Group1} = create_test_group(User1, <<"group1_tag">>),
    {ok, Group2} = create_test_group(User1, <<"group2_tag">>),
    {ok, Group3} = create_test_group(User1, <<"group3_tag">>),
    Context = #{
        user1 => User1,
        group1 => Group1,
        group2 => Group2,
        group3 => Group3
    },
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_create_category() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建群分组
    CategoryName = <<"工作群"/utf8>>,
    {ok, CategoryId} = group_category_logic:create(User1, CategoryName),

    % 2. 验证返回结果
    ?assert(is_integer(CategoryId)),
    ?assert(CategoryId > 0),
    {ok, Category} = group_category_repo:find_by_name(User1, CategoryName),
    ?assertEqual(CategoryId, maps:get(<<"id">>, Category)),
    ?assertEqual(CategoryName, maps:get(<<"category_name">>, Category)),

    ok.

test_update_category() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建分组
    OldName = <<"原始名称"/utf8>>,
    {ok, CategoryId} = group_category_logic:create(User1, OldName),

    % 2. 更新分组
    NewName = <<"更新后的名称"/utf8>>,
    ok = group_category_logic:rename(User1, CategoryId, NewName),

    % 3. 验证更新成功
    {ok, #{}} = group_category_repo:find_by_name(User1, OldName),
    {ok, UpdatedCategory} = group_category_repo:find_by_name(User1, NewName),
    ?assertEqual(CategoryId, maps:get(<<"id">>, UpdatedCategory)),
    ?assertEqual(NewName, maps:get(<<"category_name">>, UpdatedCategory)),

    ok.

test_delete_category() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建分组
    CategoryName = <<"要删除的分组"/utf8>>,
    {ok, CategoryId} = group_category_logic:create(User1, CategoryName),

    % 2. 删除分组
    ok = group_category_logic:delete(User1, CategoryId),

    % 3. 验证删除成功
    {ok, #{}} = group_category_repo:find_by_name(User1, CategoryName),

    ok.

test_category_sort() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建多个分组
    {ok, CatId1} = group_category_logic:create(User1, <<"分组1"/utf8>>),
    {ok, CatId2} = group_category_logic:create(User1, <<"分组2"/utf8>>),
    {ok, CatId3} = group_category_logic:create(User1, <<"分组3"/utf8>>),

    % 2. 更新排序
    NewOrder = [{CatId3, 1}, {CatId1, 2}, {CatId2, 3}],
    ok = group_category_logic:update_sort_order(User1, NewOrder),

    % 3. 验证排序
    {ok, Categories} = group_category_logic:list(User1),
    ActualOrder = custom_category_ids(Categories),
    ?assertEqual([CatId3, CatId1, CatId2], ActualOrder),

    ok.

test_add_group_to_category() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),
    Group2 = maps:get(group2, Context),

    % 1. 创建分组
    {ok, CategoryId} = group_category_logic:create(User1, <<"分组A"/utf8>>),

    % 2. 将群加入分组
    ok = group_category_logic:move_group(User1, Group1, CategoryId),
    ok = group_category_logic:move_group(User1, Group2, CategoryId),

    % 3. 获取分组内的群
    {ok, Groups} = group_category_repo:list_groups_by_category(User1, CategoryId, <<"gm.group_id">>),
    GroupIds = lists:sort([maps:get(<<"group_id">>, Group) || Group <- Groups]),

    % 4. 验证数量
    ?assertEqual(2, length(Groups)),
    ?assertEqual(lists:sort([Group1, Group2]), GroupIds),

    ok.

test_create_tag() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),

    % 1. 创建群标签
    TagName = <<"技术交流"/utf8>>,
    {ok, TagId} = group_tag_logic:add(Group1, User1, TagName),

    % 2. 验证返回结果
    ?assert(is_integer(TagId)),
    {ok, Tags} = group_tag_logic:list(Group1, User1),
    ?assert(lists:any(fun(Tag) ->
        maps:get(<<"id">>, Tag) =:= TagId andalso maps:get(<<"tag_name">>, Tag) =:= TagName
    end, Tags)),

    ok.

test_add_tag_to_group() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),

    % 1. 创建标签
    {ok, _TagId1} = group_tag_logic:add(Group1, User1, <<"标签1"/utf8>>),
    {ok, _TagId2} = group_tag_logic:add(Group1, User1, <<"标签2"/utf8>>),

    % 2. 为群添加标签
    {ok, Tags} = group_tag_logic:list(Group1, User1),
    TagNames = lists:sort([maps:get(<<"tag_name">>, Tag) || Tag <- Tags]),

    % 4. 验证数量
    ?assertEqual(2, length(Tags)),
    ?assertEqual([<<"标签1"/utf8>>, <<"标签2"/utf8>>], TagNames),

    ok.

test_filter_groups_by_tag() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),
    Group2 = maps:get(group2, Context),
    Group3 = maps:get(group3, Context),

    % 1. 创建标签
    TagName = <<"公共标签"/utf8>>,
    {ok, _} = group_tag_logic:add(Group1, User1, TagName),
    {ok, _} = group_tag_logic:add(Group2, User1, TagName),

    % 2. 为群添加标签
    % Group3 不添加标签

    % 3. 按标签筛选群
    {ok, Groups} = group_tag_logic:search(TagName),
    GroupIds = lists:sort([maps:get(<<"group_id">>, Group) || Group <- Groups]),

    % 4. 验证数量
    ?assertEqual(2, length(Groups)),
    ?assertEqual(lists:sort([Group1, Group2]), GroupIds),
    ?assertNot(lists:member(Group3, GroupIds)),

    ok.

test_batch_operations() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),
    Group2 = maps:get(group2, Context),
    Group3 = maps:get(group3, Context),

    % 1. 创建分组
    {ok, CategoryId} = group_category_logic:create(User1, <<"批量测试分组"/utf8>>),

    % 2. 批量将群加入分组
    lists:foreach(fun(GroupId) ->
        ok = group_category_logic:move_group(User1, GroupId, CategoryId)
    end, [Group1, Group2, Group3]),

    % 3. 验证所有群都已加入
    {ok, Groups} = group_category_repo:list_groups_by_category(User1, CategoryId, <<"gm.group_id">>),
    GroupIds = lists:sort([maps:get(<<"group_id">>, Group) || Group <- Groups]),
    ?assertEqual(3, length(Groups)),
    ?assertEqual(lists:sort([Group1, Group2, Group3]), GroupIds),

    % 4. 创建标签
    TagName = <<"批量标签"/utf8>>,
    lists:foreach(fun(GroupId) ->
        {ok, _} = group_tag_logic:add(GroupId, User1, TagName)
    end, [Group1, Group2, Group3]),

    % 5. 批量为群添加标签
    {ok, TaggedGroups0} = group_tag_logic:search(TagName),
    TaggedGroupIds0 = lists:sort([maps:get(<<"group_id">>, Group) || Group <- TaggedGroups0]),
    ?assertEqual(lists:sort([Group1, Group2, Group3]), TaggedGroupIds0),

    % 6. 删除其中一个群的标签并验证收敛
    ok = group_tag_logic:remove(Group2, User1, TagName),
    {ok, Group2Tags} = group_tag_logic:list(Group2, User1),
    ?assertNot(lists:any(fun(Tag) -> maps:get(<<"tag_name">>, Tag) =:= TagName end, Group2Tags)),
    {ok, TaggedGroups} = group_tag_logic:search(TagName),
    TaggedGroupIds = lists:sort([maps:get(<<"group_id">>, Group) || Group <- TaggedGroups]),
    ?assertEqual(lists:sort([Group1, Group3]), TaggedGroupIds),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}).

custom_category_ids(Categories) ->
    [maps:get(<<"id">>, Category)
     || Category <- Categories,
        maps:get(<<"id">>, Category) =/= 0].

create_test_user(Nickname) ->
    Uid = elib_tsid:generate(),
    Suffix = integer_to_binary(erlang:phash2(Uid, 1000000000)),
    User = #{
        <<"uid">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => <<Nickname/binary, "_", Suffix/binary>>,
        <<"mobile">> => list_to_binary(io_lib:format("13~9..0B", [erlang:phash2(Uid, 1000000000)])),
        <<"email">> => <<"test_", Suffix/binary, "@example.com">>,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = user_repo:create(User),
    {ok, Uid}.

create_test_group(OwnerId, Name) ->
    Gid = elib_tsid:generate(),
    Group = #{
        <<"gid">> => Gid,
        <<"owner_uid">> => OwnerId,
        <<"name">> => Name,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = group_repo:create(Group),
    ok = group_member_ds:add_member(Gid, OwnerId),
    {ok, Gid}.
