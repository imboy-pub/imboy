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
    application:set_env(imboy, env, test),
    % 创建测试用户
    {ok, User1} = create_test_user(<<"user1_category">>),
    % 创建测试群组
    {ok, Group1} = create_test_group(User1, <<"group1_tag">>),
    {ok, Group2} = create_test_group(User1, <<"group2_tag">>),
    {ok, Group3} = create_test_group(User1, <<"group3_tag">>),
    #{
        user1 => User1,
        group1 => Group1,
        group2 => Group2,
        group3 => Group3
    }.

cleanup(_Context) ->
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_create_category() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建群分组
    CategoryName = <<"工作群"/utf8>>,
    {ok, Category} = group_category_logic:create(User1, CategoryName),

    % 2. 验证返回结果
    ?assertEqual(CategoryName, maps:get(<<"name">>, Category)),
    ?assertEqual(elib_hashids:encode(User1), maps:get(<<"user_id">>, Category)),

    ok.

test_update_category() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建分组
    {ok, Category} = group_category_logic:create(User1, <<"原始名称"/utf8>>),
    CategoryId = maps:get(<<"id">>, Category),

    % 2. 更新分组
    NewName = <<"更新后的名称"/utf8>>,
    {ok, UpdatedCategory} = group_category_logic:update(CategoryId, User1, NewName),

    % 3. 验证更新成功
    ?assertEqual(NewName, maps:get(<<"name">>, UpdatedCategory)),

    ok.

test_delete_category() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建分组
    {ok, Category} = group_category_logic:create(User1, <<"要删除的分组"/utf8>>),
    CategoryId = maps:get(<<"id">>, Category),

    % 2. 删除分组
    ok = group_category_logic:delete(CategoryId, User1),

    % 3. 验证删除成功
    Result = group_category_repo:find_by_id(CategoryId),
    ?assertMatch({error, not_found}, Result),

    ok.

test_category_sort() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建多个分组
    {ok, Cat1} = group_category_logic:create(User1, <<"分组1"/utf8>>),
    {ok, Cat2} = group_category_logic:create(User1, <<"分组2"/utf8>>),
    {ok, Cat3} = group_category_logic:create(User1, <<"分组3"/utf8>>),

    CatId1 = maps:get(<<"id">>, Cat1),
    CatId2 = maps:get(<<"id">>, Cat2),
    CatId3 = maps:get(<<"id">>, Cat3),

    % 2. 更新排序
    NewOrder = [CatId3, CatId1, CatId2],
    ok = group_category_logic:update_sort(User1, NewOrder),

    % 3. 验证排序
    {ok, Categories} = group_category_logic:list(User1),
    ActualOrder = lists:map(fun(C) -> maps:get(<<"id">>, C) end, Categories),
    ?assertEqual(NewOrder, ActualOrder),

    ok.

test_add_group_to_category() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),
    Group2 = maps:get(group2, Context),

    % 1. 创建分组
    {ok, Category} = group_category_logic:create(User1, <<"分组A"/utf8>>),
    CategoryId = maps:get(<<"id">>, Category),

    % 2. 将群加入分组
    ok = group_category_logic:add_group(CategoryId, User1, Group1),
    ok = group_category_logic:add_group(CategoryId, User1, Group2),

    % 3. 获取分组内的群
    {ok, Groups} = group_category_logic:list_groups(CategoryId, User1),

    % 4. 验证数量
    ?assertEqual(2, length(Groups)),

    ok.

test_create_tag() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),

    % 1. 创建群标签
    TagName = <<"技术交流"/utf8>>,
    {ok, Tag} = group_tag_logic:create(User1, TagName),

    % 2. 验证返回结果
    ?assertEqual(TagName, maps:get(<<"name">>, Tag)),

    ok.

test_add_tag_to_group() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),

    % 1. 创建标签
    {ok, Tag1} = group_tag_logic:create(User1, <<"标签1"/utf8>>),
    {ok, Tag2} = group_tag_logic:create(User1, <<"标签2"/utf8>>),

    TagId1 = maps:get(<<"id">>, Tag1),
    TagId2 = maps:get(<<"id">>, Tag2),

    % 2. 为群添加标签
    ok = group_tag_logic:add_tag_to_group(Group1, TagId1),
    ok = group_tag_logic:add_tag_to_group(Group1, TagId2),

    % 3. 获取群的标签
    {ok, Tags} = group_tag_logic:list_group_tags(Group1),

    % 4. 验证数量
    ?assertEqual(2, length(Tags)),

    ok.

test_filter_groups_by_tag() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),
    Group2 = maps:get(group2, Context),
    Group3 = maps:get(group3, Context),

    % 1. 创建标签
    {ok, Tag} = group_tag_logic:create(User1, <<"公共标签"/utf8>>),
    TagId = maps:get(<<"id">>, Tag),

    % 2. 为群添加标签
    ok = group_tag_logic:add_tag_to_group(Group1, TagId),
    ok = group_tag_logic:add_tag_to_group(Group2, TagId),
    % Group3 不添加标签

    % 3. 按标签筛选群
    {ok, Groups} = group_tag_logic:list_groups_by_tag(TagId),

    % 4. 验证数量
    ?assertEqual(2, length(Groups)),

    ok.

test_batch_operations() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group1 = maps:get(group1, Context),
    Group2 = maps:get(group2, Context),
    Group3 = maps:get(group3, Context),

    % 1. 创建分组
    {ok, Category} = group_category_logic:create(User1, <<"批量测试分组"/utf8>>),
    CategoryId = maps:get(<<"id">>, Category),

    % 2. 批量将群加入分组
    ok = group_category_logic:add_groups(CategoryId, User1, [Group1, Group2, Group3]),

    % 3. 验证所有群都已加入
    {ok, Groups} = group_category_logic:list_groups(CategoryId, User1),
    ?assertEqual(3, length(Groups)),

    % 4. 创建标签
    {ok, Tag} = group_tag_logic:create(User1, <<"批量标签"/utf8>>),
    TagId = maps:get(<<"id">>, Tag),

    % 5. 批量为群添加标签
    ok = group_tag_logic:add_tag_to_groups([Group1, Group2, Group3], TagId),

    % 6. 验证标签关联
    {ok, TaggedGroups} = group_tag_logic:list_groups_by_tag(TagId),
    ?assertEqual(3, length(TaggedGroups)),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    get(test_context).

create_test_user(Nickname) ->
    Uid = imboy_hashid:uid(),
    User = #{
        <<"uid">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => Nickname,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = user_repo:create(User),
    {ok, Uid}.

create_test_group(OwnerId, Name) ->
    Gid = imboy_hashid:uid(),
    Group = #{
        <<"gid">> => Gid,
        <<"owner_uid">> => OwnerId,
        <<"name">> => Name,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = group_repo:create(Group),
    ok = group_member_ds:add_member(Gid, OwnerId),
    {ok, Gid}.
