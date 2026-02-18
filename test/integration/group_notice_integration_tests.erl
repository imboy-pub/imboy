%% @doc 群公告功能集成测试
%% 测试范围：
%% - 创建群公告
%% - 更新群公告
%% - 删除群公告
%% - 获取群公告列表
%% - 置顶群公告
%% - 权限控制
-module(group_notice_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% 测试夹具
group_notice_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"创建群公告", fun test_create_notice/0},
      {"更新群公告", fun test_update_notice/0},
      {"删除群公告", fun test_delete_notice/0},
      {"获取群公告列表", fun test_list_notices/0},
      {"置顶群公告", fun test_pin_notice/0},
      {"普通成员无法创建公告", fun test_member_cannot_create/0},
      {"公告内容长度限制", fun test_notice_length_limit/0},
      {"批量操作公告", fun test_batch_notices/0}
     ]
    }.

setup() ->
    application:set_env(imboy, env, test),
    % 创建测试用户
    {ok, Owner} = create_test_user(<<"owner_notice">>),
    {ok, Admin} = create_test_user(<<"admin_notice">>),
    {ok, Member} = create_test_user(<<"member_notice">>),
    % 创建测试群组
    {ok, Group} = create_test_group(Owner, <<"notice_test_group">>),
    % 添加管理员和普通成员
    ok = group_member_ds:add_member(Group, Admin),
    ok = group_member_ds:add_member(Group, Member),
    % 设置管理员角色
    ok = group_member_ds:set_role(Group, Admin, <<"admin">>),
    #{
        owner => Owner,
        admin => Admin,
        member => Member,
        group => Group
    }.

cleanup(_Context) ->
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_create_notice() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Group = maps:get(group, Context),

    % 1. 群主创建公告
    Content = <<"这是第一条群公告，用于测试创建功能。"/utf8>>,
    {ok, Notice} = group_notice_logic:create(Group, Owner, Content),

    % 2. 验证返回结果
    ?assertEqual(Content, maps:get(<<"content">>, Notice)),
    ?assertEqual(elib_hashids:encode(Owner), maps:get(<<"author_id">>, Notice)),
    ?assertEqual(false, maps:get(<<"is_pinned">>, Notice)),

    ok.

test_update_notice() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Group = maps:get(group, Context),

    % 1. 创建公告
    {ok, Notice} = group_notice_logic:create(Group, Owner, <<"原始公告"/utf8>>),
    NoticeId = maps:get(<<"id">>, Notice),

    % 2. 更新公告
    NewContent = <<"更新后的公告内容"/utf8>>,
    {ok, UpdatedNotice} = group_notice_logic:update(Group, NoticeId, Owner, NewContent),

    % 3. 验证更新成功
    ?assertEqual(NewContent, maps:get(<<"content">>, UpdatedNotice)),

    ok.

test_delete_notice() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Group = maps:get(group, Context),

    % 1. 创建公告
    {ok, Notice} = group_notice_logic:create(Group, Owner, <<"要删除的公告"/utf8>>),
    NoticeId = maps:get(<<"id">>, Notice),

    % 2. 删除公告
    ok = group_notice_logic:delete(Group, NoticeId, Owner),

    % 3. 验证删除成功（查询应该返回空或错误）
    Result = group_notice_repo:find_by_id(NoticeId),
    ?assertMatch({error, not_found}, Result),

    ok.

test_list_notices() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Group = maps:get(group, Context),

    % 1. 创建多条公告
    lists:foreach(fun(N) ->
        Content = <<N/integer, "号公告"/utf8>>,
        {ok, _} = group_notice_logic:create(Group, Owner, Content)
    end, lists:seq(1, 5)),

    % 2. 获取公告列表
    {ok, Notices} = group_notice_logic:list(Group, #{limit => 20}),

    % 3. 验证数量
    ?assertEqual(5, length(Notices)),

    ok.

test_pin_notice() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Group = maps:get(group, Context),

    % 1. 创建公告
    {ok, Notice} = group_notice_logic:create(Group, Owner, <<"要置顶的公告"/utf8>>),
    NoticeId = maps:get(<<"id">>, Notice),

    % 2. 置顶公告
    ok = group_notice_logic:pin(Group, NoticeId, Owner),

    % 3. 验证置顶状态
    {ok, PinnedNotice} = group_notice_repo:find_by_id(NoticeId),
    ?assertEqual(true, maps:get(<<"is_pinned">>, PinnedNotice)),

    % 4. 取消置顶
    ok = group_notice_logic:unpin(Group, NoticeId, Owner),

    % 5. 验证取消成功
    {ok, UnpinnedNotice} = group_notice_repo:find_by_id(NoticeId),
    ?assertEqual(false, maps:get(<<"is_pinned">>, UnpinnedNotice)),

    ok.

test_member_cannot_create() ->
    Context = get_context(),
    Member = maps:get(member, Context),
    Group = maps:get(group, Context),

    % 普通成员尝试创建公告
    Result = group_notice_logic:create(Group, Member, <<"普通成员公告"/utf8>>),

    % 验证失败
    ?assertMatch({error, permission_denied}, Result),

    ok.

test_notice_length_limit() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Group = maps:get(group, Context),

    % 1. 创建超长公告（假设限制2000字符）
    LongContent = binary:copy(<<"a">>, 3000),
    Result = group_notice_logic:create(Group, Owner, LongContent),

    % 2. 验证失败
    ?assertMatch({error, {invalid_param, _}}, Result),

    ok.

test_batch_notices() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin = maps:get(admin, Context),
    Group = maps:get(group, Context),

    % 1. 群主和管理员分别创建多条公告
    {ok, _} = group_notice_logic:create(Group, Owner, <<"群主公告1"/utf8>>),
    {ok, _} = group_notice_logic:create(Group, Owner, <<"群主公告2"/utf8>>),
    {ok, _} = group_notice_logic:create(Group, Admin, <<"管理员公告1"/utf8>>),

    % 2. 获取列表
    {ok, Notices} = group_notice_logic:list(Group, #{limit => 20}),

    % 3. 验证数量
    ?assertEqual(3, length(Notices)),

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
