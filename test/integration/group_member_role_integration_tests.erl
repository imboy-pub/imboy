%% @doc 群组成员角色管理集成测试
%% 测试范围：
%% - 设置管理员
%% - 移除管理员
%% - 转让群主
%% - 移除成员
%% - 禁言成员
%% - 角色权限验证
-module(group_member_role_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% 测试夹具
group_member_role_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"设置管理员", fun test_set_admin/0},
      {"移除管理员", fun test_remove_admin/0},
      {"转让群主", fun test_transfer_owner/0},
      {"移除普通成员", fun test_remove_member/0},
      {"禁言成员", fun test_mute_member/0},
      {"取消禁言", fun test_unmute_member/0},
      {"管理员权限验证", fun test_admin_permission/0},
      {"普通成员权限限制", fun test_member_permission/0},
      {"批量设置管理员", fun test_batch_set_admin/0}
     ]
    }.

setup() ->
    application:set_env(imboy, env, test),
    % 创建测试用户
    {ok, Owner} = create_test_user(<<"owner_role">>),
    {ok, Admin1} = create_test_user(<<"admin1_role">>),
    {ok, Admin2} = create_test_user(<<"admin2_role">>),
    {ok, Member1} = create_test_user(<<"member1_role">>),
    {ok, Member2} = create_test_user(<<"member2_role">>),
    % 创建测试群组
    {ok, Group} = create_test_group(Owner, <<"role_test_group">>),
    % 添加成员
    ok = group_member_ds:add_member(Group, Admin1),
    ok = group_member_ds:add_member(Group, Admin2),
    ok = group_member_ds:add_member(Group, Member1),
    ok = group_member_ds:add_member(Group, Member2),
    #{
        owner => Owner,
        admin1 => Admin1,
        admin2 => Admin2,
        member1 => Member1,
        member2 => Member2,
        group => Group
    }.

cleanup(_Context) ->
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_set_admin() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member1 = maps:get(member1, Context),
    Group = maps:get(group, Context),

    % 1. 设置管理员
    ok = group_member_logic:set_role(Group, Member1, <<"admin">>, Owner),

    % 2. 验证角色已更新
    {ok, MemberInfo} = group_member_ds:member_info(Group, Member1),
    ?assertEqual(<<"admin">>, maps:get(<<"role">>, MemberInfo)),

    ok.

test_remove_admin() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin1 = maps:get(admin1, Context),
    Group = maps:get(group, Context),

    % 1. 先设置为管理员
    ok = group_member_logic:set_role(Group, Admin1, <<"admin">>, Owner),

    % 2. 移除管理员（降为普通成员）
    ok = group_member_logic:set_role(Group, Admin1, <<"member">>, Owner),

    % 3. 验证角色已更新
    {ok, MemberInfo} = group_member_ds:member_info(Group, Admin1),
    ?assertEqual(<<"member">>, maps:get(<<"role">>, MemberInfo)),

    ok.

test_transfer_owner() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin1 = maps:get(admin1, Context),
    Group = maps:get(group, Context),

    % 1. 转让群主
    ok = group_member_logic:transfer_owner(Group, Owner, Admin1),

    % 2. 验证新群主
    {ok, GroupInfo} = group_repo:find_by_gid(Group),
    ?assertEqual(Admin1, maps:get(<<"owner_uid">>, GroupInfo)),

    % 3. 验证原群主变为管理员
    {ok, OldOwnerInfo} = group_member_ds:member_info(Group, Owner),
    ?assertEqual(<<"admin">>, maps:get(<<"role">>, OldOwnerInfo)),

    ok.

test_remove_member() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member2 = maps:get(member2, Context),
    Group = maps:get(group, Context),

    % 1. 移除成员
    ok = group_member_logic:remove_member(Group, Member2, Owner),

    % 2. 验证成员已移除
    false = group_member_ds:is_member(Group, Member2),

    ok.

test_mute_member() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member1 = maps:get(member1, Context),
    Group = maps:get(group, Context),

    % 1. 禁言成员（1小时）
    MuteUntil = elib_dt:millisecond() + 3600000,
    ok = group_member_logic:mute_member(Group, Member1, MuteUntil, Owner),

    % 2. 验证禁言状态
    true = group_member_ds:is_muted(Group, Member1),

    ok.

test_unmute_member() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member1 = maps:get(member1, Context),
    Group = maps:get(group, Context),

    % 1. 先禁言
    MuteUntil = elib_dt:millisecond() + 3600000,
    ok = group_member_logic:mute_member(Group, Member1, MuteUntil, Owner),

    % 2. 取消禁言
    ok = group_member_logic:unmute_member(Group, Member1, Owner),

    % 3. 验证禁言已取消
    false = group_member_ds:is_muted(Group, Member1),

    ok.

test_admin_permission() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin1 = maps:get(admin1, Context),
    Member1 = maps:get(member1, Context),
    Group = maps:get(group, Context),

    % 1. 设置为管理员
    ok = group_member_logic:set_role(Group, Admin1, <<"admin">>, Owner),

    % 2. 管理员移除普通成员
    ok = group_member_logic:remove_member(Group, Member1, Admin1),

    % 3. 验证成功
    false = group_member_ds:is_member(Group, Member1),

    ok.

test_member_permission() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member1 = maps:get(member1, Context),
    Member2 = maps:get(member2, Context),
    Group = maps:get(group, Context),

    % 普通成员尝试移除其他成员
    Result = group_member_logic:remove_member(Group, Member2, Member1),

    % 验证失败
    ?assertMatch({error, permission_denied}, Result),

    ok.

test_batch_set_admin() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin1 = maps:get(admin1, Context),
    Admin2 = maps:get(admin2, Context),
    Group = maps:get(group, Context),

    % 1. 批量设置管理员
    ok = group_member_logic:set_role(Group, Admin1, <<"admin">>, Owner),
    ok = group_member_logic:set_role(Group, Admin2, <<"admin">>, Owner),

    % 2. 获取管理员列表
    {ok, Admins} = group_member_ds:list_admins(Group),

    % 3. 验证数量（包含群主 + 2个管理员）
    ?assertEqual(3, length(Admins)),

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
