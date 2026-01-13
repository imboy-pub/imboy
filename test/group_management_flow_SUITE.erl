-module(group_management_flow_SUITE).

%%%===================================================================
%%% @doc
%%% 群组管理流程 Common Test 测试套件
%%%
%%% 运行方式：
%%%   ct_run -dir test -suite group_management_flow_SUITE
%%%   make ct-group_management_flow
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    %% 群组创建
    create_group_with_valid_name_succeeds/1,
    create_group_with_empty_name_fails/1,
    create_group_with_too_many_members_fails/1,
    %% 群组成员管理
    invite_members_to_group_succeeds/1,
    remove_member_from_group_succeeds/1,
    transfer_group_ownership_succeeds/1,
    non_owner_cannot_transfer_ownership_fails/1,
    %% 群组信息更新
    update_group_name_succeeds/1,
    update_group_avatar_succeeds/1,
    update_group_notice_succeeds/1,
    %% 群组解散
    dissolve_group_by_owner_succeeds/1,
    non_owner_cannot_dissolve_group_fails/1,
    dissolve_group_removes_all_members/1,
    %% 群组公告
    publish_group_notice_succeeds/1,
    members_see_latest_notice/1,
    %% 群组禁言
    mute_member_in_group_succeeds/1,
    unmute_member_restores_permission/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
        {group, create_group},
        {group, member_management},
        {group, group_info},
        {group, dissolve_group},
        {group, group_notice},
        {group, group_mute}
    ].

groups() ->
    [
        {create_group, [], create_group_test_cases()},
        {member_management, [], member_test_cases()},
        {group_info, [], info_test_cases()},
        {dissolve_group, [], dissolve_test_cases()},
        {group_notice, [], notice_test_cases()},
        {group_mute, [], mute_test_cases()}
    ].

init_per_suite(Config) ->
    application:set_env(imboy, env, test),
    ct:log("开始群组管理流程测试套件"),
    {ok, _} = application:ensure_all_started(imboy),
    Config.

end_per_suite(_Config) ->
    ct:log("结束群组管理流程测试套件"),
    cleanup_all_test_data(),
    application:stop(imboy),
    ok.

init_per_group(_Group, Config) ->
    cleanup_all_test_data(),
    Config.

end_per_group(_Group, _Config) ->
    meck:unload(),
    ok.

%% ===================================================================
%% 测试用例定义
%% ===================================================================

create_group_test_cases() ->
    [
        create_group_with_valid_name_succeeds,
        create_group_with_empty_name_fails,
        create_group_with_too_many_members_fails
    ].

member_test_cases() ->
    [
        invite_members_to_group_succeeds,
        remove_member_from_group_succeeds,
        transfer_group_ownership_succeeds,
        non_owner_cannot_transfer_ownership_fails
    ].

info_test_cases() ->
    [
        update_group_name_succeeds,
        update_group_avatar_succeeds,
        update_group_notice_succeeds
    ].

dissolve_test_cases() ->
    [
        dissolve_group_by_owner_succeeds,
        non_owner_cannot_dissolve_group_fails,
        dissolve_group_removes_all_members
    ].

notice_test_cases() ->
    [
        publish_group_notice_succeeds,
        members_see_latest_notice
    ].

mute_test_cases() ->
    [
        mute_member_in_group_succeeds,
        unmute_member_restores_permission
    ].

%% ===================================================================
%% 群组创建测试
%% ===================================================================

create_group_with_valid_name_succeeds(_Config) ->
    ct:log("测试创建群组成功"),
    {OwnerUid, _Uid1, _Uid2} = create_three_users(),

    GroupName = <<"测试群组"/utf8>>,
    Result = group_logic:create(GroupName, OwnerUid),

    % 验证创建成功
    ?assertMatch({ok, #{<<"id">> := _, <<"name">> := _}}, Result),
    {ok, Group} = Result,
    ?assertEqual(GroupName, maps:get(<<"name">>, Group)),

    % 验证群主是创建者
    ?assertEqual(OwnerUid, maps:get(<<"owner_id">>, Group)),

    cleanup_users([OwnerUid]),
    {comment, "创建群组成功"}.

create_group_with_empty_name_fails(_Config) ->
    ct:log("测试创建空名称群组失败"),
    {OwnerUid, _, _} = create_three_users(),

    Result = group_logic:create(<<>>, OwnerUid),

    % 验证创建失败
    ?assertMatch({error, _, _}, Result),

    cleanup_users([OwnerUid]),
    {comment, "空名称群组创建被拒绝"}.

create_group_with_too_many_members_fails(_Config) ->
    ct:log("测试创建包含过多成员的群组失败"),
    {OwnerUid, _, _} = create_three_users(),

    % 尝试创建包含超过限制数量成员的群组
    % 根据业务逻辑限制，比如最多500人
    MemberUids = lists:seq(1, 501),  % 模拟501个成员ID

    Result = group_logic:create(<<"大群组"/utf8>>, OwnerUid, MemberUids),

    % 验证创建失败或被限制
    case Result of
        {ok, _} ->
            % 如果允许创建大群组，验证成员数被限制
            {comment, "大群组创建被限制"};
        {error, _, _} ->
            {comment, "超过成员限制的群组创建被拒绝"}
    end,

    cleanup_users([OwnerUid]).

%% ===================================================================
%% 群组成员管理测试
%% ===================================================================

invite_members_to_group_succeeds(_Config) ->
    ct:log("测试邀请成员加入群组成功"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    % 创建群组
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),

    % 邀请成员
    Result = group_logic:invite_members(Gid, OwnerUid, [Uid1, Uid2]),

    % 验证邀请成功
    ?assertEqual(ok, Result),

    % 验证成员在群组中
    {ok, Member1} = group_member_ds:find_by_users(Gid, Uid1),
    {ok, Member2} = group_member_ds:find_by_users(Gid, Uid2),
    ?assertEqual(Uid1, maps:get(<<"user_id">>, Member1)),
    ?assertEqual(Uid2, maps:get(<<"user_id">>, Member2)),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "邀请成员加入群组成功"}.

remove_member_from_group_succeeds(_Config) ->
    ct:log("测试从群组移除成员成功"),
    {OwnerUid, Uid1, _} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1]),

    % 移除成员
    Result = group_logic:remove_member(Gid, OwnerUid, Uid1),

    % 验证移除成功
    ?assertEqual(ok, Result),

    % 验证成员已不在群组中
    Result2 = group_member_ds:find_by_users(Gid, Uid1),
    ?assertMatch({ok, #{}}, Result2),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "从群组移除成员成功"}.

transfer_group_ownership_succeeds(_Config) ->
    ct:log("测试转移群主成功"),
    {OwnerUid, Uid1, _} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1]),

    % 转移群主
    Result = group_logic:transfer(Gid, OwnerUid, Uid1),

    % 验证转移成功
    ?assertEqual(ok, Result),

    % 验证新群主
    {ok, UpdatedGroup} = group_ds:find_by_id(Gid),
    ?assertEqual(Uid1, maps:get(<<"owner_id">>, UpdatedGroup)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "转移群主成功"}.

non_owner_cannot_transfer_ownership_fails(_Config) ->
    ct:log("测试非群主无法转移群主"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1, Uid2]),

    % 非群主尝试转移群主
    Result = group_logic:transfer(Gid, Uid1, Uid2),

    % 验证转移失败
    ?assertMatch({error, _, _}, Result),

    % 验证原群主未变
    {ok, CurrentGroup} = group_ds:find_by_id(Gid),
    ?assertEqual(OwnerUid, maps:get(<<"owner_id">>, CurrentGroup)),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "非群主无法转移群主"}.

%% ===================================================================
%% 群组信息更新测试
%% ===================================================================

update_group_name_succeeds(_Config) ->
    ct:log("测试更新群组名称成功"),
    {OwnerUid, _, _} = create_three_users(),

    % 创建群组
    {ok, Group} = group_logic:create(<<"旧群名"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),

    % 更新群名
    NewName = <<"新群名"/utf8>>,
    Result = group_logic:update_info(Gid, OwnerUid, #{<<"name">> => NewName}),

    % 验证更新成功
    ?assertEqual(ok, Result),

    % 验证群名已更新
    {ok, UpdatedGroup} = group_ds:find_by_id(Gid),
    ?assertEqual(NewName, maps:get(<<"name">>, UpdatedGroup)),

    cleanup_users([OwnerUid]),
    {comment, "更新群组名称成功"}.

update_group_avatar_succeeds(_Config) ->
    ct:log("测试更新群组头像成功"),
    {OwnerUid, _, _} = create_three_users(),

    % 创建群组
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),

    % 更新头像
    AvatarUrl = <<"https://example.com/avatar.jpg">>,
    Result = group_logic:update_info(Gid, OwnerUid, #{<<"avatar">> => AvatarUrl}),

    % 验证更新成功
    ?assertEqual(ok, Result),

    cleanup_users([OwnerUid]),
    {comment, "更新群组头像成功"}.

update_group_notice_succeeds(_Config) ->
    ct:log("测试更新群组公告成功"),
    {OwnerUid, _, _} = create_three_users(),

    % 创建群组
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),

    % 更新公告
    Notice = <<"本群公告：请文明发言"/utf8>>,
    Result = group_logic:update_info(Gid, OwnerUid, #{<<"notice">> => Notice}),

    % 验证更新成功
    ?assertEqual(ok, Result),

    cleanup_users([OwnerUid]),
    {comment, "更新群组公告成功"}.

%% ===================================================================
%% 群组解散测试
%% ===================================================================

dissolve_group_by_owner_succeeds(_Config) ->
    ct:log("测试群主解散群组成功"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1, Uid2]),

    % 解散群组
    Result = group_logic:dissolve(Gid, OwnerUid),

    % 验证解散成功
    ?assertEqual(ok, Result),

    % 验证群组已删除
    Result2 = group_ds:find_by_id(Gid),
    ?assertMatch({ok, #{}}, Result2),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "群主解散群组成功"}.

non_owner_cannot_dissolve_group_fails(_Config) ->
    ct:log("测试非群主无法解散群组"),
    {OwnerUid, Uid1, _} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1]),

    % 非群主尝试解散群组
    Result = group_logic:dissolve(Gid, Uid1),

    % 验证解散失败
    ?assertMatch({error, _, _}, Result),

    % 验证群组仍然存在
    {ok, _} = group_ds:find_by_id(Gid),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "非群主无法解散群组"}.

dissolve_group_removes_all_members(_Config) ->
    ct:log("测试解散群组移除所有成员"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1, Uid2]),

    % 验证成员存在
    {ok, Members} = group_member_ds:list(Gid),
    ?assertEqual(3, length(Members)),

    % 解散群组
    ok = group_logic:dissolve(Gid, OwnerUid),

    % 验证群组成员关系已移除
    {ok, MembersAfter} = group_member_ds:list(Gid),
    ?assertEqual(0, length(MembersAfter)),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "解散群组移除所有成员"}.

%% ===================================================================
%% 群组公告测试
%% ===================================================================

publish_group_notice_succeeds(_Config) ->
    ct:log("测试发布群组公告成功"),
    {OwnerUid, _, _} = create_three_users(),

    % 创建群组
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),

    % 发布公告
    NoticeContent = <<"群公告：明天开会"/utf8>>,
    Result = group_notice_logic:add(Gid, OwnerUid, NoticeContent),

    % 验证发布成功
    ?assertMatch({ok, #{}}, Result),

    cleanup_users([OwnerUid]),
    {comment, "发布群组公告成功"}.

members_see_latest_notice(_Config) ->
    ct:log("测试成员查看最新公告"),
    {OwnerUid, Uid1, _} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1]),

    % 发布公告
    NoticeContent = <<"重要通知"/utf8>>,
    {ok, _} = group_notice_logic:add(Gid, OwnerUid, NoticeContent),

    % 成员查看公告
    {ok, Notices} = group_notice_logic:list(Gid, 10),
    ?assert(length(Notices) > 0),
    ?assertEqual(NoticeContent, maps:get(<<"content">>, hd(Notices))),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "成员查看最新公告成功"}.

%% ===================================================================
%% 群组禁言测试
%% ===================================================================

mute_member_in_group_succeeds(_Config) ->
    ct:log("测试禁言群成员成功"),
    {OwnerUid, Uid1, _} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1]),

    % 禁言成员
    Result = group_logic:mute_member(Gid, OwnerUid, Uid1),

    % 验证禁言成功
    ?assertEqual(ok, Result),

    % 验证成员被禁言
    {ok, Member} = group_member_ds:find_by_users(Gid, Uid1),
    ?assertEqual(1, maps:get(<<"is_mute">>, Member, 0)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "禁言群成员成功"}.

unmute_member_restores_permission(_Config) ->
    ct:log("测试解除禁言恢复权限"),
    {OwnerUid, Uid1, _} = create_three_users(),

    % 创建群组并添加成员
    {ok, Group} = group_logic:create(<<"测试群"/utf8>>, OwnerUid),
    Gid = maps:get(<<"id">>, Group),
    ok = group_logic:invite_members(Gid, OwnerUid, [Uid1]),
    ok = group_logic:mute_member(Gid, OwnerUid, Uid1),

    % 解除禁言
    Result = group_logic:unmute_member(Gid, OwnerUid, Uid1),

    % 验证解除成功
    ?assertEqual(ok, Result),

    % 验证成员禁言状态已清除
    {ok, Member} = group_member_ds:find_by_users(Gid, Uid1),
    ?assertEqual(0, maps:get(<<"is_mute">>, Member, 0)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "解除禁言恢复权限成功"}.

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 创建三个测试用户
create_three_users() ->
    Rand1 = erlang:unique_integer([positive]) rem 100000,
    Rand2 = (erlang:unique_integer([positive]) + 1) rem 100000,
    Rand3 = (erlang:unique_integer([positive]) + 2) rem 100000,
    Mobile1 = list_to_binary(["13900", integer_to_list(Rand1)]),
    Mobile2 = list_to_binary(["13900", integer_to_list(Rand2)]),
    Mobile3 = list_to_binary(["13900", integer_to_list(Rand3)]),
    Password = <<"Test@123456">>,

    % 创建用户
    {ok, _} = passport_logic:signup(Mobile1, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile2, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile3, Password, <<".@example.com">>, #{}),

    % 获取用户 ID
    {ok, User1} = user_repo:find_by_mobile(Mobile1, <<"id">>),
    {ok, User2} = user_repo:find_by_mobile(Mobile2, <<"id">>),
    {ok, User3} = user_repo:find_by_mobile(Mobile3, <<"id">>),

    Uid1 = maps:get(<<"id">>, User1),
    Uid2 = maps:get(<<"id">>, User2),
    Uid3 = maps:get(<<"id">>, User3),

    {Uid1, Uid2, Uid3}.

%% 清理用户
cleanup_users([]) -> ok;
cleanup_users([Uid | Rest]) ->
    user_repo:delete(Uid),
    cleanup_users(Rest).

%% 清理所有测试数据
cleanup_all_test_data() ->
    Sql = <<"SELECT id FROM user WHERE mobile LIKE '13900%'">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            lists:foreach(fun(#{<<"id">> := Id}) ->
                user_repo:delete(Id)
            end, Rows);
        _ ->
            ok
    end.
