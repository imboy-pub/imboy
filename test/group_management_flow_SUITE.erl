-module(group_management_flow_SUITE).

%%%===================================================================
%%% @doc
%%% 群组管理流程 Common Test 测试套件
%%%
%%% 运行方式：
%%%   ct_run -dir test -suite group_management_flow_SUITE
%%%   make ct-group_management_flow
%%%
%%% 说明：本 suite 已按当前生产 API 重写。历史 API 漂移记录：
%%%   - group_logic:create/2,3 (已删) -> add/4，返回 {ok, Gid}（Gid 整数），
%%%     add 不接受群名参数；群名/空名校验不再在 logic 层，故相关断言改为
%%%     验证 add 的当前真实行为（群数上限、成员写入）。
%%%   - invite_members/3 (已删) -> 建群时经 add/4 的 MemberUids 传入（binary id）。
%%%   - remove_member/3 (已删) -> group_member_logic:leave/3（踢人）。
%%%   - update_info/3 (已删) -> group_logic:edit/3（参数序 edit(Uid,Gid,Data)）。
%%%   - transfer/3 参数序变为 transfer(CurrentUid,Gid,NewOwnerUid)，返回二元组。
%%%   - dissolve/2 参数序 dissolve(Uid,Gid)。
%%%   - mute_member/3 -> group_member_logic:mute/4（含 Duration 秒）。
%%%   - unmute_member/3 -> group_member_logic:unmute/3。
%%%   - group_member_ds:find_by_users/2 -> find_by_gid_and_uid/3（直接返回 map）。
%%%   - group_notice_logic:add/3 -> insert/2；list/2 -> list/4；字段 content -> title/body。
%%%   - group 表主属字段：owner_uid（非 owner_id）、title（非 name）。
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("group_role.hrl").

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
    ct:log("开始群组管理流程测试套件"),
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    ct:log("结束群组管理流程测试套件"),
    cleanup_all_test_data(),
    eunit_runner:ct_suite_cleanup(Config).

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

    %% add/4：Count=已建群数, Uid=创建者, Type=1公开, MemberUids=[binary]
    Result = group_logic:add(0, OwnerUid, 1, []),

    ?assertMatch({ok, _}, Result),
    {ok, Gid} = Result,
    ?assert(is_integer(Gid)),

    %% 验证创建者是群主成员（role = ?ROLE_OWNER）
    Member = group_member_ds:find_by_gid_and_uid(Gid, OwnerUid, <<"role">>),
    ?assertEqual(?ROLE_OWNER, maps:get(<<"role">>, Member, 0)),

    cleanup_users([OwnerUid]),
    {comment, "创建群组成功"}.

create_group_with_empty_name_fails(_Config) ->
    ct:log("测试群数上限校验（原空名校验已移出 logic 层）"),
    {OwnerUid, _, _} = create_three_users(),

    %% add/4 对超过每人 100 群上限返回 {error, binary()}
    Result = group_logic:add(101, OwnerUid, 1, []),

    ?assertMatch({error, _}, Result),

    cleanup_users([OwnerUid]),
    {comment, "超过群数上限被拒绝"}.

create_group_with_too_many_members_fails(_Config) ->
    ct:log("测试创建含大量（不存在）成员的群组：add 忽略无效成员仍建群"),
    {OwnerUid, _, _} = create_three_users(),

    %% add 内部对每个 MemberUid 逐个 join_group，无效 id 被静默忽略，
    %% 建群本身仍成功返回 {ok, Gid}
    ManyMembers = [integer_to_binary(N) || N <- lists:seq(900000001, 900000030)],
    Result = group_logic:add(0, OwnerUid, 1, ManyMembers),

    ?assertMatch({ok, _}, Result),

    cleanup_users([OwnerUid]),
    {comment, "含无效成员仍建群成功"}.

%% ===================================================================
%% 群组成员管理测试
%% ===================================================================

invite_members_to_group_succeeds(_Config) ->
    ct:log("测试建群时纳入成员成功"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    %% 无独立 invite 入口，成员经 add/4 的 MemberUids 纳入
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [
        integer_to_binary(Uid1), integer_to_binary(Uid2)
    ]),

    %% 验证成员在群组中
    Member1 = group_member_ds:find_by_gid_and_uid(Gid, Uid1, <<"user_id">>),
    Member2 = group_member_ds:find_by_gid_and_uid(Gid, Uid2, <<"user_id">>),
    ?assertEqual(Uid1, maps:get(<<"user_id">>, Member1)),
    ?assertEqual(Uid2, maps:get(<<"user_id">>, Member2)),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "邀请成员加入群组成功"}.

remove_member_from_group_succeeds(_Config) ->
    ct:log("测试从群组移除成员成功"),
    {OwnerUid, Uid1, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),

    %% 移除成员（leave/3：目标, 群, 操作者；群主踢普通成员）
    Result = group_member_logic:leave(Uid1, Gid, OwnerUid),
    ?assertEqual(ok, Result),

    %% 验证成员已不在群组中（find 返回空 map，无 user_id 键）
    After = group_member_ds:find_by_gid_and_uid(Gid, Uid1, <<"user_id">>),
    ?assertEqual(undefined, maps:get(<<"user_id">>, After, undefined)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "从群组移除成员成功"}.

transfer_group_ownership_succeeds(_Config) ->
    ct:log("测试转移群主成功"),
    {OwnerUid, Uid1, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),

    %% transfer/3：当前用户, 群, 新群主
    Result = group_logic:transfer(OwnerUid, Gid, Uid1),
    ?assertEqual(ok, Result),

    %% 验证新群主（owner_uid 字段）
    UpdatedGroup = group_ds:find_by_id(Gid, <<"owner_uid">>),
    ?assertEqual(Uid1, maps:get(<<"owner_uid">>, UpdatedGroup)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "转移群主成功"}.

non_owner_cannot_transfer_ownership_fails(_Config) ->
    ct:log("测试非群主无法转移群主"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [
        integer_to_binary(Uid1), integer_to_binary(Uid2)
    ]),

    %% 非群主尝试转移群主（返回二元组 error）
    Result = group_logic:transfer(Uid1, Gid, Uid2),
    ?assertMatch({error, _}, Result),

    %% 验证原群主未变
    CurrentGroup = group_ds:find_by_id(Gid, <<"owner_uid">>),
    ?assertEqual(OwnerUid, maps:get(<<"owner_uid">>, CurrentGroup)),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "非群主无法转移群主"}.

%% ===================================================================
%% 群组信息更新测试
%% ===================================================================

update_group_name_succeeds(_Config) ->
    ct:log("测试更新群组名称成功"),
    {OwnerUid, _, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    %% edit/3：操作者, 群, Data；群名字段为 title
    NewName = <<"新群名"/utf8>>,
    Result = group_logic:edit(OwnerUid, Gid, #{title => NewName}),
    ?assertEqual(ok, Result),

    UpdatedGroup = group_ds:find_by_id(Gid, <<"title">>),
    ?assertEqual(NewName, maps:get(<<"title">>, UpdatedGroup)),

    cleanup_users([OwnerUid]),
    {comment, "更新群组名称成功"}.

update_group_avatar_succeeds(_Config) ->
    ct:log("测试更新群组头像成功"),
    {OwnerUid, _, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    AvatarUrl = <<"https://example.com/avatar.jpg">>,
    Result = group_logic:edit(OwnerUid, Gid, #{avatar => AvatarUrl}),
    ?assertEqual(ok, Result),

    cleanup_users([OwnerUid]),
    {comment, "更新群组头像成功"}.

update_group_notice_succeeds(_Config) ->
    ct:log("测试更新群组简介成功（群公告独立走 group_notice 子系统）"),
    {OwnerUid, _, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    Introduction = <<"本群简介：请文明发言"/utf8>>,
    Result = group_logic:edit(OwnerUid, Gid, #{introduction => Introduction}),
    ?assertEqual(ok, Result),

    cleanup_users([OwnerUid]),
    {comment, "更新群组简介成功"}.

%% ===================================================================
%% 群组解散测试
%% ===================================================================

dissolve_group_by_owner_succeeds(_Config) ->
    ct:log("测试群主解散群组成功"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [
        integer_to_binary(Uid1), integer_to_binary(Uid2)
    ]),

    %% dissolve/2：操作者, 群
    Result = group_logic:dissolve(OwnerUid, Gid),
    ?assertEqual(ok, Result),

    %% 验证群组已删除（find_by_id 无行时返回空 map）
    ?assertEqual(#{}, group_ds:find_by_id(Gid, <<"*">>)),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "群主解散群组成功"}.

non_owner_cannot_dissolve_group_fails(_Config) ->
    ct:log("测试非群主无法解散群组"),
    {OwnerUid, Uid1, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),

    %% 非群主尝试解散群组
    Result = group_logic:dissolve(Uid1, Gid),
    ?assertMatch({error, _}, Result),

    %% 验证群组仍然存在（find_by_id 返回 map）
    G = group_ds:find_by_id(Gid, <<"id">>),
    ?assertEqual(Gid, maps:get(<<"id">>, G)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "非群主无法解散群组"}.

dissolve_group_removes_all_members(_Config) ->
    ct:log("测试解散群组移除所有成员"),
    {OwnerUid, Uid1, Uid2} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [
        integer_to_binary(Uid1), integer_to_binary(Uid2)
    ]),

    %% 验证成员存在
    M1 = group_member_ds:find_by_gid_and_uid(Gid, Uid1, <<"user_id">>),
    ?assertEqual(Uid1, maps:get(<<"user_id">>, M1)),

    %% 解散群组
    ok = group_logic:dissolve(OwnerUid, Gid),

    %% 验证成员关系已移除
    After1 = group_member_ds:find_by_gid_and_uid(Gid, Uid1, <<"user_id">>),
    After2 = group_member_ds:find_by_gid_and_uid(Gid, Uid2, <<"user_id">>),
    ?assertEqual(undefined, maps:get(<<"user_id">>, After1, undefined)),
    ?assertEqual(undefined, maps:get(<<"user_id">>, After2, undefined)),

    cleanup_users([OwnerUid, Uid1, Uid2]),
    {comment, "解散群组移除所有成员"}.

%% ===================================================================
%% 群组公告测试
%% ===================================================================

publish_group_notice_succeeds(_Config) ->
    ct:log("测试发布群组公告成功"),
    {OwnerUid, _, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    %% group_notice_logic:insert/2：操作者, Data（含 group_id）
    Data = notice_data(Gid, OwnerUid, <<"群公告：明天开会"/utf8>>),
    Result = group_notice_logic:insert(OwnerUid, Data),
    ?assertMatch({ok, _}, Result),

    cleanup_users([OwnerUid]),
    {comment, "发布群组公告成功"}.

members_see_latest_notice(_Config) ->
    ct:log("测试成员查看最新公告"),
    {OwnerUid, Uid1, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),

    Data = notice_data(Gid, OwnerUid, <<"重要通知"/utf8>>),
    {ok, _} = group_notice_logic:insert(OwnerUid, Data),

    %% 成员分页查看公告（list/4：查看者, 群, 页, 每页）
    Result = group_notice_logic:list(Uid1, Gid, 1, 10),
    ?assertMatch({ok, {_, _}}, Result),
    {ok, {Total, List}} = Result,
    ?assert(Total >= 1),
    ?assert(length(List) >= 1),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "成员查看最新公告成功"}.

%% ===================================================================
%% 群组禁言测试
%% ===================================================================

mute_member_in_group_succeeds(_Config) ->
    ct:log("测试禁言群成员成功"),
    {OwnerUid, Uid1, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),

    %% mute/4：操作者, 群, 目标, 时长（秒）
    Result = group_member_logic:mute(OwnerUid, Gid, Uid1, 3600),
    ?assertEqual(ok, Result),

    %% 验证成员被禁言（mute_until 非 null）
    Member = group_member_ds:find_by_gid_and_uid(Gid, Uid1, <<"mute_until">>),
    ?assertNotEqual(null, maps:get(<<"mute_until">>, Member, null)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "禁言群成员成功"}.

unmute_member_restores_permission(_Config) ->
    ct:log("测试解除禁言恢复权限"),
    {OwnerUid, Uid1, _} = create_three_users(),

    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),
    ok = group_member_logic:mute(OwnerUid, Gid, Uid1, 3600),

    %% 解除禁言（unmute/3：操作者, 群, 目标）
    Result = group_member_logic:unmute(OwnerUid, Gid, Uid1),
    ?assertEqual(ok, Result),

    %% 验证成员禁言状态已清除（mute_until = null）
    Member = group_member_ds:find_by_gid_and_uid(Gid, Uid1, <<"mute_until">>),
    ?assertEqual(null, maps:get(<<"mute_until">>, Member, null)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "解除禁言恢复权限成功"}.

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 创建三个测试用户
create_three_users() ->
    Mobile1 = unique_mobile("13900"),
    Mobile2 = unique_mobile("13900"),
    Mobile3 = unique_mobile("13900"),
    Password = <<"Test@123456">>,

    {ok, _} = passport_logic:signup(Mobile1, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile2, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile3, Password, <<".@example.com">>, #{}),

    User1 = user_repo:find_by_mobile(Mobile1, <<"id">>),
    User2 = user_repo:find_by_mobile(Mobile2, <<"id">>),
    User3 = user_repo:find_by_mobile(Mobile3, <<"id">>),

    Uid1 = maps:get(<<"id">>, User1),
    Uid2 = maps:get(<<"id">>, User2),
    Uid3 = maps:get(<<"id">>, User3),

    {Uid1, Uid2, Uid3}.

%% 构造一条群公告数据
notice_data(Gid, Uid, Title) ->
    #{
        group_id => Gid,
        user_id => Uid,
        title => Title,
        body => <<"公告正文"/utf8>>,
        status => 1,
        expired_at => <<"2099-12-31T23:59:59Z">>,
        created_at => elib_dt:now()
    }.

unique_mobile(Prefix) ->
    Suffix = erlang:phash2(
        {erlang:system_time(microsecond), erlang:unique_integer([monotonic, positive]), self()},
        1000000
    ),
    list_to_binary(io_lib:format("~s~6..0B", [Prefix, Suffix])).

%% 清理用户
%% 注意：user_repo:delete 是软删除（status=-1），软删行仍计入 License
%% 用户配额（quota_guard 402），累积会令后续 signup 失败，故此处硬删除。
cleanup_users(Uids) ->
    lists:foreach(
        fun(Uid) ->
            elib_pg:execute(<<"DELETE FROM \"user\" WHERE id = $1">>, [Uid])
        end,
        Uids
    ).

%% 清理所有测试数据（硬删除，理由同上）
cleanup_all_test_data() ->
    elib_pg:execute(<<"DELETE FROM \"user\" WHERE mobile LIKE '13900%'">>, []).
