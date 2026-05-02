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
-include("group_role.hrl").

%% 测试夹具
group_member_role_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
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
            };
        {error, _Reason} ->
            {"Database not available",
             fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    {ok, Owner} = create_test_user(<<"owner_role">>),
    {ok, Admin1} = create_test_user(<<"admin1_role">>),
    {ok, Admin2} = create_test_user(<<"admin2_role">>),
    {ok, Member1} = create_test_user(<<"member1_role">>),
    {ok, Member2} = create_test_user(<<"member2_role">>),
    {ok, Group} = create_test_group(Owner, <<"role_test_group">>),
    ok = group_member_ds:add_member(Group, Admin1),
    ok = group_member_ds:add_member(Group, Admin2),
    ok = group_member_ds:add_member(Group, Member1),
    ok = group_member_ds:add_member(Group, Member2),
    Context = #{
        owner => Owner,
        admin1 => Admin1,
        admin2 => Admin2,
        member1 => Member1,
        member2 => Member2,
        group => Group
    },
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_set_admin() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member1 = maps:get(member1, Context),
    Group = maps:get(group, Context),

    ok = group_member_logic:update_role(Owner, Group, Member1, ?ROLE_ADMIN),

    {ok, MemberInfo} = group_member_ds:get_member_info(Group, Member1, <<"role">>),
    ?assertEqual(?ROLE_ADMIN, maps:get(<<"role">>, MemberInfo)),
    true = group_member_logic:has_admin_permission(Group, Member1),

    ok.

test_remove_admin() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin1 = maps:get(admin1, Context),
    Group = maps:get(group, Context),

    ok = group_member_logic:update_role(Owner, Group, Admin1, ?ROLE_ADMIN),
    ok = group_member_logic:update_role(Owner, Group, Admin1, ?ROLE_MEMBER),

    {ok, MemberInfo} = group_member_ds:get_member_info(Group, Admin1, <<"role">>),
    ?assertEqual(?ROLE_MEMBER, maps:get(<<"role">>, MemberInfo)),
    false = group_member_logic:has_admin_permission(Group, Admin1),

    ok.

test_transfer_owner() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin1 = maps:get(admin1, Context),
    Group = maps:get(group, Context),

    ok = group_logic:transfer(Owner, Group, Admin1, true),

    {ok, GroupInfo} = group_repo:find_by_gid(Group),
    ?assertEqual(Admin1, maps:get(<<"owner_uid">>, GroupInfo)),
    {ok, NewOwnerInfo} = group_member_ds:get_member_info(Group, Admin1, <<"role">>),
    ?assertEqual(?ROLE_OWNER, maps:get(<<"role">>, NewOwnerInfo)),
    {ok, OldOwnerInfo} = group_member_ds:get_member_info(Group, Owner, <<"role">>),
    ?assertEqual(?ROLE_VICE_OWNER, maps:get(<<"role">>, OldOwnerInfo)),
    true = group_member_logic:is_owner_or_vice_owner(Group, Owner),

    ok.

test_remove_member() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member2 = maps:get(member2, Context),
    Group = maps:get(group, Context),

    ok = group_member_logic:leave(Member2, Group, Owner),

    ?assertEqual(#{}, group_member_repo:find(Group, Member2, <<"id">>)),

    ok.

test_mute_member() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member1 = maps:get(member1, Context),
    Group = maps:get(group, Context),

    ok = group_member_logic:mute(Owner, Group, Member1, 3600),

    true = group_member_logic:check_mute(Group, Member1),
    {ok, MemberInfo} = group_member_ds:get_member_info(Group, Member1, <<"mute_until">>),
    ?assert(maps:get(<<"mute_until">>, MemberInfo) =/= null),

    ok.

test_unmute_member() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Member1 = maps:get(member1, Context),
    Group = maps:get(group, Context),

    ok = group_member_logic:mute(Owner, Group, Member1, 3600),
    ok = group_member_ds:update_mute(undefined, Group, Member1, null),

    false = group_member_logic:check_mute(Group, Member1),

    ok.

test_admin_permission() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin1 = maps:get(admin1, Context),
    Group = maps:get(group, Context),

    ok = group_member_logic:update_role(Owner, Group, Admin1, ?ROLE_ADMIN),

    true = group_member_logic:has_admin_permission(Group, Admin1),
    false = group_member_logic:has_senior_admin_permission(Group, Admin1),
    false = group_member_logic:is_owner_or_vice_owner(Group, Admin1),

    ok.

test_member_permission() ->
    Context = get_context(),
    Member1 = maps:get(member1, Context),
    Member2 = maps:get(member2, Context),
    Group = maps:get(group, Context),

    Result = group_member_logic:update_role(Member1, Group, Member2, ?ROLE_ADMIN),

    ?assertMatch({error, _}, Result),
    false = group_member_logic:has_admin_permission(Group, Member1),

    ok.

test_batch_set_admin() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Admin1 = maps:get(admin1, Context),
    Admin2 = maps:get(admin2, Context),
    Group = maps:get(group, Context),

    ok = group_member_logic:update_role(Owner, Group, Admin1, ?ROLE_ADMIN),
    ok = group_member_logic:update_role(Owner, Group, Admin2, ?ROLE_ADMIN),

    {ok, Members} = group_member_repo:list_by_gid(Group, <<"user_id, role">>),
    AdminMembers = [Member || Member <- Members,
                              lists:member(maps:get(<<"user_id">>, Member, 0),
                                           [Owner, Admin1, Admin2]),
                              lists:member(maps:get(<<"role">>, Member, ?ROLE_MEMBER),
                                           [?ROLE_ADMIN, ?ROLE_OWNER, ?ROLE_VICE_OWNER])],
    ?assertEqual(3, length(AdminMembers)),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}).

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
    ok = group_member_ds:update_role(undefined, Gid, OwnerId, ?ROLE_OWNER),
    {ok, Gid}.
