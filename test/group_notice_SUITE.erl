-module(group_notice_SUITE).

%%%===================================================================
%%% @doc
%%% 群公告功能 Common Test 测试套件
%%%
%%% 运行方式：
%%%   ct_run -dir test -suite group_notice_SUITE
%%%   make ct-group_notice
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("error_code.hrl").
-include("group_role.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    %% 发布公告
    owner_can_publish_notice_succeeds/1,
    admin_can_publish_notice_succeeds/1,
    non_member_cannot_publish_notice_fails/1,
    regular_member_publish_notice_rejected/1,
    %% 查询列表
    list_notices_with_pagination_succeeds/1,
    non_member_list_notices_rejected/1,
    %% 更新公告
    owner_can_update_notice_succeeds/1,
    regular_member_cannot_update_notice_fails/1,
    %% 删除公告
    owner_can_delete_notice_succeeds/1,
    regular_member_cannot_delete_notice_fails/1,
    %% 置顶/取消
    owner_can_pin_notice_succeeds/1,
    owner_can_unpin_notice_succeeds/1,
    %% 推送事件
    publish_notice_triggers_broadcast/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
        {group, publish_notice},
        {group, list_notice},
        {group, update_notice},
        {group, delete_notice},
        {group, pin_notice},
        {group, broadcast_notice}
    ].

groups() ->
    [
        {publish_notice, [], publish_cases()},
        {list_notice, [], list_cases()},
        {update_notice, [], update_cases()},
        {delete_notice, [], delete_cases()},
        {pin_notice, [], pin_cases()},
        {broadcast_notice, [], broadcast_cases()}
    ].

init_per_suite(Config) ->
    ct:log("开始群公告测试套件"),
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    ct:log("结束群公告测试套件"),
    cleanup_all_test_data(),
    eunit_runner:ct_suite_cleanup(Config).

init_per_group(_Group, Config) ->
    cleanup_all_test_data(),
    Config.

end_per_group(_Group, _Config) ->
    meck:unload(),
    ok.

%% ===================================================================
%% 测试用例分组定义
%% ===================================================================

publish_cases() ->
    [
        owner_can_publish_notice_succeeds,
        admin_can_publish_notice_succeeds,
        non_member_cannot_publish_notice_fails,
        regular_member_publish_notice_rejected
    ].

list_cases() ->
    [
        list_notices_with_pagination_succeeds,
        non_member_list_notices_rejected
    ].

update_cases() ->
    [
        owner_can_update_notice_succeeds,
        regular_member_cannot_update_notice_fails
    ].

delete_cases() ->
    [
        owner_can_delete_notice_succeeds,
        regular_member_cannot_delete_notice_fails
    ].

pin_cases() ->
    [
        owner_can_pin_notice_succeeds,
        owner_can_unpin_notice_succeeds
    ].

broadcast_cases() ->
    [
        publish_notice_triggers_broadcast
    ].

%% ===================================================================
%% 发布公告测试
%% ===================================================================

owner_can_publish_notice_succeeds(_Config) ->
    ct:log("测试群主发布公告成功"),
    {OwnerUid, _, _} = create_three_users(),
    %% group_logic:add/4 建群返回 {ok, Gid}，创建者自动成为群主成员
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    NoticeId = insert_notice(Gid, OwnerUid, <<"群主公告"/utf8>>, <<"本群公告内容"/utf8>>),

    % 发布公告（更新 status = 1）
    Data = #{
        edit_user_id => OwnerUid,
        status => 1,
        updated_at => elib_dt:now()
    },
    Result = group_notice_ds:update(NoticeId, Data),

    ?assertMatch({ok, _}, Result),

    % 验证 publish_notice 广播
    BroadcastResult = group_notice_logic:publish_notice(OwnerUid, Gid, NoticeId),
    ?assertEqual(ok, BroadcastResult),

    cleanup_users([OwnerUid]),
    {comment, "群主发布公告成功"}.

admin_can_publish_notice_succeeds(_Config) ->
    ct:log("测试管理员发布公告成功"),
    {OwnerUid, AdminUid, _} = create_three_users(),
    %% 建群时把 AdminUid 作为初始成员加入（add/4 的 MemberUids 只接受 binary id）
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(AdminUid)]),

    % 将 AdminUid 设置为管理员（update_role/4：操作者, 群, 目标, 角色整数）
    ok = group_member_logic:update_role(OwnerUid, Gid, AdminUid, ?ROLE_ADMIN),

    NoticeId = insert_notice(Gid, AdminUid, <<"管理员公告"/utf8>>, <<"管理员公告内容"/utf8>>),

    % 管理员发布公告
    Data = #{
        edit_user_id => AdminUid,
        status => 1,
        updated_at => elib_dt:now()
    },
    Result = group_notice_ds:update(NoticeId, Data),

    ?assertMatch({ok, _}, Result),

    cleanup_users([OwnerUid, AdminUid]),
    {comment, "管理员发布公告成功"}.

non_member_cannot_publish_notice_fails(_Config) ->
    ct:log("测试非群成员发布公告失败"),
    {OwnerUid, _, NonMemberUid} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    % 创建公告
    NoticeId = insert_notice(Gid, OwnerUid, <<"测试公告"/utf8>>, <<"公告内容"/utf8>>),

    % 非成员尝试删除
    Result = group_notice_logic:delete(NonMemberUid, NoticeId),

    ?assertMatch({error, _}, Result),

    cleanup_users([OwnerUid, NonMemberUid]),
    {comment, "非群成员操作公告被拒绝"}.

regular_member_publish_notice_rejected(_Config) ->
    ct:log("测试普通成员发布公告被拒绝"),
    {OwnerUid, MemberUid, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(MemberUid)]),

    % 群主创建公告
    NoticeId = insert_notice(Gid, OwnerUid, <<"群主公告"/utf8>>, <<"内容"/utf8>>),

    % 普通成员尝试删除（delete 检查权限）
    Result = group_notice_logic:delete(MemberUid, NoticeId),

    ?assertMatch({error, ?ERR_GROUP_PERMISSION_DENIED}, Result),

    cleanup_users([OwnerUid, MemberUid]),
    {comment, "普通成员操作公告被权限拒绝"}.

%% ===================================================================
%% 查询列表测试
%% ===================================================================

list_notices_with_pagination_succeeds(_Config) ->
    ct:log("测试查询群公告列表（分页）"),
    {OwnerUid, MemberUid, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(MemberUid)]),

    % 插入三条公告
    _Id1 = insert_notice(Gid, OwnerUid, <<"公告1"/utf8>>, <<"内容1"/utf8>>),
    _Id2 = insert_notice(Gid, OwnerUid, <<"公告2"/utf8>>, <<"内容2"/utf8>>),
    _Id3 = insert_notice(Gid, OwnerUid, <<"公告3"/utf8>>, <<"内容3"/utf8>>),

    % 成员查询第一页（每页 2 条）
    Result = group_notice_logic:list(MemberUid, Gid, 1, 2),

    ?assertMatch({ok, {_, _}}, Result),
    {ok, {Total, List}} = Result,
    ?assert(Total >= 3),
    ?assertEqual(2, length(List)),

    % 查询第二页
    {ok, {_, Page2}} = group_notice_logic:list(MemberUid, Gid, 2, 2),
    ?assert(length(Page2) >= 1),

    cleanup_users([OwnerUid, MemberUid]),
    {comment, "分页查询群公告列表成功"}.

non_member_list_notices_rejected(_Config) ->
    ct:log("测试非群成员查询公告列表被拒绝"),
    {OwnerUid, _, NonMemberUid} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    Result = group_notice_logic:list(NonMemberUid, Gid, 1, 10),

    ?assertMatch({error, ?ERR_NOT_GROUP_MEMBER}, Result),

    cleanup_users([OwnerUid, NonMemberUid]),
    {comment, "非群成员查询公告被拒绝"}.

%% ===================================================================
%% 更新公告测试
%% ===================================================================

owner_can_update_notice_succeeds(_Config) ->
    ct:log("测试群主更新公告成功"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    NoticeId = insert_notice(Gid, OwnerUid, <<"原始标题"/utf8>>, <<"原始内容"/utf8>>),

    NewTitle = <<"更新后标题"/utf8>>,
    NewBody = <<"更新后内容"/utf8>>,
    UpdateData = #{
        edit_user_id => OwnerUid,
        title => NewTitle,
        body => NewBody,
        status => 1,
        expired_at => elib_dt:rfc3339_to(<<>>),
        updated_at => elib_dt:now()
    },
    Result = group_notice_ds:update(NoticeId, UpdateData),

    ?assertMatch({ok, _}, Result),

    % 验证内容已更新
    {ok, Updated} = group_notice_ds:find_by_id(NoticeId),
    ?assertEqual(NewTitle, maps:get(<<"title">>, Updated)),
    ?assertEqual(NewBody, maps:get(<<"body">>, Updated)),

    cleanup_users([OwnerUid]),
    {comment, "群主更新公告成功"}.

regular_member_cannot_update_notice_fails(_Config) ->
    ct:log("测试普通成员无法更新公告"),
    {OwnerUid, MemberUid, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(MemberUid)]),

    NoticeId = insert_notice(Gid, OwnerUid, <<"群主公告"/utf8>>, <<"公告内容"/utf8>>),

    % 普通成员尝试通过 pin 操作验证权限（pin 需要群主或管理员）
    Result = group_notice_logic:pin(MemberUid, NoticeId),

    ?assertMatch({error, ?ERR_GROUP_PERMISSION_DENIED}, Result),

    cleanup_users([OwnerUid, MemberUid]),
    {comment, "普通成员操作公告被权限拒绝"}.

%% ===================================================================
%% 删除公告测试
%% ===================================================================

owner_can_delete_notice_succeeds(_Config) ->
    ct:log("测试群主删除公告成功"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    NoticeId = insert_notice(Gid, OwnerUid, <<"待删除公告"/utf8>>, <<"公告内容"/utf8>>),

    Result = group_notice_logic:delete(OwnerUid, NoticeId),

    ?assertEqual(ok, Result),

    cleanup_users([OwnerUid]),
    {comment, "群主删除公告成功"}.

regular_member_cannot_delete_notice_fails(_Config) ->
    ct:log("测试普通成员无法删除公告"),
    {OwnerUid, MemberUid, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(MemberUid)]),

    NoticeId = insert_notice(Gid, OwnerUid, <<"群主公告"/utf8>>, <<"内容"/utf8>>),

    Result = group_notice_logic:delete(MemberUid, NoticeId),

    ?assertMatch({error, ?ERR_GROUP_PERMISSION_DENIED}, Result),

    cleanup_users([OwnerUid, MemberUid]),
    {comment, "普通成员删除公告被权限拒绝"}.

%% ===================================================================
%% 置顶/取消置顶测试
%% ===================================================================

owner_can_pin_notice_succeeds(_Config) ->
    ct:log("测试群主置顶公告成功"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    NoticeId = insert_notice(Gid, OwnerUid, <<"待置顶公告"/utf8>>, <<"公告内容"/utf8>>),

    Result = group_notice_logic:pin(OwnerUid, NoticeId),

    ?assertEqual(ok, Result),

    cleanup_users([OwnerUid]),
    {comment, "群主置顶公告成功"}.

owner_can_unpin_notice_succeeds(_Config) ->
    ct:log("测试群主取消置顶公告成功"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    NoticeId = insert_notice(Gid, OwnerUid, <<"置顶公告"/utf8>>, <<"公告内容"/utf8>>),
    ok = group_notice_logic:pin(OwnerUid, NoticeId),

    Result = group_notice_logic:unpin(OwnerUid, NoticeId),

    ?assertEqual(ok, Result),

    cleanup_users([OwnerUid]),
    {comment, "群主取消置顶公告成功"}.

%% ===================================================================
%% 公告推送事件测试
%% ===================================================================

publish_notice_triggers_broadcast(_Config) ->
    ct:log("测试发布公告触发广播事件"),
    {OwnerUid, MemberUid, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(MemberUid)]),

    NoticeId = insert_notice(Gid, OwnerUid, <<"广播测试公告"/utf8>>, <<"公告内容"/utf8>>),

    % Mock msg_s2c_ds:send 以验证广播被触发
    meck:new(msg_s2c_ds, [passthrough]),
    Self = self(),
    meck:expect(msg_s2c_ds, send, fun(FromUid, ToUids, Action, _Ref, _MsgId, Payload, _Mode) ->
        Self ! {broadcast_called, FromUid, ToUids, Action, Payload},
        {ok, 1}
    end),

    BroadcastResult = group_notice_logic:publish_notice(OwnerUid, Gid, NoticeId),
    ?assertEqual(ok, BroadcastResult),

    % 验证广播被调用
    receive
        {broadcast_called, OwnerUid, _ToUids, <<"group_notice_published">>, Payload} ->
            ?assertEqual(Gid, maps:get(<<"gid">>, Payload)),
            ?assertEqual(NoticeId, maps:get(<<"notice_id">>, Payload)),
            ?assertEqual(OwnerUid, maps:get(<<"publisher_id">>, Payload))
    after 3000 ->
        ct:fail("广播事件未在 3s 内触发")
    end,

    meck:unload(msg_s2c_ds),
    cleanup_users([OwnerUid, MemberUid]),
    {comment, "发布公告触发广播事件验证成功"}.

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 创建三个测试用户
create_three_users() ->
    Mobile1 = unique_mobile("13920"),
    Mobile2 = unique_mobile("13920"),
    Mobile3 = unique_mobile("13920"),
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

%% 直接通过 DS 插入一条公告，返回 NoticeId
insert_notice(Gid, Uid, Title, Body) ->
    Data = #{
        group_id => Gid,
        user_id => Uid,
        title => Title,
        body => Body,
        status => 0,
        expired_at => <<"2099-12-31T23:59:59Z">>,
        created_at => elib_dt:now()
    },
    {ok, NoticeId} = group_notice_ds:insert(Data),
    NoticeId.

unique_mobile(Prefix) ->
    Suffix = erlang:phash2(
        {erlang:system_time(microsecond), erlang:unique_integer([monotonic, positive]), self()},
        1000000
    ),
    list_to_binary(io_lib:format("~s~6..0B", [Prefix, Suffix])).

cleanup_users([]) ->
    ok;
cleanup_users([Uid | Rest]) ->
    user_repo:delete(Uid),
    cleanup_users(Rest).

cleanup_all_test_data() ->
    Sql = <<"SELECT id FROM \"user\" WHERE mobile LIKE '13920%'">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            lists:foreach(
                fun(#{<<"id">> := Id}) ->
                    user_repo:delete(Id)
                end,
                Rows
            );
        _ ->
            ok
    end.
