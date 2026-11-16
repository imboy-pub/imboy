-module(group_member_join_auth_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc group_member_handler:join/2 的成员身份门（Critical #3）
%%%
%%% 缺陷：join 原本零鉴权——任何持有效 JWT 的用户都能把**任意 uid** 塞进
%%% **任意有容量的群**，不需要自己是该群成员。修复见 commit 4e3a0f87。
%%%
%%% 非成员自行加入仅允许服务端标记为公开且无需审核的群。
%%% 扫码 / 面对面有独立的服务端授权入口，不能以 member_uids=[自己]
%%% 或客户端传入 join_mode 为凭据。群邀请在落成员行的事务内再次校验。
%%%===================================================================

-define(UID, 12345).
-define(GID, 101).

%% IsMember 由 find_by_gid_and_uid 的返回是否为空 map 决定
mocks(MemberUids, MembershipRow) ->
    [
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> ?UID end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"gid">> => ?GID, <<"member_uids">> => MemberUids}
            end}
        ]},
        {throttle, [
            {'check', 2, fun(three_second_once, {group_member, ?UID}) -> ok end}
        ]},
        {group_member_logic, [
            {'build_invite_join_mode', 1, fun(?UID) -> <<"invite_12345_x">> end},
            {'find_by_gid_and_uid', 3, fun(?GID, ?UID, <<"id">>) -> MembershipRow end},
            %% 过了鉴权门才会走到这里；返回 error 让流程在此收口
            {'get_group_capacity', 1, fun(?GID) -> {error, not_found} end},
            {'allows_public_join', 1, fun(?GID) -> false end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, Msg) -> Req#{err => Msg} end}
        ]}
    ].

%% ?WITH_MECKS 返回的是 {setup,...} fixture 而不是值，所以断言必须写在
%% fixture 内部，不能先取回结果再比。
%% join/2 未导出，走公开入口 init/2 派发（不给生产代码开测试后门）
assert_join(MemberUids, MembershipRow, Expected) ->
    ?WITH_MECKS(mocks(MemberUids, MembershipRow), fun() ->
        {ok, Req, _State} = group_member_handler:init(#{}, #{action => join}),
        ?assertEqual(Expected, maps:get(err, Req, no_error))
    end).

not_member() -> #{}.
is_member() -> #{<<"id">> => 999}.

%% ===================================================================
%% 核心安全断言
%% ===================================================================

%% 非成员邀请他人 → 必须拒绝。这正是缺陷本体：
%% 任何持 token 的用户都能把任意 uid 塞进任意有容量的群。
non_member_cannot_invite_others_test_() ->
    assert_join([999], not_member(), <<"你不是群成员"/utf8>>).

%% 群成员邀请他人 → 放行（越过门后撞到"群组不存在"）
member_can_invite_others_test_() ->
    assert_join([999], is_member(), <<"群组不存在"/utf8>>).

%% Knowing a private group's ID is not authorization to join it.
self_join_denied_for_non_member_test_() ->
    assert_join([?UID], not_member(), <<"你不是群成员"/utf8>>).

public_open_self_join_allowed_test_() ->
    ?WITH_MECKS(mocks([?UID], not_member()), fun() ->
        meck:expect(group_member_logic, allows_public_join, fun(?GID) -> true end),
        {ok, Req, _} = group_member_handler:init(#{}, #{action => join}),
        ?assertEqual(<<"群组不存在"/utf8>>, maps:get(err, Req))
    end).

%% 边界：自己 + 他人 不算自加入，非成员仍须拒绝。
%% 少了这条，攻击者只要在 member_uids 里捎带上自己就能绕过整道门。
self_plus_other_is_not_self_join_test_() ->
    assert_join([?UID, 999], not_member(), <<"你不是群成员"/utf8>>).

%% String IDs do not restore the former self-join bypass.
self_join_denied_when_uid_is_binary_test_() ->
    assert_join([<<"12345">>], not_member(), <<"你不是群成员"/utf8>>).

%% ===================================================================
%% 既有参数校验不得被鉴权门顶掉（顺序回归）
%% ===================================================================

empty_member_uids_still_rejected_before_membership_check_test_() ->
    assert_join([], not_member(), <<"member_uids 不能为空"/utf8>>).

non_list_member_uids_still_rejected_test_() ->
    assert_join(<<"999">>, not_member(), <<"member_uids 必须是list"/utf8>>).

%% ===================================================================
%% 成员身份查询必须惰性：限流/参数校验先拦，别让 DB 被放大
%% ===================================================================

%% 早先版本把 IsMember 提到 case 之前求值，导致**被限流拒绝的请求也照打一次
%% DB 查询** —— 限流就挡不住数据库放大了。这里断言限流命中时根本不查库。
membership_query_skipped_when_throttled_test_() ->
    Mocks = lists:keyreplace(
        throttle,
        1,
        mocks([999], not_member()),
        {throttle, [
            {'check', 2, fun(three_second_once, {group_member, ?UID}) ->
                {limit_exceeded, 1, 1}
            end}
        ]}
    ),
    ?WITH_MECKS(Mocks, fun() ->
        {ok, Req, _} = group_member_handler:init(#{}, #{action => join}),
        ?assertEqual(<<"在处理中，请稍后重试"/utf8>>, maps:get(err, Req, no_error)),
        ?assertNot(
            meck:called(group_member_logic, find_by_gid_and_uid, ['_', '_', '_']),
            "限流命中时不得查询成员身份"
        )
    end).

%% 参数非法时同样不该查库（gid 格式错 / member_uids 为空都在成员门之前）
membership_query_skipped_on_invalid_params_test_() ->
    ?WITH_MECKS(mocks([], not_member()), fun() ->
        {ok, Req, _} = group_member_handler:init(#{}, #{action => join}),
        ?assertEqual(<<"member_uids 不能为空"/utf8>>, maps:get(err, Req, no_error)),
        ?assertNot(
            meck:called(group_member_logic, find_by_gid_and_uid, ['_', '_', '_']),
            "参数校验失败时不得查询成员身份"
        )
    end).

%% Self-join must pass the same membership/policy authorization boundary.
membership_and_policy_checked_on_self_join_test_() ->
    ?WITH_MECKS(mocks([?UID], not_member()), fun() ->
        {ok, _Req, _} = group_member_handler:init(#{}, #{action => join}),
        ?assert(meck:called(group_member_logic, find_by_gid_and_uid, ['_', '_', '_'])),
        ?assert(meck:called(group_member_logic, allows_public_join, [?GID]))
    end).
