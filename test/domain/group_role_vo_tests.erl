%%% @doc group_role_vo 值对象 eunit 测试（零 mock）。
%%% 验证角色枚举校验（0-5，权威源 include/group_role.hrl）与 value/equal/name。
-module(group_role_vo_tests).

-include_lib("eunit/include/eunit.hrl").
-include("group_role.hrl").

%% 合法角色（群主）可构造。
new_owner_test() ->
    {ok, R} = group_role_vo:new(?ROLE_OWNER),
    ?assertEqual(?ROLE_OWNER, group_role_vo:value(R)).

%% 边界角色（未定义 0 / 副群主 5）合法。
new_boundary_test() ->
    ?assertMatch({ok, _}, group_role_vo:new(?ROLE_UNDEFINED)),
    ?assertMatch({ok, _}, group_role_vo:new(?ROLE_VICE_OWNER)).

%% 越界角色拒绝。
new_out_of_range_test() ->
    ?assertEqual({error, invalid_group_role}, group_role_vo:new(6)),
    ?assertEqual({error, invalid_group_role}, group_role_vo:new(-1)).

%% 非整数拒绝。
new_non_integer_test() ->
    ?assertEqual({error, invalid_group_role}, group_role_vo:new(<<"owner">>)).

%% 相同角色相等。
equal_test() ->
    {ok, A} = group_role_vo:new(?ROLE_ADMIN),
    {ok, B} = group_role_vo:new(?ROLE_ADMIN),
    {ok, C} = group_role_vo:new(?ROLE_MEMBER),
    ?assert(group_role_vo:equal(A, B)),
    ?assertNot(group_role_vo:equal(A, C)).

%% name/1 返回中文角色名。
name_test() ->
    {ok, R} = group_role_vo:new(?ROLE_OWNER),
    ?assertEqual(<<"群主"/utf8>>, group_role_vo:name(R)).
