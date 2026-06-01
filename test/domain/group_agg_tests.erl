%%% @doc group_agg 群组聚合根 eunit 测试（零 mock，纯函数）。
%%% 验证成员上限（<=500）、成员增减（count>=0）、转让（仅 owner）、
%%% 解散（终态幂等）四类不变量及其领域事件产出。
%%% SOURCE 语义：group_member_logic 角色权限矩阵 + include/group_role.hrl。
-module(group_agg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("group_role.hrl").

%% 测试夹具：构造一份活跃群（id=g1，owner=u1，成员数 N）。
fixture(N) ->
    group_agg:rehydrate(#{
        <<"group_id">> => <<"g1">>,
        <<"owner">> => <<"u1">>,
        <<"member_count">> => N,
        <<"type">> => 1
    }).

%% ---- 构造 / 访问器 ----

%% rehydrate 后状态正确，活跃态。
rehydrate_test() ->
    G = fixture(3),
    ?assertEqual(<<"g1">>, group_agg:id(G)),
    ?assertEqual(<<"u1">>, group_agg:owner(G)),
    ?assertEqual(3, group_agg:member_count(G)),
    ?assertEqual(active, group_agg:status(G)).

%% rehydrate 缺省 member_count 归零、脏负值规整为 0。
rehydrate_clamp_test() ->
    G = group_agg:rehydrate(#{
        <<"group_id">> => <<"g1">>,
        <<"owner">> => <<"u1">>,
        <<"member_count">> => -5,
        <<"type">> => 1
    }),
    ?assertEqual(0, group_agg:member_count(G)).

%% ---- 成员上限不变量 ----

%% add_member 正常：member_count +1，产出 member_added 事件。
add_member_test() ->
    G = fixture(3),
    {ok, G1, Events} = group_agg:add_member(G, <<"u2">>),
    ?assertEqual(4, group_agg:member_count(G1)),
    ?assertEqual([{member_added, <<"g1">>, <<"u2">>}], Events).

%% add_member 触顶（499→500 成功，500→拒绝）。
add_member_limit_test() ->
    G499 = fixture(499),
    {ok, G500, _} = group_agg:add_member(G499, <<"u2">>),
    ?assertEqual(500, group_agg:member_count(G500)),
    ?assertEqual(
        {error, member_limit_reached},
        group_agg:add_member(G500, <<"u3">>)
    ).

%% add_member 已解散群拒绝。
add_member_dissolved_test() ->
    {ok, GD, _} = group_agg:dissolve(fixture(3)),
    ?assertEqual({error, group_dissolved}, group_agg:add_member(GD, <<"u2">>)).

%% ---- 成员移除不变量 ----

%% remove_member 正常：member_count -1，产出 member_removed 事件。
remove_member_test() ->
    G = fixture(3),
    {ok, G1, Events} = group_agg:remove_member(G, <<"u2">>),
    ?assertEqual(2, group_agg:member_count(G1)),
    ?assertEqual([{member_removed, <<"g1">>, <<"u2">>}], Events).

%% remove_member 下限：member_count 不为负，空群移除幂等无事件。
remove_member_floor_test() ->
    G0 = fixture(0),
    {ok, G1, Events} = group_agg:remove_member(G0, <<"u2">>),
    ?assertEqual(0, group_agg:member_count(G1)),
    ?assertEqual([], Events).

%% remove_member 已解散群拒绝。
remove_member_dissolved_test() ->
    {ok, GD, _} = group_agg:dissolve(fixture(3)),
    ?assertEqual({error, group_dissolved}, group_agg:remove_member(GD, <<"u2">>)).

%% ---- 转让不变量（仅 owner，group_role_vo 接入）----

%% transfer_owner 正常：owner 发起，owner 字段变更，产出 owner_transferred 事件。
transfer_owner_test() ->
    G = fixture(3),
    {ok, G1, Events} = group_agg:transfer_owner(G, {<<"u1">>, <<"u2">>}),
    ?assertEqual(<<"u2">>, group_agg:owner(G1)),
    ?assertEqual([{owner_transferred, <<"g1">>, <<"u1">>, <<"u2">>}], Events).

%% transfer_owner 非 owner 发起 → not_owner（角色 VO 判定）。
transfer_owner_not_owner_test() ->
    G = fixture(3),
    ?assertEqual(
        {error, not_owner},
        group_agg:transfer_owner(G, {<<"u9">>, <<"u2">>})
    ).

%% transfer_owner 已解散群拒绝。
transfer_owner_dissolved_test() ->
    {ok, GD, _} = group_agg:dissolve(fixture(3)),
    ?assertEqual(
        {error, group_dissolved},
        group_agg:transfer_owner(GD, {<<"u1">>, <<"u2">>})
    ).

%% ---- 解散不变量（终态幂等）----

%% dissolve：status 变 dissolved，产出 group_dissolved 事件。
dissolve_test() ->
    G = fixture(3),
    {ok, G1, Events} = group_agg:dissolve(G),
    ?assertEqual(dissolved, group_agg:status(G1)),
    ?assertEqual([{group_dissolved, <<"g1">>}], Events).

%% dissolve 幂等：已解散再 dissolve 无事件。
dissolve_idempotent_test() ->
    {ok, G1, _} = group_agg:dissolve(fixture(3)),
    {ok, G2, Events} = group_agg:dissolve(G1),
    ?assertEqual(dissolved, group_agg:status(G2)),
    ?assertEqual([], Events).

%% ---- group_role_vo 接入 ----

%% role_of：owner 返回 ROLE_OWNER VO，普通成员返回 ROLE_MEMBER VO。
role_of_test() ->
    G = fixture(3),
    ?assertEqual(?ROLE_OWNER, group_role_vo:value(group_agg:role_of(G, <<"u1">>))),
    ?assertEqual(?ROLE_MEMBER, group_role_vo:value(group_agg:role_of(G, <<"u9">>))).
