-module(user_dnd_rule_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_dnd_rule_ds 模块的 EUnit 测试
%%%
%%% 目标：验证免打扰领域服务的缓存读写与时段判断
%%% 覆盖：find_by_uid(缓存命中/未命中)、save、delete、is_dnd_at(同日/跨午夜/停用/起止相等)
%%%===================================================================

%% ===================================================================
%% find_by_uid/1 缓存
%% ===================================================================

find_by_uid_cache_hit_skips_repo_test_() ->
    Rule = #{<<"user_id">> => 1, <<"status">> => 1},
    ?WITH_MECK(
        imboy_cache,
        [
            {'get', 1, fun({user_dnd_rule, 1}) -> {ok, Rule} end}
        ],
        fun() ->
            %% 命中缓存：不应触及 repo（repo 未 mock，若被调用将 undef 报错）
            ?assertEqual(Rule, user_dnd_rule_ds:find_by_uid(1))
        end
    ).

find_by_uid_cache_miss_loads_and_sets_test_() ->
    Rule = #{<<"user_id">> => 2, <<"status">> => 1},
    ?WITH_MECKS(
        [
            {imboy_cache, [
                {'get', 1, fun(_Key) -> undefined end},
                {'set', 3, fun(_K, _V, _Ttl) -> ok end}
            ]},
            {user_dnd_rule_repo, [{'find_by_uid', 1, fun(2) -> Rule end}]}
        ],
        fun() ->
            ?assertEqual(Rule, user_dnd_rule_ds:find_by_uid(2))
        end
    ).

%% ===================================================================
%% save/1 与 delete/1：写后失效缓存
%% ===================================================================

save_upserts_and_invalidates_cache_test_() ->
    ?WITH_MECKS(
        [
            {user_dnd_rule_repo, [{'upsert', 1, fun(#{<<"user_id">> := 1}) -> ok end}]},
            {imboy_cache, [{'delete', 1, fun({user_dnd_rule, 1}) -> ok end}]}
        ],
        fun() ->
            Data = #{
                <<"user_id">> => 1, <<"start_min">> => 0, <<"end_min">> => 0, <<"status">> => 1
            },
            ?assertEqual(ok, user_dnd_rule_ds:save(Data))
        end
    ).

delete_removes_and_invalidates_cache_test_() ->
    ?WITH_MECKS(
        [
            {user_dnd_rule_repo, [{'delete_by_uid', 1, fun(1) -> ok end}]},
            {imboy_cache, [{'delete', 1, fun({user_dnd_rule, 1}) -> ok end}]}
        ],
        fun() ->
            ?assertEqual(ok, user_dnd_rule_ds:delete(1))
        end
    ).

%% ===================================================================
%% is_dnd_at/2 时段判断（经缓存喂入规则）
%% ===================================================================

%% 同日区间 [480, 1320) 即 08:00-22:00
is_dnd_at_same_day_inside_test_() ->
    with_rule(#{<<"status">> => 1, <<"start_min">> => 480, <<"end_min">> => 1320}, fun() ->
        % 10:00 在区间内
        ?assert(user_dnd_rule_ds:is_dnd_at(1, 600))
    end).

is_dnd_at_same_day_outside_test_() ->
    with_rule(#{<<"status">> => 1, <<"start_min">> => 480, <<"end_min">> => 1320}, fun() ->
        % 23:00 区间外
        ?assertNot(user_dnd_rule_ds:is_dnd_at(1, 1380))
    end).

%% 跨午夜区间 [1320, 480) 即 22:00-次日08:00
is_dnd_at_overnight_inside_test_() ->
    with_rule(#{<<"status">> => 1, <<"start_min">> => 1320, <<"end_min">> => 480}, fun() ->
        % 23:00 命中
        ?assert(user_dnd_rule_ds:is_dnd_at(1, 1380)),
        % 01:00 命中
        ?assert(user_dnd_rule_ds:is_dnd_at(1, 60))
    end).

is_dnd_at_overnight_outside_test_() ->
    with_rule(#{<<"status">> => 1, <<"start_min">> => 1320, <<"end_min">> => 480}, fun() ->
        % 10:00 不命中
        ?assertNot(user_dnd_rule_ds:is_dnd_at(1, 600))
    end).

%% 规则停用 status=0
is_dnd_at_disabled_returns_false_test_() ->
    with_rule(#{<<"status">> => 0, <<"start_min">> => 480, <<"end_min">> => 1320}, fun() ->
        ?assertNot(user_dnd_rule_ds:is_dnd_at(1, 600))
    end).

%% 起止相等视为未设置
is_dnd_at_equal_bounds_returns_false_test_() ->
    with_rule(#{<<"status">> => 1, <<"start_min">> => 0, <<"end_min">> => 0}, fun() ->
        ?assertNot(user_dnd_rule_ds:is_dnd_at(1, 600))
    end).

%% 无规则（空 map）→ false
is_dnd_at_no_rule_returns_false_test_() ->
    with_rule(#{}, fun() ->
        ?assertNot(user_dnd_rule_ds:is_dnd_at(1, 600))
    end).

%% ===================================================================
%% 内部辅助：用缓存命中喂入指定规则
%% ===================================================================
with_rule(Rule, TestFun) ->
    ?WITH_MECK(
        imboy_cache,
        [
            {'get', 1, fun(_Key) -> {ok, Rule} end}
        ],
        TestFun
    ).
