-module(group_ds_member_uids_strict_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc group_ds:member_uids_strict/1 的 fail-closed 语义测试
%%%
%%% 背景：member_uids/1 此前把 group_member_repo 的 {error,_} 折成 []，
%%% 使"DB 抖动"与"空群"不可区分。msg_c2g_logic 拿到 [] 后仍继续投递，
%%% 把空收件人列表写进 staging —— 消息落库、发送方看到成功、但永远
%%% 投递不出去。
%%%
%%% 本测试锁定：strict 版必须把失败原样透出，宽松版可以降级但不得静默。
%%% @end
%%%===================================================================

-define(GID, 7001).

setup() ->
    meck:new(group_member_repo, [no_link, passthrough]),
    meck:new(imboy_cache, [no_link, passthrough]),
    meck:new(elib_metric, [no_link, passthrough]),
    %% 默认缓存未命中，强制走 repo 分支
    meck:expect(imboy_cache, get, fun(_Key) -> undefined end),
    meck:expect(imboy_cache, set, fun(_Key, _Val, _TTL) -> ok end),
    meck:expect(elib_metric, increment, fun(_Name, _N, _Labels) -> ok end),
    ok.

cleanup(_) ->
    meck:unload(elib_metric),
    meck:unload(imboy_cache),
    meck:unload(group_member_repo),
    ok.

member_uids_strict_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun repo_error_is_propagated/0,
        fun repo_error_is_not_confused_with_empty_group/0,
        fun empty_group_returns_ok_empty/0,
        fun empty_group_is_not_cached/0,
        fun normal_group_returns_uids_and_caches/0,
        fun cache_hit_short_circuits_repo/0,
        fun unexpected_repo_shape_is_treated_as_error/0,
        fun lenient_version_degrades_but_reports/0
    ]}.

%% repo 返回 {error,_} 时必须原样透出，不得折成 []
repo_error_is_propagated() ->
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) ->
        {error, connection_closed}
    end),
    ?assertEqual(
        {error, connection_closed},
        group_ds:member_uids_strict(?GID)
    ).

%% 关键不变量：失败与空群的返回值必须可区分
repo_error_is_not_confused_with_empty_group() ->
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) ->
        {error, timeout}
    end),
    Failed = group_ds:member_uids_strict(?GID),
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) -> {ok, []} end),
    Empty = group_ds:member_uids_strict(?GID),
    ?assertNotEqual(Failed, Empty),
    ?assertMatch({error, _}, Failed),
    ?assertEqual({ok, []}, Empty).

empty_group_returns_ok_empty() ->
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) -> {ok, []} end),
    ?assertEqual({ok, []}, group_ds:member_uids_strict(?GID)).

%% 空群不写缓存：避免把"刚建群还没加人"的瞬时空态缓存 1 小时
empty_group_is_not_cached() ->
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) -> {ok, []} end),
    _ = group_ds:member_uids_strict(?GID),
    ?assertEqual(0, length(meck_calls(imboy_cache, set))).

normal_group_returns_uids_and_caches() ->
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) ->
        {ok, [
            #{<<"user_id">> => 11},
            #{<<"user_id">> => 22},
            #{<<"user_id">> => 33}
        ]}
    end),
    ?assertEqual({ok, [11, 22, 33]}, group_ds:member_uids_strict(?GID)),
    ?assertEqual(1, length(meck_calls(imboy_cache, set))).

%% 缓存命中时不应触达 repo
cache_hit_short_circuits_repo() ->
    meck:expect(imboy_cache, get, fun(_Key) -> {ok, [5, 6]} end),
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) ->
        erlang:error(repo_should_not_be_called)
    end),
    ?assertEqual({ok, [5, 6]}, group_ds:member_uids_strict(?GID)).

%% 非 {ok,_}/{error,_} 的第三种形态当作失败，而不是当作空群
unexpected_repo_shape_is_treated_as_error() ->
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) -> undefined end),
    ?assertMatch(
        {error, {unexpected_repo_result, undefined}},
        group_ds:member_uids_strict(?GID)
    ).

%% 宽松版仍返回 []（30 个既有调用点依赖此签名），但必须计数，不得静默
lenient_version_degrades_but_reports() ->
    meck:expect(group_member_repo, list_by_gid, fun(?GID, _Col) ->
        {error, connection_closed}
    end),
    ?assertEqual([], group_ds:member_uids(?GID)),
    ?assertEqual(1, length(meck_calls(elib_metric, increment))).

%% ===================================================================
%% Internal
%% ===================================================================

%% meck:history 条目是 {Pid, {M, F, Args}, Result} 三元组
meck_calls(Mod, Fun) ->
    [H || {_Pid, {M, F, _Args}, _Res} = H <- meck:history(Mod), M =:= Mod, F =:= Fun].
