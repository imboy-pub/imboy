-module(verification_code_consume_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc verification_code_ds:consume/2 一次性消费语义测试
%%%
%%% 背景：verify_code/2 只读不写 —— 验证成功后码仍然有效，直到 10 分钟
%%% 有效期自然过期。叠加"有效期内重复请求会重发同一个码"，同一个 6 位码
%%% 可稳定存活 10 分钟，给穷举 000000-999999 留出足够窗口。
%%%
%%% 本测试锁定：验证成功必须立即失效；验证失败不得动数据。
%%% @end
%%%===================================================================

-define(ID, <<"+8613900001111">>).
-define(CODE, <<"123456">>).

setup() ->
    meck:new(verification_code_repo, [no_link, passthrough]),
    meck:new(elib_dt, [no_link, passthrough]),
    meck:new(imboy_env, [no_link, passthrough]),
    %% 固定"现在"，避免依赖真实时钟
    meck:expect(elib_dt, now, fun() -> <<"2026-07-31T00:00:00Z">> end),
    %% 生产环境语义：万能码通道关闭
    meck:expect(imboy_env, current, fun() -> <<"pro">> end),
    meck:expect(verification_code_repo, save, fun(_Id, _Code, _V, _C) -> {ok, 1} end),
    ok.

cleanup(_) ->
    meck:unload(imboy_env),
    meck:unload(elib_dt),
    meck:unload(verification_code_repo),
    ok.

%% 有效期在"现在"之后 => 码有效
mock_valid_code() ->
    meck:expect(verification_code_repo, find_by_id, fun(?ID) ->
        #{<<"code">> => ?CODE, <<"validity_at">> => <<"2026-07-31T00:10:00Z">>}
    end).

saves(Mod) ->
    [H || {_Pid, {M, F, _A}, _R} = H <- meck:history(Mod), M =:= Mod, F =:= save].

consume_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun valid_code_is_accepted/0,
        fun valid_code_is_invalidated_after_use/0,
        fun invalidation_writes_expired_validity/0,
        fun wrong_code_is_rejected_and_writes_nothing/0,
        fun expired_code_is_rejected_and_writes_nothing/0
    ]}.

valid_code_is_accepted() ->
    mock_valid_code(),
    ?assertMatch({ok, _}, verification_code_ds:consume(?ID, ?CODE)).

%% 核心不变量：消费成功后必须发生一次失效写入
valid_code_is_invalidated_after_use() ->
    mock_valid_code(),
    ?assertMatch({ok, _}, verification_code_ds:consume(?ID, ?CODE)),
    ?assertEqual(1, length(saves(verification_code_repo))).

%% 失效方式：validity_at 改写为当前时刻（Now < ValidityAt 立即为假）+ 清空 code
invalidation_writes_expired_validity() ->
    mock_valid_code(),
    _ = verification_code_ds:consume(?ID, ?CODE),
    [{_Pid, {_M, save, [Id, Code, ValidityAt, _CreatedAt]}, _R} | _] =
        saves(verification_code_repo),
    ?assertEqual(?ID, Id),
    ?assertEqual(<<>>, Code),
    ?assertEqual(<<"2026-07-31T00:00:00Z">>, ValidityAt),
    %% Now < ValidityAt 为假 => 后续 verify 必失败
    ?assertNot(<<"2026-07-31T00:00:00Z">> < ValidityAt).

%% 猜错的码不得触发任何写入（否则等于给攻击者一个清除/覆写通道）
wrong_code_is_rejected_and_writes_nothing() ->
    mock_valid_code(),
    ?assertMatch({error, _}, verification_code_ds:consume(?ID, <<"999999">>)),
    ?assertEqual(0, length(saves(verification_code_repo))).

expired_code_is_rejected_and_writes_nothing() ->
    meck:expect(verification_code_repo, find_by_id, fun(?ID) ->
        #{<<"code">> => ?CODE, <<"validity_at">> => <<"2026-07-30T23:00:00Z">>}
    end),
    ?assertMatch({error, _}, verification_code_ds:consume(?ID, ?CODE)),
    ?assertEqual(0, length(saves(verification_code_repo))).
