-module(manual_pending_user_setting_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W09.a（channel-group-ux-v2.2 §6.7/J17.c）：user_setting 承载的按会话
%%% 人工待处理元数据（真实 PostgreSQL 向量）：
%%% - 显式目标 true/false；无变化同目标重试返回当前状态不推进版本；
%%% - 目标变化推进该会话版本；多 uk3 相互隔离；
%%% - 与 setting 内既有键（chat_state 等）共存不覆盖；
%%% - 非布尔目标拒绝。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(UID, 880951).
-define(UK3A, <<"C2C_880951_880952">>).
-define(UK3B, <<"C2G_880951_880953">>).

cleanup_test_rows() ->
    _ = elib_pg:execute(<<"DELETE FROM user_setting WHERE user_id = $1">>, [
        ?UID
    ]),
    ok.

manual_pending_user_setting_vectors_test_() ->
    %% 用例体抽成宏外函数：epp 宏参数切分不识别 <<>> 内部逗号。
    ?TEST_WITH_DB_TIMEOUT(60, fun vectors/0).

vectors() ->
    cleanup_test_rows(),
    try
        %% ── V1：首设 true → version 1；同目标重试版本不变（无重复副作用）。
        {ok, #{<<"value">> := true, <<"version">> := 1}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, true),
        {ok, #{<<"value">> := true, <<"version">> := 1}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, true),
        ?assertMatch(
            #{?UK3A := #{<<"value">> := true, <<"version">> := 1}},
            user_setting_ds:manual_pending_map(?UID)
        ),

        %% ── V2：显式 false → version 2；映射回读同值。
        {ok, #{<<"value">> := false, <<"version">> := 2}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, false),
        ?assertMatch(
            #{?UK3A := #{<<"value">> := false, <<"version">> := 2}},
            user_setting_ds:manual_pending_map(?UID)
        ),

        %% ── V3：多 uk3 隔离，各自版本独立推进。
        {ok, #{<<"version">> := 1}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3B, true),
        {ok, #{<<"version">> := 3}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, true),
        Map = user_setting_ds:manual_pending_map(?UID),
        ?assertMatch(
            #{
                ?UK3A := #{<<"value">> := true, <<"version">> := 3},
                ?UK3B := #{<<"value">> := true, <<"version">> := 1}
            },
            Map
        ),

        %% ── V4：与 setting 既有键共存（chat_state 不被覆盖）。
        ok = user_setting_repo:update(?UID, #{<<"chat_state">> => <<"online">>}),
        {ok, #{<<"version">> := 1}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, false),
        Setting = user_setting_repo:find_by_uid(?UID, <<"setting">>),
        ?assertMatch(#{<<"setting">> := _}, Setting),
        #{<<"setting">> := Bin} = Setting,
        Decoded = jsone:decode(Bin, [{object_format, map}]),
        ?assertMatch(#{<<"chat_state">> := <<"online">>}, Decoded),
        ?assertMatch(
            #{<<"manual_pending">> := #{?UK3A := #{<<"value">> := false}}},
            Decoded
        ),

        %% ── V5：非布尔目标拒绝（明确目标 true/false，J17.c）。
        ?assertMatch(
            {error, _},
            user_setting_ds:set_manual_pending(?UID, ?UK3A, 1)
        ),
        ?assertMatch(
            {error, _},
            user_setting_ds:set_manual_pending(?UID, <<>>, true)
        ),

        ok
    after
        cleanup_test_rows()
    end.
