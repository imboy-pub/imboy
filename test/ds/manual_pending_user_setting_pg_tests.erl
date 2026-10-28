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
-define(PEER, 880952).
-define(UK3A, <<"C2C_880951_880952">>).
-define(UK3B, <<"C2G_880951_880953">>).

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM user_setting WHERE user_id = $1">>, [?UID]},
            {<<"DELETE FROM conversation_pin WHERE user_id = $1">>, [?UID]}
        ]
    ),
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

        %% ── V6：CAS expected_version 匹配 → 应用并推进（J17.d）。
        {ok, #{<<"version">> := V6}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, true),
        {ok, #{<<"version">> := V6b}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, false, V6),
        ?assert(V6b =:= V6 + 1),

        %% ── V7：stale expected_version → conflict + 当前状态可回读，
        %% 当前值不被覆盖（保留 desired_value 由客户端决定重试）。
        {conflict, CurrentEntry} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, true, V6),
        ?assertMatch(
            #{<<"value">> := false, <<"version">> := V6b}, CurrentEntry
        ),
        ?assertMatch(
            #{?UK3A := #{<<"value">> := false, <<"version">> := V6b}},
            user_setting_ds:manual_pending_map(?UID)
        ),

        %% ── V8：同 expected_version 重放（已达目标）→ 正常返回当前状态，
        %% 版本不再推进（无变化同目标重试无重复副作用）。
        {ok, #{<<"version">> := V6b}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, false, V6b),

        %% ── V9：旧写 bump——pin 旧入口推进同会话版本（J17.e），新端凭
        %% stale expected_version 可检测旧写冲突。
        ok = conversation_pin_ds:pin_conversation(?UID, ?PEER, <<"c2c">>),
        {ok, #{<<"version">> := VBumped}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3A, true, V6b),
        ?assert(VBumped > V6b),
        %% uk3 构造与服务端约定一致（C2C_小_大）。
        ?assertEqual(
            <<"C2C_880951_880962">>,
            user_setting_ds:conversation_uk3(<<"c2c">>, ?UID, 880962)
        ),
        ?assertEqual(
            <<"C2C_880951_880962">>,
            user_setting_ds:conversation_uk3(<<"c2c">>, 880962, ?UID)
        ),

        ok
    after
        cleanup_test_rows()
    end.
