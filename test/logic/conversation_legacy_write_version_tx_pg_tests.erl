-module(conversation_legacy_write_version_tx_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W09.c（channel-group-ux-v2.2 §6.7/D7/J17.e）：旧会话写入与版本推进
%%% 的原子性（真实 PostgreSQL 向量）：
%%% - 旧 pin/unpin/delete/restore 与 user_setting 会话版本推进同一事务
%%%   原子提交（任一步失败整体回滚，不靠事后补偿）；
%%% - 无变化同目标重试不推进版本、不建立条目（不重复副作用）；
%%% - 旧写推进版本后，新端 stale expected_version 写入返回 conflict
%%%   （J17.e：新端能检测旧写冲突）；
%%% - C2G 旧写在事务内绑定 active 成员世代（与行写入同一 Conn 快照）；
%%% - 失败注入：版本推进失败 → 置顶行一并回滚。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(UID, 880991).
-define(PEER, 880992).
-define(GID, 880995).

-define(UK3_C2C, <<"C2C_880991_880992">>).
-define(UK3_C2G, <<"C2G_880991_880995">>).

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM user_setting WHERE user_id = $1">>, [?UID]},
            {
                <<
                    "DELETE FROM conversation_pin WHERE user_id = $1 "
                    "OR conversation_id = $2"
                >>,
                [?UID, ?GID]
            },
            {
                <<
                    "DELETE FROM conversation_delete WHERE user_id = $1 "
                    "OR conversation_id = $2"
                >>,
                [?UID, ?GID]
            },
            {<<"DELETE FROM group_member_generation WHERE group_id = $1">>, [
                ?GID
            ]},
            {<<"DELETE FROM \"user\" WHERE id = ANY($1)">>, [[?UID, ?PEER]]}
        ]
    ),
    ok.

seed_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
        [
            {
                <<"INSERT INTO \"user\" (id, account, nickname, password, reg_ip, reg_cosv) VALUES "/utf8,
                    "($1,'w09c-uid','w09c-uid','w09c-synthetic','127.0.0.1','w09c-synthetic'),"/utf8,
                    "($2,'w09c-peer','w09c-peer','w09c-synthetic','127.0.0.1','w09c-synthetic')"/utf8>>,
                [?UID, ?PEER]
            },
            %% open 世代行（C2G 旧写事务内绑定 active 世代用）。
            {
                <<
                    "INSERT INTO group_member_generation "
                    "(id, group_id, user_id, generation_no, start_seq) "
                    "VALUES (880996, $1, $2, 1, 100)"
                >>,
                [?GID, ?UID]
            }
        ]
    ),
    ok.

%% user_setting.manual_pending 中某会话条目的 {Value, Version} 投影。
mp_entry(Uk3) ->
    Map = user_setting_ds:manual_pending_map(?UID),
    case maps:get(Uk3, Map, undefined) of
        Entry when is_map(Entry) ->
            {
                maps:get(<<"value">>, Entry),
                maps:get(<<"version">>, Entry)
            };
        _ ->
            absent
    end.

pin_row_exists(Cid, Type) ->
    case
        elib_pg:one(
            <<
                "SELECT count(*) AS c FROM conversation_pin "
                "WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = $3"
            >>,
            [?UID, Cid, Type]
        )
    of
        {ok, #{<<"c">> := N}} -> N > 0;
        _ -> false
    end.

delete_row_exists(Cid, Type) ->
    case
        elib_pg:one(
            <<
                "SELECT count(*) AS c FROM conversation_delete "
                "WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = $3"
            >>,
            [?UID, Cid, Type]
        )
    of
        {ok, #{<<"c">> := N}} -> N > 0;
        _ -> false
    end.

conversation_legacy_write_version_tx_test_() ->
    %% 用例体抽成宏外函数：epp 宏参数切分不识别 <<>> 内部逗号。
    ?TEST_WITH_DB_TIMEOUT(60, fun vectors/0).

vectors() ->
    cleanup_test_rows(),
    seed_test_rows(),
    try
        %% ── V1（J17.e）：旧 pin 推进版本（条目不存在 → value=false,v1）；
        %% 旧 unpin 再推进（v2）；新端 stale expected_version → conflict。
        ok = conversation_pin_logic:pin(?UID, ?PEER, <<"c2c">>),
        true = pin_row_exists(?PEER, <<"c2c">>),
        {false, 1} = mp_entry(?UK3_C2C),
        ok = conversation_pin_logic:unpin(?UID, ?PEER, <<"c2c">>),
        false = pin_row_exists(?PEER, <<"c2c">>),
        {false, 2} = mp_entry(?UK3_C2C),
        {conflict, #{<<"version">> := 2}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3_C2C, true, 1),
        %% 冲突回读：新端重试原目标以当前版本成功。
        {ok, #{<<"version">> := 3, <<"value">> := true}} =
            user_setting_ds:set_manual_pending(?UID, ?UK3_C2C, true, 2),

        %% ── V2：无变化同目标重试不推进版本；unpin 未置顶不建条目。
        %% （V1 末尾 manual_pending 已达 true,v3；pin 的 bump 保留原 value。）
        ok = conversation_pin_logic:pin(?UID, ?PEER, <<"c2c">>),
        {true, 4} = mp_entry(?UK3_C2C),
        ok = conversation_pin_logic:pin(?UID, ?PEER, <<"c2c">>),
        {true, 4} = mp_entry(?UK3_C2C),
        ok = conversation_pin_logic:unpin(?UID, 999001, <<"c2c">>),
        absent = mp_entry(<<"C2C_880991_999001">>),

        %% ── V3：delete/restore 分别推进版本；重复 delete 无变化不推进。
        ok = conversation_logic:delete(?UID, ?PEER, <<"c2c">>),
        true = delete_row_exists(?PEER, <<"c2c">>),
        {true, 5} = mp_entry(?UK3_C2C),
        ok = conversation_logic:delete(?UID, ?PEER, <<"c2c">>),
        {true, 5} = mp_entry(?UK3_C2C),
        ok = conversation_logic:restore(?UID, ?PEER, <<"c2c">>),
        false = delete_row_exists(?PEER, <<"c2c">>),
        {true, 6} = mp_entry(?UK3_C2C),
        ok = conversation_logic:restore(?UID, ?PEER, <<"c2c">>),
        {true, 6} = mp_entry(?UK3_C2C),

        %% ── V4（失败注入）：版本推进失败 → 置顶行整体回滚（不吞错）。
        meck:new(user_setting_repo, [passthrough, no_link]),
        meck:expect(
            user_setting_repo,
            find_setting_for_update_tx,
            2,
            fun(_Conn, _Uid) -> {error, w09c_injected} end
        ),
        {error, _} = conversation_pin_logic:pin(?UID, 999002, <<"c2c">>),
        meck:unload(user_setting_repo),
        false = pin_row_exists(999002, <<"c2c">>),
        %% 注入解除后重试：写入与版本推进一起成功。
        ok = conversation_pin_logic:pin(?UID, 999002, <<"c2c">>),
        true = pin_row_exists(999002, <<"c2c">>),
        {false, 1} = mp_entry(<<"C2C_880991_999002">>),

        %% ── V5：C2G 旧写在事务内绑定 active 世代并推进 C2G 会话版本。
        {ok, #{<<"id">> := GenId}} =
            elib_pg:one(
                <<
                    "SELECT id FROM group_member_generation "
                    "WHERE group_id = $1 AND user_id = $2 AND end_seq IS NULL"
                >>,
                [?GID, ?UID]
            ),
        ok = conversation_pin_logic:pin(?UID, ?GID, <<"c2g">>),
        {ok, #{<<"generation_id">> := GenId}} =
            elib_pg:one(
                <<
                    "SELECT generation_id FROM conversation_pin "
                    "WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = 'c2g'"
                >>,
                [?UID, ?GID]
            ),
        {false, 1} = mp_entry(?UK3_C2G),
        ok
    after
        cleanup_test_rows()
    end.
