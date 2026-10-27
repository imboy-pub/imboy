-module(conversation_pref_generation_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% W09.b（channel-group-ux-v2.2 §6.7/J17.a/b/d）：会话偏好（pin/delete）
%%% 世代绑定（真实 PostgreSQL 向量）：
%%% - C2C 行恒适用（generation_id=0）；
%%% - C2G 写入绑定 active 世代；跨世代（退群/重入）旧行不适用
%%%   （旧值不能用于新世代，W09.d 固定退出）；
%%% - 重入群重新置顶绑定新世代；
%%% - 迁移回填：generation_id=0 的 C2G 旧行绑到 active 世代。
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(UID, 880961).
-define(PEER, 880962).
-define(GID, 880970).

cleanup_test_rows() ->
    lists:foreach(
        fun({Sql, Params}) -> _ = elib_pg:execute(Sql, Params) end,
        [
            {
                <<
                    "DELETE FROM conversation_pin WHERE user_id = ANY($1) "
                    "OR conversation_id = $2"
                >>,
                [[?UID, ?PEER], ?GID]
            },
            {
                <<
                    "DELETE FROM conversation_delete WHERE user_id = ANY($1) "
                    "OR conversation_id = $2"
                >>,
                [[?UID, ?PEER], ?GID]
            },
            {<<"DELETE FROM group_member_generation WHERE group_id = $1">>, [
                ?GID
            ]},
            {<<"DELETE FROM msg_store_seq WHERE conv_key = $1">>, [
                msg_archive_ds:conv_key_c2g(?GID)
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
                    "($1,'w09b-uid','w09b-uid','w09b-synthetic','127.0.0.1','w09b-synthetic'),"/utf8,
                    "($2,'w09b-peer','w09b-peer','w09b-synthetic','127.0.0.1','w09b-synthetic')"/utf8>>,
                [?UID, ?PEER]
            },
            %% 世代行手工构造（open 世代）：绕开 msg 计数器锁链。
            {
                <<
                    "INSERT INTO group_member_generation "
                    "(id, group_id, user_id, generation_no, start_seq) "
                    "VALUES (880971, $1, $2, 1, 100)"
                >>,
                [?GID, ?UID]
            }
        ]
    ),
    ok.

backfill_pin_row() ->
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO conversation_pin (id, user_id, conversation_id, conversation_type) "
            "VALUES (880981, $1, $2, 'c2g')"
        >>,
        [?UID, ?GID]
    ),
    %% 重放迁移 233 的回填语句（幂等：仅 generation_id=0 的行）。
    {ok, _} = elib_pg:execute(
        <<
            "UPDATE public.conversation_pin p SET generation_id = g.id "
            "FROM public.group_member_generation g "
            "WHERE p.conversation_type = 'c2g' AND p.generation_id = 0 "
            "AND g.group_id = p.conversation_id AND g.user_id = p.user_id "
            "AND g.end_seq IS NULL"
        >>,
        []
    ),
    ok.

conversation_pref_generation_vectors_test_() ->
    %% 用例体抽成宏外函数：epp 宏参数切分不识别 <<>> 内部逗号。
    ?TEST_WITH_DB_TIMEOUT(60, fun vectors/0).

vectors() ->
    cleanup_test_rows(),
    try
        seed_test_rows(),

        %% ── V1：C2C 置顶恒适用（generation_id=0）。
        ok = conversation_pin_ds:pin_conversation(?UID, ?PEER, <<"c2c">>),
        {ok, Rows} = conversation_pin_repo:list(?UID),
        ?assertMatch([#{<<"conversation_type">> := <<"c2c">>}], Rows),

        %% ── V2：C2G 置顶绑定 active 世代（gen 880971），读取适用。
        ok = conversation_pin_ds:pin_conversation(?UID, ?GID, <<"c2g">>),
        {ok, Rows2} = conversation_pin_repo:list(?UID),
        ?assertEqual(2, length(Rows2)),
        ?assertEqual(true, conversation_pin_repo:is_pinned(?UID, ?GID, <<"c2g">>)),
        {ok, [#{<<"generation_id">> := 880971}]} = elib_pg:query(
            <<
                "SELECT generation_id FROM conversation_pin "
                "WHERE user_id = $1 AND conversation_id = $2 "
                "AND conversation_type = 'c2g'"
            >>,
            [?UID, ?GID]
        ),

        %% ── V3：关闭世代（退群）→ C2G 旧行不适用（旧值不用于新世代），
        %% C2C 行不受影响。
        {ok, _} = elib_pg:execute(
            <<
                "UPDATE group_member_generation SET end_seq = 105 "
                "WHERE id = 880971"
            >>,
            []
        ),
        {ok, Rows3} = conversation_pin_repo:list(?UID),
        ?assertEqual(1, length(Rows3)),
        ?assertMatch([#{<<"conversation_type">> := <<"c2c">>}], Rows3),
        ?assertEqual(
            false, conversation_pin_repo:is_pinned(?UID, ?GID, <<"c2g">>)
        ),
        %% 隐藏态同样不跨世代。
        {ok, _} = conversation_delete_repo:mark_deleted(?UID, ?GID, <<"c2g">>),
        ?assertEqual(
            false, conversation_delete_repo:is_deleted(?UID, ?GID, <<"c2g">>)
        ),

        %% ── V4：重入群（新世代 880972）→ 旧行仍不适用；重新写入
        %% UPSERT 换绑新世代（单行，generation_id 刷新）。
        {ok, _} = elib_pg:execute(
            <<
                "INSERT INTO group_member_generation "
                "(id, group_id, user_id, generation_no, start_seq) "
                "VALUES (880972, $1, $2, 2, 200)"
            >>,
            [?GID, ?UID]
        ),
        ?assertEqual(
            false, conversation_pin_repo:is_pinned(?UID, ?GID, <<"c2g">>)
        ),
        ok = conversation_pin_ds:pin_conversation(?UID, ?GID, <<"c2g">>),
        ?assertEqual(
            true, conversation_pin_repo:is_pinned(?UID, ?GID, <<"c2g">>)
        ),
        {ok, GenRows} = elib_pg:query(
            <<
                "SELECT generation_id FROM conversation_pin "
                "WHERE user_id = $1 AND conversation_id = $2 "
                "AND conversation_type = 'c2g'"
            >>,
            [?UID, ?GID]
        ),
        ?assertMatch([#{<<"generation_id">> := 880972}], GenRows),

        %% ── V5：迁移回填——generation_id=0 的 C2G 旧行绑到 active 世代。
        _ = elib_pg:execute(
            <<
                "DELETE FROM conversation_pin WHERE user_id = $1 "
                "AND conversation_id = $2"
            >>,
            [?UID, ?GID]
        ),
        _ = elib_pg:execute(
            <<
                "UPDATE group_member_generation SET end_seq = NULL "
                "WHERE id = 880972"
            >>,
            []
        ),
        backfill_pin_row(),
        ?assertEqual(
            true, conversation_pin_repo:is_pinned(?UID, ?GID, <<"c2g">>)
        ),

        ok
    after
        cleanup_test_rows()
    end.
