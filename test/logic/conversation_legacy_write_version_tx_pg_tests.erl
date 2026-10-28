-module(conversation_legacy_write_version_tx_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

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
%%% - pin/unpin/delete/restore 的行写入、设置读取及写入失败均回滚；
%%% - stale 世代 unpin/restore 无当前状态变化，保留历史、不推版本或通知；
%%% - conflict 经真实 Handler/JSON envelope 仅回传白名单字段。
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
        fun({Sql, Params}) -> {ok, _} = elib_pg:execute(Sql, Params) end,
        [
            {<<"DELETE FROM user_setting WHERE user_id = $1">>, [?UID]},
            {
                <<
                    "DELETE FROM conversation_pin WHERE user_id = $1"
                >>,
                [?UID]
            },
            {
                <<
                    "DELETE FROM conversation_delete WHERE user_id = $1"
                >>,
                [?UID]
            },
            {<<"DELETE FROM group_member_generation WHERE group_id = $1 AND user_id = $2">>, [
                ?GID, ?UID
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
        Other -> error({pin_row_query_failed, Other})
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
        Other -> error({delete_row_query_failed, Other})
    end.

conversation_legacy_write_version_tx_test_() ->
    %% 用例体抽成宏外函数：epp 宏参数切分不识别 <<>> 内部逗号。
    ?TEST_WITH_DB_TIMEOUT(60, fun vectors/0).

vectors() ->
    with_fixture(fun legacy_vectors/0).

with_fixture(TestFun) ->
    cleanup_test_rows(),
    try
        meck:new(msg_s2c_ds, [no_link]),
        meck:expect(
            msg_s2c_ds,
            send,
            fun(?UID, [?UID], _Action, <<>>, null, _Payload, no_save) -> ok end
        ),
        meck:new(imboy_cache, [passthrough, no_link]),
        meck:expect(imboy_cache, invalidate, fun(_Key) -> ok end),
        seed_test_rows(),
        TestFun()
    after
        lists:foreach(
            fun(Module) -> meck:unload(Module) end,
            [
                Module
             || Module <- [msg_s2c_ds, imboy_cache],
                lists:member(Module, meck:mocked())
            ]
        ),
        cleanup_test_rows()
    end.

legacy_vectors() ->
    legacy_pin_vectors(),
    legacy_delete_vectors(),
    c2g_pin_generation_binding().

legacy_pin_vectors() ->
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
    absent = mp_entry(<<"C2C_880991_999001">>).

legacy_delete_vectors() ->
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
    {true, 6} = mp_entry(?UK3_C2C).

c2g_pin_generation_binding() ->
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
    c2g_delete_generation_binding(GenId).

c2g_delete_generation_binding(GenId) ->
    meck:new(group_member_ds, [passthrough, no_link]),
    meck:new(conversation_delete_repo, [passthrough, no_link]),
    try
        ok = conversation_logic:delete(?UID, ?GID, <<"c2g">>),
        {ok, #{<<"generation_id">> := GenId}} = elib_pg:one(
            <<
                "SELECT generation_id FROM conversation_delete "
                "WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = 'c2g'"
            >>,
            [?UID, ?GID]
        ),
        [GenerationConn] = [
            Conn
         || {_Pid, {group_member_ds, active_generation_id_tx, [Conn, ?GID, ?UID]}, Result} <- meck:history(
                group_member_ds
            ),
            Result =:= GenId
        ],
        ?assert(is_pid(GenerationConn)),
        ?assert(
            meck:called(
                conversation_delete_repo,
                mark_deleted_tx,
                [GenerationConn, ?UID, ?GID, <<"c2g">>, GenId]
            )
        ),
        ?assertEqual(0, meck:num_calls(group_member_ds, active_generation_id, 2)),
        {false, 2} = mp_entry(?UK3_C2G),
        ?assert(meck:validate(group_member_ds)),
        ?assert(meck:validate(conversation_delete_repo))
    after
        meck:unload(conversation_delete_repo),
        meck:unload(group_member_ds)
    end.

legacy_write_rollback_test_() ->
    {inorder, [
        {
            atom_to_list(Action) ++ " rollback at " ++ atom_to_list(Stage),
            rollback_test(Action, Stage)
        }
     || Action <- [pin, unpin, delete, restore],
        Stage <- [row_write, setting_read, setting_write, after_setting_write, setting_query]
    ]}.

rollback_test(Action, Stage) ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_fixture(fun() -> rollback_vector(Action, Stage) end)
    end).

rollback_vector(Action, Stage) ->
    Version = prepare_rollback(Action),
    Before = snapshot(),
    reset_side_effects(),
    {Repo, Function} = injection_target(Action, Stage),
    meck:new(Repo, [passthrough, no_link]),
    try
        inject_failure(Action, Stage, Repo, Function),
        ?assertEqual(
            {error, action_error(Action)},
            legacy_write(Action, ?PEER, <<"c2c">>)
        ),
        assert_injection_calls(Repo, Function),
        ?assert(meck:validate(Repo))
    after
        meck:unload(Repo)
    end,
    ?assertEqual(Before, snapshot()),
    assert_no_side_effects(),
    ok = legacy_write(Action, ?PEER, <<"c2c">>),
    ?assertEqual({true, Version + 1}, mp_entry(?UK3_C2C)),
    assert_action_row(Action),
    ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
    ExpectedInvalidations =
        case Action of
            delete -> 1;
            restore -> 1;
            _ -> 0
        end,
    ?assertEqual(ExpectedInvalidations, meck:num_calls(imboy_cache, invalidate, 1)),
    ok = legacy_write(Action, ?PEER, <<"c2c">>),
    ?assertEqual({true, Version + 1}, mp_entry(?UK3_C2C)),
    ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
    ?assertEqual(ExpectedInvalidations, meck:num_calls(imboy_cache, invalidate, 1)).

prepare_rollback(Action) ->
    {ok, _} = user_setting_ds:set_manual_pending(?UID, ?UK3_C2C, true, 0),
    case Action of
        unpin -> ok = legacy_write(pin, ?PEER, <<"c2c">>);
        restore -> ok = legacy_write(delete, ?PEER, <<"c2c">>);
        _ -> ok
    end,
    {true, Version} = mp_entry(?UK3_C2C),
    Version.

injection_target(pin, row_write) -> {conversation_pin_repo, pin_tx};
injection_target(unpin, row_write) -> {conversation_pin_repo, unpin_tx};
injection_target(delete, row_write) -> {conversation_delete_repo, mark_deleted_tx};
injection_target(restore, row_write) -> {conversation_delete_repo, restore_tx};
injection_target(_Action, setting_query) -> {elib_pg, query};
injection_target(_Action, setting_read) -> {user_setting_repo, find_setting_for_update_tx};
injection_target(_Action, _Stage) -> {user_setting_repo, upsert_setting_tx}.

inject_failure(Action, row_write, Repo, Function) when Action =:= pin; Action =:= delete ->
    meck:expect(Repo, Function, fun(Conn, Uid, Cid, Type, GenerationId) ->
        {ok, 1} = meck:passthrough([Conn, Uid, Cid, Type, GenerationId]),
        assert_mutated(Conn, Action),
        {error, injected_row_write}
    end);
inject_failure(Action, row_write, Repo, Function) ->
    meck:expect(Repo, Function, fun(Conn, Uid, Cid, Type) ->
        {ok, 1} = meck:passthrough([Conn, Uid, Cid, Type]),
        assert_mutated(Conn, Action),
        {error, injected_row_write}
    end);
inject_failure(Action, setting_read, Repo, Function) ->
    meck:expect(Repo, Function, fun(Conn, ?UID) ->
        assert_mutated(Conn, Action),
        {error, injected_setting_read}
    end);
inject_failure(Action, setting_query, elib_pg, query) ->
    inject_pg_query_failure(Action, setting_query);
inject_failure(Action, Stage, Repo, Function) ->
    meck:expect(Repo, Function, fun(Conn, ?UID, Setting) ->
        assert_mutated(Conn, Action),
        case Stage of
            after_setting_write ->
                ok = meck:passthrough([Conn, ?UID, Setting]),
                ?assertEqual(
                    {ok, Setting},
                    user_setting_repo:find_setting_for_update_tx(Conn, ?UID)
                );
            setting_write ->
                ok
        end,
        {error, {injected, Stage}}
    end).

assert_mutated(Conn, Action) ->
    Table = action_table(Action),
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(
        Conn,
        <<"SELECT count(*) AS count FROM ", Table/binary,
            " WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = 'c2c'">>,
        [?UID, ?PEER]
    ),
    Expected =
        case Action of
            pin -> 1;
            delete -> 1;
            _ -> 0
        end,
    ?assertEqual(Expected, Count).

assert_action_row(pin) -> ?assert(pin_row_exists(?PEER, <<"c2c">>));
assert_action_row(unpin) -> ?assertNot(pin_row_exists(?PEER, <<"c2c">>));
assert_action_row(delete) -> ?assert(delete_row_exists(?PEER, <<"c2c">>));
assert_action_row(restore) -> ?assertNot(delete_row_exists(?PEER, <<"c2c">>)).

action_table(pin) -> <<"conversation_pin">>;
action_table(unpin) -> <<"conversation_pin">>;
action_table(delete) -> <<"conversation_delete">>;
action_table(restore) -> <<"conversation_delete">>.

legacy_write(pin, Cid, Type) -> conversation_pin_logic:pin(?UID, Cid, Type);
legacy_write(unpin, Cid, Type) -> conversation_pin_logic:unpin(?UID, Cid, Type);
legacy_write(delete, Cid, Type) -> conversation_logic:delete(?UID, Cid, Type);
legacy_write(restore, Cid, Type) -> conversation_logic:restore(?UID, Cid, Type).

action_error(pin) -> <<"置顶会话失败"/utf8>>;
action_error(unpin) -> <<"取消置顶会话失败"/utf8>>;
action_error(delete) -> <<"删除会话失败"/utf8>>;
action_error(restore) -> <<"恢复会话失败"/utf8>>.

snapshot() ->
    [
        begin
            {ok, Rows} = elib_pg:query(
                <<"SELECT * FROM ", Table/binary, " WHERE user_id = $1">>, [?UID]
            ),
            {Table, lists:sort(Rows)}
        end
     || Table <- [<<"conversation_pin">>, <<"conversation_delete">>, <<"user_setting">>]
    ].

reset_side_effects() ->
    meck:reset(msg_s2c_ds),
    meck:reset(imboy_cache).

assert_no_side_effects() ->
    ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7)),
    ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1)).

assert_injection_calls(elib_pg, query) ->
    ?assertEqual(
        1,
        length([
            ok
         || {_Pid, {elib_pg, query, _Args}, {error, #error{codename = division_by_zero}}} <- meck:history(
                elib_pg
            )
        ])
    );
assert_injection_calls(Repo, Function) ->
    ?assertEqual(1, meck:num_calls(Repo, Function, '_')).

inject_pg_query_failure(Action, Stage) ->
    meck:expect(elib_pg, query, fun(Conn, Sql, Params) ->
        case failed_query(Stage, iolist_to_binary(Sql), Params) of
            true ->
                case Stage of
                    setting_query -> assert_mutated(Conn, Action);
                    _ -> ok
                end,
                Result = meck:passthrough([Conn, <<"SELECT 1 / 0">>, []]),
                ?assertMatch({error, #error{codename = division_by_zero}}, Result),
                Result;
            false ->
                meck:passthrough([Conn, Sql, Params])
        end
    end).

failed_query(setting_query, <<"SELECT setting FROM ", _Rest/binary>>, [?UID]) ->
    true;
failed_query(
    state_query,
    <<"SELECT count(*) as count FROM ", _Rest/binary>>,
    [?UID, ?GID, <<"c2g">>]
) ->
    true;
failed_query(
    generation_query,
    <<"SELECT id FROM public.group_member_generation ", _Rest/binary>>,
    [?GID, ?UID]
) ->
    true;
failed_query(_Stage, _Sql, _Params) ->
    false.

prewrite_pg_query_failure_test_() ->
    {inorder, [
        {atom_to_list(Action) ++ " " ++ atom_to_list(Stage), prewrite_failure_test(Action, Stage)}
     || Action <- [pin, delete],
        Stage <- [state_query, generation_query]
    ]}.

prewrite_failure_test(Action, Stage) ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_fixture(fun() -> prewrite_failure_vector(Action, Stage) end)
    end).

prewrite_failure_vector(Action, Stage) ->
    {ok, _} = user_setting_ds:set_manual_pending(?UID, ?UK3_C2G, true, 0),
    Before = snapshot(),
    reset_side_effects(),
    meck:new(elib_pg, [passthrough, no_link]),
    try
        inject_pg_query_failure(Action, Stage),
        ?assertEqual({error, action_error(Action)}, legacy_write(Action, ?GID, <<"c2g">>)),
        assert_injection_calls(elib_pg, query),
        ?assertEqual(0, meck:num_calls(elib_pg, execute_count, 3)),
        ?assertEqual(0, meck:num_calls(elib_pg, execute, 3)),
        ?assert(meck:validate(elib_pg))
    after
        meck:unload(elib_pg)
    end,
    ?assertEqual(Before, snapshot()),
    assert_no_side_effects(),
    ok = legacy_write(Action, ?GID, <<"c2g">>),
    ?assertEqual({true, 2}, mp_entry(?UK3_C2G)),
    ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)).

invalid_setting_pg_rollback_test_() ->
    {inorder, [
        {
            atom_to_list(Action) ++ " rejects " ++ atom_to_list(Kind),
            invalid_setting_test(Action, Bin)
        }
     || Action <- [pin, unpin, delete, restore],
        {Kind, Bin} <- [{array, <<"[]">>}, {boolean, <<"false">>}]
    ]}.

invalid_setting_test(Action, Bin) ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_fixture(fun() -> invalid_setting_vector(Action, Bin) end)
    end).

invalid_setting_vector(Action, Bin) ->
    Version = prepare_rollback(Action),
    {ok, #{<<"setting">> := Healthy}} = elib_pg:one(
        <<"SELECT setting FROM user_setting WHERE user_id = $1">>, [?UID]
    ),
    replace_setting(Bin),
    Before = snapshot(),
    reset_side_effects(),
    meck:new(user_setting_repo, [passthrough, no_link]),
    try
        ?assertEqual({error, action_error(Action)}, legacy_write(Action, ?PEER, <<"c2c">>)),
        ?assertEqual([{error, invalid_setting_json}], [
            Result
         || {_Pid, {user_setting_repo, find_setting_for_update_tx, [_Conn, ?UID]}, Result} <-
                meck:history(user_setting_repo)
        ]),
        ?assert(meck:validate(user_setting_repo))
    after
        meck:unload(user_setting_repo)
    end,
    ?assertEqual(Before, snapshot()),
    assert_no_side_effects(),
    replace_setting(Healthy),
    ok = legacy_write(Action, ?PEER, <<"c2c">>),
    ?assertEqual({true, Version + 1}, mp_entry(?UK3_C2C)),
    assert_action_row(Action),
    ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)).

replace_setting(Bin) ->
    ?assertEqual(
        {ok, 1},
        elib_pg:execute_count(
            <<"UPDATE user_setting SET setting = $1 WHERE user_id = $2">>, [Bin, ?UID]
        )
    ).

stale_generation_noop_test_() ->
    {inorder, [
        {atom_to_list(Action) ++ " preserves stale generation", stale_test(Action)}
     || Action <- [unpin, restore]
    ]}.

stale_test(Action) ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_fixture(fun() -> stale_generation_vector(Action) end)
    end).

stale_generation_vector(Action) ->
    SetAction =
        case Action of
            unpin -> pin;
            restore -> delete
        end,
    ok = legacy_write(SetAction, ?GID, <<"c2g">>),
    {false, 1} = mp_entry(?UK3_C2G),
    {ok, _} = elib_pg:execute(
        <<
            "UPDATE group_member_generation SET end_seq = 200 "
            "WHERE group_id = $1 AND user_id = $2 AND end_seq IS NULL"
        >>,
        [?GID, ?UID]
    ),
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO group_member_generation "
            "(id, group_id, user_id, generation_no, start_seq) VALUES (880997, $1, $2, 2, 201)"
        >>,
        [?GID, ?UID]
    ),
    ?assertEqual(false, conversation_pin_repo:is_pinned(?UID, ?GID, <<"c2g">>)),
    ?assertEqual(false, conversation_delete_repo:is_deleted(?UID, ?GID, <<"c2g">>)),
    Before = snapshot(),
    reset_side_effects(),
    ok = legacy_write(Action, ?GID, <<"c2g">>),
    ?assertEqual(Before, snapshot()),
    assert_no_side_effects(),
    ok = legacy_write(SetAction, ?GID, <<"c2g">>),
    Table = action_table(Action),
    ?assertEqual(
        {ok, #{<<"generation_id">> => 880997}},
        elib_pg:one(
            <<"SELECT generation_id FROM ", Table/binary,
                " WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = 'c2g'">>,
            [?UID, ?GID]
        )
    ),
    ?assertEqual({false, 2}, mp_entry(?UK3_C2G)),
    ok = legacy_write(Action, ?GID, <<"c2g">>),
    ?assertEqual({false, 3}, mp_entry(?UK3_C2G)),
    ?assertEqual(
        {ok, []},
        elib_pg:query(
            <<"SELECT id FROM ", Table/binary,
                " WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = 'c2g'">>,
            [?UID, ?GID]
        )
    ),
    ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7)),
    ExpectedInvalidations =
        case Action of
            restore -> 2;
            unpin -> 0
        end,
    ?assertEqual(ExpectedInvalidations, meck:num_calls(imboy_cache, invalidate, 1)).

conflict_envelope_whitelist_test_() ->
    {inorder, [
        {atom_to_list(Kind) ++ " conflict envelope whitelist", conflict_test(Kind)}
     || Kind <- [existing, absent]
    ]}.

conflict_test(Kind) ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_fixture(fun() -> conflict_envelope_vector(Kind) end)
    end).

conflict_envelope_vector(Kind) ->
    {Version, ExpectedVersion, Setting} = conflict_setting(Kind),
    {ok, _} = elib_pg:execute(
        <<"INSERT INTO user_setting (user_id, setting, updated_at) VALUES ($1, $2, $3)">>,
        [?UID, jsone:encode(Setting), elib_dt:now()]
    ),
    Before = snapshot(),
    Body = jsone:encode(#{
        <<"uk3">> => ?UK3_C2C,
        <<"value">> => true,
        <<"expected_version">> => ExpectedVersion,
        <<"user_id">> => ?PEER
    }),
    meck:new(cowboy_req, [no_link]),
    try
        meck:expect(cowboy_req, read_body, fun(Req) -> {ok, Body, Req} end),
        meck:expect(cowboy_req, reply, fun(Status, Headers, Wire, _Req) ->
            #{status => Status, headers => Headers, body => Wire}
        end),
        {ok, Response, #{current_uid := ?UID}} = conversation_handler:init(
            req,
            #{action => set_manual_pending, current_uid => ?UID}
        ),
        ?assertEqual(200, maps:get(status, Response)),
        Envelope = jsone:decode(
            iolist_to_binary(maps:get(body, Response)),
            [{object_format, map}]
        ),
        ?assert(is_integer(maps:get(<<"sv_ts">>, Envelope))),
        ?assertEqual(
            #{
                <<"code">> => ?ERR_OPERATION_FAILED,
                <<"msg">> => <<"版本冲突"/utf8>>,
                <<"payload">> => #{},
                <<"conflict">> => true,
                <<"current">> => #{
                    <<"uk3">> => ?UK3_C2C,
                    <<"value">> => false,
                    <<"version">> => Version
                }
            },
            maps:remove(<<"sv_ts">>, Envelope)
        ),
        ?assertEqual(1, meck:num_calls(cowboy_req, reply, 4)),
        ?assert(meck:validate(cowboy_req)),
        ?assertEqual(Before, snapshot()),
        assert_no_side_effects()
    after
        meck:unload(cowboy_req)
    end.

conflict_setting(Kind) ->
    Other = #{
        <<"value">> => true,
        <<"version">> => 91,
        <<"private">> => <<"synthetic-other-conversation">>
    },
    Pending = #{<<"C2C_880991_999003">> => Other},
    Setting = #{
        <<"private">> => <<"synthetic-account-setting">>,
        <<"manual_pending">> => Pending
    },
    case Kind of
        absent ->
            {0, 1, Setting};
        existing ->
            Entry = #{
                <<"value">> => false,
                <<"version">> => 7,
                <<"updated_at">> => 123456,
                <<"generation_id">> => 880996,
                <<"private">> => <<"synthetic-entry-private">>
            },
            {7, 6, Setting#{<<"manual_pending">> => Pending#{?UK3_C2C => Entry}}}
    end.
