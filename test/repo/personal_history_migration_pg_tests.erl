-module(personal_history_migration_pg_tests).

%% M-04 / MSG-H17：个人历史独立存储迁移的真库验证（M-04 卡测试命令入口）。
%% 复用 inttest_marker_db 一次性 marker 库配方：隔离库 + 全链 erlang_migrate
%% （strict，与生产/演练同口径）；无 skip 语义，任何环境/建库/迁移失败显式 error。
%% 库资源租约：.task-state/IMBOY_MASTER_V2_2_20261118/locks/m04/qa-db.json
%% （127.0.0.1:4323，仅 inttest_m04ph_* 一次性库；不触碰 imboy_test_v1）。

-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------- generators

%% 全链 fresh 库（含 276-279）：干净安装、幂等重跑、默认零授权、约束守卫。
migration_test_() ->
    {timeout, 1800,
        {setup, fun setup_full/0, fun cleanup/1, fun(S) ->
            [
                ?_test(fresh_full_chain(S)),
                ?_test(rerun_idempotent(S)),
                ?_test(default_no_authorization(S)),
                ?_test(constraint_guards(S))
            ]
        end}}.

%% 回退演练库（停在 275 → goto 279 → down 4 → up）：MSG-H17 真实 up/down/up。
rollback_test_() ->
    {timeout, 1800,
        {setup, fun setup_rehearsal/0, fun cleanup/1, fun(S) ->
            [?_test(rollback_rehearsal(S))]
        end}}.

%% ---------------------------------------------------------------- setup/cleanup

setup_full() ->
    inttest_marker_db:provision(#{
        env_prefix => <<"M04PH">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

setup_rehearsal() ->
    inttest_marker_db:provision(#{
        env_prefix => <<"M04PHRB">>,
        migration_target => 275,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

cleanup(S) ->
    inttest_marker_db:release(S).

%% ---------------------------------------------------------------- helpers

mig_conf(#{conn := Conn}) ->
    #{conn => Conn, dir => "priv/migrations", strict => true}.

current_version(Conn) ->
    case epgsql:equery(Conn, "SELECT max(version)::bigint FROM schema_migrations", []) of
        {ok, _, [{V}]} when is_integer(V) -> V;
        {ok, _, [{null}]} -> 0;
        {error, Reason} -> erlang:error({current_version_failed, Reason})
    end.

table_exists(Conn, Name) ->
    {ok, _, Rows} = epgsql:equery(
        Conn,
        "SELECT 1 FROM information_schema.tables "
        "WHERE table_schema='public' AND table_name=$1",
        [Name]
    ),
    Rows =/= [].

expect_pg_error(Conn, Sql, Params, WantCode) ->
    case epgsql:equery(Conn, Sql, Params) of
        {error, {error, _, WantCode, _, _, _}} -> ok;
        {error, {error, WantCode, _, _, _, _}} -> ok;
        {error, Reason} -> erlang:error({wrong_pg_error, WantCode, Reason});
        Ok -> erlang:error({expected_pg_error, WantCode, Ok})
    end.

%% ---------------------------------------------------------------- full-chain suite

fresh_full_chain(#{conn := Conn} = _S) ->
    Head = inttest_marker_db:migration_head(),
    %% 协调者修复（2026-11-20）：M-01/M-02 落地 280/281 后 head 推进，本卡锚点
    %% 放宽为 >=279；不变式保持「库版本==目录 head」+ 下方 276-279 台账行齐全。
    ?assert(Head >= 279, {migration_head_moved, Head}),
    ?assertEqual(Head, current_version(Conn)),
    lists:foreach(
        fun(V) ->
            {ok, _, [{N}]} = epgsql:equery(
                Conn, "SELECT count(*)::bigint FROM schema_migrations_history WHERE version=$1", [V]
            ),
            ?assertEqual(1, N, {missing_ledger_version, V})
        end,
        [276, 277, 278, 279]
    ),
    lists:foreach(
        fun(T) -> ?assert(table_exists(Conn, T), {missing_table, T}) end,
        [
            <<"personal_history_authorization">>,
            <<"personal_history_archive">>,
            <<"personal_history_cursor">>,
            <<"personal_history_key_package">>
        ]
    ),
    %% 基线依赖表仍在（全链未受影响）
    ?assert(table_exists(Conn, <<"msg_delivery">>)),
    ?assert(table_exists(Conn, <<"enterprise_message_delivery">>)),
    {comment, <<"fresh chain to head 279 verified">>}.

rerun_idempotent(#{conn := Conn} = S) ->
    Before = current_version(Conn),
    case erlang_migrate:up(mig_conf(S)) of
        ok -> ok;
        {ok, _Applied} -> ok;
        {error, Reason} -> erlang:error({rerun_failed, Reason})
    end,
    ?assertEqual(Before, current_version(Conn)),
    {comment, <<"second up is a no-op">>}.

%% MSG-H01/MSG-H17：旧用户默认无个人授权 —— 新表零默认行，既有域原样。
default_no_authorization(#{conn := Conn} = _S) ->
    lists:foreach(
        fun(T) ->
            {ok, _, [{N}]} = epgsql:equery(
                Conn, "SELECT count(*)::bigint FROM " ++ binary_to_list(T), []
            ),
            ?assertEqual(0, N, {unexpected_default_rows, T})
        end,
        [
            <<"personal_history_authorization">>,
            <<"personal_history_archive">>,
            <<"personal_history_cursor">>,
            <<"personal_history_key_package">>
        ]
    ),
    %% 既有企业与队列表结构未被本域改动（expand-only 证明）
    {ok, _, [{DeliveryCols}]} = epgsql:equery(
        Conn,
        "SELECT count(*)::bigint FROM information_schema.columns "
        "WHERE table_schema='public' AND table_name='msg_delivery'",
        []
    ),
    ?assertEqual(5, DeliveryCols),
    {ok, _, [{EbCols}]} = epgsql:equery(
        Conn,
        "SELECT count(*)::bigint FROM information_schema.columns "
        "WHERE table_schema='public' AND table_name='enterprise_message_delivery'",
        []
    ),
    ?assert(EbCols > 0),
    {comment, <<"defaults OFF; existing domains untouched">>}.

constraint_guards(#{conn := Conn} = _S) ->
    %% 授权：插入 + generation CAS（成功/过期）+ 状态机 CHECK + 唯一
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO personal_history_authorization "
        "(account_id, domain, state, generation, client_instance_id) "
        "VALUES ($1,$2,'ON',$3,$4)",
        [9001, <<"d1">>, 5, <<"inst-a">>]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE personal_history_authorization "
        "SET state='OFF_PURGE_PENDING', generation=6, purge_started_at=CURRENT_TIMESTAMP "
        "WHERE account_id=$1 AND domain=$2 AND generation=$3",
        [9001, <<"d1">>, 5]
    ),
    {ok, 0} = epgsql:equery(
        Conn,
        "UPDATE personal_history_authorization SET state='OFF', generation=7 "
        "WHERE account_id=$1 AND domain=$2 AND generation=$3",
        [9001, <<"d1">>, 5]
    ),
    expect_pg_error(
        Conn,
        "UPDATE personal_history_authorization SET state='BOGUS' "
        "WHERE account_id=$1 AND domain=$2",
        [9001, <<"d1">>],
        <<"23514">>
    ),
    expect_pg_error(
        Conn,
        "INSERT INTO personal_history_authorization "
        "(account_id, domain, state, generation) VALUES ($1,$2,'ON',$3)",
        [9001, <<"d1">>, 9],
        <<"23505">>
    ),
    %% 归档：幂等键 + nonce 守卫
    InsArc =
        "INSERT INTO personal_history_archive "
        "(account_id, domain, conversation_id, msg_id, action_version, conv_seq, "
        " generation, key_version, nonce, ciphertext, envelope_meta) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)",
    Env = #{<<"alg">> => <<"AES-256-GCM">>, <<"aad">> => <<"canonical-bound">>},
    {ok, 1} = epgsql:equery(
        Conn,
        InsArc,
        [
            9001,
            <<"d1">>,
            <<"conv-1">>,
            <<"m1">>,
            0,
            100,
            5,
            1,
            <<0, 1, 2, 3>>,
            <<192, 4, 5, 6>>,
            json:encode(Env)
        ]
    ),
    expect_pg_error(
        Conn,
        InsArc,
        [
            9001,
            <<"d1">>,
            <<"conv-1">>,
            <<"m1">>,
            0,
            100,
            5,
            1,
            <<9, 9, 9, 9>>,
            <<1, 1, 1, 1>>,
            json:encode(Env)
        ],
        <<"23505">>
    ),
    expect_pg_error(
        Conn,
        InsArc,
        [
            9001,
            <<"d1">>,
            <<"conv-1">>,
            <<"m2">>,
            0,
            101,
            5,
            1,
            <<0, 1, 2, 3>>,
            <<7, 7, 7, 7>>,
            json:encode(Env)
        ],
        <<"23505">>
    ),
    %% 游标：ack 水位不得超前发放
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO personal_history_cursor "
        "(account_id, domain, generation, client_instance, change_cursor, acked_cursor) "
        "VALUES ($1,$2,$3,$4,$5,$6)",
        [9001, <<"d1">>, 5, <<"inst-a">>, 10, 10]
    ),
    expect_pg_error(
        Conn,
        "UPDATE personal_history_cursor SET acked_cursor=$1 "
        "WHERE account_id=$2 AND domain=$3",
        [11, 9001, <<"d1">>],
        <<"23514">>
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE personal_history_cursor SET change_cursor=12, acked_cursor=12 "
        "WHERE account_id=$1 AND domain=$2",
        [9001, <<"d1">>]
    ),
    %% 密钥包：版本唯一
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO personal_history_key_package "
        "(account_id, domain, key_version, generation, wrapped_key, kdf_meta, integrity_hash) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7)",
        [
            9001,
            <<"d1">>,
            1,
            5,
            <<1, 2, 3>>,
            json:encode(#{
                <<"alg">> => <<"HKDF-SHA256">>,
                <<"info">> => <<"imboy/personal-history/v1">>,
                <<"iterations">> => 310000
            }),
            <<"sha256-deadbeef">>
        ]
    ),
    expect_pg_error(
        Conn,
        "INSERT INTO personal_history_key_package "
        "(account_id, domain, key_version, generation, wrapped_key, kdf_meta, integrity_hash) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7)",
        [9001, <<"d1">>, 1, 5, <<9>>, json:encode(#{}), <<"x">>],
        <<"23505">>
    ),
    {comment, <<"CAS/unique/CHECK/watermark guards verified">>}.

%% ---------------------------------------------------------------- rollback rehearsal

rollback_rehearsal(#{conn := Conn} = S) ->
    %% 停在 275：个人域四表不存在
    ?assertEqual(275, current_version(Conn)),
    lists:foreach(
        fun(T) -> ?assertNot(table_exists(Conn, T), {unexpected_table, T}) end,
        [
            <<"personal_history_authorization">>,
            <<"personal_history_archive">>,
            <<"personal_history_cursor">>,
            <<"personal_history_key_package">>
        ]
    ),
    %% goto 279：四表就位
    case erlang_migrate:goto(mig_conf(S), 279) of
        ok -> ok;
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({goto_279_failed, Reason})
    end,
    ?assertEqual(279, current_version(Conn)),
    lists:foreach(
        fun(T) -> ?assert(table_exists(Conn, T), {missing_after_up, T}) end,
        [
            <<"personal_history_authorization">>,
            <<"personal_history_archive">>,
            <<"personal_history_cursor">>,
            <<"personal_history_key_package">>
        ]
    ),
    %% down 4：真实回退 279→275；既有域表不受回退影响
    case erlang_migrate:down(mig_conf(S), 4) of
        ok -> ok;
        {ok, _} -> ok;
        {error, Reason2} -> erlang:error({down_failed, Reason2})
    end,
    ?assertEqual(275, current_version(Conn)),
    lists:foreach(
        fun(T) -> ?assertNot(table_exists(Conn, T), {table_survived_down, T}) end,
        [
            <<"personal_history_authorization">>,
            <<"personal_history_archive">>,
            <<"personal_history_cursor">>,
            <<"personal_history_key_package">>
        ]
    ),
    ?assert(table_exists(Conn, <<"msg_delivery">>)),
    ?assert(table_exists(Conn, <<"enterprise_message_delivery">>)),
    %% 再 up：恢复到 279（协调者修复：up 会迁到 head，head 已推进到 280/281，
    %% 演练意图是回到本卡末端 279，改用显式 goto 锚定）
    case erlang_migrate:goto(mig_conf(S), 279) of
        ok -> ok;
        {ok, _} -> ok;
        {error, Reason3} -> erlang:error({up_after_down_failed, Reason3})
    end,
    ?assertEqual(279, current_version(Conn)),
    ?assert(table_exists(Conn, <<"personal_history_authorization">>)),
    {comment, <<"up→down→up rehearsal on real PG verified">>}.
