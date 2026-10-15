-module(attach_pending_cleanup_tests).
-include_lib("eunit/include/eunit.hrl").

%% Unit orchestration only; PostgreSQL durability/locks and object bytes require
%% the separate integration suite. No application boot or external storage here.
%%
%% W4-01 入口收口（02 §4）：pending/orphan 清理只发删除意图（迁移 179/184），
%% 不删字节、不删行——elib_oss 零调用；资格由 fn 锁后复核（单元层以
%% intent_conflict/tombstone/not_found 模拟，按 skipped 裁决）。
cleanup_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun pending_intent_queued/0,
        fun pending_intent_conflict_skipped/0,
        fun pending_intent_tombstone_skipped/0,
        fun pending_intent_not_found_skipped/0,
        fun pending_intent_error_aborts/0,
        fun pending_list_error/0,
        fun orphan_intent_queued/0,
        fun orphan_conflict_skipped/0,
        fun orphan_missing_path_skipped/0,
        fun orphan_intent_error_aborts/0,
        fun zero_object_deletes/0
    ]}.

setup() ->
    Modules = [
        attach_pending_repo,
        attachment_repo,
        elib_pg,
        %% 仅用于「零删字节」断言（called 检查）；编排本身不触存储
        elib_oss
    ],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    meck:expect(elib_pg, with_tx, fun transaction/1),
    meck:expect(attach_pending_repo, list_expired, fun(_) -> {ok, []} end),
    meck:expect(attachment_repo, orphan_list_for_delete, fun(_) -> {ok, []} end),
    Modules.

cleanup(Modules) -> lists:foreach(fun meck:unload/1, Modules).

transaction(Fun) ->
    try
        Fun(test_connection)
    catch
        throw:{abort_tx, Reason} -> {error, Reason}
    end.

pending_row(Key) -> #{<<"object_key">> => Key}.
orphan_row(Id, Path) -> #{<<"id">> => Id, <<"path">> => Path}.

expired(Key) ->
    meck:expect(attach_pending_repo, list_expired, fun(24) -> {ok, [pending_row(Key)]} end).

orphan(Id, Path) ->
    meck:expect(
        attachment_repo, orphan_list_for_delete, fun(_) -> {ok, [orphan_row(Id, Path)]} end
    ).

expect_intent(Key, Result) ->
    meck:expect(
        attach_pending_repo,
        intent_pending_delete_tx,
        fun(_Conn, K, <<"sweep">>, _) when K =:= Key -> Result end
    ).

expect_orphan_intent(Path, Result) ->
    meck:expect(
        attachment_repo,
        orphan_path_tx,
        fun(_Conn, _Id, _Opts) -> {ok, [#{<<"path">> => Path}]} end
    ),
    meck:expect(
        attachment_repo,
        intent_ordinary_delete_tx,
        fun(_Conn, P, _) when P =:= Path -> Result end
    ).

pending_counts(Cleaned, Errors) ->
    ?assertEqual(
        {ok, #{cleaned => Cleaned, errors => Errors}},
        attachment_ds:pending_cleanup(24)
    ).

orphan_counts(Cleaned, Errors) ->
    ?assertEqual(
        {ok, #{cleaned => Cleaned, errors => Errors}},
        attachment_ds:orphan_cleanup(#{age_days => 30})
    ).

%% 意图成功：cleaned 计数（cleaned 语义 = 已发删除意图数）
pending_intent_queued() ->
    expired(<<"key-a">>),
    expect_intent(<<"key-a">>, ok),
    pending_counts(1, 0),
    ?assert(
        meck:called(
            attach_pending_repo, intent_pending_delete_tx, ['_', <<"key-a">>, <<"sweep">>, '_']
        )
    ).

%% 资格不满足（gc/能力窗等，fn 23510）：保守保留，skipped 不计错误
pending_intent_conflict_skipped() ->
    expired(<<"key-b">>),
    expect_intent(<<"key-b">>, {error, intent_conflict}),
    pending_counts(0, 0).

%% 已在队列（tombstone）：skipped
pending_intent_tombstone_skipped() ->
    expired(<<"key-c">>),
    expect_intent(<<"key-c">>, {error, tombstone}),
    pending_counts(0, 0).

%% 无行（并发已清）：skipped
pending_intent_not_found_skipped() ->
    expired(<<"key-d">>),
    expect_intent(<<"key-d">>, {error, not_found}),
    pending_counts(0, 0).

%% 其他错误：整体回滚按 error 计数，绝不回退到直接删字节
pending_intent_error_aborts() ->
    expired(<<"key-e">>),
    expect_intent(<<"key-e">>, {error, {db, unavailable}}),
    pending_counts(0, 1),
    ?assertNot(meck:called(elib_oss, delete_object, ['_', '_'])).

pending_list_error() ->
    meck:expect(attach_pending_repo, list_expired, fun(_) -> {error, down} end),
    ?assertEqual({error, down}, attachment_ds:pending_cleanup(24)).

%% orphan：意图成功 → cleaned
orphan_intent_queued() ->
    orphan(9901, <<"v5e2e/orphan-a">>),
    expect_orphan_intent(<<"v5e2e/orphan-a">>, ok),
    orphan_counts(1, 0).

%% orphan：资格不满足 → skipped（保守保留，不计错误）
orphan_conflict_skipped() ->
    orphan(9902, <<"v5e2e/orphan-b">>),
    expect_orphan_intent(<<"v5e2e/orphan-b">>, {error, intent_conflict}),
    orphan_counts(0, 0).

%% orphan：行已被并发清理（path 查不到）→ skipped
orphan_missing_path_skipped() ->
    orphan(9903, <<"v5e2e/orphan-c">>),
    meck:expect(attachment_repo, orphan_path_tx, fun(_C, _Id, _O) -> {ok, []} end),
    orphan_counts(0, 0).

%% orphan：其他错误 → error 计数
orphan_intent_error_aborts() ->
    orphan(9904, <<"v5e2e/orphan-d">>),
    expect_orphan_intent(<<"v5e2e/orphan-d">>, {error, {db, down}}),
    orphan_counts(0, 1).

%% 清理编排零对象存储触达（物理删除只允许 driver/worker）
zero_object_deletes() ->
    expired(<<"key-z">>),
    expect_intent(<<"key-z">>, ok),
    orphan(9905, <<"v5e2e/orphan-z">>),
    expect_orphan_intent(<<"v5e2e/orphan-z">>, ok),
    pending_counts(1, 0),
    orphan_counts(1, 0),
    ?assertNot(meck:called(elib_oss, delete_object, ['_', '_'])).
