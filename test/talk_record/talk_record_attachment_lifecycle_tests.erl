-module(talk_record_attachment_lifecycle_tests).
%%%
% T06 隔离验收（专用 PG 127.0.0.1:55434/talk_record_t06；schema 由
% runners/T06/t06_attachment_lifecycle_check.sh 提供：fixture 基底 +
% 真实迁移 00000211 + 00000212 up）。T06_PG_* 缺失时整套 skip（非 T06
% 环境不应假绿）；runner gate 按精确通过数断言，空转不算绿。
%
% 矩阵（T06 卡交付 7）：
%   A 首绑/释放幂等（含 100 次 confirm 失联重试重放）
%   B 墓碑/删除意图存在时绑定拒绝
%   C 并发首绑与 GC 竞争（两连接两事务一绑一清，双向顺序，锁序不误删）
%   D 跨 record 复用 attachment_id / 跨 scope 拼接拒绝
%   E 其他业务引用存在时不误删（retained_by_other_reference）
%   F purpose 授权链（记录 ACL → 附件授权；上传权；企业组织归属）
%   G processing lease（有效挡 GC/到期失效/取消/只延长）
%   H 企业治理映射（retention 阻断释放；EAR hold defense-in-depth）
%   I Disk 驱动路径（intent→claim→driver DELETE→ack→tombstone）
%   J 孤儿清理谓词（bound 引用挡 orphan；释放后按龄可回收）
%%%

-include_lib("eunit/include/eunit.hrl").

-define(F, talk_record_t06_fixture).

%% ===================================================================
%% 生成器
%% ===================================================================

talk_record_t06_test_() ->
    case ?F:env_ready() of
        true ->
            {setup, fun setup/0, fun cleanup/1, fun tests/1};
        false ->
            {skip, "T06 isolated PG env (T06_PG_*) not set"}
    end.

setup() ->
    _ = application:ensure_all_started(crypto),
    _ = application:ensure_all_started(jsone),
    try
        elib_tsid:init(#{dc_id => 1, node_id => 61})
    catch
        _:{elib_tsid_already_initialized, _} -> ok
    end,
    ?F:reset_business_data(),
    #{conn => ?F:connect()}.

cleanup(#{conn := Conn}) ->
    ?F:close(Conn).

tests(#{conn := Conn}) ->
    [
        %% A 幂等
        {timeout, 60, ?_test(t_bind_first_and_replay_idempotent(Conn))},
        {timeout, 60, ?_test(t_confirm_lost_retry_100_replays(Conn))},
        {timeout, 60, ?_test(t_release_idempotent(Conn))},
        {timeout, 60, ?_test(t_same_business_key_different_asset_conflict(Conn))},
        {timeout, 60, ?_test(t_released_key_never_rebinds(Conn))},
        %% B 墓碑
        {timeout, 60, ?_test(t_bind_rejected_on_tombstone(Conn))},
        {timeout, 60, ?_test(t_bind_rejected_when_delete_intent_job_exists(Conn))},
        {timeout, 60, ?_test(t_bind_explicit_confirm_missing_branch(Conn))},
        %% C 并发
        {timeout, 120, ?_test(t_race_bind_wins_then_intent_rejected(Conn))},
        {timeout, 120, ?_test(t_race_intent_wins_then_bind_tombstoned(Conn))},
        {timeout, 120, ?_test(t_cleanup_waits_for_reference_lock(Conn))},
        {timeout, 120, ?_test(t_retry_waits_for_identity_lock(Conn))},
        {timeout, 120, ?_test(t_concurrent_multi_asset_cleanup(Conn))},
        %% D 拼接
        {timeout, 60, ?_test(t_cross_record_attachment_rejected(Conn))},
        {timeout, 60, ?_test(t_cross_scope_attachment_rejected(Conn))},
        {timeout, 60, ?_test(t_session_actor_mismatch_rejected(Conn))},
        {timeout, 60, ?_test(t_capture_mismatch_rejected(Conn))},
        %% E 其他引用
        {timeout, 60, ?_test(t_retained_by_other_reference_collect(Conn))},
        %% F 授权链
        {timeout, 60, ?_test(t_authorize_record_acl_chain(Conn))},
        {timeout, 60, ?_test(t_can_upload_owner_and_org_governance(Conn))},
        %% G lease
        {timeout, 60, ?_test(t_processing_lease_blocks_gc_until_expiry(Conn))},
        {timeout, 60, ?_test(t_processing_lease_cancel_and_sweep(Conn))},
        {timeout, 60, ?_test(t_processing_lease_extend_only(Conn))},
        %% H 治理
        {timeout, 60, ?_test(t_governance_retention_blocks_release(Conn))},
        {timeout, 60, ?_test(t_governance_ear_hold_defense(Conn))},
        {timeout, 60, ?_test(t_cleanup_maps_states_and_scans_org(Conn))},
        %% I Disk 驱动
        {timeout, 120, ?_test(t_disk_driver_full_delete_path(Conn))},
        %% J 孤儿谓词
        {timeout, 60, ?_test(t_orphan_predicate_respects_bound_reference(Conn))}
    ].

%% ===================================================================
%% A 幂等
%% ===================================================================

t_bind_first_and_replay_idempotent(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5001, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5001),
    ?assertMatch(
        {ok, _, true},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3001, 5001)
        end)
    ),
    %% 同 part 重试幂等（First=false）
    ?assertMatch(
        {ok, _, false},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3001, 5001)
        end)
    ),
    Count = count_refs(Conn, 1001),
    ?assertEqual(1, Count).

t_confirm_lost_retry_100_replays(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5002, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5002),
    lists:foreach(
        fun(I) ->
            {ok, _, _} =
                ?F:tx(Conn, fun(_) ->
                    talk_record_attachment_adapter:bind_audio_part_tx(
                        Conn, 1, 1001, 2001, 3002, 5002
                    )
                end),
            I
        end,
        lists:seq(1, 100)
    ),
    ?assertEqual(1, count_refs(Conn, 1001)),
    %% 首绑后 gc_verified 刷新必须识别引用（proven=false）
    {ok, false} = attachment_repo:refresh_gc_verified_tx(Conn, Path).

t_release_idempotent(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5003, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5003),
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3003, 5003)
        end),
    ?assertEqual(
        {ok, released},
        talk_record_attachment_adapter:release_tx(
            Conn, 1001, <<"audio_part">>, <<"audio_part:3003">>
        )
    ),
    ?assertEqual(
        {ok, already_released},
        talk_record_attachment_adapter:release_tx(
            Conn, 1001, <<"audio_part">>, <<"audio_part:3003">>
        )
    ),
    %% 释放后（无其他引用、明文、位置完整）proven 翻 true
    ok = fill_storage_location(Conn, 5003),
    {ok, true} = attachment_repo:refresh_gc_verified_tx(Conn, Path).

t_same_business_key_different_asset_conflict(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := P1} = ?F:insert_attachment(Conn, #{id => 5101, record_id => 1001}),
    #{path := P2} = ?F:insert_attachment(Conn, #{id => 5102, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, P1, 5101),
    ?F:insert_pending_confirmed(Conn, P2, 5102),
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3101, 5101)
        end),
    %% 同 business_key（part 3101）换资产 → 真实冲突，不是幂等
    ?assertEqual(
        {error, business_key_conflict},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3101, 5102)
        end)
    ).

t_released_key_never_rebinds(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5103, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5103),
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3103, 5103)
        end),
    {ok, released} =
        talk_record_attachment_adapter:release_tx(
            Conn, 1001, <<"audio_part">>, <<"audio_part:3103">>
        ),
    %% released 后同 key 同资产不可复活（fail-closed）
    ?assertEqual(
        {error, already_released},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3103, 5103)
        end)
    ).

%% ===================================================================
%% B 墓碑
%% ===================================================================

t_bind_rejected_on_tombstone(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5201, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5201),
    {ok, []} = ?F:q(
        Conn, <<"UPDATE attachment SET lifecycle_state='delete_intent' WHERE id=5201">>, []
    ),
    ?assertEqual(
        {error, tombstone},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3201, 5201)
        end)
    ),
    ?assertEqual(0, count_refs(Conn, 1001)).

t_bind_rejected_when_delete_intent_job_exists(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5202, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5202),
    %% 完整 intent 路径（gc_verified=true 且无引用）→ 墓碑 + 队列
    ok = fill_storage_location(Conn, 5202),
    {ok, true} = attachment_repo:refresh_gc_verified_tx(Conn, Path),
    ?assertEqual(
        ok,
        ?F:tx(Conn, fun(C) ->
            attachment_repo:intent_ordinary_delete_tx(C, Path, <<"t06">>)
        end)
    ),
    ?assertEqual(
        {error, tombstone},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3202, 5202)
        end)
    ),
    ?assertEqual(0, count_refs(Conn, 1001)).

t_bind_explicit_confirm_missing_branch(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    %% 无资产行（伪造 attachment_id）：bind_reference_lock_tx 走"无元数据行仅取
    %% 对象锁"分支返回 ok——repo 必须显式拒绝，不得静默跳过。
    ?assertEqual(
        {error, attachment_not_found},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_artifact_tx(
                Conn, 1001, 999999, <<"manifest">>, <<"m1">>
            )
        end)
    ).

%% ===================================================================
%% C 并发（两连接两事务；AC04 双 PG 连接屏障口径）
%% ===================================================================

%% 顺序 1：bind 先提交 → intent 锁后复核 23510 拒绝（字节保留）
t_race_bind_wins_then_intent_rejected(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5301, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5301),
    ok = fill_storage_location(Conn, 5301),
    %% 预置 gc_verified=true（陈旧标志：绑定尚未发生）
    {ok, true} = attachment_repo:refresh_gc_verified_tx(Conn, Path),
    C1 = ?F:connect(),
    C2 = ?F:connect(),
    try
        Binder = self(),
        P1 = spawn_link(fun() ->
            ?F:tx(C1, fun(Conn1) ->
                Binder !
                    {bound,
                        talk_record_attachment_adapter:bind_audio_part_tx(
                            Conn1, 1, 1001, 2001, 3301, 5301
                        )}
            end),
            Binder ! p1_done
        end),
        receive
            {bound, {ok, _, true}} -> ok
        after 10000 -> error(bind_timeout)
        end,
        %% P1 已持有 attachment 行锁 + advisory；intent 在另一连接上必须被行锁
        %% 阻塞到 P1 COMMIT，随后锁后复核看到 talk_record 引用 → 23510。
        IntentResult = collect_intent_in_tx(C2, Path),
        receive
            p1_done -> ok
        after 10000 -> error(p1_done_timeout)
        end,
        ?assertEqual({error, intent_conflict}, IntentResult),
        %% 资产仍 live、引用仍 bound（未误删）
        ?assertEqual(<<"live">>, lifecycle_of(Conn, Path)),
        ?assertEqual(1, count_refs(Conn, 1001)),
        unlink(P1)
    after
        ?F:close(C1),
        ?F:close(C2)
    end.

%% 顺序 2：intent 先提交 → bind 墓碑拒绝（零业务行变更）
t_race_intent_wins_then_bind_tombstoned(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5302, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5302),
    ok = fill_storage_location(Conn, 5302),
    {ok, true} = attachment_repo:refresh_gc_verified_tx(Conn, Path),
    C1 = ?F:connect(),
    C2 = ?F:connect(),
    try
        Runner = self(),
        P1 = spawn_link(fun() ->
            ?F:tx(C1, fun(Conn1) ->
                Runner !
                    {intent,
                        attachment_repo:intent_ordinary_delete_tx(
                            Conn1, Path, <<"t06-race">>
                        )}
            end),
            Runner ! p1_done
        end),
        receive
            {intent, ok} -> ok
        after 10000 -> error(intent_timeout)
        end,
        %% P1 持有 attachment 行 FOR UPDATE（delete_intent 未提交）；bind 的
        %% bind_reference_lock_tx SELECT FOR UPDATE 阻塞至 P1 COMMIT → 墓碑拒绝。
        BindResult =
            ?F:tx(C2, fun(Conn2) ->
                talk_record_attachment_adapter:bind_audio_part_tx(
                    Conn2, 1, 1001, 2001, 3302, 5302
                )
            end),
        receive
            p1_done -> ok
        after 10000 -> error(p1_done_timeout)
        end,
        ?assertEqual({error, tombstone}, BindResult),
        ?assertEqual(0, count_refs(Conn, 1001)),
        ?assertEqual(<<"delete_intent">>, lifecycle_of(Conn, Path)),
        unlink(P1)
    after
        ?F:close(C1),
        ?F:close(C2)
    end.

collect_intent_in_tx(C2, Path) ->
    %% intent 在独立事务中执行：会阻塞在 C1 的行锁上；epgsql 同步查询本身
    %% 就是屏障——直接串行执行（C1 已在收到 {bound,...} 后由 spawn 进程尽快
    %% COMMIT，行锁释放后本查询继续）。
    ?F:tx(C2, fun(Conn2) ->
        attachment_repo:intent_ordinary_delete_tx(Conn2, Path, <<"t06-race">>)
    end).

%% ===================================================================
%% D 拼接拒绝
%% ===================================================================

t_cross_record_attachment_rejected(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    %% 资产登记归属 record 1001，尝试绑给 record 1002
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5401, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5401),
    %% record 1002 需要自己的 capture
    {ok, []} = ?F:q(
        Conn,
        <<
            "INSERT INTO talk_record_capture(id, record_id, device_session_id, device_id,"
            " started_by, status) VALUES (2002, 1002,"
            " '00000000-0000-0000-0000-0000000000bb', 'dev2', 1, 'recording')"
        >>,
        []
    ),
    ?assertEqual(
        {error, scope_mismatch},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1002, 2002, 3401, 5401)
        end)
    ).

t_cross_scope_attachment_rejected(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    %% group 域附件伪装 talk_record 绑定
    {ok, []} = ?F:q(
        Conn,
        <<
            "INSERT INTO attachment(id, file_hash256, mime_type, name, path, url, size,"
            " referer_time, creator_user_id, scope, scope_ref)"
            " VALUES (5402, repeat('b',64), 'audio/aac', 'x', 'u1/g1', 'u1/g1', 10, 1, 1,"
            " 'group', '777')"
        >>,
        []
    ),
    ?assertEqual(
        {error, scope_mismatch},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_artifact_tx(
                Conn, 1001, 5402, <<"manifest">>, <<"m">>
            )
        end)
    ).

t_session_actor_mismatch_rejected(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{
        id => 5403, record_id => 1001, creator_user_id => 3
    }),
    %% pending 会话登记主体是 stranger(3)，不是 owner(1)
    ok = ?F:insert_pending_confirmed(Conn, Path, 5403, 3),
    ?assertEqual(
        {error, session_mismatch},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3403, 5403)
        end)
    ).

t_capture_mismatch_rejected(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5404, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5404),
    %% capture 2001 属 record 1001 但 started_by=1；用 stranger(3) 发起
    ?assertEqual(
        {error, capture_actor_mismatch},
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 3, 1001, 2001, 3404, 5404)
        end)
    ).

%% ===================================================================
%% E 其他引用保留
%% ===================================================================

t_cleanup_waits_for_reference_lock(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5502, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5502),
    ok = fill_storage_location(Conn, 5502),
    {ok, _, true} = ?F:tx(Conn, fun(_) ->
        talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3502, 5502)
    end),
    Other = ?F:connect(),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        {ok, _, _} = epgsql:equery(Other, "SET statement_timeout='10s'", []),
        {ok, _, _} = epgsql:equery(Conn, "BEGIN", []),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT pg_advisory_xact_lock(hashtextextended($1,168))",
            [Path]
        ),
        Parent = self(),
        spawn(fun() ->
            Result =
                try
                    ?F:tx(Other, fun(C) ->
                        talk_record_attachment_adapter:request_record_cleanup_tx(C, 1001)
                    end)
                catch
                    Class:Reason -> {Class, Reason}
                end,
            Parent ! {cleanup_result, Result}
        end),
        wait_cleanup_lock(Conn, Pid, 100),
        ?assertEqual(ok, attachment_repo:bind_reference_lock_tx(Conn, Path)),
        {ok, _, _} = epgsql:equery(Conn, "COMMIT", []),
        receive
            {cleanup_result, {ok, #{outcomes := [#{outcome := intended}]}}} -> ok;
            {cleanup_result, Unexpected} -> error({unexpected_cleanup_result, Unexpected})
        after 12000 -> error(cleanup_timeout)
        end
    after
        try
            epgsql:equery(Conn, "ROLLBACK", [])
        catch
            _:_ -> ok
        end,
        ?F:close(Other)
    end.

t_retry_waits_for_identity_lock(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5503, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5503),
    ok = fill_storage_location(Conn, 5503),
    {ok, _, true} = ?F:tx(Conn, fun(_) ->
        talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3503, 5503)
    end),
    {ok, 1, _, [{Revision, Generation}]} = epgsql:equery(
        Conn,
        "UPDATE talk_record SET deleted_at=now(),cleanup_status='failed' WHERE id=1001"
        " RETURNING revision,cleanup_generation",
        []
    ),
    Other = ?F:connect(),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        {ok, _, _} = epgsql:equery(Other, "SET statement_timeout='10s'", []),
        {ok, _, _} = epgsql:equery(Conn, "BEGIN", []),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT fn_v5_lock_identity(storage_driver,storage_namespace,storage_container,path)"
            " FROM attachment WHERE path=$1",
            [Path]
        ),
        Parent = self(),
        spawn(fun() ->
            Parent !
                {retry_result,
                    epgsql:equery(
                        Other,
                        "SELECT fn_talk_record_cleanup_retry(1,1001,$1,$2,'lock-test',$3,'test',900001)",
                        [Revision, Generation, binary:copy(<<"a">>, 64)]
                    )}
        end),
        wait_cleanup_lock(Conn, Pid, 100),
        {ok, _, [_]} = epgsql:equery(Conn, "SELECT id FROM attachment WHERE path=$1 FOR UPDATE", [
            Path
        ]),
        {ok, _, _} = epgsql:equery(Conn, "COMMIT", []),
        receive
            {retry_result, {ok, _, [{Result}]}} ->
                ?assertNotEqual(nomatch, binary:match(Result, <<"cleanup_protected">>));
            {retry_result, Unexpected} ->
                error({unexpected_retry_result, Unexpected})
        after 12000 -> error(retry_timeout)
        end
    after
        try
            epgsql:equery(Conn, "ROLLBACK", [])
        catch
            _:_ -> ok
        end,
        ?F:close(Other)
    end.

t_concurrent_multi_asset_cleanup(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    Paths = lists:map(
        fun({AttachmentId, PartId}) ->
            #{path := Path} = ?F:insert_attachment(Conn, #{id => AttachmentId, record_id => 1001}),
            ?F:insert_pending_confirmed(Conn, Path, AttachmentId),
            ok = fill_storage_location(Conn, AttachmentId),
            {ok, _, true} = ?F:tx(Conn, fun(_) ->
                talk_record_attachment_adapter:bind_audio_part_tx(
                    Conn, 1, 1001, 2001, PartId, AttachmentId
                )
            end),
            Path
        end,
        [{5504, 3504}, {5505, 3505}]
    ),
    Other = ?F:connect(),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        {ok, _, _} = epgsql:equery(Other, "SET statement_timeout='10s'", []),
        {ok, _, _} = epgsql:equery(Conn, "BEGIN", []),
        {ok, #{released := 2, outcomes := First}} =
            talk_record_attachment_adapter:request_record_cleanup_tx(Conn, 1001),
        ?assertEqual([intended, intended], lists:sort([maps:get(outcome, X) || X <- First])),
        Parent = self(),
        spawn(fun() ->
            Result =
                try
                    ?F:tx(Other, fun(C) ->
                        talk_record_attachment_adapter:request_record_cleanup_tx(C, 1001)
                    end)
                catch
                    Class:Reason -> {Class, Reason}
                end,
            Parent ! {multi_cleanup_result, Result}
        end),
        wait_cleanup_lock(Conn, Pid, 100),
        {ok, _, _} = epgsql:equery(Conn, "COMMIT", []),
        receive
            {multi_cleanup_result, {ok, #{released := 0, outcomes := Second}}} ->
                ?assertEqual(
                    [tombstone, tombstone],
                    lists:sort([maps:get(outcome, X) || X <- Second])
                );
            {multi_cleanup_result, Unexpected} ->
                error({unexpected_multi_cleanup, Unexpected})
        after 12000 -> error(multi_cleanup_timeout)
        end,
        lists:foreach(
            fun(Path) ->
                {ok, _, [{1}]} = epgsql:equery(
                    Conn,
                    "SELECT count(*) FROM attachment_object_deletion WHERE path=$1",
                    [Path]
                )
            end,
            Paths
        )
    after
        try
            epgsql:equery(Conn, "ROLLBACK", [])
        catch
            _:_ -> ok
        end,
        ?F:close(Other)
    end.

wait_cleanup_lock(_Conn, _Pid, 0) ->
    error(cleanup_did_not_wait_for_lock);
wait_cleanup_lock(Conn, Pid, Left) ->
    {ok, _, [{Waiting}]} = epgsql:equery(
        Conn,
        "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE pid=$1 AND wait_event_type='Lock')",
        [Pid]
    ),
    case Waiting of
        true ->
            ok;
        false ->
            timer:sleep(50),
            wait_cleanup_lock(Conn, Pid, Left - 1)
    end.

t_retained_by_other_reference_collect(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(
        Conn, #{id => 5501, record_id => 1001, hash => binary:copy(<<"c">>, 64)}
    ),
    ?F:insert_pending_confirmed(Conn, Path, 5501),
    ok = fill_storage_location(Conn, 5501),
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3501, 5501)
        end),
    %% 收藏引用（同 hash、status=1）
    {ok, []} = ?F:q(
        Conn,
        <<
            "INSERT INTO user_collect(id, user_id, attach_file_hash256, status)"
            " VALUES (1, 2, $1, 1)"
        >>,
        [binary:copy(<<"c">>, 64)]
    ),
    {ok, #{outcomes := [#{outcome := Outcome}]}} =
        ?F:tx(Conn, fun(C) ->
            talk_record_attachment_adapter:request_record_cleanup_tx(C, 1001)
        end),
    ?assertEqual(retained_by_other_reference, Outcome),
    %% 资产仍 live（字节保留），引用已按本域释放
    ?assertEqual(<<"live">>, lifecycle_of(Conn, Path)),
    ?assertEqual(0, count_refs(Conn, 1001)).

%% ===================================================================
%% F 授权链
%% ===================================================================

t_authorize_record_acl_chain(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5601, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5601),
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3601, 5601)
        end),
    Rec = attachment_rec(Conn, Path),
    %% owner 放行
    ?assert(talk_record_attachment_adapter:authorize_tx(Conn, 1, Rec)),
    %% 无 grant 的陌生人拒绝（参与者/成员不自动可见）
    ?assertNot(talk_record_attachment_adapter:authorize_tx(Conn, 3, Rec)),
    %% 授予 viewer → 放行
    ?F:grant_role(Conn, 1001, 2, <<"viewer">>),
    Rec2 = attachment_rec(Conn, Path),
    ?assert(talk_record_attachment_adapter:authorize_tx(Conn, 2, Rec2)),
    %% 撤销 → 立即失效
    {ok, []} = ?F:q(
        Conn, <<"UPDATE talk_record_grant SET revoked_at=now() WHERE grantee_user_id=2">>, []
    ),
    Rec3 = attachment_rec(Conn, Path),
    ?assertNot(talk_record_attachment_adapter:authorize_tx(Conn, 2, Rec3)),
    %% 记录软删 → owner 也不可读（删除立即撤权）
    {ok, []} = ?F:q(Conn, <<"UPDATE talk_record SET deleted_at=now() WHERE id=1001">>, []),
    Rec4 = attachment_rec(Conn, Path),
    ?assertNot(talk_record_attachment_adapter:authorize_tx(Conn, 1, Rec4)),
    %% 引用释放后（记录恢复可见）伪造 scope_ref 仍无授权
    {ok, []} = ?F:q(Conn, <<"UPDATE talk_record SET deleted_at=NULL WHERE id=1001">>, []),
    {ok, released} = talk_record_attachment_adapter:release_tx(
        Conn, 1001, <<"audio_part">>, <<"audio_part:3601">>
    ),
    Rec5 = attachment_rec(Conn, Path),
    ?assertNot(talk_record_attachment_adapter:authorize_tx(Conn, 1, Rec5)).

t_can_upload_owner_and_org_governance(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    %% personal 记录：owner 放行、陌生人拒绝
    ?assertEqual(ok, talk_record_attachment_adapter:can_upload_tx(Conn, 1, <<"1001">>)),
    ?assertEqual(
        {error, forbidden}, talk_record_attachment_adapter:can_upload_tx(Conn, 3, <<"1001">>)
    ),
    %% 企业记录（org 10 active + owner 活跃成员）→ 放行
    ?assertEqual(ok, talk_record_attachment_adapter:can_upload_tx(Conn, 1, <<"1002">>)),
    %% org 20 suspended 上的记录 → 拒绝（组织失活不能上传）
    {ok, []} = ?F:q(
        Conn,
        <<
            "INSERT INTO talk_record(id, owner_user_id, created_by, client_recording_id, scope,"
            " organization_id) VALUES (1003, 1, 1,"
            " '00000000-0000-0000-0000-000000000003', 'organization', 20)"
        >>,
        []
    ),
    ?assertEqual(
        {error, forbidden}, talk_record_attachment_adapter:can_upload_tx(Conn, 1, <<"1003">>)
    ),
    %% 未知记录
    ?assertEqual(
        {error, forbidden}, talk_record_attachment_adapter:can_upload_tx(Conn, 1, <<"424242">>)
    ).

%% ===================================================================
%% G processing lease
%% ===================================================================

t_processing_lease_blocks_gc_until_expiry(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5701, record_id => 1001}),
    ok = fill_storage_location(Conn, 5701),
    %% 处理任务对源文件持有期限引用（转码场景：源未绑 audio_part，仅 lease）
    {ok, _, true} = talk_record_attachment_adapter:grant_processing_lease_tx(
        Conn, 1001, 5701, <<"transcoded_audio">>, <<"lease-1">>, 3600
    ),
    {ok, false} = attachment_repo:refresh_gc_verified_tx(Conn, Path),
    ?assertEqual(
        {error, intent_conflict},
        ?F:tx(Conn, fun(C) ->
            attachment_repo:intent_ordinary_delete_tx(C, Path, <<"t06-lease">>)
        end)
    ),
    %% 到期（直接把 lease_until 改到过去模拟超时）→ 谓词不再挡
    {ok, []} = ?F:q(
        Conn,
        <<
            "UPDATE talk_record_attachment_ref SET lease_until=now() - interval '1 second'"
            " WHERE record_id=1001 AND business_key='lease-1'"
        >>,
        []
    ),
    {ok, true} = attachment_repo:refresh_gc_verified_tx(Conn, Path),
    ?assertEqual(
        ok,
        ?F:tx(Conn, fun(C) ->
            attachment_repo:intent_ordinary_delete_tx(C, Path, <<"t06-lease2">>)
        end)
    ).

t_processing_lease_cancel_and_sweep(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    ?F:insert_attachment(Conn, #{id => 5702, record_id => 1001}),
    {ok, _, true} = talk_record_attachment_adapter:grant_processing_lease_tx(
        Conn, 1001, 5702, <<"transcoded_audio">>, <<"lease-2">>, 3600
    ),
    %% 取消 → 立即释放
    ?assertEqual(
        {ok, released},
        talk_record_attachment_adapter:cancel_processing_lease_tx(
            Conn, 1001, <<"transcoded_audio">>, <<"lease-2">>
        )
    ),
    %% 到期清扫：另一 lease 过期后由 sweep 转 released
    ?F:insert_attachment(Conn, #{id => 5703, record_id => 1001}),
    {ok, _, true} = talk_record_attachment_adapter:grant_processing_lease_tx(
        Conn, 1001, 5703, <<"transcoded_audio">>, <<"lease-3">>, 3600
    ),
    {ok, []} = ?F:q(
        Conn,
        <<
            "UPDATE talk_record_attachment_ref SET lease_until=now() - interval '1 second'"
            " WHERE business_key='lease-3'"
        >>,
        []
    ),
    {ok, 1} = talk_record_attachment_adapter:release_expired_leases_tx(Conn),
    {ok, 0} = talk_record_attachment_adapter:release_expired_leases_tx(Conn).

t_processing_lease_extend_only(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    ?F:insert_attachment(Conn, #{id => 5704, record_id => 1001}),
    {ok, R1, true} = talk_record_attachment_adapter:grant_processing_lease_tx(
        Conn, 1001, 5704, <<"manifest">>, <<"lease-4">>, 60
    ),
    #{<<"lease_until">> := T1} = ref_row(Conn, R1),
    %% 续约 3600s：同 key 同资产 → 幂等（First=false）+ 只延长
    {ok, R1, false} = talk_record_attachment_adapter:grant_processing_lease_tx(
        Conn, 1001, 5704, <<"manifest">>, <<"lease-4">>, 3600
    ),
    #{<<"lease_until">> := T2} = ref_row(Conn, R1),
    ?assert(T2 > T1),
    %% 缩短（重放 60s 值）不得回退
    {ok, R1, false} = talk_record_attachment_adapter:grant_processing_lease_tx(
        Conn, 1001, 5704, <<"manifest">>, <<"lease-4">>, 60
    ),
    #{<<"lease_until">> := T3} = ref_row(Conn, R1),
    ?assertEqual(T2, T3).

%% ===================================================================
%% H 治理
%% ===================================================================

t_governance_retention_blocks_release(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5801, record_id => 1001}),
    ?F:insert_pending_confirmed(Conn, Path, 5801),
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3801, 5801)
        end),
    ok = fill_storage_location(Conn, 5801),
    %% 留存未到期：release 被阻断，引用保持 bound，intent 拒绝
    {ok, []} = ?F:q(
        Conn,
        <<"UPDATE talk_record SET retention_until=now() + interval '30 days' WHERE id=1001">>,
        []
    ),
    ?assertMatch(
        {ok, blocked, {blocked_retention, _}},
        talk_record_attachment_adapter:release_record_tx(Conn, 1001)
    ),
    ?assertEqual(1, count_refs(Conn, 1001)),
    {ok, #{blocked := {blocked_retention, _}}} =
        ?F:tx(Conn, fun(C) ->
            talk_record_attachment_adapter:request_record_cleanup_tx(C, 1001)
        end),
    ?assertEqual(
        {error, intent_conflict},
        ?F:tx(Conn, fun(C) ->
            attachment_repo:intent_ordinary_delete_tx(C, Path, <<"t06-retention">>)
        end)
    ),
    %% 留存到期 → 正常释放并按引用零入队
    {ok, []} = ?F:q(
        Conn,
        <<"UPDATE talk_record SET retention_until=now() - interval '1 day' WHERE id=1001">>,
        []
    ),
    {ok, #{outcomes := [#{outcome := intended}]}} =
        ?F:tx(Conn, fun(C) ->
            talk_record_attachment_adapter:request_record_cleanup_tx(C, 1001)
        end),
    ?assertEqual(<<"delete_intent">>, lifecycle_of(Conn, Path)).

t_governance_ear_hold_defense(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    {ok, []} = ?F:q(
        Conn,
        <<
            "INSERT INTO talk_record_capture(id,record_id,device_session_id,device_id,started_by,status)"
            " VALUES(2002,1002,'00000000-0000-0000-0000-0000000000cc','dev2',1,'recording')"
        >>,
        []
    ),
    #{path := Path} = ?F:insert_attachment(Conn, #{id => 5802, record_id => 1002}),
    ?F:insert_pending_confirmed(Conn, Path, 5802),
    {ok, _, true} = ?F:tx(Conn, fun(C) ->
        talk_record_attachment_adapter:bind_audio_part_tx(C, 1, 1002, 2002, 3802, 5802)
    end),
    %% 首绑真实 EAR，既有治理操作设 hold；不伪造 application 来源。
    ok = ?F:tx(Conn, fun(C) ->
        enterprise_attachment_retention_repo:hold_tx(C, 5802, <<"t06-hold">>)
    end),
    ?assertMatch(
        {ok, blocked, {blocked_hold, 1}},
        talk_record_attachment_adapter:release_record_tx(Conn, 1002)
    ),
    ?assertEqual(1, count_refs(Conn, 1002)).

t_cleanup_maps_states_and_scans_org(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    %% record 1002 的 capture 先建（capture 必须同 record）
    {ok, []} = ?F:q(
        Conn,
        <<
            "INSERT INTO talk_record_capture(id, record_id, device_session_id, device_id,"
            " started_by, status) VALUES (2002, 1002,"
            " '00000000-0000-0000-0000-0000000000cc', 'dev2', 1, 'recording')"
        >>,
        []
    ),
    #{path := P1} = ?F:insert_attachment(Conn, #{id => 5811, record_id => 1002}),
    ?F:insert_pending_confirmed(Conn, P1, 5811),
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1002, 2002, 3811, 5811)
        end),
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_artifact_tx(
                Conn, 1002, 5811, <<"manifest">>, <<"manifest:1002:v1">>
            )
        end),
    %% 治理状态快照必须声明已接入；实际绑定登记 EAR/WAR。
    {ok, Gov} = talk_record_attachment_adapter:governance_state_tx(Conn, 1002),
    ?assertEqual(10, maps:get(<<"organization_id">>, Gov)),
    ?assertEqual(true, maps:get(enterprise_governance_integrated, Gov)),
    %% 企业资源分类查询：workspace=NULL 记录不逃过组织检索
    {ok, Rows} = talk_record_attachment_ref_repo:scan_organization_tx(Conn, 10, #{}),
    ?assertMatch([#{<<"scope">> := <<"talk_record">>} | _], Rows),
    ?assertMatch([#{<<"active_roles">> := <<"audio_part,manifest">>} | _], Rows),
    %% workspace 过滤
    {ok, []} = talk_record_attachment_ref_repo:scan_organization_tx(Conn, 10, #{workspace_id => 999}).

%% ===================================================================
%% I Disk 驱动（Garage 驱动留待有授权环境——如实标注，见 T06 报告）
%% ===================================================================

t_disk_driver_full_delete_path(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    Root = ?F:disk_root(disk),
    #{namespace := Ns, container := Container} = ?F:disk_registry_apply(Root),
    Digest = ?F:disk_locator_digest(),
    Key = <<"u1/t06-disk-1">>,
    #{path := Path} = ?F:insert_attachment(
        Conn, #{
            id => 5901,
            record_id => 1001,
            path => Key,
            storage_driver => <<"disk">>,
            storage_namespace => Ns,
            storage_container => Container,
            storage_locator => Digest
        }
    ),
    ?F:insert_pending_confirmed(Conn, Path, 5901),
    %% 真实字节落盘（Disk driver put）
    ?assertEqual(
        ok, attachment_storage_disk:put(Ns, Container, Key, <<"t06-bytes">>)
    ),
    DiskPath = filename:join([Root, binary_to_list(Container), binary_to_list(Key)]),
    ?assertEqual({ok, <<"t06-bytes">>}, file:read_file(DiskPath)),
    %% 绑定 → 释放 → intent（引用零 + 完整位置）→ ready 队列
    {ok, _, true} =
        ?F:tx(Conn, fun(_) ->
            talk_record_attachment_adapter:bind_audio_part_tx(Conn, 1, 1001, 2001, 3901, 5901)
        end),
    {ok, #{outcomes := [#{outcome := intended}]}} =
        ?F:tx(Conn, fun(C) ->
            talk_record_attachment_adapter:request_record_cleanup_tx(C, 1001)
        end),
    %% worker：claim → Disk driver DELETE → ack done → 墓碑 completed
    {ok, 1} = attachment_delete_worker:run_batch(
        Conn, fun(_Ns2, _C, _K, I) -> attachment_storage:delete_snapshot(I) end
    ),
    ?assertEqual({error, enoent}, file:read_file(DiskPath)),
    {ok, [#{<<"job_state">> := <<"done">>, <<"completed_at">> := Done}]} = ?F:q(
        Conn,
        <<"SELECT job_state, completed_at FROM attachment_object_deletion WHERE path=$1">>,
        [Path]
    ),
    ?assert(Done =/= null).

%% ===================================================================
%% J 孤儿谓词（共享 attachment_repo:orphan_predicate 行为不变 + 新增）
%% ===================================================================

t_orphan_predicate_respects_bound_reference(Conn) ->
    ?F:reset_business_data(),
    ?F:seed_world(Conn),
    #{path := _Path} = ?F:insert_attachment(
        Conn, #{id => 5951, record_id => 1001, referer_time => 0}
    ),
    %% 超龄孤儿候选（回填 created_at/last_referer_at）
    {ok, []} = ?F:q(
        Conn,
        <<
            "UPDATE attachment SET created_at=now() - interval '40 days',"
            " last_referer_at=now() - interval '40 days' WHERE id=5951"
        >>,
        []
    ),
    OrphanSql =
        <<"SELECT a.id FROM attachment a WHERE ", (attachment_repo:orphan_predicate())/binary>>,
    %% bound 引用挡 orphan（含 talk_record 谓词新增分支）
    {ok, _, true} = ?F:tx(Conn, fun(_) ->
        talk_record_attachment_ref_repo:bind_tx(Conn, #{
            record_id => 1001,
            attachment_id => 5951,
            role => <<"manifest">>,
            business_key => <<"orphan-1">>
        })
    end),
    {ok, []} = ?F:q(Conn, OrphanSql, [30]),
    %% 有效 processing lease 同样挡（时间比较口径一致）
    ?F:insert_attachment(Conn, #{id => 5952, record_id => 1001, referer_time => 0}),
    {ok, []} = ?F:q(
        Conn,
        <<
            "UPDATE attachment SET created_at=now() - interval '40 days',"
            " last_referer_at=now() - interval '40 days' WHERE id=5952"
        >>,
        []
    ),
    {ok, _, true} = talk_record_attachment_adapter:grant_processing_lease_tx(
        Conn, 1001, 5952, <<"manifest">>, <<"orphan-2">>, 3600
    ),
    {ok, []} = ?F:q(Conn, OrphanSql, [30]),
    %% 释放后同一年龄可回收（既有行为不变）
    {ok, released} = talk_record_attachment_ref_repo:release_tx(
        Conn, 1001, <<"manifest">>, <<"orphan-1">>
    ),
    {ok, [#{<<"id">> := 5951}]} = ?F:q(Conn, OrphanSql, [30]).

%% ===================================================================
%% 内部
%% ===================================================================

count_refs(Conn, RecordId) ->
    %% 只数 bound（released 行按 §10.2 保留为释放证据，不算活跃引用）
    {ok, [#{<<"n">> := N}]} = ?F:q(
        Conn,
        <<
            "SELECT count(*) AS n FROM talk_record_attachment_ref"
            " WHERE record_id=$1 AND state='bound'"
        >>,
        [RecordId]
    ),
    N.

lifecycle_of(Conn, Path) ->
    {ok, [#{<<"lifecycle_state">> := L}]} = ?F:q(
        Conn, <<"SELECT lifecycle_state FROM attachment WHERE path=$1">>, [Path]
    ),
    L.

attachment_rec(Conn, Path) ->
    {ok, [Row]} = ?F:q(
        Conn,
        <<
            "SELECT id, path, url, creator_user_id, scope, scope_ref, status, mime_type"
            " FROM attachment WHERE path=$1"
        >>,
        [Path]
    ),
    Row.

fill_storage_location(Conn, Id) ->
    %% 位置必须首次 INSERT 就完整；正式冻结守卫不可通过 UPDATE 绕过。
    {ok, [
        #{
            <<"storage_driver">> := <<"disk">>,
            <<"storage_namespace">> := Ns,
            <<"storage_container">> := Container,
            <<"storage_locator_sha256">> := Digest
        }
    ]} =
        ?F:q(
            Conn,
            <<
                "SELECT storage_driver,storage_namespace,storage_container,"
                "storage_locator_sha256 FROM attachment WHERE id=$1"
            >>,
            [Id]
        ),
    ?assert(is_binary(Ns) andalso byte_size(Ns) > 0),
    ?assert(is_binary(Container) andalso byte_size(Container) > 0),
    ?assertEqual(64, byte_size(Digest)),
    ok.

ref_row(Conn, RefId) ->
    {ok, [Row]} = ?F:q(
        Conn,
        <<"SELECT id, state, lease_until FROM talk_record_attachment_ref WHERE id=$1">>,
        [RefId]
    ),
    Row.
