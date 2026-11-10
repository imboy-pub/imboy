%%% @doc talk_record 采集域 DS 层真库测试（T07 卡，127.0.0.1:55433 隔离库）。
%%%
%%% 覆盖：
%%%   * captures 创建：draft 初始态/记录投影/幂等重放/业务键重放/同记录与
%%%     同设备活跃唯一 409/If-Match 412；
%%%   * 事件：状态机 draft→recording↔paused→ended、interrupt→interrupted、
%%%     幂等重放、同 seq 异 id 409、乱序窗口（跳 seq + 补洞）折叠、
%%%     空录音 end 409、ended 后写 409 capture_ended；
%%%   * 分段：注册/稳定 V5 上传引用、同 seq 同 sha 重放、
%%%     同 seq 异 sha 409、时间轴重叠 409、空段 422、confirm 200/重放/
%%%     回执异 hash 409/附件未物化 202 verifying；
%%%   * 失联恢复：lease 过期不阻断写、recover 原会话恢复/异设备 409/
%%%     恢复后续写、abandon（owner+If-Match）后 capture_fenced 与解绑活跃；
%%%   * D7 失联残留懒收敛（T-D7-1..8）：过期 lease 残留不阻塞新创建、
%%%     insert 23505 兜底 sweep+单次重试、NULL 保守不清、旧会话复活得
%%%     capture_ended 非 capture_fenced、无无限重试。
%%%     映射：T-D7-1 d7_t1 / T-D7-2 d7_t2 / T-D7-3 d7_t3 /
%%%     T-D7-4 capture_active_conflict_fallback_test_（既有，NULL 占位仍 409）/
%%%     T-D7-5 d7_t5 / T-D7-6 lease_expiry_does_not_block_write_test_（既有）/
%%%     T-D7-7 d7_t7 / T-D7-8 d7_t8。
%%%   * 恢复视图 GET captures 分段汇总。
-module(talk_record_capture_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REC1, <<"00000000-0000-0000-0000-000000030001">>).
-define(EV2(S), uuid_from_seq(<<"80000000">>, S)).
-define(REC2, <<"00000000-0000-0000-0000-000000030002">>).
-define(DS1, <<"10000000-0000-0000-0000-000000000001">>).
-define(DS2, <<"10000000-0000-0000-0000-000000000002">>).
-define(DS3, <<"10000000-0000-0000-0000-000000000003">>).

setup() ->
    State = talk_record_t07_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t07_support:seed(State),
            State
        end,
        fun talk_record_t07_support:teardown_all/1
    ).

cleanup(State) ->
    talk_record_t07_support:teardown_all(State).

with_db(TestFun) ->
    with_db(5, TestFun).

with_db(TestTimeout, TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            {timeout, TestTimeout, ?_test(TestFun(State))}
        end}}.

%%%===================================================================
%%% captures 创建
%%%===================================================================

capture_create_and_projection_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-create-1">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-c-1">>),
        ?assert(is_integer(Cid)),
        Row = talk_record_t07_support:one(
            C,
            "SELECT status, lease_epoch, revision FROM talk_record_capture WHERE id = " ++
                integer_to_list(Cid)
        ),
        ?assertEqual(<<"draft">>, maps:get(<<"status">>, Row)),
        ?assertEqual(0, maps:get(<<"lease_epoch">>, Row)),
        ?assertEqual(1, maps:get(<<"revision">>, Row)),
        %% 记录投影：active_capture_id CAS + capture_status；record.revision 不动（§11.2）
        Rec = talk_record_t07_support:one(
            C,
            "SELECT active_capture_id, capture_status, revision FROM talk_record WHERE id = " ++
                integer_to_list(Rid)
        ),
        ?assertEqual(Cid, maps:get(<<"active_capture_id">>, Rec)),
        ?assertEqual(<<"draft">>, maps:get(<<"capture_status">>, Rec)),
        ?assertEqual(1, maps:get(<<"revision">>, Rec))
    end).

capture_idempotent_and_business_key_replay_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-create-2">>, ?REC2),
        {ok, C1} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-c-2">>),
        %% 同 Idempotency-Key 同 payload → 幂等重放（同 capture）
        {ok, C2} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-c-2">>),
        ?assertEqual(C1, C2),
        %% 异 Idempotency-Key 同 (record, device_session_id) → 业务键重放
        {ok, C3} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-c-2b">>),
        ?assertEqual(C1, C3)
    end).

capture_active_unique_conflicts_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, R1} = talk_record_t07_support:create_record(U1, <<"t07-k-cu-1">>, ?REC1),
        {ok, R2} = talk_record_t07_support:create_record(U1, <<"t07-k-cu-2">>, ?REC2),
        {ok, _} = talk_record_t07_support:create_capture(U1, R1, ?DS1, <<"t07-cu-1">>),
        %% 同记录第二采集（不同设备会话）→ 409
        ?assertEqual(
            {error, capture_active_on_record},
            talk_record_t07_support:create_capture(U1, R1, ?DS2, <<"t07-cu-2">>)
        ),
        %% 同设备（不同记录）→ 409 capture_active_on_device
        ?assertEqual(
            {error, capture_active_on_device},
            talk_record_t07_support:create_capture(U1, R2, ?DS1, <<"t07-cu-3">>, <<"t07-device-1">>)
        ),
        %% 不同设备不同会话的第二记录 → 允许
        {ok, _} = talk_record_t07_support:create_capture(
            U1, R2, ?DS3, <<"t07-cu-4">>, <<"t07-device-2">>
        )
    end).

capture_if_match_mismatch_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-im">>, ?REC1),
        Body = #{<<"device_session_id">> => ?DS1, <<"device_id">> => <<"dev">>},
        Resource = <<"/api/v1/talk-records/", (integer_to_binary(Rid))/binary, "/captures">>,
        {ok, Digest} = talk_record_logic:request_digest(<<"POST">>, Resource, Body),
        ?assertEqual(
            {error, precondition_failed},
            talk_record_ds:create_capture(U1, Rid, 9, Body, <<"t07-im-1">>, Digest)
        )
    end).

%%%===================================================================
%%% 事件流与状态机
%%%===================================================================

event_state_machine_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-ev">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-ev-1">>),
        {ok, S1} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        ?assertEqual(<<"recording">>, maps:get(<<"status">>, S1)),
        Rev1 = maps:get(<<"revision">>, S1),
        {ok, S2} = ev(U1, Rid, Cid, 2, <<"pause">>, 30_000),
        ?assertEqual(<<"paused">>, maps:get(<<"status">>, S2)),
        %% 暂停不计时：resume 后 duration 仍为事件最大有效位置（§5/§11.1）
        {ok, S3} = ev(U1, Rid, Cid, 3, <<"resume">>, 30_000),
        ?assertEqual(<<"recording">>, maps:get(<<"status">>, S3)),
        {ok, S4} = ev(U1, Rid, Cid, 4, <<"interrupt">>, 60_000),
        ?assertEqual(<<"interrupted">>, maps:get(<<"status">>, S4)),
        {ok, S5} = ev(U1, Rid, Cid, 5, <<"resume">>, 60_000),
        ?assertEqual(<<"recording">>, maps:get(<<"status">>, S5)),
        ?assert(maps:get(<<"revision">>, S5) > Rev1),
        {ok, S6} = ev(U1, Rid, Cid, 6, <<"end">>, 90_000),
        ?assertEqual(<<"ended">>, maps:get(<<"status">>, S6)),
        Row = talk_record_t07_support:one(
            C,
            "SELECT duration_ms, ended_at IS NOT NULL AS ended FROM talk_record_capture WHERE id = " ++
                integer_to_list(Cid)
        ),
        ?assertEqual(90_000, maps:get(<<"duration_ms">>, Row)),
        ?assert(maps:get(<<"ended">>, Row)),
        %% ended 后新事件 → 409 capture_ended
        ?assertEqual({error, capture_ended}, ev(U1, Rid, Cid, 7, <<"pause">>, 91_000))
    end).

event_idempotent_replay_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-er">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-er-1">>),
        {ok, S1} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        %% 同 client_event_id 重发 → 幂等重放，revision 不推进
        {ok, S2} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        ?assertEqual(maps:get(<<"revision">>, S1), maps:get(<<"revision">>, S2)),
        ?assertEqual(true, maps:get(<<"idempotent_replayed">>, S2))
    end).

event_seq_conflict_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-sc">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-sc-1">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        %% 同 seq 异 client_event_id → 409
        Base2 = #{<<"client_event_id">> => <<"20000000-0000-0000-0000-000000000001">>},
        ?assertEqual(
            {error, event_seq_conflict},
            talk_record_t07_support:send_event(
                U1,
                Rid,
                Cid,
                #{
                    <<"client_event_id">> => <<"10000000-0000-0000-0000-000000000001">>,
                    <<"device_session_id">> => ?DS1,
                    <<"seq">> => 1,
                    <<"event">> => <<"start">>,
                    <<"position_ms">> => 0
                },
                Base2
            )
        )
    end).

event_out_of_order_window_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-oo">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-oo-1">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        %% 跳 seq（乱序窗口）：3 先到（seq 2 缺失）
        {ok, S3} = ev(U1, Rid, Cid, 3, <<"interrupt">>, 50_000),
        ?assertEqual(<<"interrupted">>, maps:get(<<"status">>, S3)),
        %% 补洞 seq 2：折叠按 seq 序全量重放 → recording+pause+interrupt = interrupted
        {ok, S2} = ev(U1, Rid, Cid, 2, <<"pause">>, 20_000),
        ?assertEqual(<<"interrupted">>, maps:get(<<"status">>, S2)),
        Stored = talk_record_t07_support:one(
            C,
            "SELECT count(*) AS n FROM talk_record_capture_event WHERE capture_id = " ++
                integer_to_list(Cid)
        ),
        ?assertEqual(3, maps:get(<<"n">>, Stored))
    end).

event_empty_end_rejected_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-ee">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-ee-1">>),
        %% draft + end → 409（不生成空“已完成录音”，§11.1）
        ?assertEqual({error, invalid_event_transition}, ev(U1, Rid, Cid, 1, <<"end">>, 0))
    end).

%%%===================================================================
%%% 分段注册与 confirm
%%%===================================================================

part_register_and_replay_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        AssetsBefore = attachment_ids(C),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-pr">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-pr-1">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        {ok, created, P1} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-p-0">>),
        ?assertEqual(<<"pending">>, maps:get(<<"upload_status">>, P1)),
        ?assertEqual(null, maps:get(<<"attachment_id">>, P1)),
        %% direct-v3 合同：upload_ref 为 direct 会话（GRANTED + s3_put 描述符，
        %% ds 直返内层为原子键）
        Ref = maps:get(<<"upload_ref">>, P1),
        ?assertEqual(<<"GRANTED">>, maps:get(state, Ref)),
        Grant = maps:get(grant, Ref),
        ?assertEqual(<<"s3_put">>, maps:get(transport, Grant)),
        ?assert(is_binary(maps:get(url, Grant))),
        ?assert(
            lists:keymember(
                <<"x-amz-checksum-sha256">>, 1, maps:get(required_headers, Grant)
            )
        ),
        ?assertEqual(
            #{<<"n">> => 0},
            talk_record_t07_support:one(C, [
                "SELECT count(*) AS n FROM attachment WHERE scope='talk_record' AND scope_ref='",
                integer_to_binary(Rid),
                "'"
            ])
        ),
        %% 同 seq 同 sha（异 Idem-Key）→ 幂等重放同 part
        {ok, replay, P2} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-p-0b">>),
        ?assertEqual(maps:get(<<"part_id">>, P1), maps:get(<<"part_id">>, P2)),
        ?assertEqual(
            maps:get(object_key, Ref),
            maps:get(object_key, maps:get(<<"upload_ref">>, P2))
        ),
        ?assertEqual(AssetsBefore, attachment_ids(C))
    end).

part_same_seq_diff_sha_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-ps">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-ps-1">>),
        {ok, created, _} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-ps-0">>),
        ?assertEqual(
            {error, part_seq_conflict},
            part_sha(U1, Rid, Cid, 0, 0, 30_000, <<"t07-ps-1">>, talk_record_t07_support:sha(99))
        )
    end).

part_overlap_and_empty_segment_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-po">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-po-1">>),
        {ok, created, _} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-po-0">>),
        %% 与既有段重叠（乱序注册亦按区间相交判）→ 409
        ?assertEqual(
            {error, part_time_overlap},
            part(U1, Rid, Cid, 1, 29_000, 60_000, <<"t07-po-1">>)
        ),
        %% 不重叠但 seq 乱序（先注册 seq 3 后注册 seq 2，区间不交）→ 允许
        {ok, created, _} = part(U1, Rid, Cid, 3, 90_000, 120_000, <<"t07-po-3">>),
        {ok, created, _} = part(U1, Rid, Cid, 2, 30_000, 60_000, <<"t07-po-2">>),
        %% 空段（end =< start）→ 422 invalid_part_time_axis（暂停不产生空段）
        ?assertEqual(
            {error, invalid_part_time_axis},
            part(U1, Rid, Cid, 4, 120_000, 120_000, <<"t07-po-4">>)
        )
    end).

part_confirm_flow_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-cf">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-cf-1">>),
        {ok, created, P} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-cf-0">>),
        Pid = maps:get(<<"part_id">>, P),
        %% 仅声明不能确认：无 staging 字节，直拷面拒 copy_source_missing
        %% （S3-only direct 合同；audio_part_verification_failed 在字节存在
        %% 但失配时才生效）。
        ?assertEqual(null, maps:get(<<"attachment_id">>, P)),
        ?assertEqual(
            {error, {rejected, copy_source_missing}},
            talk_record_ds:confirm_part(U1, Rid, Cid, Pid, #{})
        ),
        %% 首次 finalize 已拒该 attempt（copy_source_missing）：按 A08 重发
        %% 凭据合同，注册重放取得新 grant/attempt，重 PUT 后 confirm 收口。
        {ok, replay, P1b} = part(U1, Rid, Cid, 0, 0, 30000, <<"t07-cf-0">>),
        ok = talk_record_upload_test_support:upload(U1, P1b),
        {ok, confirmed, C1} = talk_record_ds:confirm_part(U1, Rid, Cid, Pid, #{}),
        ?assertEqual(<<"confirmed">>, maps:get(<<"upload_status">>, C1)),
        %% Lost registration response replays the current asset/status, not its
        %% original pending snapshot, and never issues a second object key.
        {ok, replay, RegisteredAgain} = part(U1, Rid, Cid, 0, 0, 30000, <<"t07-cf-0">>),
        ?assertEqual(
            maps:get(<<"attachment_id">>, C1), maps:get(<<"attachment_id">>, RegisteredAgain)
        ),
        ?assertEqual(<<"confirmed">>, maps:get(<<"upload_status">>, RegisteredAgain)),
        %% 重复同 hash → 安全重放
        {ok, replay, _} = talk_record_ds:confirm_part(U1, Rid, Cid, Pid, #{}),
        %% 回执异 hash → 409
        ?assertEqual(
            {error, audio_part_hash_conflict},
            talk_record_ds:confirm_part(
                U1,
                Rid,
                Cid,
                Pid,
                #{sha256 => talk_record_t07_support:sha(999)}
            )
        )
    end).

part_confirm_verifying_202_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-vf">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-vf-1">>),
        {ok, created, P} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-vf-0">>),
        Pid = maps:get(<<"part_id">>, P),
        %% A missing upload session is pending, never an asset confirmation.
        talk_record_t07_support:sql(C, "DELETE FROM attach_pending"),
        {ok, verifying, V} = talk_record_ds:confirm_part(U1, Rid, Cid, Pid, #{}),
        ?assertEqual(<<"verifying">>, maps:get(<<"status">>, V))
    end).

%%%===================================================================
%%% 失联恢复 / fencing（§11.1）
%%%===================================================================

lease_expiry_does_not_block_write_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-lz">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-lz-1">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        %% lease 过期仅表示失联，不证明停录 → 写仍允许
        talk_record_t07_support:sql(
            C,
            "UPDATE talk_record_capture SET lease_until = now() - interval '1 hour' WHERE id = " ++
                integer_to_list(Cid)
        ),
        {ok, S} = ev(U1, Rid, Cid, 2, <<"pause">>, 10_000),
        ?assertEqual(<<"paused">>, maps:get(<<"status">>, S))
    end).

recover_flow_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-rv">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-rv-1">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        {ok, created, P} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-rv-0">>),
        ok = talk_record_upload_test_support:upload(U1, P),
        {ok, confirmed, _} = talk_record_ds:confirm_part(
            U1,
            Rid,
            Cid,
            maps:get(<<"part_id">>, P),
            #{}
        ),
        %% 异设备会话 → 409（不接管）
        ?assertEqual(
            {error, capture_device_mismatch},
            talk_record_ds:recover_capture(U1, Rid, Cid, ?DS2)
        ),
        %% lease 过期后原会话 recover → 恢复 + 已确认分段
        talk_record_t07_support:sql(
            C,
            "UPDATE talk_record_capture SET lease_until = now() - interval '1 hour' WHERE id = " ++
                integer_to_list(Cid)
        ),
        {ok, R} = talk_record_ds:recover_capture(U1, Rid, Cid, ?DS1),
        ?assertEqual(true, maps:get(<<"recovered">>, R)),
        ?assertEqual(1, length(maps:get(<<"confirmed_parts">>, R))),
        ?assertEqual(1, maps:get(<<"confirmed">>, maps:get(<<"parts">>, R))),
        %% 恢复后可续写
        {ok, S} = ev(U1, Rid, Cid, 2, <<"pause">>, 30_000),
        ?assertEqual(<<"paused">>, maps:get(<<"status">>, S))
    end).

abandon_fences_old_device_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-ab">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-ab-1">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        {ok, S2} = ev(U1, Rid, Cid, 2, <<"pause">>, 5_000),
        Rev = maps:get(<<"revision">>, S2),
        %% 错误 If-Match → 412
        ?assertEqual(
            {error, precondition_failed},
            talk_record_ds:abandon_capture(U1, Rid, Cid, Rev + 1)
        ),
        %% 正确 If-Match → ended + epoch 推进
        {ok, A} = talk_record_ds:abandon_capture(U1, Rid, Cid, Rev),
        ?assertEqual(1, maps:get(<<"lease_epoch">>, A)),
        ?assertEqual(<<"ended">>, maps:get(<<"status">>, A)),
        %% 旧设备后续写 → 409 capture_fenced
        ?assertEqual({error, capture_fenced}, ev(U1, Rid, Cid, 3, <<"resume">>, 5_000)),
        ?assertEqual(
            {error, capture_fenced},
            part(U1, Rid, Cid, 9, 10_000, 20_000, <<"t07-ab-9">>)
        ),
        %% recover 也被冻结
        ?assertEqual(
            {error, capture_fenced},
            talk_record_ds:recover_capture(U1, Rid, Cid, ?DS1)
        ),
        %% abandon 解绑活跃：同记录可开新采集
        {ok, _} = talk_record_t07_support:create_capture(
            U1, Rid, ?DS2, <<"t07-ab-2">>, <<"t07-device-2">>
        ),
        Row = talk_record_t07_support:one(
            C,
            "SELECT lease_epoch FROM talk_record_capture WHERE id = " ++ integer_to_list(Cid)
        ),
        ?assertEqual(1, maps:get(<<"lease_epoch">>, Row))
    end).

captures_recovery_view_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-k-ls">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-ls-1">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        {ok, created, P} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-ls-0">>),
        ok = talk_record_upload_test_support:upload(U1, P),
        {ok, confirmed, CP} = talk_record_ds:confirm_part(
            U1,
            Rid,
            Cid,
            maps:get(<<"part_id">>, P),
            #{}
        ),
        ?assertEqual(<<"confirmed">>, maps:get(<<"upload_status">>, CP)),
        {ok, #{<<"captures">> := [Item | _]}} = talk_record_ds:list_captures(U1, Rid),
        ?assertEqual(Cid, maps:get(<<"capture_id">>, Item)),
        ?assertEqual(1, maps:get(<<"confirmed">>, maps:get(<<"parts">>, Item))),
        ?assertEqual(0, maps:get(<<"pending">>, maps:get(<<"parts">>, Item))),
        ?assertEqual(1, maps:get(<<"last_event_seq">>, Item)),
        %% 无权主体 → 404
        ?assertEqual({error, not_found}, talk_record_ds:list_captures(maps:get(u7, Ids), Rid))
    end).

%%%===================================================================
%%% P1 修复回归：采集写 owner-only 门禁（Q-review-T07）
%%%===================================================================

%% 陌生人（无 grant，记录不可见）写 events/audio-parts/confirm → 统一 404
capture_write_stranger_404_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        U7 = maps:get(u7, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-gk-s">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-gk-s-c">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        {ok, created, P} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-gk-s-p">>),
        %% 陌生人 events → 404（不泄露存在性）
        ?assertEqual(
            {error, not_found},
            talk_record_t07_support:send_event(
                U7,
                Rid,
                Cid,
                #{
                    <<"client_event_id">> => ?EV2(900),
                    <<"device_session_id">> => ?DS1,
                    <<"seq">> => 2,
                    <<"event">> => <<"pause">>,
                    <<"position_ms">> => 1000
                },
                #{}
            )
        ),
        %% 陌生人 audio-parts 注册 / confirm → 404
        ?assertEqual(
            {error, not_found},
            part_sha(
                U7, Rid, Cid, 1, 30_000, 60_000, <<"t07-gk-s-p1">>, talk_record_t07_support:sha(1)
            )
        ),
        ?assertEqual(
            {error, not_found},
            talk_record_ds:confirm_part(U7, Rid, Cid, maps:get(<<"part_id">>, P), #{})
        )
    end).

%% grant viewer 可读不可写：events / audio-parts / confirm 全拒 403
capture_write_viewer_403_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-gk-v">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-gk-v-c">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        {ok, created, P} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-gk-v-p">>),
        insert_grant(Rid, U1, U2, <<"viewer">>),
        %% viewer 可见（读权限成立）但采集写 403
        {ok, _} = talk_record_ds:list_captures(U2, Rid),
        ?assertEqual(
            {error, forbidden},
            talk_record_t07_support:send_event(
                U2,
                Rid,
                Cid,
                #{
                    <<"client_event_id">> => ?EV2(901),
                    <<"device_session_id">> => ?DS1,
                    <<"seq">> => 2,
                    <<"event">> => <<"pause">>,
                    <<"position_ms">> => 1000
                },
                #{}
            )
        ),
        ?assertEqual(
            {error, forbidden},
            part_sha(
                U2, Rid, Cid, 1, 30_000, 60_000, <<"t07-gk-v-p1">>, talk_record_t07_support:sha(1)
            )
        ),
        ?assertEqual(
            {error, forbidden},
            talk_record_ds:confirm_part(U2, Rid, Cid, maps:get(<<"part_id">>, P), #{})
        )
    end).

%% grant editor 的校对权不含采集写（§4 owner/合法 capture session 才可调用）
capture_write_editor_403_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        U3 = maps:get(u3, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-gk-e">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-gk-e-c">>),
        {ok, _} = ev(U1, Rid, Cid, 1, <<"start">>, 0),
        {ok, created, P} = part(U1, Rid, Cid, 0, 0, 30_000, <<"t07-gk-e-p">>),
        insert_grant(Rid, U1, U3, <<"editor">>),
        %% editor 对记录有读权限（marks/notes 可编辑），但采集写全拒 403
        {ok, _} = talk_record_ds:list_captures(U3, Rid),
        ?assertEqual(
            {error, forbidden},
            talk_record_t07_support:send_event(
                U3,
                Rid,
                Cid,
                #{
                    <<"client_event_id">> => ?EV2(902),
                    <<"device_session_id">> => ?DS1,
                    <<"seq">> => 2,
                    <<"event">> => <<"pause">>,
                    <<"position_ms">> => 1000
                },
                #{}
            )
        ),
        ?assertEqual(
            {error, forbidden},
            part_sha(
                U3, Rid, Cid, 1, 30_000, 60_000, <<"t07-gk-e-p1">>, talk_record_t07_support:sha(1)
            )
        ),
        ?assertEqual(
            {error, forbidden},
            talk_record_ds:confirm_part(U3, Rid, Cid, maps:get(<<"part_id">>, P), #{})
        ),
        %% owner 本人不受影响（门禁修复不破坏正向路径）
        {ok, _} = ev(U1, Rid, Cid, 2, <<"pause">>, 1000)
    end).

%% 另一 owner（跨记录可见性不成立）对不属于自己的采集写 → 404
capture_write_other_owner_404_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        U4 = maps:get(u4, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-gk-o">>, ?REC1),
        {ok, Cid} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-gk-o-c">>),
        ?assertEqual(
            {error, not_found},
            talk_record_t07_support:send_event(
                U4,
                Rid,
                Cid,
                #{
                    <<"client_event_id">> => ?EV2(903),
                    <<"device_session_id">> => ?DS1,
                    <<"seq">> => 1,
                    <<"event">> => <<"start">>,
                    <<"position_ms">> => 0
                },
                #{}
            )
        )
    end).

%% P4：活跃唯一冲突的并发兜底分支（capture_active_conflict）——
%% 两连接真并发：A 事务插入同设备活跃 capture（未提交），B 预检
%% （READ COMMITTED 不可见）通过后 INSERT 阻塞，A 提交 → B 撞唯一索引
%% → ON CONFLICT DO NOTHING 0 行 → unique_violation → 兜底 409。
capture_active_conflict_fallback_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-race">>, ?REC1),
        %% 连接 A（测试 raw 连接）：未提交的同设备活跃 capture
        {ok, [], []} = epgsql:squery(C, "BEGIN"),
        {ok, 1} = epgsql:equery(
            C,
            "INSERT INTO talk_record_capture (id, record_id, device_session_id, device_id, started_by)"
            " VALUES ($1, $2, $3::uuid, $4, $5)",
            [
                9199999000001,
                Rid,
                <<"a0000000-0000-0000-0000-000000000001">>,
                <<"t07-race-dev">>,
                U1
            ]
        ),
        %% 连接 B（DS 池连接）：预检通过 → INSERT 阻塞至 A 提交
        Self = self(),
        spawn(fun() ->
            Self !
                {race_result,
                    talk_record_t07_support:create_capture(
                        U1,
                        Rid,
                        <<"b0000000-0000-0000-0000-000000000001">>,
                        <<"t07-race-1">>,
                        <<"t07-race-dev">>
                    )}
        end),
        timer:sleep(400),
        {ok, [], []} = epgsql:squery(C, "COMMIT"),
        Result =
            receive
                {race_result, R} -> R
            after 15000 ->
                erlang:error(race_result_timeout)
            end,
        ?assertEqual({error, capture_active_conflict}, Result),
        %% 兜底后 A 行占位同记录活跃唯一：再创建走预检 409（on_record 先于
        %% on_device 判定——同一行同时占两个唯一索引，语义等价）
        ?assertEqual(
            {error, capture_active_on_record},
            talk_record_t07_support:create_capture(
                U1,
                Rid,
                <<"c0000000-0000-0000-0000-000000000001">>,
                <<"t07-race-2">>,
                <<"t07-race-dev">>
            )
        )
    end).

%%%===================================================================
%%% D7：失联残留懒收敛（lease_until 预检放宽 + insert 23505 兜底 sweep+重试）
%%%===================================================================

%% T-D7-1：同设备异记录残留（status='recording'，lease 过期 20h）→
%% 新创建 201；旧 capture 收敛为 ended 且 lease_epoch 不动（不 fencing）；
%% 其记录投影清 active_capture_id=NULL。
capture_d7_t1_expired_device_residue_swept_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, R1} = talk_record_t07_support:create_record(U1, <<"t07-d7t1-r1">>, ?REC1),
        {ok, R2} = talk_record_t07_support:create_record(U1, <<"t07-d7t1-r2">>, ?REC2),
        {ok, C1} = talk_record_t07_support:create_capture(U1, R1, ?DS1, <<"t07-d7t1-1">>),
        {ok, _} = ev(U1, R1, C1, 1, <<"start">>, 0),
        %% 失联：lease 停在过去（进程被杀后不再续租）
        talk_record_t07_support:sql(
            C,
            "UPDATE talk_record_capture SET lease_until = now() - interval '20 hours'"
            " WHERE id = " ++
                integer_to_list(C1)
        ),
        %% 同设备（t07-device-1，与 C1 的 4 参 helper 默认 device 同）异记录 →
        %% 预检放行（lease 过期），INSERT 撞 uq_trc_device_active → sweep+重试
        {ok, C2} = talk_record_t07_support:create_capture(
            U1, R2, ?DS2, <<"t07-d7t1-2">>, <<"t07-device-1">>
        ),
        ?assert(is_integer(C2)),
        Old = talk_record_t07_support:one(
            C,
            "SELECT status, lease_epoch FROM talk_record_capture WHERE id = " ++
                integer_to_list(C1)
        ),
        ?assertEqual(<<"ended">>, maps:get(<<"status">>, Old)),
        ?assertEqual(0, maps:get(<<"lease_epoch">>, Old)),
        Proj = talk_record_t07_support:one(
            C,
            "SELECT active_capture_id, capture_status FROM talk_record WHERE id = " ++
                integer_to_list(R1)
        ),
        ?assertEqual(null, maps:get(<<"active_capture_id">>, Proj)),
        ?assertEqual(<<"ended">>, maps:get(<<"capture_status">>, Proj))
    end).

%% T-D7-2：同记录异设备残留 → 新创建 201，talk_record.active_capture_id
%% 指向新 capture（NC5 r7「设备永久阻塞」的根场景回归）。
capture_d7_t2_expired_record_resume_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-d7t2-r">>, ?REC1),
        {ok, C1} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-d7t2-1">>),
        {ok, _} = ev(U1, Rid, C1, 1, <<"start">>, 0),
        talk_record_t07_support:sql(
            C,
            "UPDATE talk_record_capture SET lease_until = now() - interval '20 hours'"
            " WHERE id = " ++
                integer_to_list(C1)
        ),
        {ok, C2} = talk_record_t07_support:create_capture(
            U1, Rid, ?DS2, <<"t07-d7t2-2">>, <<"t07-d7t2-dev">>
        ),
        ?assert(is_integer(C2)),
        ?assertNotEqual(C1, C2),
        Proj = talk_record_t07_support:one(
            C,
            "SELECT active_capture_id FROM talk_record WHERE id = " ++ integer_to_list(Rid)
        ),
        ?assertEqual(C2, maps:get(<<"active_capture_id">>, Proj))
    end).

%% T-D7-3：活 lease 新鲜冲突（lease_until=now()+5min）→ 预检仍判活跃，
%% 同记录/同设备均 409（回归：D7 只放宽过期残留，语义不变）。
capture_d7_t3_fresh_lease_still_conflicts_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, R1} = talk_record_t07_support:create_record(U1, <<"t07-d7t3-r1">>, ?REC1),
        {ok, R2} = talk_record_t07_support:create_record(U1, <<"t07-d7t3-r2">>, ?REC2),
        {ok, C1} = talk_record_t07_support:create_capture(U1, R1, ?DS1, <<"t07-d7t3-1">>),
        talk_record_t07_support:sql(
            C,
            "UPDATE talk_record_capture SET lease_until = now() + interval '5 minutes'"
            " WHERE id = " ++
                integer_to_list(C1)
        ),
        ?assertEqual(
            {error, capture_active_on_record},
            talk_record_t07_support:create_capture(U1, R1, ?DS2, <<"t07-d7t3-2">>, <<"t07-d7t3-a">>)
        ),
        ?assertEqual(
            {error, capture_active_on_device},
            talk_record_t07_support:create_capture(
                U1, R2, ?DS3, <<"t07-d7t3-3">>, <<"t07-device-1">>
            )
        )
    end).

%% T-D7-5：sweep 收敛（epoch 不动）后旧会话复活写 events → 409 capture_ended
%% （对照：abandon 路径 epoch 推进 → capture_fenced）。
capture_d7_t5_swept_session_revival_capture_ended_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-d7t5-r">>, ?REC1),
        {ok, C1} = talk_record_t07_support:create_capture(U1, Rid, ?DS1, <<"t07-d7t5-1">>),
        {ok, _} = ev(U1, Rid, C1, 1, <<"start">>, 0),
        talk_record_t07_support:sql(
            C,
            "UPDATE talk_record_capture SET lease_until = now() - interval '20 hours'"
            " WHERE id = " ++
                integer_to_list(C1)
        ),
        %% 新创建触发 sweep（同记录新设备会话）
        {ok, _C2} = talk_record_t07_support:create_capture(
            U1, Rid, ?DS2, <<"t07-d7t5-2">>, <<"t07-d7t5-dev">>
        ),
        %% 旧会话复活写 → ended 语义拒（非 fenced）
        ?assertEqual({error, capture_ended}, ev(U1, Rid, C1, 2, <<"pause">>, 10_000)),
        %% 对照：abandon 推进 epoch → capture_fenced
        {ok, R3} = talk_record_t07_support:create_record(U1, <<"t07-d7t5-r3">>, ?REC2),
        {ok, C3} = talk_record_t07_support:create_capture(
            U1, R3, ?DS3, <<"t07-d7t5-3">>, <<"t07-d7t5-dev3">>
        ),
        {ok, _} = ev(U1, R3, C3, 1, <<"start">>, 0),
        Row = talk_record_t07_support:one(
            C,
            "SELECT revision FROM talk_record_capture WHERE id = " ++ integer_to_list(C3)
        ),
        {ok, _} = talk_record_ds:abandon_capture(
            U1, R3, C3, maps:get(<<"revision">>, Row)
        ),
        ?assertEqual({error, capture_fenced}, ev(U1, R3, C3, 2, <<"pause">>, 10_000))
    end).

%% T-D7-7：lease_until IS NULL 的活跃行 → 预检保守视为活跃 409；
%% sweep 不清（end_expired_conflicts_tx 返回空、行保持活跃）。
capture_d7_t7_null_lease_conservative_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-d7t7-r">>, ?REC1),
        {ok, [], []} = epgsql:squery(C, "BEGIN"),
        {ok, 1} = epgsql:equery(
            C,
            "INSERT INTO talk_record_capture (id, record_id, device_session_id, device_id,"
            " started_by, status) VALUES ($1, $2, $3::uuid, $4, $5, 'recording')",
            [
                9199999000071,
                Rid,
                <<"d7000000-0000-0000-0000-000000000001">>,
                <<"t07-d7t7-dev">>,
                U1
            ]
        ),
        {ok, [], []} = epgsql:squery(C, "COMMIT"),
        %% NULL → 预检保守活跃
        ?assertEqual(
            {error, capture_active_on_record},
            talk_record_t07_support:create_capture(
                U1,
                Rid,
                <<"d7000000-0000-0000-0000-000000000002">>,
                <<"t07-d7t7-1">>,
                <<"t07-d7t7-dev2">>
            )
        ),
        %% sweep 不清 NULL 行
        {ok, []} = talk_record_capture_repo:end_expired_conflicts_tx(
            C, Rid, <<"t07-d7t7-dev">>
        ),
        Row = talk_record_t07_support:one(
            C,
            "SELECT status FROM talk_record_capture WHERE id = 9199999000071"
        ),
        ?assertEqual(<<"recording">>, maps:get(<<"status">>, Row))
    end).

%% T-D7-8：并发注入——sweep 命中过期残留 X 后单次重试，再撞新鲜活跃行 Y
%% （A 连接提交窗内出现）→ 仍 409 capture_active_conflict，无无限重试；
%% 失败路径的 sweep 随 B 事务回滚（X 保持活跃，Y 保持活跃）。
capture_d7_t8_retry_once_then_conflict_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t07_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, Rid} = talk_record_t07_support:create_record(U1, <<"t07-d7t8-r">>, ?REC1),
        {ok, R2} = talk_record_t07_support:create_record(U1, <<"t07-d7t8-r2">>, ?REC2),
        %% X：过期残留（已提交，sweep 目标）——异记录、与 B 的尝试同设备
        %% （end_expired_conflicts_tx 按 record_id=$1 OR device_id=$2 匹配；
        %%  同记录放两条活跃行会先撞 uq_trc_record_active，故走设备维匹配）
        {ok, 1} = epgsql:equery(
            C,
            "INSERT INTO talk_record_capture (id, record_id, device_session_id, device_id,"
            " started_by, status, lease_until)"
            " VALUES ($1, $2, $3::uuid, $4, $5, 'recording', now() - interval '20 hours')",
            [
                9199999000081,
                R2,
                <<"d8000000-0000-0000-0000-000000000001">>,
                <<"t07-d7t8-dev">>,
                U1
            ]
        ),
        %% Y：A 连接未提交的新鲜活跃行（B 预检不可见）
        {ok, [], []} = epgsql:squery(C, "BEGIN"),
        {ok, 1} = epgsql:equery(
            C,
            "INSERT INTO talk_record_capture (id, record_id, device_session_id, device_id,"
            " started_by, status, lease_until)"
            " VALUES ($1, $2, $3::uuid, $4, $5, 'recording', now() + interval '5 minutes')",
            [
                9199999000082,
                Rid,
                <<"d8000000-0000-0000-0000-000000000002">>,
                <<"t07-d7t8-dev2">>,
                U1
            ]
        ),
        Self = self(),
        spawn(fun() ->
            Self !
                {d7t8_result,
                    talk_record_t07_support:create_capture(
                        U1,
                        Rid,
                        <<"d8000000-0000-0000-0000-000000000003">>,
                        <<"t07-d7t8-b">>,
                        <<"t07-d7t8-dev">>
                    )}
        end),
        timer:sleep(400),
        {ok, [], []} = epgsql:squery(C, "COMMIT"),
        Result =
            receive
                {d7t8_result, R} -> R
            after 15000 ->
                erlang:error(d7t8_result_timeout)
            end,
        ?assertEqual({error, capture_active_conflict}, Result),
        %% 失败路径 sweep 随 B 事务一并回滚（语义：不 fence 旧会话除非新采集
        %% 真落地；残留仍过期，下次创建会再 sweep——无永久阻塞、无无限重试）
        X = talk_record_t07_support:one(
            C,
            "SELECT status FROM talk_record_capture WHERE id = 9199999000081"
        ),
        ?assertEqual(<<"recording">>, maps:get(<<"status">>, X)),
        Y = talk_record_t07_support:one(
            C,
            "SELECT status FROM talk_record_capture WHERE id = 9199999000082"
        ),
        ?assertEqual(<<"recording">>, maps:get(<<"status">>, Y))
    end).

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec ev(integer(), integer(), integer(), integer(), binary(), integer()) ->
    {ok, map()} | {error, term()}.
ev(Uid, Rid, Cid, Seq, Event, Position) ->
    talk_record_t07_support:send_event(
        Uid,
        Rid,
        Cid,
        #{
            <<"client_event_id">> => event_uuid(Seq),
            <<"device_session_id">> => ?DS1,
            <<"seq">> => Seq,
            <<"event">> => Event,
            <<"position_ms">> => Position
        },
        #{}
    ).

-spec event_uuid(integer()) -> binary().
event_uuid(Seq) ->
    uuid_from_seq(<<"30000000">>, Seq).

-spec uuid_from_seq(binary(), integer()) -> binary().
uuid_from_seq(Prefix, Seq) ->
    Hex = integer_to_binary(Seq, 16),
    PadLen = max(0, 12 - byte_size(Hex)),
    Pad = <<<<$0>> || _ <- lists:seq(1, PadLen)>>,
    <<Prefix/binary, "-0000-0000-0000-", Pad/binary, Hex/binary>>.

-spec insert_grant(integer(), integer(), integer(), binary()) -> ok.
insert_grant(RecordId, Grantor, Grantee, Role) ->
    {ok, _} = elib_pg:query(
        <<"INSERT INTO talk_record_grant (id, record_id, grantee_user_id, role, granted_by)",
            " VALUES ($1, $2, $3, $4, $5)">>,
        [elib_tsid:generate(talk_record), RecordId, Grantee, Role, Grantor]
    ),
    ok.

%% Upload closure regressions：direct-v1 会话 + garage staging 字节 + 引用绑定。
part_renew_preserves_identity_test_() ->
    %% dv1 现行合同：session_deadline 由 fn_dv1_register 服务端设定
    %% （now()+7200）且不可变；presign 有效期在 grant 描述符 expires_in
    %% （签名内表达），不落 attach_pending.upload_expires_at（V5 遗留列，
    %% dv1 恒 NULL）——注册重放合同 = 身份恒等 + 位置不变 + 可继续上传。
    with_db(15, fun(State) ->
        Conn = maps:get(conn, State),
        {Uid, Rid, Cid, P} = upload_fixture(),
        Ref = maps:get(<<"upload_ref">>, P),
        Key = maps:get(object_key, Ref),
        Original = upload_identity(Conn, Key),
        {ok, OriginalLocation} = attachment_storage:session_location(Key),
        {ok, replay, P2} = part(Uid, Rid, Cid, 0, 0, 30000, <<"up-retry">>),
        Ref2 = maps:get(<<"upload_ref">>, P2),
        Renewed = upload_identity(Conn, Key),
        ?assertEqual({ok, OriginalLocation}, attachment_storage:session_location(Key)),
        ?assertEqual(maps:remove(<<"expiry">>, Original), maps:remove(<<"expiry">>, Renewed)),
        ?assertEqual(Key, maps:get(object_key, Ref2)),
        ok = talk_record_upload_test_support:upload(Uid, P2),
        ?assertMatch(
            {ok, confirmed, _},
            talk_record_ds:confirm_part(Uid, Rid, Cid, maps:get(<<"part_id">>, P2), #{})
        )
    end).

part_wrong_declared_hash_no_asset_test_() ->
    with_db(fun(State) ->
        AssetsBefore = attachment_ids(maps:get(conn, State)),
        {Uid, Rid, Cid, _} = upload_fixture(),
        {ok, created, P} = part_sha(
            Uid, Rid, Cid, 1, 30000, 60000, <<"up-wrong">>, talk_record_t07_support:sha(99)
        ),
        %% S3-only direct 合同：presign 头携带声明 sha 的 checksum，与正文
        %% 不一致在 PUT 即被 S3 400 拒收（fail-closed 前移）；confirm 侧验证
        %% 在字节存在但失配时才生效。
        ok = talk_record_upload_test_support:upload_failclosed(P),
        ?assertEqual(
            {error, {rejected, copy_source_missing}},
            talk_record_ds:confirm_part(Uid, Rid, Cid, maps:get(<<"part_id">>, P), #{})
        ),
        ?assertEqual(
            #{<<"n">> => 0},
            talk_record_t07_support:one(maps:get(conn, State), [
                "SELECT count(*) AS n FROM attachment WHERE scope='talk_record' AND scope_ref='",
                integer_to_binary(Rid),
                "'"
            ])
        ),
        ?assertEqual(AssetsBefore, attachment_ids(maps:get(conn, State)))
    end).

part_pin_without_put_no_confirm_test_() ->
    with_db(fun(_State) ->
        {Uid, Rid, Cid, P} = upload_fixture(),
        Key = maps:get(object_key, maps:get(<<"upload_ref">>, P)),
        %% dv1 合同：内容身份在 declare 即冻结（direct_expected），V5 内容
        %% pin 面不再接受 dv1 会话（防绕开冻结身份改写内容）。
        ?assertEqual(
            {error, session_not_renewable},
            attach_pending_repo:register_content_pin(Key, #{
                <<"version">> => 1,
                <<"sha256">> => talk_record_t07_support:sha(0),
                <<"size_bytes">> => <<"4096">>,
                <<"mime">> => <<"audio/aac">>
            })
        ),
        %% 未 PUT 即无字节：confirm 拒 copy_source_missing，不产资产。
        ?assertEqual(
            {error, {rejected, copy_source_missing}},
            talk_record_ds:confirm_part(Uid, Rid, Cid, maps:get(<<"part_id">>, P), #{})
        )
    end).

part_ended_accepts_late_upload_test_() ->
    with_db(fun(_State) ->
        {Uid, Rid, Cid, P} = upload_fixture(),
        {ok, _} = ev(Uid, Rid, Cid, 1, <<"start">>, 0),
        {ok, _} = ev(Uid, Rid, Cid, 2, <<"end">>, 60000),
        {ok, created, _} = part(Uid, Rid, Cid, 1, 30000, 60000, <<"late">>),
        ok = talk_record_upload_test_support:upload(Uid, P),
        ?assertMatch(
            {ok, confirmed, _},
            talk_record_ds:confirm_part(Uid, Rid, Cid, maps:get(<<"part_id">>, P), #{})
        ),
        {ok, Cid2} = talk_record_t07_support:create_capture(Uid, Rid, ?DS2, <<"second-capture">>),
        {ok, created, P2} = part(Uid, Rid, Cid2, 0, 0, 30000, <<"second-part">>),
        ?assertNotEqual(
            maps:get(object_key, maps:get(<<"upload_ref">>, P)),
            maps:get(object_key, maps:get(<<"upload_ref">>, P2))
        )
    end).

part_concurrent_confirms_one_reference_test_() ->
    with_db(fun(State) ->
        {Uid, Rid, Cid, P} = upload_fixture(),
        Pid = maps:get(<<"part_id">>, P),
        ok = talk_record_upload_test_support:upload(Uid, P),
        Self = self(),
        Tag = make_ref(),
        [
            spawn(fun() -> Self ! {Tag, talk_record_ds:confirm_part(Uid, Rid, Cid, Pid, #{})} end)
         || _ <- [1, 2]
        ],
        Results = [
            receive
                {Tag, R} -> R
            after 10000 -> timeout
            end
         || _ <- [1, 2]
        ],
        ?assertEqual([confirmed, replay], lists:sort([Mark || {ok, Mark, _} <- Results])),
        ?assertEqual(
            #{<<"n">> => 1},
            talk_record_t07_support:one(
                maps:get(conn, State),
                "SELECT count(*) AS n FROM talk_record_attachment_ref WHERE state='bound'"
            )
        )
    end).

%% S3-only dv1 合同：staging 字节只能经 talk-record confirm 面绑为资产
%% （直拷+证明+原子绑定）；遗留 V5 attach 面不认 dv1 staging 对象——
%% 如实拒 object_not_found，防止面外绑定。资产墓碑/撤权的读面拒收合同
%% 由 playback_stream_tests 承载。
part_binding_surface_isolation_test_() ->
    with_db(fun(State) ->
        Conn = maps:get(conn, State),
        {Uid, Rid, Cid, P} = upload_fixture(),
        Key = maps:get(object_key, maps:get(<<"upload_ref">>, P)),
        ok = talk_record_upload_test_support:upload(Uid, P),
        ?assertEqual(
            {error, object_not_found},
            attach_logic:confirm(Uid, Key, <<"talk_record">>, integer_to_binary(Rid), #{})
        ),
        ?assertEqual(
            #{<<"n">> => 0},
            talk_record_t07_support:one(
                Conn, "SELECT count(*) AS n FROM talk_record_attachment_ref"
            )
        ),
        %% 唯一绑定路径是 confirm 面：成功后 ref 恰绑定一条、资产 live。
        {ok, confirmed, _} = talk_record_ds:confirm_part(
            Uid, Rid, Cid, maps:get(<<"part_id">>, P), #{}
        ),
        ?assertEqual(
            #{<<"n">> => 1},
            talk_record_t07_support:one(
                Conn, "SELECT count(*) AS n FROM talk_record_attachment_ref"
            )
        )
    end).

part_revoked_org_member_rejects_test_() ->
    with_db(fun(State) ->
        Conn = maps:get(conn, State),
        {Uid, Rid, Cid, P} = organization_upload_fixture(Conn, false),
        Org = maps:get(org_a, talk_record_t07_support:ids()),
        ok = talk_record_upload_test_support:upload(Uid, P),
        {ok, _} = elib_pg:query(
            Conn,
            <<"UPDATE organization_member SET status='suspended' WHERE organization_id=$1 AND user_id=$2">>,
            [Org, Uid]
        ),
        ?assertEqual(
            {error, not_found},
            talk_record_ds:confirm_part(Uid, Rid, Cid, maps:get(<<"part_id">>, P), #{})
        )
    end).

part_workspace_revoke_waits_then_rejects_test_() ->
    with_db(fun(State) ->
        Conn = maps:get(conn, State),
        {Uid, Rid, Cid, P} = organization_upload_fixture(Conn, true),
        Ws = maps:get(ws_a1, talk_record_t07_support:ids()),
        ok = talk_record_upload_test_support:upload(Uid, P),
        {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
        try
            {ok, _} = elib_pg:query(
                Conn,
                <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>,
                [Ws, Uid]
            ),
            Self = self(),
            Tag = make_ref(),
            spawn(fun() ->
                Self !
                    {Tag,
                        talk_record_ds:confirm_part(Uid, Rid, Cid, maps:get(<<"part_id">>, P), #{})}
            end),
            %% Revoke owns the row lock first; confirmation cannot bind before
            %% that change commits and must observe the revoked membership.
            receive
                {Tag, Early} -> erlang:error({confirm_ignored_membership_lock, Early})
            after 50 -> ok
            end,
            {ok, [], []} = epgsql:squery(Conn, "COMMIT"),
            Result =
                receive
                    {Tag, R} -> R
                after 10000 -> timeout
                end,
            ?assertEqual({error, not_found}, Result),
            ?assertEqual(
                #{<<"n">> => 0},
                talk_record_t07_support:one(
                    Conn, "SELECT count(*) AS n FROM talk_record_attachment_ref"
                )
            )
        after
            epgsql:squery(Conn, "ROLLBACK")
        end
    end).

upload_fixture() ->
    Uid = maps:get(u1, talk_record_t07_support:ids()),
    {ok, Rid} = talk_record_t07_support:create_record(Uid, <<"upload-record">>, ?REC1),
    {ok, Cid} = talk_record_t07_support:create_capture(Uid, Rid, ?DS1, <<"upload-capture">>),
    {ok, created, P} = part(Uid, Rid, Cid, 0, 0, 30000, <<"upload-part">>),
    {Uid, Rid, Cid, P}.

-spec part(integer(), integer(), integer(), integer(), integer(), integer(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
part(Uid, Rid, Cid, Seq, Start, End, IdemKey) ->
    part_sha(Uid, Rid, Cid, Seq, Start, End, IdemKey, talk_record_t07_support:sha(Seq)).

-spec part_sha(
    integer(), integer(), integer(), integer(), integer(), integer(), binary(), binary()
) ->
    {ok, created | replay, map()} | {error, term()}.
part_sha(Uid, Rid, Cid, Seq, Start, End, IdemKey, Sha) ->
    talk_record_t07_support:register_part(
        Uid,
        Rid,
        Cid,
        #{
            <<"seq">> => Seq,
            <<"start_ms">> => Start,
            <<"end_ms">> => End,
            <<"sha256">> => Sha,
            <<"byte_size">> => 4096,
            <<"codec">> => <<"aac">>
        },
        IdemKey
    ).

%% The fixture owns this BEAM VM/database window: equality detects accidental
%% assets in every scope, including a regression binding to the wrong record.
attachment_ids(Conn) ->
    {ok, Rows} = elib_pg:query(Conn, <<"SELECT id FROM attachment ORDER BY id">>, []),
    [maps:get(<<"id">>, Row) || Row <- Rows].

%% Ordinary U2 creates its enterprise record through the real context gate.
%% U1 remains the enterprise's active Human owner throughout both revoke cases.
organization_upload_fixture(Conn, WithWorkspace) ->
    Ids = talk_record_t07_support:ids(),
    Uid = maps:get(u2, Ids),
    Org = maps:get(org_a, Ids),
    Ws = maps:get(ws_a1, Ids),
    Base = #{
        <<"client_recording_id">> => ?REC1,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"organization">>,
        <<"organization_id">> => Org
    },
    Body =
        case WithWorkspace of
            false ->
                Base;
            true ->
                {ok, 1} = elib_pg:execute(
                    Conn,
                    <<
                        "INSERT INTO workspace_member(workspace_id,user_id,role,status)"
                        " VALUES($1,$2,'member','active')"
                    >>,
                    [Ws, Uid]
                ),
                Base#{<<"workspace_id">> => Ws}
        end,
    {ok, created, Record, _} = talk_record_logic:create(Uid, Body, <<"org-upload-record">>),
    Rid = maps:get(<<"id">>, Record),
    {ok, Cid} = talk_record_t07_support:create_capture(Uid, Rid, ?DS1, <<"org-upload-capture">>),
    {ok, created, P} = part(Uid, Rid, Cid, 0, 0, 30000, <<"org-upload-part">>),
    {Uid, Rid, Cid, P}.

%% Same narrow initial-registration seam used by the W403 short capability tests.
%% VM-exclusive meck intercepts only input parameters; SQL and results stay real.

upload_identity(Conn, Key) ->
    %% Capture every actual pending column, not a guessed/frozen subset. Only
    %% upload_expires_at may change during renewal; actor, scope/ref, JSON subject/
    %% declaration/content, session state, link, storage identity and deadline stay.
    {ok, [Row]} = elib_pg:query(
        Conn,
        <<
            "SELECT to_jsonb(p)-'upload_expires_at' AS identity,"
            "extract(epoch from session_deadline)::bigint AS deadline,"
            "extract(epoch from upload_expires_at)::bigint AS expiry"
            " FROM attach_pending p WHERE object_key=$1"
        >>,
        [Key]
    ),
    Identity0 = maps:get(<<"identity">>, Row),
    Identity =
        case Identity0 of
            B when is_binary(B) -> jsone:decode(B);
            M -> M
        end,
    true = is_map(Identity),
    true = lists:all(
        fun(Name) -> maps:is_key(Name, Identity) end,
        [
            <<"object_key">>,
            <<"bucket">>,
            <<"creator_user_id">>,
            <<"scope">>,
            <<"scope_ref">>,
            <<"client_upload_id">>,
            <<"upload_registration">>,
            <<"upload_content">>,
            <<"session_state">>,
            <<"confirmed_attachment_id">>,
            <<"session_deadline">>,
            <<"storage_driver">>,
            <<"storage_namespace">>,
            <<"storage_container">>,
            <<"storage_locator_sha256">>
        ]
    ),
    Row#{<<"identity">> => Identity}.
