%%% @doc talk_record T12 eunit：持久 job worker / complete / assemble /
%%% jobs 生命周期 / processing-requests / 迟到结果拒绝。
%%?
%%% 覆盖（T12 卡验收面）：
%%%   * complete：202 链路（job 投递 + manifest 固化）、幂等重放、
%%%     dedupe 不重复投递、缺段 409 missing_sequences、manifest_hash 冲突、
%%%     capture 未结束、门禁（If-Match 412/viewer 403/陌生人 404/缺幂等键）；
%%%   * assemble：成功全链（media available + 段永久引用 + manifest 正确性、
%%%     乱序分段归位、确定性 hash）、processing lease（运行期 bound、取消
%%%     释放、到期清扫）、迟到结果拒绝（取消后 lease_lost / 删除后
%%%     record_deleted / manifest 变更 input_stale——均不发布）；
%%%   * worker：崩溃后过期租约 reclaim（attempt 保留）、未过期不回收、
%%%     瞬时错误退避重试后恢复、attempts 上限终态、transcribe/summarize
%%%     处理器未选型诚实挂起（不 claim suspended）、export/purge 骨架挂起；
%%%   * 生命周期 API：jobs 列表（可重试标记/可见性）、retry（owner/editor
%%%     角色面、仅可重试失败、幂等重放）、processing-requests（默认 503
%%%     处理器未选型、能力开后 202+同意事件+mode 推进、media 未就绪 409）。
-module(talk_record_t12_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REC(N), rec_uuid(N)).
-define(HEX64(S), begin
    Hex = integer_to_binary(S, 16),
    Pad = <<<<$0>> || _ <- lists:seq(1, 64 - byte_size(Hex))>>,
    <<Pad/binary, Hex/binary>>
end).

setup() ->
    State0 = talk_record_t12_support:setup_all(),
    talk_record_t12_support:seed(State0),
    ProcessorPrevious = application:get_env(imboy, talk_record_processor),
    application:unset_env(imboy, talk_record_processor),
    State0#{
        processor_prev => ProcessorPrevious,
        caps_prev => application:get_env(imboy, talk_record_capabilities),
        worker_enabled_prev => application:get_env(imboy, talk_record_job_worker_enabled),
        worker_tick_prev => application:get_env(imboy, talk_record_job_worker_tick_ms)
    }.

cleanup(State) ->
    %% 防泄漏：worker gen_server 用例失败时兜底停止
    catch gen_server:stop(whereis(talk_record_job_worker)),
    case lists:member(talk_record_attachment_adapter, meck:mocked()) of
        true -> catch meck:unload(talk_record_attachment_adapter);
        false -> ok
    end,
    case lists:member(elib_pg, meck:mocked()) of
        true -> catch meck:unload(elib_pg);
        false -> ok
    end,
    restore_env(talk_record_processor, maps:get(processor_prev, State, undefined)),
    restore_env(talk_record_capabilities, maps:get(caps_prev, State, undefined)),
    restore_env(talk_record_job_worker_enabled, maps:get(worker_enabled_prev, State, undefined)),
    restore_env(talk_record_job_worker_tick_ms, maps:get(worker_tick_prev, State, undefined)),
    talk_record_t12_support:teardown_all(State).

restore_env(_Key, undefined) ->
    application:unset_env(imboy, _Key);
restore_env(Key, {ok, Value}) ->
    application:set_env(imboy, Key, Value).

-spec rec_uuid(integer()) -> binary().
rec_uuid(N) ->
    Hex = integer_to_binary(N, 16),
    PadLen = max(0, 12 - byte_size(Hex)),
    Pad = <<<<$0>> || _ <- lists:seq(1, PadLen)>>,
    <<"00000040-0000-0000-0000-", Pad/binary, Hex/binary>>.

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%%%===================================================================
%%% complete 端点链路（202 + 缺段 409 + 幂等 + dedupe）
%%%===================================================================

complete_happy_path_and_idempotency_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _Parts}} = talk_record_t12_support:ready_record(
            U1, ?REC(1), <<"record_only">>, [0, 1], <<"t12-hp-1">>
        ),
        {ok, created, Payload} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-hp-c-1">>),
        JobId = maps:get(<<"job_id">>, Payload),
        ?assert(is_integer(JobId)),
        ?assertEqual(<<"assembling">>, maps:get(<<"media_status">>, Payload)),
        ?assert(is_binary(maps:get(<<"manifest_hash">>, Payload))),
        %% media 投影 + job 落库（queued；输入版本绑定 input_manifest_hash）
        ?assertEqual(<<"assembling">>, talk_record_t12_support:media_status(C, Rid)),
        Job = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"assemble">>, maps:get(<<"kind">>, Job)),
        ?assertEqual(<<"queued">>, maps:get(<<"state">>, Job)),
        ?assert(is_binary(maps:get(<<"input_manifest_hash">>, Job))),
        %% manifest 固化行（§3：固化在 complete 事务中）
        Asm = talk_record_t12_support:one(
            C,
            "SELECT * FROM talk_record_assembly WHERE record_id = " ++ integer_to_list(Rid)
        ),
        ?assertEqual(maps:get(<<"input_manifest_hash">>, Job), maps:get(<<"manifest_hash">>, Asm)),
        %% Q-L1：固化行回填产出 job（manifest 先落、assemble job 后投递同事务）
        ?assertEqual(JobId, maps:get(<<"job_id">>, Asm)),
        ?assertEqual(2, maps:get(<<"part_count">>, Asm)),
        ?assertEqual(null, maps:get(<<"audio_attachment_id">>, Asm)),
        %% 同 Idempotency-Key 同 body → 幂等重放（同 job_id，不二次投递）
        {ok, replay, P2} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-hp-c-1">>),
        ?assertEqual(JobId, maps:get(<<"job_id">>, P2)),
        NJobs = talk_record_t12_support:one(
            C,
            "SELECT count(*) AS n FROM talk_record_job WHERE record_id = " ++ integer_to_list(Rid)
        ),
        ?assertEqual(1, maps:get(<<"n">>, NJobs)),
        %% 同键异 body（last_seq 不同 → digest 不同）→ 幂等冲突
        ?assertEqual(
            {error, idempotency_conflict},
            talk_record_t12_support:complete_body(U1, Rid, Cid, <<"t12-hp-c-1">>, #{last_seq => 0})
        )
    end).

complete_rejects_new_key_after_submission_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(2), <<"record_only">>, [0, 1], <<"t12-dd-2">>
        ),
        {ok, created, P1} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-dd-c-1">>),
        %% 已接受清单的超时恢复重放原键；新键不得再次提交处理。
        {ok, replay, P2} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-dd-c-1">>),
        ?assertEqual(P1, P2),
        ?assertEqual(
            {error, completion_already_submitted},
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-dd-c-2">>)
        ),
        RejectedKey = talk_record_t12_support:one(
            C,
            "SELECT count(*) AS n FROM talk_record_idempotency WHERE actor_id = " ++
                integer_to_list(U1) ++ " AND idempotency_key = 't12-dd-c-2'"
        ),
        ?assertEqual(0, maps:get(<<"n">>, RejectedKey)),
        NJobs = talk_record_t12_support:one(
            C,
            "SELECT count(*) AS n FROM talk_record_job WHERE record_id = " ++ integer_to_list(Rid)
        ),
        ?assertEqual(1, maps:get(<<"n">>, NJobs))
    end).

complete_same_key_concurrent_replay_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        U1 = maps:get(u1, talk_record_t12_support:ids()),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(190), <<"record_only">>, [0, 1], <<"t12-concurrent">>
        ),
        Results = parallel_complete(U1, Rid, Cid, <<"t12-concurrent-key">>),
        [{ok, created, P1}, {ok, replay, P2}] = lists:sort(Results),
        ?assertEqual(P1, P2),
        Jobs = talk_record_t12_support:one(
            C,
            "SELECT count(*) AS n FROM talk_record_job WHERE record_id = " ++ integer_to_list(Rid)
        ),
        ?assertEqual(1, maps:get(<<"n">>, Jobs)),
        Idem = talk_record_t12_support:one(
            C,
            "SELECT count(*) AS n FROM talk_record_idempotency WHERE actor_id = " ++
                integer_to_list(U1) ++ " AND idempotency_key = 't12-concurrent-key'"
        ),
        ?assertEqual(1, maps:get(<<"n">>, Idem))
    end).

parallel_complete(Uid, Rid, Cid, Key) ->
    Parent = self(),
    Ref = make_ref(),
    Workers = [
        spawn_monitor(fun() ->
            Parent ! {Ref, ready, self()},
            receive
                {Ref, go} ->
                    Result =
                        try
                            talk_record_t12_support:complete(Uid, Rid, Cid, Key)
                        catch
                            Class:Reason -> {worker_exception, Class, Reason}
                        end,
                    Parent ! {Ref, self(), Result}
            end
        end)
     || _ <- [1, 2]
    ],
    try
        lists:foreach(
            fun({Pid, _}) ->
                receive
                    {Ref, ready, Pid} -> ok
                after 10000 -> error(concurrent_complete_start_timeout)
                end
            end,
            Workers
        ),
        lists:foreach(fun({Pid, _}) -> Pid ! {Ref, go} end, Workers),
        [
            receive
                {Ref, Pid, Result} -> Result;
                {'DOWN', Monitor, process, Pid, Reason} -> error({worker_exit, Reason})
            after 30000 -> error(concurrent_complete_result_timeout)
            end
         || {Pid, Monitor} <- Workers
        ]
    after
        lists:foreach(
            fun({Pid, Monitor}) ->
                case is_process_alive(Pid) of
                    true ->
                        exit(Pid, kill),
                        receive
                            {'DOWN', Monitor, process, Pid, _} -> ok
                        after 5000 -> error(concurrent_complete_cleanup_timeout)
                        end;
                    false ->
                        ok
                end,
                erlang:demonitor(Monitor, [flush])
            end,
            Workers
        )
    end.

complete_missing_sequences_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(3), <<"record_only">>, [0, 1], <<"t12-ms-3">>
        ),
        %% 声明 last_seq=2：seq 2 缺失（未注册）
        ?assertMatch(
            {error,
                {missing_sequences, #{
                    <<"capture_id">> := Cid, <<"missing">> := [2]
                }}},
            talk_record_t12_support:complete_body(U1, Rid, Cid, <<"t12-ms-c">>, #{last_seq => 2})
        ),
        %% 未 confirm 的分段同样算缺（seq 1 置回 pending）
        talk_record_t12_support:sql(
            C,
            "UPDATE talk_record_audio_part SET upload_status='pending' WHERE seq = 1"
        ),
        ?assertMatch(
            {error, {missing_sequences, #{<<"missing">> := [1]}}},
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-ms-c2">>)
        ),
        %% 失败不投递 job、不推进 media
        NJobs = talk_record_t12_support:one(
            C,
            "SELECT count(*) AS n FROM talk_record_job WHERE record_id = " ++ integer_to_list(Rid)
        ),
        ?assertEqual(0, maps:get(<<"n">>, NJobs)),
        ?assertEqual(<<"local_only">>, talk_record_t12_support:media_status(C, Rid))
    end).

complete_manifest_hash_conflict_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(4), <<"record_only">>, [0, 1], <<"t12-mh-4">>
        ),
        Bad = talk_record_t12_support:sha(999),
        ?assertEqual(
            {error, manifest_hash_conflict},
            talk_record_t12_support:complete_body(
                U1,
                Rid,
                Cid,
                <<"t12-mh-c">>,
                #{
                    last_seq => 1,
                    <<"captures">> => [
                        #{
                            <<"capture_id">> => Cid,
                            <<"last_seq">> => 1,
                            <<"manifest_hash">> => Bad
                        }
                    ]
                }
            )
        )
    end).

complete_capture_not_ended_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        %% 只建 record + capture（未发 end 事件）——capture 未 ended 不可 complete
        {ok, Rid} = create_rec(U1, ?REC(5), <<"t12-ce-5">>),
        {ok, Cid} = talk_record_t07_support:create_capture(
            U1,
            Rid,
            <<"10000000-0000-0000-0000-000000000001">>,
            <<"t12-ce-cap">>
        ),
        ?assertEqual(
            {error, capture_not_ended},
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-ce-c">>)
        )
    end).

complete_gates_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        U7 = maps:get(u7, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(6), <<"record_only">>, [0, 1], <<"t12-gt-6">>
        ),
        %% If-Match 过期 → 412
        ?assertEqual(
            {error, precondition_failed},
            talk_record_job_logic:complete(U1, Rid, <<"\"9\"">>, <<"t12-gt-412">>, #{
                <<"captures">> => [#{<<"capture_id">> => Cid, <<"last_seq">> => 1}]
            })
        ),
        %% viewer（有活跃 grant）→ 403
        insert_grant(C, Rid, U1, U2, <<"viewer">>),
        ?assertEqual(
            {error, forbidden},
            talk_record_t12_support:complete(U2, Rid, Cid, <<"t12-gt-403">>)
        ),
        %% 陌生人 → 404（不泄露存在性）
        ?assertEqual(
            {error, not_found},
            talk_record_t12_support:complete(U7, Rid, Cid, <<"t12-gt-404">>)
        ),
        %% 缺幂等键 → 422
        ?assertEqual(
            {error, idempotency_key_required},
            talk_record_job_logic:complete(
                U1,
                Rid,
                <<"\"1\"">>,
                undefined,
                #{<<"captures">> => [#{<<"capture_id">> => Cid, <<"last_seq">> => 1}]}
            )
        )
    end).

complete_transcribe_mode_gates_and_enqueue_test_() ->
    with_db(fun(State) ->
        Previous = application:get_env(imboy, talk_record_processor),
        application:unset_env(imboy, talk_record_processor),
        try
            complete_transcribe_enqueue_component(State)
        after
            restore_env(talk_record_processor, Previous)
        end
    end).

%% VM-exclusive capability seam proves consent/enqueue transactions only.
%% No processor is executed: this is not ASR, diarization or model acceptance.
complete_transcribe_enqueue_component(State) ->
    C = maps:get(conn, State),
    U1 = maps:get(u1, talk_record_t12_support:ids()),
    {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
        U1, ?REC(7), <<"transcribe">>, [0, 1], <<"t12-tr-7">>
    ),
    Base = #{<<"captures">> => [#{<<"capture_id">> => Cid, <<"last_seq">> => 1}]},
    Consent = #{
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"processor_config_version">> => <<"pc-v1">>
    },
    Body = Base#{<<"processing_consent">> => Consent},
    Complete = fun(Key, B) ->
        talk_record_job_logic:complete(U1, Rid, <<"\"1\"">>, Key, B)
    end,
    ?assertEqual({error, processing_consent_required}, Complete(<<"t12-tr-no">>, Base)),
    ?assertEqual(null, talk_record_processor_config:version()),
    ?assertEqual(false, maps:get(<<"asr_enabled">>, talk_record_ds:capabilities())),
    ?assertEqual({error, processing_unavailable}, Complete(<<"t12-tr-ok">>, Body)),
    ?assertEqual(0, enqueue_component_count(C, Rid, jobs)),
    ?assertEqual(0, enqueue_component_count(C, Rid, consent)),
    NoKey = talk_record_t12_support:one(
        C,
        "SELECT count(*) AS n FROM talk_record_idempotency WHERE actor_id = " ++
            integer_to_list(U1) ++ " AND idempotency_key = 't12-tr-ok'"
    ),
    ?assertEqual(0, maps:get(<<"n">>, NoKey)),
    with_enqueue_capability(fun() ->
        BadPolicy = Body#{<<"processing_consent">> => Consent#{<<"policy_version">> => <<"old">>}},
        BadProcessor = Body#{
            <<"processing_consent">> =>
                Consent#{<<"processor_config_version">> => <<"old">>}
        },
        ?assertEqual({error, processing_unavailable}, Complete(<<"t12-tr-pv">>, BadPolicy)),
        ?assertEqual({error, processing_unavailable}, Complete(<<"t12-tr-pcv">>, BadProcessor)),
        ?assertEqual(0, enqueue_component_count(C, Rid, jobs)),
        ?assertEqual(0, enqueue_component_count(C, Rid, consent)),
        %% Reuse the genuinely rejected key: failed capability checks cannot burn it.
        {ok, created, P} = Complete(<<"t12-tr-ok">>, Body),
        ?assert(is_integer(maps:get(<<"transcribe_job_id">>, P))),
        ?assertEqual(<<"assembling">>, maps:get(<<"media_status">>, P)),
        {ok, replay, Replay} = Complete(<<"t12-tr-ok">>, Body),
        ?assertEqual(P, Replay),
        ?assertEqual(2, enqueue_component_count(C, Rid, jobs)),
        ?assertEqual(1, enqueue_component_count(C, Rid, consent))
    end),
    ?assertEqual(null, talk_record_processor_config:version()),
    ?assertEqual(false, maps:get(<<"asr_enabled">>, talk_record_ds:capabilities())).

enqueue_component_count(C, Rid, Kind) ->
    Tail =
        case Kind of
            jobs ->
                "talk_record_job WHERE record_id = " ++ integer_to_list(Rid) ++
                    " AND kind IN ('assemble','transcribe')";
            consent ->
                "talk_record_consent_event WHERE record_id = " ++ integer_to_list(Rid) ++
                    " AND kind = 'processing_opt_in'"
        end,
    maps:get(<<"n">>, talk_record_t12_support:one(C, "SELECT count(*) AS n FROM " ++ Tail)).

%%%===================================================================
%%% assemble：成功链 / manifest 正确性 / lease / 迟到拒绝
%%%===================================================================

worker_assemble_success_full_chain_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, Parts}} = talk_record_t12_support:ready_record(
            U1, ?REC(8), <<"record_only">>, [0, 1], <<"t12-as-8">>
        ),
        {ok, created, #{<<"job_id">> := JobId}} =
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-as-c">>),
        {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
        %% media 发布 + job 终态
        ?assertEqual(<<"available">>, talk_record_t12_support:media_status(C, Rid)),
        Job = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"succeeded">>, maps:get(<<"state">>, Job)),
        ?assert(maps:get(<<"finished_at">>, Job) =/= null),
        %% 段永久业务引用（audio_part:<part_id>，lease_until NULL——GC 永久可见）
        Perms = talk_record_t12_support:permanent_refs(C, Rid),
        ?assertEqual(length(Parts), length(Perms)),
        ?assert(
            lists:all(
                fun(R) ->
                    maps:get(<<"state">>, R) =:= <<"bound">> andalso
                        maps:get(<<"lease_until">>, R) =:= null
                end,
                Perms
            )
        ),
        %% job processing lease 已释放（成功路径退场）
        ?assertEqual([], talk_record_t12_support:lease_refs(C, JobId, bound)),
        ?assertEqual(2, length(talk_record_t12_support:lease_refs(C, JobId, released)))
    end).

manifest_build_pure_test_() ->
    with_db(fun(_State) ->
        Sha = talk_record_t12_support:sha(0),
        Parts = [
            #{
                <<"seq">> => 0,
                <<"id">> => 101,
                <<"attachment_id">> => 11,
                <<"sha256">> => Sha,
                <<"byte_size">> => 1024,
                <<"start_ms">> => 0,
                <<"end_ms">> => 30_000
            },
            #{
                <<"seq">> => 1,
                <<"id">> => 102,
                <<"attachment_id">> => 12,
                <<"sha256">> => Sha,
                <<"byte_size">> => 2048,
                <<"start_ms">> => 30_000,
                <<"end_ms">> => 75_000
            }
        ],
        M = talk_record_assembly_logic:build_manifest(77, Parts),
        ?assertEqual(2, length(maps:get(<<"parts">>, M))),
        [S0, S1] = maps:get(<<"parts">>, M),
        ?assertEqual(0, maps:get(<<"offset_ms">>, S0)),
        ?assertEqual(30_000, maps:get(<<"duration_ms">>, S0)),
        ?assertEqual(30_000, maps:get(<<"offset_ms">>, S1)),
        ?assertEqual(45_000, maps:get(<<"duration_ms">>, S1)),
        ?assertEqual(75_000, maps:get(<<"total_duration_ms">>, M)),
        ?assertEqual(3072, maps:get(<<"total_bytes">>, M)),
        ?assertMatch(<<_:64/binary>>, maps:get(<<"manifest_hash">>, M)),
        %% 确定性：同一输入两次构建同 hash
        M2 = talk_record_assembly_logic:build_manifest(77, Parts),
        ?assertEqual(maps:get(<<"manifest_hash">>, M), maps:get(<<"manifest_hash">>, M2)),
        %% 缺段纯函数
        ?assertEqual([], talk_record_assembly_logic:missing_sequences(Parts, 1)),
        ?assertEqual([2], talk_record_assembly_logic:missing_sequences(Parts, 2))
    end).

out_of_order_registration_manifest_ordered_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        %% seq 1 先注册、seq 0 后注册（离线乱序补传归位）
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(9), <<"record_only">>, [1, 0], <<"t12-oo-9">>
        ),
        {ok, created, _} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-oo-c1">>),
        A1 = talk_record_t12_support:one(
            C,
            "SELECT segments, manifest_hash FROM talk_record_assembly WHERE record_id = " ++
                integer_to_list(Rid)
        ),
        %% jsonb 列以 JSON 二进制回读（epgsql 默认 codec 透传），显式解码
        Segs = jsone:decode(maps:get(<<"segments">>, A1)),
        ?assertEqual(2, length(Segs)),
        ?assertEqual(0, maps:get(<<"seq">>, hd(Segs))),
        ?assertEqual(1, maps:get(<<"seq">>, lists:last(Segs))),
        ?assertEqual(0, maps:get(<<"offset_ms">>, hd(Segs))),
        ?assertEqual(30_000, maps:get(<<"offset_ms">>, lists:last(Segs))),
        %% 确定性：同一记录分段重算 manifest 与固化行哈希一致
        {ok, Parts} = elib_pg:with_tx(
            fun(Conn) -> talk_record_assembly_logic:confirmed_parts_tx(Conn, Rid) end,
            [{reraise, false}]
        ),
        Recomputed = talk_record_assembly_logic:build_manifest(Cid, Parts),
        ?assertEqual(
            maps:get(<<"manifest_hash">>, A1),
            maps:get(<<"manifest_hash">>, Recomputed)
        )
    end).

processing_lease_bound_and_cancel_releases_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, Parts}} = talk_record_t12_support:ready_record(
            U1, ?REC(11), <<"record_only">>, [0, 1], <<"t12-lc-11">>
        ),
        {ok, created, #{<<"job_id">> := JobId}} =
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-lc-c">>),
        %% 模拟 worker：claim → prepare（lease 授予）——不 finish
        {ok, Job} = claim_job(),
        ?assertEqual(JobId, maps:get(<<"id">>, Job)),
        {ok, Prepared} = elib_pg:with_tx(
            fun(Conn) -> talk_record_assembly_logic:prepare_tx(Conn, Job) end,
            [{reraise, false}]
        ),
        ?assertMatch(#{parts := _, manifest := _}, Prepared),
        %% 运行期：processing lease bound 且有期限（§10.3）
        Leases = talk_record_t12_support:lease_refs(C, JobId, bound),
        ?assertEqual(length(Parts), length(Leases)),
        ?assert(lists:all(fun(R) -> maps:get(<<"lease_until">>, R) =/= null end, Leases)),
        %% 取消（删除流程入口）：job cancelled + lease 立即释放
        {ok, 1} = talk_record_job_logic:cancel_record_jobs(Rid),
        ?assertEqual([], talk_record_t12_support:lease_refs(C, JobId, bound)),
        ?assertEqual(length(Parts), length(talk_record_t12_support:lease_refs(C, JobId, released))),
        %% 迟到 finish：CAS 落空，结果不发布
        ?assertEqual(
            {error, lease_lost},
            elib_pg:with_tx(
                fun(Conn) -> talk_record_assembly_logic:finish_tx(Conn, Job, Prepared) end,
                [{reraise, false}]
            )
        ),
        ?assertEqual(<<"assembling">>, talk_record_t12_support:media_status(C, Rid)),
        ?assertEqual(
            <<"cancelled">>, maps:get(<<"state">>, talk_record_t12_support:job_row(C, JobId))
        )
    end).

late_result_rejected_after_delete_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        %% 场景 A：删除后取消（T20 删除流接线形态）→ CAS 落空
        {ok, {RidA, CidA, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(12), <<"record_only">>, [0, 1], <<"t12-dl-12">>
        ),
        {ok, created, _} = talk_record_t12_support:complete(U1, RidA, CidA, <<"t12-dl-c1">>),
        {ok, JobA} = claim_job(),
        {ok, PreparedA} = elib_pg:with_tx(
            fun(Conn) -> talk_record_assembly_logic:prepare_tx(Conn, JobA) end, [{reraise, false}]
        ),
        talk_record_t12_support:sql(
            C,
            "UPDATE talk_record SET deleted_at = now() WHERE id = " ++ integer_to_list(RidA)
        ),
        {ok, 1} = talk_record_job_logic:cancel_record_jobs(RidA),
        ?assertEqual(
            {error, lease_lost},
            elib_pg:with_tx(
                fun(Conn) -> talk_record_assembly_logic:finish_tx(Conn, JobA, PreparedA) end,
                [{reraise, false}]
            )
        ),
        ?assertEqual(<<"assembling">>, talk_record_t12_support:media_status(C, RidA)),
        %% 场景 B：只软删、未取消（防御纵深）→ fencing 直接拒绝
        {ok, {RidB, CidB, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(13), <<"record_only">>, [0, 1], <<"t12-dl-13">>
        ),
        {ok, created, _} = talk_record_t12_support:complete(U1, RidB, CidB, <<"t12-dl-c2">>),
        {ok, JobB} = claim_job(),
        {ok, PreparedB} = elib_pg:with_tx(
            fun(Conn) -> talk_record_assembly_logic:prepare_tx(Conn, JobB) end, [{reraise, false}]
        ),
        talk_record_t12_support:sql(
            C,
            "UPDATE talk_record SET deleted_at = now() WHERE id = " ++ integer_to_list(RidB)
        ),
        ?assertEqual(
            {error, record_deleted},
            elib_pg:with_tx(
                fun(Conn) -> talk_record_assembly_logic:finish_tx(Conn, JobB, PreparedB) end,
                [{reraise, false}]
            )
        ),
        ?assertEqual(<<"assembling">>, talk_record_t12_support:media_status(C, RidB))
    end).

input_stale_manifest_changed_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(14), <<"record_only">>, [0, 1], <<"t12-is-14">>
        ),
        {ok, created, _} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-is-c">>),
        {ok, Job} = claim_job(),
        {ok, Prepared} = elib_pg:with_tx(
            fun(Conn) -> talk_record_assembly_logic:prepare_tx(Conn, Job) end, [{reraise, false}]
        ),
        %% prepare 与 finish 之间 manifest 被改（模拟重 complete 产生新清单）：
        %% job 输入绑定（input_manifest_hash）已过时 → 迟到结果拒绝发布
        talk_record_t12_support:sql(
            C,
            "UPDATE talk_record_assembly SET manifest_hash = '" ++ binary_to_list(?HEX64(1)) ++
                "' WHERE record_id = " ++ integer_to_list(Rid)
        ),
        ?assertEqual(
            {error, input_stale},
            elib_pg:with_tx(
                fun(Conn) -> talk_record_assembly_logic:finish_tx(Conn, Job, Prepared) end,
                [{reraise, false}]
            )
        ),
        ?assertEqual(<<"assembling">>, talk_record_t12_support:media_status(C, Rid)),
        %% input_stale 为证据性失败：不可人工重试（重跑同一输入无意义）
        ?assert(not talk_record_job_logic:retryable_error(<<"input_stale">>))
    end).

worker_crash_reap_and_reclaim_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(15), <<"record_only">>, [0, 1], <<"t12-cr-15">>
        ),
        {ok, created, #{<<"job_id">> := JobId}} =
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-cr-c">>),
        %% 模拟 worker 崩溃：claim 后不 finish，行留 running
        {ok, _CrashedJob} = claim_job(),
        CrashRow = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"running">>, maps:get(<<"state">>, CrashRow)),
        ?assertEqual(1, maps:get(<<"attempt">>, CrashRow)),
        %% 租约未过期：claim 不到（SKIP LOCKED + 候选谓词排除 running）
        ?assertEqual({ok, done}, talk_record_job_worker:run_once()),
        %% 未过期 reaper 不回收
        ?assertEqual({ok, 0}, talk_record_job_worker:reap_expired()),
        %% 租约过期 → 零副作用回收（attempt 保留）→ 重投递成功
        talk_record_t12_support:sql(
            C,
            "UPDATE talk_record_job SET lease_until = now() - interval '1 second' WHERE id = " ++
                integer_to_list(JobId)
        ),
        ?assertEqual({ok, 1}, talk_record_job_worker:reap_expired()),
        Reclaimed = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"queued">>, maps:get(<<"state">>, Reclaimed)),
        ?assertEqual(1, maps:get(<<"attempt">>, Reclaimed)),
        ?assertEqual(null, maps:get(<<"lease_token">>, Reclaimed)),
        {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
        Final = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"succeeded">>, maps:get(<<"state">>, Final)),
        ?assertEqual(2, maps:get(<<"attempt">>, Final)),
        ?assertEqual(<<"available">>, talk_record_t12_support:media_status(C, Rid))
    end).

transient_failure_backoff_and_recovery_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(16), <<"record_only">>, [0, 1], <<"t12-bk-16">>
        ),
        {ok, created, #{<<"job_id">> := JobId}} =
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-bk-c">>),
        %% 注入瞬时失败：processing lease 授予报 db_error（一次尝试）
        ok = meck_adapter_grant_error(db_error),
        {ok, #{outcome := requeued, reason := <<"lease_grant_failed">>}} =
            talk_record_job_worker:run_once(),
        Row1 = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"retry_wait">>, maps:get(<<"state">>, Row1)),
        ?assertEqual(<<"lease_grant_failed">>, maps:get(<<"error_code">>, Row1)),
        ?assert(maps:get(<<"retry_at">>, Row1) =/= null),
        ?assertEqual(1, maps:get(<<"attempt">>, Row1)),
        %% 退避窗口内不可再抢
        ?assertEqual({ok, done}, talk_record_job_worker:run_once()),
        ok = meck:unload(talk_record_attachment_adapter),
        %% 到期重投递：恢复成功
        talk_record_t12_support:sql(
            C,
            "UPDATE talk_record_job SET retry_at = now() - interval '1 second' WHERE id = " ++
                integer_to_list(JobId)
        ),
        {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"available">>, talk_record_t12_support:media_status(C, Rid)),
        ?assertEqual(2, maps:get(<<"attempt">>, talk_record_t12_support:job_row(C, JobId)))
    end).

tick_db_failure_no_crash_backoff_test_() ->
    with_db(fun(_State) ->
        %% Q-M1：tick 全链 DB 故障（with_tx 恒 error）——reconcile 容错返回、
        %% gen_server 不崩溃、fails 退避计数增长（有界唤醒，非 badmatch 崩溃）
        ok = application:set_env(imboy, talk_record_job_worker_enabled, true),
        ok = application:set_env(imboy, talk_record_job_worker_tick_ms, 500),
        ok = meck_with_tx_error(),
        ?assertMatch(
            {error, {reap, injected_tx_error}},
            talk_record_job_worker:reconcile_tick()
        ),
        {ok, Pid} = talk_record_job_worker:start_link(),
        timer:sleep(1200),
        ?assert(erlang:is_process_alive(Pid)),
        #{fails := Fails} = sys:get_state(Pid),
        ?assert(Fails >= 1),
        ?assertMatch(
            {error, {reap, injected_tx_error}},
            talk_record_job_worker:reconcile_tick()
        ),
        catch gen_server:stop(Pid),
        ok = meck:unload(elib_pg)
    end).

prepare_db_error_transient_not_terminal_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(27), <<"record_only">>, [0, 1], <<"t12-qm2-27">>
        ),
        {ok, created, #{<<"job_id">> := JobId}} =
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-qm2-c">>),
        %% Q-M2：记录门禁 SELECT 读错误（瞬时抖动）——不得误判 record_deleted
        %% 走终态 cancelled；应分类 db_error → retry_wait 退避（attempt 保留）
        ok = meck_query_fail_on(<<"SELECT deleted_at FROM talk_record">>),
        {ok, #{outcome := requeued, reason := <<"db_error">>}} =
            talk_record_job_worker:run_once(),
        Row = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"retry_wait">>, maps:get(<<"state">>, Row)),
        ?assertEqual(<<"db_error">>, maps:get(<<"error_code">>, Row)),
        ?assertEqual(1, maps:get(<<"attempt">>, Row)),
        %% 恢复（卸载注入 + retry_at 提前）后自动重投递成功
        ok = meck:unload(elib_pg),
        talk_record_t12_support:sql(
            C,
            "UPDATE talk_record_job SET retry_at = now() - interval '1 second' WHERE id = " ++
                integer_to_list(JobId)
        ),
        {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"available">>, talk_record_t12_support:media_status(C, Rid))
    end).

backoff_seconds_pure_test_() ->
    ?_assertEqual(
        [2, 4, 8, 60, 60],
        [talk_record_job_worker:backoff_seconds(N) || N <- [1, 2, 3, 6, 20]]
    ).

attempts_exhausted_terminal_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(17), <<"record_only">>, [0, 1], <<"t12-ax-17">>
        ),
        {ok, created, #{<<"job_id">> := JobId}} =
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-ax-c">>),
        %% attempt 预置到上限（模拟毒行反复回收）：claim 后直接终态
        talk_record_t12_support:sql(
            C,
            "UPDATE talk_record_job SET attempt = 6 WHERE id = " ++ integer_to_list(JobId)
        ),
        {ok, #{outcome := failed, error_code := <<"attempts_exhausted">>}} =
            talk_record_job_worker:run_once(),
        Final = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"failed">>, maps:get(<<"state">>, Final)),
        ?assertEqual(<<"attempts_exhausted">>, maps:get(<<"error_code">>, Final)),
        ?assert(maps:get(<<"finished_at">>, Final) =/= null),
        %% Terminal assembly failure must not leave a perpetual assembling UI.
        ?assertEqual(<<"retryable_failed">>, talk_record_t12_support:media_status(C, Rid)),
        ?assertEqual({ok, done}, talk_record_job_worker:run_once()),
        ?assertEqual(Final, talk_record_t12_support:job_row(C, JobId)),
        %% Failure cannot publish a manifest/audio pointer, derived ledger or ref.
        {ok, [Projection]} = elib_pg:query(
            C,
            <<
                "SELECT r.audio_manifest_attachment_id,a.audio_attachment_id"
                " FROM talk_record r JOIN talk_record_assembly a ON a.record_id=r.id"
                " WHERE r.id=$1"
            >>,
            [Rid]
        ),
        ?assertEqual(
            #{
                <<"audio_manifest_attachment_id">> => null,
                <<"audio_attachment_id">> => null
            },
            Projection
        ),
        {ok, [#{<<"n">> := 0}]} = elib_pg:query(
            C,
            <<
                "SELECT count(*) n FROM attachment_derived_write w"
                " JOIN talk_record_job j ON j.id=w.source_job_id WHERE j.record_id=$1"
            >>,
            [Rid]
        ),
        {ok, [#{<<"n">> := 0}]} = elib_pg:query(
            C,
            <<
                "SELECT count(*) n FROM talk_record_attachment_ref"
                " WHERE record_id=$1 AND role IN ('manifest','transcoded_audio','export')"
            >>,
            [Rid]
        ),
        %% The two real source-part references remain intact and permanent.
        {ok, [#{<<"n">> := 2}]} = elib_pg:query(
            C,
            <<
                "SELECT count(*) n FROM talk_record_attachment_ref WHERE record_id=$1"
                " AND role='audio_part' AND state='bound' AND lease_until IS NULL"
            >>,
            [Rid]
        ),
        %% attempts_exhausted 不可人工重试
        ?assert(not talk_record_job_logic:retryable_error(<<"attempts_exhausted">>))
    end).

transcribe_suspends_waiting_processor_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(18), <<"transcribe">>, [0, 1], <<"t12-sp-18">>
        ),
        {ok, created, _} = with_enqueue_capability(fun() ->
            talk_record_job_logic:complete(U1, Rid, <<"\"1\"">>, <<"t12-sp-c">>, #{
                <<"captures">> => [#{<<"capture_id">> => Cid, <<"last_seq">> => 1}],
                <<"processing_consent">> => #{
                    <<"policy_version">> => <<"talk-record-notice-v1">>,
                    <<"processor_config_version">> => <<"pc-v1">>
                }
            })
        end),
        ?assertEqual(null, talk_record_processor_config:version()),
        ?assertEqual(false, maps:get(<<"asr_enabled">>, talk_record_ds:capabilities())),
        %% assemble 先跑成功，transcribe 随后挂起（waiting_processor，不伪造成功）
        {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
        {ok, #{outcome := suspended, kind := <<"transcribe">>}} = talk_record_job_worker:run_once(),
        TJob = talk_record_t12_support:one(
            C,
            "SELECT * FROM talk_record_job WHERE record_id = " ++ integer_to_list(Rid) ++
                " AND kind = 'transcribe'"
        ),
        ?assertEqual(<<"retry_wait">>, maps:get(<<"state">>, TJob)),
        ?assert(maps:get(<<"suspended">>, TJob)),
        ?assertEqual(<<"waiting_processor">>, maps:get(<<"error_code">>, TJob)),
        %% suspended 不被再抢（不烧 attempts）
        ?assertEqual({ok, done}, talk_record_job_worker:run_once()),
        %% 选型后 unsuspend（T13 接线入口）：恢复调度、再次诚实挂起
        {ok, 1} = elib_pg:with_tx(
            fun(Conn) -> talk_record_job_repo:unsuspend_kind_tx(Conn, <<"transcribe">>) end,
            [{reraise, false}]
        ),
        {ok, #{outcome := suspended, kind := <<"transcribe">>}} = talk_record_job_worker:run_once(),
        TJob2 = talk_record_t12_support:one(
            C,
            "SELECT state, attempt, suspended, error_code FROM talk_record_job WHERE record_id = " ++
                integer_to_list(Rid) ++ " AND kind = 'transcribe'"
        ),
        ?assertEqual(2, maps:get(<<"attempt">>, TJob2)),
        ?assert(maps:get(<<"suspended">>, TJob2))
    end).

export_missing_binding_and_purge_guard_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, _Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(19), <<"record_only">>, [0], <<"t12-ep-19">>
        ),
        %% 无导出行绑定的作业必须终态拒绝，不能再悬挂为未接线骨架。
        {ok, created, ExportJob} = elib_pg:with_tx(
            fun(Conn) ->
                talk_record_job_repo:enqueue_tx(Conn, #{
                    record_id => Rid,
                    kind => <<"export">>,
                    dedupe_key => <<"export:", (integer_to_binary(Rid))/binary>>
                })
            end,
            [{reraise, false}]
        ),
        {ok, #{outcome := failed, error_code := not_found}} = talk_record_job_worker:run_once(),
        ERow = talk_record_t12_support:job_row(C, ExportJob),
        ?assertEqual(<<"failed">>, maps:get(<<"state">>, ERow)),
        ?assertEqual(<<"not_found">>, maps:get(<<"error_code">>, ERow)),
        ?assertEqual(
            {error, export_requires_worker},
            talk_record_job_logic:execute_export_tx(C, ERow)
        ),
        %% purge 已由 T20 接线：未软删记录的 purge 行防御拒绝
        %% （record_not_deleted——同输入重试无意义，不伪造协调成功）
        ?assertEqual(
            {error, record_not_deleted},
            talk_record_job_logic:execute_purge_tx(C, ERow)
        )
    end).

%%%===================================================================
%%% jobs 列表 / retry / processing-requests
%%%===================================================================

jobs_list_payload_and_visibility_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        U7 = maps:get(u7, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(20), <<"record_only">>, [0, 1], <<"t12-jl-20">>
        ),
        {ok, created, #{<<"job_id">> := JobId}} =
            talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-jl-c">>),
        {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
        %% 人工造两类 failed：可重试 db_error / 不可重试 input_stale
        ok = talk_record_t12_support:sql(
            C,
            "INSERT INTO talk_record_job (id, record_id, kind, dedupe_key, state, attempt,"
            " error_code, finished_at, suspended, created_at, updated_at) VALUES (990001, " ++
                integer_to_list(Rid) ++
                ", 'assemble', 'x-dup-a', 'failed', 1, 'db_error', now(),"
                " false, now(), now())"
        ),
        ok = talk_record_t12_support:sql(
            C,
            "INSERT INTO talk_record_job (id, record_id, kind, dedupe_key, state, attempt,"
            " error_code, finished_at, suspended, created_at, updated_at) VALUES (990002, " ++
                integer_to_list(Rid) ++
                ", 'assemble', 'x-dup-b', 'failed', 1, 'input_stale',"
                " now(), false, now(), now())"
        ),
        %% owner 查列表：状态面 + 可重试标记 + 归一化错误码
        {ok, #{<<"jobs">> := Jobs}} = talk_record_job_logic:list_jobs(U1, Rid),
        ById = maps:from_list([{maps:get(<<"job_id">>, J), J} || J <- Jobs]),
        ?assertMatch(
            #{<<"state">> := <<"succeeded">>, <<"retryable">> := false},
            maps:get(JobId, ById)
        ),
        ?assertMatch(
            #{<<"retryable">> := true, <<"error_code">> := <<"db_error">>},
            maps:get(990001, ById)
        ),
        ?assertMatch(
            #{<<"retryable">> := false, <<"error_code">> := <<"input_stale">>},
            maps:get(990002, ById)
        ),
        %% viewer 有读权可查；陌生人 404
        insert_grant(C, Rid, U1, U2, <<"viewer">>),
        ?assertMatch({ok, #{<<"jobs">> := [_ | _]}}, talk_record_job_logic:list_jobs(U2, Rid)),
        ?assertEqual({error, not_found}, talk_record_job_logic:list_jobs(U7, Rid))
    end).

retry_flow_and_idempotency_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, _Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(21), <<"record_only">>, [0, 1], <<"t12-rt-21">>
        ),
        ok = talk_record_t12_support:sql(
            C,
            "INSERT INTO talk_record_job (id, record_id, kind, dedupe_key, state, attempt,"
            " error_code, finished_at, suspended, created_at, updated_at) VALUES (990010, " ++
                integer_to_list(Rid) ++
                ", 'assemble', 'x-dup-c', 'failed', 3, 'db_error',"
                " now(), false, now(), now())"
        ),
        {ok, created, Payload} = talk_record_job_logic:retry_job(
            U1, Rid, 990010, <<"\"1\"">>, <<"t12-rt-k1">>
        ),
        ?assertEqual(<<"queued">>, maps:get(<<"state">>, Payload)),
        ?assertEqual(0, maps:get(<<"attempt">>, Payload)),
        Row = talk_record_t12_support:job_row(C, 990010),
        ?assertEqual(<<"queued">>, maps:get(<<"state">>, Row)),
        ?assertEqual(null, maps:get(<<"error_code">>, Row)),
        ?assertEqual(null, maps:get(<<"finished_at">>, Row)),
        %% 同幂等键重放：返回首次结果（幂等）
        {ok, replay, P2} = talk_record_job_logic:retry_job(
            U1, Rid, 990010, <<"\"1\"">>, <<"t12-rt-k1">>
        ),
        ?assertEqual(<<"queued">>, maps:get(<<"state">>, P2)),
        %% 已 queued（非 failed）再 retry（异键）→ 409 job_not_retryable
        ?assertEqual(
            {error, job_not_retryable},
            talk_record_job_logic:retry_job(U1, Rid, 990010, <<"\"1\"">>, <<"t12-rt-k2">>)
        )
    end).

retry_role_gates_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        U3 = maps:get(u3, Ids),
        U7 = maps:get(u7, Ids),
        {ok, {Rid, _Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(22), <<"record_only">>, [0, 1], <<"t12-rg-22">>
        ),
        insert_grant(C, Rid, U1, U2, <<"viewer">>),
        insert_grant(C, Rid, U1, U3, <<"editor">>),
        Rows = [
            {990020, <<"transcribe">>}, {990021, <<"assemble">>}, {990022, <<"export">>}
        ],
        lists:foreach(
            fun({Id, Kind}) ->
                ok = talk_record_t12_support:sql(
                    C,
                    "INSERT INTO talk_record_job (id, record_id, kind, dedupe_key, state, attempt,"
                    " error_code, finished_at, suspended, created_at, updated_at) VALUES (" ++
                        integer_to_list(Id) ++ ", " ++ integer_to_list(Rid) ++ ", '" ++
                        binary_to_list(Kind) ++ "', 'x-dup-" ++ integer_to_list(Id) ++
                        "', 'failed', 1, 'db_error', now(), false, now(), now())"
                )
            end,
            Rows
        ),
        %% editor：transcribe 可（§4 owner 或 editor 仅转写/摘要）
        ?assertMatch(
            {ok, created, _},
            talk_record_job_logic:retry_job(
                U3, Rid, 990020, <<"\"1\"">>, <<"t12-rg-e1">>
            )
        ),
        %% editor：assemble 不可 → 403
        ?assertEqual(
            {error, forbidden},
            talk_record_job_logic:retry_job(
                U3, Rid, 990021, <<"\"1\"">>, <<"t12-rg-e2">>
            )
        ),
        %% viewer：任何 kind → 403
        ?assertEqual(
            {error, forbidden},
            talk_record_job_logic:retry_job(
                U2, Rid, 990021, <<"\"1\"">>, <<"t12-rg-v">>
            )
        ),
        %% owner：assemble 可；export（T19 请求者限定）→ 403
        ?assertMatch(
            {ok, created, _},
            talk_record_job_logic:retry_job(
                U1, Rid, 990021, <<"\"1\"">>, <<"t12-rg-o1">>
            )
        ),
        ?assertEqual(
            {error, forbidden},
            talk_record_job_logic:retry_job(
                U1, Rid, 990022, <<"\"1\"">>, <<"t12-rg-o2">>
            )
        ),
        %% 陌生人 → 404；异记录 job → 404（不泄露）
        ?assertEqual(
            {error, not_found},
            talk_record_job_logic:retry_job(
                U7, Rid, 990020, <<"\"1\"">>, <<"t12-rg-s">>
            )
        )
    end).

retry_non_retryable_rejected_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, _Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(23), <<"record_only">>, [0, 1], <<"t12-nr-23">>
        ),
        ok = talk_record_t12_support:sql(
            C,
            "INSERT INTO talk_record_job (id, record_id, kind, dedupe_key, state, attempt,"
            " error_code, finished_at, suspended, created_at, updated_at) VALUES (990030, " ++
                integer_to_list(Rid) ++
                ", 'assemble', 'x-dup-d', 'failed', 1,"
                " 'missing_sequences', now(), false, now(), now())"
        ),
        ?assertEqual(
            {error, job_not_retryable},
            talk_record_job_logic:retry_job(U1, Rid, 990030, <<"\"1\"">>, <<"t12-nr-k">>)
        ),
        %% 不存在的 job → 404
        ?assertEqual(
            {error, not_found},
            talk_record_job_logic:retry_job(U1, Rid, 999999, <<"\"1\"">>, <<"t12-nr-404">>)
        )
    end).

processing_requests_unavailable_503_test_() ->
    with_db(fun(_State) ->
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(24), <<"record_only">>, [0, 1], <<"t12-pu-24">>
        ),
        {ok, created, _} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-pu-c">>),
        {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
        %% V1 默认能力面 asr_enabled=false：处理器未选型如实 503
        ?assertEqual(
            {error, processing_unavailable},
            talk_record_job_logic:processing_request(U1, Rid, <<"\"1\"">>, <<"t12-pu-k">>, #{
                <<"mode">> => <<"transcribe">>,
                <<"processing_consent">> => #{
                    <<"policy_version">> => <<"talk-record-notice-v1">>,
                    <<"processor_config_version">> => <<"pc-v1">>
                }
            })
        ),
        ?assertEqual(
            {error, processing_unavailable},
            talk_record_job_logic:processing_request(U1, Rid, <<"\"1\"">>, <<"t12-pu-k2">>, #{
                <<"mode">> => <<"summarize">>,
                <<"processing_consent">> => #{
                    <<"policy_version">> => <<"talk-record-notice-v1">>,
                    <<"processor_config_version">> => <<"pc-v1">>
                }
            })
        )
    end).

processing_requests_full_flow_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(25), <<"record_only">>, [0, 1], <<"t12-pf-25">>
        ),
        {ok, created, _} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-pf-c">>),
        {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
        %% Synthetic enqueue gate only; provider remains absent for the real worker.
        {ok, created, P} = with_enqueue_capability(fun() ->
            talk_record_job_logic:processing_request(
                U1, Rid, <<"\"1\"">>, <<"t12-pf-k">>, #{
                    <<"mode">> => <<"transcribe">>,
                    <<"processing_consent">> => #{
                        <<"policy_version">> => <<"talk-record-notice-v1">>,
                        <<"processor_config_version">> => <<"pc-v1">>
                    }
                }
            )
        end),
        JobId = maps:get(<<"job_id">>, P),
        ?assert(is_integer(JobId)),
        ?assertEqual(2, maps:get(<<"revision">>, P)),
        %% 记录推进：processing_mode + revision；同意事件落库
        Rec = talk_record_t12_support:one(
            C,
            "SELECT processing_mode, revision FROM talk_record WHERE id = " ++ integer_to_list(Rid)
        ),
        ?assertEqual(<<"transcribe">>, maps:get(<<"processing_mode">>, Rec)),
        ?assertEqual(2, maps:get(<<"revision">>, Rec)),
        Consent = talk_record_t12_support:one(
            C,
            "SELECT count(*) AS n FROM talk_record_consent_event WHERE record_id = " ++
                integer_to_list(Rid) ++ " AND kind = 'processing_opt_in'"
        ),
        ?assertEqual(1, maps:get(<<"n">>, Consent)),
        ?assertEqual(null, talk_record_processor_config:version()),
        ?assertEqual(false, maps:get(<<"asr_enabled">>, talk_record_ds:capabilities())),
        %% job 真实排队；worker 侧处理器仍未选型 → 诚实挂起（不伪造成功）
        {ok, #{outcome := suspended, kind := <<"transcribe">>}} = talk_record_job_worker:run_once(),
        TJob = talk_record_t12_support:job_row(C, JobId),
        ?assertEqual(<<"waiting_processor">>, maps:get(<<"error_code">>, TJob)),
        %% 幂等重放
        {ok, replay, P2} = with_enqueue_capability(fun() ->
            talk_record_job_logic:processing_request(
                U1, Rid, <<"\"2\"">>, <<"t12-pf-k">>, #{
                    <<"mode">> => <<"transcribe">>,
                    <<"processing_consent">> => #{
                        <<"policy_version">> => <<"talk-record-notice-v1">>,
                        <<"processor_config_version">> => <<"pc-v1">>
                    }
                }
            )
        end),
        ?assertEqual(JobId, maps:get(<<"job_id">>, P2))
    end).

processing_requests_gates_test_() ->
    with_db(fun(State) ->
        C = maps:get(conn, State),
        Ids = talk_record_t12_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        Consent = #{
            <<"policy_version">> => <<"talk-record-notice-v1">>,
            <<"processor_config_version">> => <<"pc-v1">>
        },
        %% 缺幂等键 / 非法 mode / 缺同意
        ?assertEqual(
            {error, idempotency_key_required},
            talk_record_job_logic:processing_request(U1, 1, <<"\"1\"">>, undefined, #{
                <<"mode">> => <<"transcribe">>, <<"processing_consent">> => Consent
            })
        ),
        ?assertEqual(
            {error, invalid_processing_mode},
            talk_record_job_logic:processing_request(U1, 1, <<"\"1\"">>, <<"t12-pg-1">>, #{
                <<"mode">> => <<"record_only">>
            })
        ),
        ?assertEqual(
            {error, processing_consent_required},
            talk_record_job_logic:processing_request(U1, 1, <<"\"1\"">>, <<"t12-pg-2">>, #{
                <<"mode">> => <<"transcribe">>
            })
        ),
        %% media 未就绪（assembling，未跑 worker）→ 409（能力开前提下）
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(26), <<"record_only">>, [0, 1], <<"t12-pg-26">>
        ),
        {ok, created, _} = talk_record_t12_support:complete(U1, Rid, Cid, <<"t12-pg-c">>),
        ?assertEqual(
            {error, media_not_ready},
            with_enqueue_capability(fun() ->
                talk_record_job_logic:processing_request(U1, Rid, <<"\"1\"">>, <<"t12-pg-3">>, #{
                    <<"mode">> => <<"transcribe">>, <<"processing_consent">> => Consent
                })
            end)
        ),
        %% viewer → 403
        insert_grant(C, Rid, U1, U2, <<"viewer">>),
        ?assertEqual(
            {error, forbidden},
            talk_record_job_logic:processing_request(U2, Rid, <<"\"1\"">>, <<"t12-pg-4">>, #{
                <<"mode">> => <<"transcribe">>, <<"processing_consent">> => Consent
            })
        )
    end).

%%%===================================================================
%%% 内部
%%%===================================================================

%% @private 手动 claim 最老一条 job（模拟 worker claim 后崩溃/挂起场景）。
claim_job() ->
    elib_pg:with_tx(
        fun(Conn) ->
            talk_record_job_repo:claim_next_tx(
                Conn,
                <<"t12-test">>,
                <<"50000000-0000-0000-0000-000000000001">>,
                120
            )
        end,
        [{reraise, false}]
    ).

%% @private meck 注入：processing lease 授予失败（passthrough 其余函数）。
meck_adapter_grant_error(Reason) ->
    case lists:member(talk_record_attachment_adapter, meck:mocked()) of
        true ->
            ok;
        false ->
            ok = meck:new(talk_record_attachment_adapter, [passthrough, unstick, no_link])
    end,
    ok = meck:expect(
        talk_record_attachment_adapter,
        grant_processing_lease_tx,
        fun(_Conn, _Rid, _Aid, _Role, _Key, _Secs) -> {error, Reason} end
    ).

%% @private meck 注入：SQL 命中 SqlFragment 时 elib_pg:query/3 返回 DB 错误
%% （其余 passthrough；Q-M2 用例——记录门禁读抖动不得误判删除）。
meck_query_fail_on(SqlFragment) ->
    case lists:member(elib_pg, meck:mocked()) of
        true ->
            ok;
        false ->
            ok = meck:new(elib_pg, [passthrough, unstick, no_link])
    end,
    ok = meck:expect(
        elib_pg,
        query,
        fun(Conn, Sql, Params) ->
            SqlBin = iolist_to_binary(Sql),
            case binary:match(SqlBin, SqlFragment) of
                nomatch ->
                    meck:passthrough([Conn, SqlBin, Params]);
                _ ->
                    {error, injected_db_error}
            end
        end
    ).

%% @private meck 注入：elib_pg:with_tx/2 恒失败（Q-M1 用例——tick 全链 DB
%% 故障时 gen_server 不得崩溃，走有界唤醒退避）。
meck_with_tx_error() ->
    case lists:member(elib_pg, meck:mocked()) of
        true ->
            ok;
        false ->
            ok = meck:new(elib_pg, [passthrough, unstick, no_link])
    end,
    ok = meck:expect(
        elib_pg,
        with_tx,
        fun(_Fun, _Opts) -> {error, injected_tx_error} end
    ).

%% @private 建 record（无 capture）——capture_not_ended 用。
create_rec(Uid, ClientRecordingId, IdemPrefix) ->
    Body = #{
        <<"client_recording_id">> => ClientRecordingId,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"title">> => <<"t12"/utf8>>
    },
    case talk_record_logic:create(Uid, Body, <<IdemPrefix/binary, "-rec">>) of
        {ok, _Mark, Payload, _ETag} -> {ok, maps:get(<<"id">>, Payload)};
        {error, Reason} -> {error, Reason}
    end.

%% @private 插入活跃 grant（t07 测试同款形态）。
insert_grant(Conn, RecordId, Grantor, Grantee, Role) ->
    {ok, _} = elib_pg:query(
        Conn,
        <<"INSERT INTO talk_record_grant (id, record_id, grantee_user_id, role, granted_by)",
            " VALUES ($1, $2, $3, $4, $5)">>,
        [9800000 + Grantee, RecordId, Grantee, Role, Grantor]
    ),
    ok.

%% Only capability/version are synthetic; real SQL, consent and worker stay intact.
with_enqueue_capability(Fun) ->
    true = not lists:member(talk_record_processor_config, meck:mocked()),
    ok = meck:new(talk_record_processor_config, [passthrough, no_link]),
    try
        ok = meck:expect(talk_record_processor_config, capabilities, fun(Cap) ->
            Cap#{
                <<"asr_enabled">> => true,
                <<"summary_enabled">> => false,
                <<"processing_available">> => true,
                <<"processor_config_version">> => <<"pc-v1">>
            }
        end),
        ok = meck:expect(talk_record_processor_config, version, fun() -> <<"pc-v1">> end),
        Fun()
    after
        ok = meck:unload(talk_record_processor_config)
    end.
