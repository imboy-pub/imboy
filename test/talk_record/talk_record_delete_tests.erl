%%% @doc talk_record T20 删除/清理/注销预检闭环测试（logic + worker 层）。
%%%
%%% 覆盖（BACKEND-API §4 DELETE /:id、§10.3、§10.4、§11.3；T21 冻结契约）：
%%%   A. DELETE 主流程（owner+If-Match、428/412/403/404、幂等重放）
%%%   B. 撤权即时性（grants revoked、playback 票据失效、新读全拒）
%%%   C. 删除后 owner 白名单（形状、非 owner 404、protected/retryable 标志）
%%%   D. 七态矩阵（pending→blocked_retention/blocked_hold/
%%%      retained_by_other_reference/failed/complete 推进；真 disk 队列路径）
%%%   E. 迟到模型结果不发布（在途 job 取消 + worker 无可抢 + purge 自身存活）
%%%   F. 与 PUT grants 并发互斥（同一行 FOR UPDATE 串行化，两序终态一致）
%%%   G. 注销预检（facts 形状/个人 blocker/企业不级联 + run/1 六域集成 +
%%%      custody handover 投影）
%%%   H. T06-L2 修复回归（cast 洁净假设/ILIKE 转义/LIMIT 钳制）
-module(talk_record_delete_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REC(N), <<"30000000-0000-0000-0000-0000000000", (integer_to_binary(N))/binary>>).

%%%===================================================================
%%% 支撑
%%%===================================================================

setup() ->
    State = talk_record_t20_support:setup_all(),
    talk_record_t20_support:seed(State),
    State.

cleanup(State) ->
    talk_record_t20_support:teardown_all(State).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%% complete 就绪记录（complete 投 assemble → 先消费成功），返回 {Rid, Cid, U1}。
%% presign 分段会话统一排空（upload_expires_at 置过去——模拟上传能力窗口
%% 关闭；否则 intent fn 的 attach_pending 能力检查使清理误判 retained）。
ready(State, N) ->
    Ids = talk_record_t20_support:ids(),
    U1 = maps:get(u1, Ids),
    {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
        U1, ?REC(N), <<"record_only">>, [0, 1], <<"t20-", (integer_to_binary(N))/binary>>
    ),
    {ok, created, _} = talk_record_t12_support:complete(
        U1, Rid, Cid, <<"t20-", (integer_to_binary(N))/binary, "-cmp">>
    ),
    %% assemble job 消费成功（record_only 无模型 job）
    {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
    drain_upload_sessions(State, Rid),
    {Rid, Cid, U1}.

drain_upload_sessions(State, Rid) ->
    C = maps:get(conn, State),
    talk_record_t20_support:sql(C, [
        "UPDATE attach_pending SET upload_expires_at = now() - interval '2 days'",
        " WHERE object_key LIKE 'talk_record/",
        integer_to_binary(Rid),
        "/%'"
    ]),
    %% V5"完整位置"资格：分段附件补 storage 四元组（模拟真实上传后的
    %% 位置事实；disk 真字节路径测试再覆盖真实 locator digest）
    talk_record_t20_support:sql(C, [
        "UPDATE attachment SET storage_driver='disk', storage_namespace='t20-ns',",
        " storage_container='t20c', storage_locator_sha256='",
        binary:copy(<<"c">>, 64),
        "' WHERE path LIKE 'talk_record/",
        integer_to_binary(Rid),
        "/%'"
    ]).

grant_viewer(Conn, RecordId, Uid) ->
    talk_record_t20_support:sql(Conn, [
        "INSERT INTO talk_record_grant(id, record_id, grantee_user_id, role, granted_by)",
        " VALUES (",
        integer_to_binary(RecordId + 900000),
        ", ",
        integer_to_binary(RecordId),
        ", ",
        integer_to_binary(Uid),
        ", 'viewer', 700001)"
    ]).

row(Conn, Sql) ->
    talk_record_t20_support:one(Conn, Sql).

%%%===================================================================
%%% A. DELETE 主流程
%%%===================================================================

delete_owner_202_pending_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _Cid, U1} = ready(State, 81),
        {ok, deleted, Payload} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        ?assertEqual(true, maps:get(<<"logical_deleted">>, Payload)),
        ?assertEqual(<<"pending">>, maps:get(<<"cleanup_status">>, Payload)),
        ?assert(maps:get(<<"deleted_at">>, Payload) =/= null),
        Row = row(
            C,
            "SELECT deleted_at, cleanup_status, revision, acl_revision,"
            " custody_status FROM talk_record WHERE id = " ++ integer_to_binary(Rid)
        ),
        ?assert(maps:get(<<"deleted_at">>, Row) =/= null),
        ?assertEqual(<<"pending">>, maps:get(<<"cleanup_status">>, Row)),
        ?assertEqual(2, maps:get(<<"revision">>, Row)),
        ?assertEqual(2, maps:get(<<"acl_revision">>, Row)),
        %% purge job 已投递（dedupe attempt 0）
        ?assertMatch([#{<<"state">> := <<"queued">>}], talk_record_t20_support:purge_jobs(Rid))
    end).

delete_preconditions_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        U7 = maps:get(u7, Ids),
        {Rid, _, _} = ready(State, 82),
        %% 缺 If-Match → 428
        ?assertEqual(
            {error, precondition_required},
            talk_record_delete_logic:delete(U1, Rid, undefined)
        ),
        %% 非法 If-Match → 422
        ?assertEqual(
            {error, invalid_if_match},
            talk_record_delete_logic:delete(U1, Rid, <<"abc">>)
        ),
        %% 旧 revision → 412
        ?assertEqual(
            {error, precondition_failed},
            talk_record_delete_logic:delete(U1, Rid, <<"99">>)
        ),
        %% viewer（可见非 owner）→ forbidden
        grant_viewer(C, Rid, U2),
        ?assertEqual(
            {error, forbidden},
            talk_record_delete_logic:delete(U2, Rid, <<"1">>)
        ),
        %% 陌生人 → not_found（不披露存在性）
        ?assertEqual(
            {error, not_found},
            talk_record_delete_logic:delete(U7, Rid, <<"1">>)
        )
    end).

delete_idempotent_replay_test_() ->
    with_db(fun(State) ->
        {Rid, _, U1} = ready(State, 83),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        %% 重放（任意旧版本号——重放先返回首次结果再校验版本，§3）
        {ok, replay, Payload2} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        ?assertEqual(true, maps:get(<<"logical_deleted">>, Payload2)),
        ?assertEqual(<<"pending">>, maps:get(<<"cleanup_status">>, Payload2)),
        %% 非 owner 重放 → 404
        Ids = talk_record_t20_support:ids(),
        ?assertEqual(
            {error, not_found},
            talk_record_delete_logic:delete(maps:get(u2, Ids), Rid, <<"1">>)
        )
    end).

delete_fences_active_capture_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid, Cid, _} = ready(State, 84),
        %% 复活一条 recording capture + active_capture_id 投影
        talk_record_t20_support:sql(C, [
            "INSERT INTO talk_record_capture(id, record_id, device_session_id, device_id,"
            " started_by, status) VALUES (",
            integer_to_binary(Cid + 5),
            ", ",
            integer_to_binary(Rid),
            ", '10000000-0000-0000-0000-00000000dead', 'dev2', ",
            integer_to_binary(U1),
            ", 'recording')"
        ]),
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET active_capture_id = ",
            integer_to_binary(Cid + 5),
            " WHERE id = ",
            integer_to_binary(Rid)
        ]),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        Row = row(
            C,
            "SELECT status, lease_epoch FROM talk_record_capture WHERE id = " ++
                integer_to_binary(Cid + 5)
        ),
        ?assertEqual(<<"interrupted">>, maps:get(<<"status">>, Row)),
        ?assertEqual(1, maps:get(<<"lease_epoch">>, Row)),
        ?assertEqual(
            null,
            maps:get(
                <<"active_capture_id">>,
                row(
                    C,
                    "SELECT active_capture_id FROM talk_record WHERE id = " ++
                        integer_to_binary(Rid)
                )
            )
        )
    %% active_capture_id 投影已清（deleted_at 在上一断言组已验）
    end).

%% 分段附件真实字节落盘（disk driver put + locator 对齐 registry）。
seg_put_bytes(C, Rid, Ns, Container, Digest) ->
    talk_record_t20_support:sql(C, [
        "UPDATE attachment SET storage_namespace='",
        Ns,
        "', storage_container='",
        Container,
        "', storage_locator_sha256='",
        Digest,
        "' WHERE path LIKE 'talk_record/",
        integer_to_binary(Rid),
        "/%'"
    ]),
    {ok, Rows} = elib_pg:query(
        C,
        <<"SELECT path FROM attachment WHERE path LIKE $1">>,
        [<<"talk_record/", (integer_to_binary(Rid))/binary, "/%">>]
    ),
    lists:foreach(
        fun(#{<<"path">> := P}) ->
            ok = attachment_storage_disk:put(Ns, Container, P, <<"t20-seg">>)
        end,
        Rows
    ).

%%%===================================================================
%%% B. 撤权即时性
%%%===================================================================

revocation_immediate_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {Rid, _, _} = ready(State, 85),
        grant_viewer(C, Rid, U2),
        %% 删除前 U2 可读
        ?assertMatch({ok, _}, talk_record_ds:detail(U2, Rid)),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        %% 全部活跃 grant 已撤销
        Revoked = row(C, [
            "SELECT count(*) AS n FROM talk_record_grant WHERE record_id = ",
            integer_to_binary(Rid),
            " AND revoked_at IS NULL"
        ]),
        ?assertEqual(0, maps:get(<<"n">>, Revoked)),
        %% find_active_grant 拒绝（撤权后授权链失效）
        ?assertMatch({error, not_found}, talk_record_repo:find_active_grant(Rid, U2)),
        %% 新读全拒：U2 detail 404（不落入白名单——仅 owner）
        ?assertMatch({error, not_found}, talk_record_logic:detail(U2, Rid)),
        %% owner detail 走白名单（不再返回正文 payload 形状）
        {ok, WL} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(true, maps:get(<<"logical_deleted">>, WL)),
        ?assert(maps:get(<<"deleted_at">>, WL) =/= null),
        %% 撤回的授权使 adapter 附件授权链同步失效（记录删除分支）
        ?assertEqual(
            false,
            talk_record_attachment_adapter:authorize(
                U2, #{<<"id">> => 1, <<"scope_ref">> => integer_to_binary(Rid)}
            )
        )
    end).

playback_ticket_revoked_on_delete_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {Rid, _, _} = ready(State, 86),
        grant_viewer(C, Rid, U2),
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET media_status='available', duration_ms=60000 WHERE id = ",
            integer_to_binary(Rid)
        ]),
        %% U2 开票成功
        {ok, #{<<"ticket">> := Ticket}} = talk_record_grant_playback:open(U2, Rid, 100),
        ?assertMatch({ok, _}, talk_record_grant_playback:verify_ticket(Ticket)),
        %% 删除 → 同一票据立即失效（Range/逐段重验点，§11.3）
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        ?assertMatch(
            {error, not_found},
            talk_record_grant_playback:verify_ticket(Ticket)
        ),
        %% owner 也无法再开票（软删后 visible 失败）
        ?assertMatch(
            {error, not_found},
            talk_record_grant_playback:open(U1, Rid, 0)
        )
    end).

%%%===================================================================
%%% C. 删除后 owner 白名单
%%%===================================================================

whitelist_shape_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid, _, _} = ready(State, 87),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        {ok, WL} = talk_record_logic:detail(U1, Rid),
        %% 白名单键集（§11.3：无标题/正文/参与者/授权名单）
        Allowed = [
            <<"id">>,
            <<"logical_deleted">>,
            <<"cleanup_status">>,
            <<"physical_delete_state">>,
            <<"revision">>,
            <<"deleted_at">>,
            <<"capabilities_for_actor">>
        ],
        ?assertEqual(lists:sort(Allowed), lists:sort(maps:keys(WL))),
        ?assertEqual(
            false,
            maps:get(
                <<"read_content">>,
                maps:get(<<"capabilities_for_actor">>, WL)
            )
        ),
        %% 标题等正文域确实不出现
        ?assertNot(maps:is_key(<<"title">>, WL)),
        ?assertNot(maps:is_key(<<"notes">>, WL)),
        ?assertNot(maps:is_key(<<"participants">>, WL))
    end).

whitelist_flags_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid, _, _} = ready(State, 88),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        %% blocked_retention → protected 标志
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET cleanup_status='blocked_retention' WHERE id = ",
            integer_to_binary(Rid)
        ]),
        {ok, WL1} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(true, maps:get(protected, WL1)),
        ?assertNot(maps:is_key(retryable, WL1)),
        %% failed → retryable 标志
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET cleanup_status='failed' WHERE id = ",
            integer_to_binary(Rid)
        ]),
        {ok, WL2} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(true, maps:get(retryable, WL2)),
        %% pending → 两者皆无
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET cleanup_status='pending' WHERE id = ",
            integer_to_binary(Rid)
        ]),
        {ok, WL3} = talk_record_logic:detail(U1, Rid),
        ?assertNot(maps:is_key(protected, WL3)),
        ?assertNot(maps:is_key(retryable, WL3))
    end).

%%%===================================================================
%%% D. 七态矩阵（purge 协调 + 物理队列回读）
%%%===================================================================

%% none：未删除初始态 + 纯函数
project_outcomes_pure_functions_test_() ->
    [
        ?_assertEqual(
            <<"pending">>,
            talk_record_cleanup_logic:project_outcomes(
                [
                    #{path => <<"a">>, outcome => intended},
                    #{path => <<"b">>, outcome => tombstone}
                ]
            )
        ),
        ?_assertEqual(
            <<"retained_by_other_reference">>,
            talk_record_cleanup_logic:project_outcomes(
                [
                    #{path => <<"a">>, outcome => intended},
                    #{path => <<"b">>, outcome => retained_by_other_reference}
                ]
            )
        ),
        ?_assertEqual(
            <<"failed">>,
            talk_record_cleanup_logic:project_outcomes(
                [#{path => <<"a">>, outcome => asset_row_missing}]
            )
        ),
        ?_assertEqual(
            deleted,
            talk_record_cleanup_logic:project_physical_state(
                #{<<"lifecycle_state">> => <<"deleted">>}
            )
        ),
        ?_assertEqual(
            deleted,
            talk_record_cleanup_logic:project_physical_state(#{<<"queue_done">> => true})
        ),
        ?_assertEqual(
            failed,
            talk_record_cleanup_logic:project_physical_state(
                #{<<"queue_state">> => <<"blocked">>, <<"queue_exhausted">> => true}
            )
        ),
        ?_assertEqual(
            none,
            talk_record_cleanup_logic:project_physical_state(
                #{<<"queue_state">> => null, <<"lifecycle_state">> => <<"live">>}
            )
        ),
        ?_assertEqual(
            deleting,
            talk_record_cleanup_logic:project_physical_state(
                #{<<"queue_state">> => <<"ready">>}
            )
        )
    ].

%% pending → complete（无附件记录：协调即完成）
purge_complete_no_attachments_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, U1} = ready(State, 90),
        %% 装配"无附件记录"：清除分段附件引用行（未上传任何字节的草稿形态）
        talk_record_t20_support:sql(C, [
            "DELETE FROM talk_record_attachment_ref WHERE record_id = ", integer_to_binary(Rid)
        ]),
        {ok, deleted, P0} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        ?assertEqual(<<"none">>, maps:get(<<"physical_delete_state">>, P0)),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"pending">>, talk_record_t20_support:cleanup_status(Rid)),
        %% worker tick 回读（无附件 → complete）
        {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
        ?assertEqual(<<"complete">>, talk_record_t20_support:cleanup_status(Rid)),
        %% purge job succeeded 终态
        [PJob] = talk_record_t20_support:purge_jobs(Rid),
        ?assertEqual(<<"succeeded">>, maps:get(<<"state">>, PJob))
    end).

%% pending →（协调 intended）→ 真 disk 队列消费 → complete
purge_complete_disk_queue_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, U1} = ready(State, 91),
        Root = talk_record_t20_support:disk_root(disk1),
        #{namespace := Ns, container := Container} = talk_record_t20_support:disk_registry_apply(
            Root
        ),
        Digest = talk_record_t20_support:disk_locator_digest(),
        Path = talk_record_t20_support:insert_attachment(C, 8101, #{
            record_id => Rid,
            storage_driver => <<"disk">>,
            storage_namespace => Ns,
            storage_container => Container,
            storage_locator => Digest
        }),
        ok = attachment_storage_disk:put(Ns, Container, Path, <<"t20-bytes">>),
        ok = talk_record_t20_support:bind_ref(C, Rid, 8101, <<"audio_part:1">>),
        %% 分段附件换真实 locator + 真字节落盘（完整位置资格 + worker 可删）
        seg_put_bytes(C, Rid, Ns, Container, Digest),
        {ok, deleted, P0} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        ?assertEqual(<<"none">>, maps:get(<<"physical_delete_state">>, P0)),
        %% purge 协调：引用释放 + intent 入队（物理未完成 → pending）
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"pending">>, talk_record_t20_support:cleanup_status(Rid)),
        {St, _AnyCount} = talk_record_t20_support:ref_state(C, Rid),
        ?assertEqual(<<"released">>, St),
        ?assertEqual(<<"delete_intent">>, talk_record_t20_support:attachment_state(C, Path)),
        %% 既有附件 worker：claim → disk DELETE → ack done（真路径；
        %% 3 任务 = 8101 + 两段分段附件——全部真字节落盘后可删）
        {ok, 3} = attachment_delete_worker:run_batch(
            C, fun(_Ns2, _C2, _K, I) -> attachment_storage:delete_snapshot(I) end
        ),
        %% ack 后墓碑保持 delete_intent（行删除归队列 ack 协议后续；
        %% 物理完成证据 = 队列 done + 字节消失）
        ?assertEqual(<<"delete_intent">>, talk_record_t20_support:attachment_state(C, Path)),
        DiskFile = filename:join([
            talk_record_t20_support:disk_root(disk1),
            "t20c",
            binary_to_list(Path)
        ]),
        ?assertEqual({error, enoent}, file:read_file(DiskFile)),
        %% 状态回读 → complete（queue done → deleted 投影）
        {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
        ?assertEqual(<<"complete">>, talk_record_t20_support:cleanup_status(Rid)),
        %% 白名单 physical_delete_state 投影 deleted
        {ok, WL} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(<<"deleted">>, maps:get(<<"physical_delete_state">>, WL))
    end).

%% retention_until 未到期 → blocked_retention（引用保持 bound，GC 谓词继续挡）
purge_blocked_retention_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, U1} = ready(State, 92),
        Path = talk_record_t20_support:insert_attachment(C, 8201, #{record_id => Rid}),
        ok = talk_record_t20_support:bind_ref(C, Rid, 8201, <<"audio_part:1">>),
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET retention_until = now() + interval '30 days' WHERE id = ",
            integer_to_binary(Rid)
        ]),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"blocked_retention">>, talk_record_t20_support:cleanup_status(Rid)),
        %% 字节保留：引用保持 bound + 资产仍 live
        {St, _AnyCount} = talk_record_t20_support:ref_state(C, Rid),
        ?assertEqual(<<"bound">>, St),
        ?assertEqual(<<"live">>, talk_record_t20_support:attachment_state(C, Path)),
        %% blocked 不被回读改写（refresh 幂等跳过）
        {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
        ?assertEqual(<<"blocked_retention">>, talk_record_t20_support:cleanup_status(Rid)),
        %% blocked 不可 retry（治理保护需先解除留存）
        ?assertEqual(
            {error, cleanup_not_retryable},
            talk_record_delete_logic:retry_purge(Rid)
        )
    end).

%% EAR hold 行 → blocked_hold（defense-in-depth）
purge_blocked_hold_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, U1} = ready(State, 93),
        Path = talk_record_t20_support:insert_attachment(C, 8301, #{record_id => Rid}),
        ok = talk_record_t20_support:bind_ref(C, Rid, 8301, <<"audio_part:1">>),
        ok = talk_record_t20_support:hold_row(C, 8301),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"blocked_hold">>, talk_record_t20_support:cleanup_status(Rid)),
        ?assertEqual(<<"bound">>, element(1, talk_record_t20_support:ref_state(C, Rid))),
        ?assertEqual(<<"live">>, talk_record_t20_support:attachment_state(C, Path))
    end).

%% 收藏引用（同 hash）→ retained_by_other_reference（字节保留）
purge_retained_by_other_reference_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, U1} = ready(State, 94),
        CollectHash = binary:copy(<<"e">>, 64),
        Path = talk_record_t20_support:insert_attachment(C, 8401, #{
            record_id => Rid, hash => CollectHash
        }),
        ok = talk_record_t20_support:bind_ref(C, Rid, 8401, <<"audio_part:1">>),
        ok = talk_record_t20_support:collect_hash(C, 8401, CollectHash),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(
            <<"retained_by_other_reference">>,
            talk_record_t20_support:cleanup_status(Rid)
        ),
        %% 引用已释放（本域视角）；资产因其他业务引用保留字节
        ?assertEqual(<<"released">>, element(1, talk_record_t20_support:ref_state(C, Rid))),
        ?assertEqual(<<"live">>, talk_record_t20_support:attachment_state(C, Path)),
        %% 白名单 protected 标志（受保护≠异常，T21 语义）
        {ok, WL} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(true, maps:get(protected, WL))
    end).

%% 队列终态失败（attempts 耗尽 blocked）→ failed；retry 恢复
purge_failed_then_retry_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, U1} = ready(State, 95),
        Path = talk_record_t20_support:insert_attachment(C, 8501, #{
            record_id => Rid,
            storage_driver => <<"disk">>,
            storage_namespace => <<"t20-ns">>,
            storage_container => <<"t20c">>,
            storage_locator => binary:copy(<<"c">>, 64)
        }),
        ok = talk_record_t20_support:bind_ref(C, Rid, 8501, <<"audio_part:1">>),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        %% 模拟队列 8 次重试耗尽（blocked 终态）
        talk_record_t20_support:queue_exhaust_row(C, Path),
        {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
        ?assertEqual(<<"failed">>, talk_record_t20_support:cleanup_status(Rid)),
        {ok, WL} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(true, maps:get(retryable, WL)),
        %% 运维 retry：failed → pending + 新 purge job（attempt 1）
        {ok, RetryPayload} = talk_record_delete_logic:retry_purge(Rid),
        ?assert(is_integer(maps:get(<<"job_id">>, RetryPayload))),
        Jobs = talk_record_t20_support:purge_jobs(Rid),
        ?assertEqual(2, length(Jobs)),
        %% 队列修复（done；含分段附件的队列行）→ retry purge 协调 + 回读 complete
        talk_record_t20_support:sql(C, [
            "UPDATE attachment_object_deletion SET job_state='done', completed_at=now() WHERE path = '",
            Path,
            "' OR path LIKE 'talk_record/",
            integer_to_binary(Rid),
            "/%'"
        ]),
        talk_record_t20_support:sql(C, [
            "UPDATE attachment SET lifecycle_state='deleted' WHERE path = '",
            Path,
            "' OR path LIKE 'talk_record/",
            integer_to_binary(Rid),
            "/%'"
        ]),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
        ?assertEqual(<<"complete">>, talk_record_t20_support:cleanup_status(Rid))
    end).

%%%===================================================================
%%% E. 迟到模型结果不发布
%%%===================================================================

late_results_not_published_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        %% transcribe 模式记录：complete 投 assemble + transcribe 两 job（不消费）
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, ?REC(96), <<"transcribe">>, [0, 1], <<"t20-96">>
        ),
        {ok, created, _} = talk_record_t12_support:complete_body(
            U1,
            Rid,
            Cid,
            <<"t20-96-cmp">>,
            #{
                <<"processing_consent">> => #{
                    <<"policy_version">> => <<"talk-record-notice-1">>,
                    <<"processor_config_version">> => <<"pc-1">>
                }
            }
        ),
        Queued = row(C, [
            "SELECT count(*) AS n FROM talk_record_job WHERE record_id = ",
            integer_to_binary(Rid),
            " AND state IN ('queued','retry_wait')"
        ]),
        ?assertEqual(2, maps:get(<<"n">>, Queued)),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        %% 删除事务取消全部非终态 job；队列仅剩 purge（迟到模型结果不发布）
        Cancelled = row(C, [
            "SELECT count(*) AS n FROM talk_record_job WHERE record_id = ",
            integer_to_binary(Rid),
            " AND state = 'cancelled'"
        ]),
        ?assertEqual(2, maps:get(<<"n">>, Cancelled)),
        %% worker：purge 正常执行（不被自身取消），模型 job 无可抢
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual({ok, done}, talk_record_job_worker:run_once()),
        %% 迟到发布被 fencing 拒绝：对已软删记录直接执行 assemble prepare
        %% → record_deleted（不重新发布内容）
        #{<<"id">> := AsmJob} = row(C, [
            "SELECT id FROM talk_record_job WHERE record_id = ",
            integer_to_binary(Rid),
            " AND kind='assemble'"
        ]),
        ?assertMatch(
            {error, record_deleted},
            elib_pg:with_tx(
                fun(Conn) ->
                    talk_record_assembly_logic:prepare_tx(
                        Conn, #{
                            <<"id">> => AsmJob,
                            <<"record_id">> => Rid,
                            <<"input_manifest_hash">> => binary:copy(<<"f">>, 64),
                            <<"input_content_revision">> => 1
                        }
                    )
                end,
                [{reraise, false}]
            )
        )
    end).

%%%===================================================================
%%% F. 与 PUT grants 并发互斥
%%%===================================================================

delete_grants_mutual_exclusion_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        {Rid, _, _} = ready(State, 97),
        %% conn1 持删除行锁（未提交）；conn2 的授权行锁必须等待
        C2 = talk_record_t20_support:fresh_conn(),
        try
            {ok, [], []} = epgsql:squery(C2, "SET statement_timeout = '600ms'"),
            Self = self(),
            spawn(fun() ->
                R =
                    (catch talk_record_t20_support:raw_tx(
                        C,
                        fun(ConnL) ->
                            {ok, live, _} =
                                talk_record_delete_repo:lock_for_delete_tx(ConnL, Rid, 1),
                            {ok, _} = talk_record_delete_repo:soft_delete_tx(ConnL, Rid),
                            timer:sleep(1200)
                        end
                    )),
                Self ! {conn1_done, R}
            end),
            timer:sleep(200),
            %% conn2 同行 lock_for_acl 在 conn1 提交前不可完成（互斥）——
            %% statement_timeout 打断等待
            Locked = (fun() ->
                {ok, [], []} = epgsql:squery(C2, "BEGIN"),
                try
                    talk_record_repo:lock_for_acl_tx(C2, Rid, 1)
                catch
                    _:_ -> {error, lock_wait_interrupted}
                after
                    catch epgsql:squery(C2, "ROLLBACK")
                end
            end)(),
            receive
                {conn1_done, _} -> ok
            after 5000 ->
                ?assert(false)
            end,
            %% 600ms 超时打断（query canceled / lock 不可得），非 {ok,_}
            ?assertMatch({error, _}, Locked),
            %% conn1 提交后 conn2 可拿锁并正确观测软删（lock_for_acl 复核
            %% deleted_at → not_found）
            {ok, [], []} = epgsql:squery(C2, "SET statement_timeout = '5s'"),
            {ok, [], []} = epgsql:squery(C2, "BEGIN"),
            R2 = talk_record_repo:lock_for_acl_tx(C2, Rid, 1),
            catch epgsql:squery(C2, "ROLLBACK"),
            ?assertMatch({error, not_found}, R2)
        after
            catch epgsql:close(C2)
        end
    end).

grants_first_then_delete_consistent_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {Rid, _, _} = ready(State, 98),
        %% PUT grants 先行（acl_revision 1 → 2，record.revision 不动）
        Body = #{
            <<"grants">> => [
                #{
                    <<"grantee_user_id">> => U2,
                    <<"role">> => <<"viewer">>,
                    <<"allow_export">> => false
                }
            ]
        },
        {ok, _} = talk_record_grant_logic:replace(U1, Rid, <<"acl:1">>, Body),
        ?assertEqual(
            2,
            maps:get(
                <<"v">>,
                row(C, [
                    "SELECT acl_revision AS v FROM talk_record WHERE id = ",
                    integer_to_binary(Rid)
                ])
            )
        ),
        %% DELETE 后（revision 域未被授权触碰 → If-Match 1 仍有效）：删除成功
        {ok, deleted, Payload} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        ?assertEqual(true, maps:get(<<"logical_deleted">>, Payload)),
        %% PUT 后新增的 grant 也一并撤销
        Active = row(C, [
            "SELECT count(*) AS n FROM talk_record_grant WHERE record_id = ",
            integer_to_binary(Rid),
            " AND revoked_at IS NULL"
        ]),
        ?assertEqual(0, maps:get(<<"n">>, Active))
    end).

%%%===================================================================
%%% G. 注销预检
%%%===================================================================

preflight_facts_personal_vs_organization_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid1, _, _} = ready(State, 99),
        {Rid2, _, _} = ready(State, 50),
        %% Rid2 转企业归属（SQL 直改——迁移 209 归属不可经 API 改，测试装配）
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET scope='organization', organization_id=710001,",
            " workspace_id=NULL WHERE id = ",
            integer_to_binary(Rid2)
        ]),
        %% Rid3：个人已删清理未完成
        {Rid3, _, _} = ready(State, 51),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid3, <<"1">>),
        {ok, Fact} = talk_record_delete_preflight:facts(U1),
        ?assertEqual(U1, maps:get(subject_user_id, Fact)),
        ?assertEqual(talk_record, maps:get(domain, Fact)),
        ?assert(is_integer(maps:get(observed_at, Fact))),
        ?assertEqual(1, maps:get(fact_version, Fact)),
        Codes = [maps:get(code, B) || B <- maps:get(blockers, Fact)],
        ?assert(lists:member(<<"TALK_RECORD_PERSONAL_PENDING">>, Codes)),
        ?assert(lists:member(<<"TALK_RECORD_CLEANUP_PENDING">>, Codes)),
        Counts = maps:get(counts, Fact),
        %% Rid1=personal active；Rid2 转 organization（不计个人）；Rid3 已删入 cleanup
        ?assertEqual(1, maps:get(personal_active, Counts)),
        ?assertEqual(1, maps:get(personal_cleanup_pending, Counts)),
        %% 无用户记录的陌生人：零 blocker
        U7 = maps:get(u7, Ids),
        {ok, Empty} = talk_record_delete_preflight:facts(U7),
        ?assertEqual([], maps:get(blockers, Empty))
    end).

preflight_run_integration_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid, _, _} = ready(State, 52),
        %% 五域 provider meck 为空 blockers 合法形状（编排链验证——真实五域
        %% 查询不依赖 talk_record 桩列），talk_record 域用真实实现
        ok = meck:new(organization_preflight_facts_pg, [no_link]),
        Dummy = fun(Domain) ->
            fun(Uid) ->
                {ok, #{
                    subject_user_id => Uid,
                    domain => Domain,
                    observed_at => os:system_time(millisecond),
                    fact_version => 1,
                    blockers => []
                }}
            end
        end,
        ok = meck:expect(organization_preflight_facts_pg, facts_organization, Dummy(organization)),
        ok = meck:expect(organization_preflight_facts_pg, facts_workspace, Dummy(workspace)),
        ok = meck:new(eb_preflight_facts_pg, [no_link]),
        ok = meck:expect(
            eb_preflight_facts_pg, facts_enterprise_business, Dummy(enterprise_business)
        ),
        ok = meck:new(cs_preflight_facts_pg, [no_link]),
        ok = meck:expect(cs_preflight_facts_pg, facts_customer_service, Dummy(customer_service)),
        ok = meck:new(agent_preflight_facts, [no_link]),
        ok = meck:expect(agent_preflight_facts, facts_agent, Dummy(agent)),
        Registry = [
            {organization, organization_preflight_facts_pg, facts_organization},
            {workspace, organization_preflight_facts_pg, facts_workspace},
            {enterprise_business, eb_preflight_facts_pg, facts_enterprise_business},
            {customer_service, cs_preflight_facts_pg, facts_customer_service},
            {agent, agent_preflight_facts, facts_agent},
            {talk_record, talk_record_delete_preflight, facts}
        ],
        try
            Result = talk_record_t20_support:with_env(
                deletion_preflight_providers,
                Registry,
                fun() -> organization_deletion_preflight:run(U1) end
            ),
            ?assertMatch({ok, #{blockers := [_ | _]}}, Result),
            {ok, #{facts := Facts, blockers := Blockers}} = Result,
            ?assert(lists:any(fun(F) -> maps:get(domain, F) =:= talk_record end, Facts)),
            TalkBlockers = [
                B
             || B <- Blockers,
                maps:get(resource_type, B) =:= <<"talk_record">>
            ],
            ?assertMatch([_ | _], TalkBlockers),
            %% 冻结四字段形状
            [B1 | _] = TalkBlockers,
            ?assertEqual(null, maps:get(organization_id, B1)),
            ?assert(is_binary(maps:get(code, B1)))
        after
            catch meck:unload(agent_preflight_facts),
            catch meck:unload(cs_preflight_facts_pg),
            catch meck:unload(eb_preflight_facts_pg),
            catch meck:unload(organization_preflight_facts_pg)
        end
    end).

departure_custody_projection_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid1, _, _} = ready(State, 53),
        {Rid2, _, _} = ready(State, 54),
        %% Rid2 企业归属；U1 保持 org 成员
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET scope='organization', organization_id=710001,",
            " workspace_id=NULL WHERE id = ",
            integer_to_binary(Rid2)
        ]),
        %% U1 失去企业成员资格（离企/注销投影触发条件）
        talk_record_t20_support:sql(C, [
            "UPDATE organization_member SET status='suspended'",
            " WHERE organization_id=710001 AND user_id=",
            integer_to_binary(U1)
        ]),
        {ok, 1} = elib_pg:with_tx(
            fun(Conn) -> talk_record_delete_logic:project_departure_custody_tx(Conn, U1) end,
            [{reraise, false}]
        ),
        Custody2 = row(
            C,
            "SELECT custody_status, deleted_at FROM talk_record WHERE id = " ++
                integer_to_binary(Rid2)
        ),
        ?assertEqual(<<"handover_required">>, maps:get(<<"custody_status">>, Custody2)),
        %% 不级联误删：企业记录行保留（deleted_at 仍 NULL）
        ?assertEqual(null, maps:get(<<"deleted_at">>, Custody2)),
        %% 个人记录不受离企投影影响
        Custody1 = row(
            C,
            "SELECT custody_status FROM talk_record WHERE id = " ++
                integer_to_binary(Rid1)
        ),
        ?assertEqual(<<"active">>, maps:get(<<"custody_status">>, Custody1)),
        %% owner 失权：U1 对企业记录 visible 失败（成员资格谓词）
        ?assertMatch({error, not_found}, talk_record_ds:detail(U1, Rid2))
    end).

%%%===================================================================
%%% H. T06-L2 修复回归
%%%===================================================================

scan_organization_l2_regression_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        OrgA = maps:get(org_a, Ids),
        {Rid1, _, _} = ready(State, 55),
        talk_record_t20_support:sql(C, [
            "UPDATE talk_record SET scope='organization', organization_id=",
            integer_to_binary(OrgA),
            ", workspace_id=NULL WHERE id = ",
            integer_to_binary(Rid1)
        ]),
        %% 附件命名：一个字面含 % 的名字 + 一个普通名
        Path1 = talk_record_t20_support:insert_attachment(C, 8601, #{record_id => Rid1}),
        talk_record_t20_support:sql(C, [
            "UPDATE attachment SET name='plan 50% done' WHERE path = '", Path1, "'"
        ]),
        talk_record_t20_support:sql(C, [
            "INSERT INTO attachment(id, file_hash256, mime_type, name, path, url, size,"
            " referer_time, creator_user_id, scope, scope_ref, gc_verified)"
            " VALUES (8602, repeat('d',64), 'audio/aac', 'abc50x', 't20/dirty-8602',"
            " 't20/dirty-8602', 10, 1, ",
            integer_to_binary(U1),
            ", 'talk_record', 'not-a-number', true)"
        ]),
        %% 1) 脏 scope_ref（非数字）不再炸查询（文本比较跳行；本 org 的
        %% talk_record 附件含分段附件与 8601，脏行不在内）
        {ok, Rows} = talk_record_attachment_ref_repo:scan_organization_tx(
            C, OrgA, #{}
        ),
        ?assert(lists:member(Path1, [maps:get(<<"path">>, R) || R <- Rows])),
        ?assertNot(
            lists:member(
                <<"t20/dirty-8602">>,
                [maps:get(<<"path">>, R) || R <- Rows]
            )
        ),
        %% 2) ILIKE 转义：keyword "50%" 只匹配字面 '50%'，不匹配 '50x'
        {ok, [Hit]} = talk_record_attachment_ref_repo:scan_organization_tx(
            C, OrgA, #{keyword => <<"50%">>}
        ),
        ?assertEqual(<<"plan 50% done">>, maps:get(<<"name">>, Hit)),
        {ok, []} = talk_record_attachment_ref_repo:scan_organization_tx(
            C, OrgA, #{keyword => <<"50x">>}
        ),
        %% 下划线通配同样字面化
        {ok, []} = talk_record_attachment_ref_repo:scan_organization_tx(
            C, OrgA, #{keyword => <<"pl_n">>}
        ),
        %% 3) LIMIT 钳制（>100 → 100；非法 → 100）
        {ok, Rows2} = talk_record_attachment_ref_repo:scan_organization_tx(
            C, OrgA, #{limit => 999999}
        ),
        ?assert(length(Rows2) =< 100)
    end).
