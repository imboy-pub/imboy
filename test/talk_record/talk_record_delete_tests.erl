%%% @doc talk_record 删除/清理/注销预检闭环测试（logic + worker 层）。
%%%
%%% 车道：TALK_OWNED marker 库（t12 支撑链）。原 T20 冻结注册表车道在本仓
%%% 无供给方（无编译 pin、无控制器资产），两个控制器级 case 已改为本车道
%%% 等价真实旅程（磁盘队列自然过期、队列八轮真实耗尽 + 公共恢复 API）。
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

-export([prepare_disk_queue_fixture/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(REC(N), <<"30000000-0000-0000-0000-0000000000", (integer_to_binary(N))/binary>>).

%%%===================================================================
%%% 支撑
%%%===================================================================

%% The frozen T20 case registry has no supplier in the owned marker lane (no
%% compiled pin, no controller assets); every generator runs on the shared
%% migrated marker DB through the t12 support chain, like the rest of the family.
setup(_Case) ->
    %% 运行 nonce：共享 marker 库跨 VM 复用，全部夹具 id/幂等键派生自它
    %% （v5 冻结位/首绑守卫使固定 id 残留不可覆盖，运行唯一化是正解）。
    RunNonce = 100000 + os:system_time(second) rem 8000000,
    persistent_term:put({?MODULE, run_nonce}, RunNonce),
    State = talk_record_t12_support:setup_all(),
    try
        talk_record_t12_support:seed(State),
        State
    catch
        Class:Reason:Stack ->
            try
                cleanup(State)
            catch
                CleanupClass:CleanupReason ->
                    error(
                        {setup_failed_and_cleanup_failed, {Class, Reason},
                            {CleanupClass, CleanupReason}}
                    )
            end,
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(State) ->
    talk_record_t12_support:teardown_all(State).

with_db(Case, TestFun) ->
    {timeout, 120,
        {setup, fun() -> setup(Case) end, fun cleanup/1, fun(State) ->
            {timeout, 120, ?_test(TestFun(State))}
        end}}.

%% complete 就绪记录（complete 投 assemble → 先消费成功），返回 {Rid, Cid, U1}。
%% presign 分段会话统一排空（upload_expires_at 置过去——模拟上传能力窗口
%% 关闭；否则 intent fn 的 attach_pending 能力检查使清理误判 retained）。
ready(State, N) ->
    Ids = talk_record_t20_support:ids(),
    ready(State, N, maps:get(u1, Ids)).

ready(State, N, U1) ->
    {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
        U1,
        nrec(N),
        <<"record_only">>,
        [0, 1],
        nidem(iolist_to_binary([<<"t20-">>, integer_to_binary(N)]))
    ),
    {ok, created, _} = talk_record_t12_support:complete(
        U1, Rid, Cid, <<"t20-", (integer_to_binary(N))/binary, "-cmp">>
    ),
    %% assemble job 消费成功（record_only 无模型 job）
    {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
    drain_upload_sessions(State, Rid),
    {Rid, Cid, U1}.

draft(Uid, N, Scope) ->
    Body = maps:merge(
        #{
            <<"client_recording_id">> => nrec(N),
            <<"notice_ack">> => true,
            <<"policy_version">> => <<"talk-record-notice-v1">>,
            <<"processing_mode">> => <<"record_only">>
        },
        Scope
    ),
    {ok, created, Payload, ETag} = talk_record_logic:create(
        Uid, Body, nidem(iolist_to_binary([<<"t20-draft-">>, integer_to_binary(N)]))
    ),
    ?assertEqual({ok, 1}, talk_record_logic:parse_if_match(ETag)),
    maps:get(<<"id">>, Payload).

%%%===================================================================
%%% Owned marker-lane local helpers
%%%（原 T20 冻结注册表车道 helpers 的本车道等价：同 SQL/同连接事实，
%%%  去掉冻结注册表校验链——该车道在本仓无供给方。disk 事实与
%%%  talk_record_upload_test_support:configure/0 的注册表单一对齐。）
%%%===================================================================

cleanup_status(Conn, RecordId) ->
    Row = talk_record_t12_support:one(Conn, [
        "SELECT cleanup_status FROM talk_record WHERE id = ",
        integer_to_binary(RecordId)
    ]),
    maps:get(<<"cleanup_status">>, Row).

purge_jobs(Conn, RecordId) ->
    {ok, Rows} =
        elib_pg:query(
            Conn,
            <<"SELECT id, state, error_code, attempt FROM talk_record_job",
                " WHERE record_id = $1 AND kind = 'purge' ORDER BY created_at ASC, id ASC">>,
            [RecordId]
        ),
    Rows.

insert_attachment(Conn, Id, Opts) ->
    RecordId = maps:get(record_id, Opts),
    Path = maps:get(path, Opts, path_of(Id)),
    Hash = maps:get(hash, Opts, binary:copy(<<"b">>, 64)),
    GcVerified = maps:get(gc_verified, Opts, true),
    Driver = maps:get(storage_driver, Opts, null),
    Ns = maps:get(storage_namespace, Opts, null),
    Container = maps:get(storage_container, Opts, null),
    Locator = maps:get(storage_locator, Opts, null),
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO attachment(id, file_hash256, mime_type, name, path, url, size,"
                " referer_time, creator_user_id, scope, scope_ref, cipher, gc_verified,"
                " storage_driver, storage_namespace, storage_container, storage_locator_sha256)"
                " VALUES ($1,$2,'audio/aac',$3,$4,$4,10,1,1,'talk_record',$5::bigint::text,NULL,$6,"
                " $7,$8,$9,$10)"
            >>,
            [Id, Hash, Path, Path, RecordId, GcVerified, Driver, Ns, Container, Locator]
        ),
    Path.

path_of(Id) ->
    <<"t20/att-", (integer_to_binary(Id))/binary>>.

bind_ref(Conn, RecordId, AttachmentId, BusinessKey) ->
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO talk_record_attachment_ref(id, record_id, attachment_id,"
                " role, business_key) VALUES ($1,$2,$3,'audio_part',$4)"
            >>,
            [AttachmentId * 1000 + 1, RecordId, AttachmentId, BusinessKey]
        ),
    ok.

collect_hash(Conn, CollectId, Hash) ->
    %% 前序运行残留的 (700002, kind) 收藏行会撞 uk_user_collect_uid_kind
    %% （共享 marker 库），夹具用户收藏面先清再插。
    {ok, _} =
        elib_pg:query(
            Conn, <<"DELETE FROM user_collect WHERE user_id = 700002">>, []
        ),
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO user_collect(id, user_id, attach_file_hash256, status)"
                " VALUES ($1, 700002, $2, 1)"
            >>,
            [CollectId, Hash]
        ),
    ok.

hold_row(Conn, AttachmentId) ->
    {ok, [
        #{
            <<"organization_id">> := _Org,
            <<"talk_record_id">> := _Rid,
            <<"application_id">> := null,
            <<"source_kind">> := <<"talk_record">>
        }
    ]} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO enterprise_attachment_retention(attachment_id, organization_id,"
                " application_id, source_kind, talk_record_id, retention_until, hold_state, hold_reason, hold_set_at)"
                " SELECT a.id,r.organization_id,NULL,'talk_record',r.id,"
                " now() - interval '1 day','held','t20-hold',now()"
                " FROM attachment a JOIN talk_record r ON a.scope_ref=r.id::text"
                " WHERE a.id=$1 AND a.scope='talk_record' AND r.scope='organization'"
                " RETURNING organization_id,talk_record_id,application_id,source_kind"
            >>,
            [AttachmentId]
        ),
    ok.

fresh_conn() ->
    {ok, Conn} = epgsql:connect(fresh_conn_opts()),
    Conn.

fresh_conn_opts() ->
    #{
        host => os:getenv("T05_PG_HOST", "127.0.0.1"),
        port => list_to_integer(os:getenv("T05_PG_PORT", "55433")),
        username => list_to_binary(os:getenv("T05_PG_USER", "t05admin")),
        password => os:getenv("T05_PG_PASSWORD", ""),
        database => list_to_binary(os:getenv("T05_PG_DB", "talk_record_t05")),
        ssl => false,
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }.

raw_tx(Conn, Fun) ->
    {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
    try
        R = Fun(Conn),
        {ok, [], []} = epgsql:squery(Conn, "COMMIT"),
        R
    catch
        Class:Reason:Stack ->
            catch epgsql:squery(Conn, "ROLLBACK"),
            erlang:raise(Class, Reason, Stack)
    end.

%% 运行唯一派生 id：client_recording_id（uuid 形）、幂等键、附件 int8 id。
nonce() ->
    persistent_term:get({?MODULE, run_nonce}).

nrec(N) ->
    %% 严格 uuid36（8-4-4-4-12）：nonce 折进第 2/3 组，N 折进末组。
    iolist_to_binary([
        "30000000-",
        io_lib:format("~4.16.0b", [(nonce() bsr 16) band 16#FFFF]),
        "-",
        io_lib:format("~4.16.0b", [nonce() band 16#FFFF]),
        "-0000-",
        io_lib:format("~12.16.0b", [N])
    ]).

nidem(Prefix) ->
    iolist_to_binary([Prefix, $-, integer_to_binary(nonce())]).

nat(K) ->
    nonce() * 1000 + K.

fixture_ns() ->
    <<"talk-upload-test">>.

fixture_registry() ->
    {ok, Rec} = attachment_storage:resolve(fixture_ns()),
    Rec.

fixture_locator_digest() ->
    Ns = fixture_ns(),
    {ok, Rec} = attachment_storage:resolve(Ns),
    attachment_storage:locator_digest(Ns, Rec).

%% 原车道意图是"排空 presign 分段会话窗口"（置 upload_expires_at 过去）。
%% 本车道事实：V5 分段会话在 t12 confirm 链后即 confirmed（终态），本 lane
%% 删除链不检查会话窗口（能力检查仅属旧控制器车道），因此显式 no-op——
%% 分段位即真实 garage 对象，直接进入删除协调。
drain_upload_sessions(_State, _Rid) ->
    ok.

%% S3-only 退役收编后的夹具字节面：行先建（garage 冻结位），再
%% frozen_location → presign PUT → httpc 直传真字节。
put_fixture_bytes(C, Id, Opts, Bytes) ->
    Path = insert_attachment(C, Id, Opts#{
        storage_driver => <<"garage">>,
        storage_namespace => fixture_ns(),
        storage_container => maps:get(container, fixture_registry()),
        storage_locator => fixture_locator_digest()
    }),
    {ok, I} = attachment_storage:frozen_location(Path, attachment_row(C, Id)),
    {ok, Url} = attachment_storage_garage:presign_put_snapshot(
        I, <<"application/octet-stream">>, 300
    ),
    {ok, {{_, 200, _}, _, _}} =
        httpc:request(
            put,
            {binary_to_list(Url), [], "application/octet-stream", Bytes},
            [],
            [{body_format, binary}]
        ),
    Path.

attachment_row(C, Id) ->
    {ok, [Row]} = elib_pg:query(
        C, <<"SELECT * FROM attachment WHERE id=$1">>, [Id]
    ),
    Row.

%% 冻结位字节读（presign GET）；返回 {ok, {{_,200,_},_,Bytes}} | {error,_}。
fixture_bytes(Row) ->
    {ok, I} = attachment_storage:frozen_location(maps:get(<<"path">>, Row), Row),
    case attachment_storage_garage:presign_snapshot(I, 300) of
        {ok, Url} ->
            httpc:request(
                get, {binary_to_list(Url), []}, [], [{body_format, binary}]
            );
        Error ->
            Error
    end.

%% 字节已物理消失（HEAD fail-closed）。
object_gone(Row) ->
    {ok, I} = attachment_storage:frozen_location(maps:get(<<"path">>, Row), Row),
    ?assertMatch({error, _}, attachment_storage:head_snapshot(I)).

%% 把删除协调面的附件位（bound 引用、part 行、assembly 成品位）换挂到夹具
%% 自建的 garage 真实对象：CONFIRMED 会话无出边（v5 合法转移集封死），其
%% 3600s 能力窗不可排空，intent 资格门的窗口检查会保守保留——与 disk 时代
%% rebind 同理，用无会话的夹具对象承接；字节按原对象真读真写，SHA 冻结一致。
rebind_garage(C, Rid) ->
    %% 派生台账附件（manifest）无上传会话、不受窗口检查，换挂会令
    %% attachment_derived_write 与引用位指向分裂——跳过，仅换挂会话位。
    {ok, Rows} = elib_pg:query(
        C,
        <<
            "SELECT DISTINCT r.attachment_id AS aid"
            " FROM talk_record_attachment_ref r WHERE r.record_id=$1 AND r.state='bound'"
            " AND NOT EXISTS (SELECT 1 FROM attachment_derived_write w"
            "                 WHERE w.attachment_id = r.attachment_id)"
            " ORDER BY r.attachment_id"
        >>,
        [Rid]
    ),
    lists:foreach(
        fun({N, #{<<"aid">> := Aid}}) ->
            {ok, [OldRow]} = elib_pg:query(
                C, <<"SELECT * FROM attachment WHERE id=$1">>, [Aid]
            ),
            {ok, {{_, 200, _}, _, Bytes}} = fixture_bytes(OldRow),
            NewId = nat(700 + N),
            NewPath = put_fixture_bytes(
                C,
                NewId,
                located_attachment(Rid, #{
                    hash => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase)
                }),
                Bytes
            ),
            true = is_binary(NewPath) andalso byte_size(NewPath) > 0,
            {ok, _} = elib_pg:query(
                C,
                <<
                    "UPDATE talk_record_attachment_ref SET attachment_id=$1"
                    " WHERE attachment_id=$2 AND record_id=$3"
                >>,
                [NewId, Aid, Rid]
            ),
            {ok, _} = elib_pg:query(
                C,
                <<
                    "UPDATE talk_record_audio_part SET attachment_id=$1"
                    " WHERE attachment_id=$2 AND record_id=$3"
                >>,
                [NewId, Aid, Rid]
            ),
            {ok, _} = elib_pg:query(
                C,
                <<
                    "UPDATE talk_record_assembly SET audio_attachment_id=$1"
                    " WHERE record_id=$2 AND audio_attachment_id=$3"
                >>,
                [NewId, Rid, Aid]
            ),
            ok
        end,
        lists:zip(lists:seq(1, length(Rows)), Rows)
    ),
    ok.

grant_viewer(Conn, RecordId, Uid) ->
    talk_record_t12_support:sql(Conn, [
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
    talk_record_t12_support:one(Conn, Sql).

%% Protected/retained assets still require valid V5 locations; these cases do
%% not claim a successful physical deletion or manufacture gc completion.
located_attachment(Rid, Extra) ->
    maps:merge(
        #{
            record_id => Rid,
            storage_driver => <<"garage">>,
            storage_namespace => fixture_ns(),
            storage_container => maps:get(container, fixture_registry()),
            storage_locator => fixture_locator_digest()
        },
        Extra
    ).

%% Real config validation and consent/enqueue, with a fail-only executable
%% sentinel. The queued model job must be cancelled before any execution;
%% this proves deletion fencing, not transcription or model quality.
complete_for_cancellation(C, Uid, Rid, Cid) ->
    Version = <<"synthetic-cancel-only-v1">>,
    Body = #{
        <<"processing_consent">> => #{
            <<"policy_version">> => <<"talk-record-notice-v1">>,
            <<"processor_config_version">> => Version
        }
    },
    Complete = fun() ->
        talk_record_t12_support:complete_body(
            Uid, Rid, Cid, nidem(<<"t20-96-cmp">>), Body
        )
    end,
    talk_record_t20_support:with_env(talk_record_processor, #{}, fun() ->
        ?assertEqual({error, processing_unavailable}, Complete()),
        #{<<"n">> := 0} = row(C, [
            "SELECT count(*) AS n FROM talk_record_job WHERE record_id=",
            integer_to_binary(Rid)
        ])
    end),
    Bundle = #{
        enabled => true,
        approved => true,
        version => Version,
        transcribe => #{
            executable => "/usr/bin/false",
            model_version => <<"synthetic-must-not-execute">>,
            diarization => true
        }
    },
    talk_record_t20_support:with_env(talk_record_processor, Bundle, fun() ->
        ?assertMatch(
            {ok, #{executable := "/usr/bin/false"}},
            talk_record_processor_config:get(<<"transcribe">>)
        ),
        ?assertEqual(true, maps:get(<<"asr_enabled">>, talk_record_ds:capabilities())),
        ?assertEqual(Version, talk_record_processor_config:version()),
        {ok, created, _} = Complete(),
        #{<<"n">> := 1} = row(C, [
            "SELECT count(*) AS n FROM talk_record_consent_event WHERE kind='processing_opt_in' AND record_id=",
            integer_to_binary(Rid)
        ]),
        ok
    end).

%%%===================================================================
%%% A. DELETE 主流程
%%%===================================================================

delete_owner_202_pending_test_() ->
    with_db(delete_owner_202_pending_test_, fun(State) ->
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
        ?assertMatch([#{<<"state">> := <<"queued">>}], purge_jobs(C, Rid))
    end).

delete_preconditions_test_() ->
    with_db(delete_preconditions_test_, fun(State) ->
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
    with_db(delete_idempotent_replay_test_, fun(State) ->
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
    with_db(delete_fences_active_capture_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid, Cid, _} = ready(State, 84),
        %% 复活一条 recording capture + active_capture_id 投影
        talk_record_t12_support:sql(C, [
            "INSERT INTO talk_record_capture(id, record_id, device_session_id, device_id,"
            " started_by, status) VALUES (",
            integer_to_binary(Cid + 5),
            ", ",
            integer_to_binary(Rid),
            ", '10000000-0000-0000-0000-00000000dead', 'dev2', ",
            integer_to_binary(U1),
            ", 'recording')"
        ]),
        talk_record_t12_support:sql(C, [
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

%% V1 合同：成品=客户端直传附件（publish 经 bind_artifact_tx 引用，非派生
%% 写入）；唯一派生产物=manifest。故 derived 恒 1，成品出现在 refs 并集。
garage_objects(C, Rid) ->
    Rows = attachment_rows(C, Rid),
    ?assertEqual(2, length([R || #{<<"part">> := true} = R <- Rows])),
    ?assertEqual(1, length([R || #{<<"derived">> := true} = R <- Rows])),
    [garage_object(R) || R <- Rows].

attachment_rows(C, Rid) ->
    {ok, Rows} = elib_pg:query(
        C,
        <<
            "SELECT a.*,EXISTS(SELECT 1 FROM talk_record_audio_part p"
            " WHERE p.record_id=$1 AND p.attachment_id=a.id) AS part,"
            "EXISTS(SELECT 1 FROM attachment_derived_write w JOIN talk_record_job j"
            " ON j.id=w.source_job_id WHERE j.record_id=$1 AND w.attachment_id=a.id) AS derived"
            " FROM attachment a WHERE a.id IN"
            " (SELECT attachment_id FROM talk_record_attachment_ref WHERE record_id=$1"
            " UNION SELECT w.attachment_id FROM attachment_derived_write w JOIN talk_record_job j"
            " ON j.id=w.source_job_id WHERE j.record_id=$1) ORDER BY a.path"
        >>,
        [Rid]
    ),
    Rows.

%% 冻结位读回：presign GET 字节、逐对象 SHA 与行冻结身份比对；row 随身
%% 携带（后续仍在/已消失断言直接用行驱动）。
garage_object(#{<<"path">> := Path} = Row) ->
    {ok,
        #{
            driver := garage,
            namespace := Ns,
            container := Container,
            key := Key,
            locator_digest := Digest
        } = I} =
        attachment_storage:frozen_location(Path, Row),
    {ok, Url} = attachment_storage_garage:presign_snapshot(I, 300),
    {ok, {{_, 200, _}, _, Bytes}} =
        httpc:request(get, {binary_to_list(Url), []}, [], [{body_format, binary}]),
    ?assert(byte_size(Bytes) > 0),
    Hash = crypto:hash(sha256, Bytes),
    ?assertEqual(binary:decode_hex(maps:get(<<"file_hash256">>, Row)), Hash),
    #{
        path => Path,
        row => Row,
        sha256 => Hash,
        identity => {Ns, Container, Digest, Key}
    }.

%%%===================================================================
%%% B. 撤权即时性
%%%===================================================================

revocation_immediate_test_() ->
    with_db(revocation_immediate_test_, fun(State) ->
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
    with_db(playback_ticket_revoked_on_delete_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {Rid, _, _} = ready(State, 86),
        grant_viewer(C, Rid, U2),
        talk_record_t12_support:sql(C, [
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
    with_db(whitelist_shape_test_, fun(State) ->
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
            <<"cleanup_generation">>,
            <<"cleanup_reason">>,
            <<"cleanup_retry">>,
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
    with_db(whitelist_flags_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid, _, _} = ready(State, 88),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        %% blocked_retention → protected 标志
        talk_record_t12_support:sql(C, [
            "UPDATE talk_record SET cleanup_status='blocked_retention' WHERE id = ",
            integer_to_binary(Rid)
        ]),
        {ok, WL1} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(true, maps:get(protected, WL1)),
        ?assertNot(maps:is_key(retryable, WL1)),
        %% failed → retryable 标志
        talk_record_t12_support:sql(C, [
            "UPDATE talk_record SET cleanup_status='failed' WHERE id = ",
            integer_to_binary(Rid)
        ]),
        {ok, WL2} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(true, maps:get(retryable, WL2)),
        %% pending → 两者皆无
        talk_record_t12_support:sql(C, [
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
    with_db(purge_complete_no_attachments_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        U1 = maps:get(u1, talk_record_t20_support:ids()),
        Rid = draft(U1, 90, #{<<"scope">> => <<"personal">>}),
        %% 真实未上传草稿：没有分段、派生资产或处理任务，不能删引用伪造空记录。
        {ok, [Empty]} = elib_pg:query(
            C,
            <<
                "SELECT (SELECT count(*) FROM talk_record_attachment_ref WHERE record_id=$1) AS refs,"
                "(SELECT count(*) FROM talk_record_capture WHERE record_id=$1) AS captures,"
                "(SELECT count(*) FROM talk_record_audio_part WHERE record_id=$1) AS parts,"
                "(SELECT count(*) FROM talk_record_job WHERE record_id=$1) AS jobs,"
                "(SELECT count(*) FROM attachment_derived_write w JOIN talk_record_job j"
                " ON j.id=w.source_job_id WHERE j.record_id=$1) AS derived"
            >>,
            [Rid]
        ),
        ?assertEqual(
            #{
                <<"refs">> => 0,
                <<"captures">> => 0,
                <<"parts">> => 0,
                <<"jobs">> => 0,
                <<"derived">> => 0
            },
            Empty
        ),
        {ok, deleted, P0} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        ?assertEqual(<<"none">>, maps:get(<<"physical_delete_state">>, P0)),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"pending">>, cleanup_status(C, Rid)),
        %% worker tick 回读（无附件 → complete）
        {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
        ?assertEqual(<<"complete">>, cleanup_status(C, Rid)),
        %% purge job succeeded 终态
        [PJob] = purge_jobs(C, Rid),
        ?assertEqual(<<"succeeded">>, maps:get(<<"state">>, PJob))
    end).

%% pending →（协调 intended）→ 真 disk 队列消费 → complete
purge_complete_disk_queue_test_() ->
    with_db(purge_complete_disk_queue_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        %% Owned-lane journey: real upload/confirm/assemble, the upload window
        %% closed (never the storage identity), then the real delete/purge/
        %% worker path with the natural-expiry assertions intact.
        Prepared = prepare_disk_queue_fixture(State),
        #{
            <<"record">> := #{<<"id">> := Rid, <<"owner_user_id">> := U1},
            <<"extra_attachment_path">> := Path
        } = Prepared,
        Objects = garage_objects(C, Rid),
        ?assertEqual(5, length(Objects)),
        ?assertEqual(5, length(lists:usort([maps:get(path, O) || O <- Objects]))),
        ?assert(lists:member(Path, [maps:get(path, O) || O <- Objects])),
        {ok, deleted, P0} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        ?assertEqual(<<"none">>, maps:get(<<"physical_delete_state">>, P0)),
        %% purge 协调：引用释放 + intent 入队（物理未完成 → pending）
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"pending">>, cleanup_status(C, Rid)),
        {St, _AnyCount} = talk_record_t20_support:ref_state(C, Rid),
        ?assertEqual(<<"released">>, St),
        ?assertEqual(<<"delete_intent">>, talk_record_t20_support:attachment_state(C, Path)),
        %% 既有附件 worker：claim → 真删（garage delete_snapshot）→ ack done
        %% （真路径；5 个真实对象：8101、两段上传、assemble 的 audio 与 manifest）。
        {ok, 5} = attachment_delete_worker:run_batch(
            C, fun(_Ns2, _C2, _K, I) -> attachment_storage:delete_snapshot(I) end
        ),
        %% ack 后墓碑保持 delete_intent（行删除归队列 ack 协议后续；
        %% 物理完成证据 = 队列 done + 字节消失）
        ?assertEqual(<<"delete_intent">>, talk_record_t20_support:attachment_state(C, Path)),
        lists:foreach(
            fun(#{row := Row}) -> object_gone(Row) end,
            Objects
        ),
        {ok, Purged} = talk_record_delete_repo:purged_attachment_paths_tx(C, Rid),
        ?assertEqual(
            lists:sort([maps:get(path, O) || O <- Objects]),
            lists:sort([maps:get(<<"path">>, P) || P <- Purged])
        ),
        lists:foreach(
            fun(P) ->
                ?assertEqual(true, maps:get(<<"queue_done">>, P)),
                ?assertEqual(<<"done">>, maps:get(<<"queue_state">>, P))
            end,
            Purged
        ),
        %% 状态回读 → complete（queue done → deleted 投影）
        {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
        ?assertEqual(<<"complete">>, cleanup_status(C, Rid)),
        %% 白名单 physical_delete_state 投影 deleted
        {ok, WL} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(<<"deleted">>, maps:get(<<"physical_delete_state">>, WL))
    end).

%% Owned-lane creation phase: real upload/confirm/assemble, then STOP before
%% DELETE with the upload window closed. The storage identity is never
%% rewritten — segments and derived artifacts keep their real V5 locations.
prepare_disk_queue_fixture(State) ->
    C = talk_record_t20_support:conn_of(State),
    %% 本根先序 case 的 job/队列行残留是测试装置（其断言完成后不再有意义）；
    %% 本旅程要求干净的 job 与队列面（run_once 的 claim、run_batch 的扫描都是
    %% 全局面），先清装置。置于装配之前：CASCADE 不误伤本 case 将建的行。
    %% （共享 marker 库、本根独占——不触碰他根事实。）
    talk_record_t12_support:sql(C, "TRUNCATE talk_record_job CASCADE"),
    talk_record_t12_support:sql(C, "TRUNCATE attachment_object_deletion"),
    %% 本套件 cleanup-retry 旅程的装置行（共享 marker 库，t12 夹具清洁门
    %% 要求该表为空）。
    talk_record_t12_support:sql(C, "TRUNCATE talk_record_cleanup_retry"),
    Uid = maps:get(u1, talk_record_t20_support:ids()),
    {ok, {Rid, Cid, _PartIds}} = talk_record_t12_support:ready_record(
        Uid, nrec(91), <<"record_only">>, [0, 1], nidem(<<"t20-disk-natural-91">>)
    ),
    {ok, created, _} = talk_record_t12_support:complete(
        Uid, Rid, Cid, nidem(<<"t20-disk-natural-91-complete">>)
    ),
    %% Assemble returns outcome only; garage_objects verifies the actual job
    %% kind/state. CONFIRMED 会话能力窗不可排空 → 引用位换挂夹具自有对象。
    {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
    rebind_garage(C, Rid),
    Bytes = <<"t20-bytes!">>,
    ExtraId = nat(101),
    Path = put_fixture_bytes(
        C,
        ExtraId,
        #{
            record_id => Rid,
            gc_verified => false,
            hash => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase)
        },
        Bytes
    ),
    ok = bind_ref(C, Rid, ExtraId, <<"audio_part:1">>),
    {ok, [Record]} = elib_pg:query(
        C, <<"SELECT r.* FROM talk_record r WHERE r.id=$1">>, [Rid]
    ),
    #{<<"record">> => Record, <<"extra_attachment_path">> => Path}.

%% retention_until 未到期 → blocked_retention（引用保持 bound，GC 谓词继续挡）
purge_blocked_retention_test_() ->
    with_db(purge_blocked_retention_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, U1} = ready(State, 92),
        RetainId = nat(201),
        Path = insert_attachment(C, RetainId, located_attachment(Rid, #{})),
        ok = bind_ref(C, Rid, RetainId, <<"audio_part:1">>),
        talk_record_t12_support:sql(C, [
            "UPDATE talk_record SET retention_until = now() + interval '30 days' WHERE id = ",
            integer_to_binary(Rid)
        ]),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"blocked_retention">>, cleanup_status(C, Rid)),
        %% 字节保留：引用保持 bound + 资产仍 live
        {St, _AnyCount} = talk_record_t20_support:ref_state(C, Rid),
        ?assertEqual(<<"bound">>, St),
        ?assertEqual(<<"live">>, talk_record_t20_support:attachment_state(C, Path)),
        %% blocked 不被回读改写（refresh 幂等跳过）
        {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
        ?assertEqual(<<"blocked_retention">>, cleanup_status(C, Rid)),
        %% 无 actor 的旧入口拒绝；已鉴权入口仍不得越过留存保护。
        ?assertEqual({error, cleanup_actor_required}, talk_record_delete_logic:retry_purge(Rid)),
        #{<<"revision">> := Revision, <<"cleanup_generation">> := Generation} = row(
            C,
            [
                "SELECT revision,cleanup_generation FROM talk_record WHERE id=",
                integer_to_binary(Rid)
            ]
        ),
        Cap = talk_record_cleanup_retry_repo:capability_tx(C, U1, Rid),
        ?assertEqual(false, maps:get(<<"allowed">>, Cap)),
        ?assertEqual(<<"protected">>, maps:get(<<"reason">>, Cap)),
        ?assertEqual(
            {error, cleanup_protected},
            talk_record_cleanup_retry_logic:retry(
                U1,
                Rid,
                integer_to_binary(Revision),
                <<"t20-retention-retry">>,
                #{<<"cleanup_generation">> => Generation}
            )
        ),
        ?assertEqual(<<"bound">>, element(1, talk_record_t20_support:ref_state(C, Rid))),
        ?assertEqual(<<"live">>, talk_record_t20_support:attachment_state(C, Path))
    end).

%% EAR hold 行 → blocked_hold（defense-in-depth）
purge_blocked_hold_test_() ->
    with_db(purge_blocked_hold_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        U1 = maps:get(u1, talk_record_t20_support:ids()),
        Rid = draft(U1, 93, #{<<"scope">> => <<"organization">>, <<"organization_id">> => 710001}),
        HoldId = nat(301),
        Path = insert_attachment(C, HoldId, located_attachment(Rid, #{})),
        ok = bind_ref(C, Rid, HoldId, <<"audio_part:1">>),
        ok = hold_row(C, HoldId),
        {ok, [#{<<"held_count">> := 1, <<"retained_count">> := 0}]} =
            talk_record_governance_repo:facts_tx(C, Rid),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(<<"blocked_hold">>, cleanup_status(C, Rid)),
        ?assertEqual(<<"bound">>, element(1, talk_record_t20_support:ref_state(C, Rid))),
        ?assertEqual(<<"live">>, talk_record_t20_support:attachment_state(C, Path)),
        {ok, [#{<<"state">> := <<"bound">>, <<"attachment_id">> := HoldId}]} =
            elib_pg:query(
                C,
                <<
                    "SELECT state,attachment_id FROM talk_record_attachment_ref"
                    " WHERE record_id=$1"
                >>,
                [Rid]
            ),
        {ok, [#{<<"n">> := 0}]} = elib_pg:query(
            C,
            <<"SELECT count(*) AS n FROM attachment_object_deletion WHERE path=$1">>,
            [Path]
        ),
        %% 装置自清理：本 case 插入的 EAR hold 行（hold_reason='t20-hold'）是
        %% 测试装置而非治理事实。共享 marker 库的 seed 守卫要求 EAR 表为空，
        %% 行级触发器禁止 DELETE 但不拦 TRUNCATE（t05 合同同款口径）——
        %% 先断言表内仅剩本 case 的装置行，再 TRUNCATE，绝不触碰他者事实。
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            C,
            <<
                "SELECT count(*) AS n FROM enterprise_attachment_retention"
                " WHERE attachment_id=$1 AND hold_reason='t20-hold'"
            >>,
            [HoldId]
        ),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            C, <<"SELECT count(*) AS n FROM enterprise_attachment_retention">>, []
        ),
        talk_record_t12_support:sql(C, "TRUNCATE enterprise_attachment_retention")
    end).

%% 收藏引用（同 hash）→ retained_by_other_reference（字节保留）
purge_retained_by_other_reference_test_() ->
    with_db(purge_retained_by_other_reference_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, U1} = ready(State, 94),
        CollectHash = binary:copy(<<"e">>, 64),
        Path = insert_attachment(
            C, nat(401), located_attachment(Rid, #{hash => CollectHash})
        ),
        ok = bind_ref(C, Rid, nat(401), <<"audio_part:1">>),
        ok = collect_hash(C, nat(401), CollectHash),
        {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
        {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
        ?assertEqual(
            <<"retained_by_other_reference">>,
            cleanup_status(C, Rid)
        ),
        %% 引用已释放（本域视角）；资产因其他业务引用保留字节
        ?assertEqual(<<"released">>, element(1, talk_record_t20_support:ref_state(C, Rid))),
        ?assertEqual(<<"live">>, talk_record_t20_support:attachment_state(C, Path)),
        %% 白名单 protected 标志（受保护≠异常，T21 语义）
        {ok, WL} = talk_record_logic:detail(U1, Rid),
        ?assertEqual(true, maps:get(protected, WL))
    end).

%% Controller prepares eight actual lease expiries in this case's exclusive DB.
%% Never replace this journey with manufactured attempts/done rows or a skip.
purge_failed_then_retry(State) ->
    C = talk_record_t20_support:conn_of(State),
    %% 本根先序 case 的 job/队列行残留是测试装置（含前序 purge 的 derived
    %% ready 行与 queued purge job）；耗尽旅程的 claim/run_once/run_batch 都是
    %% 全局面，先清装置（置于装配之前：CASCADE 不误伤本 case 将建的行）。
    talk_record_t12_support:sql(C, "TRUNCATE talk_record_job CASCADE"),
    talk_record_t12_support:sql(C, "TRUNCATE attachment_object_deletion"),
    %% 本套件 cleanup-retry 旅程的装置行（共享 marker 库，t12 夹具清洁门
    %% 要求该表为空）。
    talk_record_t12_support:sql(C, "TRUNCATE talk_record_cleanup_retry"),
    Ids = talk_record_t20_support:ids(),
    U1 = maps:get(u1, Ids),
    U7 = maps:get(u7, Ids),
    {ok, {Rid, Cid, _PartIds}} = talk_record_t12_support:ready_record(
        U1, nrec(95), <<"record_only">>, [0, 1], nidem(<<"t20-retry-95">>)
    ),
    {ok, created, _} = talk_record_t12_support:complete(
        U1, Rid, Cid, nidem(<<"t20-retry-95-complete">>)
    ),
    {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
    rebind_garage(C, Rid),
    {ok, deleted, _} = talk_record_delete_logic:delete(U1, Rid, <<"1">>),
    {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
    %% 两段上传 + 成品（bind 引用）+ manifest：四个真实对象、四个真实位置
    Rows = attachment_rows(C, Rid),
    ?assertEqual(2, length([R || #{<<"part">> := true} = R <- Rows])),
    ?assertEqual(1, length([R || #{<<"derived">> := true} = R <- Rows])),
    Objects = [garage_object(R) || R <- Rows],
    ?assertEqual(4, length(lists:usort([maps:get(identity, O) || O <- Objects]))),
    Paths = [maps:get(path, O) || O <- Objects],
    ?assertEqual(4, length(Paths)),
    %% 八轮真实 claim → 存储故障 → failed ack（协议观察当场断言）
    ok = exhaust_queue(C, Paths),
    {ok, Rows0} = talk_record_delete_repo:purged_attachment_paths_tx(C, Rid),
    ?assertEqual(lists:sort(Paths), lists:sort([maps:get(<<"path">>, R) || R <- Rows0])),
    {ok, Pending} = elib_pg:query(
        C,
        <<"SELECT path FROM attachment_object_deletion WHERE completed_at IS NULL ORDER BY path">>,
        []
    ),
    ?assertEqual(lists:sort(Paths), [maps:get(<<"path">>, R) || R <- Pending]),
    %% 字节仍在（故障 = 未删除）
    lists:foreach(
        fun(#{row := Row, sha256 := Hash}) ->
            {ok, {{_, 200, _}, _, Bytes}} = fixture_bytes(Row),
            ?assertEqual(Hash, crypto:hash(sha256, Bytes))
        end,
        Objects
    ),
    %% 公共恢复 API：陌生人/owner capability、replay 幂等、世代 CAS
    #{<<"revision">> := Revision, <<"cleanup_generation">> := Generation} = row(C, [
        "SELECT revision,cleanup_generation FROM talk_record WHERE id=",
        integer_to_binary(Rid)
    ]),
    Body = #{<<"cleanup_generation">> => Generation, <<"reason">> => <<"owned fault repaired">>},
    Key =
        <<"cleanup-retry-", (integer_to_binary(Rid))/binary, "-",
            (integer_to_binary(Generation))/binary>>,
    Match = integer_to_binary(Revision),
    ?assertEqual(
        false,
        maps:get(<<"allowed">>, talk_record_cleanup_retry_repo:capability_tx(C, U7, Rid))
    ),
    {error, not_found} = talk_record_cleanup_retry_logic:retry(U7, Rid, Match, Key, Body),
    ?assertEqual(
        true,
        maps:get(<<"allowed">>, talk_record_cleanup_retry_repo:capability_tx(C, U1, Rid))
    ),
    {ok, P} = talk_record_cleanup_retry_logic:retry(U1, Rid, Match, Key, Body),
    ?assert(maps:get(<<"reopened_resources">>, P) > 0),
    {ok, Replay} = talk_record_cleanup_retry_logic:retry(U1, Rid, Match, Key, Body),
    ?assertEqual(maps:get(<<"job_id">>, P), maps:get(<<"job_id">>, Replay)),
    ?assertEqual(true, maps:get(<<"replayed">>, Replay)),
    {error, idempotency_conflict} = talk_record_cleanup_retry_logic:retry(
        U1, Rid, Match, Key, Body#{<<"reason">> => <<"different request">>}
    ),
    {error, cleanup_generation_conflict} = talk_record_cleanup_retry_logic:retry(
        U1,
        Rid,
        integer_to_binary(maps:get(<<"revision">>, P)),
        <<Key/binary, "-stale">>,
        Body
    ),
    %% 控制器故障已修复的等价位：purge 重跑 → 生产 worker 真删 → complete
    {ok, #{outcome := succeeded, kind := <<"purge">>}} = talk_record_job_worker:run_once(),
    {ok, Count} = attachment_delete_worker:run_batch(
        C, fun(_Ns2, _C2, _K, I) -> attachment_storage:delete_snapshot(I) end
    ),
    ?assertEqual(length(Paths), Count),
    lists:foreach(
        fun(#{row := Row}) -> object_gone(Row) end,
        Objects
    ),
    {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
    ?assertEqual(<<"complete">>, cleanup_status(C, Rid)),
    {ok, [#{<<"state">> := <<"succeeded">>}]} = elib_pg:query(
        C,
        <<"SELECT state FROM talk_record_job WHERE id=$1 AND record_id=$2">>,
        [maps:get(<<"job_id">>, P), Rid]
    ),
    ok.

%% 逐行 8 轮真实 claim → 存储故障 → failed ack。claim 目标由生产 fn 的
%% next_retry_at/path 排序决定（本行推进后其余行可能先行），因此按行计数
%% 驱动到各自第 8 轮，每轮当场断言租约窗口/attempts/状态机转移。
exhaust_queue(C, Paths) ->
    exhaust_loop(C, maps:from_list([{P, 0} || P <- Paths])),
    lists:foreach(fun(Path) -> assert_exhausted_queue(C, Path) end, Paths).

exhaust_loop(C, Done) ->
    case [P || {P, N} <- maps:to_list(Done), N < 8] of
        [] ->
            ok;
        _Remaining ->
            Token = queue_token(),
            {ok, Claimed} = attachment_gc_repo:v5_claim(C, Token),
            Path = maps:get(<<"out_path">>, Claimed),
            Round = maps:get(Path, Done) + 1,
            ?assertEqual(Round, maps:get(<<"out_attempts">>, Claimed)),
            Row = queue_row(C, Path),
            ?assertEqual(<<"leased">>, maps:get(<<"job_state">>, Row)),
            ?assertEqual(Token, maps:get(<<"lease_token">>, Row)),
            %% 生产 fn 恒定 120 秒租约（02 §6）；claim 后立即读，窗口 (115,120]
            {ok, [#{<<"win">> := Win}]} = elib_pg:query(
                C,
                <<
                    "SELECT extract(epoch FROM (lease_until - clock_timestamp()))::int AS win"
                    " FROM attachment_object_deletion WHERE path=$1"
                >>,
                [Path]
            ),
            ?assert(Win >= 115 andalso Win =< 120),
            ?assertEqual(ok, attachment_gc_repo:v5_ack(C, Path, Token, false)),
            Expected =
                case Round >= 8 of
                    true -> <<"blocked">>;
                    false -> <<"retry_wait">>
                end,
            After = queue_row(C, Path),
            ?assertEqual(Round, maps:get(<<"attempts">>, After)),
            ?assertEqual(Expected, maps:get(<<"job_state">>, After)),
            ?assertEqual(null, maps:get(<<"lease_token">>, After)),
            ?assertEqual(
                case Round >= 8 of
                    true -> <<"max_attempts">>;
                    false -> <<"delete_failed">>
                end,
                maps:get(<<"last_error_code">>, After)
            ),
            ok = advance_queue_clock(C, Path),
            exhaust_loop(C, Done#{Path := Round})
    end.

assert_exhausted_queue(C, Path) ->
    Row = queue_row(C, Path),
    ?assertEqual(8, maps:get(<<"attempts">>, Row)),
    ?assertEqual(<<"blocked">>, maps:get(<<"job_state">>, Row)),
    ?assertEqual(<<"max_attempts">>, maps:get(<<"last_error_code">>, Row)),
    ?assertEqual(null, maps:get(<<"lease_token">>, Row)),
    ok.

advance_queue_clock(C, Path) ->
    {ok, _} = elib_pg:query(
        C,
        <<
            "UPDATE attachment_object_deletion"
            " SET next_retry_at = clock_timestamp() - interval '1 second'"
            " WHERE path=$1"
        >>,
        [Path]
    ),
    ok.

queue_row(C, Path) ->
    {ok, [Row]} = elib_pg:query(
        C, <<"SELECT * FROM attachment_object_deletion WHERE path=$1">>, [Path]
    ),
    Row.

queue_token() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, Cc:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", Cc/binary, "-", D/binary, "-", E/binary>>.

%%%===================================================================
%%% E. 迟到模型结果不发布
%%%===================================================================

late_results_not_published_test_() ->
    with_db(late_results_not_published_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        %% transcribe 模式记录：complete 投 assemble + transcribe 两 job（不消费）
        {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
            U1, nrec(96), <<"transcribe">>, [0, 1], nidem(<<"t20-96">>)
        ),
        ok = complete_for_cancellation(C, U1, Rid, Cid),
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
    with_db(delete_grants_mutual_exclusion_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        {Rid, _, _} = ready(State, 97),
        C2 = fresh_conn(),
        Ref = make_ref(),
        Parent = self(),
        {Writer, Monitor} = spawn_monitor(fun() -> locked_delete(C, Rid, Parent, Ref) end),
        try
            receive
                {Ref, locked, Writer} -> ok;
                {'DOWN', Monitor, process, Writer, Reason} -> error({writer_before_lock, Reason})
            after 5000 -> error(writer_lock_timeout)
            end,
            {ok, [], []} = epgsql:squery(C2, "SET statement_timeout = '600ms'"),
            {ok, [], []} = epgsql:squery(C2, "BEGIN"),
            Locked =
                try
                    talk_record_repo:lock_for_acl_tx(C2, Rid, 1)
                after
                    {ok, [], []} = epgsql:squery(C2, "ROLLBACK")
                end,
            %% 唯有真实 PG statement_timeout 证明等待持有中的同一行锁。
            ?assertMatch({error, #error{code = <<"57014">>}}, Locked),
            Writer ! {Ref, release},
            receive
                {Ref, committed, Writer} -> ok
            after 5000 -> error(writer_commit_timeout)
            end,
            receive
                {'DOWN', Monitor, process, Writer, normal} -> ok;
                {'DOWN', Monitor, process, Writer, Why} -> error({writer_failed, Why})
            after 5000 -> error(writer_exit_timeout)
            end,
            {ok, [], []} = epgsql:squery(C2, "SET statement_timeout = '5s'"),
            {ok, [], []} = epgsql:squery(C2, "BEGIN"),
            R2 =
                try
                    talk_record_repo:lock_for_acl_tx(C2, Rid, 1)
                after
                    {ok, [], []} = epgsql:squery(C2, "ROLLBACK")
                end,
            ?assertEqual({error, not_found}, R2),
            Deleted = row(C, [
                "SELECT deleted_at,revision,acl_revision FROM talk_record WHERE id=",
                integer_to_binary(Rid)
            ]),
            ?assertNotEqual(null, maps:get(<<"deleted_at">>, Deleted)),
            ?assertEqual(2, maps:get(<<"revision">>, Deleted)),
            ?assertEqual(2, maps:get(<<"acl_revision">>, Deleted))
        after
            try
                release_delete_writer(C, Writer, Monitor, Ref)
            after
                catch epgsql:close(C2)
            end
        end
    end).

locked_delete(C, Rid, Parent, Ref) ->
    ParentMonitor = monitor(process, Parent),
    try
        committed = raw_tx(C, fun(Conn) ->
            {ok, live, _} = talk_record_delete_repo:lock_for_delete_tx(Conn, Rid, 1),
            {ok, _} = talk_record_delete_repo:soft_delete_tx(Conn, Rid),
            Parent ! {Ref, locked, self()},
            receive
                {Ref, release} -> committed;
                {'DOWN', ParentMonitor, process, Parent, Why} -> error({parent_down, Why})
            after 5000 -> error(writer_release_timeout)
            end
        end),
        Parent ! {Ref, committed, self()}
    after
        demonitor(ParentMonitor, [flush])
    end.

release_delete_writer(C, Writer, Monitor, Ref) ->
    Writer ! {Ref, release},
    case is_process_alive(Writer) of
        false ->
            demonitor(Monitor, [flush]);
        true ->
            receive
                {'DOWN', Monitor, process, Writer, _} -> ok
            after 5000 ->
                exit(Writer, kill),
                receive
                    {'DOWN', Monitor, process, Writer, _} -> ok
                after 5000 -> error(writer_cleanup_timeout)
                end,
                {ok, [], []} = epgsql:squery(C, "ROLLBACK")
            end
    end.

grants_first_then_delete_consistent_test_() ->
    with_db(grants_first_then_delete_consistent_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {Rid, _, _} = ready(State, 98),
        %% 前序运行残留的双向好友行会使下方拒绝预言提前失效（共享 marker
        %% 库、personal 目录判定=双向 user_friend），先清两用户间全部好友行。
        {ok, _} = elib_pg:query(
            C,
            <<
                "DELETE FROM user_friend WHERE (from_user_id=$1 AND to_user_id=$2)"
                " OR (from_user_id=$2 AND to_user_id=$1)"
            >>,
            [U1, U2]
        ),
        %% Personal grants require actual mutual active contacts, not merely
        %% membership in the same organization. Preserve the rejection oracle.
        ?assertEqual(
            {error, target_not_in_directory},
            talk_record_grant_logic:replace(U1, Rid, <<"acl:1">>, #{
                <<"grants">> => [
                    #{
                        <<"grantee_user_id">> => U2,
                        <<"role">> => <<"viewer">>,
                        <<"allow_export">> => false
                    }
                ]
            })
        ),
        {ok, []} = elib_pg:query(
            C,
            <<
                "INSERT INTO user_friend(id,from_user_id,to_user_id,status) VALUES"
                " ($1,$2,$3,1),($4,$3,$2,1)"
            >>,
            [960001, U1, U2, 960002]
        ),
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
    with_db(preflight_facts_personal_vs_organization_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        {Rid1, _, _} = ready(State, 99),
        {Rid2, _, _} = ready(State, 50),
        %% Rid2 转企业归属（SQL 直改——迁移 209 归属不可经 API 改，测试装配）
        talk_record_t12_support:sql(C, [
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
    with_db(preflight_run_integration_test_, fun(State) ->
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
                lists:member(maps:get(code, B), [
                    <<"TALK_RECORD_PERSONAL_PENDING">>,
                    <<"TALK_RECORD_RETAINED_IDENTITY_PENDING">>
                ])
            ],
            ?assertEqual(2, length(TalkBlockers)),
            lists:foreach(
                fun(B) ->
                    ?assertEqual(
                        lists:sort([code, resource_type, resource_id, organization_id]),
                        lists:sort(maps:keys(B))
                    ),
                    ?assertEqual(<<"account">>, maps:get(resource_type, B)),
                    ?assertEqual(integer_to_binary(U1), maps:get(resource_id, B)),
                    ?assertEqual(null, maps:get(organization_id, B))
                end,
                TalkBlockers
            ),
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
    with_db(departure_custody_projection_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        OwnerSql = <<
            "SELECT o.owner_id,m.role,m.status FROM organization o"
            " JOIN organization_member m ON m.organization_id=o.id AND m.user_id=o.owner_id"
            " WHERE o.id=710001"
        >>,
        {ok, [OwnerBefore]} = elib_pg:query(C, OwnerSql, []),
        ?assertMatch(
            #{
                <<"owner_id">> := U1,
                <<"role">> := <<"owner">>,
                <<"status">> := <<"active">>
            },
            OwnerBefore
        ),
        {Rid1, _, U2} = ready(State, 53, U2),
        {Rid2, _, U2} = ready(State, 54, U2),
        %% 离企主体为普通成员 U2；不得制造有效企业 owner 失权的非法状态。
        talk_record_t12_support:sql(C, [
            "UPDATE talk_record SET scope='organization', organization_id=710001,",
            " workspace_id=NULL WHERE id = ",
            integer_to_binary(Rid2)
        ]),
        %% U2 失去企业成员资格（离企/注销投影触发条件）
        talk_record_t12_support:sql(C, [
            "UPDATE organization_member SET status='suspended'",
            " WHERE organization_id=710001 AND user_id=",
            integer_to_binary(U2)
        ]),
        {ok, 1} = elib_pg:with_tx(
            fun(Conn) -> talk_record_delete_logic:project_departure_custody_tx(Conn, U2) end,
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
        %% 记录 owner 离企后不可见；企业 owner 身份约束始终保持。
        ?assertMatch({error, not_found}, talk_record_ds:detail(U2, Rid2)),
        ?assertEqual({ok, [OwnerBefore]}, elib_pg:query(C, OwnerSql, []))
    end).

%%%===================================================================
%%% H. T06-L2 修复回归
%%%===================================================================

scan_organization_l2_regression_test_() ->
    with_db(scan_organization_l2_regression_test_, fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        OrgA = maps:get(org_a, Ids),
        {Rid1, _, _} = ready(State, 55),
        talk_record_t12_support:sql(C, [
            "UPDATE talk_record SET scope='organization', organization_id=",
            integer_to_binary(OrgA),
            ", workspace_id=NULL WHERE id = ",
            integer_to_binary(Rid1)
        ]),
        %% 附件命名：一个字面含 % 的名字 + 一个普通名
        Path1 = insert_attachment(
            C,
            nat(601),
            #{record_id => Rid1, gc_verified => false}
        ),
        talk_record_t12_support:sql(C, [
            "UPDATE attachment SET name='plan 50% done' WHERE path = '", Path1, "'"
        ]),
        DirtyId = nat(602),
        DirtyPath = iolist_to_binary(["t20/dirty-", integer_to_binary(DirtyId)]),
        talk_record_t12_support:sql(C, [
            "INSERT INTO attachment(id, file_hash256, mime_type, name, path, url, size,"
            " referer_time, creator_user_id, scope, scope_ref, gc_verified)"
            " VALUES (",
            integer_to_binary(DirtyId),
            ", repeat('d',64), 'audio/aac', 'abc50x', '",
            DirtyPath,
            "', '",
            DirtyPath,
            "', 10, 1, ",
            integer_to_binary(U1),
            ", 'talk_record', 'not-a-number', false)"
        ]),
        %% 1) 脏 scope_ref（非数字）不再炸查询（文本比较跳行；本 org 的
        %% talk_record 附件含分段附件与 8601，脏行不在内）
        {ok, Rows} = talk_record_attachment_ref_repo:scan_organization_tx(
            C, OrgA, #{}
        ),
        ?assert(lists:member(Path1, [maps:get(<<"path">>, R) || R <- Rows])),
        ?assertNot(
            lists:member(
                DirtyPath,
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

%% Owned-lane continuation of the controller-frozen exhaustion journey: for
%% every real queued object, eight actual claim → storage-fault → failed-ack
%% rounds driven through the production queue functions (lease window,
%% attempts and backoff state asserted as they happen; the local lane advances
%% the clock columns between rounds — same nature as the upload-window drain —
%% while every row/token/state transition is produced by the real fns), then
%% the public authenticated recovery through production APIs to physical
%% completion. No manufactured attempts/done rows, no skip.
purge_failed_then_retry_test_() ->
    with_db(purge_failed_then_retry_test_, fun purge_failed_then_retry/1).
