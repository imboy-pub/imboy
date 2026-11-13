%% T06 租约 fencing 真实 PG 验收：仅在一次性独占库 + 新 VM 显式调用，
%% 不被 eunit 自动发现（无 _test）。repo/worker/事务全部真实，无业务 meck。
%%
%% 覆盖验收矩阵行"两节点 claim、慢批超 lease、worker 中断"：
%%  - 双 claim 并发不重叠（FOR UPDATE SKIP LOCKED）
%%  - claim RETURNING 原始精度 available_at::text token
%%  - 慢批超 lease → 新 owner 重领；旧 owner CAS 续租 0 行
%%  - stale owner 的完成/失败/终态写回全部 0 行（不破坏新 owner 状态）
%%  - stale owner 的 complete_staged 回滚，绝不触发 ACK 清理
%%  - 新 owner 完成成功且 ACK 清理恰好执行（游标与 ACK 一致）
%%  - 全程无永久滞留、无重复业务效果
-module(msg_store_worker_fencing_pg_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(ConnOpts0) ->
    ConnOpts = ConnOpts0#{codecs => [{epgsql_codec_rfc3339_bin, []}]},
    assert_no_app(),
    undefined = whereis(msg_store_worker),
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    ok = application:set_env(imboy, msg_archive_enabled, false),
    ok = application:set_env(imboy, msg_store_worker_tick_ms, 0),
    ok = elib_tsid:init(#{dc_id => 0, node_id => 1}),
    {ok, Verify} = epgsql:connect(ConnOpts),
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 4,
        max_count => 4,
        queue_max => 50,
        start_mfa => {epgsql, connect, [ConnOpts]}
    }),
    try
        create_schema(Verify),
        concurrent_claims_are_exclusive(Verify),
        claim_token_is_raw_available_at(Verify),
        Slow = lease_expiry_scenario(Verify),
        stale_writebacks_rejected(Verify, Slow),
        stale_owner_never_triggers_ack_cleanup(Verify, Slow),
        new_owner_completes_and_cleans_ack(Verify, Slow),
        worker_skips_expired_row_entirely(Verify),
        assert_no_permanent_stall(Verify),
        io:format("MSG_STORE_WORKER_FENCING_PG_CHECKS=PASS~n"),
        ok
    after
        stop_worker(),
        pooler:rm_pool(pgsql),
        epgsql:close(Verify)
    end.

%% ------------------------------------------------------------------
%% 场景 1：两个"节点"并发 claim 同一批池，行集不相交。
%% ------------------------------------------------------------------
concurrent_claims_are_exclusive(C) ->
    seed_staging_rows(C, 20, <<"conc">>),
    Self = self(),
    Claimants = [
        spawn_link(fun() -> Self ! {claim, self(), msg_store_repo:claim_pending(10, 30)} end)
     || _ <- lists:seq(1, 2)
    ],
    Results = [
        receive
            {claim, P, R} -> R
        end
     || P <- Claimants
    ],
    [{ok, RowsA}, {ok, RowsB}] = Results,
    IdsA = [maps:get(<<"id">>, R) || R <- RowsA],
    IdsB = [maps:get(<<"id">>, R) || R <- RowsB],
    %% 两个"节点"的 claim 行集必须不相交（两个方向的交集都为空）。
    ?assertEqual([], IdsA -- (IdsA -- IdsB)),
    ?assertEqual([], IdsB -- (IdsB -- IdsA)),
    ?assertEqual(10, length(IdsA)),
    ?assertEqual(10, length(IdsB)),
    io:format("FENCING_CONCURRENT_CLAIM_EXCLUSIVE=PASS (~p vs ~p)~n", [length(IdsA), length(IdsB)]).

%% ------------------------------------------------------------------
%% 场景 2：lease token 是 DB 内 available_at 的原始精度文本。
%% ------------------------------------------------------------------
claim_token_is_raw_available_at(C) ->
    seed_staging_rows(C, 1, <<"token">>),
    {ok, [Row]} = msg_store_repo:claim_pending(1, 30),
    Token = maps:get(<<"lease_token">>, Row, missing),
    ?assert(is_binary(Token)),
    DbText = scalar(
        C,
        "SELECT available_at::text FROM msg_store_staging WHERE id=$1",
        [maps:get(<<"id">>, Row)]
    ),
    ?assertEqual(DbText, Token),
    %% 原始精度 = 逐字符相等（含微秒段），不是格式化近似。
    ?assert(length(binary:split(DbText, <<".">>)) =:= 2),
    io:format("FENCING_TOKEN_RAW_PRECISION=PASS (~s)~n", [Token]).

%% ------------------------------------------------------------------
%% 场景 3：慢批超 lease → 新 owner 重领；旧 token 全部失效。
%% 返回两代 claim 的行供后续场景复用。
%% ------------------------------------------------------------------
lease_expiry_scenario(C) ->
    MsgId = <<"fence-slow-1">>,
    StagingId = seed_one_s2c(C, MsgId, 77001, 77),
    seed_delivery(C, <<"s2c">>, MsgId, 77, <<"dev-x">>),
    {ok, [RowA]} = msg_store_repo:claim_pending(1, 1),
    ?assertEqual(StagingId, maps:get(<<"id">>, RowA)),
    TokenA = maps:get(<<"lease_token">>, RowA),
    timer:sleep(1300),
    {ok, [RowB]} = msg_store_repo:claim_pending(1, 30),
    ?assertEqual(StagingId, maps:get(<<"id">>, RowB)),
    TokenB = maps:get(<<"lease_token">>, RowB),
    ?assertNotEqual(TokenA, TokenB),
    Id = maps:get(<<"id">>, RowA),
    ?assertEqual({ok, 0}, msg_store_repo:renew_lease(Id, TokenA, 30)),
    %% 注意：这里不能再用 TokenB 续租——续租成功即改写 available_at，
    %% TokenB 随之失效；新 owner 的续租能力由场景 6 的 process_row 内部
    %% （renew_row_lease 首步）真实完成。
    io:format("FENCING_LEASE_EXPIRY_RECLAIM=PASS~n"),
    #{
        id => Id,
        msg_id => MsgId,
        row_a => RowA,
        row_b => RowB,
        token_a => TokenA,
        token_b => TokenB
    }.

%% ------------------------------------------------------------------
%% 场景 4：stale owner 的完成/失败/终态写回全部被 CAS 拒绝（0 行），
%% 新 owner 的行状态不被破坏。
%% ------------------------------------------------------------------
stale_writebacks_rejected(C, #{id := Id, msg_id := MsgId, token_a := TokenA}) ->
    Before = row_state(C, Id),
    ?assertEqual(
        {ok, 0}, msg_store_repo:mark_processed(C, <<"s2c">>, MsgId, TokenA)
    ),
    ?assertEqual(
        {ok, 0}, msg_store_repo:mark_failed(<<"s2c">>, MsgId, <<"stale">>, 1, TokenA)
    ),
    ?assertEqual(
        {ok, 0}, msg_store_repo:mark_terminal(<<"s2c">>, MsgId, <<"stale">>, TokenA)
    ),
    ?assertEqual(Before, row_state(C, Id)),
    io:format("FENCING_STALE_WRITEBACK_REJECTED=PASS~n").

%% ------------------------------------------------------------------
%% 场景 5：正式表已写、完成写回才发现 stale 的 interleaving——
%% complete_staged 必须 rollback，绝不能触发 ACK 清理（reconcile/
%% delete_delivered_batch 均未执行，delivery 与主行原样保留）。
%% ------------------------------------------------------------------
stale_owner_never_triggers_ack_cleanup(C, #{row_a := RowA, msg_id := MsgId}) ->
    %% 真实时间线：A 在 lease 过期前已把消息写入正式表（与 worker 同款
    %% do_write，保证 B 侧重放时逐列一致、幂等冲突路径可达），随后完成
    %% 写回时 token 已失效。
    ok = msg_store_worker:do_write(s2c, RowA),
    Result = msg_store_worker:complete_staged(RowA),
    ?assertMatch({error, {stale_lease, <<"s2c">>, MsgId}}, Result),
    %% stale 完成不得动 staging（仍是 pending）、不得清 delivery、不得删主行。
    ?assertEqual(1, scalar(C, "SELECT count(*) FROM msg_delivery WHERE msg_id=$1", [MsgId])),
    ?assertEqual(1, scalar(C, "SELECT count(*) FROM msg_s2c WHERE msg_id=$1", [MsgId])),
    ?assertEqual(
        1,
        scalar(
            C, "SELECT count(*) FROM msg_store_staging WHERE msg_id=$1 AND processed_at IS NULL", [
                MsgId
            ]
        )
    ),
    io:format("FENCING_STALE_NO_ACK_CLEANUP=PASS~n").

%% ------------------------------------------------------------------
%% 场景 6：新 owner B 完整 process_row 成功：完成写回成功、ACK 清理
%% 恰好执行一次（delivery 清空、已确认主行删除）、staging 转正、无重复。
%% ------------------------------------------------------------------
new_owner_completes_and_cleans_ack(C, #{row_b := RowB, msg_id := MsgId}) ->
    ok = msg_store_worker:process_row(RowB),
    ?assertEqual(
        1,
        scalar(
            C,
            "SELECT count(*) FROM msg_store_staging WHERE msg_id=$1 "
            "AND processed_at IS NOT NULL AND error_msg IS NULL",
            [MsgId]
        )
    ),
    ?assertEqual(0, scalar(C, "SELECT count(*) FROM msg_delivery WHERE msg_id=$1", [MsgId])),
    %% 消息已被全部设备确认 → 主行按 ACK 清理语义删除（游标与 ACK 一致）。
    ?assertEqual(0, scalar(C, "SELECT count(*) FROM msg_s2c WHERE msg_id=$1", [MsgId])),
    io:format("FENCING_NEW_OWNER_COMPLETES_ACK_CONSISTENT=PASS~n").

%% ------------------------------------------------------------------
%% 场景 7：worker 收到已过期（被重领）的行时整体跳过——不写正式表、
%% 不 mark 任何状态，行留给新 owner。模拟"worker 中断后恢复仍握旧行"。
%% ------------------------------------------------------------------
worker_skips_expired_row_entirely(C) ->
    MsgId = <<"fence-skip-1">>,
    Id = seed_one_s2c(C, MsgId, 77002, 78),
    {ok, [RowA]} = msg_store_repo:claim_pending(1, 1),
    timer:sleep(1300),
    {ok, [RowB]} = msg_store_repo:claim_pending(1, 30),
    Before = row_state(C, Id),
    ok = msg_store_worker:process_row(RowA),
    ?assertEqual(Before, row_state(C, Id)),
    %% 正式表绝不能被过期 owner 写入。
    ?assertEqual(0, scalar(C, "SELECT count(*) FROM msg_s2c WHERE msg_id=$1", [MsgId])),
    %% 新 owner 正常完成（无永久滞留）。
    ok = msg_store_worker:process_row(RowB),
    ?assertEqual(
        1,
        scalar(
            C,
            "SELECT count(*) FROM msg_store_staging WHERE msg_id=$1 AND processed_at IS NOT NULL",
            [MsgId]
        )
    ),
    io:format("FENCING_WORKER_SKIP_EXPIRED_ROW=PASS~n").

%% 行为级"无永久滞留"证明：此前场景遗留的已 claim 行在租约到期后全部
%% 可被下一轮 claim 并转正。把遗留 pending 行的 available_at 归位到当前
%% 时刻（等价于等待租约自然到期，前面场景已证明未过期行不可领取），
%% 然后 drain 到空：结束时不存在 pending 行，也不存在到期未领的行。
assert_no_permanent_stall(C) ->
    sql(C, "UPDATE msg_store_staging SET available_at = NOW() WHERE processed_at IS NULL", []),
    drain_all(50),
    Pending = scalar(C, "SELECT count(*) FROM msg_store_staging WHERE processed_at IS NULL", []),
    ?assertEqual(0, Pending),
    Ready =
        scalar(
            C,
            "SELECT count(*) FROM msg_store_staging WHERE processed_at IS NULL AND available_at <= NOW()",
            []
        ),
    ?assertEqual(0, Ready),
    io:format("FENCING_NO_PERMANENT_STALL=PASS (pending=0, claimable=0)~n").

drain_all(0) ->
    ok;
drain_all(N) ->
    case msg_store_repo:claim_pending(100, 30) of
        {ok, []} ->
            ok;
        {ok, Rows} ->
            _ = [msg_store_worker:process_row(Row) || Row <- Rows],
            drain_all(N - 1)
    end.

%% ================= fixtures =================

create_schema(C) ->
    Names = [
        <<"msg_store_staging">>,
        <<"msg_s2c">>,
        <<"msg_delivery">>,
        <<"user_device">>
    ],
    ?assertEqual(
        0,
        scalar(
            C,
            "SELECT count(*) FROM pg_tables WHERE schemaname='public' AND tablename=ANY($1::text[])",
            [Names]
        )
    ),
    %% 列兼容切片：165（staging）+ pg_checks 同款 msg_s2c/msg_delivery/user_device。
    Ddl = [
        "CREATE TABLE msg_s2c(id bigint NOT NULL,from_id bigint NOT NULL,to_id bigint NOT NULL,"
        "msg_id varchar(40) NOT NULL,action varchar(40) NOT NULL,msg_type varchar(40) DEFAULT '' NOT NULL,"
        "e2ee jsonb,payload jsonb NOT NULL,server_ts timestamptz DEFAULT now(),"
        "created_at timestamptz NOT NULL DEFAULT now(),PRIMARY KEY(id,created_at))",
        "CREATE TABLE msg_delivery(msg_kind varchar(8) NOT NULL,msg_id varchar(40) NOT NULL,"
        "to_uid bigint NOT NULL,to_did varchar(40) NOT NULL,created_at timestamptz DEFAULT now(),"
        "PRIMARY KEY(msg_kind,msg_id,to_uid,to_did))",
        "CREATE TABLE user_device(user_id bigint NOT NULL,device_id varchar(40) NOT NULL,"
        "status smallint DEFAULT 1,last_active_at timestamptz DEFAULT now())"
    ],
    [sql(C, D, []) || D <- Ddl],
    {ok, StagingDdl} = file:read_file(
        "priv/migrations/00000165_msg_staging_before_tsid_bootstrap.up.sql"
    ),
    Results = epgsql:squery(C, StagingDdl),
    ?assert(is_list(Results)),
    ?assertEqual([], [Error || {error, _} = Error <- Results]),
    sql(
        C,
        "ALTER TABLE msg_store_staging ADD COLUMN accepted_at timestamptz,ADD COLUMN expire_at timestamptz",
        []
    ),
    ok.

seed_staging_rows(C, N, Prefix) ->
    sql(C, "TRUNCATE msg_store_staging,msg_s2c,msg_delivery RESTART IDENTITY", []),
    [
        begin
            sql(
                C,
                "INSERT INTO msg_store_staging(id,type,msg_id,msg_type,action,payload,from_id,to_id,created_at,server_ts) "
                "VALUES($1,'s2c',$2,'','probe','{}'::jsonb,1,77,now(),now())",
                [900000 + I, <<Prefix/binary, (integer_to_binary(I))/binary>>]
            )
        end
     || I <- lists:seq(1, N)
    ],
    ok.

seed_one_s2c(C, MsgId, StagingId, ToId) ->
    sql(
        C,
        "INSERT INTO msg_store_staging(id,type,msg_id,msg_type,action,payload,from_id,to_id,created_at,server_ts) "
        "VALUES($1,'s2c',$2,'','probe','{}'::jsonb,1,$3,now(),now())",
        [StagingId, MsgId, ToId]
    ),
    StagingId.

seed_delivery(C, Kind, MsgId, ToUid, ToDid) ->
    sql(
        C,
        "INSERT INTO msg_delivery(msg_kind,msg_id,to_uid,to_did) VALUES($1,$2,$3,$4)",
        [Kind, MsgId, ToUid, ToDid]
    ),
    ok.

row_state(C, Id) ->
    rows(
        C,
        "SELECT processed_at IS NULL AS pending, retry_count, error_msg IS NULL AS no_error, "
        "available_at IS NOT NULL AS has_lease FROM msg_store_staging WHERE id=$1",
        [Id]
    ).

sql(C, Sql, Params) ->
    case epgsql:equery(C, iolist_to_binary(Sql), Params) of
        {ok, _} -> ok;
        {ok, [], []} -> ok;
        Other -> error({fixture_sql_failed, Sql, Other})
    end.

rows(C, Sql, Params) ->
    case epgsql:equery(C, Sql, Params) of
        {ok, _, R} -> R;
        Other -> error({fixture_query_failed, Sql, Other})
    end.

scalar(C, Sql) ->
    scalar(C, Sql, []).

scalar(C, Sql, Params) ->
    [{Value}] = rows(C, Sql, Params),
    Value.

assert_no_app() ->
    ?assertNot(lists:keymember(imboy, 1, application:which_applications())).

stop_worker() ->
    case whereis(msg_store_worker) of
        undefined -> ok;
        Pid -> gen_statem:stop(Pid)
    end.
