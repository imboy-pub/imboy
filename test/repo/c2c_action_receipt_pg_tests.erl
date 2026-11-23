-module(c2c_action_receipt_pg_tests).

-include_lib("eunit/include/eunit.hrl").

c2c_action_receipt_pg_test_() ->
    {timeout, 900,
        {setup, fun setup/0, fun cleanup/1, fun(_State) ->
            [
                ?_test(authenticity_and_replay()),
                ?_test(forward_revoke_serializes()),
                ?_test(concurrent_single_consumer()),
                ?_test(missing_target_keeps_receipt()),
                ?_test(worker_reconciles_stale_snapshot()),
                ?_test(edit_revoke_ordering_and_replay()),
                ?_test(action_version_migration_compat()),
                ?_test(edit_newest_accepted_is_authoritative()),
                ?_test(revoke_terminal_state_never_revives()),
                ?_test(server_time_authority_and_window_boundary()),
                ?_test(terminal_states_survive_history_repull())
            ]
        end}}.

setup() ->
    application:load(imboy),
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, msg_archive_enabled, false),
    %% 本套件不经 eunit_runner boot，直调 msg_store_worker:process_row 会走到
    %% elib_tsid:generate(msg_c2c)；TSID runtime 未初始化时写入报
    %% elib_tsid_not_initialized。按仓库测试惯例幂等初始化（已初始化则保留）。
    try
        elib_tsid:init(#{dc_id => 0, node_id => 0, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    {ok, _} = application:ensure_all_started(lager),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"C2C_ACTION_ACK">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    application:set_env(pooler, pools, []),
    {ok, _} = application:ensure_all_started(pooler),
    %% 全量跑时前序根（runner 默认池或漏清 pgsql 池的套件，如
    %% group_schedule_remind_pg_tests）可能已注册同名 pgsql 池，顺序耦合
    %% 下 new_pool 报 {error,{already_started,_}}（CI run 41393644512 实证）。
    %% 先移除再建，保证池 start_mfa 指向本套件自己的 marker database，
    %% 绝不复用他人指向的库。rm_pool 不存在时返回 {error, not_found}
    %% （pooler_sup:rm_pool 同步 terminate+delete，建池前必然已腾位）。
    _ = pooler:rm_pool(pgsql),
    PoolOpts = (maps:get(server, State))#{
        database => binary_to_list(maps:get(db, State)),
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 4,
        max_count => 4,
        start_mfa => {epgsql, connect, [PoolOpts]}
    }),
    State.

cleanup(State) ->
    pooler:rm_pool(pgsql),
    inttest_marker_db:release(State),
    application:unset_env(imboy, msg_archive_enabled).

authenticity_and_replay() ->
    Original = <<"ack-auth-original">>,
    Request = <<"ack-auth-request">>,
    Hash = hash(<<"accepted content">>),
    Result = <<"{\"action\":\"message_edit_ack\",\"content\":\"accepted content\"}">>,
    insert_formal(Original, <<"old">>),
    ?assertEqual(
        {ok, new},
        c2c_action_receipt_repo:accept(Request, Original, 101, 202, <<"edit">>, Hash)
    ),
    ?assertEqual(
        {ok, duplicate},
        c2c_action_receipt_repo:accept(Request, Original, 101, 202, <<"edit">>, Hash)
    ),
    ?assertEqual(
        {error, conflict},
        c2c_action_receipt_repo:accept(Request, Original, 101, 202, <<"revoke">>, null)
    ),
    ?assertEqual(
        {error, not_found},
        c2c_action_receipt_repo:consume(Request, Original, 101, <<"edit">>, Hash, Result)
    ),
    ?assertEqual(
        {error, not_found},
        c2c_action_receipt_repo:consume(
            Request, Original, 202, <<"edit">>, hash(<<"tampered">>), Result
        )
    ),
    ?assertEqual(<<"old">>, formal_payload(Original)),
    ?assertEqual(
        ok, c2c_action_receipt_repo:consume(Request, Original, 202, <<"edit">>, Hash, Result)
    ),
    ?assertEqual(Result, formal_payload(Original)),
    ?assertEqual(
        {error, replayed},
        c2c_action_receipt_repo:consume(Request, Original, 202, <<"edit">>, Hash, Result)
    ).

forward_revoke_serializes() ->
    Original = <<"forward-lock-original">>,
    Request = <<"forward-lock-request">>,
    insert_formal(Original, <<"old">>),
    {ok, [#{<<"conv_seq">> := Seq}]} = elib_pg:query(
        <<"SELECT conv_seq FROM public.msg_c2c WHERE msg_id=$1">>, [Original]
    ),
    {ok, 1} = elib_pg:execute_count(
        <<"INSERT INTO public.c2c_forward_source_fact(msg_id,sender_id,recipient_id,conv_seq) VALUES($1,101,202,$2)">>,
        [Original, Seq]
    ),
    Parent = self(),
    Gate = spawn(fun() ->
        Result = elib_pg:with_tx(fun(C) ->
            ok = forward_attachment_source_repo:c2c_tx(C, Original, 101),
            Parent ! {forward_lock_held, self()},
            receive
                release_forward_lock -> ok
            after 10000 -> error(lock_release_timeout)
            end
        end),
        Parent ! {forward_lock_result, Result}
    end),
    receive
        {forward_lock_held, Gate} -> ok
    after 10000 -> error(lock_wait_timeout)
    end,
    Revoke = spawn(fun() ->
        Parent ! {forward_revoke_trying, self()},
        Parent !
            {forward_revoke_result,
                c2c_action_receipt_repo:accept(
                    Request, Original, 101, 202, <<"revoke">>, null
                )}
    end),
    receive
        {forward_revoke_trying, Revoke} -> ok
    after 10000 -> error(revoke_start_timeout)
    end,
    receive
        {forward_revoke_result, Unexpected} -> error({revoke_crossed_lock, Unexpected})
    after 200 -> ok
    end,
    Gate ! release_forward_lock,
    receive
        {forward_lock_result, ok} -> ok
    after 10000 -> error(lock_finish_timeout)
    end,
    receive
        {forward_revoke_result, {ok, new}} -> ok
    after 10000 -> error(revoke_finish_timeout)
    end,
    ?assertEqual(
        {error, forbidden},
        elib_pg:with_tx(fun(C) -> forward_attachment_source_repo:c2c_tx(C, Original, 101) end)
    ).

concurrent_single_consumer() ->
    Original = <<"ack-race-original">>,
    Request = <<"ack-race-request">>,
    Hash = hash(<<"race content">>),
    Payload = <<"{\"action\":\"message_edit_ack\",\"content\":\"race content\"}">>,
    insert_formal(Original, <<"old">>),
    {ok, new} = c2c_action_receipt_repo:accept(
        Request, Original, 101, 202, <<"edit">>, Hash
    ),
    Parent = self(),
    Run = fun() ->
        Parent !
            {
                consumed,
                c2c_action_receipt_repo:consume(
                    Request, Original, 202, <<"edit">>, Hash, Payload
                )
            }
    end,
    _ = [spawn(Run) || _ <- [1, 2]],
    Results = lists:sort([receive_result(), receive_result()]),
    %% Erlang 项序：atom(ok) < tuple({error,_})，排序后恒为 [ok, {error, replayed}]；
    %% 恰好一个消费者成功、另一个 replayed（consumed_count 断言兜底单一消费）。
    ?assertEqual([ok, {error, replayed}], Results),
    ?assertEqual(Payload, formal_payload(Original)),
    ?assertEqual(1, consumed_count(Request)).

missing_target_keeps_receipt() ->
    Original = <<"ack-missing-original">>,
    Request = <<"ack-missing-request">>,
    {ok, new} = c2c_action_receipt_repo:accept(
        Request, Original, 101, 202, <<"revoke">>, null
    ),
    ?assertEqual(
        {error, not_found},
        c2c_action_receipt_repo:consume(
            Request, Original, 202, <<"revoke">>, null, <<"{\"action\":\"message_revoke_ack\"}">>
        )
    ),
    ?assertEqual(0, consumed_count(Request)).

worker_reconciles_stale_snapshot() ->
    Original = <<"ack-staging-original">>,
    Request = <<"ack-staging-request">>,
    Result = <<"{\"action\":\"message_revoke_ack\",\"original_msg_id\":\"ack-staging-original\"}">>,
    insert_staging(Original, <<"{\"content\":\"old snapshot\"}">>),
    {ok, [OldSnapshot]} = elib_pg:query(
        <<"SELECT * FROM public.msg_store_staging WHERE type='c2c' AND msg_id=$1">>,
        [Original]
    ),
    {ok, new} = c2c_action_receipt_repo:accept(
        Request, Original, 101, 202, <<"revoke">>, null
    ),
    ok = c2c_action_receipt_repo:consume(
        Request, Original, 202, <<"revoke">>, null, Result
    ),
    ok = msg_store_worker:process_row(OldSnapshot),
    %% consume_action 侧的 AckPayload 经 encode_json（jsone 默认带空格排版）落
    %% result_payload/staging/formal；对账后 formal 以已消费事实为准。断言解码
    %% 语义等价，不绑定 JSON 具体排版。
    ExpectedMap = jsone:decode(Result),
    ?assertEqual(ExpectedMap, jsone:decode(formal_payload(Original))),
    ?assertEqual(ExpectedMap, jsone:decode(staging_payload(Original))).

insert_formal(MsgId, Payload) ->
    {ok, 1} = elib_pg:execute_count(
        <<"INSERT INTO public.msg_c2c ", "(id,msg_id,from_id,to_id,payload,msg_type,conv_seq) ",
            "VALUES($1,$2,101,202,$3,'text',$4)">>,
        %% conv_seq 带 monotonic：c2c_order_write_guard 要求新行 seq 严格大于
        %% 同 pair 既有最大值，无 monotonic 的 unique_integer 不保证递增，
        %% 概率性回退即触发 c2c_order_conflict（run 41424669122 实证）。
        [
            erlang:unique_integer([positive]),
            MsgId,
            Payload,
            erlang:unique_integer([positive, monotonic])
        ]
    ),
    ok.

insert_staging(MsgId, Payload) ->
    {ok, 1} = elib_pg:execute_count(
        <<"INSERT INTO public.msg_store_staging ",
            "(id,type,msg_id,msg_type,payload,from_id,to_id,conv_seq) ",
            "VALUES($1,'c2c',$2,'text',$3::jsonb,101,202,$4)">>,
        [
            erlang:unique_integer([positive]),
            MsgId,
            Payload,
            erlang:unique_integer([positive, monotonic])
        ]
    ),
    ok.

formal_payload(MsgId) ->
    scalar(<<"SELECT payload FROM public.msg_c2c WHERE msg_id=$1">>, [MsgId]).

staging_payload(MsgId) ->
    scalar(<<"SELECT payload::text FROM public.msg_store_staging WHERE msg_id=$1">>, [MsgId]).

consumed_count(RequestId) ->
    scalar(
        <<"SELECT count(*) FROM public.c2c_action_receipt ",
            "WHERE request_id=$1 AND consumed_at IS NOT NULL">>,
        [RequestId]
    ).

scalar(Sql, Params) ->
    {ok, [Row]} = elib_pg:query(Sql, Params),
    hd(maps:values(Row)).

receive_result() ->
    receive
        {consumed, Result} -> Result
    after 10000 ->
        error(consume_timeout)
    end.

hash(Content) ->
    binary:encode_hex(crypto:hash(sha256, Content), lowercase).

%% BE-03（IMBOY_MASTER_V2_2_20261118）：同一 original 的编辑/撤回动作序与
%% 重放幂等（真库）。权威语义：advisory lock 按 original 串行 accept；
%% request_id 幂等（同参重放=duplicate，异参拒绝）；consume 后 replayed。
%% 「接受提交后崩溃恢复」= 状态全部在库（receipt 行+consumed_at），不依赖
%% 原进程内存——本用例全程独立连接/事务即证明。
edit_revoke_ordering_and_replay() ->
    Original = <<"be03-order-original">>,
    %% consume_locked 按 (msg_id, actor, peer) 更新 msg_c2c 正文——先落正式行
    insert_formal(Original, <<"old">>),
    EditReq = <<"be03-order-edit-req">>,
    RevokeReq = <<"be03-order-revoke-req">>,
    EditHash = hash(<<"edited content v2">>),
    EditResult = <<"{\"action\":\"message_edit_ack\",\"content\":\"edited content v2\"}">>,
    %% 1) 编辑受理（新 request；repo 层动作枚举 = 'edit'）→ 幂等重放同参 = duplicate
    ?assertEqual(
        {ok, new},
        c2c_action_receipt_repo:accept(
            EditReq, Original, 101, 202, <<"edit">>, EditHash
        )
    ),
    ?assertEqual(
        {ok, duplicate},
        c2c_action_receipt_repo:accept(
            EditReq, Original, 101, 202, <<"edit">>, EditHash
        )
    ),
    %% 2) 同 request 异内容 = 拒绝（真实性校验，不静默覆盖）
    ?assertMatch(
        {error, _},
        c2c_action_receipt_repo:accept(
            EditReq, Original, 101, 202, <<"edit">>, hash(<<"forged">>)
        )
    ),
    %% 3) 同 request 异 action = 同样拒绝（request 绑定动作语义）
    ?assertMatch(
        {error, _},
        c2c_action_receipt_repo:accept(EditReq, Original, 101, 202, <<"revoke">>, EditHash)
    ),
    %% 4) consume：编辑生效（此刻仍是受理序最新——BE-04 受理序权威）→
    %%    重复 consume = replayed（不重复业务效果）
    ?assertEqual(
        ok,
        c2c_action_receipt_repo:consume(
            EditReq, Original, 202, <<"edit">>, EditHash, EditResult
        )
    ),
    ?assertEqual(
        {error, replayed},
        c2c_action_receipt_repo:consume(
            EditReq, Original, 202, <<"edit">>, EditHash, EditResult
        )
    ),
    %% 5) 撤回后到受理（异 request 同 original）：advisory lock 串行下正常受理；
    %%    若编辑 ACK 在撤回受理之后才消费，将被受理序 fence 显式拒绝
    %%    （{error, stale}，见 edit_newest_accepted_is_authoritative）
    ?assertEqual(
        {ok, new}, c2c_action_receipt_repo:accept(RevokeReq, Original, 101, 202, <<"revoke">>, null)
    ),
    %% 6) 撤回 consume 生效（权威终态）；编辑与撤回互不覆盖对方收据行（独立 request）
    ?assertEqual(
        ok,
        c2c_action_receipt_repo:consume(
            RevokeReq, Original, 202, <<"revoke">>, null, <<"{\"action\":\"revoke_ack\"}">>
        )
    ),
    {ok, Rows} = elib_pg:query(
        <<
            "SELECT request_id, action, consumed_at FROM public.c2c_action_receipt "
            "WHERE original_msg_id=$1 ORDER BY request_id"
        >>,
        [Original]
    ),
    ?assertEqual(2, length(Rows)),
    %% 7) 断线恢复语义：收据与消费状态全在库——新连接按库态可判 replayed
    ?assertEqual(
        {error, replayed},
        c2c_action_receipt_repo:consume(
            RevokeReq, Original, 202, <<"revoke">>, null, <<"{\"action\":\"revoke_ack\"}">>
        )
    ),
    ok.

%% M-01（IMBOY_MASTER_V2_2_20261118）：000280 c2c_action_version 迁移
%% 兼容断言（真库，marker 已含全链至 280）。
action_version_migration_compat() ->
    %% 列在位（NOT NULL DEFAULT 0）
    {ok, Cols} = elib_pg:query(
        <<
            "SELECT column_name, is_nullable, column_default FROM information_schema.columns "
            "WHERE table_schema='public' AND table_name='msg_c2c' AND column_name='action_version'"
        >>,
        []
    ),
    ?assertEqual(1, length(Cols)),
    %% 旧式 INSERT（不带 action_version）成功 = 旧端兼容写；读回默认 0
    LegacyId = <<"m01-legacy-row">>,
    insert_formal(LegacyId, <<"legacy payload">>),
    {ok, [#{<<"action_version">> := Ver}]} = elib_pg:query(
        <<"SELECT action_version FROM public.msg_c2c WHERE msg_id=$1">>, [LegacyId]
    ),
    ?assertEqual(0, Ver),
    %% down：列移除（与 000280 down 文件语义逐字等价的真回退），旧式读写仍通
    {ok, _} =
        elib_pg:execute(
            <<"ALTER TABLE public.msg_c2c DROP COLUMN IF EXISTS action_version">>, []
        ),
    {ok, []} = elib_pg:query(
        <<
            "SELECT 1 FROM information_schema.columns "
            "WHERE table_schema='public' AND table_name='msg_c2c' AND column_name='action_version'"
        >>,
        []
    ),
    LegacyId2 = <<"m01-legacy-after-down">>,
    insert_formal(LegacyId2, <<"post-down payload">>),
    {ok, [#{<<"payload">> := <<"post-down payload">>}]} = elib_pg:query(
        <<"SELECT payload FROM public.msg_c2c WHERE msg_id=$1">>, [LegacyId2]
    ),
    %% 再 up：列恢复（与 000280 up 文件语义逐字等价），存量行默认 0
    {ok, _} =
        elib_pg:execute(
            <<"ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS action_version bigint NOT NULL DEFAULT 0">>,
            []
        ),
    {ok, [#{<<"action_version">> := Ver2}]} = elib_pg:query(
        <<"SELECT action_version FROM public.msg_c2c WHERE msg_id=$1">>, [LegacyId2]
    ),
    ?assertEqual(0, Ver2),
    ok.

%% ---------------------------------------------------------------- BE-04 断言

%% BE-04:LOCAL:01 两次编辑反序、并发编辑、旧设备重放：受理序 = 权威序。
%% 只有受理序最新的编辑 ACK 可消费生效；过期 ACK 显式 {error, stale}，
%% 无静默覆盖；同请求重放 = replayed（幂等）。
edit_newest_accepted_is_authoritative() ->
    Original = <<"be04-order-original">>,
    insert_formal(Original, <<"v0">>),
    E1 = <<"be04-edit-e1">>,
    E2 = <<"be04-edit-e2">>,
    H1 = hash(<<"v1">>),
    H2 = hash(<<"v2">>),
    R1 = <<"{\"action\":\"message_edit_ack\",\"content\":\"v1\"}">>,
    R2 = <<"{\"action\":\"message_edit_ack\",\"content\":\"v2\"}">>,
    ?assertEqual(
        {ok, new}, c2c_action_receipt_repo:accept(E1, Original, 101, 202, <<"edit">>, H1)
    ),
    ?assertEqual(
        {ok, new}, c2c_action_receipt_repo:accept(E2, Original, 101, 202, <<"edit">>, H2)
    ),
    %% 反序：受理序最新（E2）先消费生效
    ?assertEqual(
        ok, c2c_action_receipt_repo:consume(E2, Original, 202, <<"edit">>, H2, R2)
    ),
    %% 先受理（E1）的迟到 ACK 被显式拒绝，权威最新状态保留
    ?assertEqual(
        {error, stale}, c2c_action_receipt_repo:consume(E1, Original, 202, <<"edit">>, H1, R1)
    ),
    ?assertEqual(R2, formal_payload(Original)),
    ?assertEqual(1, action_version_of(Original)),
    %% 旧设备重放：同请求重放 = replayed；被拒 ACK 零业务效果（版本不再递增）
    ?assertEqual(
        {error, replayed}, c2c_action_receipt_repo:consume(E2, Original, 202, <<"edit">>, H2, R2)
    ),
    ?assertEqual(1, action_version_of(Original)),
    %% 并发编辑：两个不同请求的 ACK 同时消费——per-original advisory lock
    %% 串行下终态恒为受理序最新（C2），恰一 ok 一 stale，与锁序无关。
    Original2 = <<"be04-race-original">>,
    insert_formal(Original2, <<"c0">>),
    C1 = <<"be04-race-e1">>,
    C2 = <<"be04-race-e2">>,
    H1b = hash(<<"c1">>),
    H2b = hash(<<"c2">>),
    R1b = <<"{\"action\":\"message_edit_ack\",\"content\":\"c1\"}">>,
    R2b = <<"{\"action\":\"message_edit_ack\",\"content\":\"c2\"}">>,
    ?assertEqual(
        {ok, new}, c2c_action_receipt_repo:accept(C1, Original2, 101, 202, <<"edit">>, H1b)
    ),
    ?assertEqual(
        {ok, new}, c2c_action_receipt_repo:accept(C2, Original2, 101, 202, <<"edit">>, H2b)
    ),
    Parent = self(),
    Run = fun(Req, H, Res) ->
        spawn(fun() ->
            Parent !
                {be04_consume,
                    c2c_action_receipt_repo:consume(Req, Original2, 202, <<"edit">>, H, Res)}
        end)
    end,
    Run(C1, H1b, R1b),
    Run(C2, H2b, R2b),
    %% Erlang 项序：atom(ok) < tuple({error,_})，排序后恒为 [ok, {error, stale}]
    Results = lists:sort([receive_be04_consume(), receive_be04_consume()]),
    ?assertEqual([ok, {error, stale}], Results),
    ?assertEqual(R2b, formal_payload(Original2)),
    ?assertEqual(1, action_version_of(Original2)),
    ok.

%% BE-04:LOCAL:02 撤回先到、编辑后到：正文不复活，控制事实可恢复。
%% 场景 A：撤回先消费成终态 → 后到编辑在 accept 层被显式拒绝；
%% 场景 B：编辑先受理、撤回后受理并消费 → 迟到编辑 ACK 在 consume 层被
%% 终态守卫拒绝（先于受理序 fence，理由精确为 revoked）。
revoke_terminal_state_never_revives() ->
    %% 场景 A
    Original = <<"be04-revfirst-original">>,
    insert_formal(Original, <<"secret-body">>),
    RevReq = <<"be04-revfirst-revoke">>,
    RevResult =
        <<"{\"action\":\"message_revoke_ack\",\"original_msg_id\":\"be04-revfirst-original\"}">>,
    ?assertEqual(
        {ok, new}, c2c_action_receipt_repo:accept(RevReq, Original, 101, 202, <<"revoke">>, null)
    ),
    ?assertEqual(
        ok, c2c_action_receipt_repo:consume(RevReq, Original, 202, <<"revoke">>, null, RevResult)
    ),
    ?assertEqual(
        {error, revoked},
        c2c_action_receipt_repo:accept(
            <<"be04-revfirst-edit">>, Original, 101, 202, <<"edit">>, hash(<<"late">>)
        )
    ),
    ?assertEqual(RevResult, formal_payload(Original)),
    ?assertEqual(1, action_version_of(Original)),
    %% 场景 B
    Original2 = <<"be04-lateedit-original">>,
    insert_formal(Original2, <<"late-body">>),
    EditReq = <<"be04-lateedit-edit">>,
    RevokeReq = <<"be04-lateedit-revoke">>,
    EditHash = hash(<<"late-body">>),
    ?assertEqual(
        {ok, new},
        c2c_action_receipt_repo:accept(EditReq, Original2, 101, 202, <<"edit">>, EditHash)
    ),
    ?assertEqual(
        {ok, new},
        c2c_action_receipt_repo:accept(RevokeReq, Original2, 101, 202, <<"revoke">>, null)
    ),
    RevResult2 =
        <<"{\"action\":\"message_revoke_ack\",\"original_msg_id\":\"be04-lateedit-original\"}">>,
    ?assertEqual(
        ok,
        c2c_action_receipt_repo:consume(
            RevokeReq, Original2, 202, <<"revoke">>, null, RevResult2
        )
    ),
    ?assertEqual(
        {error, revoked},
        c2c_action_receipt_repo:consume(
            EditReq,
            Original2,
            202,
            <<"edit">>,
            EditHash,
            <<"{\"action\":\"message_edit_ack\",\"content\":\"late-body\"}">>
        )
    ),
    %% 终态保持：正文 = 撤回 tombstone，编辑未复活
    ?assertEqual(RevResult2, formal_payload(Original2)),
    ?assertEqual(1, action_version_of(Original2)),
    %% 控制事实可恢复：receipt 行在库可查（撤回已消费带 result_payload，编辑未消费）
    {ok, [EditRow, RevRow]} = elib_pg:query(
        <<
            "SELECT request_id, action, consumed_at, result_payload "
            "FROM public.c2c_action_receipt WHERE original_msg_id=$2 AND request_id IN ($1,$3) "
            "ORDER BY request_id"
        >>,
        [EditReq, Original2, RevokeReq]
    ),
    ?assertEqual(RevokeReq, maps:get(<<"request_id">>, RevRow)),
    ?assert(maps:get(<<"consumed_at">>, RevRow) =/= null),
    ?assertEqual(EditReq, maps:get(<<"request_id">>, EditRow)),
    ?assertEqual(null, maps:get(<<"consumed_at">>, EditRow)),
    ok.

%% BE-04:LOCAL:03 客户端时间被修改、时间窗边界：服务端可信规则生效。
%% 权威基准 = accepted_at（trusted）；伪造 created_at 不影响基准；
%% 旧行 accepted_at NULL 走低可信回退；全部缺失 fail-closed；
%% 边界语义：差值 == 窗口仍在窗内，> 窗口才过期。
server_time_authority_and_window_boundary() ->
    Original = <<"be04-time-original">>,
    insert_formal(Original, <<"t0">>),
    %% 可信基准显式写入：insert_formal 不带 accepted_at（DDL 无默认，旧行
    %% NULL 即 R04 低可信形态，下面单独验证 low_trust/no_time_source）
    TrustedBase = elib_dt:millisecond() - 50000,
    {ok, 1} =
        elib_pg:execute_count(
            <<"UPDATE public.msg_c2c SET accepted_at=$1::text::timestamptz WHERE msg_id=$2">>,
            [elib_dt:to_rfc3339(TrustedBase, millisecond), Original]
        ),
    %% 生产读取形状（find_msg_by_id 列集合）直接作为判定输入
    {ok, Row0} = msg_c2c_ds:find_msg_by_id(Original),
    {ok, BaseMs, trusted} = msg_store_repo:resolve_accepted_ms(Row0),
    %% 客户端时间被修改：created_at 伪造为另一时刻（created_at 是 hypertable
    %% 维度，跨 chunk 伪造会被 chunk 约束拒绝——取同 chunk 内与 accepted_at
    %% 差 150s 的值），accepted_at 权威基准不受任何影响
    Forged = elib_dt:to_rfc3339(TrustedBase - 150000, millisecond),
    {ok, 1} =
        elib_pg:execute_count(
            <<"UPDATE public.msg_c2c SET created_at=$1::text::timestamptz WHERE msg_id=$2">>,
            [Forged, Original]
        ),
    {ok, Row1} = msg_c2c_ds:find_msg_by_id(Original),
    ?assertEqual({ok, BaseMs, trusted}, msg_store_repo:resolve_accepted_ms(Row1)),
    %% 旧行形态：accepted_at 缺失 → 低可信回退（low_trust）。
    %% 现 DDL accepted_at NOT NULL（hypertable chunk 约束），真行无法置 NULL；
    %% 以 find_msg_by_id 真实行键形状构造缺失 accepted_at 的判定输入。
    LegacyShape = Row1#{<<"accepted_at">> => null},
    ?assertMatch({ok, _, low_trust}, msg_store_repo:resolve_accepted_ms(LegacyShape)),
    %% 全部时间源缺失 → fail-closed（纯判定，显式构造缺失行形状）
    NoSource = #{<<"accepted_at">> => null, <<"server_ts">> => null, <<"created_at">> => null},
    ?assertEqual({error, no_time_source}, msg_store_repo:resolve_accepted_ms(NoSource)),
    ?assertMatch({no_time_source, _}, msg_c2c_logic:trusted_window_decision(NoSource, 100000)),
    %% 时间窗边界（服务端时钟 vs 可信基准；== 窗口在窗内，> 窗口过期）
    Win = 100000,
    NowMs = elib_dt:millisecond(),
    ?assertEqual(ok, msg_c2c_logic:trusted_window_decision(time_row(NowMs - Win), Win, NowMs)),
    ?assertMatch(
        {expired, _}, msg_c2c_logic:trusted_window_decision(time_row(NowMs - Win - 1), Win, NowMs)
    ),
    ?assertEqual(ok, msg_c2c_logic:trusted_window_decision(time_row(NowMs - Win + 1), Win, NowMs)),
    ok.

%% BE-04:LOCAL:04 撤回/个人删除/自毁后历史重拉：不重新显示被限制内容，
%% 不误删其他人的内容（真库、真实离线撤回链与删除/自毁 repo 语义）。
terminal_states_survive_history_repull() ->
    %% 个人删除：仅删 to_id 匹配的行，不误删其他人的内容
    %% （先落普通行：正式链 execute_formal_tx 的 conv_seq 分配量级远大于
    %% unique_integer，同 pair 后插小序会触发 c2c_order_conflict）
    M1 = <<"be04-del-m1">>,
    M2 = <<"be04-del-m2">>,
    insert_pair(M1, 101, 202, <<"to-r1">>),
    insert_pair(M2, 101, 203, <<"to-r2">>),
    {ok, 1} = msg_c2c_ds:delete_by_msg_ids_and_to_id([M1], 202),
    {ok, []} = elib_pg:query(<<"SELECT 1 FROM public.msg_c2c WHERE msg_id=$1">>, [M1]),
    {ok, [_]} = elib_pg:query(<<"SELECT 1 FROM public.msg_c2c WHERE msg_id=$1">>, [M2]),
    %% 自毁：仅到期行被清理，未到期行保留
    Past = elib_dt:to_rfc3339(elib_dt:millisecond() - 60000, millisecond),
    {ok, 1} = msg_c2c_ds:set_expire_at(M2, Past),
    Deleted = msg_c2c_repo:delete_expired(100),
    ?assert(is_integer(Deleted)),
    ?assert(Deleted >= 1, {no_expired_deleted, Deleted}),
    {ok, []} = elib_pg:query(<<"SELECT 1 FROM public.msg_c2c WHERE msg_id=$1">>, [M2]),
    %% 撤回（对端离线真实链 /9）：原文行被 tombstone 覆盖，版本随写递增
    Original = <<"be04-tomb-original">>,
    insert_formal(Original, <<"tomb-body">>),
    NotifyId = <<"be04-tomb-notify">>,
    RevPayload = #{
        <<"content">> => <<>>,
        <<"original_msg_id">> => Original,
        <<"revoked_at">> => elib_dt:millisecond()
    },
    ok = msg_c2c_ds:revoke_offline_msg(
        RevPayload,
        elib_dt:now(),
        NotifyId,
        Original,
        101,
        202,
        <<"custom">>,
        <<"message_revoke_ack">>,
        null
    ),
    TombMap = jsone:decode(formal_payload(Original)),
    ?assertEqual(<<"message_revoke_ack">>, maps:get(<<"action">>, TombMap)),
    ?assertEqual(Original, maps:get(<<"original_msg_id">>, TombMap)),
    ?assertEqual(
        nomatch, binary:match(formal_payload(Original), <<"tomb-body">>)
    ),
    ?assertEqual(1, action_version_of(Original)),
    %% 历史重拉读取路径（find_msg_by_id）：只见控制事实，不见原文
    {ok, HistRow} = msg_c2c_ds:find_msg_by_id(Original),
    HistPayload = maps:get(<<"payload">>, HistRow),
    ?assertEqual(nomatch, binary:match(HistPayload, <<"tomb-body">>)),
    ok.

%% ---------------------------------------------------------------- BE-04 helpers

action_version_of(MsgId) ->
    scalar(<<"SELECT action_version FROM public.msg_c2c WHERE msg_id=$1">>, [MsgId]).

%% 自定义收发双方的正式行（个人删除/自毁断言需要不同 to_id）
insert_pair(MsgId, FromId, ToId, Payload) ->
    {ok, 1} = elib_pg:execute_count(
        <<"INSERT INTO public.msg_c2c ", "(id,msg_id,from_id,to_id,payload,msg_type,conv_seq) ",
            "VALUES($1,$2,$3,$4,$5,'text',$6)">>,
        [
            erlang:unique_integer([positive]),
            MsgId,
            FromId,
            ToId,
            Payload,
            erlang:unique_integer([positive, monotonic])
        ]
    ),
    ok.

%% 时间窗边界判定输入：仅 accepted_at 可信源（其余显式缺失）
time_row(AcceptedMs) ->
    #{
        <<"accepted_at">> => elib_dt:to_rfc3339(AcceptedMs, millisecond),
        <<"server_ts">> => null,
        <<"created_at">> => null
    }.

receive_be04_consume() ->
    receive
        {be04_consume, Result} -> Result
    after 10000 ->
        error(be04_consume_timeout)
    end.
