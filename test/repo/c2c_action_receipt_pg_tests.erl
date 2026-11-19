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
                ?_test(action_version_migration_compat())
            ]
        end}}.

setup() ->
    application:load(imboy),
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, msg_archive_enabled, false),
    %% 本套件不经 eunit_runner boot，直调 msg_store_worker:process_row 会走到
    %% elib_tsid:generate(msg_c2c)；TSID runtime 未初始化时写入报
    %% elib_tsid_not_initialized。按仓库测试惯例幂等初始化（已初始化则保留）。
    _ = (catch elib_tsid:init(#{dc_id => 0, node_id => 0, dc_bits => 3})),
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
    %% 4) 撤回后到（异 request 同 original）：advisory lock 串行下正常受理
    ?assertEqual(
        {ok, new}, c2c_action_receipt_repo:accept(RevokeReq, Original, 101, 202, <<"revoke">>, null)
    ),
    %% 5) consume：编辑生效 → 重复 consume = replayed（不重复业务效果）
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
    %% 6) 撤回 consume 生效；编辑与撤回互不覆盖对方收据行（独立 request）
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
