-module(attach_direct_quota_gc_pg_tests).
-moduledoc "direct-v1 配额/GC/生命周期互斥（step-7 / T15/T20/T46 SQL 行为）".
%%%
% 真实 PG：
%   * winner 与 GC claim 同锁互斥（先到先得，后者 fail-closed）；
%   * cancel 捕获全部未 WINNER attempt 的 staging+candidate 物理身份；
%   * 每 subject 并发 live 会话 ≤4；
%   * cleanup claim/ack：龄期预算 + lease 互斥 + done。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(UID, 990702).

quota_gc_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun gc_claim_after_winner_rejected/0,
        fun gc_claim_then_winner_rejected/0,
        fun cancel_captures_physical_identities/0,
        fun session_quota_fail_closed/0,
        fun cleanup_claim_ack_flow/0
    ]}.

setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Booted = eunit_runner:eunit_setup(),
    State = #{booted => Booted, before_env => Before},
    try
        %% 自净仅限无守卫的 live 会话表：迁移 270 跨域守卫（23510 physical
        %% tombstone permanent）令 attach_direct_cleanup / attachment 行不可
        %% DELETE——清理账残留由 cleanup_claim_ack_flow 的命名空间断言 +
        %% 龄期排序吸收。
        elib_pg:query(<<"DELETE FROM attach_pending WHERE creator_user_id = $1">>, [?UID]),
        garage_apply(),
        State
    catch
        Class:Reason:Stack ->
            cleanup(State),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{booted := Booted, before_env := Before}) ->
    try
        %% 迁移 270 跨域守卫（23510 physical tombstone permanent）令
        %% attach_direct_cleanup / attachment 行不可 DELETE——DELETE 被
        %% elib_pg 收敛为 {error,_}，只清无守卫的 attach_pending。
        elib_pg:query(<<"DELETE FROM attach_pending WHERE creator_user_id = $1">>, [?UID]),
        eunit_runner:eunit_cleanup(Booted)
    after
        [
            case Value of
                undefined -> application:unset_env(imboy, K);
                {ok, V} -> application:set_env(imboy, K, V)
            end
         || {K, Value} <- Before
        ]
    end.

garage_apply() ->
    EP = <<"http://127.0.0.1:39999">>,
    Bucket = <<"dv1-test-bucket">>,
    ok = application:set_env(imboy, garage, #{
        endpoint => EP,
        bucket => Bucket,
        access_key => <<"test-ak">>,
        secret_key => <<"test-sk">>
    }),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    Uploads = maps:get(upload_namespaces, Registry, #{}),
    Namespace = <<"dv1gc-", (binary:encode_hex(crypto:strong_rand_bytes(16), lowercase))/binary>>,
    Record = #{
        driver => <<"garage">>,
        namespace => Namespace,
        endpoint => EP,
        container => Bucket
    },
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots => Roots#{Namespace => Record},
        upload_namespaces => Uploads#{<<"private">> => Namespace}
    }).

%% ---------------------------------------------------------------- helpers

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).

new_session(Sha) ->
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.bin">>
    ),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"application/octet-stream">>),
    {ok, #{attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    {Key, No}.

%% attempt 手动推进到 PROOF_VERIFIED（不经 finalize/confirm）
bring_to_proof_verified(Key, No, ProofSha) ->
    %% winner_pin 要求会话处于 FINALIZING（finalize 语义：GRANTED→FINALIZING）
    elib_pg:query(
        <<
            "UPDATE attach_pending SET direct_state='FINALIZING'"
            " WHERE object_key=$1 AND direct_state='GRANTED'"
        >>,
        [Key]
    ),
    elib_pg:with_tx(fun(Conn) ->
        ok = attach_direct_attempt_repo:mark_dispatched_tx(Conn, Key, No),
        ok = attach_direct_attempt_repo:record_outcome_tx(Conn, Key, No, copy_completed, undefined),
        attach_direct_attempt_repo:record_proof_tx(Conn, Key, No, ProofSha, 10)
    end).

cancel(Key) ->
    {ok, [_]} = elib_pg:query(<<"SELECT fn_dv1_cancel($1,$2)">>, [Key, <<"test-cancel">>]),
    ok.

attempt_target(Key, No) ->
    {ok, [#{<<"target_key">> := T}]} = elib_pg:query(
        <<"SELECT target_key FROM attach_direct_attempt WHERE object_key=$1 AND attempt_no=$2">>,
        [Key, No]
    ),
    T.

%% ---------------------------------------------------------------- cases

gc_claim_after_winner_rejected() ->
    {ok, #{object_key := Key}} =
        attach_direct_logic:register(?UID, <<"private">>, undefined, undefined, <<"a.bin">>),
    ok = attach_direct_logic:declare(Key, hex64(16#A1), 10, <<"application/octet-stream">>),
    {ok, #{attempt_id := No}} = attach_direct_logic:grant(Key, hex64(16#A1)),
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        copy_dispatch,
        4,
        fun(_N, _C, _S, _T) -> {ok, #{}} end
    ),
    ok = meck:expect(
        attachment_storage_garage,
        head_bounded,
        3,
        fun(_N, _C, _T) -> {ok, #{size => 10}} end
    ),
    {ok, #{state := <<"CONFIRMED">>}} = attach_direct_logic:finalize(Key, No),
    meck:unload(attachment_storage_garage),
    %% winner 已定（attempt 1 = WINNER）：GC claim 后到必须被状态门拒绝
    ?assertEqual(
        {ok, [#{<<"fn_dv1_gc_claim">> => <<"not_claimable">>}]},
        elib_pg:query(<<"SELECT fn_dv1_gc_claim($1,$2,$3)">>, [Key, No, <<"late-gc">>])
    ),
    ok.

gc_claim_then_winner_rejected() ->
    {Key, No1} = new_session(hex64(16#B1)),
    %% C08 起期望值在 declare 冻结：renew 必须同 sha（幂等同值，异值
    %% expected_conflict）。proof sha（B3）与 expected 是两回事，保持独立。
    {ok, #{attempt_id := No2}} = attach_direct_logic:renew_grant(Key, hex64(16#B1)),
    bring_to_proof_verified(Key, No2, hex64(16#B3)),
    %% GC claim 先提交
    ?assertEqual(
        {ok, [#{<<"fn_dv1_gc_claim">> => <<"claimed">>}]},
        elib_pg:query(<<"SELECT fn_dv1_gc_claim($1,$2,$3)">>, [Key, No2, <<"gc-first">>])
    ),
    ?assertEqual(
        {ok, [#{<<"attempt_state">> => <<"GC_CLAIMED">>}]},
        elib_pg:query(
            <<
                "SELECT attempt_state FROM attach_direct_attempt"
                " WHERE object_key=$1 AND attempt_no=$2"
            >>,
            [Key, No2]
        )
    ),
    %% winner 后到：PROOF_VERIFIED 前置不满足 → winner_conflict fail-closed
    %% attempt_target/2 走 elib_pg:query/2（隐式取连接），必须留在 with_tx 外求值：
    %% with_tx 作用域内隐式取第二连接触发 nested_connection_unsupported 守卫
    %% （elib_pg:with_conn/1 + in_pg_tx_scope）。target_key 是 attempt 建行时
    %% 冻结的静态列，事务外读取与事务内读取等价，winner_pin_tx 收到的值不变。
    TargetKey = attempt_target(Key, No2),
    ?assertEqual(
        {error, winner_conflict},
        elib_pg:with_tx(fun(Conn) ->
            attach_direct_attempt_repo:winner_pin_tx(
                Conn,
                Key,
                No2,
                TargetKey,
                hex64(16#B3)
            )
        end)
    ),
    %% claim 入清理账（unique physical_key）
    {ok, Rows} = elib_pg:query(
        <<"SELECT physical_key FROM attach_direct_cleanup WHERE object_key=$1">>, [Key]
    ),
    ?assertEqual([attempt_target(Key, No2)], [maps:get(<<"physical_key">>, R) || R <- Rows]),
    _ = No1,
    ok.

cancel_captures_physical_identities() ->
    {Key, No} = new_session(hex64(16#C1)),
    %% 同上：expected 冻结后 renew 必须与 declare 同 sha。
    {ok, #{attempt_id := No2}} = attach_direct_logic:renew_grant(Key, hex64(16#C1)),
    ok = cancel(Key),
    %% staging+candidate ×2 attempts 全部入账（去重后）
    {ok, Rows} = elib_pg:query(
        <<"SELECT physical_key FROM attach_direct_cleanup WHERE object_key=$1 ORDER BY physical_key">>,
        [Key]
    ),
    ?assertEqual(4, length(Rows)),
    {ok, [Att]} = elib_pg:query(
        <<"SELECT direct_state, session_state FROM attach_pending WHERE object_key=$1">>, [Key]
    ),
    ?assertEqual(<<"CANCELLED">>, maps:get(<<"direct_state">>, Att)),
    ?assertEqual(<<"rejected">>, maps:get(<<"session_state">>, Att)),
    %% 两个 attempt 都存在
    ?assertEqual(2, No + No2 - No),
    ok.

session_quota_fail_closed() ->
    Sha = hex64(16#D1),
    Keys = [
        begin
            {ok, #{object_key := K}} = attach_direct_logic:register(
                ?UID,
                <<"private">>,
                undefined,
                undefined,
                <<"a.bin">>
            ),
            ok = attach_direct_logic:declare(K, Sha, 10, <<"application/octet-stream">>),
            K
        end
     || _ <- lists:seq(1, 4)
    ],
    ?assertEqual(4, length(Keys)),
    ?assertEqual(
        {error, quota_exceeded},
        attach_direct_logic:register(?UID, <<"private">>, undefined, undefined, <<"a.bin">>)
    ),
    ok.

cleanup_claim_ack_flow() ->
    {Key, _No} = new_session(hex64(16#E1)),
    ok = cancel(Key),
    %% 270 跨域守卫（23510 physical tombstone permanent）令清理账行不可
    %% DELETE：中断运行留下的 ready 残留常驻全局队列（claim 按 created_at
    %% 取最老一行）。龄期预算断言因此收敛到本 run 命名空间——本 run 新行
    %% created_at 最新，不被立即领走。
    {ok, [#{<<"target_namespace">> := OwnNs}]} =
        elib_pg:query(
            <<
                "SELECT target_namespace FROM attach_direct_cleanup"
                " WHERE object_key=$1 ORDER BY created_at DESC LIMIT 1"
            >>,
            [Key]
        ),
    {ok, Claimed0} = elib_pg:query(<<"SELECT * FROM fn_dv1_cleanup_claim($1)">>, [
        elib_uuid:gen_v7()
    ]),
    ?assert(
        lists:all(
            fun(R) -> maps:get(<<"out_namespace">>, R) =/= OwnNs end,
            Claimed0
        )
    ),
    %% 龄期预算：本 run 行置为一年前（immutable 守卫允许非身份列 UPDATE），
    %% 循环 claim 直到领到本 run 行——先前 run 同样 1y 龄期的行 created_at
    %% 更早会先被领走，每次被领都 +1 attempts（<8 才可领），有限步内退役。
    {ok, _} = elib_pg:query(
        <<
            "UPDATE attach_direct_cleanup SET created_at = now() - interval '1 year'"
            " WHERE object_key=$1 RETURNING 1"
        >>,
        [Key]
    ),
    {T1, Id} = claim_until_own(OwnNs, 256),
    %% lease 互斥语义：同一行不会被再次发出（其余 aged 行可被后续 claim 领走）
    {ok, All2} = elib_pg:query(<<"SELECT * FROM fn_dv1_cleanup_claim($1)">>, [elib_uuid:gen_v7()]),
    ?assert(lists:all(fun(R) -> maps:get(<<"out_id">>, R) =/= Id end, All2)),
    {ok, [#{<<"job_state">> := <<"leased">>, <<"lease_token">> := T1}]} =
        elib_pg:query(<<"SELECT job_state, lease_token FROM attach_direct_cleanup WHERE id=$1">>, [
            Id
        ]),
    %% ack done
    ?assertEqual(
        {ok, [#{<<"fn_dv1_cleanup_ack">> => <<"done">>}]},
        elib_pg:query(<<"SELECT fn_dv1_cleanup_ack($1,$2,true)">>, [Id, T1])
    ),
    ok.

%% @doc 循环 claim 直到领到本 run 命名空间的行；返回 {Token, Id}。
claim_until_own(_OwnNs, 0) ->
    erlang:error(claim_own_row_not_reached);
claim_until_own(OwnNs, N) ->
    Token = elib_uuid:gen_v7(),
    case elib_pg:query(<<"SELECT * FROM fn_dv1_cleanup_claim($1)">>, [Token]) of
        {ok, [#{<<"out_id">> := Id, <<"out_namespace">> := OwnNs}]} ->
            {Token, Id};
        {ok, _} ->
            claim_until_own(OwnNs, N - 1);
        {error, Reason} ->
            erlang:error({cleanup_claim_failed, Reason})
    end.
