-module(attach_direct_logic_pg_tests).
-moduledoc "direct-v1 会话状态机 PG 测试（step-6 / T01/T02/T03/T17 相关 SQL 行为）".
%%%
% 真实 PG + meck 存储控制面（copy_dispatch/head_bounded）；A0/5a 真桶
% 行为不在本套件（BLOCKED 记录见 state）。覆盖：
%   * happy path：RESERVED→DECLARED→GRANTED→FINALIZING→CONFIRMED，物理键映射；
%   * unknown 无晋升边 + 同 attempt 不重发 + 新 attempt 重试成功；
%   * attempt 预算 ≤3 fail-closed；FAILED 收敛；
%   * declare 幂等/冲突（T02/T03）；跨主体 status 不泄露；size mismatch 拒绝。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(UID, 990701).

direct_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun happy_path_confirm_physical_key/0,
        fun unknown_no_promotion_and_no_redispatch/0,
        fun retry_new_attempt_succeeds/0,
        fun attempt_budget_fail_closed/0,
        fun declare_idempotent_and_conflict/0,
        fun cross_subject_status_hidden/0,
        fun size_mismatch_rejected/0,
        fun register_idempotent_by_client_upload_id/0
    ]}.

setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Booted = eunit_runner:eunit_setup(),
    State = #{booted => Booted, before_env => Before},
    try
        garage_apply(),
        State
    catch
        Class:Reason:Stack ->
            cleanup(State),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{booted := Booted, before_env := Before}) ->
    try
        %% 本套件自清理（只删本任务测试创建的行：固定 uid 前缀 + direct 键形）
        elib_pg:query(
            <<
                "DELETE FROM attach_pending WHERE creator_user_id = $1"
                " AND (direct_state IS NOT NULL OR object_key LIKE 'u990701/%')"
            >>,
            [?UID]
        ),
        elib_pg:query(<<"DELETE FROM attachment WHERE creator_user_id = $1">>, [?UID]),
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

%% 假 garage 端点 + 独立 registry namespace（不触网；控制面调用被 meck）
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
    Namespace = <<"dv1-", (binary:encode_hex(crypto:strong_rand_bytes(16), lowercase))/binary>>,
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

with_storage_mock(CopyFun, HeadFun, Inner) ->
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(attachment_storage_garage, copy_dispatch, 4, CopyFun),
    ok = meck:expect(attachment_storage_garage, head_bounded, 3, HeadFun),
    try
        Inner()
    after
        meck:unload(attachment_storage_garage)
    end.

session_attempt_state(Key, No) ->
    {ok, Rows} = attach_direct_attempt_repo:list_for_session(Key),
    [A] = [R || R <- Rows, ec_cnv:to_integer(maps:get(<<"attempt_no">>, R)) =:= No],
    maps:get(<<"attempt_state">>, A).

attempt_target(Key, No) ->
    {ok, Rows} = attach_direct_attempt_repo:list_for_session(Key),
    [A] = [
        R
     || R <- Rows,
        ec_cnv:to_integer(maps:get(<<"attempt_no">>, R)) =:= No
    ],
    maps:get(<<"target_key">>, A).

pending_row(Key) ->
    {ok, [Row]} = elib_pg:query(
        <<
            "SELECT direct_state, session_state, confirmed_attachment_id, storage_locator_sha256"
            " FROM attach_pending WHERE object_key=$1"
        >>,
        [Key]
    ),
    Row.

attachment_row(Key) ->
    case
        elib_pg:query(
            <<
                "SELECT path, storage_physical_key, storage_driver, file_hash256, size"
                " FROM attachment WHERE path=$1"
            >>,
            [Key]
        )
    of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found}
    end.

%% ---------------------------------------------------------------- cases

happy_path_confirm_physical_key() ->
    Sha = hex64(16#AA),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.png">>
    ),
    ?assertEqual(<<"RESERVED">>, maps:get(<<"direct_state">>, pending_row(Key))),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"image/png">>),
    {ok, #{grant := G, attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    %% 描述符合同（I04/T07）：s3_put、两个 required_headers、仅 PUT
    ?assertEqual(<<"s3_put">>, maps:get(transport, G)),
    ?assertEqual(<<"PUT">>, maps:get(method, G)),
    ?assertEqual(2, length(maps:get(required_headers, G))),
    ?assert(lists:keyfind(<<"x-amz-checksum-sha256">>, 1, maps:get(required_headers, G)) =/= false),
    {ok, Status} = attach_direct_logic:status(Key, ?UID),
    ?assertEqual(<<"GRANTED">>, maps:get(state, Status)),
    Attempts = maps:get(attempts, Status),
    ?assertEqual(1, length(Attempts)),
    ?assertEqual(1, maps:get(attempt_id, hd(Attempts))),
    with_storage_mock(
        fun(_Ns, _C, _S, _T) -> {ok, #{etag => <<"\"e\"">>}} end,
        fun(_Ns, _C, _T) -> {ok, #{size => 10, content_type => <<"image/png">>}} end,
        fun() ->
            ?assertMatch(
                {ok, #{state := <<"CONFIRMED">>}},
                attach_direct_logic:finalize(Key, No)
            )
        end
    ),
    %% 原子映射落库（协议 §6）：物理键=attempt target；pending CONFIRMED
    {ok, Att} = attachment_row(Key),
    ?assertEqual(attempt_target(Key, No), maps:get(<<"storage_physical_key">>, Att)),
    ?assertEqual(Sha, maps:get(<<"file_hash256">>, Att)),
    Row = pending_row(Key),
    ?assertEqual(<<"CONFIRMED">>, maps:get(<<"direct_state">>, Row)),
    ?assertEqual(<<"confirmed">>, maps:get(<<"session_state">>, Row)),
    ok.

declare_size_cap_test() ->
    garage_apply(),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"big.bin">>
    ),
    %% T08：普通上限 104857600+1 在声明点硬拒（无上传残留）
    ?assertEqual(
        {error, size_limit_exceeded},
        attach_direct_logic:declare(
            Key, hex64(16#CC), 104857600 + 1, <<"application/octet-stream">>
        )
    ),
    %% 负数声明同样拒绝
    ?assertEqual(
        {error, size_limit_exceeded},
        attach_direct_logic:declare(Key, hex64(16#CD), -1, <<"application/octet-stream">>)
    ),
    ok.

unknown_no_promotion_and_no_redispatch() ->
    Sha = hex64(16#BB),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.bin">>
    ),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"application/octet-stream">>),
    {ok, #{attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    %% Copy 超时 → UNKNOWN_QUARANTINED（§3.2）
    with_storage_mock(
        fun(_Ns, _C, _S, _T) -> {error, timeout} end,
        fun(_Ns, _C, _T) -> {error, timeout} end,
        fun() ->
            ?assertMatch(
                {ok, #{outcome := <<"UNKNOWN_QUARANTINED">>}},
                attach_direct_logic:finalize(Key, No)
            )
        end
    ),
    ?assertEqual(<<"UNKNOWN_QUARANTINED">>, session_attempt_state(Key, No)),
    %% 同 attempt 重复 finalize：不重发（每 attempt 恰一次写授权）
    with_storage_mock(
        fun(_Ns, _C, _S, _T) -> meck:exception(error, must_not_redispatch) end,
        fun(_Ns, _C, _T) -> {error, timeout} end,
        fun() ->
            ?assertEqual(
                {error, attempt_not_dispatchable},
                attach_direct_logic:finalize(Key, No)
            )
        end
    ),
    ?assertEqual(<<"UNKNOWN_QUARANTINED">>, session_attempt_state(Key, No)),
    ok.

retry_new_attempt_succeeds() ->
    Sha = hex64(16#CC),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.bin">>
    ),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"application/octet-stream">>),
    {ok, #{attempt_id := No1}} = attach_direct_logic:grant(Key, Sha),
    with_storage_mock(
        fun(_Ns, _C, _S, _T) -> {error, timeout} end,
        fun(_Ns, _C, _T) -> {error, timeout} end,
        fun() -> _ = attach_direct_logic:finalize(Key, No1) end
    ),
    %% 新 attempt + 新 target（I05）
    {ok, #{grant := G2, attempt_id := No2}} = attach_direct_logic:renew_grant(Key, Sha),
    ?assertEqual(2, No2),
    ?assertNotEqual(attempt_target(Key, No1), attempt_target(Key, No2)),
    with_storage_mock(
        fun(_Ns, _C, _S, _T) -> {ok, #{}} end,
        fun(_Ns, _C, _T) -> {ok, #{size => 10}} end,
        fun() ->
            ?assertMatch(
                {ok, #{state := <<"CONFIRMED">>}},
                attach_direct_logic:finalize(Key, No2)
            )
        end
    ),
    ?assertEqual(<<"WINNER">>, session_attempt_state(Key, No2)),
    ok.

attempt_budget_fail_closed() ->
    Sha = hex64(16#DD),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.bin">>
    ),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"application/octet-stream">>),
    {ok, #{attempt_id := No1}} = attach_direct_logic:grant(Key, Sha),
    Unknown = {fun(_Ns, _C, _S, _T) -> {error, timeout} end, fun(_Ns, _C, _T) ->
        {error, timeout}
    end},
    with_storage_mock(
        element(1, Unknown),
        element(2, Unknown),
        fun() -> _ = attach_direct_logic:finalize(Key, No1) end
    ),
    {ok, #{attempt_id := No2}} = attach_direct_logic:renew_grant(Key, Sha),
    with_storage_mock(
        element(1, Unknown),
        element(2, Unknown),
        fun() -> _ = attach_direct_logic:finalize(Key, No2) end
    ),
    {ok, #{attempt_id := No3}} = attach_direct_logic:renew_grant(Key, Sha),
    with_storage_mock(
        element(1, Unknown),
        element(2, Unknown),
        fun() -> _ = attach_direct_logic:finalize(Key, No3) end
    ),
    %% 第 4 次：预算耗尽（23514 → 40904 语义）
    ?assertEqual(
        {error, attempt_budget_exhausted},
        attach_direct_logic:renew_grant(Key, Sha)
    ),
    ok.

declare_idempotent_and_conflict() ->
    Sha = hex64(16#EE),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.png">>
    ),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"image/png">>),
    %% 同值幂等（T02）
    ?assertEqual(ok, attach_direct_logic:declare(Key, Sha, 10, <<"image/png">>)),
    %% 异值冲突（T03）
    ?assertEqual(
        {error, expected_conflict},
        attach_direct_logic:declare(Key, hex64(16#EF), 10, <<"image/png">>)
    ),
    ?assertEqual(
        {error, expected_conflict},
        attach_direct_logic:declare(Key, Sha, 11, <<"image/png">>)
    ),
    ok.

cross_subject_status_hidden() ->
    Sha = hex64(16#F0),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.png">>
    ),
    ?assertEqual({error, not_found}, attach_direct_logic:status(Key, ?UID + 1)),
    ?assertMatch({ok, #{object_key := Key}}, attach_direct_logic:status(Key, ?UID)),
    ok.

size_mismatch_rejected() ->
    Sha = hex64(16#F1),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.bin">>
    ),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"application/octet-stream">>),
    {ok, #{attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    with_storage_mock(
        fun(_Ns, _C, _S, _T) -> {ok, #{}} end,
        fun(_Ns, _C, _T) -> {ok, #{size => 11}} end,
        fun() ->
            ?assertEqual(
                {error, {rejected, size_mismatch}},
                attach_direct_logic:finalize(Key, No)
            )
        end
    ),
    ?assertEqual(<<"REJECTED">>, session_attempt_state(Key, No)),
    ?assertEqual({error, not_found}, attachment_row(Key)),
    ok.

register_idempotent_by_client_upload_id() ->
    Cid = elib_uuid:gen_v7(),
    {ok, #{object_key := Key1}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        Cid,
        <<"same.png">>
    ),
    %% 同 id 同声明 → 同会话（T02）
    {ok, #{object_key := Key2}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        Cid,
        <<"same.png">>
    ),
    ?assertEqual(Key1, Key2),
    %% 同 id 异声明 → 冲突（T03）
    ?assertEqual(
        {error, declaration_conflict},
        attach_direct_logic:register(?UID, <<"private">>, undefined, Cid, <<"other.png">>)
    ),
    ok.
