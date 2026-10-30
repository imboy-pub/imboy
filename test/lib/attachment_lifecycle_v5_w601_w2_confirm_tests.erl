%%% W6-01 C 组 confirm 面（58 合同叶）。
%%%
%%% W2-02/confirmed-content-immutable（42）：真实对象字节（非 HEAD/ETag）作
%%%   确认依据——旧能力/在途写/静默覆写都不得改变已确认字节；密文对象只存
%%%   密文 hash；pin 后崩溃按同 receipt 恢复（每叶走 entrypoint 命名的真实
%%%   HTTP 入口）；
%%% W2-02/confirm-idempotent（8）：100 次顺序重放与两路并发 confirm 得同一
%%%   attachment id、metadata 唯一；
%%% W2-02/confirm-old-client（8）：旧请求格式幂等接受同一对象且不新建会话；
%%%   未知 legacy pending 受保护（稳定错误、绝不静默删除）。
%%%
%%% domain→entrypoint 映射（合同叶 entrypoint 逐字）：confirm-idempotent /
%%% confirm-old-client 组的 entrypoint 是组名本身；ordinary 域走
%%% /api/v1/attachment/confirm，enterprise 域走 /api/internal/v1/files/confirm。
-module(attachment_lifecycle_v5_w601_w2_confirm_tests).

-export([specs/0]).
-ifdef(TEST).
-export([imm_scenario/4]).
-endif.

-include_lib("eunit/include/eunit.hrl").

-define(S, attachment_lifecycle_v5_w601_w2_support).
-define(F, attachment_w403_http_fixture).
-define(FW, attachment_w403_http_fixture_w601).
-define(C_IMM, <<"W2-02/confirmed-content-immutable">>).
-define(C_IDEM, <<"W2-02/confirm-idempotent">>).
-define(C_OLD, <<"W2-02/confirm-old-client">>).
-define(BODY, <<"w601 confirmed immutable content">>).
-define(OTHER, <<"w601 overwritten different bytes">>).

-define(EP4, [ordinary, enterprise_internal, enterprise_seat, cs_widget]).
-define(DRV, [disk, garage]).

w601_w2_confirm_test_() ->
    {setup, fun ?F:setup/0, fun ?F:cleanup/1, fun(S) ->
        [leaf_unit(S, Spec) || Spec <- specs()]
    end}.

%% ------------------------------------------------------------ leaf matrix

specs() -> imm_specs() ++ idem_specs() ++ old_specs().

imm_specs() ->
    [
        {?C_IMM, Ep, Driver, Sc, ?S:surface_domain(Ep), <<"AC03">>}
     || Ep <- ?EP4,
        Driver <- ?DRV,
        Sc <- imm_scenarios(Ep)
    ].

imm_scenarios(ordinary) -> imm_all();
imm_scenarios(_) -> imm_all() -- [<<"ciphertext_only_no_plaintext_hash">>].

imm_all() ->
    [
        <<"put_A_confirm_old_capability_put_B">>,
        <<"overwrite_between_stat_and_confirm">>,
        <<"inflight_same_content_after_confirm">>,
        <<"hash_mismatch_no_confirm">>,
        <<"ciphertext_only_no_plaintext_hash">>,
        <<"crash_after_pin_before_put_resume">>
    ].

%% confirm-idempotent / confirm-old-client：domain ordinary|enterprise，
%% entrypoint 为组名；ordinary 域用 ordinary surface，enterprise 域用
%% enterprise_internal surface（confirm-idempotent/old-client 是域级 case）。
idem_specs() ->
    [
        {?C_IDEM, <<"confirm-idempotent">>, Driver, Sc, Domain, <<"AC03">>}
     || Domain <- [<<"ordinary">>, <<"enterprise">>],
        Driver <- ?DRV,
        Sc <- [<<"sequential_100_replays">>, <<"concurrent_two_confirms">>]
    ].

old_specs() ->
    [
        {?C_OLD, <<"confirm-old-client">>, Driver, Sc, Domain, <<"AC03">>}
     || Domain <- [<<"ordinary">>, <<"enterprise">>],
        Driver <- ?DRV,
        Sc <- [<<"confirmed_legacy_request">>, <<"unknown_legacy_pending">>]
    ].

leaf_unit(S, {Case, Ep, Driver, Sc, Dom, _Ac} = Spec) ->
    Leaf = leaf_name(Case, Ep, Driver, Sc, Dom),
    {
        binary_to_list(Leaf),
        {timeout, 240, fun() ->
            Measured = scenario(S#{leaf => Leaf}, Spec),
            ?S:emit(Spec, Measured, Leaf)
        end}
    }.

leaf_name(Case, Ep, Driver, Sc, Dom) ->
    [_, Short] = binary:split(Case, <<"/">>),
    %% Ep 对域级 case（confirm-idempotent / confirm-old-client）是二进制
    %% entrypoint 组名（idem_specs/old_specs 直传 <<"confirm-idempotent">>），
    %% 对 surface 级 case 是原子（ordinary 等）——atom_to_binary 对二进制
    %% 必 badarg（三跑实证：实例化即崩、全模块取消、夹具 cleanup 未跑且
    %% 证据表泄漏，pending 模块 setup 连带 badmatch）。
    <<
        Short/binary,
        "/",
        (ep_bin(Ep))/binary,
        "/",
        (atom_to_binary(Driver, utf8))/binary,
        "/",
        Dom/binary,
        "/",
        Sc/binary
    >>.

ep_bin(Ep) when is_atom(Ep) -> atom_to_binary(Ep, utf8);
ep_bin(Ep) when is_binary(Ep) -> Ep.

%% Construct the entire matrix before touching PG/HTTP. Binary entrypoint
%% names and the two domains must produce 58 distinct runnable descriptors.
%% 建立完整矩阵无需 PG/HTTP；二进制入口和两个域须生成 58 个不同描述符。
confirm_matrix_instantiation_test() ->
    {setup, _, _, Instantiate} = w601_w2_confirm_test_(),
    Units = Instantiate(#{}),
    ?assertEqual(58, length(Units)),
    Names = [Name || {Name, {timeout, 240, Fun}} <- Units, is_function(Fun, 0)],
    ?assertEqual(58, length(lists:usort(Names))).

%% -------------------------------------------------------- scenario dispatch

scenario(S, {?C_IMM, Ep, Driver, Sc, _, _}) ->
    imm_scenario(S, Ep, Driver, Sc);
scenario(S, {?C_IDEM, _, Driver, Sc, <<"ordinary">>, _}) ->
    idem_scenario(S, ordinary, Driver, Sc);
scenario(S, {?C_IDEM, _, Driver, Sc, <<"enterprise">>, _}) ->
    idem_scenario(S, enterprise_internal, Driver, Sc);
scenario(S, {?C_OLD, _, Driver, Sc, <<"ordinary">>, _}) ->
    old_scenario(S, ordinary, Driver, Sc);
scenario(S, {?C_OLD, _, Driver, Sc, <<"enterprise">>, _}) ->
    old_scenario(S, enterprise_internal, Driver, Sc).

%% ------------------------------------------------- confirmed-content-immutable

imm_scenario(S, Ep, Driver, <<"put_A_confirm_old_capability_put_B">>) ->
    T = reg(S, Ep, Driver),
    %% First write and confirmation complete before arming the provider barrier.
    FirstPut = ?F:put(T, ?BODY),
    ?assert(?F:success(FirstPut)),
    {ok, Pin} = ?S:content_pin_any(Ep, T),
    ?assertEqual(?S:sha(?BODY), maps:get(<<"sha256">>, Pin)),
    First = ?F:confirm(T),
    ok = ?F:bind_request(maps:get(fixture, T), First, key(T), confirm),
    ?assert(?F:success(First)),
    DbBefore = confirmed_snapshot(Ep, T),
    ?S:with_put_barrier(T, fun(Barrier) ->
        Job = ?S:start_job(fun() -> ?F:put(T, ?OTHER) end),
        Old = await_rejected_put(Barrier, Job),
        ?assertEqual(409, maps:get(status, Old)),
        ?assertEqual(0, ?S:put_calls(?S:driver_mod(Driver))),
        ?assertEqual(DbBefore, confirmed_snapshot(Ep, T)),
        {ok, PinAfter} = ?S:content_pin_any(Ep, T),
        ?assertEqual(Pin, PinAfter),
        {ok, ObservedBytes} = ?F:read(T),
        ?assertEqual(?BODY, ObservedBytes),
        #{
            object_key => key(T),
            client_upload_id => maps:get(client_upload_id, T),
            presign_request_id => maps:get(registration_request_id, T),
            put_request_id => maps:get(request_id, FirstPut),
            confirm_request_id => maps:get(request_id, First),
            old_put_request_id => maps:get(request_id, Old),
            confirmed_db_state => DbBefore,
            pin_sha256 => maps:get(<<"sha256">>, Pin),
            object_sha256_after => ?S:sha(ObservedBytes),
            old_capability_put_status => maps:get(status, Old),
            bytes_unchanged => true
        }
    end);
imm_scenario(S, Ep, Driver, <<"overwrite_between_stat_and_confirm">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    ?S:with_read_barrier(T, fun(Barrier) ->
        Confirm = ?S:start_job(fun() -> ?F:confirm(T) end),
        Worker = ?S:await_barrier(Barrier),
        %% stat（GET 字节）与 confirm 之间：provider 上的真实字节被静默换掉。
        overwrite_object(T),
        ?S:release(Barrier, Worker),
        Result = ?S:finish_job(Confirm),
        ?assertNot(?F:success(Result))
    end),
    ?assertEqual(0, ?S:attach_count(key(T))),
    #{confirm_rejected => true, metadata_not_created => true};
imm_scenario(S, Ep, Driver, <<"inflight_same_content_after_confirm">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    ?S:with_put_barrier(T, fun(Barrier) ->
        Inflight = ?S:start_job(fun() -> ?F:put(T, ?BODY) end),
        Worker = ?S:await_barrier(Barrier),
        Confirmed = ?F:confirm(T),
        ?assert(?F:success(Confirmed)),
        ?S:release(Barrier, Worker),
        Result = ?S:finish_job(Inflight),
        ?assert(
            ?F:success(Result) orelse lists:member(maps:get(status, Result), [409, 410])
        ),
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    end),
    #{confirmed_bytes => <<"unchanged">>};
imm_scenario(S, Ep, Driver, <<"hash_mismatch_no_confirm">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    %% pin 之后 provider 字节被换（不经应用链路）：真实 hash 裁决拒绝确认。
    overwrite_object(T),
    Result = ?F:confirm(T),
    ?assertNot(?F:success(Result)),
    ?assertEqual(0, ?S:attach_count(key(T))),
    #{confirm_status => maps:get(status, Result), real_hash_enforced => true};
imm_scenario(S, ordinary, Driver, <<"ciphertext_only_no_plaintext_hash">>) ->
    Cipher = <<"w601 ciphertext-only bytes (client encrypted)">>,
    T = reg(S, ordinary, Driver),
    ?assert(?F:success(?F:put(T, Cipher))),
    Ok = ?FW:confirm_with(maps:get(fixture, T), T, #{cipher => <<"AES-256-GCM">>}),
    ?assert(?F:success(Ok)),
    {ok, [#{<<"cipher">> := StoredCipher, <<"file_hash256">> := StoredHash}]} =
        elib_pg:query(
            <<"SELECT cipher, file_hash256 FROM attachment WHERE path=$1">>, [key(T)]
        ),
    ?assertEqual(<<"AES-256-GCM">>, StoredCipher),
    ?assertEqual(?S:sha(Cipher), StoredHash),
    %% 未知套件 fail-closed：宁可拒绝 confirm 也不把密文对象标成明文。
    T2 = reg(S, ordinary, Driver),
    ?assert(?F:success(?F:put(T2, Cipher))),
    Bad = ?FW:confirm_with(maps:get(fixture, T2), T2, #{cipher => <<"ROT13">>}),
    ?assertNot(?F:success(Bad)),
    ?assertEqual(0, ?S:attach_count(key(T2))),
    #{cipher_stored => <<"AES-256-GCM">>, plaintext_hash_absent => true};
imm_scenario(S, Ep, Driver, <<"crash_after_pin_before_put_resume">>) ->
    T = reg(S, Ep, Driver),
    Mod = ?S:driver_mod(Driver),
    Key = key(T),
    Parent = self(),
    FaultRef = make_ref(),
    meck:new(Mod, [passthrough, no_link]),
    try
        ok = meck:expect(Mod, put_snapshot, fun(I, A, B, C) ->
            case maps:get(key, I, undefined) of
                Key ->
                    Parent ! {w601_put_fault, FaultRef, Key},
                    erlang:error(pin_committed_then_crash);
                _ ->
                    meck:passthrough([I, A, B, C])
            end
        end),
        ?assertNot(?F:success(?F:put(T, ?BODY))),
        receive
            {w601_put_fault, FaultRef, Key} -> ok
        after 1000 ->
            erlang:error(expected_provider_fault_not_observed)
        end
    after
        meck:unload(Mod)
    end,
    {ok, Pin} = ?S:content_pin_any(Ep, T),
    ?assertEqual(?S:sha(?BODY), maps:get(<<"sha256">>, Pin)),
    ?assertEqual({error, not_found}, ?F:read(T)),
    Before = unconfirmed_snapshot(Ep, T),
    Missing = ?F:confirm(T),
    assert_missing_confirm(Ep, Missing),
    ?assertEqual(Before, unconfirmed_snapshot(Ep, T)),
    {ok, PinBeforeResume} = ?S:content_pin_any(Ep, T),
    ?assertEqual(Pin, PinBeforeResume),
    ?assert(?F:success(?F:put(T, ?BODY))),
    First = ?F:confirm(T),
    ?assert(?F:success(First)),
    Id = confirm_id(First, Ep),
    _ = confirmed_snapshot(Ep, T),
    ReceiptBefore = resume_receipt(Ep, T, Id),
    Replay = ?F:confirm(T),
    ?assert(?F:success(Replay)),
    ?assertEqual(Id, confirm_id(Replay, Ep)),
    ?assertEqual(ReceiptBefore, resume_receipt(Ep, T, Id)),
    {ok, ?BODY} = ?F:read(T),
    {ok, PinAfter} = ?S:content_pin_any(Ep, T),
    ?assertEqual(Pin, PinAfter),
    Stored = confirmed_hash(Ep, T),
    ?assertEqual(?S:sha(?BODY), Stored),
    #{
        resumed_same_receipt => true,
        fault_kind => <<"provider_exception">>,
        stored_hash => Stored,
        object_key => key(T),
        object_sha256_after => ?S:sha(?BODY)
    }.

resume_receipt(Ep, T, Id) when Ep =:= ordinary; Ep =:= enterprise_internal ->
    {ok, [#{<<"id">> := Id, <<"referer_time">> := 1} = Row]} = elib_pg:query(
        <<"SELECT id, referer_time FROM attachment WHERE path=$1">>, [key(T)]
    ),
    Row;
resume_receipt(_, T, Id) ->
    Scope = maps:get(scope, maps:get(fixture, T)),
    Id = int(maps:get(<<"asset_id">>, maps:get(registration, T))),
    {ok, [#{<<"id">> := Id} = Row]} = elib_pg:query(
        <<
            "SELECT id, conversation_id, message_id FROM enterprise_asset"
            " WHERE organization_id=$1 AND workspace_id=$2 AND object_key=$3"
        >>,
        [maps:get(org_id, Scope), maps:get(workspace_id, Scope), key(T)]
    ),
    ?assertEqual(0, ?S:attach_count(key(T))),
    Row.

unconfirmed_snapshot(Ep, T) when Ep =:= ordinary; Ep =:= enterprise_internal ->
    {ok, #{<<"session_state">> := <<"registered">>} = Row} = ?S:pending_row(key(T)),
    ?assertEqual(0, ?S:attach_count(key(T))),
    #{session_state => <<"registered">>, storage_identity => storage_identity(Row)};
unconfirmed_snapshot(_, T) ->
    Scope = maps:get(scope, maps:get(fixture, T)),
    AssetId = int(maps:get(<<"asset_id">>, maps:get(registration, T))),
    {ok, #{<<"status">> := <<"pending_confirm">>} = Row} = ?S:eb_row(
        maps:get(org_id, Scope), maps:get(workspace_id, Scope), AssetId
    ),
    {ok, [#{<<"message_id">> := null}]} = elib_pg:query(
        <<
            "SELECT message_id FROM enterprise_asset WHERE organization_id=$1"
            " AND workspace_id=$2 AND id=$3"
        >>,
        [maps:get(org_id, Scope), maps:get(workspace_id, Scope), AssetId]
    ),
    ?assertEqual(0, ?S:attach_count(key(T))),
    #{
        asset_id => AssetId,
        status => <<"pending_confirm">>,
        message_id => null,
        storage_identity => storage_identity(Row)
    }.

assert_missing_confirm(ordinary, #{status := 200, body := Body}) ->
    #{<<"code">> := 400, <<"msg">> := <<"对象不存在或未完成上传"/utf8>>} = jsone:decode(Body),
    ok;
assert_missing_confirm(enterprise_internal, #{status := 400, body := Body}) ->
    #{<<"error">> := #{<<"code">> := <<"invalid_request">>}} = jsone:decode(Body),
    ok;
assert_missing_confirm(Ep, #{status := 500, body := Body}) when
    Ep =:= enterprise_seat; Ep =:= cs_widget
->
    #{<<"code">> := 500, <<"msg">> := <<"object_unreadable.object_store.not_found">>} =
        jsone:decode(Body),
    ok.

confirmed_hash(Ep, T) when Ep =:= ordinary; Ep =:= enterprise_internal ->
    {ok, [#{<<"file_hash256">> := Stored}]} =
        elib_pg:query(<<"SELECT file_hash256 FROM attachment WHERE path=$1">>, [key(T)]),
    Stored;
confirmed_hash(_, T) ->
    Scope = maps:get(scope, maps:get(fixture, T)),
    AssetId = int(maps:get(<<"asset_id">>, maps:get(registration, T))),
    {ok, [#{<<"object_hash">> := Stored}]} = elib_pg:query(
        <<
            "SELECT object_hash FROM enterprise_asset WHERE organization_id=$1"
            " AND workspace_id=$2 AND id=$3 AND object_key=$4"
        >>,
        [maps:get(org_id, Scope), maps:get(workspace_id, Scope), AssetId, key(T)]
    ),
    Stored.

%% ------------------------------------------------------- confirm-idempotent

idem_scenario(S, Ep, Driver, <<"sequential_100_replays">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    First = ?F:confirm(T),
    ?assert(?F:success(First)),
    Id0 = confirm_id(First, Ep),
    %% F7：审计基线在重放前取证——enterprise 域 REQUIRED_AUDIT 首确认恰
    %% 1 行 file.confirmed 审计；ordinary 域无审计写入恒 0 行。
    AuditBefore = audit_rows(Id0),
    [
        begin
            Replay = ?F:confirm(T),
            ?assert(?F:success(Replay)),
            Id0 = confirm_id(Replay, Ep)
        end
     || _ <- lists:seq(1, 100)
    ],
    ?assertEqual(1, ?S:attach_count(key(T))),
    %% F7：幂等 oracle 的两个 SQL 级子句（重放不递增 referer——旧 save/4
    %% 路径 DO UPDATE 会累加 referer_time；治理审计按域冻结：enterprise 域
    %% 首确认写 1 行、重放不增——private 域恒 0 行，任何重放路径误写在此红）。
    assert_replay_oracle(key(T), Id0, Ep),
    %% F7：重放前后审计行数相等（幂等的审计面表达，两域同口径）。
    ?assertEqual(AuditBefore, audit_rows(Id0)),
    #{
        replays => 100,
        single_attachment_id => true,
        referer_unchanged => 1,
        confirm_audit_rows => audit_expected(Ep)
    };
idem_scenario(S, Ep, Driver, <<"concurrent_two_confirms">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    ?S:with_sql_barrier([<<"fn_v5_confirm_pending">>], fun(Barrier) ->
        First = ?S:start_job(fun() -> ?F:confirm(T) end),
        Worker = ?S:await_barrier(Barrier),
        %% 第二路在行锁上排队；两路都以 fn_v5_confirm_pending 线性化。
        Second = ?S:start_job(fun() -> ?F:confirm(T) end),
        ?S:release(Barrier, Worker),
        R1 = ?S:finish_job(First),
        R2 = ?S:finish_job(Second),
        ?assert(?F:success(R1)),
        ?assert(?F:success(R2)),
        Id1 = confirm_id(R1, Ep),
        ?assertEqual(Id1, confirm_id(R2, Ep))
    end),
    ?assertEqual(1, ?S:attach_count(key(T))),
    %% F7：两路线性化后审计仍按域冻结（enterprise 域恰 1 行——只有线性化的
    %% 首路写审计；ordinary 域恒 0 行）。
    assert_replay_oracle(key(T), confirm_id_of_key(key(T)), Ep),
    #{concurrent_winer_ids => <<"identical">>, single_row => true}.

%% ------------------------------------------------------ confirm-old-client

old_scenario(S, Ep, Driver, <<"confirmed_legacy_request">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    First = ?F:confirm(T),
    ?assert(?F:success(First)),
    Id0 = confirm_id(First, Ep),
    Legacy = ?FW:confirm_legacy(T),
    ?assert(?F:success(Legacy)),
    ?assertEqual(Id0, confirm_id(Legacy, Ep)),
    ?assertEqual(1, ?S:attach_count(key(T))),
    #{legacy_accepted => true, no_new_session => true, same_object => true};
old_scenario(S, ordinary, Driver, <<"unknown_legacy_pending">>) ->
    T = reg(S, ordinary, Driver),
    Uid = owner_uid(T),
    GhostKey = elib_oss:build_object_key(Uid, <<"private">>, undefined, <<"w601-ghost.txt">>),
    Bucket = elib_oss:get_bucket(<<"private">>),
    ok = attach_pending_repo:add(GhostKey, Bucket, <<"private">>, Uid),
    Legacy = ?FW:confirm_with(
        maps:get(fixture, T),
        T#{key => GhostKey, confirm_body => #{object_key => GhostKey, scope => <<"private">>}},
        #{file_hash256 => ?S:sha(?BODY), md5 => <<>>}
    ),
    ?assertNot(?F:success(Legacy)),
    {ok, [#{<<"n">> := 1}]} = elib_pg:query(
        <<"SELECT count(*)::bigint AS n FROM attach_pending WHERE object_key=$1">>, [GhostKey]
    ),
    ?assertEqual(0, ?S:attach_count(GhostKey)),
    #{ghost_status => maps:get(status, Legacy), pending_protected => true};
old_scenario(S, enterprise_internal, Driver, <<"unknown_legacy_pending">>) ->
    T = reg(S, enterprise_internal, Driver),
    S2 = maps:get(fixture, T),
    Scope = maps:get(scope, S2),
    Org = maps:get(org_id, Scope),
    GhostKey = enterprise_asset_repo:build_object_key(
        Org, maps:get(service_identity_id, Scope), <<"w601-ghost.bin">>
    ),
    %% F9：enterprise internal V5 pending 与 ordinary 同在 attach_pending
    %% （confirm 的 pending_find_locked_tx 即查此表）——先真实登记受保护
    %% ghost，再证 legacy confirm 拒绝后该行存活、metadata 绝不产生。
    ok = attach_pending_repo:add(
        GhostKey, elib_oss:get_bucket(<<"private">>), <<"private">>, owner_uid(T)
    ),
    Ghost = ?FW:confirm_with(
        S2, T#{key => GhostKey, confirm_body => #{object_key => GhostKey}}, #{}
    ),
    ?assertNot(?F:success(Ghost)),
    {ok, [#{<<"n">> := 1}]} = elib_pg:query(
        <<"SELECT count(*)::bigint AS n FROM attach_pending WHERE object_key=$1">>,
        [GhostKey]
    ),
    ?assertEqual(0, ?S:attach_count(GhostKey)),
    #{ghost_status => maps:get(status, Ghost), pending_protected => true, row_created => false}.

%% ---------------------------------------------------------------- helpers

reg(S, Ep, Driver) -> ?F:register(S, Ep, Driver, ?BODY).

key(T) -> maps:get(key, T).

%% A rejected old capability must not reach the provider. If it does, release
%% the real worker before failing so the fixture does not strand a request.
await_rejected_put(Barrier, {_, Monitor, Ref} = Job) ->
    receive
        {w601_barrier, Barrier, Worker} ->
            ok = ?S:release(Barrier, Worker),
            _ =
                try
                    ?S:finish_job(Job)
                catch
                    _:_ -> ignored
                end,
            erlang:error(old_capability_reached_provider);
        {w601_result, Ref, Result} ->
            erlang:demonitor(Monitor, [flush]),
            put(w601_jobs, lists:delete(Job, get(w601_jobs))),
            case Result of
                {ok, Value} -> Value;
                {error, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
            end
    after 10000 ->
        erlang:error(old_capability_response_timeout)
    end.

confirmed_snapshot(Ep, T) when Ep =:= ordinary; Ep =:= enterprise_internal ->
    {ok, #{<<"session_state">> := State} = Row} = ?S:pending_row(key(T)),
    ?assertEqual(<<"confirmed">>, State),
    ?assertEqual(1, ?S:attach_count(key(T))),
    #{session_state => State, attachment_count => 1, storage_identity => storage_identity(Row)};
confirmed_snapshot(_, T) ->
    Scope = maps:get(scope, maps:get(fixture, T)),
    AssetId = int(maps:get(<<"asset_id">>, maps:get(registration, T))),
    {ok, #{<<"status">> := State} = Row} = ?S:eb_row(
        maps:get(org_id, Scope), maps:get(workspace_id, Scope), AssetId
    ),
    ?assertEqual(<<"active">>, State),
    #{asset_id => AssetId, status => State, storage_identity => storage_identity(Row)}.

storage_identity(Row) ->
    Keys = [
        <<"object_key">>,
        <<"storage_driver">>,
        <<"storage_namespace">>,
        <<"storage_container">>,
        <<"storage_locator_sha256">>
    ],
    Values = maps:with(Keys, Row),
    ?assertEqual(length(Keys), map_size(Values)),
    ?assert(
        lists:all(
            fun(V) -> is_binary(V) andalso byte_size(V) > 0 end,
            maps:values(Values)
        )
    ),
    Values.

confirm_id(Resp, ordinary) ->
    int(maps:get(<<"attachment_id">>, ?F:payload(Resp)));
confirm_id(Resp, enterprise_internal) ->
    int(maps:get(<<"file_id">>, ?F:payload(Resp)));
confirm_id(Resp, Ep) when Ep =:= enterprise_seat; Ep =:= cs_widget ->
    int(maps:get(<<"asset_id">>, ?F:payload(Resp))).

confirm_id_of_key(Key) ->
    {ok, [#{<<"id">> := Id}]} = elib_pg:query(
        <<"SELECT id FROM attachment WHERE path=$1">>, [Key]
    ),
    Id.

%% F7：幂等 oracle 服务端取证——referer_time 恒 1（confirm 重放复用既有
%% 行、不递增引用计数；旧 save/4 路径的 DO UPDATE 递增会在此红灯）；
%% 治理审计按域冻结：enterprise 域 INT-08 REQUIRED_AUDIT 首确认在同事务写
%% 恰 1 行 file.confirmed 审计（enterprise_asset_logic.erl:786-791
%% audit_mutation → enterprise_audit_event_repo:append_tx，:589 v5 路径同；
%% 重放 First=false 分支 :771-777 不再写）；ordinary 域（attach_logic/
%% attach_pending_repo）无审计写入，恒 0 行。任何域的重放误写审计都不得
%% 出现（sequential 场景另以「重放前后行数相等」表达幂等）。
assert_replay_oracle(Key, AttachmentId, Domain) ->
    {ok, [#{<<"referer_time">> := Referer}]} = elib_pg:query(
        <<"SELECT referer_time FROM attachment WHERE path=$1">>, [Key]
    ),
    ?assertEqual(1, Referer),
    ?assertEqual(audit_expected(Domain), audit_rows(AttachmentId)),
    ok.

%% F7：file.confirmed 审计行数取证（enterprise_audit_event：
%% resource_type='attachment'，resource_id=file_id，action='file.confirmed'）。
audit_rows(AttachmentId) ->
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        <<
            "SELECT count(*)::bigint AS n FROM enterprise_audit_event"
            " WHERE resource_type='attachment' AND resource_id=$1"
            " AND action='file.confirmed'"
        >>,
        [AttachmentId]
    ),
    N.

%% F7：审计行数的域冻结期望（与生产 REQUIRED_AUDIT 政策逐字对齐）。
audit_expected(enterprise_internal) -> 1;
audit_expected(ordinary) -> 0.

int(I) when is_integer(I) -> I;
int(B) when is_binary(B) -> binary_to_integer(B).

owner_uid(T) ->
    {ok, #{<<"creator_user_id">> := Uid}} = ?S:pending_row(key(T)),
    Uid.

%% provider 上的真实静默覆写（绕过应用，只经 driver 写同一冻结位置）。
overwrite_object(T) ->
    I = maps:get(location, T),
    Ns = maps:get(namespace, I),
    Container = maps:get(container, I),
    Mod = ?S:driver_mod(maps:get(driver, T)),
    ok = Mod:put(Ns, Container, key(T), ?OTHER).
