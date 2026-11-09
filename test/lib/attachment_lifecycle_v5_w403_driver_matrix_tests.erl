%%% 每叶通过真实入口执行；缺入口 harness 是 NO_GO，不能用 raw driver 顶替。
-module(attachment_lifecycle_v5_w403_driver_matrix_tests).
-include_lib("eunit/include/eunit.hrl").

%% Focused first-stage proof. The immutable matrix below remains independent;
%% a successful round trip never seals an unexecuted race/fault scenario.
roundtrip_test_() ->
    %% 受控让位（仓库先例：attachment_lifecycle_v5_pg_tests r20_test_/
    %% v5_gate_pinned、w403_gateway_protocol_tests protocol_test_：eunit 无
    %% {skip, ...} 顶层描述符，生成器返回空清单才是等价的静默让位机制）。
    %% 本套件经 attachment_w403_http_fixture:setup 依赖
    %% attachment_lifecycle_v5_w403_gateway_protocol_tests:setup，后者
    %% required("W403_RESOURCE_MANIFEST") fail-closed；nightly 全量裸跑
    %% （make eunit-local-legacy）不经 gate，缺钉名 env 时在生成器入口
    %% 让位，不进 setup。gate 路径（env 在场）语义与断言零删减。
    case os:getenv("W403_RESOURCE_MANIFEST") of
        V when is_list(V), V =/= "" ->
            roundtrip_units();
        _ ->
            io:format(user, "W403DM_EUNIT_YIELD missing_env=\"W403_RESOURCE_MANIFEST\"~n", []),
            []
    end.

roundtrip_units() ->
    {setup, fun attachment_w403_http_fixture:setup/0, fun attachment_w403_http_fixture:cleanup/1,
        fun(S) ->
            [
                {
                    atom_to_list(Surface) ++ "/" ++ atom_to_list(Driver),
                    {timeout, 180, fun() ->
                        Leaf =
                            <<"roundtrip/", (atom_to_binary(Surface))/binary, "/",
                                (atom_to_binary(Driver))/binary>>,
                        try
                            roundtrip(S#{leaf => Leaf}, Surface, Driver)
                        after
                            report_evidence(Leaf)
                        end
                    end}
                }
             || Surface <-
                    %% V3-14：INT 面直传形态无 gateway put 会话（409 拒覆写/
                    %% pin 链不适用），矩阵收敛三面。
                    [ordinary, enterprise_seat, cs_widget],
                %% V3-20：disk 驱动退役，disk 侧矩阵随驱动删除，garage 侧保留。
                Driver <- [garage]
            ]
        end}.

%% Separate from the fixed 64-leaf matrix: the declared hash and size match the
%% real body, but a disallowed text control byte lies beyond the first 64 KiB.
sealed_body_validation_test_() ->
    %% 受控让位：同 roundtrip_test_/0 的 W403_RESOURCE_MANIFEST gate。
    case os:getenv("W403_RESOURCE_MANIFEST") of
        V when is_list(V), V =/= "" ->
            sealed_body_validation_units();
        _ ->
            io:format(user, "W403DM_EUNIT_YIELD missing_env=\"W403_RESOURCE_MANIFEST\"~n", []),
            []
    end.

sealed_body_validation_units() ->
    {setup, fun attachment_w403_http_fixture:setup/0, fun attachment_w403_http_fixture:cleanup/1,
        fun(S) ->
            [
                {
                    atom_to_list(Surface) ++ "/" ++ atom_to_list(Driver),
                    {timeout, 180, fun() ->
                        Leaf =
                            <<"sealed-body/", (atom_to_binary(Surface))/binary, "/",
                                (atom_to_binary(Driver))/binary>>,
                        try
                            Prefix = binary:copy(<<"a">>, 65537),
                            Bytes = <<Prefix/binary, 1, "tail">>,
                            T = reg(S#{leaf => Leaf}, Surface, Driver, Bytes),
                            Mod = driver(Driver),
                            meck:new(Mod, [passthrough, no_link]),
                            try
                                Result = put_bytes(T, Bytes),
                                ?assertNot(attachment_w403_http_fixture:success(Result)),
                                ?assertEqual(0, put_calls(Mod)),
                                ?assertEqual(
                                    {error, not_found}, attachment_w403_http_fixture:read(T)
                                )
                            after
                                meck:unload(Mod)
                            end
                        after
                            report_evidence(Leaf)
                        end
                    end}
                }
             || Surface <- [enterprise_seat, cs_widget],
                %% V3-20：disk 驱动退役，disk 侧矩阵随驱动删除，garage 侧保留。
                Driver <- [garage]
            ]
        end}.

report_evidence(Leaf) ->
    io:format(user, "W403_HTTP_EVIDENCE ~s~n", [
        jsone:encode(#{
            leaf => Leaf,
            requests => attachment_w403_http_fixture:evidence(Leaf)
        })
    ]).

w403_driver_matrix_test_() ->
    %% 受控让位：同 roundtrip_test_/0 的 W403_RESOURCE_MANIFEST gate。
    case os:getenv("W403_RESOURCE_MANIFEST") of
        V when is_list(V), V =/= "" ->
            matrix_units();
        _ ->
            io:format(user, "W403DM_EUNIT_YIELD missing_env=\"W403_RESOURCE_MANIFEST\"~n", []),
            []
    end.

matrix_units() ->
    {setup, fun attachment_w403_http_fixture:setup/0, fun attachment_w403_http_fixture:cleanup/1,
        fun(S) ->
            [
                {
                    atom_to_list(Surface) ++ "/" ++ atom_to_list(Driver) ++ "/" ++
                        atom_to_list(Scenario),
                    {timeout, 180, fun() -> run_leaf(S, Surface, Driver, Scenario) end}
                }
             || Surface <-
                    %% V3-14：INT 面直传形态无 gateway put 会话（409 拒覆写/
                    %% pin 链不适用），矩阵收敛三面。
                    [ordinary, enterprise_seat, cs_widget],
                %% V3-20：disk 驱动退役，disk 侧矩阵随驱动删除，garage 侧保留。
                Driver <- [garage],
                Scenario <- scenarios()
            ]
        end}.
scenarios() ->
    [
        confirm_old_put_different_bytes,
        stat_then_different_put,
        inflight_same_bytes_after_confirm,
        put_paused_before_dispatch_until_expiry,
        same_key_different_worker_payload,
        provider_bypass_writer_denied,
        max_supported_file_gateway_budget,
        disk_put_delete_lock_order_or_garage_expiry_fence
    ].

-define(F, attachment_w403_http_fixture).
-define(FW, attachment_w403_http_fixture_w601).
-define(BODY, <<"w403 original registered content">>).

%% C10 S3-only 面分派登记/字节链：seat/widget 的 presign 响应为 opaque
%% token descriptor（无 upload.url，主 fixture 的 seat/widget 分支随字节
%% 代理链退役已不可用）——登记走 ?FW:register；字节写走各面存留的生产
%% 链（ordinary=multipart 上传端点 attachment_upload_logic:pin_and_put；
%% seat/widget=eb 应用链 eb_asset_app:put_object），均经真实 driver
%% put_snapshot（barrier/meck 观察面保持）。
reg(S, Surface, Driver, Bytes) ->
    ?FW:register(S, Surface, Driver, Bytes).

put_bytes(T, Bytes) ->
    S = maps:get(fixture, T),
    case maps:get(surface, T) of
        ordinary -> ?FW:upload_put(S, T, Bytes);
        _ -> ?FW:put(T, Bytes)
    end.

%% C10：ordinary 面字节链仍为真实 HTTP（multipart 上传端点），Cowboy 派发
%% 观察在场——overlap 双请求配对证据保持；seat/widget 字节链已随 HTTP
%% 代理退役移到应用层（无派发观察），「两请求真实交叠」的 HTTP 配对证据
%% 不再可表达——保留字节级断言（拒写/provider 零写/字节不变），不伪造。
maybe_overlap(ordinary, Leaf, Worker, Competing) ->
    ?F:overlap(Leaf, Worker, Competing);
maybe_overlap(_Surface, _Leaf, _Worker, _Competing) ->
    ok.

%% C10 S3-only 真实往返：读回/确认/确认后换体拒写断言面原样，字节写走
%% 上述生产链（确认后旧能力换体在会话状态门/凭证 hash 门被拒）。
roundtrip(S, Surface, Driver) ->
    Bytes = <<"w403 authenticated immutable content">>,
    Session = reg(S, Surface, Driver, Bytes),
    ?assertEqual({error, not_found}, ?F:read(Session)),
    ?assert(?F:success(put_bytes(Session, Bytes))),
    ?assertEqual({ok, Bytes}, ?F:read(Session)),
    ?assert(?F:success(?F:confirm(Session))),
    ?assertNot(?F:success(put_bytes(Session, <<"different bytes">>))),
    ?assertEqual({ok, Bytes}, ?F:read(Session)),
    Session.

run_leaf(S0, Surface, Driver, Scenario) ->
    Leaf = <<
        (atom_to_binary(Surface))/binary,
        "/",
        (atom_to_binary(Driver))/binary,
        "/",
        (atom_to_binary(Scenario))/binary
    >>,
    S = S0#{leaf => Leaf},
    try
        scenario(S, Surface, Driver, Scenario),
        w403dm_emit(Surface, Driver, Scenario, Leaf)
    after
        cleanup_jobs(),
        report_evidence(Leaf)
    end.

%% 逐叶观测：64 cell（surface × driver × scenario）断言全过后落盘。
%% domain 按契约矩阵：enterprise_seat/cs_widget 为 enterprise 面，其余 ordinary。
%% measured 携带服务器侧绑定证据：evidence(Leaf) 的每条真实请求
%% （request_id/route/method/handler/status 与 bind_request 绑定的
%% object_key/action），以及该请求进程内 server_trace 收集的生产存储
%% 解析记录（trust 标记如实为 diagnostic_untrusted）。adapter 侧据此
%% 完成 HTTP 与对象来源对服务器的独立绑定校验；此处不放宽任何语义。
w403dm_emit(Surface, Driver, Scenario, Leaf) ->
    Requests = attachment_w403_http_fixture:evidence(Leaf),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => <<"W4-03/driver-content-immutable">>,
        entrypoint => atom_to_binary(Surface, utf8),
        scenario => atom_to_binary(Scenario, utf8),
        domain => w403dm_domain(Surface),
        schema_phase => <<"expand-queue">>,
        measured => #{
            driver => atom_to_binary(Driver, utf8),
            entrypoint_case => atom_to_binary(Scenario, utf8),
            ac_id => <<"AC03">>,
            object_key => w403dm_object_key(Requests),
            http_requests => Requests
        }
    }),
    ok.

w403dm_object_key([]) -> <<>>;
w403dm_object_key([#{object_key := Key} | _]) -> Key;
w403dm_object_key([_ | Rest]) -> w403dm_object_key(Rest).

w403dm_domain(enterprise_seat) -> <<"enterprise">>;
w403dm_domain(cs_widget) -> <<"enterprise">>;
w403dm_domain(_) -> <<"ordinary">>.

scenario(S, Surface, Driver, confirm_old_put_different_bytes) ->
    T = reg(S, Surface, Driver, ?BODY),
    Mod = driver(Driver),
    meck:new(Mod, [passthrough, no_link]),
    try
        ?assert(?F:success(put_bytes(T, ?BODY))),
        ?assert(?F:success(?F:confirm(T))),
        Before = put_calls(Mod),
        ?assertNot(?F:success(put_bytes(T, <<"different old capability content">>))),
        ?assertEqual(Before, put_calls(Mod)),
        %% Fresh same-content PUT after confirmation is read-only/idempotent or
        %% rejected. Neither outcome permits another provider write.
        _ = put_bytes(T, ?BODY),
        ?assertEqual(Before, put_calls(Mod)),
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    after
        meck:unload(Mod)
    end;
scenario(S, Surface, Driver, stat_then_different_put) ->
    T = reg(S, Surface, Driver, ?BODY),
    ?assert(?F:success(put_bytes(T, ?BODY))),
    with_read_barrier(T, fun(Barrier) ->
        Job = start_job(fun() -> ?F:confirm(T) end),
        Worker = await_barrier(Barrier),
        case Surface of
            enterprise_internal ->
                serialized_competing_put(T, Barrier, Worker, Job);
            _ ->
                Competing = put_bytes(T, <<"replacement after observed bytes">>),
                ?assertNot(?F:success(Competing)),
                maybe_overlap(Surface, maps:get(leaf, T), Worker, Competing),
                release(Barrier, Worker),
                ?assert(?F:success(finish_job(Job)))
        end,
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    end);
scenario(S, Surface, Driver, inflight_same_bytes_after_confirm) ->
    T = reg(S, Surface, Driver, ?BODY),
    ?assert(?F:success(put_bytes(T, ?BODY))),
    with_put_barrier(T, fun(Barrier) ->
        Job = start_job(fun() -> put_bytes(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        Confirmed = ?F:confirm(T),
        ?assert(?F:success(Confirmed)),
        maybe_overlap(Surface, maps:get(leaf, T), Worker, Confirmed),
        release(Barrier, Worker),
        Result = finish_job(Job),
        ?assert(?F:success(Result) orelse lists:member(maps:get(status, Result), [409, 410])),
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    end);
scenario(S, Surface, Driver, put_paused_before_dispatch_until_expiry) ->
    T = short_registration(S, Surface, Driver, ?BODY),
    with_put_barrier(T, fun(Barrier) ->
        Job = start_job(fun() -> put_bytes(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        wait_expired(T),
        release(Barrier, Worker),
        ?assertNot(?F:success(finish_job(Job))),
        ?assertEqual({error, not_found}, ?F:read(T)),
        ?assertNot(?F:success(?F:confirm(T)))
    end);
scenario(S, Surface, Driver, same_key_different_worker_payload) ->
    T = reg(S, Surface, Driver, ?BODY),
    with_put_barrier(T, fun(Barrier) ->
        First = start_job(fun() -> put_bytes(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        %% The first worker has committed its receipt, but not dispatched I/O.
        Second = start_job(fun() -> put_bytes(T, <<"different competing worker payload">>) end),
        Competing = finish_job(Second),
        ?assertNot(?F:success(Competing)),
        maybe_overlap(Surface, maps:get(leaf, T), Worker, Competing),
        ?assertEqual({error, not_found}, ?F:read(T)),
        release(Barrier, Worker),
        ?assert(?F:success(finish_job(First))),
        ?assert(?F:success(?F:confirm(T))),
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    end);
scenario(S, Surface, Driver, provider_bypass_writer_denied) ->
    T = reg(S, Surface, Driver, ?BODY),
    ?assert(?F:success(put_bytes(T, ?BODY))),
    ?assert(?F:success(?F:confirm(T))),
    bypass_denied(T),
    ?assertEqual({ok, ?BODY}, ?F:read(T));
scenario(S, Surface, Driver, max_supported_file_gateway_budget) ->
    Size = ?F:max_bytes(Surface),
    Bytes = binary:copy(<<"a">>, Size),
    T = reg(S, Surface, Driver, Bytes),
    erlang:garbage_collect(),
    Baseline = erlang:memory(binary),
    Parent = self(),
    Ref = make_ref(),
    Sampler = spawn_link(fun() -> sample_memory(Parent, Ref, Baseline) end),
    Started = erlang:monotonic_time(millisecond),
    Response =
        try
            put_bytes(T, Bytes)
        after
            Sampler ! {stop, Ref}
        end,
    Elapsed = erlang:monotonic_time(millisecond) - Started,
    Peak =
        receive
            {memory_peak, Ref, P} -> P
        after 1000 ->
            unlink(Sampler),
            exit(Sampler, kill),
            erlang:error(memory_sampler_timeout)
        end,
    Delta = max(0, Peak - Baseline),
    io:format(user, "W403_GATEWAY_BUDGET ~s~n", [
        jsone:encode(#{
            leaf => maps:get(leaf, T),
            payload_bytes => Size,
            peak_binary_delta_bytes => Delta,
            elapsed_ms => Elapsed,
            throughput_bytes_per_second => (Size * 1000) div max(1, Elapsed),
            peak_binary_delta_per_payload => Delta / Size
        })
    ]),
    %% Preserve the configured maximum size and HTTP timeout. Memory is recorded
    %% as evidence; do not invent a new SLO for the retained httpc binary path.
    ?assert(?F:success(Response)),
    ?assert(Elapsed < 120000),
    ?assertEqual({ok, Bytes}, ?F:read(T)),
    ?assert(?F:success(?F:confirm(T)));
%% V3-20：disk 驱动退役，disk_put_delete_lock_order 场景随驱动删除，
%% 该叶仅保留 garage 的签名过期栅栏（garage_signed_expiry）。
scenario(S, Surface, garage, disk_put_delete_lock_order_or_garage_expiry_fence) ->
    garage_signed_expiry(short_registration(S, Surface, garage, ?BODY)).

%% Barriers only pause a real operation for this unique key. Authentication,
%% persistence, signatures and provider responses always remain production code.
serialized_competing_put(T, Barrier, ConfirmWorker, ConfirmJob) ->
    %% Internal confirm holds its identity/session transaction across the read.
    %% Observe the competing authenticated request before its real admission
    %% waits for that lock, then release confirm and require the final rejection.
    Key = maps:get(key, T),
    Bytes = <<"replacement after observed bytes">>,
    Hash = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Parent = self(),
    Admission = make_ref(),
    ok = meck:new(attach_pending_repo, [passthrough, no_link]),
    try
        ok = meck:expect(attach_pending_repo, register_content_pin, fun(K, Receipt) ->
            case K =:= Key andalso maps:get(<<"sha256">>, Receipt) =:= Hash of
                true -> Parent ! {w403_admission_entered, Admission, self()};
                false -> ok
            end,
            meck:passthrough([K, Receipt])
        end),
        PutJob = start_job(fun() -> put_bytes(T, Bytes) end),
        PutWorker =
            receive
                {w403_admission_entered, Admission, Pid} -> Pid
            after 10000 -> erlang:error(w403_admission_not_reached)
            end,
        RequestId = ?F:inflight_overlap(maps:get(leaf, T), ConfirmWorker, PutWorker),
        ?assertEqual(0, put_calls(driver(maps:get(driver, T)))),
        release(Barrier, ConfirmWorker),
        ?assert(?F:success(finish_job(ConfirmJob))),
        Competing = finish_job(PutJob),
        ?assertEqual(RequestId, maps:get(request_id, Competing)),
        ?assertEqual(409, maps:get(status, Competing)),
        ?assertNot(?F:success(Competing)),
        ?assertEqual(0, put_calls(driver(maps:get(driver, T))))
    after
        release(Barrier, ConfirmWorker),
        cleanup_jobs(),
        meck:unload(attach_pending_repo)
    end.

with_read_barrier(T, Fun) ->
    Mod = driver(maps:get(driver, T)),
    Key = maps:get(key, T),
    Parent = self(),
    Barrier = make_ref(),
    Gate = ets:new(w403_barrier, [public, set]),
    meck:new(Mod, [passthrough, no_link]),
    try
        meck:expect(Mod, read_snapshot, fun(I) ->
            Result = meck:passthrough([I]),
            case maps:get(key, I) =:= Key andalso Result =:= {ok, ?BODY} of
                true -> pause_once(Parent, Barrier, Gate);
                false -> ok
            end,
            Result
        end),
        Fun(Barrier)
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(Mod)
    end.

with_put_barrier(T, Fun) ->
    Mod = driver(maps:get(driver, T)),
    Key = maps:get(key, T),
    Parent = self(),
    Barrier = make_ref(),
    Gate = ets:new(w403_barrier, [public, set]),
    meck:new(Mod, [passthrough, no_link]),
    try
        %% V3-20：disk 驱动退役，put_snapshot 栅栏仅剩 garage 四参形态。
        case maps:get(driver, T) of
            garage ->
                meck:expect(Mod, put_snapshot, fun(I, Source, Mime, Ctx) ->
                    maybe_pause(I, Key, Parent, Barrier, Gate),
                    meck:passthrough([I, Source, Mime, Ctx])
                end)
        end,
        Fun(Barrier)
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(Mod)
    end.

maybe_pause(I, Key, Parent, Barrier, Gate) ->
    case maps:get(key, I) =:= Key of
        true -> pause_once(Parent, Barrier, Gate);
        false -> ok
    end.
pause_once(Parent, Barrier, Gate) ->
    Entered =
        try
            ets:insert_new(Gate, {worker, self()})
            %% The owning leaf is already cleaning up.
        catch
            error:badarg -> false
        end,
    case Entered of
        false ->
            ok;
        true ->
            Parent ! {w403_barrier, Barrier, self()},
            receive
                {w403_continue, Barrier} -> ok
            after 30000 -> erlang:error(w403_barrier_timeout)
            end
    end.
await_barrier(Barrier) ->
    receive
        {w403_barrier, Barrier, Pid} ->
            erlang:put({w403_waiter, Barrier}, Pid),
            Pid
    after 10000 -> erlang:error(w403_barrier_not_reached)
    end.
release(Barrier, Pid) ->
    Pid ! {w403_continue, Barrier},
    ok.
release_all(Barrier, Gate) ->
    [release(Barrier, Pid) || {worker, Pid} <- ets:tab2list(Gate)],
    erlang:erase({w403_waiter, Barrier}),
    ets:delete(Gate),
    ok.
start_job(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, Monitor} = spawn_opt(
        fun() ->
            Result =
                try
                    {ok, Fun()}
                catch
                    Class:Reason:Stack -> {error, Class, Reason, Stack}
                end,
            Parent ! {w403_result, Ref, Result}
        end,
        [link, monitor]
    ),
    Job = {Pid, Monitor, Ref},
    erlang:put(w403_jobs, [Job | jobs()]),
    Job.
finish_job({Pid, Monitor, Ref} = Job) ->
    try
        receive
            {w403_result, Ref, {ok, Value}} ->
                erlang:demonitor(Monitor, [flush]),
                Value;
            {w403_result, Ref, {error, Class, Reason, Stack}} ->
                erlang:demonitor(Monitor, [flush]),
                erlang:raise(Class, Reason, Stack);
            {'DOWN', Monitor, process, Pid, Reason} ->
                erlang:error({w403_worker_exit, Reason})
        after 130000 ->
            unlink(Pid),
            exit(Pid, kill),
            erlang:error(w403_worker_timeout)
        end
    after
        erlang:put(w403_jobs, lists:delete(Job, jobs()))
    end.
jobs() ->
    case erlang:get(w403_jobs) of
        L when is_list(L) -> L;
        _ -> []
    end.
cleanup_jobs() ->
    %% Release real-operation barriers before waiting here. Do not unload meck
    %% while an owned client or its server-side operation is still returning.
    [
        try
            finish_job(Job)
        catch
            _:_ -> ok
        end
     || Job <- jobs()
    ],
    erlang:erase(w403_jobs),
    ok.
%% V3-20：disk 驱动退役，driver 映射仅剩 garage（attachment_storage_disk 已删除）。
driver(garage) -> attachment_storage_garage.
put_calls(Mod) -> meck:num_calls(Mod, put_snapshot, '_').

%% Shorten only this leaf's initial persisted expiry, before production signing.
%% This is a real short-lived session, not a forged token or mocked PG result.
short_registration(S, Surface, Driver, Bytes) ->
    Cid = elib_uuid:gen_v7(),
    Parent = self(),
    meck:new(elib_pg, [passthrough, no_link]),
    T =
        try
            meck:expect(elib_pg, query, fun(Sql, Params) ->
                meck:passthrough([Sql, short_expiry(Sql, Params, Cid, Parent)])
            end),
            meck:expect(elib_pg, query, fun(Conn, Sql, Params) ->
                meck:passthrough([Conn, Sql, short_expiry(Sql, Params, Cid, Parent)])
            end),
            reg(S#{registration_cid => Cid}, Surface, Driver, Bytes)
        after
            meck:unload(elib_pg)
        end,
    receive
        {short_expiry, Cid, Expiry, OriginalDeadline} ->
            {Expiry, OriginalDeadline} = persisted_times(T),
            ?assertEqual(Expiry, expiry(maps:get(<<"expires_at">>, maps:get(registration, T)))),
            T#{expiry => Expiry}
    after 1000 -> erlang:error(short_expiry_not_persisted)
    end.

short_expiry(Sql0, Args, Cid, Parent) when is_list(Args) ->
    Sql = iolist_to_binary(Sql0),
    Positions =
        case
            {
                binary:match(Sql, <<"fn_v5_register_pending(">>),
                binary:match(Sql, <<"fn_v5_register_eb(">>)
            }
        of
            {{_, _}, _} when length(Args) =:= 12 -> {5, 7, 8};
            {_, {_, _}} when length(Args) =:= 9 -> {5, 8, 9};
            _ -> none
        end,
    case Positions of
        {C, D, E} ->
            case lists:nth(C, Args) =:= Cid of
                true ->
                    Deadline = expiry(lists:nth(D, Args)),
                    Original = expiry(lists:nth(E, Args)),
                    Short = min(erlang:system_time(second) + 3, min(Original, Deadline)),
                    Parent ! {short_expiry, Cid, Short, Deadline},
                    replace_nth(E, iso(Short), Args);
                false ->
                    Args
            end;
        none ->
            Args
    end;
short_expiry(_, Args, _, _) ->
    Args.
replace_nth(N, V, List) ->
    {A, [_ | B]} = lists:split(N - 1, List),
    A ++ [V | B].
iso(T) -> list_to_binary(calendar:system_time_to_rfc3339(T, [{unit, second}, {offset, "Z"}])).
expiry(T) when is_integer(T) -> T;
expiry(T) when is_binary(T) ->
    try
        binary_to_integer(T)
    catch
        error:badarg ->
            calendar:rfc3339_to_system_time(binary_to_list(T), [{unit, second}])
    end;
expiry(T) when is_list(T) -> calendar:rfc3339_to_system_time(T, [{unit, second}]).
session_table(#{surface := Surface}) when Surface =:= ordinary; Surface =:= enterprise_internal ->
    <<"attach_pending">>;
session_table(_) ->
    <<"enterprise_asset">>.
persisted_times(#{key := Key} = T) ->
    Table = session_table(T),
    {ok, [#{<<"expiry">> := E, <<"deadline">> := D}]} = elib_pg:query(
        <<"SELECT extract(epoch from upload_expires_at)::bigint AS expiry, extract(epoch from session_deadline)::bigint AS deadline FROM ",
            Table/binary, " WHERE object_key=$1">>,
        [Key]
    ),
    {E, D}.
wait_expired(T) ->
    Expiry = maps:get(expiry, T),
    Delay = max(0, Expiry * 1000 - erlang:system_time(millisecond) + 50),
    timer:sleep(Delay),
    ?assert(erlang:system_time(second) >= Expiry),
    Table = session_table(T),
    {ok, [#{<<"expired">> := true}]} = elib_pg:query(
        <<"SELECT clock_timestamp()>=upload_expires_at AS expired FROM ", Table/binary,
            " WHERE object_key=$1">>,
        [maps:get(key, T)]
    ).

bypass_denied(#{driver := garage, location := I} = T) ->
    %% The actual provider receives an unsigned write, not a mocked denial.
    Key = maps:get(key, I),
    match = re:run(Key, <<"^[a-zA-Z0-9/_.-]+$">>, [{capture, none}]),
    Endpoint = string:trim(binary_to_list(maps:get(endpoint, I)), trailing, "/"),
    Url = Endpoint ++ "/" ++ binary_to_list(maps:get(container, I)) ++ "/" ++ binary_to_list(Key),
    {ok, {{_, Code, _}, _, _}} = httpc:request(
        put,
        {Url, [], "text/plain", <<"unsigned bypass writer">>},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ?assert(lists:member(Code, [401, 403])),
    bypass_evidence(T, #{provider_http_status => Code}).
%% V3-20：disk 驱动退役，bypass_denied 的 disk 子句（runuser/sudo/container
%% 非特权写者拒绝证明）随驱动删除，仅保留 garage 未签名直写拒绝面。

bypass_evidence(T, Detail) ->
    %% EUnit's group leader owns the per-case output consumed by the native
    %% Surefire proof validator. The user device bypasses that capture.
    io:format("W403_PROVIDER_BYPASS ~s~n", [jsone:encode(Detail#{leaf => maps:get(leaf, T)})]).

sample_memory(Parent, Ref, Peak0) ->
    Peak = max(Peak0, erlang:memory(binary)),
    receive
        {stop, Ref} -> Parent ! {memory_peak, Ref, Peak}
    after 2 -> sample_memory(Parent, Ref, Peak)
    end.

%% Capture only the actual signer result in memory, then pause the entry upload.
%% The application's failed late upload and Garage's rejection of the unchanged
%% signed URL are separate assertions. A local abort is never a remote drain proof.
garage_signed_expiry(T) ->
    Key = maps:get(key, T),
    Parent = self(),
    Barrier = make_ref(),
    Gate = ets:new(w403_signed_barrier, [public, set]),
    meck:new(elib_s3_sign, [passthrough, no_link]),
    try
        meck:expect(
            elib_s3_sign,
            presign_url_at,
            fun(Method, EP, C, K, Mime, Issued, Ttl, Cfg) ->
                Url = meck:passthrough([Method, EP, C, K, Mime, Issued, Ttl, Cfg]),
                case Method =:= <<"PUT">> andalso K =:= Key of
                    true ->
                        ?assertEqual(
                            maps:get(expiry, T),
                            calendar:datetime_to_gregorian_seconds(Issued) + Ttl - 62167219200
                        ),
                        Parent ! {frozen_provider_url, Barrier, Url, Mime},
                        pause_once(Parent, Barrier, Gate);
                    false ->
                        ok
                end,
                Url
            end
        ),
        Job = start_job(fun() -> put_bytes(T, ?BODY) end),
        Worker = await_barrier(Barrier),
        {Url, Mime} =
            receive
                {frozen_provider_url, Barrier, U, M} -> {U, M}
            after 1000 -> erlang:error(frozen_provider_url_missing)
            end,
        wait_expired(T),
        release(Barrier, Worker),
        %% This proves the real application entry did not accept a late upload.
        ?assertNot(?F:success(finish_job(Job))),
        ?assertEqual({error, not_found}, ?F:read(T)),
        %% Independently prove the real provider enforces its signed deadline.
        %% No URL rewriting, refreshed signature, credential or response mock.
        ProbeId = <<"w403-", (elib_uuid:gen_v7())/binary>>,
        {ok, {{_, Code, _}, _, ErrorBody}} = httpc:request(
            put,
            {
                binary_to_list(Url),
                [{"x-w403-request-id", binary_to_list(ProbeId)}],
                binary_to_list(Mime),
                ?BODY
            },
            [{timeout, 30000}, {autoredirect, false}],
            [{body_format, binary}]
        ),
        ExpiryReason = provider_expiry_reason(Code, ErrorBody),
        io:format(user, "W403_GARAGE_EXPIRY ~s~n", [
            jsone:encode(#{
                leaf => maps:get(leaf, T),
                stage => <<"unchanged_signed_provider_replay">>,
                method => <<"PUT">>,
                request_id => ProbeId,
                status => Code,
                provider_error_code => provider_error_code(ErrorBody),
                provider_expiry_reason => atom_to_binary(ExpiryReason)
            })
        ]),
        ?assertEqual(signed_url_expired, ExpiryReason),
        ?assertEqual({error, not_found}, ?F:read(T)),
        ?assertNot(?F:success(?F:confirm(T)))
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(elib_s3_sign)
    end.

%% Garage v2.4.1 (268334bd): signature/payload.rs rejects expired presigned
%% dates as BadRequest; common_error.rs maps that to 400 / InvalidRequest.
%% Require the exact fixed message as well: unrelated 400s and forbidden bad
%% signatures are not expiry evidence. Never return or log the raw message/body.
provider_expiry_reason(400, Body) when is_binary(Body) ->
    case
        {
            provider_error_code(Body),
            binary:match(Body, <<"<Message>Bad request: Date is too old</Message>">>)
        }
    of
        {<<"InvalidRequest">>, {_, _}} -> signed_url_expired;
        _ -> unrecognized
    end;
provider_expiry_reason(_, _) ->
    unrecognized.

provider_expiry_reason_test() ->
    Expired =
        <<"<Error><Code>InvalidRequest</Code>",
            "<Message>Bad request: Date is too old</Message></Error>">>,
    ?assertEqual(signed_url_expired, provider_expiry_reason(400, Expired)),
    ?assertEqual(unrecognized, provider_expiry_reason(403, Expired)),
    ?assertEqual(
        unrecognized,
        provider_expiry_reason(
            400,
            <<"<Error><Code>InvalidRequest</Code><Message>Bad request: Invalid date</Message></Error>">>
        )
    ),
    ?assertEqual(
        unrecognized,
        provider_expiry_reason(
            403,
            <<"<Error><Code>AccessDenied</Code><Message>Forbidden: Invalid signature</Message></Error>">>
        )
    ),
    ?assertEqual(
        unrecognized,
        provider_expiry_reason(
            400,
            <<"<Error><Code>AccessDenied</Code><Message>Bad request: Date is too old</Message></Error>">>
        )
    ),
    ?assertEqual(unrecognized, provider_expiry_reason(400, <<"unclassified">>)).

%% Emit only the provider's symbolic error code, never its URL, headers, body,
%% signature or credentials.
provider_error_code(Body) ->
    case re:run(Body, <<"<Code>([A-Za-z0-9_]{1,64})</Code>">>, [{capture, [1], binary}]) of
        {match, [Code]} -> Code;
        nomatch -> <<"unclassified">>
    end.
