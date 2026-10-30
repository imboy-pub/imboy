%%% W6-01 C 组（W2-01/W2-02/W2-03，262 合同叶）共享助手。
%%%
%%% 只承载三套件共用的机制，不含任何场景断言：
%%%   * emit/2 —— 逐叶观测（schema_phase 取合同叶值 integrated；
%%%     ac_id 放 measured，pending-multipart 的 AC01/AC02 双验收叶
%%%     复用同一次真实执行，先例为既有 w2_01_pending_multipart adapter）；
%%%   * barrier 助手 —— 只暂停本 leaf 唯一 key 上的真实操作
%%%     （driver put_snapshot / elib_pg SQL 匹配），认证、持久化、
%%%     签名与 provider 响应始终是生产代码；
%%%   * SQL 直通与注入点 —— 失败注入遵循 W4-03 先例：meck 单点返回
%%%     DB 错误/异常，其余链路（HTTP→handler→logic→repo→fn_v5_*）
%%%     全部真实。
-module(attachment_lifecycle_v5_w601_w2_support).

-include_lib("eunit/include/eunit.hrl").

-export([
    emit/2,
    surface_domain/1,
    driver_mod/1,
    put_calls/1,
    sha/1,
    with_put_barrier/2,
    with_read_barrier/2,
    with_sql_barrier/2,
    fail_next_query/3,
    start_job/1,
    finish_job/1,
    await_barrier/1,
    release/2,
    cleanup_jobs/0,
    pending_row/1,
    eb_row/3,
    eb_row/4,
    row_content/1,
    content_pin/1,
    content_pin_eb/3,
    register_count/1,
    attach_count/1,
    eb_asset_status/3,
    update_pending_expiry/2,
    update_pending_deadline/2,
    update_eb_expiry/4,
    update_eb_deadline/4,
    register_expired_pending/2,
    content_pin_any/2,
    expired_pending_copy/1,
    unregister_meck/1
]).

-define(BARRIER_TAG, w601_barrier).

%% ------------------------------------------------------------------ emit

%% 逐叶观测：断言全部通过后才调用（emit 前置断言，不先 emit 后断言）。
emit({Case, Entry, Driver, Scenario, Domain, Ac}, Measured) ->
    attachment_lifecycle_v5_obs:emit(#{
        case_id => Case,
        entrypoint => Entry,
        scenario => Scenario,
        domain => Domain,
        schema_phase => <<"integrated">>,
        measured => Measured#{driver => Driver, ac_id => Ac}
    }),
    ok.

%% 契约域映射：ordinary/enterprise_internal 面 = ordinary 域；
%% enterprise_seat/cs_widget 面 = enterprise 域。
surface_domain(ordinary) -> <<"ordinary">>;
surface_domain(enterprise_internal) -> <<"ordinary">>;
surface_domain(enterprise_seat) -> <<"enterprise">>;
surface_domain(cs_widget) -> <<"enterprise">>.

driver_mod(disk) -> attachment_storage_disk;
driver_mod(garage) -> attachment_storage_garage;
driver_mod(<<"disk">>) -> attachment_storage_disk;
driver_mod(<<"garage">>) -> attachment_storage_garage.

put_calls(Mod) -> meck:num_calls(Mod, put_snapshot, '_').

sha(Bytes) -> binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).

%% ------------------------------------------------------- barrier helpers

%% 暂停本 key 的真实 provider PUT（生产函数 passthrough）。
with_put_barrier(T, Fun) ->
    Mod = driver_mod(maps:get(driver, T)),
    Key = maps:get(key, T),
    {Parent, Barrier, Gate} = barrier_ctx(),
    meck:new(Mod, [passthrough, no_link]),
    try
        meck:expect(Mod, put_snapshot, fun(I, A, B, C) ->
            maybe_pause(I, Key, Parent, Barrier, Gate),
            meck:passthrough([I, A, B, C])
        end),
        Fun(Barrier)
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(Mod)
    end.

%% 暂停匹配 SQL 片段的第一条真实查询（elib_pg:query/2 与 /3；以及
%% elib_pg:execute/2——eb 面写路径经 eb_pg_exec:execute_returning 落在
%% execute/2（eb_pg_exec.erl:64），只 mock query 会漏拦 renew 等 UPDATE）。
with_sql_barrier(Fragments, Fun) ->
    {Parent, Barrier, Gate} = barrier_ctx(),
    meck:new(elib_pg, [passthrough, no_link]),
    try
        meck:expect(elib_pg, query, fun(Sql, Args) ->
            sql_pause(Sql, Fragments, Parent, Barrier, Gate),
            meck:passthrough([Sql, Args])
        end),
        meck:expect(elib_pg, query, fun(Conn, Sql, Args) ->
            sql_pause(Sql, Fragments, Parent, Barrier, Gate),
            meck:passthrough([Conn, Sql, Args])
        end),
        meck:expect(elib_pg, execute, fun(Sql, Args) ->
            sql_pause(Sql, Fragments, Parent, Barrier, Gate),
            meck:passthrough([Sql, Args])
        end),
        Fun(Barrier)
    after
        release_all(Barrier, Gate),
        cleanup_jobs(),
        meck:unload(elib_pg)
    end.

%% 单点失败注入：匹配 SQL 片段的第一条查询返回 {error, Reason}（或 raise），
%% 之后 passthrough。用于 DB 故障类场景（COMMIT 未知 / insert 错误 / 断连）。
fail_next_query(Fragment, Inject, Fun) ->
    Owner = self(),
    meck:new(elib_pg, [passthrough, no_link]),
    try
        Expect = fun(Sql, Args) ->
            Injected = inject_match(Sql, Fragment, Inject, Owner),
            case Injected of
                skip -> meck:passthrough([Sql, Args]);
                {raise, Class, Reason} -> erlang:raise(Class, Reason, [w601_injected]);
                {error, _} = E -> E
            end
        end,
        ok = meck:expect(elib_pg, query, Expect),
        ok = meck:expect(elib_pg, query, fun(Conn, Sql, Args) ->
            Injected = inject_match(Sql, Fragment, Inject, Owner),
            case Injected of
                skip -> meck:passthrough([Conn, Sql, Args]);
                {raise, Class, Reason} -> erlang:raise(Class, Reason, [w601_injected]);
                {error, _} = E -> E
            end
        end),
        Fun()
    after
        meck:unload(elib_pg)
    end.

inject_match(Sql0, Fragment, Inject, _Owner) ->
    Sql = iolist_to_binary(Sql0),
    case binary:match(Sql, Fragment) =/= nomatch of
        true ->
            %% fragment 命中即注入；「仅一次」由调用方的 unmock 边界保证
            %% （每次注入只包一层 try/after）。
            Inject;
        false ->
            skip
    end.

barrier_ctx() ->
    {self(), make_ref(), ets:new(w601_barrier_gate, [public, set])}.

maybe_pause(I, Key, Parent, Barrier, Gate) ->
    case maps:get(key, I) =:= Key of
        true -> pause_once(Parent, Barrier, Gate);
        false -> ok
    end.

sql_pause(Sql0, Fragments, Parent, Barrier, Gate) ->
    Sql = iolist_to_binary(Sql0),
    Hit = lists:any(fun(F) -> binary:match(Sql, F) =/= nomatch end, Fragments),
    case Hit of
        true -> pause_once(Parent, Barrier, Gate);
        false -> ok
    end.

pause_once(Parent, Barrier, Gate) ->
    Entered =
        try
            ets:insert_new(Gate, {worker, self()})
        catch
            error:badarg -> false
        end,
    case Entered of
        false ->
            ok;
        true ->
            Parent ! {?BARRIER_TAG, Barrier, self()},
            receive
                {w601_continue, Barrier} -> ok
            after 30000 -> erlang:error(w601_barrier_timeout)
            end
    end.

await_barrier(Barrier) ->
    receive
        {?BARRIER_TAG, Barrier, Pid} -> Pid
    after 10000 -> erlang:error(w601_barrier_not_reached)
    end.

release(Barrier, Pid) ->
    Pid ! {w601_continue, Barrier},
    ok.

release_all(Barrier, Gate) ->
    [release(Barrier, Pid) || {worker, Pid} <- ets:tab2list(Gate)],
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
            Parent ! {w601_result, Ref, Result}
        end,
        [link, monitor]
    ),
    Job = {Pid, Monitor, Ref},
    erlang:put(w601_jobs, [Job | jobs()]),
    Job.

finish_job({Pid, Monitor, Ref} = Job) ->
    try
        receive
            {w601_result, Ref, {ok, Value}} ->
                erlang:demonitor(Monitor, [flush]),
                Value;
            {w601_result, Ref, {error, Class, Reason, Stack}} ->
                erlang:demonitor(Monitor, [flush]),
                erlang:raise(Class, Reason, Stack);
            {'DOWN', Monitor, process, Pid, Reason} ->
                erlang:error({w601_worker_exit, Reason})
        after 130000 ->
            unlink(Pid),
            exit(Pid, kill),
            erlang:error(w601_worker_timeout)
        end
    after
        erlang:put(w601_jobs, lists:delete(Job, jobs()))
    end.

jobs() ->
    case erlang:get(w601_jobs) of
        L when is_list(L) -> L;
        _ -> []
    end.

cleanup_jobs() ->
    [
        try
            finish_job(Job)
        catch
            _:_ -> ok
        end
     || Job <- jobs()
    ],
    erlang:erase(w601_jobs),
    ok.

unregister_meck(Mod) ->
    try
        meck:unload(Mod)
    catch
        _:_ -> ok
    end,
    ok.

%% 暂停本 key 的真实对象读（confirm 的 GET 字节裁决点）。
with_read_barrier(T, Fun) ->
    Mod = driver_mod(maps:get(driver, T)),
    Key = maps:get(key, T),
    {Parent, Barrier, Gate} = barrier_ctx(),
    meck:new(Mod, [passthrough, no_link]),
    try
        meck:expect(Mod, read_snapshot, fun(I) ->
            Result = meck:passthrough([I]),
            case maps:get(key, I) =:= Key of
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

attach_count(Key) ->
    {ok, [#{<<"n">> := N}]} =
        elib_pg:query(
            <<"SELECT count(*)::bigint AS n FROM attachment WHERE path=$1">>, [Key]
        ),
    N.

eb_asset_status(Org, Ws, AssetId) ->
    case eb_row(Org, Ws, AssetId) of
        {ok, #{<<"status">> := St}} -> St;
        {error, not_found} -> <<"absent">>
    end.

%% ------------------------------------------------------------- SQL helpers

-define(PENDING_TABLE, <<"attach_pending">>).

pending_row(Key) ->
    {ok, Rows} =
        elib_pg:query(
            <<
                "SELECT object_key, session_state, client_upload_id, creator_user_id, upload_content,"
                " upload_registration, storage_driver, storage_namespace, storage_container,"
                " storage_locator_sha256, extract(epoch from upload_expires_at)::bigint AS expires_at,"
                " extract(epoch from session_deadline)::bigint AS deadline"
                " FROM attach_pending WHERE object_key=$1 LIMIT 1"
            >>,
            [Key]
        ),
    case Rows of
        [Row] -> {ok, decode(Row)};
        [] -> {error, not_found}
    end.

eb_row(Org, Ws, AssetId) ->
    {ok, Rows} =
        elib_pg:query(
            <<
                "SELECT id, object_key, status, client_upload_id, upload_content, upload_registration,"
                " storage_driver, storage_namespace, storage_container, storage_locator_sha256,"
                " extract(epoch from upload_expires_at)::bigint AS expires_at,"
                " extract(epoch from session_deadline)::bigint AS deadline"
                " FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, AssetId]
        ),
    case Rows of
        [Row] -> {ok, decode(Row)};
        [] -> {error, not_found}
    end.

eb_row(Org, Ws, AssetId, _Extra) -> eb_row(Org, Ws, AssetId).

decode(Row) ->
    maps:map(
        fun
            (<<"upload_content">>, V) when is_binary(V) -> jsone:decode(V);
            (<<"upload_registration">>, V) when is_binary(V) -> jsone:decode(V);
            (_, V) -> V
        end,
        Row
    ).

row_content(#{<<"upload_content">> := C}) when is_map(C) -> C;
row_content(_) -> #{}.

%% pending 行的 pin 快照（无 pin 返回 not_pinned）。
content_pin(Key) ->
    case pending_row(Key) of
        {ok, Row} -> pin_or(row_content(Row));
        {error, _} = E -> E
    end.

content_pin_eb(Org, Ws, AssetId) ->
    case eb_row(Org, Ws, AssetId) of
        {ok, Row} -> pin_or(row_content(Row));
        {error, _} = E -> E
    end.

pin_or(#{<<"sha256">> := _} = Pin) -> {ok, Pin};
pin_or(_) -> {error, not_pinned}.

register_count(Fragment) ->
    {ok, [#{<<"n">> := N}]} =
        elib_pg:query(
            <<"SELECT count(*)::bigint AS n FROM attach_pending WHERE object_key LIKE $1">>,
            [iolist_to_binary([<<"%">>, Fragment, <<"%">>])]
        ),
    N.

%% 测试夹具专用 SQL：仅缩短会话自身的 expiry（登记时冻结的 session_deadline
%% 不改；deadline 过期场景用专用 update_pending_deadline 显式登记已到期会话）。
update_pending_expiry(Key, SecsFromNow) ->
    {ok, _} =
        elib_pg:query(
            <<
                "UPDATE attach_pending SET upload_expires_at = clock_timestamp() + ($2 * INTERVAL '1 second')"
                " WHERE object_key=$1"
            >>,
            [Key, SecsFromNow]
        ),
    ok.

update_pending_deadline(Key, SecsFromNow) ->
    {ok, _} =
        elib_pg:query(
            <<
                "UPDATE attach_pending SET session_deadline = clock_timestamp() + ($2 * INTERVAL '1 second'),"
                " upload_expires_at = clock_timestamp() + ($2 * INTERVAL '1 second')"
                " WHERE object_key=$1"
            >>,
            [Key, SecsFromNow]
        ),
    ok.

update_eb_expiry(Org, Ws, AssetId, SecsFromNow) ->
    {ok, _} =
        elib_pg:query(
            <<
                "UPDATE enterprise_asset SET upload_expires_at = clock_timestamp() + ($4 * INTERVAL '1 second')"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, AssetId, SecsFromNow]
        ),
    ok.

update_eb_deadline(Org, Ws, AssetId, SecsFromNow) ->
    {ok, _} =
        elib_pg:query(
            <<
                "UPDATE enterprise_asset SET session_deadline = statement_timestamp() + ($4 * INTERVAL '1 second'),"
                " upload_expires_at = statement_timestamp() + ($4 * INTERVAL '1 second')"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, AssetId, SecsFromNow]
        ),
    ok.

%% ------------------------------------------------ 过期前提的合法构造（F8）

%% 显式登记已到期会话：经生产登记函数 fn_v5_register_pending 以原登记行的
%% 冻结快照（bucket/scope/creator/registration/location 逐字复制）造
%% "新 key + 新 cid" 的已到期行，返回 {ok, NewKey, NewCid}。UPDATE 路径被
%% fn_v5_pending_session_guard 冻结语义拒绝（23510：v5 upload expiry cannot
%% shrink / v5 session deadline immutable，迁移 00000190 激活）；登记路径无
%% "不得过去"下界（pg fn 源码 :12-14 仅要求非空且 expiry<=deadline，
%% 2026-10-30 在 run 的 slot1_w102_expand 实库实证过期登记成功）。
register_expired_pending(Key, Mode) ->
    {ok, Row} = pending_row(Key),
    {ok, [#{<<"bucket">> := Bucket, <<"scope">> := Scope, <<"creator">> := Creator}]} =
        elib_pg:query(
            <<
                "SELECT bucket, scope, creator_user_id AS creator"
                " FROM attach_pending WHERE object_key=$1"
            >>,
            [Key]
        ),
    #{
        <<"storage_driver">> := Drv,
        <<"storage_namespace">> := Ns,
        <<"storage_container">> := Ctr,
        <<"storage_locator_sha256">> := Loc,
        <<"upload_registration">> := Registration
    } = Row,
    Cid2 = elib_uuid:gen_v7(),
    Key2 = iolist_to_binary([Key, <<"-expired-">>, elib_uuid:gen_v7()]),
    Now = os:system_time(second),
    {Deadline, Expiry} =
        case Mode of
            %% 能力过期（session 未到期）→ renew 同 key 重签语义的前提
            expiry -> {Now + 3600, Now - 60};
            %% 绝对 deadline 过期 → renew fail-closed 语义的前提
            deadline -> {Now - 300, Now - 301}
        end,
    {ok, [_]} =
        elib_pg:query(
            <<
                "SELECT out_object_key FROM fn_v5_register_pending("
                "$1,$2,$3,$4,$5::uuid,$6::jsonb,"
                "$7::timestamptz,$8::timestamptz,$9,$10,$11,$12)"
            >>,
            [
                Key2,
                Bucket,
                Scope,
                Creator,
                Cid2,
                jsone:encode(Registration),
                iso8601_ts(Deadline),
                iso8601_ts(Expiry),
                Drv,
                Ns,
                Ctr,
                Loc
            ]
        ),
    {ok, Key2, Cid2}.

iso8601_ts(Secs) ->
    iolist_to_binary(
        calendar:system_time_to_rfc3339(Secs, [{unit, second}, {offset, "Z"}])
    ).

%% pin 快照按面取证：ordinary/internal 在 attach_pending.upload_content；
%% seat/widget（eb 面）在 enterprise_asset.upload_content。
content_pin_any(Ep, T) when Ep =:= ordinary; Ep =:= enterprise_internal ->
    content_pin(maps:get(key, T));
content_pin_any(_ProxyEp, T) ->
    org_ws_of(T, fun(Org, Ws, AssetId) -> content_pin_eb(Org, Ws, AssetId) end).

org_ws_of(T, Fun) ->
    S = maps:get(fixture, T),
    Scope = maps:get(scope, S),
    AssetId =
        case maps:get(asset_id, maps:get(confirm_body, T), undefined) of
            I when is_integer(I) -> I;
            B when is_binary(B) -> binary_to_integer(B);
            undefined -> int_of(maps:get(<<"asset_id">>, maps:get(registration, T)))
        end,
    Fun(maps:get(org_id, Scope), maps:get(workspace_id, Scope), AssetId).

int_of(I) when is_integer(I) -> I;
int_of(B) when is_binary(B) -> binary_to_integer(B).

%% Build a separate, already-expired fixture from a real registered/pinned PUT.
%% INSERT still runs every production guard; never shrink an immutable session.
%% 从真实登记、已 pin 的 PUT 建立独立已过期夹具；INSERT 保留全部生产守卫，
%% 不缩短既有不可变会话。新对象也登记到拥有者的清理账本。
expired_pending_copy(T) ->
    Key = maps:get(key, T),
    NewKey = <<Key/binary, "-expired-", (elib_uuid:gen_v7())/binary>>,
    Cid = elib_uuid:gen_v7(),
    {ok, Bytes} = attachment_w403_http_fixture:read(T),
    {ok, 1} = elib_pg:execute(<<
        "INSERT INTO attach_pending SELECT (jsonb_populate_record(NULL::attach_pending, "
        "to_jsonb(p) || jsonb_build_object('object_key',$2::text, "
        "'client_upload_id',$3::uuid,'upload_expires_at',"
        "statement_timestamp()-interval '60 seconds'))).* "
        "FROM attach_pending p WHERE object_key=$1 AND session_state='registered'"
    >>, [Key, NewKey, Cid]),
    I = (maps:get(location, T))#{key => NewKey},
    Mod = driver_mod(maps:get(driver, T)),
    ok = Mod:put(maps:get(namespace, I), maps:get(container, I), NewKey, Bytes),
    true = ets:insert(attachment_w403_http_evidence, {{object, NewKey}, I}),
    Body = (maps:get(confirm_body, T))#{object_key => NewKey},
    T#{key => NewKey, client_upload_id => Cid, location => I, confirm_body => Body}.
