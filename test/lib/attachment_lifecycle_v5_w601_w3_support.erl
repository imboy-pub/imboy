%%% W6-01 D 组（W3-01/W3-02/W3-03，204 合同叶）共享助手。
%%%
%%% 只承载三套件共用的机制，不含任何场景断言：
%%%   * driver 双跑 —— V5_PROBE_DRIVER（默认 garage）决定本遍种子行的
%%%     storage 四元组与叶展开；同一套件被 gate 以两种 env 各跑一遍。
%%%     env 以字符串匹配（os:getenv 返回 list），非法取值 fail-closed。
%%%     registry 恒装双根（disk + garage），种子四元组一律从 run-owned
%%%     registry 解析（attachment_storage:identity），并逐叶以
%%%     frozen_location 证明：正四元组可解析（identity_resolved）、
%%%     换错 driver 字面量被拒（cross_driver_rejected）。
%%%   * 真实对象探针 —— probe_io/2 经 sanctioned driver（garage 用
%%%     W202_GARAGE_* env、disk 用 V5_DISK_ROOT registry 根）真实
%%%     PUT/GET 探针对象，读回字节 sha256 一致；清理删除并容忍已合法删除。
%%%   * emit —— 断言全过后才调用；schema_phase 取合同叶值 integrated；
%%%     ac_id 与 driver 放 measured。
%%%   * 屏障 —— with_row_lock_barrier/4：{advisory, Key} 或
%%%     {row, Sql, Args}（生产入口第一把锁的真实形状）；worker 真实执行
%%%     后以 blocking_proof/3 经 pg_blocking_pids + pg_locks 对账阻塞
%%%     对象（waiter 被 holder 阻塞），禁 sleep 猜并发。
%%%   * 池桥接 —— setup 先 override pg_conf 到步级库再 boot（w404 先例），
%%%     池形态生产入口（attachment_ds / eb_asset_store / elib_pg）真实
%%%     作用于本步库；cleanup 还原。
-module(attachment_lifecycle_v5_w601_w3_support).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([
    driver/0,
    other_driver/1,
    driver_index/1,
    setup/1,
    cleanup/1,
    emit/2,
    seed_identity/3,
    identity_proof/2,
    probe_io/2,
    probe_cleanup_after/0,
    q/2,
    q/3,
    e/2,
    e/3,
    lenient_q/3,
    purge_attachment_metadata/2,
    cleanup_enterprise_asset/2,
    cleanup_enterprise_queue/2,
    await_upload_expiry/2,
    hex64/1,
    loc/1,
    seed_tenants/1,
    registration_fixture/2,
    receipt_fixture/1,
    seed_confirmed/8,
    seed_confirmed_live/8,
    bind_group_file/2,
    unbind_group_file/2,
    seed_plain_row/8,
    seed_eb_row/4,
    install_governance/3,
    install_workspace_hold/2,
    governance_ctx/0,
    clear_intents/3,
    barrier_connect/1,
    with_row_lock_barrier/4,
    blocking_proof/3,
    start_step/1,
    collect_step/2,
    refresh_gc_verified/2,
    run_cleanup/1,
    eb_row_lock/1,
    eb_workspace_lock/2,
    attachment_row_lock/1,
    ear_release_and_purge/2,
    release_workspace_holds/1
]).

-define(NS_DISK, <<"w601-w3-disk">>).
-define(NS_GARAGE, <<"w601-w3-garage">>).
-define(CONTAINER_DISK, <<"w601c">>).

%% ------------------------------------------------------------- driver 双跑

%% 本遍种子 driver（gate 以 V5_PROBE_DRIVER=garage|disk 各跑一遍）。
%% os:getenv 返回 string；按字符串匹配并显式校验取值，非法值 fail-closed。
driver() ->
    case os:getenv("V5_PROBE_DRIVER") of
        false -> <<"garage">>;
        "" -> <<"garage">>;
        "garage" -> <<"garage">>;
        "disk" -> <<"disk">>;
        Other -> erlang:error({no_go, w601_w3_unknown_probe_driver, Other})
    end.

other_driver(<<"garage">>) -> <<"disk">>;
other_driver(<<"disk">>) -> <<"garage">>.

%% id 空间分段：同库双 driver 遍互不撞主键（garage 低半段 / disk 高半段）。
driver_index(<<"garage">>) -> 0;
driver_index(<<"disk">>) -> 10.

%% ------------------------------------------------------------------ setup

setup(Prefix) ->
    Db = inttest_marker_db:provision(#{
        env_prefix => Prefix,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    ok = ensure_loaded(imboy),
    PriorPgConf = application:get_env(imboy, pg_conf),
    ok = override_pg_database(Db),
    PriorRegistry = application:get_env(imboy, attachment_storage_registry),
    PriorGarage = application:get_env(imboy, garage),
    PriorJwt = application:get_env(imboy, jwt_key),
    PriorBaseUrl = application:get_env(imboy, base_url),
    Conn = maps:get(conn, Db),
    Boot = eunit_runner:eunit_setup(),
    try
        Garage = garage_env(),
        ok = application:set_env(imboy, garage, Garage),
        ok = ensure_started(inets),
        ok = ensure_started(ssl),
        ok = ensure_read_env(),
        Base = disk_base(Prefix),
        ok = application:set_env(imboy, attachment_storage_registry, registry(Base, Garage)),
        case attachment_storage:validate_registry() of
            ok -> ok;
            Other -> erlang:error({w601_w3_registry_not_executable, Other})
        end,
        ok = seed_tenants(Conn),
        #{
            boot => Boot,
            db => Db,
            conn => Conn,
            base => Base,
            garage => Garage,
            prior_registry => PriorRegistry,
            prior_garage => PriorGarage,
            prior_pg_conf => PriorPgConf,
            prior_jwt_key => PriorJwt,
            prior_base_url => PriorBaseUrl
        }
    catch
        Class:Reason:Stack ->
            restore_env(PriorRegistry, PriorGarage),
            restore_read_env(PriorJwt, PriorBaseUrl),
            restore_pg_conf(PriorPgConf),
            inttest_marker_db:release(Db),
            eunit_runner:eunit_cleanup(Boot),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(
    #{
        boot := Boot,
        db := Db,
        base := Base,
        prior_registry := PR,
        prior_garage := PG,
        prior_pg_conf := PPC,
        prior_jwt_key := PJ,
        prior_base_url := PB
    }
) ->
    restore_env(PR, PG),
    restore_read_env(PJ, PB),
    restore_pg_conf(PPC),
    _ = file:del_dir_r(Base),
    inttest_marker_db:release(Db),
    eunit_runner:eunit_cleanup(Boot),
    ok.

restore_env({ok, V}, PG) ->
    ok = application:set_env(imboy, attachment_storage_registry, V),
    restore_garage(PG);
restore_env(undefined, PG) ->
    ok = application:unset_env(imboy, attachment_storage_registry),
    restore_garage(PG).

restore_garage({ok, V}) -> application:set_env(imboy, garage, V);
restore_garage(undefined) -> application:unset_env(imboy, garage).

%% 读鉴权（attach_logic:view_url 票据面）自证环境：缺省补测试值（不发网络
%% 请求，仅签名/组 URL），cleanup 还原。
ensure_read_env() ->
    case application:get_env(imboy, jwt_key) of
        {ok, K} when is_binary(K), byte_size(K) > 0 -> ok;
        _ -> ok = application:set_env(imboy, jwt_key, <<"w601w3-view-ticket-key">>)
    end,
    case application:get_env(imboy, base_url) of
        {ok, U} when is_binary(U), byte_size(U) > 0 -> ok;
        _ -> ok = application:set_env(imboy, base_url, <<"http://127.0.0.1:9800">>)
    end,
    ok.

restore_read_env({ok, V}, PB) ->
    ok = application:set_env(imboy, jwt_key, V),
    restore_base_url(PB);
restore_read_env(undefined, PB) ->
    ok = application:unset_env(imboy, jwt_key),
    restore_base_url(PB).

restore_base_url({ok, V}) -> application:set_env(imboy, base_url, V);
restore_base_url(undefined) -> application:unset_env(imboy, base_url).

ensure_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok
    end.

%% 池桥接（w404 bind 先例）：boot 前 override pg_conf 指向步级库，使
%% attachment_ds / eb_asset_store / elib_pg 等池形态生产入口真实作用于
%% 本步库而非 sys config 库。
override_pg_database(Db) ->
    DbName = binary_to_list(maps:get(db, Db)),
    case application:get_env(imboy, pg_conf) of
        {ok, #{start_mfa := {epgsql, connect, [Opts]}} = Pg0} ->
            Pg = Pg0#{start_mfa := {epgsql, connect, [Opts#{database := DbName}]}},
            ok = application:set_env(imboy, pg_conf, Pg),
            ok;
        Other ->
            erlang:error({no_go, w601_w3_pg_conf_shape, Other})
    end.

restore_pg_conf({ok, V}) -> ok = application:set_env(imboy, pg_conf, V);
restore_pg_conf(undefined) -> ok = application:unset_env(imboy, pg_conf).

ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> erlang:error({w601_w3_app_start_failed, App, Reason})
    end.

garage_env() ->
    Keys = [
        "W202_GARAGE_ENDPOINT",
        "W202_GARAGE_BUCKET",
        "W202_GARAGE_ACCESS_KEY",
        "W202_GARAGE_SECRET_KEY"
    ],
    Values = [os:getenv(K) || K <- Keys],
    case lists:all(fun(V) -> is_list(V) andalso V =/= "" end, Values) of
        false ->
            erlang:error({no_go, garage_env_missing, lists:zip(Keys, Values)});
        true ->
            [EP, Bucket, AK, SK] = [list_to_binary(V) || V <- Values],
            #{endpoint => EP, bucket => Bucket, access_key => AK, secret_key => SK}
    end.

%% V5_DISK_ROOT 由 gate 固定为 admission allowlist 内路径；缺省退回临时目录。
disk_base(Prefix) ->
    Parent =
        case os:getenv("V5_DISK_ROOT") of
            false -> temp_parent();
            "" -> temp_parent();
            Dir -> Dir
        end,
    Base = filename:join(
        Parent,
        "w601-w3-" ++ binary_to_list(Prefix) ++ "-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = filelib:ensure_dir(filename:join(Base, "placeholder")),
    ok = file:change_mode(Base, 8#700),
    Base.

temp_parent() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.

%% 生产 attachment_storage:registry/0 只认 version=1 信封（缺 version 的
%% 值一律替换为空 registry → validate_registry 报 empty_roots，见
%% src/lib/attachment_storage/attachment_storage.erl registry/0），故按
%% imboy_env 的构造形状包装（对照 w601_w4_context:registry/2 与
%% w403_storage_tests 的 registry 信封先例）。三套件经
%% attachment_storage:resolve/identity/frozen_location 生产访问器消费，
%% 无裸读 roots 的路径。
registry(Base, Garage) ->
    #{
        version => 1,
        roots => #{
            <<"w601-w3-disk-root">> => #{
                driver => <<"disk">>,
                namespace => ?NS_DISK,
                aliases => [],
                endpoint => list_to_binary("file://" ++ Base),
                container => ?CONTAINER_DISK
            },
            <<"w601-w3-garage-root">> => #{
                driver => <<"garage">>,
                namespace => ?NS_GARAGE,
                aliases => [],
                endpoint => maps:get(endpoint, Garage),
                container => maps:get(bucket, Garage)
            }
        },
        upload_namespaces => #{}
    }.

%% ------------------------------------------------------------ leaf / emit

%% Spec6 = {Case, Entry, Driver, Scenario, Domain, AcId}。断言全过后才 emit。
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

%% ------------------------------------------------------- registry 身份证明

%% 种子四元组从 run-owned registry 解析（冻结身份 = driver+ns+container+digest）。
seed_identity(_S, Driver, Key) ->
    {Ns, Container} =
        case Driver of
            <<"garage">> -> {?NS_GARAGE, garage_bucket()};
            <<"disk">> -> {?NS_DISK, ?CONTAINER_DISK}
        end,
    {ok, #{locator_digest := Digest}} = attachment_storage:identity(Ns, Container, Key),
    #{
        <<"storage_driver">> => Driver,
        <<"storage_namespace">> => Ns,
        <<"storage_container">> => Container,
        <<"storage_locator_sha256">> => Digest
    }.

garage_bucket() ->
    {ok, Garage} = application:get_env(imboy, garage),
    maps:get(bucket, Garage).

%% 每叶身份真实性：正四元组经 frozen_location 解析成功（driver 与 registry
%% 根一致）；同一 ns/container/digest 换错 driver 字面量被拒（location_changed）。
identity_proof(Key, Quad) ->
    {ok, #{driver := D}} = frozen(Key, Quad),
    Wrong = other_driver(maps:get(<<"storage_driver">>, Quad)),
    {error, location_changed} = frozen(Key, Quad#{<<"storage_driver">> => Wrong}),
    #{
        identity_resolved => true,
        cross_driver_rejected => true,
        frozen_driver => D,
        rejected_driver => Wrong
    }.

frozen(Key, Quad) ->
    case attachment_storage:frozen_location(Key, Quad) of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} = Err when Reason =:= location_changed orelse Reason =:= not_found ->
            Err;
        Other ->
            erlang:error({w601_w3_frozen_location_unexpected, Other})
    end.

%% ------------------------------------------------- 真实对象探针（P1-07）

%% 每叶经 sanctioned driver 真实 PUT/GET 探针对象（garage 用 W202_GARAGE_*
%% env，disk 用 V5_DISK_ROOT registry 根），读回字节 sha256 一致；返回
%% measured 证据与幂等清理 fun（删除并容忍已合法删除）。
probe_io(Key, Quad) ->
    Driver = maps:get(<<"storage_driver">>, Quad),
    Ns = maps:get(<<"storage_namespace">>, Quad),
    Container = maps:get(<<"storage_container">>, Quad),
    Mod =
        case attachment_storage:driver_module(Driver) of
            M when is_atom(M) -> M;
            {error, unsupported_driver} ->
                erlang:error({no_go, w601_w3_probe_driver_unsupported, Driver})
        end,
    ProbeKey = <<Key/binary, "/.w601w3-probe">>,
    Body = <<"w601w3-probe:", Key/binary, ":", (hex64(crypto:strong_rand_bytes(16)))/binary>>,
    Digest = hex64(crypto:hash(sha256, Body)),
    %% 两驱动 put/4 均返回 ok（disk:atomic_write/file 写链、garage:put_snapshot）。
    ok = Mod:put(Ns, Container, ProbeKey, Body),
    {ok, Body} = Mod:read(Ns, Container, ProbeKey),
    true = hex64(crypto:hash(sha256, Body)) =:= Digest,
    %% 同一叶可多次 probe_io（hold-all-entrypoints 主/对照资产、enterprise-bind
    %% 双资产叶）：清理 fun 以列表累积，probe_cleanup_after 全部执行——单键
    %% put 会覆盖前者，使主资产探针对象永不删除。
    put(w601w3_probe_cleanup, [
        fun() -> probe_delete(Mod, Ns, Container, ProbeKey) end | probe_cleanups()
    ]),
    %% Both sanctioned drivers complete PUT with ok; readback proves the bytes.
    %% 两个正式驱动 PUT 成功均返回 ok；后续读回及摘要校验真实字节。
    ok = Mod:put(Ns, Container, ProbeKey, Body),
    {ok, Body} = Mod:read(Ns, Container, ProbeKey),
    true = hex64(crypto:hash(sha256, Body)) =:= Digest,
    #{
        probe_sha256 => Digest,
        probe_readback_ok => true
    }.

probe_cleanups() ->
    case get(w601w3_probe_cleanup) of
        undefined -> [];
        Funs when is_list(Funs) -> Funs
    end.

probe_delete(Mod, Ns, Container, ProbeKey) ->
    case Mod:delete(Ns, Container, ProbeKey) of
        {ok, _} -> ok;
        ok -> ok;
        {error, not_found} -> ok;
        Other -> erlang:error({w601_w3_probe_delete_failed, Other})
    end.

%% 失败路径兜底：每叶 after 调用，删除本叶全部探针对象（幂等，未装即跳过）。
probe_cleanup_after() ->
    case erase(w601w3_probe_cleanup) of
        undefined ->
            ok;
        Funs when is_list(Funs) ->
            lists:foreach(fun(Fun) -> Fun() end, Funs),
            ok
    end.

%% ---------------------------------------------------------------- SQL 原语

q(Conn, Sql) -> q(Conn, Sql, []).

q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w601_w3_sql_failed, Reason, Sql})
    end.

%% 受限容错写：仅容忍两类不可销毁残留跳过——
%% 23503 FK RESTRICT（治理行引用的残留行钉住前置 DELETE）、
%% 23514 CHECK 触发（迁移 168 不可变守卫挡住 WAR 行的前置清理），
%% 其余错误照抛——P2 幂等：重跑同库时治理行挡住的前置 DELETE 跳过。
lenient_q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, #error{code = <<"23503">>}} -> ok;
        {error, #error{code = <<"23514">>}} -> ok;
        {error, Reason} -> erlang:error({w601_w3_sql_failed, Reason, Sql})
    end.

%% 幂等清理 attachment 元数据行：fn_v5_metadata_delete_guard（迁移 190）
%% 要求 DELETE 前 lifecycle_state='delete_intent' 且 tombstone 在位，故在
%% 单事务内按守卫协议构造：intent 转移 → tombstone 补齐 → 删除 →（提交后）
%% tombstone 收敛为 done（迁移 179:1026 "v5 job rows are permanent
%% tombstones (no DELETE)"——队列行是永久墓碑，只能按 job guard 的合法
%% UPDATE 路径收敛，同 w601_w4_queueproto:settle_active 先例）。DELETE 被
%% 治理行 FK 钉住（23503）时整体回滚、行保持原状供 seed 的 INSERT ON
%% CONFLICT 复用；其余失败回滚后照抛。
purge_attachment_metadata(Conn, Key) ->
    q(Conn, "BEGIN", []),
    [Intent, Tombstone, Deleted] =
        [epgsql:equery(Conn, Sql, [Key]) || Sql <- purge_sqls()],
    case {Intent, Tombstone, Deleted} of
        {{ok, _}, {ok, _}, {ok, _}} ->
            q(
                Conn,
                "UPDATE attachment_object_deletion SET job_state='done',"
                " completed_at=now(), lease_token=NULL, lease_until=NULL"
                " WHERE path=$1 AND job_state IN ('ready','retry_wait','leased')",
                [Key]
            ),
            q(Conn, "COMMIT", []),
            ok;
        {_, _, {error, #error{code = <<"23503">>}}} ->
            %% 治理行 FK 钉住的不可销毁残留：回滚保持原状，复用既有行
            q(Conn, "ROLLBACK", []),
            ok;
        {_, _, _} ->
            q(Conn, "ROLLBACK", []),
            Failed = [R || {error, _} = R <- [Intent, Tombstone, Deleted]],
            erlang:error({w601_w3_sql_failed, Failed, purge_sqls()})
    end.

purge_sqls() ->
    [
        "UPDATE attachment SET lifecycle_state='delete_intent' WHERE path=$1",
        "INSERT INTO attachment_object_deletion(path,bucket) VALUES ($1,'b')"
        " ON CONFLICT (path) DO NOTHING",
        "DELETE FROM attachment WHERE path=$1"
    ].

cleanup_enterprise_asset(Conn, Id) ->
    cleanup_guarded(Conn, <<"enterprise_asset">>, <<"id">>, Id).
cleanup_enterprise_queue(Conn, Id) ->
    cleanup_guarded(Conn, <<"enterprise_asset_delete_queue">>, <<"asset_id">>, Id).

%% Protected enterprise rows belong to the owned database lifetime.
%% 企业保护行归自有数据库生命周期；校验拒删与行不变，不旁路守卫。
cleanup_guarded(Conn, Table, Column, Value) ->
    Select = <<"SELECT * FROM ", Table/binary, " WHERE ", Column/binary, "=$1">>,
    {ok, _, _} = Before = e(Conn, Select, [Value]),
    Delete = <<"DELETE FROM ", Table/binary, " WHERE ", Column/binary, "=$1">>,
    case e(Conn, Delete, [Value]) of
        {ok, _} ->
            ok;
        {error, Error = #error{}} ->
            case cleanup_guard_matches(Table, Error) of
                true -> ?assertEqual(Before, e(Conn, Select, [Value]));
                false -> error({w601_w3_protected_cleanup_failed, Table, Error})
            end
    end.

cleanup_guard_matches(<<"enterprise_asset_delete_queue">>, #error{
    code = <<"23510">>, message = <<"v5 job rows are permanent tombstones (no DELETE)">>
}) ->
    true;
cleanup_guard_matches(<<"enterprise_asset">>, #error{code = <<"23514">>, extra = Extra}) ->
    proplists:get_value(constraint_name, Extra) =:= <<"trg_enterprise_asset_purge_guard">>;
cleanup_guard_matches(_, _) ->
    false.

e(Conn, Sql) -> e(Conn, Sql, []).

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

assert_live_upload(Conn, Key) ->
    {ok, _, [{true}]} = e(
        Conn,
        "SELECT session_state='confirmed' AND upload_content IS NOT NULL"
        " AND upload_expires_at > clock_timestamp()"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    ok.

await_upload_expiry(Conn, Key) ->
    {ok, _, [_]} = e(
        Conn,
        "SELECT pg_sleep(LEAST(5.0, GREATEST(0.0,"
        " EXTRACT(EPOCH FROM (upload_expires_at - clock_timestamp())))))"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    {ok, _, [{true}]} = e(
        Conn,
        "SELECT session_state='confirmed' AND upload_content IS NOT NULL"
        " AND upload_expires_at <= clock_timestamp()"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    ok.

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

loc(N) -> hex64(crypto:hash(sha256, integer_to_binary(N))).

seed_tenants(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w601w3-u', 'w601w3', '', 1, 0, '127.0.0.1', 'w601w3', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id)"
        " VALUES (9101,'w601w3-org',101) ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES (9101,9102,'w601w3-ws',101,'{}') ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO enterprise_application"
        " (id, organization_id, application_key, name, status)"
        " VALUES (9103, 9101, 'w601w3-9103', 'w601w3 app', 'active')"
        " ON CONFLICT (organization_id, id) DO NOTHING"
    ),
    ok.

registration_fixture(Scope, ScopeRef) ->
    jsone:encode(#{
        <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => <<"101">>},
        <<"declaration">> => #{
            <<"mime">> => <<"image/png">>,
            <<"scope">> => Scope,
            <<"scope_ref">> => ScopeRef
        }
    }).

receipt_fixture(N) ->
    Hex = hex64(crypto:hash(sha256, integer_to_binary(N))),
    [
        "{\"version\":1,\"sha256\":\"",
        Hex,
        "\",\"size_bytes\":\"16\","
        "\"mime\":\"image/png\"}"
    ].

%% confirmed 元数据资产：注册（driver 四元组）+ pin + attachment 行（group_file_id
%% 按场景参数化）+ confirm + 能力窗自然耗尽（3s 真实窗口不缩）。幂等：前置
%% attach_pending/队列 DELETE + attachment 前置 DELETE 容忍治理 FK 残留 +
%% INSERT ON CONFLICT (id) DO NOTHING（重跑复用既有行）。
seed_confirmed(Conn, Key, Id, GfIdOrNull, Quad, Scope, ScopeRef, Cipher) ->
    ok = seed_confirmed_live(Conn, Key, Id, GfIdOrNull, Quad, Scope, ScopeRef, Cipher),
    await_upload_expiry(Conn, Key).

%% Prepare only this independent asset, proving pin/confirm within its real3s window.
%% 仅准备本独立资产，并证明 pin/confirm 在原三秒能力窗内完成。
seed_confirmed_live(Conn, Key, Id, GfIdOrNull, Quad, Scope, ScopeRef, Cipher) ->
    Driver = maps:get(<<"storage_driver">>, Quad),
    Ns = maps:get(<<"storage_namespace">>, Quad),
    Container = maps:get(<<"storage_container">>, Quad),
    Digest = maps:get(<<"storage_locator_sha256">>, Quad),
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    purge_attachment_metadata(Conn, Key),
    q(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b',$2,101,"
        "gen_random_uuid(),$3::jsonb,now() + interval '2 hours',"
        "now() + interval '3 seconds',$4,$5,$6,$7)",
        [
            Key,
            Scope,
            registration_fixture(Scope, ScopeRef),
            Driver,
            Ns,
            Container,
            Digest
        ]
    ),
    q(Conn, "SELECT fn_v5_pin_pending_content($1,$2::jsonb)", [Key, receipt_fixture(Id)]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,$3,$4,101,$5,$6,$7,$8,false,$9,$10,$11)"
        " ON CONFLICT (id) DO NOTHING",
        [
            Id,
            Key,
            Scope,
            ScopeRef,
            Driver,
            Ns,
            Container,
            Digest,
            Cipher,
            GfIdOrNull,
            hex64(crypto:hash(sha256, integer_to_binary(Id)))
        ]
    ),
    q(Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, Id]),
    assert_live_upload(Conn, Key).

%% group_file 绑定行重挂/释放（reference 绑定语义的唯一写面）。
bind_group_file(Conn, GfId) ->
    q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId]),
    q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)"
        " ON CONFLICT (id) DO NOTHING",
        [GfId, integer_to_binary(GfId)]
    ),
    ok.

unbind_group_file(Conn, GfId) ->
    q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId]),
    ok.

%% 裸 attachment 行（shared-owner 注销删除面）：Scope/GcVerified/Hash 显式。
seed_plain_row(Conn, Key, Id, Quad, Scope, ScopeRef, GcVerified, Hash) ->
    Driver = maps:get(<<"storage_driver">>, Quad),
    Ns = maps:get(<<"storage_namespace">>, Quad),
    Container = maps:get(<<"storage_container">>, Quad),
    Digest = maps:get(<<"storage_locator_sha256">>, Quad),
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    purge_attachment_metadata(Conn, Key),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,file_hash256)"
        " VALUES ($1,$2,$3,$4,101,$5,$6,$7,$8,$9,NULL,$10)"
        " ON CONFLICT (id) DO NOTHING",
        [Id, Key, Scope, ScopeRef, Driver, Ns, Container, Digest, GcVerified, Hash]
    ),
    ok.

%% enterprise_asset 行（企业 reference/shared 面）：storage 四元组参数化。
seed_eb_row(Conn, Key, Id, Quad) ->
    Driver = maps:get(<<"storage_driver">>, Quad),
    Ns = maps:get(<<"storage_namespace">>, Quad),
    Container = maps:get(<<"storage_container">>, Quad),
    Digest = maps:get(<<"storage_locator_sha256">>, Quad),
    q(
        Conn,
        "DELETE FROM enterprise_asset_delete_queue WHERE storage_namespace=$1 AND object_key=$2",
        [
            Ns,
            Key
        ]
    ),
    lenient_q(Conn, "DELETE FROM enterprise_asset WHERE id=$1", [Id]),
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,status,"
        "  storage_driver,storage_namespace,storage_container,storage_locator_sha256,"
        "  lifecycle_state,created_at)"
        " VALUES ($1,9101,9102,$2,$3,'text/plain',16,'active',$4,$5,$6,$7,'live',now())"
        " ON CONFLICT (id) DO NOTHING",
        [Id, Key, loc(Id), Driver, Ns, Container, Digest]
    ),
    ok.

%% 同库残留清理（双跑幂等）：intent 前收敛本 key 的 tombstone（迁移 179
%% 永久墓碑不可 DELETE，走 job guard 合法 UPDATE 到 done）与企业队列行。
%% attach_pending/attachment 的前置清理在各 seed 助手内完成。
clear_intents(Conn, Key, Quad) ->
    q(
        Conn,
        "UPDATE attachment_object_deletion SET job_state='done',"
        " completed_at=now(), lease_token=NULL, lease_until=NULL"
        " WHERE path=$1 AND job_state IN ('ready','retry_wait','leased')",
        [Key]
    ),
    case maps:get(<<"storage_namespace">>, Quad, undefined) of
        undefined ->
            ok;
        Ns ->
            q(
                Conn,
                "DELETE FROM enterprise_asset_delete_queue WHERE storage_namespace=$1"
                " AND object_key=$2",
                [Ns, Key]
            )
    end,
    ok.

%% --------------------------------------------------------------- 治理 fixture

governance_ctx() -> #{organization_id => 9101, application_id => 9103}.

%% 治理行安装（幂等 fixture）：
%%   * WAR（workspace_attachment_retention）行不可变（迁移 00000168：UPDATE/
%%     DELETE 23514 触发器 + attachment FK RESTRICT）——重复键 ON CONFLICT
%%     DO NOTHING 复用既有值；不可变行留在步级一次性库中不清理（no-op）。
%%   * EAR（enterprise_attachment_retention）行禁物理删除（00000140/00000220
%%     trg_efapi_attachment_retention_guard：DELETE 一律 23514；purged 为终态、
%%     retention_until 只可延长），清理走合法状态机 ear_release_and_purge
%%     （先一次性释放 hold，再在窗口满足时置 purged）；hold_message/
%%     hold_attachment 的重复键改 DO UPDATE 重新 hold（EAR hold 状态机本身
%%     允许 release/re-hold，生产 release_tx/hold_tx 即此语义），保证重跑时
%%     残留行被重新激活为 held。
install_governance(Conn, AttId, hold_workspace) ->
    war_upsert(Conn, AttId, 'group', <<"66">>),
    fun() -> ok end;
install_governance(Conn, AttId, hold_conversation) ->
    war_upsert(Conn, AttId, 'channel', <<"66">>),
    fun() -> ok end;
install_governance(Conn, AttId, hold_message) ->
    ear_hold(Conn, AttId, <<"w601w3 message hold">>),
    ear_cleanup(Conn, AttId);
install_governance(Conn, AttId, hold_attachment) ->
    ear_hold(Conn, AttId, <<"w601w3 attachment hold">>),
    ear_cleanup(Conn, AttId);
install_governance(Conn, AttId, future_retention) ->
    %% ck_ear_hold_reason：hold_state='none' 时 hold_reason 须为 NULL
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now() + interval '50 years','none',NULL,NULL)"
        " ON CONFLICT (attachment_id) DO NOTHING",
        [AttId]
    ),
    ear_cleanup(Conn, AttId);
install_governance(Conn, AttId, permanent_workspace_retention) ->
    war_upsert(Conn, AttId, 'group', <<"66-permanent">>),
    fun() -> ok end.

war_upsert(Conn, AttId, Scope, ScopeRef) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,$2,$3)"
        " ON CONFLICT (attachment_id) DO NOTHING",
        [AttId, Scope, ScopeRef]
    ),
    ok.

ear_hold(Conn, AttId, Reason) ->
    %% retention_until 取已到期值：hold 生效与窗口无关（intent 拒绝条件是
    %% hold_state='held'，purge 被 hold 先挡——guard 与 fn_v5_intent_ordinary_
    %% delete 的 EAR 子句均如此），到期值使 cleanup 可合法置 purge_state=
    %% 'purged' 终态，重跑时残留行不再阻塞 refresh_gc_verified_tx。
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now() - interval '1 second','held',now(),$2)"
        " ON CONFLICT (attachment_id) DO UPDATE SET hold_state='held',"
        " hold_set_at=now(), hold_reason=$2, updated_at=now()",
        [AttId, Reason]
    ),
    ok.

%% EAR 治理行清理（行禁删，DELETE 一律 23514）：走合法状态机——
%%   1) 一次性释放 hold（held→none，与生产 release_tx 同语义，guard 允许）；
%%   2) 窗口满足（hold 已释放且 now()>=retention_until）时置终态 purged
%%      （guard 强制，purged 不可回退）。
%% 窗口未满（生产入口创建的未来 until 行，如 hold-gc-race hr(3)(5)(6) 的
%% upsert_tx/extend_tx/governance_tx 行）时 purge 被 23514 拒——显式容忍，
%% 残留 (none,live) 行是冻结行为下唯一可达状态；该叶同库重跑的
%% refresh_gc_verified 前提因此失效（已知限制，如实记录，不放宽断言）。
%% 同类限制：WAR 三治理行（hold_workspace/hold_conversation/
%% permanent_workspace_retention）不可删且 FK RESTRICT 钉住 attachment 行，
%% 同库重跑时旧行令 refresh 判 false（机制不同、后果相同）。gate 侧每步
%% 重建 w601 库（w201_db_setup DROP+CREATE；hold/race 共用该授权名、各自
%% 步内新建，race 步的新建同时清掉 hold 步的治理残留），以上仅影响同库
%% 手动重跑。
ear_release_and_purge(Conn, AttId) ->
    q(
        Conn,
        "UPDATE enterprise_attachment_retention SET hold_state='none',"
        " hold_reason=NULL, hold_set_at=NULL, updated_at=now()"
        " WHERE attachment_id=$1 AND hold_state='held'",
        [AttId]
    ),
    case
        epgsql:equery(
            Conn,
            "UPDATE enterprise_attachment_retention SET purge_state='purged',"
            " purged_at=now(), updated_at=now()"
            " WHERE attachment_id=$1 AND purge_state='live'",
            [AttId]
        )
    of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        %% 留存窗口未满：guard 23514（仅未来 until 行可达），非吞错面。
        {error, #error{code = <<"23514">>}} -> ok;
        {error, Reason} -> erlang:error({w601_w3_sql_failed, Reason, ear_purge})
    end.

ear_cleanup(Conn, AttId) ->
    fun() -> ear_release_and_purge(Conn, AttId) end.

%% 企业 workspace hold 行（enterprise_retention_hold，fn_v5_intent_eb_delete
%% 的 active-hold 排除面）。行 append-only（00000117 触发器：DELETE 一律
%% 23514；release 必须一次性写入 released_at+released_by_user_id；已释放行
%% 禁止再修改——un-release 必 23514），故幂等安装不落在确定性 id 上：
%%   * id 每次安装取新鲜值（确定性 id 在重跑时只剩已释放行，无法重新激活）；
%%   * 安装前把 workspace 遗留的 active hold（上次运行崩溃残留）一次性
%%     release（触发器唯一允许的 UPDATE 面），不用 DELETE、不 un-release；
%%   * 清理 fun 只 release 本行（幂等：已释放行被 WHERE released_at IS NULL
%%     排除）。返回 #{id => HoldId, cleanup => Fun}——回执断言需真实 id。
install_workspace_hold(Conn, HintId) ->
    release_workspace_holds(Conn),
    HoldId = fresh_hold_id(HintId),
    q(
        Conn,
        "INSERT INTO enterprise_retention_hold"
        " (id,organization_id,workspace_id,scope_type,reason_code,released_at,actor_user_id,version)"
        " VALUES ($1,9101,9102,'workspace','w601w3 eb hold',NULL,101,1)",
        [HoldId]
    ),
    #{
        id => HoldId,
        cleanup => fun() -> release_hold_by_id(Conn, HoldId) end
    }.

%% workspace 遗留 active hold 一次性 release（00000117 唯一合法 UPDATE 面：
%% 未释放行同时写入 released_at+released_by_user_id）；已释放行不被命中、
%% 不触发 append-only 拒绝。
release_workspace_holds(Conn) ->
    q(
        Conn,
        "UPDATE enterprise_retention_hold SET released_at=now(),"
        " released_by_user_id=101 WHERE organization_id=9101 AND workspace_id=9102"
        " AND released_at IS NULL",
        []
    ),
    ok.

release_hold_by_id(Conn, HoldId) ->
    q(
        Conn,
        "UPDATE enterprise_retention_hold SET released_at=now(),"
        " released_by_user_id=101 WHERE id=$1 AND released_at IS NULL",
        [HoldId]
    ),
    ok.

%% 新鲜 hold id：bigint 高段隔离（叶片 id 空间之外）；叶提示值残段 × 毫秒
%% 时间戳残段组合，跨重跑不撞既有行（同叶同毫秒残段跨两次运行才可能撞，
%% 届时主键冲突显性失败，fail-closed）。上界 9.2e18 < int8 max 9.223e18。
fresh_hold_id(HintId) ->
    9000000000000000000 +
        (HintId rem 50000) * 4000000000000 +
        (erlang:system_time(millisecond) rem 4000000000000).

%% gc_verified 引用零证明（pg_tests:w301_gc_verified_refresh 直调逐字搬）。
refresh_gc_verified(Conn, Key) ->
    {ok, _, [{V}]} = e(
        Conn,
        "WITH u AS (UPDATE attachment a SET gc_verified = c.proven FROM ("
        "  SELECT a2.path, (a2.scope='group' AND a2.group_file_id IS NOT NULL"
        "    AND NOT EXISTS (SELECT 1 FROM group_file gf WHERE gf.id=a2.group_file_id)"
        "    AND a2.cipher IS NULL AND a2.storage_locator_sha256 IS NOT NULL"
        "    AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "      AND uc.attach_file_hash256 = a2.file_hash256 AND uc.status = 1)) AS proven"
        "  FROM attachment a2 WHERE a2.path=$1) c WHERE a.path=c.path RETURNING a.gc_verified)"
        " SELECT gc_verified FROM u",
        [Key]
    ),
    V.

%% ------------------------------------------------- 屏障（advisory/行锁 + 证明）

%% 第二连接（epgsql 命令与连接属主进程绑定，屏障 worker 各自真实执行）。
barrier_connect(#{db := Db}) ->
    Server = maps:get(server, Db),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{
            database => maps:get(db, Db),
            timeout => 10000
        }
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Opts),
    Other.

%% 屏障：Conn 事务内持有 worker 将要竞争的真实锁（生产入口第一把锁的形状），
%% Fun(Pids) 派生真实等待方并以 blocking_proof 对账；8000ms 兜底回滚在调用方
%% receive 超时后经 after 分支落地（fail-closed 不吞错）。
%% LockStmt：{advisory, Key} —— pg_advisory_xact_lock(hashtextextended(Key,168))
%%（fn_v5_intent_ordinary_delete 的对象锁，00000179）；
%% {row, Sql, Args} —— 行锁（attachment/enterprise_asset FOR UPDATE）。
with_row_lock_barrier(Conn, Other, {advisory, Key}, Fun) when is_function(Fun, 1) ->
    begin_barrier(Conn, Other),
    {ok, _, _} = e(Conn, "SELECT pg_advisory_xact_lock(hashtextextended($1,168))", [Key]),
    run_barrier_body(Conn, Fun);
with_row_lock_barrier(Conn, Other, {row, Sql, Args}, Fun) when is_function(Fun, 1) ->
    begin_barrier(Conn, Other),
    {ok, _, _} = e(Conn, Sql, Args),
    run_barrier_body(Conn, Fun).

begin_barrier(Conn, Other) ->
    {ok, _, [{HolderPid}]} = e(Conn, "SELECT pg_backend_pid()", []),
    {ok, _, [{WaiterPid}]} = e(Other, "SELECT pg_backend_pid()", []),
    q(Other, "SET statement_timeout = '10s'"),
    q(Conn, "BEGIN"),
    put(w601w3_barrier_pids, #{holder => HolderPid, waiter => WaiterPid}),
    ok.

run_barrier_body(Conn, Fun) ->
    Pids = erase(w601w3_barrier_pids),
    try
        Fun(Pids)
    after
        rollback_quiet(Conn)
    end.

rollback_quiet(Conn) ->
    case epgsql:equery(Conn, "ROLLBACK", []) of
        {ok, _, _} -> ok;
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% 阻塞对账：轮询 pg_blocking_pids(waiter) 必须包含 holder，并取未授权锁样本
%%（pg_locks）入证据；50ms 步进、Left*50ms 上限，耗尽即 fail-closed 抛错。
blocking_proof(Conn, #{holder := HolderPid, waiter := WaiterPid}, Left) ->
    {ok, _, Rows} = e(
        Conn,
        "SELECT pg_blocking_pids($1),"
        " (SELECT l.locktype FROM pg_locks l WHERE l.pid=$1 AND NOT l.granted"
        "  ORDER BY l.locktype LIMIT 1)",
        [WaiterPid]
    ),
    case Rows of
        [{Blockers, LockType}] when is_list(Blockers) ->
            case lists:member(HolderPid, Blockers) of
                true ->
                    #{
                        blocked_by_holder => true,
                        blocking_locktype => locktype_bin(LockType),
                        holder_pid => HolderPid,
                        waiter_pid => WaiterPid
                    };
                false ->
                    wait_tick(Conn, HolderPid, WaiterPid, Left)
            end;
        _ ->
            wait_tick(Conn, HolderPid, WaiterPid, Left)
    end.

wait_tick(_Conn, HolderPid, WaiterPid, 0) ->
    erlang:error({w601_w3_not_blocked_by_holder, HolderPid, WaiterPid});
wait_tick(Conn, HolderPid, WaiterPid, Left) ->
    timer:sleep(50),
    blocking_proof(Conn, #{holder => HolderPid, waiter => WaiterPid}, Left - 1).

locktype_bin(Bin) when is_binary(Bin) -> Bin;
locktype_bin(null) -> null.

%% 屏障内真实并发步骤：worker 进程执行（epgsql 结果/异常全量回传）。
start_step(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        Result =
            try
                {ok, Fun()}
            catch
                Class:Reason:Stack -> {error, Class, Reason, Stack}
            end,
        Parent ! {w601w3_step, Ref, Result}
    end),
    Ref.

collect_step(Ref, TimeoutMs) ->
    receive
        {w601w3_step, Ref, {ok, Value}} -> Value;
        {w601w3_step, Ref, {error, Class, Reason, Stack}} -> erlang:raise(Class, Reason, Stack)
    after TimeoutMs -> erlang:error({w601_w3_step_timeout, TimeoutMs})
    end.

%% step 返回结构统一裁决：{ok, #{cleanup => Fun | ok}} 的 cleanup 执行
%%（P1-02：rr/so/hhr 步统一 {ok, #{measured, proof, cleanup}}）。
run_cleanup(ok) -> ok;
run_cleanup(Fun) when is_function(Fun, 0) -> Fun().

%% enterprise_asset 行锁（fn_v5_intent_eb_delete 的第一把锁形状）。
eb_row_lock(Id) ->
    {row,
        <<
            "SELECT id FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2"
            " AND id=$3 FOR UPDATE"
        >>,
        [9101, 9102, Id]}.

%% attachment 行锁（fn_v5_intent_ordinary_delete 锁序中 advisory 168 之后的
%% 行锁形状）。
attachment_row_lock(Key) ->
    {row, <<"SELECT id FROM attachment WHERE path=$1 FOR UPDATE">>, [Key]}.

%% enterprise workspace 行锁（fn_v5_lock_eb_upload 的第一把锁，00000196 首语句）：
%% intent(eb) 的锁序为 workspace 行锁 → advisory 169 → identity advisory 1685
%% → 资产行锁。屏障持 workspace 行锁使 waiter 在取得任何锁之前即被阻塞，
%% holder 后续全部取锁（identity advisory 1685 / 资产行）在 waiter 之前完成，
%% 锁序不构成环；若屏障改持资产行锁，holder 的 eb_pg_store:lock_assets_in
%% 会在 identity advisory 1685 上与已持 1685 的 waiter 构成环（40P01）。
eb_workspace_lock(Org, Ws) ->
    {row, <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 FOR UPDATE">>, [Org, Ws]}.
