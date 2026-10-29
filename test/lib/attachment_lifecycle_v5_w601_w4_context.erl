%%% @doc W6-01 E 组（W4-01/W4-02/W4-04 共 450 叶）共享 fixture 上下文。
%%% 队列协议原语（claim/ack/reap/drain）在 w601_w4_queueproto。
%%%
%%% 每套件一个 {setup,...}：独立 marker DB（env_prefix 各异）+ 真实双 driver
%%% registry（disk root 指向 run-owned 目录；garage root 指向 W202_GARAGE_*
%%% 真实桶）。身份三元组 (driver, ns, container, locator) 一律由 registry
%%% 解析（attachment_storage:identity），磁盘/garage 探针字节都是真实写读。
%%%
%%% schema_phase 恒 <<"integrated">>（合同 W6-01/real-pg-garage-disk 全部
%%% 450 叶的叶值）；emit 只在场景断言全过后发生。
-module(attachment_lifecycle_v5_w601_w4_context).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([
    setup/1,
    cleanup/1,
    leaf/2,
    emit/5,
    ns/2,
    container/2,
    triple/3,
    put_probe/4,
    assert_probe_intact/4,
    seed_tenants/1,
    seed_scoped_asset/7,
    seed_group_asset/7,
    seed_pending_session/4,
    seed_orphan_row/5,
    seed_eb_asset_row/7,
    seed_ordinary_job_row/6,
    install_governance/3,
    install_eb_hold/2,
    fixtures/0,
    fixture_index/1,
    await_upload_expiry/2,
    registration_fixture/2,
    receipt_fixture/1,
    barrier_connect/1,
    wait_lock/3,
    token/0,
    unique_key/1,
    hex64/1,
    loc/1,
    leaf_id/1,
    q/2,
    q/3,
    e/2,
    e/3
]).

-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).
-define(XDOM, <<"cross_domain">>).
-define(NS_DISK, <<"w601-w4-disk">>).
-define(NS_GARAGE, <<"w601-w4-garage">>).
-define(CONTAINER_DISK, <<"w601c">>).
-define(PROBES, w601_w4_probes).

%% ---------------------------------------------------------------- setup

setup(Prefix) ->
    Boot = eunit_runner:eunit_setup(),
    Db = inttest_marker_db:provision(#{
        env_prefix => Prefix,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    PriorRegistry = application:get_env(imboy, attachment_storage_registry),
    PriorGarage = application:get_env(imboy, garage),
    Conn = maps:get(conn, Db),
    try
        Garage = garage_env(),
        ok = application:set_env(imboy, garage, Garage),
        ok = ensure_started(inets),
        ok = ensure_started(ssl),
        Base = disk_base(Prefix),
        Registry = registry(Base, Garage),
        ok = application:set_env(imboy, attachment_storage_registry, Registry),
        case attachment_storage:validate_registry() of
            ok -> ok;
            Other -> erlang:error({w601_registry_not_executable, Other})
        end,
        Probes = ets:new(?PROBES, [public, ordered_set]),
        ok = seed_tenants(Conn),
        #{
            boot => Boot,
            db => Db,
            conn => Conn,
            base => Base,
            garage => Garage,
            probes => Probes,
            prior_registry => PriorRegistry,
            prior_garage => PriorGarage
        }
    catch
        Class:Reason:Stack ->
            restore_env(PriorRegistry, PriorGarage),
            inttest_marker_db:release(Db),
            eunit_runner:eunit_cleanup(Boot),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{
    boot := Boot,
    db := Db,
    base := Base,
    probes := Probes,
    prior_registry := PriorRegistry,
    prior_garage := PriorGarage
}) ->
    delete_garage_probes(Probes),
    restore_env(PriorRegistry, PriorGarage),
    _ = file:del_dir_r(Base),
    inttest_marker_db:release(Db),
    eunit_runner:eunit_cleanup(Boot),
    ok.

delete_garage_probes(Probes) ->
    case application:get_env(imboy, garage) of
        {ok, #{bucket := Bucket}} ->
            [
                case attachment_storage_garage:head_bounded(?NS_GARAGE, Bucket, Key) of
                    {ok, _} ->
                        ok = attachment_storage_garage:delete(?NS_GARAGE, Bucket, Key);
                    %% 场景内已被合法删除（如 kill_after_delete 字节消失证明）
                    {error, _} ->
                        ok
                end
             || {_K, Key} <- ets:tab2list(Probes)
            ],
            ok;
        _ ->
            ok
    end.

restore_env({ok, V}, PG) ->
    ok = application:set_env(imboy, attachment_storage_registry, V),
    restore_garage(PG);
restore_env(undefined, PG) ->
    ok = application:unset_env(imboy, attachment_storage_registry),
    restore_garage(PG).

restore_garage({ok, V}) -> application:set_env(imboy, garage, V);
restore_garage(undefined) -> application:unset_env(imboy, garage).

ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> erlang:error({w601_app_start_failed, App, Reason})
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

disk_base(Prefix) ->
    Parent =
        case os:getenv("V5_DISK_ROOT") of
            false -> temp_parent();
            "" -> temp_parent();
            Dir -> Dir
        end,
    Base = filename:join(
        Parent,
        "w601-" ++ binary_to_list(Prefix) ++ "-" ++
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

registry(Base, Garage) ->
    %% w403p-* 两根专供 w403_predecessor_support 的 ns()/container()（intent
    %% 回放复用其封印形态）；其余套件走 w601-w4-* 根。
    #{
        <<"w601-disk-root">> => #{
            driver => <<"disk">>,
            namespace => ?NS_DISK,
            aliases => [],
            endpoint => list_to_binary("file://" ++ Base),
            container => ?CONTAINER_DISK
        },
        <<"w601-garage-root">> => #{
            driver => <<"garage">>,
            namespace => ?NS_GARAGE,
            aliases => [],
            endpoint => maps:get(endpoint, Garage),
            container => maps:get(bucket, Garage)
        },
        <<"w601-w403p-disk-root">> => #{
            driver => <<"disk">>,
            namespace => <<"w403p-disk">>,
            aliases => [],
            endpoint => list_to_binary("file://" ++ Base),
            container => <<"c">>
        },
        <<"w601-w403p-garage-root">> => #{
            driver => <<"garage">>,
            namespace => <<"w403p-garage">>,
            aliases => [],
            endpoint => maps:get(endpoint, Garage),
            container => maps:get(bucket, Garage)
        }
    }.

%% ------------------------------------------------------- identity / probe

ns(_State, <<"disk">>) -> ?NS_DISK;
ns(_State, <<"garage">>) -> ?NS_GARAGE.

container(_State, <<"disk">>) -> ?CONTAINER_DISK;
container(State, <<"garage">>) -> maps:get(bucket, maps:get(garage, State)).

%% registry 解析的冻结身份三元组（locator digest 含 root endpoint/container）。
triple(State, Driver, Key) ->
    Ns = ns(State, Driver),
    Container = container(State, Driver),
    {ok, #{locator_digest := Digest}} =
        attachment_storage:identity(Ns, Container, Key),
    #{
        driver => Driver,
        namespace => Ns,
        container => Container,
        locator => Digest
    }.

%% 真实 driver 写探针字节；garage 探针登记进 ets，cleanup 时 HEAD-仍在 + DELETE。
put_probe(_State, <<"disk">>, Key, Bytes) ->
    ok = attachment_storage_disk:put(?NS_DISK, ?CONTAINER_DISK, Key, Bytes),
    {ok, Bytes} =
        attachment_storage_disk:read(?NS_DISK, ?CONTAINER_DISK, Key),
    ok;
put_probe(State, <<"garage">>, Key, Bytes) ->
    Bucket = container(State, <<"garage">>),
    ok = attachment_storage_garage:put(?NS_GARAGE, Bucket, Key, Bytes),
    {ok, Bytes} = attachment_storage_garage:read(?NS_GARAGE, Bucket, Key),
    true = ets:insert(maps:get(probes, State), {Key, Key}),
    ok.

assert_probe_intact(_State, <<"disk">>, Key, Bytes) ->
    {ok, Bytes} =
        attachment_storage_disk:read(?NS_DISK, ?CONTAINER_DISK, Key),
    {ok, #{size := Size}} =
        attachment_storage_disk:head_bounded(?NS_DISK, ?CONTAINER_DISK, Key),
    Size = byte_size(Bytes),
    ok;
assert_probe_intact(State, <<"garage">>, Key, Bytes) ->
    Bucket = container(State, <<"garage">>),
    {ok, Bytes} = attachment_storage_garage:read(?NS_GARAGE, Bucket, Key),
    {ok, #{size := Size}} =
        attachment_storage_garage:head_bounded(?NS_GARAGE, Bucket, Key),
    Size = byte_size(Bytes),
    ok.

%% ------------------------------------------------------------ leaf/emit

leaf({Case, Entry, Driver, Scenario, Domain} = Spec, Fun) ->
    {
        leaf_desc(Spec),
        {timeout, 240, fun() ->
            Measured = Fun(),
            emit(Case, Entry, Scenario, Domain, Measured#{driver => Driver})
        end}
    }.

leaf_desc({Case, Entry, Driver, Scenario, Domain}) ->
    binary_to_list(
        iolist_to_binary([
            short_case(Case), "/", Entry, "/", Driver, "/", Scenario, "/", Domain
        ])
    ).

short_case(Case) ->
    [_, Short] = binary:split(Case, <<"/">>),
    Short.

emit(Case, Entry, Scenario, Domain, Measured) ->
    attachment_lifecycle_v5_obs:emit(#{
        case_id => Case,
        entrypoint => Entry,
        scenario => Scenario,
        domain => Domain,
        schema_phase => <<"integrated">>,
        measured => Measured
    }).

%% -------------------------------------------------------------- SQL 原语

q(Conn, Sql) -> q(Conn, Sql, []).

q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w601_sql_failed, Reason, Sql})
    end.

e(Conn, Sql) -> e(Conn, Sql, []).

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

token() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

unique_key(Prefix) ->
    <<Prefix/binary, "-", (binary:encode_hex(crypto:strong_rand_bytes(8), lowercase))/binary>>.

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

loc(N) -> hex64(crypto:hash(sha256, integer_to_binary(N))).

%% 组内叶序号（进程字典自增）：同组内每叶 key/id 空间隔离。
leaf_id(Tag) ->
    N =
        case get({w601_leaf_seq, Tag}) of
            undefined -> 1;
            K -> K + 1
        end,
    put({w601_leaf_seq, Tag}, N),
    N.

%% ---------------------------------------------------------------- 种子

seed_tenants(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w601-u', 'w601', '', 1, 0, '127.0.0.1', 'w601', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id) VALUES (9101,'w601-org',101)"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES (9101,9102,'w601-ws',101,'{}') ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO enterprise_application"
        " (id, organization_id, application_key, name, status)"
        " VALUES (9103, 9101, 'w601-9103', 'w601 app', 'active')"
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

%% confirmed 元数据资产（W4-01 hold/delete/race 面）：注册（driver 三元组 +
%% scope）+ pin + attachment 行（同三元组）+ confirm + 能力窗自然耗尽。
%% fn_v5_register_pending 为 12 参（00000191 定稿）：location 四元组
%% (driver, namespace, container, locator) 必须逐位对齐。
seed_scoped_asset(State, Conn, Key, Id, Driver, Scope, ScopeRef) ->
    Triple = triple(State, Driver, Key),
    q(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b',$2,101,"
        "gen_random_uuid(),$3::jsonb,now() + interval '2 hours',"
        "now() + interval '3 seconds',$4,$5,$6,$7)",
        [
            Key,
            Scope,
            registration_fixture(Scope, ScopeRef),
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    q(
        Conn,
        "SELECT fn_v5_pin_pending_content($1,$2::jsonb)",
        [Key, receipt_fixture(Id)]
    ),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256,"
        "referer_time)"
        " VALUES ($1,$2,$3,$4,101,$5,$6,$7,$8,true,NULL,NULL,$9,0)",
        [
            Id,
            Key,
            Scope,
            ScopeRef,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple),
            hex64(crypto:hash(sha256, integer_to_binary(Id)))
        ]
    ),
    q(Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, Id]),
    await_upload_expiry(Conn, Key),
    #{id => Id, key => Key, triple => Triple}.

%% 群文件形态种子（W4-01 reference-gc-race 面）：绑定行按 Bound 保留/释放，
%% 能力窗已过 => gc 资格可证明。
seed_group_asset(State, Conn, Key, Id, GfId, Driver, Bound) ->
    Asset = seed_scoped_asset(State, Conn, Key, Id, Driver, <<"group">>, <<"66">>),
    case Bound of
        true ->
            q(
                Conn,
                "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,"
                "file_type,file_category,file_url,file_size) VALUES"
                " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
                [GfId, integer_to_binary(GfId)]
            );
        false ->
            ok
    end,
    Asset#{gf_id => GfId, bound => Bound}.

%% 过期 pending 会话（W4-01 pending 域 hold 面 + W4-02 恢复面）。
seed_pending_session(State, Conn, Key, Driver) ->
    Triple = triple(State, Driver, Key),
    q(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now(),now() - interval '2 hours',$3,$4,$5,$6)",
        [
            Key,
            jsone:encode(#{
                <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => <<"101">>},
                <<"declaration">> => #{<<"mime">> => <<"image/png">>}
            }),
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    Triple.

%% gc_verified 明文无绑定孤儿。
seed_orphan_row(Conn, Id, Key, Triple, RefererTime) ->
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,group_file_id,file_hash256,referer_time)"
        " VALUES ($1,$2,'private',101,$3,$4,$5,$6,true,NULL,NULL,$7,$8)",
        [
            Id,
            Key,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple),
            hex64(crypto:hash(sha256, Key)),
            RefererTime
        ]
    ),
    ok.

seed_eb_asset_row(Conn, Key, Id, Triple, Status, PendingFlag, Lifecycle) ->
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,pending_object_delete,storage_driver,storage_namespace,"
        "  storage_container,storage_locator_sha256,lifecycle_state,created_at)"
        " VALUES ($1,9101,9102,$2,$3,'text/plain',16,$4,$5,$6,$7,$8,$9,$10,now())",
        [
            Id,
            Key,
            hex64(crypto:hash(sha256, integer_to_binary(Id))),
            Status,
            PendingFlag,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple),
            Lifecycle
        ]
    ),
    ok.

seed_ordinary_job_row(Conn, Key, Triple, Attempts, State, LastError) ->
    q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path,bucket,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,job_state,next_retry_at,attempts,last_error_code)"
        " VALUES ($1,'w601-b',$2,$3,$4,$5,$6,now(),$7,$8)"
        " ON CONFLICT (path) DO UPDATE SET storage_driver=EXCLUDED.storage_driver,"
        " storage_namespace=EXCLUDED.storage_namespace,"
        " storage_container=EXCLUDED.storage_container,"
        " storage_locator_sha256=EXCLUDED.storage_locator_sha256,"
        " job_state=EXCLUDED.job_state, attempts=EXCLUDED.attempts,"
        " next_retry_at=now(), last_error_code=EXCLUDED.last_error_code",
        [
            Key,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple),
            State,
            Attempts,
            LastError
        ]
    ),
    ok.

%% ---------------------------------------------------------------- 治理

fixtures() ->
    [
        hold_workspace,
        hold_conversation,
        hold_message,
        hold_attachment,
        future_retention,
        permanent_workspace_retention
    ].

fixture_index(hold_workspace) -> 1;
fixture_index(hold_conversation) -> 2;
fixture_index(hold_message) -> 3;
fixture_index(hold_attachment) -> 4;
fixture_index(future_retention) -> 5;
fixture_index(permanent_workspace_retention) -> 6.

install_governance(Conn, AttId, hold_workspace) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'group','66')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope='group', scope_ref='66'",
        [AttId]
    ),
    fun() ->
        q(Conn, "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1", [AttId])
    end;
install_governance(Conn, AttId, hold_conversation) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'channel','66')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope='channel', scope_ref='66'",
        [AttId]
    ),
    fun() ->
        q(Conn, "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1", [AttId])
    end;
install_governance(Conn, AttId, hold_message) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now()+interval '30 days','held',now(),'w601 message hold')"
        " ON CONFLICT (attachment_id) DO UPDATE SET hold_state='held',"
        " hold_set_at=now(), hold_reason='w601 message hold'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "UPDATE enterprise_attachment_retention SET hold_state='none',"
            " hold_set_at=NULL, hold_reason=NULL WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, hold_attachment) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now()+interval '30 days','held',now(),'w601 attachment hold')"
        " ON CONFLICT (attachment_id) DO UPDATE SET hold_state='held',"
        " hold_set_at=now(), hold_reason='w601 attachment hold'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "UPDATE enterprise_attachment_retention SET hold_state='none',"
            " hold_set_at=NULL, hold_reason=NULL WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, future_retention) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now() + interval '50 years','none',NULL,NULL)"
        " ON CONFLICT (attachment_id) DO UPDATE SET retention_until=now()+ interval '50 years'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "UPDATE enterprise_attachment_retention SET retention_until=now()"
            " WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, permanent_workspace_retention) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'group','66-permanent')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope='group', scope_ref='66-permanent'",
        [AttId]
    ),
    fun() ->
        q(Conn, "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1", [AttId])
    end.

%% 企业 workspace hold（append-only：清理走 release 一次性写入）。
install_eb_hold(Conn, HoldId) ->
    q(
        Conn,
        "INSERT INTO enterprise_retention_hold"
        " (id,organization_id,workspace_id,scope_type,reason_code,released_at,actor_user_id,version)"
        " VALUES (" ++ integer_to_list(HoldId) ++
            ",9101,9102,'workspace','w601 eb hold',NULL,101,1)"
    ),
    fun() ->
        q(
            Conn,
            "UPDATE enterprise_retention_hold SET released_at=now(),"
            " released_by_user_id=101 WHERE id=" ++ integer_to_list(HoldId)
        )
    end.

await_upload_expiry(Conn, Key) ->
    {ok, _, [{true}]} = e(
        Conn,
        "SELECT session_state='confirmed' AND upload_content IS NOT NULL"
        " AND upload_expires_at > clock_timestamp()"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    {ok, _, [_]} = e(
        Conn,
        "SELECT pg_sleep(LEAST(5.0, GREATEST(0.0,"
        " EXTRACT(EPOCH FROM (upload_expires_at - clock_timestamp())))))"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    {ok, _, [{true}]} = e(
        Conn,
        "SELECT upload_expires_at <= clock_timestamp()"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    ok.

%% ------------------------------------------------------- 屏障/锁竞争

barrier_connect(State) ->
    DbMap = maps:get(db, State),
    Server = maps:get(server, DbMap),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{
            database => maps:get(db, DbMap),
            timeout => 10000
        }
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Opts),
    Other.

wait_lock(_Conn, _Pid, 0) ->
    erlang:error(intent_did_not_wait_for_lock);
wait_lock(Conn, Pid, Left) ->
    {ok, _, [{Waiting}]} = e(
        Conn,
        "SELECT EXISTS(SELECT 1 FROM pg_stat_activity"
        " WHERE pid=$1 AND wait_event_type='Lock')",
        [Pid]
    ),
    case Waiting of
        true ->
            ok;
        false ->
            timer:sleep(50),
            wait_lock(Conn, Pid, Left - 1)
    end.
