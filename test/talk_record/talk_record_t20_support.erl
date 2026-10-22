%%% @doc Case-isolated T20 support. Controller preclones an empty formal schema.
%%% No setup/seed reset, database DDL, default target or shared Disk fallback.
%%% Asset shortcut helpers below remain legacy component fixtures; they do not
%%% prove V5 upload, lawful queue exhaustion or physical deletion acceptance.
-module(talk_record_t20_support).

-export([
    setup_all/0,
    setup_all/1,
    teardown_all/1,
    seed/1,
    ids/0,
    sql/2,
    one/2,
    sha/1,
    conn_of/1,
    with_env/3,
    %% 附件世界
    insert_attachment/3,
    bind_ref/4,
    collect_hash/3,
    hold_row/2,
    queue_exhaust_row/2,
    queue_done_row/2,
    attachment_state/2,
    ref_state/2,
    cleanup_status/1,
    purge_jobs/1,
    fresh_conn/0,
    raw_tx/2,
    disk_registry_apply/1,
    disk_root/1,
    disk_locator_digest/0
]).

-define(JWT_KEY, <<"t20_playback_signing_key_0123456789abcdef">>).
-define(DS1, <<"10000000-0000-0000-0000-000000000001">>).

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

-spec setup_all() -> no_return().
setup_all() -> error(t20_explicit_case_key_required).

-spec setup_all(binary() | {atom(), atom()}) -> map().
setup_all(Key) ->
    undefined = whereis(pgsql),
    undefined = whereis(pooler_pgsql_pool_sup),
    undefined = whereis(elib_tsid_meck),
    Context = talk_record_t20_case_registry:activate(Key),
    Ref = make_ref(),
    put({?MODULE, Ref}, #{environment => []}),
    try
        capture_environment(Ref, fun() -> configure_connection(Context) end, [], os_names()),
        setup_case(Ref, Context)
    catch
        Class:Reason:Stack ->
            try
                cleanup_resources(get({?MODULE, Ref}))
            after
                talk_record_t20_case_registry:clear()
            end,
            erlang:raise(Class, Reason, Stack)
    after
        erase({?MODULE, Ref})
    end.

setup_case(Ref, Context) ->
    _ = application:load(imboy),
    capture_environment(
        Ref,
        fun() ->
            application:set_env(imboy, sql_driver, pgsql),
            application:set_env(
                imboy,
                talk_record_cursor_signing_key,
                <<"t20_cursor_signing_key_0123456789abcdef">>
            )
        end,
        [sql_driver, talk_record_cursor_signing_key],
        []
    ),
    Conn = acquire_connections(Ref),
    ok = talk_record_t20_case_registry:verify_empty(Conn),
    capture_environment(
        Ref,
        fun() -> talk_record_t20_case_registry:configure_storage() end,
        [attachment_storage_registry],
        ["TALK_UPLOAD_DISK_ROOT"]
    ),
    capture_environment(
        Ref,
        fun() -> talk_record_t12_support:setup_audio_fixture() end,
        [talk_record_t12_audio_bytes, talk_record_render_tmp_dir],
        ["TMPDIR"]
    ),
    capture_environment(
        Ref,
        fun() -> application:set_env(imboy, jwt_key, ?JWT_KEY) end,
        [jwt_key],
        []
    ),
    #{conn => Conn, t20_context => Context, t20_resources => get({?MODULE, Ref})}.

acquire_connections(Ref) ->
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    undefined = whereis(pgsql),
    undefined = whereis(pooler_pgsql_pool_sup),
    {ok, PoolSupervisor} = pooler:new_pool(#{
        name => pgsql,
        max_count => 4,
        init_count => 1,
        start_mfa => {epgsql, connect, [talk_record_t20_case_registry:connection_options()]}
    }),
    PoolPid = whereis(pgsql),
    track(Ref, pool, #{supervisor => PoolSupervisor, process => PoolPid}),
    true = is_pid(PoolPid),
    ok = meck:new(elib_tsid, [passthrough, unstick, no_link]),
    track(Ref, mock, whereis(elib_tsid_meck)),
    ok = meck:expect(elib_tsid, generate, fun(_) ->
        9000000000000 + erlang:unique_integer([positive, monotonic])
    end),
    {ok, Conn} = epgsql:connect(talk_record_t20_case_registry:connection_options()),
    track(Ref, conn, Conn),
    Conn.

configure_connection(#{entry := Case}) ->
    Opts = talk_record_t20_case_registry:connection_options(),
    Db = binary_to_list(maps:get(<<"database">>, Case)),
    true = os:putenv("T20_PG_DB", Db),
    lists:foreach(
        fun({Name, Value}) -> true = os:putenv(Name, Value) end,
        [
            {"T05_PG_HOST", maps:get(host, Opts)},
            {"T05_PG_PORT", integer_to_list(maps:get(port, Opts))},
            {"T05_PG_USER", binary_to_list(maps:get(username, Opts))},
            {"T05_PG_PASSWORD", maps:get(password, Opts)},
            {"T05_PG_DB", Db}
        ]
    ),
    ok.

-spec teardown_all(map()) -> ok.
teardown_all(#{t20_resources := Resources}) ->
    try
        talk_record_t20_case_registry:recheck()
    after
        try
            cleanup_resources(Resources)
        after
            talk_record_t20_case_registry:clear()
        end
    end.

track(Ref, Name, Value) ->
    Ledger = get({?MODULE, Ref}),
    put({?MODULE, Ref}, Ledger#{Name => Value}),
    ok.

capture_environment(Ref, Fun, Keys, Names) ->
    Before =
        [{app, K, application:get_env(imboy, K)} || K <- Keys] ++
            [{os, N, os:getenv(N)} || N <- Names],
    try
        Fun()
    after
        Changed = [
            {Kind, Name, Previous, current_environment(Kind, Name)}
         || {Kind, Name, Previous} <- Before, Previous =/= current_environment(Kind, Name)
        ],
        Ledger = get({?MODULE, Ref}),
        track(Ref, environment, lists:reverse(Changed) ++ maps:get(environment, Ledger))
    end.

cleanup_resources(Resources) ->
    Results =
        [
            cleanup_connection(maps:get(conn, Resources, undefined)),
            cleanup_process(mock, maps:get(mock, Resources, undefined)),
            cleanup_process(pool, maps:get(pool, Resources, undefined))
        ] ++
            [restore_owned_environment(E) || E <- maps:get(environment, Resources)],
    case [E || E <- Results, E =/= ok] of
        [] -> ok;
        Errors -> error({t20_cleanup_ownership_changed, Errors})
    end.

cleanup_connection(undefined) ->
    ok;
cleanup_connection(Pid) ->
    try
        epgsql:close(Pid),
        ok
    catch
        _:Reason -> {connection_cleanup_failed, Reason}
    end.
cleanup_process(_, undefined) ->
    ok;
cleanup_process(pool, #{supervisor := Supervisor, process := Pid}) ->
    try
        case {whereis(pooler_pgsql_pool_sup), whereis(pgsql)} of
            {Supervisor, Pid} -> pooler:rm_pool(pgsql);
            {Supervisor, undefined} -> pooler:rm_pool(pgsql);
            {undefined, undefined} -> ok;
            Other -> {foreign_pool_preserved, Other}
        end
    catch
        _:Reason -> {cleanup_failed, pool, Reason}
    end;
cleanup_process(mock, Pid) when is_pid(Pid) ->
    case whereis(elib_tsid_meck) of
        Pid ->
            try
                meck:unload(elib_tsid)
            catch
                _:Reason -> {cleanup_failed, mock, Reason}
            end;
        undefined ->
            ok;
        Other ->
            {foreign_process_preserved, elib_tsid_meck, Other}
    end.

restore_owned_environment({Kind, Name, Previous, Owned}) ->
    case current_environment(Kind, Name) of
        Owned ->
            case {Kind, Previous} of
                {app, _} ->
                    restore_env(Name, Previous);
                {os, false} ->
                    os:unsetenv(Name),
                    ok;
                {os, Value} ->
                    os:putenv(Name, Value),
                    ok
            end;
        _ ->
            {foreign_environment_preserved, Kind, Name}
    end.
current_environment(app, Name) -> application:get_env(imboy, Name);
current_environment(os, Name) -> os:getenv(Name).
os_names() ->
    ["T05_PG_HOST", "T05_PG_PORT", "T05_PG_USER", "T05_PG_PASSWORD", "T05_PG_DB", "T20_PG_DB"].

restore_env(Key, undefined) ->
    application:unset_env(imboy, Key);
restore_env(Key, {ok, Value}) ->
    application:set_env(imboy, Key, Value).

%% @doc 临时覆写 app env（测试后恢复）。
-spec with_env(atom(), term(), fun(() -> term())) -> term().
with_env(Key, Value, Fun) ->
    Prev = application:get_env(imboy, Key),
    ok = application:set_env(imboy, Key, Value),
    try
        Fun()
    after
        restore_env(Key, Prev)
    end.

-spec seed(map()) -> ok.
seed(State) ->
    Conn = maps:get(conn, State),
    ok = talk_record_t20_case_registry:verify_empty(Conn),
    raw_tx(Conn, fun(C) -> seed_parents(C) end).

seed_parents(C) ->
    lists:foreach(
        fun(Uid) -> talk_record_parent_fixture:user(C, Uid, #{}) end,
        [700001, 700002, 700003, 700004, 700006, 700007, 700008]
    ),
    ok = talk_record_parent_fixture:organization(C, 710001, <<"t20-org-a">>, 700001, <<"active">>),
    ok = talk_record_parent_fixture:organization(C, 710002, <<"t20-org-b">>, 700004, <<"active">>),
    ok = assert_owner_member(C, 710001, 700001),
    ok = assert_owner_member(C, 710002, 700004),
    lists:foreach(
        fun({Org, User, Role}) -> seed_member(C, Org, User, Role) end,
        [
            {710001, 700002, <<"member">>},
            {710001, 700003, <<"member">>}
        ]
    ),
    lists:foreach(
        fun({Ws, Org, Owner}) ->
            talk_record_parent_fixture:workspace(C, Ws, Org, <<"t20-workspace">>, Owner)
        end,
        [{720001, 710001, 700001}, {720002, 710001, 700001}, {720011, 710002, 700004}]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'member','active')">>,
        [720001, 700001]
    ),
    lists:foreach(
        fun(N) ->
            Org = 711000 + N,
            ok = talk_record_parent_fixture:organization(
                C, Org, <<"t20-page-org">>, 700008, <<"active">>
            ),
            ok = assert_owner_member(C, Org, 700008),
            seed_member(C, Org, 700006, <<"member">>)
        end,
        lists:seq(1, 21)
    ),
    ok.

assert_owner_member(C, Org, User) ->
    %% Formal migrations create the owner's membership through a trigger.
    {ok, [
        #{
            <<"owner_id">> := User,
            <<"role">> := <<"owner">>,
            <<"status">> := <<"active">>
        }
    ]} = elib_pg:query(
        C,
        <<
            "SELECT o.owner_id,m.role,m.status FROM organization o "
            "JOIN organization_member m ON m.organization_id=o.id AND m.user_id=o.owner_id "
            "WHERE o.id=$1 AND o.owner_id=$2"
        >>,
        [Org, User]
    ),
    ok.

seed_member(C, Org, User, Role) ->
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES($1,$2,$3,'active')">>,
        [Org, User, Role]
    ),
    ok.

-spec ids() -> map().
ids() ->
    talk_record_t12_support:ids().

-spec sql(pid() | map(), iolist()) -> ok.
sql(Conn, Sql) ->
    ok = talk_record_t20_case_registry:verify_connected(Conn),
    talk_record_t12_support:sql(Conn, Sql).

-spec one(pid() | map(), iolist()) -> map().
one(Conn, Sql) ->
    talk_record_t12_support:one(Conn, Sql).

-spec sha(integer()) -> binary().
sha(N) ->
    talk_record_t12_support:sha(N).

-spec conn_of(map()) -> pid().
conn_of(State) ->
    maps:get(conn, State).

%%%===================================================================
%%% 附件世界（T06 fixture 形态构造）
%%%===================================================================

%% @doc 插入 talk_record 附件资产（live；storage 四元组可覆写——真
%% disk 驱动用例须与 registry 解析结果一致，T06 I 组同款）。
-spec insert_attachment(pid() | map(), integer(), map()) -> binary().
insert_attachment(Conn, Id, Opts) ->
    ok = talk_record_t20_case_registry:verify_connected(Conn),
    RecordId = maps:get(record_id, Opts),
    Path = maps:get(path, Opts, path_of(Id)),
    Hash = maps:get(hash, Opts, binary:copy(<<"b">>, 64)),
    GcVerified = maps:get(gc_verified, Opts, true),
    Driver = maps:get(storage_driver, Opts, null),
    Ns = maps:get(storage_namespace, Opts, null),
    Container = maps:get(storage_container, Opts, null),
    Locator = maps:get(storage_locator, Opts, null),
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO attachment(id, file_hash256, mime_type, name, path, url, size,"
                " referer_time, creator_user_id, scope, scope_ref, cipher, gc_verified,"
                " storage_driver, storage_namespace, storage_container, storage_locator_sha256)"
                " VALUES ($1,$2,'audio/aac',$3,$4,$4,10,1,1,'talk_record',$5::bigint::text,NULL,$6,"
                " $7,$8,$9,$10)"
            >>,
            [Id, Hash, Path, Path, RecordId, GcVerified, Driver, Ns, Container, Locator]
        ),
    Path.

path_of(Id) ->
    <<"t20/att-", (integer_to_binary(Id))/binary>>.

%% @doc 直接落一条 bound 业务引用（绕开首绑六条件——测试数据装配）。
-spec bind_ref(pid() | map(), integer(), integer(), binary()) -> ok.
bind_ref(Conn, RecordId, AttachmentId, BusinessKey) ->
    ok = talk_record_t20_case_registry:verify_connected(Conn),
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO talk_record_attachment_ref(id, record_id, attachment_id,"
                " role, business_key) VALUES ($1,$2,$3,'audio_part',$4)"
            >>,
            [AttachmentId * 1000 + 1, RecordId, AttachmentId, BusinessKey]
        ),
    ok.

%% @doc 收藏引用（同 hash → intent 23510 → retained_by_other_reference）。
-spec collect_hash(pid() | map(), integer(), binary()) -> ok.
collect_hash(Conn, CollectId, Hash) ->
    ok = talk_record_t20_case_registry:verify_connected(Conn),
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO user_collect(id, user_id, attach_file_hash256, status)"
                " VALUES ($1, 700002, $2, 1)"
            >>,
            [CollectId, Hash]
        ),
    ok.

%% @doc EAR hold 行（defense-in-depth：evaluate_release_blockers held 分支）。
%% 需 enterprise_application 存量（复合 FK）；CHECK 要求 held 行带
%% hold_reason + hold_set_at。
-spec hold_row(pid() | map(), integer()) -> ok.
hold_row(Conn, AttachmentId) ->
    ok = talk_record_t20_case_registry:verify_connected(Conn),
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO enterprise_application(id, organization_id)"
                " VALUES (1, 710001) ON CONFLICT DO NOTHING"
            >>,
            []
        ),
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO enterprise_attachment_retention(attachment_id, organization_id,"
                " application_id, retention_until, hold_state, hold_reason, hold_set_at)"
                " VALUES ($1, 710001, 1, now() - interval '1 day', 'held', 't20-hold', now())"
            >>,
            [AttachmentId]
        ),
    ok.

%% @doc 队列终态失败行（attempts 耗尽 blocked → refresh 投影 failed）。
%% intent 已入队时转 UPDATE（模拟 worker 8 次重试耗尽）；无行时 INSERT。
-spec queue_exhaust_row(pid() | map(), binary()) -> ok.
queue_exhaust_row(Conn, Path) ->
    ok = talk_record_t20_case_registry:verify_connected(Conn),
    {ok, _} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO attachment_object_deletion(path, bucket, attempts, job_state)"
                " VALUES ($1, 't20bucket', 8, 'blocked')"
                " ON CONFLICT (path) DO UPDATE SET attempts = 8, job_state = 'blocked',"
                " lease_token = NULL, lease_until = NULL, next_retry_at = now()"
            >>,
            [Path]
        ),
    ok.

%% @doc 队列完成行（done → refresh 投影 complete）。
-spec queue_done_row(pid() | map(), binary()) -> ok.
queue_done_row(Conn, Path) ->
    ok = talk_record_t20_case_registry:verify_connected(Conn),
    {ok, []} =
        elib_pg:query(
            Conn,
            <<
                "INSERT INTO attachment_object_deletion(path, bucket, job_state, completed_at)"
                " VALUES ($1, 't20bucket', 'done', now())"
            >>,
            [Path]
        ),
    ok.

%%%===================================================================
%%% 直查
%%%===================================================================

-spec attachment_state(pid() | map(), binary()) -> binary().
attachment_state(Conn, Path) ->
    maps:get(
        <<"lifecycle_state">>,
        one(Conn, ["SELECT lifecycle_state FROM attachment WHERE path = '", Path, "'"])
    ).

-spec ref_state(pid() | map(), integer()) -> {binary(), non_neg_integer()}.
ref_state(Conn, RecordId) ->
    Row = one(Conn, [
        "SELECT COALESCE(min(state),'none') AS st, count(*) AS n"
        " FROM talk_record_attachment_ref WHERE record_id = ",
        integer_to_binary(RecordId)
    ]),
    {maps:get(<<"st">>, Row), maps:get(<<"n">>, Row)}.

-spec cleanup_status(integer()) -> binary().
cleanup_status(RecordId) ->
    Row = one(fresh_conn(), [
        "SELECT cleanup_status FROM talk_record WHERE id = ", integer_to_binary(RecordId)
    ]),
    maps:get(<<"cleanup_status">>, Row).

-spec purge_jobs(integer()) -> [map()].
purge_jobs(RecordId) ->
    {ok, Rows} =
        elib_pg:query(
            fresh_conn(),
            <<"SELECT id, state, error_code, attempt FROM talk_record_job",
                " WHERE record_id = $1 AND kind = 'purge' ORDER BY created_at ASC, id ASC">>,
            [RecordId]
        ),
    Rows.

%% @doc 新池外连接（并发互斥用例的第二视角）。
-spec fresh_conn() -> pid().
fresh_conn() ->
    {ok, Conn} = epgsql:connect(talk_record_t20_case_registry:connection_options()),
    try
        ok = talk_record_t20_case_registry:verify_connected(Conn),
        Conn
    catch
        Class:Reason:Stack ->
            epgsql:close(Conn),
            erlang:raise(Class, Reason, Stack)
    end.

%% @doc 独立连接上的显式事务（并发用例精确控制 COMMIT 时点）。
-spec raw_tx(pid(), fun()) -> term().
raw_tx(Conn, Fun) ->
    ok = talk_record_t20_case_registry:verify_connected(Conn),
    {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
    try
        R = Fun(Conn),
        ok = talk_record_t20_case_registry:recheck(),
        {ok, [], []} = epgsql:squery(Conn, "COMMIT"),
        R
    catch
        Class:Reason:Stack ->
            catch epgsql:squery(Conn, "ROLLBACK"),
            erlang:raise(Class, Reason, Stack)
    end.

%%%===================================================================
%%% Disk 驱动（T06 I 组同款：真字节 put + worker claim → DELETE → ack）
%%%===================================================================

disk_root(_Tag) ->
    ok = talk_record_t20_case_registry:recheck(),
    #{entry := #{<<"disk_root">> := Root}} = talk_record_t20_case_registry:current(),
    binary_to_list(Root).

disk_registry_apply(Dir) ->
    Expected = disk_root(current),
    true = filename:absname(Dir) =:= Expected,
    ok = talk_record_t20_case_registry:configure_storage(),
    #{entry := #{<<"namespace">> := Namespace}} = talk_record_t20_case_registry:current(),
    #{namespace => Namespace, container => <<"capture">>}.

disk_locator_digest() ->
    ok = talk_record_t20_case_registry:recheck(),
    #{entry := #{<<"namespace">> := Namespace}} = talk_record_t20_case_registry:current(),
    {ok, Rec} = attachment_storage:resolve(Namespace),
    attachment_storage:locator_digest(Namespace, Rec).
