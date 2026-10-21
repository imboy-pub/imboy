%%% @doc T20 talk_record 删除/清理/注销预检 eunit 支撑（test-only，不进 release）.
%%%
%%% 复用 talk_record_t12_support（→t07→t05）的池/环境/tsid meck/录音夹具，
%%% 叠加：
%%%   * 池指向 T20 隔离库 127.0.0.1:55436/talk_record_t20（env T20_PG_*
%%%     可覆写；经 os:setenv 注入 t05_support 读取的 T05_PG_* 变量）；
%%%   * jwt_key 注入（playback 票据撤权即时性测试；用后恢复）；
%%%   * 附件世界（T06 fixture 形态）：talk_record 附件资产/confirmed 会话/
%%%     bound 引用/收藏引用（retained_by_other_reference 场景）/EAR hold 行
%%%     的构造与清库（attachment/attach_pending/队列/EAR/user_collect）；
%%%   * 删除域直查（cleanup_status/引用状态/队列进度）。
%%%
%%% schema 基底：runners/T06/t06_fixture_base.sql + runners/T20/t20_base_extra.sql
%%% + 迁移 00000211..00000215（runners/T20/reset_db.sh）。
-module(talk_record_t20_support).

-export([
    setup_all/0,
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

-spec setup_all() -> map().
setup_all() ->
    %% 不经 t12_support（其 setup 会把 T05_PG_* 覆写回 T12 库默认值）——
    %% 直链 t07_support（→t05：池/tsid meck），T20 库 55436 注入在此一次完成。
    os:putenv("T05_PG_HOST", os:getenv("T20_PG_HOST", "127.0.0.1")),
    os:putenv("T05_PG_PORT", os:getenv("T20_PG_PORT", "55436")),
    os:putenv("T05_PG_USER", os:getenv("T20_PG_USER", "t20admin")),
    os:putenv("T05_PG_PASSWORD", os:getenv("T20_PG_PASSWORD", "")),
    os:putenv("T05_PG_DB", os:getenv("T20_PG_DB", "talk_record_t20")),
    State0 = talk_record_t07_support:setup_all(),
    Prev = #{jwt_key => application:get_env(imboy, jwt_key)},
    ok = application:set_env(imboy, jwt_key, ?JWT_KEY),
    try
        State0#{
            t20_prev_env => Prev,
            audio_fixture => talk_record_t12_support:setup_audio_fixture()
        }
    catch
        Class:Reason:Stack ->
            restore_env(jwt_key, maps:get(jwt_key, Prev)),
            talk_record_t07_support:teardown_all(State0),
            erlang:raise(Class, Reason, Stack)
    end.

-spec teardown_all(map()) -> ok.
teardown_all(#{t20_prev_env := Prev} = State) ->
    restore_env(jwt_key, maps:get(jwt_key, Prev)),
    try
        talk_record_t12_support:cleanup_audio_fixture(maps:get(audio_fixture, State))
    after
        talk_record_t07_support:teardown_all(State)
    end.

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
    %% 引用方先清（EAR/队列/收藏对 attachment 的 FK/hash 引用），
    %% 再跑 t12 seed（其内部 DELETE FROM attachment）
    [
        sql(Conn, "DELETE FROM enterprise_attachment_retention"),
        sql(Conn, "DELETE FROM attachment_object_deletion"),
        sql(Conn, "DELETE FROM attach_pending"),
        sql(Conn, "DELETE FROM user_collect"),
        sql(Conn, "DELETE FROM enterprise_application")
    ],
    ok = talk_record_t12_support:seed(State),
    ok.

-spec ids() -> map().
ids() ->
    talk_record_t12_support:ids().

-spec sql(pid() | map(), iolist()) -> ok.
sql(Conn, Sql) ->
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
    Opts = [
        {host, os:getenv("T20_PG_HOST", "127.0.0.1")},
        {port, list_to_integer(os:getenv("T20_PG_PORT", "55436"))},
        {username, os:getenv("T20_PG_USER", "t20admin")},
        {password, ""},
        {database, os:getenv("T20_PG_DB", "talk_record_t20")},
        {ssl, false},
        {timeout, 15000},
        {codecs, [{epgsql_codec_rfc3339_bin, []}]}
    ],
    {ok, Conn} = epgsql:connect(Opts),
    Conn.

%% @doc 独立连接上的显式事务（并发用例精确控制 COMMIT 时点）。
-spec raw_tx(pid(), fun()) -> term().
raw_tx(Conn, Fun) ->
    {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
    try
        R = Fun(Conn),
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
    Base = os:getenv(
        "T20_RUN_ROOT",
        "/Users/leeyi/.codex/task-runs/talk-record-v1/20261020-193000-talkv1-193415"
    ),
    Dir = Base ++ "/pg-t20/diskroot-" ++ os:getpid(),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

disk_registry_apply(Dir) ->
    Endpoint = <<"file://", (list_to_binary(Dir))/binary>>,
    Registry = #{
        version => 1,
        roots => #{
            t20_root => #{
                driver => <<"disk">>,
                namespace => <<"t20-ns">>,
                endpoint => Endpoint
            }
        }
    },
    ok = application:set_env(imboy, attachment_storage_registry, Registry),
    #{namespace => <<"t20-ns">>, container => <<"t20c">>}.

disk_locator_digest() ->
    {ok, Rec} = attachment_storage:resolve(<<"t20-ns">>),
    attachment_storage:locator_digest(<<"t20-ns">>, Rec).
