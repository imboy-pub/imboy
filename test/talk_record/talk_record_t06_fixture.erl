-module(talk_record_t06_fixture).
%%%
% T06 隔离测试支撑（专用一次性 PG 127.0.0.1:55434/talk_record_t06，
% 由 runners/T06/t06_attachment_lifecycle_check.sh 建 fixture + 迁移后调用；
% 环境变量 T06_PG_* 缺失时套件 skip——不共享任何 eunit 池/共享库）。
%
% schema 基底：runners/T06/t06_fixture_base.sql + 真实迁移 00000211/00000212。
%%%

-export([
    env_ready/0,
    connect/0,
    close/1,
    q/2, q/3,
    tx/2,
    reset_business_data/0,
    seed_world/1,
    seed_world/2,
    insert_attachment/2,
    insert_pending_confirmed/3, insert_pending_confirmed/4,
    grant_role/4,
    disk_registry_apply/1,
    disk_root/1,
    disk_locator_digest/0,
    unique_key/1,
    past_ts/0,
    future_ts/0
]).

-define(NS, <<"t06-disk-ns">>).
-define(CONTAINER, <<"t06c">>).

%% ---------------------------------------------------------------- 连接

env_ready() ->
    lists:all(
        fun(K) -> os:getenv(K) =/= false end,
        ["T06_PG_HOST", "T06_PG_PORT", "T06_PG_USER", "T06_PG_DATABASE"]
    ).

connect() ->
    Opts = [
        {host, os:getenv("T06_PG_HOST", "127.0.0.1")},
        {port, list_to_integer(os:getenv("T06_PG_PORT", "55434"))},
        {username, os:getenv("T06_PG_USER", "t06admin")},
        {password, os:getenv("T06_PG_PASSWORD", "")},
        {database, os:getenv("T06_PG_DATABASE", "talk_record_t06")},
        {ssl, false},
        {timeout, 15000},
        %% 与生产 pg_conf 同款 timestamptz 编解码（elib_dt RFC3339 binary 直传）
        {codecs, [{epgsql_codec_rfc3339_bin, []}]}
    ],
    {ok, Conn} = epgsql:connect(Opts),
    Conn.

close(Conn) ->
    catch epgsql:close(Conn),
    ok.

q(Sql, Params) ->
    Conn = connect(),
    try
        q(Conn, Sql, Params)
    after
        close(Conn)
    end.

q(Conn, Sql, Params) ->
    elib_pg:query(Conn, Sql, Params).

%% 独立连接上的显式事务（与生产 elib_pg:with_tx 同语义的手工形态，
%% 供并发竞争用例精确控制 COMMIT 时点）。
tx(Conn, Fun) ->
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

%% ---------------------------------------------------------------- 数据

reset_business_data() ->
    Tables = [
        <<"talk_record_attachment_ref">>,
        <<"talk_record_audio_part">>,
        <<"talk_record_capture_event">>,
        <<"talk_record_capture">>,
        <<"talk_record_grant">>,
        <<"talk_record">>,
        <<"attachment_object_deletion">>,
        <<"attachment_lifecycle_v5_audit">>,
        <<"attach_pending">>,
        <<"attachment">>,
        <<"enterprise_attachment_retention">>,
        <<"workspace_attachment_retention">>,
        <<"enterprise_asset_delete_queue">>,
        <<"user_collect">>,
        <<"moment_post">>,
        <<"group_file">>,
        <<"organization_member">>,
        <<"enterprise_application">>,
        <<"workspace">>,
        <<"organization">>,
        <<"\"user\"">>
    ],
    List = iolist_to_binary(lists:join(<<", ">>, Tables)),
    {ok, []} = q(<<"TRUNCATE ", List/binary, " RESTART IDENTITY CASCADE">>, []),
    ok.

%% 最小世界：user 1(owner)/2(grantee)/3(stranger)；org 10(+ws 100)/20(inactive)；
%% personal 记录 1001（owner 1）；org 记录 1002（org 10）；capture 2001@1001。
seed_world(Conn) ->
    seed_world(Conn, #{}).

seed_world(Conn, _Opts) ->
    S = fun(Sql, P) ->
        {ok, _} = q(Conn, Sql, P),
        ok
    end,
    lists:foreach(
        fun({Id, Name}) ->
            ok = talk_record_parent_fixture:user(Conn, Id, #{<<"nickname">> => Name})
        end,
        [{1, <<"owner">>}, {2, <<"grantee">>}, {3, <<"stranger">>}]
    ),
    ok = talk_record_parent_fixture:organization(Conn, 10, <<"t06-active">>, 1, <<"active">>),
    ok = talk_record_parent_fixture:organization(Conn, 20, <<"t06-inactive">>, 1, <<"archived">>),
    ok = talk_record_parent_fixture:workspace(Conn, 100, 10, <<"t06-workspace">>, 1),
    ok = S(
        <<
            "INSERT INTO organization_member(organization_id, user_id, status)"
            " VALUES (10, 1, 'active'), (20, 1, 'active') ON CONFLICT (organization_id,user_id) DO NOTHING"
        >>,
        []
    ),
    ok = S(
        <<
            "INSERT INTO talk_record(id, owner_user_id, created_by, client_recording_id,"
            " scope, organization_id, workspace_id)"
            " VALUES (1001, 1, 1, '00000000-0000-0000-0000-000000000001', 'personal', NULL, NULL),"
            " (1002, 1, 1, '00000000-0000-0000-0000-000000000002', 'organization', 10, 100)"
        >>,
        []
    ),
    ok = S(
        <<
            "INSERT INTO talk_record_capture(id, record_id, device_session_id, device_id,"
            " started_by, status) VALUES (2001, 1001,"
            " '00000000-0000-0000-0000-0000000000aa', 'dev1', 1, 'recording')"
        >>,
        []
    ),
    ok.

%% 插入 talk_record 附件资产（confirmed 形态：live + 完整位置四元组可选）。
insert_attachment(Conn, #{id := Id, record_id := RecordId} = Opts) ->
    Path = maps:get(path, Opts, unique_key(Id)),
    Hash = maps:get(hash, Opts, binary:copy(<<"a">>, 64)),
    Driver = maps:get(storage_driver, Opts, <<"disk">>),
    Ns = maps:get(storage_namespace, Opts, <<"t06-any">>),
    Container = maps:get(storage_container, Opts, <<"t06c">>),
    Locator = maps:get(storage_locator, Opts, binary:copy(<<"0">>, 64)),
    Referer = maps:get(referer_time, Opts, 1),
    {ok, []} =
        q(
            Conn,
            <<
                "INSERT INTO attachment(id, file_hash256, mime_type, name, path, url, size,"
                " referer_time, creator_user_id, scope, scope_ref, cipher,"
                " storage_driver, storage_namespace, storage_container, storage_locator_sha256)"
                " VALUES ($1,$2,'audio/aac',$3,$4,$4,10,$5,$11,'talk_record',$6::bigint::text,NULL,"
                " $7,$8,$9,$10)"
            >>,
            [
                Id,
                Hash,
                Path,
                Path,
                Referer,
                RecordId,
                Driver,
                Ns,
                Container,
                Locator,
                maps:get(creator_user_id, Opts, 1)
            ]
        ),
    #{path => Path}.

%% confirmed 上传会话（adapter 会话校验用）。
insert_pending_confirmed(Conn, ObjectKey, AttachmentId) ->
    insert_pending_confirmed(Conn, ObjectKey, AttachmentId, 1).

insert_pending_confirmed(Conn, ObjectKey, AttachmentId, Actor) ->
    {ok, Rows} = q(
        Conn,
        <<
            "SELECT column_name FROM information_schema.columns"
            " WHERE table_schema=current_schema() AND table_name='attach_pending' AND column_name='id'"
        >>,
        []
    ),
    {Columns, Values, Params} =
        case Rows of
            [] -> {<<>>, <<>>, []};
            [_] -> {<<"id,">>, <<"$4,">>, [AttachmentId * 10 + 1]}
        end,
    tx(Conn, fun(C) ->
        {ok, []} = q(
            C,
            <<"INSERT INTO attach_pending(", Columns/binary,
                "object_key,bucket,scope,creator_user_id,session_state,upload_expires_at,"
                "storage_driver,storage_namespace,storage_container,storage_locator_sha256,"
                "upload_registration,upload_content) SELECT ", Values/binary,
                "$1,'t06bucket',scope,$3,'registered',now()-interval '1 hour',"
                "storage_driver,storage_namespace,storage_container,storage_locator_sha256,"
                "jsonb_build_object('subject',jsonb_build_object('kind','user','principal',$3::bigint::text),"
                "'declaration',jsonb_build_object('scope',scope,'scope_ref',scope_ref)),"
                "jsonb_build_object('version',1,'sha256',file_hash256,'size_bytes',size::text,'mime',mime_type)"
                " FROM attachment WHERE id=$2">>,
            [ObjectKey, AttachmentId, Actor] ++ Params
        ),
        {ok, AttachmentId} = attach_pending_repo:confirm_session_tx(C, ObjectKey, AttachmentId),
        ok
    end).

grant_role(Conn, RecordId, GranteeId, Role) ->
    {ok, []} =
        q(
            Conn,
            <<
                "INSERT INTO talk_record_grant(id, record_id, grantee_user_id, role, granted_by)"
                " VALUES ($1, $2, $3, $4, 1)"
            >>,
            [RecordId * 100 + GranteeId, RecordId, GranteeId, Role]
        ),
    ok.

%% ---------------------------------------------------------------- Disk 驱动

disk_root(_Tag) ->
    %% Disk driver 路径校验拒绝 symlink 与组/其他可写目录（attachment_storage_disk
    %% locked_path_check）——/tmp 在 macOS 是 symlink 且 1777，不能用。
    %% 根放 run 专属 pg-t06/ 下（runner 导出 T06_RUN_ROOT）。
    Base = os:getenv(
        "T06_RUN_ROOT", "/Users/leeyi/.codex/task-runs/talk-record-v1/20261020-193000-talkv1-193415"
    ),
    Dir = Base ++ "/pg-t06/diskroot-" ++ os:getpid(),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

disk_registry_apply(Dir) ->
    Endpoint = <<"file://", (list_to_binary(Dir))/binary>>,
    Registry = #{
        version => 1,
        roots => #{
            t06_root => #{
                driver => <<"disk">>,
                namespace => ?NS,
                endpoint => Endpoint
            }
        }
    },
    ok = application:set_env(imboy, attachment_storage_registry, Registry),
    #{namespace => ?NS, container => ?CONTAINER}.

disk_locator_digest() ->
    {ok, Rec} = attachment_storage:resolve(?NS),
    attachment_storage:locator_digest(?NS, Rec).

%% ---------------------------------------------------------------- 杂项

unique_key(N) ->
    <<"u1/t06-", (integer_to_binary(N))/binary, "-",
        (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

past_ts() ->
    <<"2000-01-01 00:00:00+00">>.

future_ts() ->
    <<"2099-01-01 00:00:00+00">>.
