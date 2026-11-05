-module(eb_direct_retirement_garage_tests).
-export([run/0]).

%% This probes the ordinary worker with a new disposable object; it never
%% deletes the real confirmed winner used to obtain the frozen registry root.
run() ->
    Db = os:getenv("IMBOY_PG_DB"),
    true = is_list(Db) andalso lists:prefix("c08_eb_retire_", Db),
    <<"http://127.0.0.1:49319">> = env_bin("A0_ENDPOINT"),
    <<"imboy-closeout-v3">> = env_bin("C08_EB_BUCKET"),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Ns = <<"eb-real-garage">>,
    Bucket = env_bin("C08_EB_BUCKET"),
    Root = #{
        driver => <<"garage">>,
        namespace => Ns,
        endpoint => env_bin("A0_ENDPOINT"),
        container => Bucket,
        access_key => env_bin("A0_AK"),
        secret_key => env_bin("A0_SK"),
        region => <<"garage">>
    },
    ok = application:set_env(imboy, garage, Root#{bucket => Bucket}),
    ok = application:set_env(
        imboy,
        attachment_storage_registry,
        #{version => 1, roots => #{Ns => Root}, upload_namespaces => #{<<"enterprise">> => Ns}}
    ),
    {ok, Conn} = epgsql:connect(
        "127.0.0.1",
        os:getenv("IMBOY_PG_USER"),
        os:getenv("IMBOY_PG_PASSWORD"),
        [{database, Db}, {port, 49323}]
    ),
    try
        run_on_conn(Conn, Bucket)
    after
        epgsql:close(Conn)
    end.

run_on_conn(Conn, Bucket) ->
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        Conn,
        <<"SELECT count(*) AS n FROM attachment_object_deletion WHERE job_state IN ('ready','retry_wait')">>,
        []
    ),
    {ok, [Row]} = elib_pg:query(
        Conn,
        <<
            "SELECT a.path,a.storage_driver,a.storage_namespace,a.storage_container,a.storage_locator_sha256,a.storage_physical_key"
            " FROM attachment a JOIN attach_pending p ON p.confirmed_attachment_id=a.id"
            " WHERE p.direct_state='CONFIRMED' AND a.storage_namespace='eb-real-garage' LIMIT 1"
        >>,
        []
    ),
    Hex = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    Path = <<"retire-test/", Hex/binary>>,
    Physical = <<"direct-c/retire-test/", Hex/binary>>,
    {ok, Winner} = attachment_storage:frozen_location(maps:get(<<"path">>, Row), Row),
    {ok, _} = attachment_storage:head_snapshot(Winner),
    {ok, Snapshot} = attachment_storage:frozen_location(Path, Row#{
        <<"storage_physical_key">> => Physical
    }),
    Now = calendar:universal_time(),
    Expires = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + 300
    ),
    ok = attachment_storage_garage:put_snapshot(
        Snapshot,
        <<"synthetic-retirement-test">>,
        <<"application/octet-stream">>,
        #{issued_at => Now, expires_at => Expires}
    ),
    {ok, _} = attachment_storage:head_snapshot(Snapshot),
    {ok, _} = elib_pg:execute(
        Conn,
        <<
            "INSERT INTO attachment_object_deletion(path,bucket,storage_driver,storage_namespace,"
            "storage_container,storage_locator_sha256,storage_physical_key,job_state,next_retry_at)"
            " VALUES($1,$2,$3,$4,$5,$6,$7,'ready',now())"
        >>,
        [
            Path,
            Bucket,
            maps:get(<<"storage_driver">>, Row),
            maps:get(<<"storage_namespace">>, Row),
            maps:get(<<"storage_container">>, Row),
            maps:get(<<"storage_locator_sha256">>, Row),
            Physical
        ]
    ),
    {ok, [], []} = epgsql:squery(Conn, "SET ROLE v5_worker"),
    {ok, 1} = attachment_delete_worker:run_batch(Conn, fun(_, _, _, I) ->
        attachment_storage:delete_snapshot(I)
    end),
    {ok, [], []} = epgsql:squery(Conn, "RESET ROLE"),
    {error, not_found} = attachment_storage:head_snapshot(Snapshot),
    {ok, _} = attachment_storage:head_snapshot(Winner),
    {ok, [#{<<"job_state">> := <<"done">>}]} = elib_pg:query(
        Conn,
        <<"SELECT job_state FROM attachment_object_deletion WHERE path=$1">>,
        [Path]
    ),
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        Conn,
        <<"SELECT count(*) AS n FROM attach_pending WHERE direct_state='CONFIRMED'">>,
        []
    ),
    true = N >= 2,
    io:format("EB_DIRECT_SYNTHETIC_QUEUE_GARAGE_PASS~n").

env_bin(Name) -> list_to_binary(os:getenv(Name)).
