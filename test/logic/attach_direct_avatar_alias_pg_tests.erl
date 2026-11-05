-module(attach_direct_avatar_alias_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% Dedicated, freshly migrated marker DB and unique private/public Garage buckets only.
avatar_pg_garage_test_() ->
    Run =
        case os:getenv("C08_AVATAR_RUNTIME_ONLY") of
            "1" -> fun run_runtime/1;
            _ -> fun run/1
        end,
    {setup, fun setup/0, fun teardown/1, fun(S) -> {timeout, 240, ?_test(Run(S))} end}.

run_runtime(S) ->
    {EdgePort, Edge} = start_edge(),
    try
        run_runtime_with_edge(S, EdgePort)
    after
        catch port_close(Edge),
        catch cowboy:stop_listener(c08_alias_authorize)
    end.

run_runtime_with_edge(S, EdgePort) ->
    Base = iolist_to_binary(io_lib:format("http://127.0.0.1:~B", [EdgePort])),
    ok = application:set_env(imboy, avatar_edge_base_url, Base),
    ok = application:set_env(imboy, garage, (maps:get(config, S))#{public_base_url => Base}),
    ok = application:set_env(imboy, edge_base_url, <<Base/binary, "/edge">>),
    Body = base64:decode(
        <<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/aX8AAAAASUVORK5CYII=">>
    ),
    {Key, App} = upload(7, Body),
    {ok, Pub} = attach_direct_logic:publish_avatar(Key, App, <<>>),
    Url = maps:get(public_url, Pub),
    [_, Alias] = binary:split(Url, <<"/_avatar/">>),
    ?assertMatch({ok, #{<<"path">> := Key}}, attachment_avatar_alias_repo:resolve(Alias)),
    ?assertEqual({200, Body}, edge_get(Url, [])),
    ok.

setup() ->
    Db = os:getenv("T05_PG_DB"),
    true = is_list(Db) andalso lists:prefix("c08_alias_", Db),
    S = talk_record_t05_support:setup_all(),
    <<Seed:32>> = crypto:strong_rand_bytes(4),
    ok = meck:expect(elib_tsid, generate, fun(_) ->
        9100000000000 + Seed * 100000 + erlang:unique_integer([positive, monotonic])
    end),
    C = maps:get(conn, S),
    {ok, [#{<<"comment">> := <<"c08-avatar-alias-owned">>}]} = elib_pg:query(
        C,
        <<"SELECT shobj_description(oid,'pg_database') AS comment FROM pg_database WHERE datname=current_database()">>,
        []
    ),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Priv = list_to_binary(os:getenv("C08_AVATAR_PRIVATE")),
    Public = list_to_binary(os:getenv("C08_AVATAR_PUBLIC")),
    true = Priv =/= Public,
    Endpoint = list_to_binary(os:getenv("A0_ENDPOINT")),
    Ak = list_to_binary(os:getenv("A0_AK")),
    Sk = list_to_binary(os:getenv("A0_SK")),
    Config = #{
        endpoint => Endpoint,
        bucket => Priv,
        public_bucket => Public,
        region => <<"garage">>,
        access_key => Ak,
        secret_key => Sk
    },
    Ns = <<"c08-avatar-pg">>,
    Root = #{
        driver => <<"garage">>,
        namespace => Ns,
        endpoint => Endpoint,
        container => Priv,
        access_key => Ak,
        secret_key => Sk,
        region => <<"garage">>
    },
    ok = application:set_env(imboy, garage, Config),
    ok = application:set_env(
        imboy,
        attachment_storage_registry,
        #{version => 1, roots => #{Ns => Root}, upload_namespaces => #{<<"public">> => Ns}}
    ),
    S#{config => Config}.

teardown(S) -> talk_record_t05_support:teardown_all(S).

run(S) ->
    {EdgePort, Edge} = start_edge(),
    try
        run_with_edge(S, EdgePort)
    after
        catch port_close(Edge),
        catch cowboy:stop_listener(c08_alias_authorize)
    end.

run_with_edge(S, EdgePort) ->
    Base = iolist_to_binary(io_lib:format("http://127.0.0.1:~B", [EdgePort])),
    ok = application:set_env(imboy, avatar_edge_base_url, Base),
    Cfg = (maps:get(config, S))#{public_base_url => Base},
    ok = application:set_env(imboy, garage, Cfg),
    ok = application:set_env(imboy, edge_base_url, <<Base/binary, "/edge">>),
    Body = base64:decode(
        <<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/aX8AAAAASUVORK5CYII=">>
    ),
    finalize_copy_failure_recovers(Body),
    {Key, App} = upload(1, Body),
    ?assertEqual(404, public_get(Cfg, Key)),
    {ok, SourceRow, #{<<"state">> := <<"unpublished">>}} = attachment_avatar_alias_repo:reserve(
        Key
    ),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM attachment_avatar_alias WHERE object_key=$1">>, [Key]
    ),
    {ok, 1} = elib_pg:execute(<<"UPDATE attachment SET status=0 WHERE path=$1">>, [Key]),
    {SecondKey, SecondApp} = upload(3, Body),
    {ok, _} = attach_direct_logic:publish_avatar(SecondKey, SecondApp, <<>>),
    gc_before_bind_race(SecondKey, SecondApp),
    ?assertEqual(
        {error, avatar_source_changed}, attachment_avatar_alias_repo:publish(Key, SourceRow)
    ),
    {ok, 1} = elib_pg:execute(<<"UPDATE attachment SET status=1 WHERE path=$1">>, [Key]),
    {ok, PendingUrl} = attachment_avatar_alias_repo:public_url(
        <<"00000000-0000-4000-8000-000000000001">>
    ),
    ?assertMatch({404, _}, edge_get(PendingUrl, [])),
    alias_database_failure_recovers(Key, App),
    {ok, Pub} = attach_direct_logic:publish_avatar(Key, App, <<>>),
    ?assertEqual(404, public_get(Cfg, Key)),
    ?assertEqual({ok, Pub}, attach_direct_logic:publish_avatar(Key, App, <<>>)),
    Url = maps:get(public_url, Pub),
    [_, Alias] = binary:split(Url, <<"/_avatar/">>),
    ?assertMatch({ok, #{<<"path">> := Key}}, attachment_avatar_alias_repo:resolve(Alias)),
    ?assertEqual(
        ok,
        elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            {ok, [#{<<"visible">> := true}]} = elib_pg:query(
                C,
                <<"SELECT public.fn_avatar_alias_bound_visible($1::uuid,$2) AS visible">>,
                [Alias, Url]
            ),
            ok
        end)
    ),
    ?assertEqual({200, Body}, edge_get(Url, [])),
    ?assertEqual({206, binary:part(Body, 0, 8)}, edge_get(Url, [{"range", "bytes=0-7"}])),
    {ok, {{_, 200, _}, _, _}} = httpc:request(head, {binary_to_list(Url), []}, [], []),
    ?assertMatch({416, _}, edge_get(Url, [{"range", "bytes=999999-"}])),
    {ok, Row} = attachment_repo:find_by_path(Key),
    RawUrl = iolist_to_binary([
        maps:get(endpoint, Cfg),
        "/",
        maps:get(bucket, Cfg),
        "/",
        maps:get(<<"storage_physical_key">>, Row)
    ]),
    ?assertMatch({403, _}, edge_get(RawUrl, [])),
    ?assertMatch(
        {error, _}, elib_pg:query(<<"UPDATE attachment SET status=0 WHERE path=$1">>, [Key])
    ),
    bind_gc_race(Key, App),
    {ok, 1} = elib_pg:execute(
        <<"UPDATE attachment_avatar_alias SET state='revoked' WHERE object_key=$1">>, [Key]
    ),
    ?assertMatch({404, _}, edge_get(Url, [])),
    {ok, 1} = elib_pg:execute(<<"UPDATE attachment SET status=0 WHERE path=$1">>, [Key]),
    {Unconfirmed, UnconfirmedApp} = register(2),
    ?assertEqual(
        {error, not_confirmed},
        attach_direct_logic:publish_avatar(Unconfirmed, UnconfirmedApp, <<>>)
    ),
    ok.

alias_database_failure_recovers(Key, App) ->
    {ok, _} = elib_pg:query(
        <<
            "CREATE FUNCTION c08_alias_injected_failure() RETURNS trigger LANGUAGE plpgsql AS $$ "
            "BEGIN RAISE EXCEPTION 'injected alias failure'; END $$"
        >>,
        []
    ),
    try
        {ok, _} = elib_pg:query(
            <<
                "CREATE TRIGGER c08_alias_injected_failure BEFORE INSERT ON attachment_avatar_alias "
                "FOR EACH ROW EXECUTE FUNCTION c08_alias_injected_failure()"
            >>,
            []
        ),
        ?assertMatch(
            {error, #error{code = <<"P0001">>, message = <<"injected alias failure">>}},
            attach_direct_logic:publish_avatar(Key, App, <<>>)
        ),
        {ok, [#{<<"n">> := 0}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM attachment_avatar_alias WHERE object_key=$1">>, [Key]
        ),
        {ok, Row} = attachment_repo:find_by_path(Key),
        {ok, Cfg} = application:get_env(imboy, garage),
        Raw = iolist_to_binary([
            maps:get(endpoint, Cfg),
            "/",
            maps:get(bucket, Cfg),
            "/",
            maps:get(<<"storage_physical_key">>, Row)
        ]),
        ?assertMatch({403, _}, edge_get(Raw, []))
    after
        {ok, _} = elib_pg:query(
            <<"DROP TRIGGER IF EXISTS c08_alias_injected_failure ON attachment_avatar_alias">>, []
        ),
        {ok, _} = elib_pg:query(<<"DROP FUNCTION c08_alias_injected_failure()">>, [])
    end.

finalize_copy_failure_recovers(Body) ->
    {Key, App} = register(9),
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Body), <<"image/png">>),
    {ok, #{grant := Grant, attempt_id := Attempt}} = attach_direct_logic:grant(Key, Sha),
    put_grant(Grant, Body),
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    Parent = self(),
    try
        ok = meck:expect(attachment_storage_garage, copy_dispatch, fun(
            Ns, Container, Source, Target
        ) ->
            ?assertMatch({ok, _}, meck:passthrough([Ns, Container, Source, Target])),
            Parent ! {accepted_copy, Ns, Container, Target},
            {error, timeout}
        end),
        ok = meck:expect(attachment_storage_garage, head_bounded, fun(_, _, _) ->
            {error, timeout}
        end),
        ?assertMatch(
            {ok, #{outcome := <<"UNKNOWN_QUARANTINED">>}},
            attach_direct_logic:finalize(Key, Attempt)
        ),
        ?assertMatch({error, _}, attach_direct_logic:publish_avatar(Key, App, <<>>)),
        {ok, [#{<<"n">> := 0}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM attachment_avatar_alias WHERE object_key=$1">>, [Key]
        ),
        [Unsigned | _] = binary:split(maps:get(url, Grant), <<"?">>),
        ?assertMatch({403, _}, edge_get(Unsigned, []))
    after
        meck:unload(attachment_storage_garage)
    end,
    receive
        {accepted_copy, Ns, Container, Target} ->
            ?assertMatch({ok, _}, attachment_storage_garage:head_bounded(Ns, Container, Target))
    after 10000 -> error(accepted_copy_not_observed)
    end,
    {ok, #{grant := RetryGrant, attempt_id := RetryAttempt}} =
        attach_direct_logic:renew_grant(Key, Sha),
    ?assertNotEqual(Attempt, RetryAttempt),
    put_grant(RetryGrant, Body),
    ?assertMatch(
        {ok, #{state := <<"CONFIRMED">>}},
        attach_direct_logic:finalize(Key, RetryAttempt)
    ),
    ?assertMatch({ok, #{public_url := _}}, attach_direct_logic:publish_avatar(Key, App, <<>>)).

register(_N) ->
    Suffix = binary:encode_hex(crypto:strong_rand_bytes(6), lowercase),
    Cid = <<"10000000-0000-4000-8000-", Suffix/binary>>,
    App = Cid,
    {ok, #{object_key := Key}} = attach_direct_logic:register_application(
        App, <<"0">>, <<"public">>, Cid, <<"avatar.png">>
    ),
    {Key, App}.

bind_gc_race(Key, Cid) ->
    Admin = 987654321,
    {ok, 1} = elib_pg:execute(
        <<"INSERT INTO admin_avatar_session_owner(client_upload_id,admin_user_id) VALUES($1::uuid,$2)">>,
        [Cid, Admin]
    ),
    {ok, 1} = elib_pg:execute(
        <<"UPDATE attachment_avatar_alias SET preview_expires_at=clock_timestamp()+interval '2 seconds' WHERE object_key=$1">>,
        [Key]
    ),
    Parent = self(),
    Binder = spawn_link(fun() ->
        Result = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            {ok, [#{<<"pid">> := Pid}]} = elib_pg:query(
                C, <<"SELECT pg_backend_pid() AS pid">>, []
            ),
            {ok, _} = attachment_avatar_alias_repo:bind_tx(C, Key, Cid, 123456789, true, Admin),
            Parent ! {bound, Pid},
            receive
                commit_bind -> ok
            after 10000 -> error(bind_timeout)
            end
        end),
        Parent ! {bind_result, Result}
    end),
    BinderPid =
        receive
            {bound, BoundPid} -> BoundPid
        after 10000 -> error(bind_not_ready)
        end,
    timer:sleep(2500),
    spawn_link(fun() ->
        Result = elib_pg:with_tx(fun(C) ->
            {ok, [#{<<"pid">> := GcBackendPid}]} = elib_pg:query(
                C, <<"SELECT pg_backend_pid() AS pid">>, []
            ),
            Parent ! {gc_pid, GcBackendPid},
            elib_pg:execute(C, <<"UPDATE attachment SET status=0 WHERE path=$1">>, [Key])
        end),
        Parent ! {gc_result, Result}
    end),
    GcPid =
        receive
            {gc_pid, Pid2} -> Pid2
        after 10000 -> error(gc_not_started)
        end,
    assert_blocked(GcPid, BinderPid, 50),
    Binder ! commit_bind,
    receive
        {bind_result, ok} -> ok
    after 10000 -> error(bind_commit_timeout)
    end,
    receive
        {gc_result,
            {error, #error{code = <<"23510">>, message = <<"avatar alias source is held">>}}} ->
            ok
    after 10000 -> error(gc_result_timeout)
    end,
    {ok, [#{<<"status">> := 1, <<"bound_user_id">> := 123456789}]} = elib_pg:query(
        <<
            "SELECT a.status, v.bound_user_id FROM attachment a JOIN attachment_avatar_alias v "
            "ON v.attachment_id=a.id WHERE a.path=$1"
        >>,
        [Key]
    ).

gc_before_bind_race(Key, Cid) ->
    Admin = 987654322,
    {ok, 1} = elib_pg:execute(
        <<"INSERT INTO admin_avatar_session_owner(client_upload_id,admin_user_id) VALUES($1::uuid,$2)">>,
        [Cid, Admin]
    ),
    {ok, 1} = elib_pg:execute(
        <<"UPDATE attachment_avatar_alias SET preview_expires_at=clock_timestamp()-interval '1 second' WHERE object_key=$1">>,
        [Key]
    ),
    Parent = self(),
    Gc = spawn_link(fun() ->
        Result = elib_pg:with_tx(fun(C) ->
            {ok, [#{<<"pid">> := Pid}]} = elib_pg:query(
                C, <<"SELECT pg_backend_pid() AS pid">>, []
            ),
            {ok, 1} = elib_pg:execute(C, <<"UPDATE attachment SET status=0 WHERE path=$1">>, [Key]),
            Parent ! {gc_locked, Pid},
            receive
                commit_gc -> ok
            after 10000 -> error(gc_timeout)
            end
        end),
        Parent ! {gc_commit, Result}
    end),
    GcPid =
        receive
            {gc_locked, LockedGcPid} -> LockedGcPid
        after 10000 -> error(gc_not_ready)
        end,
    spawn_link(fun() ->
        Result = elib_pg:with_tx(fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL ROLE v5_app_runtime">>, []),
            {ok, [#{<<"pid">> := LateBindPid}]} = elib_pg:query(
                C, <<"SELECT pg_backend_pid() AS pid">>, []
            ),
            Parent ! {late_bind_pid, LateBindPid},
            attachment_avatar_alias_repo:bind_tx(C, Key, Cid, 123456790, true, Admin)
        end),
        Parent ! {late_bind, Result}
    end),
    BinderPid =
        receive
            {late_bind_pid, Pid2} -> Pid2
        after 10000 -> error(bind_not_started)
        end,
    assert_blocked(BinderPid, GcPid, 50),
    Gc ! commit_gc,
    receive
        {gc_commit, ok} -> ok
    after 10000 -> error(gc_commit_timeout)
    end,
    receive
        {late_bind, {error, avatar_source_changed}} -> ok
    after 10000 -> error(late_bind_timeout)
    end.

assert_blocked(_, _, 0) ->
    error(expected_lock_wait_not_observed);
assert_blocked(Waiter, Holder, Attempts) ->
    {ok, [#{<<"blocked">> := Blocked}]} = elib_pg:query(
        <<"SELECT $2::int = ANY(pg_blocking_pids($1::int)) AS blocked">>, [Waiter, Holder]
    ),
    case Blocked of
        true ->
            ok;
        false ->
            timer:sleep(50),
            assert_blocked(Waiter, Holder, Attempts - 1)
    end.

upload(N, Body) ->
    {Key, App} = register(N),
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Body), <<"image/png">>),
    {ok, #{grant := Grant, attempt_id := Attempt}} = attach_direct_logic:grant(Key, Sha),
    put_grant(Grant, Body),
    {ok, #{state := <<"CONFIRMED">>}} = attach_direct_logic:finalize(Key, Attempt),
    {Key, App}.

put_grant(Grant, Body) ->
    Headers = maps:from_list(maps:get(required_headers, Grant)),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(maps:get(url, Grant)),
            [
                {"x-amz-checksum-sha256",
                    binary_to_list(maps:get(<<"x-amz-checksum-sha256">>, Headers))}
            ],
            "image/png",
            Body
        },
        [{autoredirect, false}],
        [{body_format, binary}]
    ),
    ok.

public_get(Cfg, Key) ->
    Bucket = maps:get(public_bucket, Cfg),
    Url = elib_s3_sign:presign_signed_url_at(
        <<"GET">>,
        maps:get(endpoint, Cfg),
        Bucket,
        Key,
        [],
        calendar:universal_time(),
        300,
        Cfg#{container => Bucket}
    ),
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        get,
        {binary_to_list(Url), []},
        [{autoredirect, false}],
        [{body_format, binary}]
    ),
    Status.

start_edge() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Token = "c08-owned-synthetic-edge-token",
    ok = application:set_env(imboy, edge_internal_token, list_to_binary(Token)),
    Dispatch = cowboy_router:compile([
        {'_', [{"/authorize", attachment_edge_authorize_handler, #{}}]}
    ]),
    {ok, _} = cowboy:start_clear(
        c08_alias_authorize,
        [{ip, {127, 0, 0, 1}}, {port, 0}],
        #{env => #{dispatch => Dispatch}}
    ),
    AuthPort = ranch:get_port(c08_alias_authorize),
    {ok, Sock} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
    {ok, {_, EdgePort}} = inet:sockname(Sock),
    ok = gen_tcp:close(Sock),
    Auth = lists:flatten(io_lib:format("http://127.0.0.1:~B/authorize", [AuthPort])),
    Paths = string:join([filename:absname(P) || P <- code:get_path()], ":"),
    Edge = open_port(
        {spawn_executable, os:find_executable("escript")},
        [
            exit_status,
            stderr_to_stdout,
            {args, [filename:absname("priv/workers/download_edge.escript")]},
            {env, [
                {"EDGE_PORT", integer_to_list(EdgePort)},
                {"EDGE_AUTHORIZE_URL", Auth},
                {"EDGE_INTERNAL_TOKEN", Token},
                {"EDGE_EBIN", filename:absname("ebin")},
                {"EDGE_CODE_PATHS", Paths}
            ]}
        ]
    ),
    receive
        {Edge, {data, Line}} ->
            true = binary:match(iolist_to_binary(Line), <<"EDGE_READY">>) =/= nomatch
    after 15000 -> error(edge_start_timeout)
    end,
    {EdgePort, Edge}.

edge_get(Url, Headers) ->
    {ok, {{_, Status, _}, _, Body}} = httpc:request(
        get,
        {binary_to_list(Url), Headers},
        [{autoredirect, false}],
        [{body_format, binary}]
    ),
    {Status, Body}.
