-module(textguard_business_fixture).

-include_lib("eunit/include/eunit.hrl").
-export([
    setup/0,
    cleanup/1,
    reset/2,
    config/1,
    counts/0,
    sql/2,
    scalar/2,
    owner/0,
    outsider/0,
    channel/0,
    queue/0,
    notifications/0,
    review_failure/2,
    remote_fault/4,
    remote_hang/3,
    race/3,
    row/2,
    export/0,
    observe/1,
    republish/0,
    apply_migration/1,
    review_binding/3,
    removal_counts/0,
    private_message/1,
    canary/0,
    consumer_bootstrap/0
]).

owner() -> 9007199254741011.
outsider() -> 9007199254741012.
channel() -> 9007199254741021.

setup() ->
    isolated_pg_fixture:setup_single_node_cache(fun setup_owned/0, fun cleanup/1).

setup_owned() ->
    ok = textguard_suite_fixture:preflight(business),
    ok = textguard_suite_fixture:require_owned_pool(),
    {ok, _} = application:ensure_all_started(gun),
    ok = textguard_export_tests:pg_setup(),
    ok = apply_schema(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 27}),
    {ok, Cache} = imboy_cache:start_link([]),
    undefined = whereis(imboy_cache_sync),
    {ok, CacheSync} = imboy_cache_sync:start_link(),
    Table = ets:new(tg20_observations, [public, named_table, set]),
    start_sinks(),
    ok = application:load(grpcbox),
    Port = list_to_integer(os:getenv("TG19_GRPC_PORT")),
    ok = application:set_env(grpcbox, client, #{
        channels => [
            {tg20_real, [{http, "127.0.0.1", Port, []}], #{sync_start => true}}
        ]
    }),
    {ok, _} = application:ensure_all_started(grpcbox),
    Channel = gproc:where({n, l, {grpcbox_channel, tg20_real}}),
    ?assert(lists:keymember(Channel, 2, supervisor:which_children(grpcbox_channel_sup))),
    ?assert(grpcbox_channel:is_ready(tg20_real)),
    FaultPort = list_to_integer(os:getenv("TG19_FAULT_GRPC_PORT")),
    {ok, Fault} = textguard_fault_service:start(FaultPort),
    {ok, _} = textguard_grpcbox_transport:start(tg20_fault, <<"127.0.0.1">>, FaultPort),
    State = #{cache => Cache, cache_sync => CacheSync, table => Table, fault => Fault},
    config([{transport, http}]),
    seed_words(),
    Path = os:getenv("TG19_ENH_FILE"),
    ok = file:write_file(Path, jsone:encode(textguard_golden:enhancement())),
    ok = application:set_env(imboy, textguard_export, [
        {t2s_table_id, <<"test-t2s-v1">>}, {enhancement_file, list_to_binary(Path)}
    ]),
    {ok, _} = textguard_publisher:start_link(#{
        reconcile_interval_ms => 3600000,
        cas_max_retries => 0,
        publish_timeout_ms => 1000
    }),
    try
        {ok, Published} = textguard_publisher:reconcile(),
        ?assertEqual(published, maps:get(state, Published)),
        ?assertEqual(false, maps:get(pending, Published))
    after
        textguard_publisher:stop()
    end,
    {ok, Export} = textguard_export:export_package(),
    ets:insert(Table, {active_revision, maps:get(package_sha256, Export)}),
    State.

cleanup(State) ->
    catch textguard_publisher:stop(),
    textguard_grpcbox_transport:stop(tg20_fault),
    textguard_grpcbox_transport:stop(tg20_real),
    textguard_fault_service:stop(maps:get(fault, State)),
    application:stop(grpcbox),
    application:unset_env(grpcbox, client),
    lists:foreach(
        fun(Module) -> catch meck:unload(Module) end,
        [channel_logic_notify, moment_logic_notify, elib_log, elib_tsid, elib_pg]
    ),
    gen_server:stop(maps:get(cache_sync, State)),
    gen_server:stop(maps:get(cache, State)),
    ets:delete(maps:get(table, State)),
    textguard_export_tests:pg_teardown(ok),
    lists:foreach(
        fun(Key) -> application:unset_env(imboy, Key) end,
        [moderation_detector, textguard_export]
    ),
    ok.

reset(_State, Transport) ->
    config([{transport, Transport}]),
    sql(
        <<
            "TRUNCATE review_queue,channel_message,channel_subscription,channel_admin,channel,"
            "moment_timeline,moment_post_acl,moment_post,user_friend,user_denylist,public.\"user\""
        >>,
        []
    ),
    lists:foreach(
        fun(Uid) ->
            sql(
                <<
                    "INSERT INTO public.\"user\" (id,password,account,reg_ip,reg_cosv,sign) "
                    "VALUES ($1,'fixture', $2,'127.0.0.1','fixture','before')"
                >>,
                [Uid, integer_to_binary(Uid)]
            )
        end,
        [owner(), outsider()]
    ),
    sql(
        <<"INSERT INTO channel(id,name,creator_uid) VALUES ($1,'tg20-fixture',$2)">>,
        [channel(), owner()]
    ),
    clear_counts(),
    ok.

config(Overrides) ->
    Defaults = [
        {mode, rust},
        {transport, http},
        {failure_policy, availability_compat},
        {http_base_url, env_bin("TG19_HTTP_URL")},
        {publish_http_base_url, env_bin("TG19_MANAGE_URL")},
        {token, secret("TEXTGUARD_CHECK_SECRET_FILE")},
        {publish_token, secret("TEXTGUARD_PUBLISH_SECRET_FILE")},
        {grpc_host, <<"127.0.0.1">>},
        {grpc_port, list_to_integer(os:getenv("TG19_GRPC_PORT"))},
        {grpc_channel, tg20_real},
        {timeout_ms, 1000},
        {max_response_bytes, 262144}
    ],
    application:set_env(
        imboy,
        moderation_detector,
        Overrides ++
            [
                Entry
             || {Key, _} = Entry <- Defaults,
                not lists:keymember(Key, 1, Overrides)
            ]
    ).

counts() ->
    maps:from_list([
        {Key, count(Key)}
     || Key <-
            [
                published,
                edited,
                deleted,
                moment,
                review_enqueue_failed,
                review_binding_invalid,
                privacy_leak
            ]
    ]).

notifications() -> maps:with([published, edited, deleted, moment], counts()).

start_sinks() ->
    lists:foreach(
        fun(Module) -> meck:new(Module, [passthrough, no_link, no_history]) end,
        [channel_logic_notify, moment_logic_notify, elib_log]
    ),
    meck:expect(
        channel_logic_notify,
        broadcast_channel_message,
        fun(_, _) ->
            tick(published),
            ok
        end
    ),
    meck:expect(
        channel_logic_notify,
        notify_message_edited,
        fun(_, _, _, _) ->
            tick(edited),
            ok
        end
    ),
    meck:expect(
        channel_logic_notify,
        notify_message_deleted,
        fun(_, _) ->
            tick(deleted),
            ok
        end
    ),
    meck:expect(channel_logic_notify, notify_channel_unread_count, fun(_, _, _) -> ok end),
    meck:expect(
        moment_logic_notify,
        notify_post_created,
        fun(_, _) ->
            tick(moment),
            ok
        end
    ),
    meck:expect(elib_log, internal_log, fun(_, Data, _, _) -> record_log(Data) end),
    meck:expect(elib_log, internal_log, fun(_, Data, _, _, _) -> record_log(Data) end).

record_log(Data) ->
    Encoded = term_to_binary(Data),
    Sensitive = [
        canary(),
        secret("TEXTGUARD_CHECK_SECRET_FILE"),
        secret("TEXTGUARD_PUBLISH_SECRET_FILE")
    ],
    case
        lists:any(
            fun(Value) ->
                byte_size(Value) > 0 andalso
                    binary:match(Encoded, Value) =/= nomatch
            end,
            Sensitive
        )
    of
        true -> tick(privacy_leak);
        false -> ok
    end,
    case Data of
        ["review_enqueue_failed" | _] -> tick(review_enqueue_failed);
        ["review_binding_invalid" | _] -> tick(review_binding_invalid);
        _ -> ok
    end,
    ok.
canary() -> <<"TG20-CANARY-NO-LOG">>.
tick(Key) -> ets:update_counter(tg20_observations, Key, 1, {Key, 0}).
count(Key) ->
    case ets:lookup(tg20_observations, Key) of
        [{_, Value}] -> Value;
        [] -> 0
    end.
clear_counts() ->
    lists:foreach(
        fun(Key) -> ets:insert(tg20_observations, {Key, 0}) end,
        [
            published,
            edited,
            deleted,
            moment,
            review_enqueue_failed,
            review_binding_invalid,
            privacy_leak,
            remove_channel,
            remove_profile,
            private_stage
        ]
    ).

apply_schema() ->
    {ok, Ddl} = file:read_file(os:getenv("TG20_SCHEMA_FILE")),
    ok = elib_pg:with_conn(fun(Connection) ->
        Results = epgsql:squery(Connection, Ddl),
        lists:foreach(
            fun
                ({error, _}) -> error(schema_fixture_failed);
                (_) -> ok
            end,
            case is_list(Results) of
                true -> Results;
                false -> [Results]
            end
        )
    end).

seed_words() ->
    ok = textguard_export_tests:trunc(),
    lists:foreach(
        fun({Id, Word, Category, Severity}) ->
            ok = textguard_export_tests:insert_word(Id, Word, Category, Severity)
        end,
        textguard_golden:rows() ++
            [{9007199254741001, <<"comma,canonical">>, <<"test">>, <<"medium">>}]
    ).

sql(Query, Params) ->
    case elib_pg:query(Query, Params) of
        {ok, Value} -> Value;
        {error, _} -> error(fixture_query_failed)
    end.
scalar(Query, Params) ->
    [Value] = maps:values(row(Query, Params)),
    Value.
row(Query, Params) ->
    [Row] = sql(Query, Params),
    Row.
queue() -> sql(<<"SELECT * FROM review_queue ORDER BY created_at,id">>, []).
export() ->
    [{_, Revision}] = ets:lookup(tg20_observations, active_revision),
    Revision.

republish() ->
    {ok, _} = textguard_publisher:start_link(#{
        reconcile_interval_ms => 3600000,
        cas_max_retries => 0,
        publish_timeout_ms => 1000
    }),
    try
        {ok, Published} = textguard_publisher:reconcile(),
        ?assertEqual(published, maps:get(state, Published)),
        ?assertEqual(false, maps:get(pending, Published)),
        {ok, Package} = textguard_export:export_package(),
        Revision = maps:get(package_sha256, Package),
        ets:insert(tg20_observations, {active_revision, Revision}),
        Revision
    after
        textguard_publisher:stop()
    end.

apply_migration(Direction) ->
    Name =
        case Direction of
            up -> "TG20_MIGRATION_UP";
            down -> "TG20_MIGRATION_DOWN"
        end,
    {ok, Ddl} = file:read_file(os:getenv(Name)),
    elib_pg:with_tx(fun(Connection) ->
        Results = epgsql:squery(Connection, Ddl),
        lists:foreach(
            fun
                ({error, _}) -> error(migration_fixture_failed);
                (_) -> ok
            end,
            case is_list(Results) of
                true -> Results;
                false -> [Results]
            end
        ),
        ok
    end).

removal_counts() -> {count(remove_channel), count(remove_profile)}.
review_binding(Row, Metadata, Fun) ->
    Modules = [moderation_ds, channel_message_ds, user_ds],
    lists:foreach(fun(Module) -> meck:new(Module, [passthrough, no_link, no_history]) end, Modules),
    BoundRow =
        case Metadata of
            missing -> maps:remove(<<"metadata">>, Row);
            _ -> Row#{<<"metadata">> => Metadata}
        end,
    ReviewId = maps:get(<<"id">>, Row),
    meck:expect(moderation_ds, review_find, fun(Id) ->
        case Id =:= ReviewId of
            true -> {ok, BoundRow};
            false -> meck:passthrough([Id])
        end
    end),
    meck:expect(channel_message_ds, delete_checked, fun(Id, Expected) ->
        tick(remove_channel),
        meck:passthrough([Id, Expected])
    end),
    meck:expect(user_ds, update_field_checked, fun(Id, Field, Value, Expected) ->
        tick(remove_profile),
        meck:passthrough([Id, Field, Value, Expected])
    end),
    try
        Fun()
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

review_failure(id_generation, Fun) ->
    meck:new(elib_tsid, [passthrough, no_link, no_history]),
    meck:expect(elib_tsid, generate, fun
        (review_queue) -> throw(tg20_injected_id_failure);
        (Namespace) -> meck:passthrough([Namespace])
    end),
    try
        Fun()
    after
        meck:unload(elib_tsid)
    end;
review_failure(query_throw, Fun) ->
    meck:new(elib_pg, [passthrough, no_link, no_history]),
    meck:expect(elib_pg, query, fun(Query, Params) ->
        case binary:match(iolist_to_binary(Query), <<"INSERT INTO public.review_queue">>) of
            nomatch -> meck:passthrough([Query, Params]);
            _ -> throw(tg20_injected_query_failure)
        end
    end),
    try
        Fun()
    after
        meck:unload(elib_pg)
    end;
review_failure(pg_reject, Fun) ->
    sql(<<"ALTER TABLE review_queue ADD CONSTRAINT tg20_reject_fixture CHECK (false)">>, []),
    try
        Fun()
    after
        sql(<<"ALTER TABLE review_queue DROP CONSTRAINT tg20_reject_fixture">>, [])
    end.

remote_fault(http, Status, Code, Fun) ->
    Fixture = peer_start(fun(Request) ->
        Body = jsone:decode(maps:get(body, Request)),
        {reply, Status, [],
            jsone:encode(#{
                error => #{
                    code => Code,
                    request_id => maps:get(<<"request_id">>, Body),
                    retryable => true
                }
            })}
    end),
    config([{transport, http}, {http_base_url, fixture_url(Fixture)}]),
    try
        Fun(fun() -> peer_count(Fixture) end)
    after
        peer_stop(Fixture)
    end;
remote_fault(grpc, Status, Code, Fun) ->
    textguard_fault_service:set({error, Status, Code, valid}),
    config([{transport, grpc}, {grpc_channel, tg20_fault}]),
    Fun(fun textguard_fault_service:count/0).

remote_hang(http, Policy, Fun) ->
    Peer = peer_start(fun(_) ->
        receive
            tg20_release -> ok
        after 5000 -> ok
        end,
        {reply, 500, [], <<>>}
    end),
    config([
        {transport, http},
        {http_base_url, fixture_url(Peer)},
        {timeout_ms, 60},
        {failure_policy, Policy}
    ]),
    try
        Fun(fun() -> peer_count(Peer) end)
    after
        peer_stop(Peer)
    end;
remote_hang(grpc, Policy, Fun) ->
    textguard_fault_service:set(hang),
    config([
        {transport, grpc},
        {grpc_channel, tg20_fault},
        {timeout_ms, 60},
        {failure_policy, Policy}
    ]),
    Fun(fun textguard_fault_service:count/0).

private_message(Type) ->
    Modules = [msg_rate_logic, friend_ds, ai_agent_ds, group_member_logic, group_ds, msg_store_ds],
    lists:foreach(fun(Module) -> meck:new(Module, [passthrough, no_link, no_history]) end, Modules),
    Previous = application:get_env(imboy, capabilities),
    application:set_env(imboy, capabilities, #{storage_mode => secure_e2ee, e2ee_mode => required}),
    meck:expect(msg_rate_logic, check_and_record, fun(_) -> ok end),
    meck:expect(friend_ds, check_relationship, fun(_, _) -> {true, false} end),
    meck:expect(ai_agent_ds, is_agent, fun(_) -> false end),
    meck:expect(group_member_logic, check_mute, fun(_, _) -> false end),
    meck:expect(group_ds, is_member, fun(_, _) -> true end),
    meck:expect(group_ds, e2ee_mode, fun(_) -> {ok, 1} end),
    meck:expect(msg_store_ds, stage_with_sequence, fun(
        <<"c2c">>, _, <<"text">>, <<>>, Envelope, Payload, _, _, _, _, _, _, _
    ) ->
        ?assert(imboy_policy:encrypted_message_body(<<"text">>, Envelope, Payload)),
        tick(private_stage),
        {ok, duplicate, 1}
    end),
    meck:expect(msg_store_ds, stage, fun(
        <<"c2g">>, _, <<"text">>, <<>>, Envelope, Payload, _, _, _, _, _, _, _
    ) ->
        ?assert(imboy_policy:encrypted_message_body(<<"text">>, Envelope, Payload)),
        tick(private_stage),
        {ok, duplicate}
    end),
    Id = <<"tg20-private-e2ee">>,
    Protocol =
        case Type of
            c2c -> <<"olm">>;
            c2g -> <<"megolm">>
        end,
    Data = #{
        <<"to">> => integer_to_binary(outsider()),
        <<"msg_type">> => <<"text">>,
        <<"created_at">> => erlang:system_time(millisecond),
        <<"payload">> => <<"opaque-ciphertext">>,
        <<"sender_did">> => <<"tg20-device">>,
        <<"e2ee">> => #{
            <<"meta_version">> => 3,
            <<"protocol_metadata">> => #{
                <<"protocol">> => Protocol,
                <<"gid">> => outsider(),
                <<"session_id">> => <<"tg20-session">>
            }
        }
    },
    BeforeStage = count(private_stage),
    try
        ?assert(imboy_policy:message_encryption_required()),
        Module =
            case Type of
                c2c -> msg_c2c_logic;
                c2g -> msg_c2g_logic
            end,
        Function = Type,
        {ok, []} = observe(fun() -> Module:Function(Id, owner(), Data) end),
        ?assertEqual(BeforeStage + 1, count(private_stage)),
        AckType =
            case Type of
                c2c -> <<"C2C_SERVER_ACK">>;
                c2g -> <<"C2G_SERVER_ACK">>
            end,
        receive
            {reply, #{<<"id">> := Id, <<"type">> := AckType}} -> ok
        after 1000 -> error(private_entrypoint_ack_missing)
        end
    after
        case Previous of
            {ok, Value} -> application:set_env(imboy, capabilities, Value);
            undefined -> application:unset_env(imboy, capabilities)
        end,
        lists:foreach(fun meck:unload/1, Modules)
    end.

consumer_bootstrap() ->
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    {ok, Conf} = application:get_env(imboy, pg_conf),
    {epgsql, connect, [Options]} = maps:get(start_mfa, Conf),
    ?assertEqual("127.0.0.1", maps:get(host, Options)),
    ?assertEqual(list_to_integer(os:getenv("TG16_PG_PORT")), maps:get(port, Options)),
    ?assertEqual(os:getenv("TG16_PG_DB"), maps:get(database, Options)),
    ?assertEqual(os:getenv("TG16_PG_USER"), maps:get(username, Options)),
    ?assert(maps:get(password, Options) =:= os:getenv("TG16_PG_PASSWORD")),
    ok = textguard_export_tests:pg_setup(),
    ?assertEqual(
        3,
        scalar(
            <<
                "SELECT count(*) FROM information_schema.columns "
                "WHERE table_schema='public' AND (table_name,column_name) IN "
                "(('review_queue','metadata'),('channel_message','moderation_version'),('user','moderation_versions'))"
            >>,
            []
        )
    ),
    sql(
        <<
            "TRUNCATE review_queue,channel_message,channel_subscription,channel_admin,channel,"
            "moment_timeline,moment_post_acl,moment_post,user_friend,user_denylist,public.\"user\""
        >>,
        []
    ),
    ok = textguard_export_tests:pg_teardown(ok),
    ok = application:set_env(imboy, sql_driver, pgsql),
    ok = meck:new(cowboy, [passthrough, no_link, no_history]),
    ok = meck:expect(cowboy, start_clear, fun(Name, Options0, Protocol) ->
        Options1 = [
            {ip, {127, 0, 0, 1}},
            {port, 0}
            | proplists:delete(port, proplists:delete(ip, Options0))
        ],
        meck:passthrough([Name, Options1, Protocol])
    end),
    ok.

race(Operation, Mutation, State) ->
    Parent = self(),
    Gate = make_ref(),
    Fixture = peer_start(fun(Request) ->
        Parent ! {Gate, self()},
        receive
            {Gate, continue} -> forward(Request)
        after 3000 -> error(gate_timeout)
        end
    end),
    config([{transport, http}, {http_base_url, fixture_url(Fixture)}, {timeout_ms, 4000}]),
    {Worker, Monitor} = spawn_monitor(fun() -> Parent ! {Gate, result, Operation()} end),
    try
        receive
            {Gate, Handler} ->
                ok = elib_pg:with_tx(fun(Connection) ->
                    {ok, _} = elib_pg:execute(
                        Connection, <<"SET LOCAL lock_timeout = '500ms'">>, []
                    ),
                    Mutation(Connection)
                end),
                Handler ! {Gate, continue}
        after 3000 -> error(remote_gate_not_reached)
        end,
        receive
            {Gate, result, Result} ->
                receive
                    {'DOWN', Monitor, process, Worker, normal} -> ok
                end,
                ?assertEqual(1, peer_count(Fixture)),
                Result;
            {'DOWN', Monitor, process, Worker, _} ->
                error(business_worker_failed)
        after 6000 -> error(business_worker_timeout)
        end
    after
        peer_stop(Fixture),
        case is_process_alive(Worker) of
            true -> exit(Worker, kill);
            false -> ok
        end,
        erlang:demonitor(Monitor, [flush]),
        config([{transport, http}]),
        _ = State
    end.

forward(Request) ->
    Url = uri_string:parse(env_bin("TG19_HTTP_URL")),
    {ok, Connection} = gun:open(
        binary_to_list(maps:get(host, Url)),
        maps:get(port, Url),
        #{protocols => [http], retry => 0}
    ),
    try
        {ok, http} = gun:await_up(Connection, 1000),
        Headers = [
            {Key, Value}
         || {Key, Value} <- maps:get(headers, Request),
            not lists:member(Key, [<<"host">>, <<"content-length">>, <<"connection">>])
        ],
        Stream = gun:post(Connection, maps:get(path, Request), Headers, maps:get(body, Request)),
        case gun:await(Connection, Stream, 1000) of
            {response, fin, Status, _} ->
                {reply, Status, [], <<>>};
            {response, nofin, Status, _} ->
                {ok, Body} = gun:await_body(Connection, Stream, 1000),
                {reply, Status, [], Body}
        end
    after
        gun:close(Connection)
    end.

fixture_url(Fixture) ->
    <<"http://127.0.0.1:", (integer_to_binary(maps:get(port, Fixture)))/binary>>.
env_bin(Name) -> list_to_binary(os:getenv(Name)).
secret(Name) ->
    {ok, Secret} = file:read_file(os:getenv(Name)),
    string:trim(Secret).

observe(Fun) ->
    {module, moderation_detector_http} = code:ensure_loaded(moderation_detector_http),
    {module, moderation_detector_grpc} = code:ensure_loaded(moderation_detector_grpc),
    Tracer = spawn(fun() -> trace_loop([]) end),
    erlang:trace_pattern({moderation_detector_http, check, 2}, true, [local]),
    erlang:trace_pattern({moderation_detector_grpc, check, 2}, true, [local]),
    erlang:trace(self(), true, [call, {tracer, Tracer}]),
    try
        Result = Fun(),
        Delivered = erlang:trace_delivered(self()),
        receive
            {trace_delivered, _, Delivered} -> ok
        after 1000 -> error(trace_timeout)
        end,
        Tracer ! {get, self()},
        receive
            {observations, Observations} -> {Result, Observations}
        after 1000 -> error(observation_timeout)
        end
    after
        erlang:trace(self(), false, [call]),
        erlang:trace_pattern({moderation_detector_http, check, 2}, false, [local]),
        erlang:trace_pattern({moderation_detector_grpc, check, 2}, false, [local]),
        exit(Tracer, kill)
    end.

trace_loop(Acc) ->
    receive
        {trace, _, call, {Module, check, [#{scene := Scene, fields := Fields}, _]}} ->
            trace_loop([
                #{
                    transport => Module,
                    scene => Scene,
                    field_ids => [Field || {Field, _} <- Fields]
                }
                | Acc
            ]);
        {get, Parent} ->
            Parent ! {observations, lists:reverse(Acc)},
            trace_loop(Acc)
    end.

peer_start(Handler) ->
    {ok, Listener} = gen_tcp:listen(0, [
        binary,
        {active, false},
        {ip, {127, 0, 0, 1}},
        {reuseaddr, true},
        {backlog, 4}
    ]),
    {ok, Port} = inet:port(Listener),
    Counter = ets:new(tg20_peer_counter, [public, set]),
    ets:insert(Counter, {requests, 0}),
    Pid = spawn(fun() -> peer_loop(Listener, Counter, Handler) end),
    #{listener => Listener, port => Port, pid => Pid, counter => Counter}.

peer_stop(Peer) ->
    gen_tcp:close(maps:get(listener, Peer)),
    exit(maps:get(pid, Peer), kill),
    ets:delete(maps:get(counter, Peer)),
    ok.
peer_count(Peer) -> ets:lookup_element(maps:get(counter, Peer), requests, 2).
peer_loop(Listener, Counter, Handler) ->
    case gen_tcp:accept(Listener) of
        {ok, Socket} ->
            try
                Request = peer_read(Socket, <<>>),
                ets:update_counter(Counter, requests, 1),
                {reply, Status, _, Body} = Handler(Request),
                ok = gen_tcp:send(Socket, [
                    <<"HTTP/1.1 ">>,
                    integer_to_binary(Status),
                    <<" Fixture\r\nConnection: close\r\nContent-Length: ">>,
                    integer_to_binary(byte_size(Body)),
                    <<"\r\n\r\n">>,
                    Body
                ])
            catch
                _:_ -> ok
            after
                gen_tcp:close(Socket)
            end,
            peer_loop(Listener, Counter, Handler);
        {error, closed} ->
            ok
    end.

peer_read(Socket, Buffer) when byte_size(Buffer) =< 262144 ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, More} = gen_tcp:recv(Socket, 0, 5000),
            peer_read(Socket, <<Buffer/binary, More/binary>>);
        {Offset, 4} ->
            <<Head:Offset/binary, _:4/binary, Partial/binary>> = Buffer,
            [RequestLine | Lines] = binary:split(Head, <<"\r\n">>, [global]),
            [Method, Path, _] = binary:split(RequestLine, <<" ">>, [global]),
            Headers = [
                begin
                    [Key, Value] = binary:split(Line, <<":">>),
                    {string:lowercase(Key), string:trim(Value)}
                end
             || Line <- Lines
            ],
            Length = binary_to_integer(proplists:get_value(<<"content-length">>, Headers, <<"0">>)),
            true = Length >= 0 andalso Length =< 262144,
            Body =
                case Length - byte_size(Partial) of
                    Remaining when Remaining > 0 ->
                        {ok, Rest} = gen_tcp:recv(Socket, Remaining, 5000),
                        <<Partial/binary, Rest/binary>>;
                    0 ->
                        Partial
                end,
            #{method => Method, path => Path, headers => Headers, body => Body}
    end.
