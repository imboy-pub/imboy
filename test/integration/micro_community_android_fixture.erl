-module(micro_community_android_fixture).
-export([run/0, run_negative/1]).
-import(
    micro_community_owned_http_fixture,
    [assert/2, rows/3, exec/3, capture/1, report_outcome/2, committed/2]
).
-include_lib("kernel/include/file.hrl").
-define(BASE, 9007199254744000).
-define(OWNER, (?BASE + 1)).
-define(SUB, (?BASE + 3)).
-define(CHANNEL, (?BASE + 20)).
-define(GROUP, (?BASE + 10)).
-define(MESSAGE, (?BASE + 30)).
%% Run-scoped private dir override (see owned_http_fixture MC_RUN_PRIVATE_DIR).
-define(PRIVATE, mc_android_private_dir()).

mc_android_private_dir() ->
    Default = "/Users/leeyi/.codex/micro-community-runs/mc-20261008T083109Z/private",
    case os:getenv("MC_RUN_PRIVATE_DIR") of
        false -> Default;
        D -> D
    end.
-define(CONFIG, ?PRIVATE "/android-app-runtime.config").
-define(CONTEXT, ?PRIVATE "/android-context-r1.json").
-define(STOP, ?PRIVATE "/android-stop-r1").
-define(BOOT, mc_owned_android_bootstrap).

%% Root serial lease only; install/device interaction is outside this runner.
run() -> micro_community_owned_http_fixture:run(fun hold/1).

run_negative(Mode) when Mode =:= live; Mode =:= reconnect ->
    micro_community_owned_http_fixture:run(fun(Ctx) -> hold(Ctx#{negative_mode => Mode}) end).

hold(Ctx = #{conn := Conn}) ->
    private_preflight(Ctx),
    resource_preflight(Conn),
    Config = private_runtime_config(),
    KeyName = sign_key_name(Config),
    Before = rows(Conn, <<"SELECT row_to_json(c)::text FROM public.config c WHERE key=$1">>, [
        KeyName
    ]),
    Outcome = capture(fun() ->
        configure_app(Config),
        prepare_fixture(Conn),
        transport_lease(Ctx, Config)
    end),
    ExtraCleanup = capture(fun() -> cleanup_extra(Conn) end),
    SignCleanup = capture(fun() -> restore_sign_config(Conn, KeyName, Before) end),
    Cleanup =
        case {ExtraCleanup, SignCleanup} of
            {ok, ok} -> ok;
            _ -> {error, android_resource_restore_failed}
        end,
    report_outcome(Outcome, Cleanup).

private_preflight(Ctx) ->
    {ContextPath, StopPath} = lease_paths(Ctx),
    {ok, #file_info{type = directory, mode = Mode}} = file:read_link_info(?PRIVATE),
    assert(Mode band 8#077 =:= 0, android_private_directory_permissions),
    assert(file:read_link_info(ContextPath) =:= {error, enoent}, android_context_preexisting),
    assert(file:read_link_info(StopPath) =:= {error, enoent}, android_stop_preexisting),
    assert(whereis(user_server) =:= undefined, android_worker_preexisting),
    assert(ets:info(ack_retry_cache_ets) =:= undefined, android_retry_table_preexisting),
    assert(not lists:keymember(syn, 1, application:which_applications()), android_syn_preexisting).

private_runtime_config() ->
    assert(
        os:getenv("IMBOY_MICRO_ANDROID_RUNTIME_CONFIG") =:= ?CONFIG, android_config_path_required
    ),
    {ok, #file_info{type = regular, mode = Mode}} = file:read_link_info(?CONFIG),
    assert(Mode band 8#777 =:= 8#600, android_config_permissions),
    {ok, [
        #{
            solidified_key := Key,
            solidified_key_iv := IV,
            login_rsa_pub_key := RSA,
            package := <<"pub.imboy.app.mc083109">>,
            sign_key_version := <<"1">>
        } = Config
    ]} = file:consult(?CONFIG),
    assert(
        is_binary(Key) andalso byte_size(Key) =:= 32 andalso is_binary(IV) andalso
            byte_size(IV) =:= 16 andalso is_binary(RSA) andalso byte_size(RSA) > 0,
        android_config_invalid
    ),
    Config.

configure_app(#{
    solidified_key := Key,
    solidified_key_iv := IV,
    login_rsa_pub_key := RSA,
    package := Package,
    sign_key_version := Version
}) ->
    [
        application:set_env(imboy, K, V)
     || {K, V} <-
            [
                {solidified_key, Key},
                {solidified_key_iv, IV},
                {login_rsa_pub_key, RSA},
                {login_pwd_rsa_encrypt, <<"off">>},
                {init_config_legacy_cbc, <<"off">>},
                {upload_url, <<"http://127.0.0.1/disabled">>},
                {public_base_url, <<"http://127.0.0.1/disabled">>}
            ]
    ],
    ok = app_version_ds:set_sign_key(<<"android">>, Version, Package, Key),
    ok = elib_tsid:init(#{
        dc_id => 7,
        node_id => 126,
        dc_bits => 3,
        names => [group_log, group_member, channel_subscription, channel_message_view, conversation]
    }).

sign_key_name(#{package := Package, sign_key_version := Version}) ->
    <<Package/binary, "_android_", Version/binary>>.

resource_preflight(Conn) ->
    Users = [?BASE + N || N <- [1, 2, 3, 4]],
    assert(
        rows(
            Conn,
            <<
                "SELECT count(*) FROM public.user_friend WHERE "
                "from_user_id=ANY($1::bigint[]) OR to_user_id=ANY($1::bigint[])"
            >>,
            [Users]
        ) =:= [{0}],
        android_friend_collision
    ),
    assert(
        rows(Conn, <<"SELECT count(*) FROM public.push_token WHERE user_id=ANY($1::bigint[])">>, [
            Users
        ]) =:= [{0}],
        android_push_collision
    ),
    assert(
        rows(
            Conn,
            <<"SELECT user_id,device_id FROM public.user_device WHERE user_id=ANY($1::bigint[]) ORDER BY user_id">>,
            [Users]
        ) =:=
            [{Uid, <<"mc-http-", (integer_to_binary(Uid))/binary>>} || Uid <- Users],
        android_foreign_device
    ),
    assert(extra_snapshot(Conn) =:= [[{0}], [{0}], [{0}]], android_extra_collision).

prepare_fixture(Conn) ->
    committed(Conn, fun(Tx) ->
        exec(
            Tx,
            <<
                "UPDATE public.channel SET visibility=0,access_type=0,"
                "discussion_group_id=$2,name='MC Android micro-community' WHERE id=$1"
            >>,
            [?CHANNEL, ?GROUP]
        ),
        exec(
            Tx,
            <<
                "UPDATE public.channel_subscription SET status=0 WHERE id=$1 "
                "AND channel_id=$2 AND user_id=$3"
            >>,
            [?BASE + 51, ?CHANNEL, ?SUB]
        ),
        exec(
            Tx,
            <<
                "UPDATE public.channel_message SET is_essence=true,"
                "payload=jsonb_set(payload,'{tags}',$2::jsonb,true) WHERE id=$1 AND channel_id=$3"
            >>,
            [?MESSAGE, jsone:encode([<<"mc-android">>]), ?CHANNEL]
        ),
        assert(
            rows(
                Tx,
                <<"SELECT count(*) FROM public.group_member WHERE group_id=$1 AND user_id=$2">>,
                [?GROUP, ?SUB]
            ) =:= [{0}],
            android_sub_already_joined
        ),
        ok
    end).

transport_lease(Ctx, Config) ->
    Outcome = capture(fun() ->
        ok = imboy_syn:init(),
        ok = ack_retry_cache:init_table(),
        {ok, Worker} = user_server:start_link(),
        put(android_owned_worker, Worker),
        Ctx1 = start_negative(Ctx, Config),
        Dispatch = cowboy_router:compile([{'_', mode_routes(Ctx1)}]),
        ok = cowboy:set_env(mc_owned_http_listener, dispatch, Dispatch),
        bootstrap_lease(Ctx1, Config)
    end),
    Cleanup = capture(fun stop_transport/0),
    report_outcome(Outcome, Cleanup).

stop_transport() ->
    Results = [
        capture(Fun)
     || Fun <- [
            fun() ->
                _ = cowboy:stop_listener(?BOOT),
                ok
            end,
            fun() ->
                _ = cowboy:stop_listener(mc_owned_http_listener),
                ok
            end,
            fun stop_negative/0,
            fun stop_worker/0,
            fun stop_retry_timer/0,
            fun() ->
                _ = application:stop(syn),
                ok
            end
        ]
    ],
    assert(lists:all(fun(Result) -> Result =:= ok end, Results), android_transport_cleanup_failed),
    ok.

bootstrap_lease(Ctx = #{conn := Conn, port := Port}, Config) ->
    Secret = binary:encode_hex(crypto:strong_rand_bytes(32)),
    Consumed = atomics:new(1, []),
    Issue = fun(Did) ->
        Payload = issue_session(Conn, Did),
        case maps:get(negative_probe, Ctx, undefined) of
            undefined -> ok;
            Probe -> ok = micro_community_android_negative_probe:session(Probe, Payload)
        end,
        Payload
    end,
    SessionRoutes = [
        {"/mc-owned/android/session", micro_community_android_session_handler, #{
            secret => Secret, consumed => Consumed, issue => Issue
        }}
    ],
    {ControlRoutes, ControlData} = negative_routes(Ctx),
    Dispatch = cowboy_router:compile([{'_', SessionRoutes ++ ControlRoutes}]),
    {ok, _} = cowboy:start_clear(
        ?BOOT,
        #{socket_opts => [{ip, {127, 0, 0, 1}}, {port, 0}], num_acceptors => 1},
        #{env => #{dispatch => Dispatch}}
    ),
    BootPort = ranch:get_port(?BOOT),
    Data = #{
        api_port => Port,
        bootstrap_port => BootPort,
        bootstrap_key => Secret,
        bootstrap_path => <<"/mc-owned/android/session">>,
        uid => integer_to_binary(?SUB),
        channel_id => integer_to_binary(?CHANNEL),
        group_id => integer_to_binary(?GROUP),
        message_id => integer_to_binary(?MESSAGE),
        product_profile => <<"community">>,
        package => maps:get(package, Config),
        sign_key_version => maps:get(sign_key_version, Config)
    },
    private_hold(Ctx, maps:merge(Data, ControlData)).

private_hold(Ctx, Data) ->
    {ContextPath, StopPath} = lease_paths(Ctx),
    {ok, File} = file:open(ContextPath, [write, exclusive, binary, raw]),
    try
        ok = file:change_mode(ContextPath, 8#600),
        ok = file:write(File, jsone:encode(Data)),
        io:format("MC_ANDROID_LEASE_READY uid=9007199254744003~n"),
        wait_stop(erlang:monotonic_time(millisecond) + 600000, StopPath),
        mode_final(Ctx)
    after
        file:close(File),
        file:delete(ContextPath),
        file:delete(StopPath)
    end.

issue_session(Conn, Did) ->
    committed(Conn, fun(Tx) ->
        exec(
            Tx,
            <<"UPDATE public.user_device SET device_id=$1 WHERE id=$2 AND user_id=$3">>,
            [Did, ?SUB + 100, ?SUB]
        ),
        assert(
            rows(
                Tx,
                <<"SELECT count(*) FROM public.user_device WHERE user_id=$1 AND device_id=$2">>,
                [?SUB, Did]
            ) =:= [{1}],
            android_device_bind_failed
        ),
        ok
    end),
    Profile = user_ds:find_by_id(?SUB, <<
        "id,account,nickname,avatar,background,gender,region,"
        "sign,birthday,profession,school,interests,status,mobile,email"
    >>),
    assert(
        maps:get(<<"mobile">>, Profile) =:= null andalso maps:get(<<"email">>, Profile) =:= null,
        android_profile_contact_rejected
    ),
    #{
        uid => integer_to_binary(?SUB),
        user_id => integer_to_binary(?SUB),
        token => token_ds:encrypt_token(?SUB, Did),
        refreshtoken => token_ds:encrypt_refreshtoken(?SUB, Did),
        did => Did,
        device_id => Did,
        profile => maps:remove(<<"id">>, Profile),
        channel_id => integer_to_binary(?CHANNEL),
        group_id => integer_to_binary(?GROUP),
        message_id => integer_to_binary(?MESSAGE),
        product_profile => <<"community">>
    }.

app_routes() ->
    Wanted = [
        "/api/v1/init",
        "/api/v1/refreshtoken",
        "/api/v1/app/features",
        "/api/v1/app/manifest",
        "/api/v1/app/policy",
        "/api/v1/channels/discover",
        "/api/v1/channels/categories",
        "/api/v1/channels/subscribed",
        "/api/v1/channels/managed",
        "/api/v1/channels/unread/summary",
        "/api/v1/channel/:channel_id",
        "/api/v1/channel/:channel_id/messages",
        "/api/v1/channel/:channel_id/subscribe",
        "/api/v1/channel/:channel_id/discussion",
        "/api/v1/channel/:channel_id/discussion/join",
        "/api/v1/channel/:channel_id/posts",
        "/api/v1/channel/:channel_id/read",
        "/api/v1/group/detail",
        "/api/v1/group/page",
        "/api/v1/group_member/page",
        "/api/v1/group/msg_page",
        "/api/v1/msg/history",
        "/api/v1/msg/offline",
        "/api/v1/conversation/mine",
        "/api/v1/conversation/pinned",
        "/api/v1/ws",
        "/api/v1/user/show"
    ],
    Routes = [
        R
     || {_, Rs} <- imboy_router:get_routes(), R = {Path, _, _} <- Rs, lists:member(Path, Wanted)
    ],
    assert(length(Routes) =:= length(Wanted), android_route_whitelist_mismatch),
    Routes.

wait_stop(Deadline, StopPath) ->
    assert(erlang:monotonic_time(millisecond) < Deadline, android_lease_timeout),
    case file:read_link_info(StopPath) of
        {ok, #file_info{type = regular}} ->
            assert(file:read_file(StopPath) =:= {ok, <<"stop">>}, android_stop_invalid);
        {error, enoent} ->
            receive
            after 250 -> wait_stop(Deadline, StopPath)
            end;
        _ ->
            assert(false, android_stop_invalid)
    end.

final_oracles(#{conn := Conn}) ->
    assert(
        rows(
            Conn,
            <<
                "SELECT count(*) FROM public.channel_subscription WHERE channel_id=$1 "
                "AND user_id=$2 AND status=1"
            >>,
            [?CHANNEL, ?SUB]
        ) =:= [{1}],
        android_subscription_final
    ),
    assert(
        rows(
            Conn,
            <<
                "SELECT role,status,is_join,join_mode FROM public.group_member WHERE "
                "group_id=$1 AND user_id=$2"
            >>,
            [?GROUP, ?SUB]
        ) =:= [{1, 1, true, <<"channel_discussion">>}],
        android_member_final
    ),
    assert(
        rows(
            Conn,
            <<
                "SELECT generation_no,end_seq FROM public.group_member_generation WHERE "
                "group_id=$1 AND user_id=$2"
            >>,
            [?GROUP, ?SUB]
        ) =:= [{1, null}],
        android_generation_final
    ),
    assert(
        rows(
            Conn,
            <<"SELECT discussion_group_id,visibility,access_type FROM public.channel WHERE id=$1">>,
            [?CHANNEL]
        ) =:= [{?GROUP, 0, 0}],
        android_binding_final
    ),
    final_content_oracle(Conn),
    io:format("MC_ANDROID_DB_SUBORACLES_PASS subscription=1 member=1 generation=1 bound=1~n"),
    ok.

final_content_oracle(Conn) ->
    assert(
        rows(
            Conn,
            <<"SELECT is_essence,is_pinned,payload->'tags'='[\"mc-android\"]'::jsonb FROM public.channel_message WHERE id=$1">>,
            [?MESSAGE]
        ) =:= [{true, true, true}],
        android_content_final
    ).

extra_snapshot(Conn) ->
    [
        rows(
            Conn, <<"SELECT count(*) FROM public.conversation WHERE user_id BETWEEN $1 AND $2">>, [
                ?BASE, ?BASE + 999
            ]
        ),
        rows(
            Conn,
            <<
                "SELECT count(*) FROM public.channel_message_view WHERE user_id BETWEEN $1 AND $2 "
                "OR channel_id BETWEEN $1 AND $2"
            >>,
            [?BASE, ?BASE + 999]
        ),
        rows(
            Conn,
            <<
                "SELECT count(*) FROM public.workspace_group_read_cursor r JOIN public.group_member_generation g "
                "ON g.id=r.generation_id WHERE g.group_id BETWEEN $1 AND $2"
            >>,
            [?BASE, ?BASE + 999]
        )
    ].

cleanup_extra(Conn) ->
    committed(Conn, fun(Tx) ->
        exec(
            Tx,
            <<
                "DELETE FROM public.workspace_group_read_cursor r USING public.group_member_generation g "
                "WHERE r.generation_id=g.id AND g.group_id IN($1,$2)"
            >>,
            [?GROUP, ?BASE + 11]
        ),
        exec(
            Tx,
            <<
                "DELETE FROM public.channel_message_view WHERE channel_id=ANY($1::bigint[]) "
                "AND user_id=ANY($2::bigint[])"
            >>,
            [[?BASE + N || N <- [20, 21, 22, 23]], [?BASE + N || N <- [1, 2, 3, 4]]]
        ),
        exec(Tx, <<"DELETE FROM public.conversation WHERE user_id=ANY($1::bigint[])">>, [
            [?BASE + N || N <- [1, 2, 3, 4]]
        ]),
        ok
    end),
    assert(extra_snapshot(Conn) =:= [[{0}], [{0}], [{0}]], android_cleanup_extra_residue),
    io:format("MC_ANDROID_EXTRA_CLEANUP_COMPLETED~n"),
    ok.

restore_sign_config(Conn, KeyName, Before) ->
    committed(Conn, fun(Tx) ->
        exec(Tx, <<"DELETE FROM public.config WHERE key=$1">>, [KeyName]),
        [
            exec(
                Tx,
                <<"INSERT INTO public.config SELECT (json_populate_record(NULL::public.config,$1::json)).*">>,
                [Raw]
            )
         || {Raw} <- Before
        ],
        ok
    end),
    ok = imboy_cache:flush({config5, KeyName}),
    assert(
        rows(Conn, <<"SELECT row_to_json(c)::text FROM public.config c WHERE key=$1">>, [KeyName]) =:=
            Before,
        android_sign_config_restore_mismatch
    ).

stop_worker() ->
    case erase(android_owned_worker) of
        Pid when is_pid(Pid) ->
            Ref = erlang:monitor(process, Pid),
            ok = gen_server:stop(Pid),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> assert(false, android_worker_stop_timeout)
            end;
        _ ->
            ok
    end.

stop_retry_timer() ->
    case ets:info(ack_retry_cache_ets, owner) of
        undefined -> ok;
        Owner when Owner =:= self() -> true = ets:delete(ack_retry_cache_ets);
        _ -> assert(false, android_retry_table_owner_changed)
    end,
    case persistent_term:get(ack_retry_cache_cleanup_timer, undefined) of
        undefined ->
            ok;
        Ref ->
            timer:cancel(Ref),
            persistent_term:erase(ack_retry_cache_cleanup_timer)
    end,
    ok.

lease_paths(#{negative_mode := Mode}) ->
    Suffix = atom_to_list(Mode),
    {
        ?PRIVATE ++ "/android-negative-" ++ Suffix ++ "-context-r1.json",
        ?PRIVATE ++ "/android-negative-" ++ Suffix ++ "-stop-r1"
    };
lease_paths(_) ->
    {?CONTEXT, ?STOP}.

start_negative(#{negative_mode := _} = Ctx, Config) ->
    {ok, Probe} = micro_community_android_negative_probe:start_link(Ctx, Config),
    put(android_negative_probe, {Probe, Ctx}),
    Ctx#{negative_probe => Probe};
start_negative(Ctx, _) ->
    Ctx.

stop_negative() ->
    case erase(android_negative_probe) of
        undefined -> ok;
        {Probe, Ctx} -> micro_community_android_negative_probe:stop(Probe, Ctx)
    end.

mode_routes(#{negative_mode := _}) ->
    Extra = [
        R
     || {_, Rs} <- imboy_router:get_routes(),
        R = {Path, Handler, State} <- Rs,
        Path =:= "/api/v1/channel/:channel_id/unsubscribe",
        Handler =:= channel_handler,
        maps:get(action, State, undefined) =:= unsubscribe
    ],
    assert(length(Extra) =:= 1, negative_unsubscribe_route_mismatch),
    app_routes() ++ Extra;
mode_routes(_) ->
    app_routes().

negative_routes(#{negative_probe := Probe, negative_mode := Mode}) ->
    PhaseKey = binary:encode_hex(crypto:strong_rand_bytes(32)),
    ControlKey = binary:encode_hex(crypto:strong_rand_bytes(32)),
    Routes = [
        {"/mc-owned/android/phase", micro_community_android_negative_control_handler, #{
            role => phase, secret => PhaseKey, probe => Probe
        }},
        {"/mc-owned/android/control", micro_community_android_negative_control_handler, #{
            role => control, secret => ControlKey, probe => Probe
        }}
    ],
    {Routes, #{
        negative_mode => Mode,
        phase_key => PhaseKey,
        control_key => ControlKey,
        phase_path => <<"/mc-owned/android/phase">>,
        control_path => <<"/mc-owned/android/control">>
    }};
negative_routes(_) ->
    {[], #{}}.

mode_final(#{negative_probe := Probe}) ->
    assert(
        micro_community_android_negative_probe:final(Probe) =:= ok, negative_final_oracle_failed
    );
mode_final(Ctx) ->
    final_oracles(Ctx).
