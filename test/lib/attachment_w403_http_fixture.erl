%%% Real W4-03 application HTTP behind TLS termination. The advertised HTTPS
%%% origin is reserved; only its unchanged path/query is forwarded to our owned
%%% loopback listener. This does not test TLS transport or bypass certificates.
%%% All credentials, authorization facts, handlers, PG and storage are real.
-module(attachment_w403_http_fixture).
-behaviour(cowboy_middleware).
-compile({no_auto_import, [put/2]}).
-export([
    setup/0,
    cleanup/1,
    execute/2,
    register/4,
    put/2,
    confirm/1,
    read/1,
    roundtrip/3,
    evidence/1,
    overlap/3,
    inflight_overlap/3,
    request/6,
    success/1,
    max_bytes/1,
    %% W6-01 C 组（W2-01/W2-02/W2-03）追加入口：恢复 API（status/renew）、
    %% multipart 直传、旧客户端 confirm、过期/裸能力 PUT 与 SQL 直通。
    %% 只增不改：上方导出与实现保持 W4-03 验收原样。
    presign_op/4,
    status/1,
    renew/1,
    multipart_register/2,
    multipart_put/3,
    confirm_with/3,
    confirm_legacy/1,
    expired_put/2,
    bare_put/2,
    q/2
]).
-include_lib("eunit/include/eunit.hrl").
-define(LISTENER, attachment_w403_http).
-define(TABLE, attachment_w403_http_evidence).
-define(ORIGIN, <<"https://w403.invalid">>).

setup() ->
    %% The common owner verifies a reused pool; evidence/listener remain ours.
    undefined = ets:whereis(?TABLE),
    try
        S0 = attachment_lifecycle_v5_w403_gateway_protocol_tests:setup(),
        erlang:put(w403_setup_resources, S0),
        true = ets:new(?TABLE, [named_table, public, ordered_set]) =:= ?TABLE,
        S = S0#{evidence_table_owned => ets:whereis(?TABLE)},
        erlang:put(w403_setup_resources, S),
        setup_throttle(),
        own_resource(previous_gateway_tmp, os:getenv("IMBOY_V5_GATEWAY_TMP")),
        Tmp = filename:join(maps:get(root, S), "gateway-tmp"),
        ok = file:make_dir(Tmp),
        ok = file:change_mode(Tmp, 8#700),
        true = os:putenv("IMBOY_V5_GATEWAY_TMP", Tmp),
        application:set_env(imboy, api_auth_switch, <<"on">>),
        application:set_env(imboy, base_url, ?ORIGIN),
        application:set_env(
            imboy,
            enterprise_internal_rate_limits,
            #{internal_read => 10000, internal_write => 10000, internal_sso => 10000}
        ),
        application:unset_env(imboy, eb_asset_object_store),
        _ = cs_pg_test_fixture:install_keyring(),
        S1 = seed(erlang:get(w403_setup_resources)),
        erlang:put(w403_setup_resources, S1),
        Dispatch = cowboy_router:compile(imboy_router:get_routes()),
        {ok, Listener} = cowboy:start_clear(
            ?LISTENER,
            [{ip, {127, 0, 0, 1}}, {port, 0}],
            #{
                env => #{dispatch => Dispatch},
                middlewares => [cowboy_router, ?MODULE, auth_middleware, cowboy_handler]
            }
        ),
        erlang:put(w403_setup_resources, S1#{listener_owned => Listener}),
        S1#{
            listener_owned => Listener,
            http_base =>
                <<"http://127.0.0.1:", (integer_to_binary(ranch:get_port(?LISTENER)))/binary>>
        }
    catch
        Class:Reason:Stack ->
            cleanup(
                (case erase(w403_setup_resources) of
                    M when is_map(M) -> M;
                    _ -> #{}
                end)
            ),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(S) ->
    try
        case maps:get(listener_owned, S, undefined) of
            Listener when is_pid(Listener) ->
                best_effort(fun() ->
                    case ranch:info(?LISTENER) of
                        #{pid := Listener} -> cowboy:stop_listener(?LISTENER);
                        _ -> ok
                    end
                end);
            _ ->
                ok
        end,
        %% Objects are unique to this run. Keep PG rows for failed-run diagnostics;
        %% the manifest database lifecycle belongs to the outer owned-DB runner.
        case {maps:get(evidence_table_owned, S, undefined), ets:whereis(?TABLE)} of
            {Table, Table} when Table =/= undefined ->
                [
                    best_effort(fun() -> attachment_storage:delete_snapshot(I) end)
                 || {{object, _}, I} <- ets:tab2list(Table)
                ],
                ets:delete(Table);
            _ ->
                ok
        end
    after
        try
            cleanup_throttle(S)
        after
            case maps:find(previous_gateway_tmp, S) of
                {ok, false} -> os:unsetenv("IMBOY_V5_GATEWAY_TMP");
                {ok, Previous} -> os:putenv("IMBOY_V5_GATEWAY_TMP", Previous);
                error -> ok
            end,
            attachment_lifecycle_v5_w403_gateway_protocol_tests:cleanup(S),
            erase(w403_setup_resources)
        end
    end,
    ok.

own_resource(Key, Value) ->
    erlang:put(w403_setup_resources, (erlang:get(w403_setup_resources))#{Key => Value}),
    ok.

%% A stopped throttle application can leave the production setup marker behind.
%% An already-running application can own scope timers and counters. Preserve
%% both: pause only the three old scope timers, use real owned throttle children,
%% then restore the untouched old counters/markers before resuming their owners.
setup_throttle() ->
    throttle_ets = application:get_env(throttle, driver, throttle_ets),
    Buckets = enterprise_internal_rate:buckets(),
    Before = [{B, persistent_value({enterprise_internal_rate, B})} || B <- Buckets],
    own_resource(throttle_markers, Before),
    {ok, Started} = application:ensure_all_started(throttle),
    own_resource(throttle_owned_apps, lists:reverse(Started)),
    own_resource(throttle_suspended, []),
    own_resource(throttle_children, []),
    lists:foreach(
        fun({_, Pid, worker, [throttle]}) ->
            {Scope, _, _} = sys:get_state(Pid, 5000),
            case lists:member(Scope, Buckets) of
                true ->
                    ok = sys:suspend(Pid, 5000),
                    S = erlang:get(w403_setup_resources),
                    own_resource(throttle_suspended, [Pid | maps:get(throttle_suspended, S)]);
                false ->
                    ok
            end
        end,
        supervisor:which_children(throttle_sup)
    ),
    Rows = [{B, ets:lookup(throttle_state_table, B)} || B <- Buckets],
    own_resource(throttle_scope_rows, Rows),
    lists:foreach(
        fun(B) ->
            {ok, Child} = supervisor:start_child(throttle_sup, [B, 10000, per_minute]),
            S = erlang:get(w403_setup_resources),
            own_resource(throttle_children, [Child | maps:get(throttle_children, S)]),
            persistent_term:put({enterprise_internal_rate, B}, 10000)
        end,
        Buckets
    ).

persistent_value(Key) ->
    try
        {present, persistent_term:get(Key)}
    catch
        error:badarg -> absent
    end.

cleanup_throttle(S) ->
    try
        [
            best_effort(fun() -> supervisor:terminate_child(throttle_sup, P) end)
         || P <- maps:get(throttle_children, S, [])
        ],
        try
            [
                case Rows of
                    [] -> ets:delete(throttle_state_table, Bucket);
                    _ -> ets:insert(throttle_state_table, Rows)
                end
             || {Bucket, Rows} <- maps:get(throttle_scope_rows, S, [])
            ]
        after
            [
                case Previous of
                    absent ->
                        persistent_term:erase({enterprise_internal_rate, Bucket});
                    {present, Value} ->
                        persistent_term:put({enterprise_internal_rate, Bucket}, Value)
                end
             || {Bucket, Previous} <- maps:get(throttle_markers, S, [])
            ]
        end
    after
        [
            best_effort(fun() -> sys:resume(P, 5000) end)
         || P <- maps:get(throttle_suspended, S, [])
        ],
        [
            best_effort(fun() -> application:stop(App) end)
         || App <- maps:get(throttle_owned_apps, S, [])
        ]
    end.

best_effort(Fun) ->
    try
        Fun()
    catch
        _:_ -> ok
    end.

seed(S) ->
    Scope = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Identity = maps:get(service_identity_id, Scope),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,function_key='customer_service' WHERE organization_id=$1 AND user_id=$2">>,
        [Org, Actor, Identity]
    ),
    {ok, Session} = cs_session_app:open_session(Org, #{
        workspace_id => Ws,
        contact_id => maps:get(contact_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        at => os:system_time(second)
    }),
    SessionId = maps:get(id, Session),
    {ok, _} = cs_session_app:claim(Org, #{
        workspace_id => Ws,
        session_id => SessionId,
        expected_version => 1,
        business_identity_id => Identity,
        actor_user_id => Actor,
        at => os:system_time(second)
    }),
    Did = <<"w403-device-", (maps:get(run, S))/binary>>,
    own_resource(owned_cache_keys, [{user_device_active, Actor, Did}]),
    ok = qr_login_logic:log_device_login(Actor, Did, <<"w403">>, <<"web">>),
    SeatToken = token_ds:encrypt_seat_token(Actor, Did),
    Installation = cs_pg_test_fixture:id(),
    Visitor = binary:encode_hex(crypto:strong_rand_bytes(32), lowercase),
    {ok, _} = cs_pg_widget:insert_widget_installation(Org, #{
        id => Installation,
        public_widget_id => integer_to_binary(Installation),
        display_name => <<"w403">>,
        allowed_origins => [],
        branding => #{},
        consent_version => <<"w403-v1">>
    }),
    {ok, _} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
        id => cs_pg_test_fixture:id(),
        contact_id => maps:get(contact_id, Scope),
        token_digest => cs_widget_support:token_digest(#{}, Visitor),
        expires_at => os:system_time(second) + 3600,
        widget_installation_id => Installation
    }),
    application:set_env(imboy, postgre_aes_key, crypto:strong_rand_bytes(32)),
    {ok, Credential} = elib_pg:with_tx(fun(C) ->
        {ok, App} = enterprise_application_repo:create_tx(
            C,
            Org,
            <<"w403-", (maps:get(run, S))/binary>>,
            <<"w403 application">>,
            Actor
        ),
        AppId = maps:get(<<"id">>, App),
        {ok, #{credential := Cred}} = enterprise_internal_ops:issue_credential_tx(
            C, Org, AppId, binary:encode_hex(crypto:strong_rand_bytes(32), lowercase), undefined
        ),
        {ok, Cred}
    end),
    SignKey = binary:encode_hex(crypto:strong_rand_bytes(32), lowercase),
    Vsn = binary:part(maps:get(run, S), 0, 12),
    ConfigKey = <<"pub.imboy.w403_android_", Vsn/binary>>,
    CacheKeys = [
        {config5, ConfigKey},
        {config5, ConfigKey, uncached_v1},
        {user_device_active, Actor, Did}
    ],
    own_resource(owned_cache_keys, CacheKeys),
    {ok, [_]} = elib_pg:query(
        <<"INSERT INTO config(tab,key,value,title) VALUES('sys',$1,'aes_cbc_' || encode(encrypt(encode($2::bytea,'base64')::bytea,$3,'aes-cbc/pad:pkcs'),'base64'),'w403') RETURNING key">>,
        [ConfigKey, jsone:encode(SignKey), config_ds:env(postgre_aes_key)]
    ),
    Plain = <<Did/binary, "|", Vsn/binary, "|android|pub.imboy.w403">>,
    SignHeaders = #{
        <<"did">> => Did,
        <<"vsn">> => Vsn,
        <<"cos">> => <<"android">>,
        <<"pkg">> => <<"pub.imboy.w403">>,
        <<"sk">> => Vsn,
        <<"method">> => <<"sha256">>,
        <<"sign">> => elib_hasher:hmac_sha256(Plain, SignKey)
    },
    S#{
        scope => Scope,
        session_id => SessionId,
        installation_id => Installation,
        owned_cache_keys => CacheKeys,
        ordinary_headers => SignHeaders#{
            <<"authorization">> => bearer(token_ds:encrypt_token(maps:get(uid, S)))
        },
        seat_headers => #{<<"authorization">> => bearer(SeatToken)},
        internal_headers => #{signed_credential => Credential},
        widget_headers => #{<<"x-cs-visit-token">> => Visitor}
    }.

register(S0, Surface, Driver, Bytes) ->
    Leaf = maps:get(leaf, S0, elib_uuid:gen_v7()),
    S = S0#{leaf => Leaf},
    Ns =
        case Driver of
            disk -> <<"w403-disk">>;
            garage -> <<"w403-garage">>
        end,
    Registry = attachment_storage:registry(),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{upload_namespaces => #{<<"private">> => Ns, <<"enterprise">> => Ns}}
    ),
    Cid = maps:get(registration_cid, S, elib_uuid:gen_v7()),
    {Method, Path, Body, Headers} = registration_request(S, Surface, Bytes, Cid),
    %% Every leaf proves the named entrypoint denies a missing principal.
    Denied = request(
        S,
        Method,
        Path,
        Body,
        maps:without(
            [signed_credential, <<"authorization">>, <<"x-cs-visit-token">>], Headers
        ),
        <<"application/json">>
    ),
    ?assertNot(success(Denied)),
    Registered = request(S, Method, Path, Body, Headers, <<"application/json">>),
    ?assert(success(Registered)),
    Data = payload(Registered),
    {Key, I, Upload, ConfirmBody} =
        case Surface of
            Ordinary when Ordinary =:= ordinary; Ordinary =:= enterprise_internal ->
                K = maps:get(<<"object_key">>, Data),
                {ok, Location} = attachment_storage:session_location(K),
                CB =
                    case Surface of
                        ordinary -> #{object_key => K, scope => <<"private">>};
                        enterprise_internal -> #{object_key => K}
                    end,
                {K, Location, maps:get(<<"put_url">>, Data), CB};
            _ ->
                Id = int(maps:get(<<"asset_id">>, Data)),
                Scope = maps:get(scope, S),
                {ok, Row} = eb_pg_asset_meta:fetch_asset(
                    maps:get(org_id, Scope),
                    maps:get(workspace_id, Scope),
                    Id
                ),
                K = maps:get(object_key, Row),
                Frozen = maps:from_list([
                    {atom_to_binary(F), maps:get(F, Row)}
                 || F <-
                        [
                            storage_driver,
                            storage_namespace,
                            storage_container,
                            storage_locator_sha256
                        ]
                ]),
                {ok, Location} = attachment_storage:frozen_location(K, Frozen),
                Base = #{upload_ref => maps:get(<<"upload_ref">>, Data)},
                CB =
                    case Surface of
                        enterprise_seat -> Base#{workspace_id => maps:get(workspace_id, Scope)};
                        cs_widget -> Base#{installation_id => maps:get(installation_id, S)}
                    end,
                {K, Location, maps:get(<<"url">>, maps:get(<<"upload">>, Data)), CB#{
                    asset_id => Id
                }}
        end,
    ?assertEqual(Driver, maps:get(driver, I)),
    true = ets:insert(?TABLE, {{object, Key}, I}),
    #{
        fixture => S,
        leaf => Leaf,
        surface => Surface,
        driver => Driver,
        key => Key,
        location => I,
        upload_url => Upload,
        headers => Headers,
        confirm_body => ConfirmBody,
        registration => Data,
        client_upload_id => Cid
    }.

registration_request(S, ordinary, _Bytes, Cid) ->
    Q = uri_string:compose_query([
        {<<"filename">>, <<"w403.txt">>},
        {<<"mime_type">>, <<"text/plain">>},
        {<<"client_upload_id">>, Cid}
    ]),
    {get, <<"/api/v1/attachment/presign?", Q/binary>>, <<>>, maps:get(ordinary_headers, S)};
registration_request(S, enterprise_internal, Bytes, Cid) ->
    {post, <<"/api/internal/v1/files/presign">>,
        #{
            file_name => <<"w403.txt">>,
            mime_type => <<"text/plain">>,
            size_bytes => byte_size(Bytes),
            client_upload_id => Cid
        },
        maps:get(internal_headers, S)};
registration_request(S, Surface, Bytes, Cid) ->
    Base = #{
        mime => <<"text/plain">>,
        size_bytes => byte_size(Bytes),
        object_hash => eb_asset_content:sha256_hex(Bytes),
        file_name => <<"w403.txt">>,
        client_upload_id => Cid
    },
    Scope = maps:get(scope, S),
    case Surface of
        enterprise_seat ->
            {post, seat_path(S, <<"presign">>),
                Base#{
                    workspace_id => maps:get(workspace_id, Scope),
                    conversation_id => maps:get(conversation_id, Scope)
                },
                maps:get(seat_headers, S)};
        cs_widget ->
            {post, widget_path(S, <<"presign">>),
                Base#{installation_id => maps:get(installation_id, S)}, maps:get(widget_headers, S)}
    end.

put(#{fixture := S, surface := Surface, upload_url := Url} = Session, Bytes) ->
    Headers =
        case Surface of
            enterprise_seat -> maps:get(headers, Session);
            _ -> #{}
        end,
    request(S, put, Url, Bytes, Headers, <<"text/plain">>).

confirm(#{fixture := S, surface := Surface, confirm_body := Body, headers := Headers}) ->
    Path =
        case Surface of
            ordinary -> <<"/api/v1/attachment/confirm">>;
            enterprise_internal -> <<"/api/internal/v1/files/confirm">>;
            enterprise_seat -> seat_path(S, <<"confirm">>);
            cs_widget -> widget_path(S, <<"confirm">>)
        end,
    request(S, post, Path, Body, Headers, <<"application/json">>).

read(#{location := I, driver := Driver}) ->
    Mod = attachment_storage:driver_module(atom_to_binary(Driver)),
    Mod:read_snapshot(I).

roundtrip(S, Surface, Driver) ->
    Bytes = <<"w403 authenticated immutable content">>,
    Session = register(S, Surface, Driver, Bytes),
    ?assertEqual({error, not_found}, read(Session)),
    ?assert(success(put(Session, Bytes))),
    ?assertEqual({ok, Bytes}, read(Session)),
    ?assert(success(confirm(Session))),
    ?assertNot(success(put(Session, <<"different bytes">>))),
    ?assertEqual({ok, Bytes}, read(Session)),
    Session.

%% Observed after Cowboy routing, before real authentication. Never record query
%% strings, capability tokens, headers, response bodies or storage credentials.
execute(Req, Env) ->
    case cowboy_req:header(<<"x-w403-request-id">>, Req) of
        <<"w403-", _/binary>> = Id ->
            true = ets:insert(
                ?TABLE,
                {{observed, Id}, #{
                    request_id => Id,
                    route => cowboy_req:path(Req),
                    method => cowboy_req:method(Req),
                    handler => atom_to_binary(maps:get(handler, Env)),
                    server_process => list_to_binary(pid_to_list(self())),
                    connection_id => list_to_binary(pid_to_list(maps:get(pid, Req))),
                    observed_at_us => erlang:monotonic_time(microsecond)
                }}
            );
        _ ->
            ok
    end,
    {ok, Req, Env}.

request(S, Method, Target, Body0, Headers0, Mime) ->
    Path = local_target(Target),
    Id = <<"w403-", (elib_uuid:gen_v7())/binary>>,
    BaseHeaders = Headers0#{
        <<"x-w403-request-id">> => Id,
        <<"idempotency-key">> => Id,
        <<"connection">> => <<"close">>
    },
    Url = binary_to_list(<<(maps:get(http_base, S))/binary, Path/binary>>),
    Body =
        case Body0 of
            M when is_map(M) -> jsone:encode(M);
            B -> B
        end,
    Headers =
        case maps:find(signed_credential, BaseHeaders) of
            {ok, Credential} ->
                intbe02_http_support:signed_headers(
                    Credential,
                    string:uppercase(atom_to_binary(Method)),
                    Path,
                    Body,
                    (maps:remove(signed_credential, BaseHeaders))#{<<"content-type">> => Mime},
                    sha256
                );
            error ->
                BaseHeaders
        end,
    H = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- maps:to_list(Headers)],
    Req =
        case Method of
            get -> {Url, H};
            head -> {Url, H};
            _ -> {Url, H, binary_to_list(Mime), Body}
        end,
    Started = erlang:monotonic_time(millisecond),
    {ok, {{_, Status, _}, _ResponseHeaders, ResponseBody}} = isolated_request(Method, Req),
    [{{observed, Id}, Observed}] = ets:lookup(?TABLE, {observed, Id}),
    true = ets:insert(
        ?TABLE,
        {{request, maps:get(leaf, S), Id}, Observed#{
            status => Status, elapsed_ms => erlang:monotonic_time(millisecond) - Started
        }}
    ),
    #{status => Status, body => ResponseBody, request_id => Id}.

%% OTP's default keep-alive profile queues concurrent requests on one socket.
%% Each application request therefore owns a standalone profile and connection.
%% Four literal slots bound OTP's profile-table atoms; no generated atom per leaf.
isolated_request(Method, Req) ->
    Slot = claim_profile([w403_http_1, w403_http_2, w403_http_3, w403_http_4]),
    try
        {ok, Pid} = inets:start(httpc, [{profile, Slot}], stand_alone),
        try
            httpc:request(
                Method,
                Req,
                [{timeout, 120000}, {autoredirect, false}],
                [{body_format, binary}],
                Pid
            )
        after
            unlink(Pid),
            gen_server:stop(Pid, shutdown, 5000)
        end
    after
        ets:delete(?TABLE, {http_profile, Slot})
    end.

claim_profile([]) ->
    erlang:error(w403_http_concurrency_exceeded);
claim_profile([Slot | Rest]) ->
    case ets:insert_new(?TABLE, {{http_profile, Slot}, self()}) of
        true -> Slot;
        false -> claim_profile(Rest)
    end.

%% Called before releasing the paused request. Both identities are observed at
%% actual Cowboy dispatch, and the competing response has already completed.
overlap(Leaf, Paused, #{request_id := CompetingId}) ->
    ?assert(is_process_alive(Paused)),
    PausedId = list_to_binary(pid_to_list(Paused)),
    [First] = [
        R
     || {{observed, _}, #{server_process := P} = R} <- ets:tab2list(?TABLE),
        P =:= PausedId
    ],
    [{{observed, CompetingId}, Second}] = ets:lookup(?TABLE, {observed, CompetingId}),
    ?assertNotEqual(maps:get(connection_id, First), maps:get(connection_id, Second)),
    ?assert(maps:get(observed_at_us, Second) >= maps:get(observed_at_us, First)),
    ?assert(is_process_alive(Paused)),
    io:format(user, "W403_HTTP_OVERLAP ~s~n", [
        jsone:encode(#{
            leaf => Leaf,
            paused_request => First,
            completed_competing_request => Second
        })
    ]),
    ok.

%% The caller observes Competing at the real receipt-admission boundary, after
%% gateway capability verification and body sealing. Admission may wait for the
%% paused confirm transaction, so its response cannot be required before release.
inflight_overlap(Leaf, Paused, Competing) ->
    ?assert(is_process_alive(Paused)),
    ?assert(is_process_alive(Competing)),
    ?assertNotEqual(Paused, Competing),
    Observed = ets:tab2list(?TABLE),
    ForPid = fun(Pid) ->
        PidBin = list_to_binary(pid_to_list(Pid)),
        [R] = [
            R0
         || {{observed, _}, #{server_process := P} = R0} <- Observed,
            P =:= PidBin
        ],
        R
    end,
    First = ForPid(Paused),
    Second = ForPid(Competing),
    ?assertNotEqual(maps:get(connection_id, First), maps:get(connection_id, Second)),
    ?assert(maps:get(observed_at_us, Second) >= maps:get(observed_at_us, First)),
    ?assert(is_process_alive(Paused)),
    ?assert(is_process_alive(Competing)),
    io:format(user, "W403_HTTP_OVERLAP ~s~n", [
        jsone:encode(#{
            leaf => Leaf,
            paused_request => First,
            inflight_competing_request => Second,
            observed_boundary => <<"attach_pending_repo:register_content_pin/2">>
        })
    ]),
    maps:get(request_id, Second).

local_target(<<"/", _/binary>> = Path) -> Path;
local_target(<<"https://w403.invalid/", Rest/binary>>) -> <<"/", Rest/binary>>;
local_target(_) -> erlang:error(unexpected_upload_origin).

success(#{status := Status, body := Body}) when Status >= 200, Status < 300 ->
    Decoded =
        try
            jsone:decode(Body)
        catch
            _:_ -> undefined
        end,
    case Decoded of
        #{<<"code">> := Code} -> Code =:= 0;
        #{<<"error">> := _} -> false;
        _ -> true
    end;
success(_) ->
    false.

payload(#{body := Body}) ->
    case jsone:decode(Body) of
        #{<<"payload">> := P} -> P;
        M -> M
    end.
evidence(Leaf) -> [R || {{request, L, _}, R} <- ets:tab2list(?TABLE), L =:= Leaf].
max_bytes(ordinary) -> elib_oss:max_file_size();
max_bytes(enterprise_internal) -> elib_oss:max_file_size();
max_bytes(_) -> eb_asset_content:max_size_bytes().
bearer(Token) -> <<"Bearer ", Token/binary>>.
int(I) when is_integer(I) -> I;
int(B) when is_binary(B) -> binary_to_integer(B).
seat_path(S, Action) ->
    Org = maps:get(org_id, maps:get(scope, S)),
    <<"/api/v1/seat/enterprise/organizations/", (integer_to_binary(Org))/binary, "/assets/",
        Action/binary>>.
widget_path(S, Action) ->
    <<"/api/v1/cs/widget/sessions/", (integer_to_binary(maps:get(session_id, S)))/binary,
        "/assets/", Action/binary>>.

%% ===================================================================
%% W6-01 C 组追加入口（只增不改）
%% ===================================================================

%% V5 恢复 API：operation=status|renew 打到各 surface 既有的 presign 端点。
%% Extra 覆盖请求身份字段（如 client_upload_id/asset_id/upload_ref）。
presign_op(_S0, T, Op, Extra) ->
    S = maps:get(fixture, T),
    Id = #{
        operation => Op,
        client_upload_id => maps:get(client_upload_id, T),
        upload_ref => upload_ref_of(T),
        asset_id => asset_id_of(T)
    },
    {Method, Path, Body} =
        case maps:get(surface, T) of
            ordinary ->
                Q = uri_string:compose_query(
                    [{<<"operation">>, Op}, {<<"client_upload_id">>, maps:get(client_upload_id, T)}]
                ),
                {get, <<"/api/v1/attachment/presign?", Q/binary>>, <<>>};
            enterprise_internal ->
                {post, <<"/api/internal/v1/files/presign">>,
                    merge(maps:with([operation, client_upload_id], Id), Extra)};
            enterprise_seat ->
                {post, seat_path(S, <<"presign">>), merge(Id, Extra)};
            cs_widget ->
                {post, widget_path(S, <<"presign">>),
                    merge(Id#{installation_id => maps:get(installation_id, S)}, Extra)}
        end,
    case Method of
        get -> request(S, get, Path, <<>>, maps:get(headers, T), <<"application/json">>);
        post -> request(S, post, Path, Body, maps:get(headers, T), <<"application/json">>)
    end.

status(T) -> presign_op(maps:get(fixture, T), T, <<"status">>, #{}).
renew(T) -> presign_op(maps:get(fixture, T), T, <<"renew">>, #{}).

%% ordinary multipart 登记入口：GET presign 带 method=multipart，响应额外
%% 携带指向本端点的 upload_url（multipart/form-data POST 目标）。
multipart_register(S0, _Bytes) ->
    S = S0,
    Cid = elib_uuid:gen_v7(),
    Registry = attachment_storage:registry(),
    Ns = maps:get(multipart_ns, S0, <<"w403-disk">>),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{upload_namespaces => #{<<"private">> => Ns, <<"enterprise">> => Ns}}
    ),
    Q = uri_string:compose_query([
        {<<"filename">>, <<"w403.txt">>},
        {<<"mime_type">>, <<"text/plain">>},
        {<<"client_upload_id">>, Cid},
        {<<"method">>, <<"multipart">>}
    ]),
    Headers = maps:get(ordinary_headers, S),
    Path = <<"/api/v1/attachment/presign?", Q/binary>>,
    NoAuth = maps:without([<<"authorization">>], Headers),
    ?assertNot(success(request(S, get, Path, <<>>, NoAuth, <<"application/json">>))),
    Registered = request(S, get, Path, <<>>, Headers, <<"application/json">>),
    ?assert(success(Registered)),
    Data = payload(Registered),
    Key = maps:get(<<"object_key">>, Data),
    {ok, Location} = attachment_storage:session_location(Key),
    ?assert(Location =/= legacy_unknown),
    true = ets:insert(?TABLE, {{object, Key}, Location}),
    #{
        fixture => S,
        leaf => maps:get(leaf, S, elib_uuid:gen_v7()),
        surface => ordinary,
        driver => maps:get(multipart_driver, S0, disk),
        key => Key,
        location => Location,
        upload_url => maps:get(<<"put_url">>, Data),
        multipart_url => maps:get(<<"upload_url">>, Data),
        headers => Headers,
        confirm_body => #{object_key => Key, scope => <<"private">>},
        registration => Data,
        client_upload_id => Cid
    }.

%% multipart/form-data POST（文件字段名 file）：真实 loopback HTTP，
%% 上传 URL 由 presign 以请求 Host 派生（同 listener），保留 evidence 观察。
multipart_put(S, T, Bytes) ->
    Url = maps:get(multipart_url, T),
    Boundary = <<"w403multipart7f3a">>,
    Body =
        <<
            "--",
            Boundary/binary,
            "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"w403.txt\"\r\n",
            "Content-Type: text/plain\r\n\r\n",
            Bytes/binary,
            "\r\n--",
            Boundary/binary,
            "--\r\n"
        >>,
    Mime = <<"multipart/form-data; boundary=", Boundary/binary>>,
    absolute_request(S, post, Url, Body, maps:get(headers, T), Mime).

%% confirm 带附加字段（cipher / anchor_msg_id / md5 等旧字段）。
confirm_with(_S0, T, Extra) ->
    T2 = T#{confirm_body => merge(maps:get(confirm_body, T), Extra)},
    confirm(T2).

%% 旧客户端 confirm：无 client_upload_id，声明型旧字段（file_hash256/md5）。
%% 服务端 V5 链路以真实对象字节裁决，客户端自报 hash 只进 legacy 兼容分支。
confirm_legacy(T) ->
    Key = maps:get(key, T),
    Sha = binary:encode_hex(crypto:hash(sha256, <<"legacy client declared bytes">>), lowercase),
    case maps:get(surface, T) of
        ordinary ->
            confirm_with(maps:get(fixture, T), T, #{
                file_hash256 => Sha,
                mime_type => <<"text/plain">>,
                size => byte_size(<<"legacy client declared bytes">>),
                md5 => <<>>
            });
        enterprise_internal ->
            confirm_with(maps:get(fixture, T), T, #{
                object_key => Key,
                file_hash256 => Sha
            });
        _ ->
            confirm(T)
    end.

%% 过期能力 PUT：ordinary/internal 用真实 JWT 签发器构造已过期能力并替换
%% gateway URL 的 capability 参数；seat/widget 的不透明 upload_ref 无法替换，
%% 过期场景由套件以 upload_ttl_seconds => -1 重新登记（签发即过期，既有
%% eb_asset_tests 先例），不走本函数。
expired_put(T, Bytes) ->
    Key = maps:get(key, T),
    {ok, Expired} =
        imboy_jwt:issue_upload_capability(Key, <<"text/plain">>, erlang:system_time(second) - 1),
    Stale = replace_capability(maps:get(upload_url, T), Expired),
    S = maps:get(fixture, T),
    Headers =
        case maps:get(surface, T) of
            enterprise_seat -> maps:get(headers, T);
            _ -> #{}
        end,
    request(S, put, Stale, Bytes, Headers, <<"text/plain">>).

%% 裸 PUT：剥离 capability 参数的 gateway URL（旧客户端直传形态）。
bare_put(T, Bytes) ->
    Stale = strip_capability(maps:get(upload_url, T)),
    S = maps:get(fixture, T),
    Headers =
        case maps:get(surface, T) of
            enterprise_seat -> maps:get(headers, T);
            _ -> #{}
        end,
    request(S, put, Stale, Bytes, Headers, <<"text/plain">>).

%% SQL 直通（套件内持久状态断言用；只读查询为主）。
q(Sql, Args) -> elib_pg:query(Sql, Args).

replace_capability(Url, NewCap) ->
    case binary:split(Url, <<"capability=">>) of
        [Prefix, Rest] ->
            Suffix =
                case binary:split(Rest, <<"&">>) of
                    [_, S] -> <<"&", S/binary>>;
                    _ -> <<>>
                end,
            <<Prefix/binary, "capability=", NewCap/binary, Suffix/binary>>;
        _ ->
            erlang:error(no_capability_in_url)
    end.

strip_capability(Url) ->
    case binary:split(Url, <<"capability=">>) of
        [Prefix, Rest] ->
            Suffix =
                case binary:split(Rest, <<"&">>) of
                    [_, S] -> <<"&", S/binary>>;
                    _ -> <<>>
                end,
            <<Prefix/binary, "capability=", Suffix/binary>>;
        _ ->
            erlang:error(no_capability_in_url)
    end.

upload_ref_of(T) ->
    Body = maps:get(confirm_body, T),
    maps:get(upload_ref, Body, undefined).

asset_id_of(T) ->
    Body = maps:get(confirm_body, T),
    maps:get(asset_id, Body, undefined).

merge(A, B) when is_map(A), is_map(B) ->
    maps:merge(A, maps:filter(fun(_, V) -> V =/= undefined end, B)).

%% multipart 上传 URL 是 presign 以请求 Host 派生的绝对 loopback 地址，
%% 不经 w403.invalid 改写；直连同 listener 并保留 evidence 观察。
absolute_request(S, Method, Url, Body, Headers0, Mime) ->
    Id = <<"w403-", (elib_uuid:gen_v7())/binary>>,
    Headers = Headers0#{<<"x-w403-request-id">> => Id, <<"connection">> => <<"close">>},
    H = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- maps:to_list(Headers)],
    Req = {binary_to_list(Url), H, binary_to_list(Mime), Body},
    Started = erlang:monotonic_time(millisecond),
    {ok, {{_, Status, _}, _, ResponseBody}} = isolated_request(Method, Req),
    true = ets:insert(
        ?TABLE,
        {{request, maps:get(leaf, S), Id}, #{
            request_id => Id,
            route => route_of(Url),
            method => atom_to_binary(Method, utf8),
            handler => <<"attach_handler">>,
            server_process => <<"loopback">>,
            connection_id => <<"standalone">>,
            observed_at_us => erlang:monotonic_time(microsecond),
            status => Status,
            elapsed_ms => erlang:monotonic_time(millisecond) - Started
        }}
    ),
    #{status => Status, body => ResponseBody, request_id => Id}.

route_of(Url) ->
    case binary:split(Url, <<"?">>) of
        [Path, _] ->
            case binary:split(Path, <<"://">>) of
                [_, Rest] ->
                    case binary:split(Rest, <<"/">>) of
                        [_, P] -> <<"/", P/binary>>;
                        _ -> Path
                    end;
                _ ->
                    Path
            end;
        _ ->
            Url
    end.
