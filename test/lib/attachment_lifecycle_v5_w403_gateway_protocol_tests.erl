%%% 真 PG + loopback HTTP gateway + 两真实 driver；不是四业务入口64叶的替代品。
-module(attachment_lifecycle_v5_w403_gateway_protocol_tests).
-export([setup/0, cleanup/1]).
-include_lib("eunit/include/eunit.hrl").

protocol_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        [
            {
                atom_to_list(D) ++ "/" ++ atom_to_list(F),
                {timeout, 180, fun() -> scenario(S, D, F) end}
            }
         || D <- [disk, garage],
            F <- [
                reject_different_after_pin,
                confirm_then_old_put,
                expired_capability,
                registry_remap_rejected,
                snapshot_switch_after_validation,
                snapshot_read_after_validation,
                ordinary_ready_delete_remap,
                ordinary_running_delete_remap,
                eb_ready_delete_remap,
                eb_running_delete_remap,
                disk_authorized_download,
                ordinary_snapshot_delete_race,
                eb_snapshot_delete_race,
                null_v5_never_guesses_location
            ],
            (F =/= null_v5_never_guesses_location orelse D =:= disk),
            (F =/= disk_authorized_download orelse D =:= disk)
        ]
    end}.
setup() ->
    {ok, ManifestBytes} = file:read_file(required("W403_RESOURCE_MANIFEST")),
    #{<<"database">> := Db} = jsone:decode(ManifestBytes),
    <<"attv5_chain_", _/binary>> = Db,
    {ok, [Config]} = file:consult(required("W403_CONFIG_PATH")),
    Env = proplists:get_value(imboy, Config),
    [application:set_env(imboy, K, V) || {K, V} <- Env],
    {ok, Pg0} = application:get_env(imboy, pg_conf),
    {epgsql, connect, [Connect0]} = maps:get(start_mfa, Pg0),
    Connect = Connect0#{database => binary_to_list(Db)},
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    application:set_env(imboy, jwt_key, crypto:strong_rand_bytes(32)),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _} = pooler:new_pool(Pg0#{name => pgsql, start_mfa => {epgsql, connect, [Connect]}}),
    {ok, [#{<<"db">> := Db}]} = elib_pg:query(<<"SELECT current_database() AS db">>, []),
    EP = list_to_binary(required("W202_GARAGE_ENDPOINT")),
    Bucket = list_to_binary(required("W202_GARAGE_BUCKET")),
    application:set_env(imboy, garage, #{
        endpoint => EP,
        bucket => Bucket,
        access_key => list_to_binary(required("W202_GARAGE_ACCESS_KEY")),
        secret_key => list_to_binary(required("W202_GARAGE_SECRET_KEY"))
    }),
    Root =
        "/private/tmp/attv5-w403-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase)),
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            <<"w403-disk-root">> => #{
                driver => <<"disk">>,
                namespace => <<"w403-disk">>,
                endpoint => list_to_binary("file://" ++ Root),
                container => <<"fixture">>
            },
            <<"w403-garage-root">> => #{
                driver => <<"garage">>,
                namespace => <<"w403-garage">>,
                endpoint => EP,
                container => Bucket
            }
        }
    }),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/gateway", attachment_storage_gateway_handler, #{action => put}},
            {"/api/v1/attachment/content", attachment_download_handler, #{}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(w403_owned_gateway, [{ip, {127, 0, 0, 1}}, {port, 0}], #{
        env => #{dispatch => Dispatch}
    }),
    Port = ranch:get_port(w403_owned_gateway),
    application:set_env(
        imboy, base_url, list_to_binary("http://127.0.0.1:" ++ integer_to_list(Port))
    ),
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    Uid = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    {ok, _} = elib_pg:query(
        <<"INSERT INTO public.\"user\"(id,account,nickname,password,status,account_type,reg_ip,reg_cosv,created_at) VALUES($1,$2,'fixture','',1,0,'127.0.0.1','w403',now()) RETURNING id">>,
        [Uid, <<"w403-", (integer_to_binary(Uid))/binary>>]
    ),
    #{
        root => Root,
        uid => Uid,
        port => Port,
        bucket => Bucket,
        run => binary:encode_hex(crypto:strong_rand_bytes(12), lowercase)
    }.
cleanup(_) ->
    cowboy:stop_listener(w403_owned_gateway),
    pooler:rm_pool(pgsql).
required(Name) ->
    case os:getenv(Name) of
        false -> erlang:error({no_go, required_resource_missing, Name});
        "" -> erlang:error({no_go, required_resource_missing, Name});
        V -> V
    end.

scenario(S, D, Fault) ->
    case Fault of
        null_v5_never_guesses_location -> null_session_rejected(S);
        ordinary_snapshot_delete_race -> delete_recovery(S, D, ordinary, snapshot);
        eb_snapshot_delete_race -> delete_recovery(S, D, enterprise, snapshot);
        ordinary_ready_delete_remap -> delete_recovery(S, D, ordinary, ready);
        ordinary_running_delete_remap -> delete_recovery(S, D, ordinary, running);
        eb_ready_delete_remap -> delete_recovery(S, D, enterprise, ready);
        eb_running_delete_remap -> delete_recovery(S, D, enterprise, running);
        _ -> upload_scenario(S, D, Fault)
    end.

upload_scenario(S, D, Fault) ->
    {Key, Ns, Container} = register(S, D, Fault),
    Bytes = <<"original registered body">>,
    Exp = erlang:system_time(second) + 60,
    {ok, Cap} = imboy_jwt:issue_upload_capability(Key, <<"text/plain">>, Exp),
    case Fault of
        disk_authorized_download ->
            ?assertEqual(200, put(S, Cap, Bytes)),
            ?assertMatch(
                {ok, _}, attach_logic:confirm(maps:get(uid, S), Key, <<"private">>, undefined, #{})
            ),
            case D of
                disk -> assert_download(S, Key, Bytes);
                garage -> assert_bytes(D, Ns, Container, Key, Bytes)
            end;
        reject_different_after_pin ->
            ?assertEqual(200, put(S, Cap, Bytes)),
            ?assertEqual(409, put(S, Cap, <<"different bytes">>)),
            assert_bytes(D, Ns, Container, Key, Bytes);
        confirm_then_old_put ->
            ?assertEqual(200, put(S, Cap, Bytes)),
            ?assertMatch(
                {ok, _}, attach_logic:confirm(maps:get(uid, S), Key, <<"private">>, undefined, #{})
            ),
            ?assert(put(S, Cap, <<"different bytes">>) =/= 200),
            assert_bytes(D, Ns, Container, Key, Bytes);
        expired_capability ->
            {ok, Expired} = imboy_jwt:issue_upload_capability(
                Key, <<"text/plain">>, erlang:system_time(second) - 1
            ),
            ?assert(put(S, Expired, Bytes) =/= 200),
            ?assertEqual({error, not_found}, read(D, Ns, Container, Key));
        snapshot_switch_after_validation ->
            snapshot_race(S, D, Key, Ns, Container, Cap, Bytes, put);
        snapshot_read_after_validation ->
            ?assertEqual(200, put(S, Cap, Bytes)),
            snapshot_race(S, D, Key, Ns, Container, Cap, Bytes, confirm);
        registry_remap_rejected ->
            ?assertEqual(200, put(S, Cap, Bytes)),
            {ok, Registry} = application:get_env(imboy, attachment_storage_registry),
            Roots = maps:get(roots, Registry),
            Changed = maps:map(
                fun(_, R) ->
                    case maps:get(namespace, R) of
                        Ns -> R#{endpoint => <<"file:///must-not-follow-remap">>};
                        _ -> R
                    end
                end,
                Roots
            ),
            application:set_env(imboy, attachment_storage_registry, Registry#{roots => Changed}),
            try
                ?assert(put(S, Cap, Bytes) =/= 200)
            after
                application:set_env(imboy, attachment_storage_registry, Registry)
            end,
            assert_bytes(D, Ns, Container, Key, Bytes)
    end.
register(S, D, Fault) ->
    Uid = maps:get(uid, S),
    Key =
        <<"u", (integer_to_binary(Uid))/binary, "/file/w403/", (maps:get(run, S))/binary, "/",
            (atom_to_binary(D))/binary, "/", (atom_to_binary(Fault))/binary>>,
    {Ns, Container} =
        case D of
            disk -> {<<"w403-disk">>, <<"fixture">>};
            garage -> {<<"w403-garage">>, maps:get(bucket, S)}
        end,
    {ok, I} = attachment_storage:identity(Ns, Container, Key),
    Reg = #{
        <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => integer_to_binary(Uid)},
        <<"declaration">> => #{
            <<"mime">> => <<"text/plain">>, <<"scope">> => <<"private">>, <<"scope_ref">> => null
        }
    },
    {ok, [_]} = elib_pg:query(
        <<"SELECT * FROM fn_v5_register_pending($1,$2,'private',$3,$4::uuid,$5::jsonb,now()+interval '2 hours',now()+interval '1 hour',$6,$7,$8,$9)">>,
        [
            Key,
            maps:get(bucket, S),
            Uid,
            elib_uuid:gen_v7(),
            jsone:encode(Reg),
            atom_to_binary(D),
            Ns,
            Container,
            maps:get(locator_digest, I)
        ]
    ),
    {Key, Ns, Container}.
put(S, Cap, Bytes) ->
    Url = "http://127.0.0.1:" ++ integer_to_list(maps:get(port, S)) ++ "/gateway",
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        put,
        {Url, [{"x-upload-capability", binary_to_list(Cap)}], "text/plain", Bytes},
        [{timeout, 30000}],
        [{body_format, binary}]
    ),
    Status.
read(disk, Ns, C, K) -> attachment_storage_disk:read(Ns, C, K);
read(garage, Ns, C, K) -> attachment_storage_garage:read(Ns, C, K).
assert_bytes(D, Ns, C, K, Expected) ->
    {ok, Actual} = read(D, Ns, C, K),
    ?assertEqual(Expected, Actual),
    ?assertEqual(crypto:hash(sha256, Expected), crypto:hash(sha256, Actual)).

%% 屏障只暂停真实 locator 读取的返回边界；PG/签名/HTTP/driver 均为生产实现。
snapshot_race(S, D, Key, Ns, Container, Cap, Bytes, Mode) ->
    Parent = self(),
    {ok, Registry} = application:get_env(imboy, attachment_storage_registry),
    {ok, Garage} = application:get_env(imboy, garage),
    {ok, Trap} = gen_tcp:listen(0, [
        binary, {active, false}, {ip, {127, 0, 0, 1}}, {reuseaddr, true}
    ]),
    {ok, {_, TrapPort}} = inet:sockname(Trap),
    TrapPid = spawn(fun() -> trap_accept(Trap, Parent) end),
    NewEndpoint =
        case D of
            disk -> list_to_binary(maps:get(root, S) ++ "-must-not-write-B");
            garage -> list_to_binary("http://127.0.0.1:" ++ integer_to_list(TrapPort))
        end,
    meck:new(attachment_storage, [passthrough, no_link]),
    meck:expect(attachment_storage, session_location, fun(K) ->
        Result = meck:passthrough([K]),
        Parent ! {validated, self(), Result},
        receive
            continue_io -> ok
        after 5000 -> erlang:error(barrier_timeout)
        end,
        Result
    end),
    spawn(fun() ->
        Result =
            try
                case Mode of
                    put ->
                        put(S, Cap, Bytes);
                    confirm ->
                        attach_logic:confirm(maps:get(uid, S), Key, <<"private">>, undefined, #{})
                end
            catch
                Class:Reason -> {Class, Reason}
            end,
        Parent ! {race_result, Result}
    end),
    try
        receive
            {validated, Worker, {ok, I}} ->
                Roots = maps:map(
                    fun(_, R) ->
                        case maps:get(namespace, R) of
                            Ns ->
                                R#{
                                    endpoint =>
                                        case D of
                                            disk -> <<"file://", NewEndpoint/binary>>;
                                            garage -> NewEndpoint
                                        end
                                };
                            _ ->
                                R
                        end
                    end,
                    maps:get(roots, Registry)
                ),
                application:set_env(imboy, attachment_storage_registry, Registry#{roots => Roots}),
                application:set_env(imboy, garage, Garage#{endpoint => NewEndpoint}),
                Worker ! continue_io,
                receive
                    {race_result, Code} ->
                        case Mode of
                            put -> ?assertEqual(200, Code);
                            confirm -> ?assertMatch({ok, _}, Code)
                        end
                after 10000 -> erlang:error(race_http_timeout)
                end,
                Actual =
                    case D of
                        disk -> attachment_storage_disk:read_snapshot(I);
                        garage -> attachment_storage_garage:read_snapshot(I)
                    end,
                ?assertEqual({ok, Bytes}, Actual),
                case D of
                    disk ->
                        ?assertNot(
                            filelib:is_file(
                                filename:join([
                                    binary_to_list(NewEndpoint),
                                    binary_to_list(Container),
                                    binary_to_list(Key)
                                ])
                            )
                        );
                    garage ->
                        ok
                end,
                receive
                    trap_b_request -> erlang:error(wrong_destination_B_touched)
                after 100 -> ok
                end
        after 10000 -> erlang:error(locator_barrier_missing)
        end
    after
        application:set_env(imboy, attachment_storage_registry, Registry),
        application:set_env(imboy, garage, Garage),
        meck:unload(attachment_storage),
        gen_tcp:close(Trap),
        exit(TrapPid, kill)
    end.
trap_accept(Trap, Parent) ->
    case gen_tcp:accept(Trap) of
        {ok, Sock} ->
            Parent ! trap_b_request,
            gen_tcp:recv(Sock, 0, 1000),
            gen_tcp:send(
                Sock,
                <<"HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n">>
            ),
            gen_tcp:close(Sock),
            trap_accept(Trap, Parent);
        _ ->
            ok
    end.

%% 只调整本 run 拥有 job 的退避/租约时间，不重置 attempt 或伪造完成状态。
delete_recovery(S, D, Domain, State) ->
    {Key, Ns, C} = register(
        S, D, list_to_atom(atom_to_list(Domain) ++ "_delete_" ++ atom_to_list(State))
    ),
    {ok, I} = attachment_storage:session_location(Key),
    Bytes = <<"run-owned delete fixture">>,
    case D of
        disk ->
            ok = attachment_storage_disk:put_snapshot(I, Bytes, erlang:system_time(second) + 60);
        garage ->
            Now = calendar:universal_time(),
            Exp = calendar:gregorian_seconds_to_datetime(
                calendar:datetime_to_gregorian_seconds(Now) + 60
            ),
            ok = attachment_storage_garage:put_snapshot(I, Bytes, <<"text/plain">>, #{
                issued_at => Now, expires_at => Exp
            })
    end,
    {Table, Predicate, Args, Drain} =
        case Domain of
            ordinary ->
                {ok, [_]} = elib_pg:query(
                    <<"SELECT fn_v5_intent_pending_delete($1,'owned test','reject')">>, [Key]
                ),
                {<<"attachment_object_deletion">>, <<"path=$1">>, [Key], fun() ->
                    attachment_delete_worker:run_batch()
                end};
            enterprise ->
                Id = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
                Ws = Id + 1,
                U = maps:get(uid, S),
                {ok, [_]} = elib_pg:query(
                    <<"INSERT INTO organization(id,name,owner_id) VALUES($1,'owned-w403',$2) RETURNING id">>,
                    [Id, U]
                ),
                {ok, [_]} = elib_pg:query(
                    <<"INSERT INTO workspace(id,organization_id,name,owner_id,branding) VALUES($1,$2,'owned-w403',$3,'{}') RETURNING id">>,
                    [Ws, Id, U]
                ),
                {ok, [_]} = elib_pg:query(
                    <<"INSERT INTO enterprise_asset(id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,status,storage_driver,storage_namespace,storage_container,storage_locator_sha256) VALUES($1,$1,$2,$3,$4,'text/plain',$5,'pending_confirm',$6,$7,$8,$9) RETURNING id">>,
                    [
                        Id,
                        Ws,
                        Key,
                        binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
                        byte_size(Bytes),
                        atom_to_binary(D),
                        Ns,
                        C,
                        maps:get(
                            locator_digest, element(2, attachment_storage:identity(Ns, C, Key))
                        )
                    ]
                ),
                {ok, [_]} = elib_pg:query(
                    <<"SELECT fn_v5_intent_eb_delete($1,$2,$1,'owned test')">>, [Id, Ws]
                ),
                {<<"enterprise_asset_delete_queue">>, <<"asset_id=$1">>, [Id], fun() ->
                    eb_pg_asset_delete_queue:drain(Id, Ws, 1)
                end}
        end,
    case State of
        ready ->
            ok;
        snapshot ->
            ok;
        running ->
            Token = elib_uuid:gen_v7(),
            case Domain of
                ordinary ->
                    {ok, [#{<<"out_path">> := Key}]} = elib_pg:query(
                        <<"SELECT * FROM fn_v5_claim_ordinary($1)">>, [Token]
                    );
                enterprise ->
                    {ok, [#{<<"out_object_key">> := Key}]} = elib_pg:query(
                        <<"SELECT * FROM fn_v5_claim_eb($1)">>, [Token]
                    )
            end,
            {ok, [_]} = elib_pg:query(
                <<"UPDATE ", Table/binary, " SET lease_until=now()-interval '1 second' WHERE ",
                    Predicate/binary, " RETURNING attempts">>,
                Args
            ),
            case Domain of
                ordinary -> {ok, _} = attachment_delete_worker:reconcile();
                enterprise -> {ok, _} = eb_pg_asset_delete_queue:reap(100)
            end,
            due_owned_job(Table, Predicate, Args)
    end,
    {ok, Registry} = application:get_env(imboy, attachment_storage_registry),
    {ok, Garage} = application:get_env(imboy, garage),
    Parent = self(),
    {ok, Trap} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, {_, TrapPort}} = inet:sockname(Trap),
    TrapPid = spawn(fun() -> trap_accept(Trap, Parent) end),
    B =
        case D of
            disk -> list_to_binary(maps:get(root, S) ++ "-delete-B");
            garage -> list_to_binary("http://127.0.0.1:" ++ integer_to_list(TrapPort))
        end,
    Roots = maps:map(
        fun(_, R) ->
            case maps:get(namespace, R) of
                Ns ->
                    R#{
                        endpoint =>
                            case D of
                                disk -> <<"file://", B/binary>>;
                                garage -> B
                            end
                    };
                _ ->
                    R
            end
        end,
        maps:get(roots, Registry)
    ),
    Barrier =
        case State of
            snapshot ->
                meck:new(attachment_storage, [passthrough, no_link]),
                meck:expect(attachment_storage, frozen_location, fun(K, R) ->
                    Result = meck:passthrough([K, R]),
                    case K =:= Key of
                        true ->
                            Parent ! {delete_validated, self()},
                            receive
                                continue_delete -> ok
                            after 5000 -> erlang:error(delete_barrier_timeout)
                            end;
                        false ->
                            ok
                    end,
                    Result
                end),
                spawn(fun() -> Parent ! {delete_result, Drain()} end),
                receive
                    {delete_validated, W} -> W
                after 5000 -> erlang:error(delete_locator_not_called)
                end;
            _ ->
                undefined
        end,
    application:set_env(imboy, attachment_storage_registry, Registry#{roots => Roots}),
    application:set_env(imboy, garage, Garage#{endpoint => B}),
    try
        case State of
            snapshot ->
                Barrier ! continue_delete,
                receive
                    {delete_result, R} -> ?assertMatch({ok, _}, R)
                after 5000 -> erlang:error(delete_result_timeout)
                end;
            _ ->
                ?assertMatch({ok, _}, Drain())
        end,
        Expected =
            case State of
                snapshot -> {error, not_found};
                _ -> {ok, Bytes}
            end,
        ?assertEqual(
            Expected,
            case D of
                disk -> attachment_storage_disk:read_snapshot(I);
                garage -> attachment_storage_garage:read_snapshot(I)
            end
        ),
        receive
            trap_b_request -> erlang:error(wrong_delete_destination)
        after 50 -> ok
        end
    after
        application:set_env(imboy, attachment_storage_registry, Registry),
        application:set_env(imboy, garage, Garage),
        gen_tcp:close(Trap),
        exit(TrapPid, kill),
        case State of
            snapshot -> meck:unload(attachment_storage);
            _ -> ok
        end
    end,
    case State of
        snapshot ->
            ok;
        _ ->
            due_owned_job(Table, Predicate, Args),
            ?assertMatch({ok, _}, Drain())
    end,
    ?assertEqual(
        {error, not_found},
        case D of
            disk -> attachment_storage_disk:read_snapshot(I);
            garage -> attachment_storage_garage:read_snapshot(I)
        end
    ),
    {ok, [#{<<"job_state">> := <<"done">>}]} = elib_pg:query(
        <<"SELECT job_state FROM ", Table/binary, " WHERE ", Predicate/binary>>, Args
    ).
due_owned_job(Table, Predicate, Args) ->
    {ok, [_]} = elib_pg:query(
        <<"UPDATE ", Table/binary, " SET next_retry_at=now()-interval '1 second' WHERE ",
            Predicate/binary, " RETURNING attempts">>,
        Args
    ),
    ok.

assert_download(S, Key, Bytes) ->
    {ok, Url} = attach_logic:view_url(maps:get(uid, S), Key),
    ?assertNotEqual(nomatch, binary:match(Url, <<"/api/v1/attachment/content?ticket=">>)),
    {ok, {{_, 200, _}, _, Bytes}} = httpc:request(get, {binary_to_list(Url), []}, [], [
        {body_format, binary}
    ]),
    {ok, {{_, 206, _}, RangeHeaders, Part}} = httpc:request(
        get, {binary_to_list(Url), [{"range", "bytes=0-7"}]}, [], [{body_format, binary}]
    ),
    ?assertEqual(binary:part(Bytes, 0, 8), Part),
    ?assertEqual(
        "bytes 0-7/" ++ integer_to_list(byte_size(Bytes)),
        proplists:get_value("content-range", RangeHeaders)
    ),
    {ok, {{_, 200, _}, HeadHeaders, <<>>}} = httpc:request(head, {binary_to_list(Url), []}, [], [
        {body_format, binary}
    ]),
    ?assertEqual(
        integer_to_list(byte_size(Bytes)), proplists:get_value("content-length", HeadHeaders)
    ),
    {ok, {{_, 416, _}, _, _}} = httpc:request(
        get, {binary_to_list(Url), [{"range", "bytes=0-1,3-4"}]}, [], [{body_format, binary}]
    ),
    ?assertEqual({error, forbidden}, attach_logic:view_url(maps:get(uid, S) + 1, Key)),
    %% ticket 发出后权限事实变化（metadata禁用/删除）必须即时拒绝。
    {ok, [_]} = elib_pg:query(<<"UPDATE attachment SET status=-1 WHERE path=$1 RETURNING id">>, [
        Key
    ]),
    {ok, {{_, 403, _}, _, _}} = httpc:request(get, {binary_to_list(Url), []}, [], [
        {body_format, binary}
    ]).

null_session_rejected(S) ->
    Uid = maps:get(uid, S),
    Key =
        <<"u", (integer_to_binary(Uid))/binary, "/file/w403/", (maps:get(run, S))/binary,
            "/historical-null">>,
    Reg = #{
        <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => integer_to_binary(Uid)},
        <<"declaration">> => #{
            <<"mime">> => <<"text/plain">>, <<"scope">> => <<"private">>, <<"scope_ref">> => null
        }
    },
    {ok, [_]} = elib_pg:query(
        <<"SELECT * FROM fn_v5_register_pending($1,$2,'private',$3,$4::uuid,$5::jsonb,now()+interval '2 hours',now()+interval '1 hour',NULL,NULL,NULL,NULL)">>,
        [Key, maps:get(bucket, S), Uid, elib_uuid:gen_v7(), jsone:encode(Reg)]
    ),
    {ok, Garage} = application:get_env(imboy, garage),
    Parent = self(),
    {ok, Trap} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, {_, P}} = inet:sockname(Trap),
    TrapPid = spawn(fun() -> trap_accept(Trap, Parent) end),
    application:set_env(imboy, garage, Garage#{
        endpoint => list_to_binary("http://127.0.0.1:" ++ integer_to_list(P))
    }),
    try
        {ok, Cap} = imboy_jwt:issue_upload_capability(
            Key, <<"text/plain">>, erlang:system_time(second) + 60
        ),
        ?assertEqual(409, put(S, Cap, <<"must-not-go-to-current-provider">>)),
        ?assertEqual(
            {error, historical_location_unverified},
            attach_logic:confirm(Uid, Key, <<"private">>, undefined, #{})
        ),
        receive
            trap_b_request -> erlang:error(historical_null_touched_current_provider)
        after 100 -> ok
        end
    after
        application:set_env(imboy, garage, Garage),
        gen_tcp:close(Trap),
        exit(TrapPid, kill)
    end.
