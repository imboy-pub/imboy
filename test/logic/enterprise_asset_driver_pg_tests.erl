%%% Real owned PG + Garage/Disk reads; governance side effects are isolated stubs.
-module(enterprise_asset_driver_pg_tests).
-include_lib("eunit/include/eunit.hrl").

internal_driver_test_() ->
    isolated_pg_fixture:test(fun internal_driver_fixture/0).

internal_driver_fixture() ->
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        %% V3-20 退役：disk driver 已删除——generator 收窄为 garage 单驱动；
        %% disk root 行为由 retired 负例钉死（fail-closed，不再有 disk 路径）。
        [
            {
                atom_to_list(D) ++ "/" ++ atom_to_list(F),
                environment_case({timeout, 90, fun() -> run(S, D, F) end})
            }
         || D <- [garage],
            F <- [
                confirm_replay,
                snapshot_switch,
                remap_rejected,
                no_uuid_registration,
                registration_unavailable
            ]
        ] ++
            [
                {"retired/disk_root_rejected",
                    environment_case({timeout, 30, fun() -> disk_root_rejected() end})}
            ]
    end}.
setup() ->
    setup(fun setup_owned/2).

setup(Start) ->
    {ok, [Config]} = file:consult(os:getenv("W403_CONFIG_PATH")),
    Env = proplists:get_value(imboy, Config),
    Before = saved_environment(
        lists:usort(
            [garage, attachment_storage_registry, jwt_key | proplists:get_keys(Env)]
        )
    ),
    Base =
        case file:read_link("/tmp") of
            {ok, Target} -> filename:absname(Target, "/");
            {error, einval} -> "/tmp";
            {error, Reason} -> error({temporary_root_unavailable, Reason})
        end,
    Root = filename:join(Base, "attv5-internal-" ++ binary_to_list(elib_uuid:gen_v7())),
    ok = file:make_dir(Root),
    State = #{root => Root, environment_before => Before},
    cache_fixture_helper:setup(Before, fun() ->
        cache_fixture_helper:on_failure(fun() -> cleanup(State) end),
        ok = file:change_mode(Root, 8#700),
        maps:merge(Start(Root, Env), State)
    end).

setup_owned(RootPath, Env) ->
    {ok, B} = file:read_file(os:getenv("W403_RESOURCE_MANIFEST")),
    #{<<"database">> := Db} = Identity = jsone:decode(B),
    ok = attachment_w403_owned_fixture:assert_chain_fixture_database(
        Db, Identity, Env, os:getenv("W403_RESOURCE_MANIFEST")
    ),
    [application:set_env(imboy, K, V) || {K, V} <- Env],
    {ok, Pg} = application:get_env(imboy, pg_conf),
    {epgsql, connect, [C]} = maps:get(start_mfa, Pg),
    [application:ensure_all_started(A) || A <- [crypto, inets, epgsql, pooler]],
    {ok, _} = pooler:new_pool(Pg#{
        name => pgsql, start_mfa => {epgsql, connect, [C#{database => binary_to_list(Db)}]}
    }),
    {ok, [#{<<"db">> := Db}]} = elib_pg:query(<<"SELECT current_database() AS db">>, []),
    EP = list_to_binary(os:getenv("W202_GARAGE_ENDPOINT")),
    Bucket = list_to_binary(os:getenv("W202_GARAGE_BUCKET")),
    application:set_env(imboy, garage, #{
        endpoint => EP,
        bucket => Bucket,
        access_key => list_to_binary(os:getenv("W202_GARAGE_ACCESS_KEY")),
        secret_key => list_to_binary(os:getenv("W202_GARAGE_SECRET_KEY"))
    }),
    Root = list_to_binary("file://" ++ RootPath),
    Registry = #{
        version => 1,
        roots => #{
            <<"internal-disk">> => #{
                driver => <<"disk">>,
                namespace => <<"internal-disk">>,
                endpoint => Root,
                container => <<"fixture">>
            },
            <<"internal-garage">> => #{
                driver => <<"garage">>,
                namespace => <<"internal-garage">>,
                endpoint => EP,
                container => Bucket
            }
        }
    },
    application:set_env(imboy, attachment_storage_registry, Registry),
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    meck:new(enterprise_asset_retention_logic, [passthrough, no_link]),
    meck:expect(enterprise_asset_retention_logic, content_policy_tx, fun(_, _, _) -> {ok, #{}} end),
    meck:expect(enterprise_asset_retention_logic, effective_max_bytes, fun(_, _) -> 1000000 end),
    meck:expect(enterprise_asset_retention_logic, mime_allowed, fun(_, M) ->
        M =:= <<"text/plain">>
    end),
    meck:expect(enterprise_asset_retention_logic, register_retention_tx, fun(_, _) -> ok end),
    meck:new(enterprise_audit_event_repo, [passthrough, no_link]),
    meck:expect(enterprise_audit_event_repo, append_tx, fun(_, _, _) -> {ok, 1} end),
    meck:new(enterprise_webhook_logic, [passthrough, no_link]),
    meck:expect(enterprise_webhook_logic, emit_event_tx, fun(_, _, _, _) -> ok end),
    meck:new(enterprise_application_usage_repo, [passthrough, no_link]),
    meck:expect(enterprise_application_usage_repo, bump_tx, fun(_, _, _, _) -> ok end),
    application:set_env(imboy, jwt_key, crypto:strong_rand_bytes(32)),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [{"/gateway", attachment_storage_gateway_handler, #{action => put}}]}
    ]),
    {ok, _} = cowboy:start_clear(internal_driver_fixture, [{ip, {127, 0, 0, 1}}, {port, 0}], #{
        env => #{dispatch => Dispatch}
    }),
    #{registry => Registry, bucket => Bucket, port => ranch:get_port(internal_driver_fixture)}.
cleanup(#{environment_before := Before} = State) ->
    try
        cleanup_resources(State)
    after
        cache_fixture_helper:restore_env(Before)
    end.

cleanup_resources(#{root := Root}) ->
    try
        cowboy:stop_listener(internal_driver_fixture)
    catch
        _:_ -> ok
    end,
    [
        try
            meck:unload(M)
        catch
            _:_ -> ok
        end
     || M <- [
            enterprise_asset_retention_logic,
            enterprise_audit_event_repo,
            enterprise_webhook_logic,
            enterprise_application_usage_repo
        ]
    ],
    try
        pooler:rm_pool(pgsql)
    catch
        _:_ -> ok
    end,
    case file:del_dir_r(Root) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> error({temporary_root_cleanup_failed, Reason})
    end.

%% Cleanup belongs to the EUnit fixture process, so a failed or timed-out
%% case cannot strand its temporary registry in the next case of this peer.
environment_case(Test) ->
    {setup,
        fun() ->
            {
                saved_environment([garage, attachment_storage_registry]),
                lists:member(attachment_storage, meck:mocked())
            }
        end,
        fun cleanup_environment_case/1, fun(_) -> Test end}.

cleanup_environment_case({Before, HadStorageMock}) ->
    try
        case {HadStorageMock, lists:member(attachment_storage, meck:mocked())} of
            {false, true} -> meck:unload(attachment_storage);
            _ -> ok
        end
    after
        cache_fixture_helper:restore_env(Before)
    end.

saved_environment(Keys) ->
    [{K, application:get_env(imboy, K)} || K <- Keys].

run(S, D, Fault) when Fault =:= no_uuid_registration; Fault =:= registration_unavailable ->
    Registry = maps:get(registry, S),
    Ns = <<"internal-", (atom_to_binary(D))/binary>>,
    R =
        case Fault of
            no_uuid_registration -> Registry#{upload_namespaces => #{<<"enterprise">> => Ns}};
            registration_unavailable -> #{version => 1, roots => #{}}
        end,
    application:set_env(imboy, attachment_storage_registry, R),
    Input = #{file_name => <<"original.txt">>, mime_type => <<"text/plain">>, size_bytes => 30},
    Result = elib_pg:with_tx(fun(C) ->
        enterprise_asset_logic:presign_tx(
            C,
            #{organization_id => 19101, application_id => 19102},
            Input
        )
    end),
    case Fault of
        registration_unavailable ->
            ?assertMatch({error, {<<"service_unavailable">>, _}}, Result);
        no_uuid_registration ->
            {ok, Stable} = Result,
            ?assertNot(maps:is_key(<<"put_url">>, Stable)),
            ?assertEqual(36, byte_size(maps:get(<<"client_upload_id">>, Stable))),
            {ok, Pending} = attach_pending_repo:status_lookup(maps:get(<<"object_key">>, Stable)),
            ?assertEqual(atom_to_binary(D), maps:get(<<"storage_driver">>, Pending)),
            ?assertEqual(Ns, maps:get(<<"storage_namespace">>, Pending))
    end,
    application:set_env(imboy, attachment_storage_registry, Registry);
run(S, D, Fault) ->
    Registry = maps:get(registry, S),
    application:set_env(imboy, attachment_storage_registry, Registry),
    Key = <<"eoa/19101/19102/fixture/", (elib_uuid:gen_v7())/binary>>,
    Ns = <<"internal-", (atom_to_binary(D))/binary>>,
    Container =
        case D of
            disk -> <<"fixture">>;
            garage -> maps:get(bucket, S)
        end,
    {ok, I} = attachment_storage:identity(Ns, Container, Key),
    Reg = #{
        <<"subject">> => #{
            <<"kind">> => <<"application">>,
            <<"principal">> => <<"19102">>,
            <<"org_id">> => <<"19101">>
        },
        <<"declaration">> => #{
            <<"scope">> => <<"enterprise">>,
            <<"org_id">> => 19101,
            <<"application_id">> => 19102,
            <<"mime">> => <<"text/plain">>
        }
    },
    {ok, [_]} = elib_pg:query(
        <<"SELECT * FROM fn_v5_register_pending($1,$2,'enterprise',0,$3::uuid,$4::jsonb,now()+interval '2 hours',now()+interval '1 hour',$5,$6,$7,$8)">>,
        [
            Key,
            maps:get(bucket, S),
            elib_uuid:gen_v7(),
            jsone:encode(Reg),
            atom_to_binary(D),
            Ns,
            Container,
            maps:get(locator_digest, I)
        ]
    ),
    Bytes = <<"real internal attachment bytes">>,
    {ok, Row} = attach_pending_repo:status_lookup(Key),
    {ok, Snapshot} = attachment_storage:frozen_location(Key, Row),
    %% C10：gateway 正文通道退役——按冻结位置快照直接签 S3 presigned PUT；
    %% pin 一次固定语义由 register_content_pin 承接（与 multipart 同形状）。
    {ok, PutUrl} = attachment_storage:presign_put_snapshot(Snapshot, <<"text/plain">>, 600),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {binary_to_list(PutUrl), [], "text/plain", Bytes},
        [{timeout, 30000}],
        [{body_format, binary}]
    ),
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    ok = attach_pending_repo:register_content_pin(Key, #{
        <<"version">> => 1,
        <<"sha256">> => Sha,
        <<"size_bytes">> => integer_to_list(byte_size(Bytes)),
        <<"mime">> => <<"text/plain">>
    }),
    case Fault of
        remap_rejected ->
            Roots = maps:map(
                fun(_, R) ->
                    case maps:get(namespace, R) of
                        Ns -> R#{endpoint => <<"file:///private/tmp/must-not-follow-internal-B">>};
                        _ -> R
                    end
                end,
                maps:get(roots, Registry)
            ),
            application:set_env(imboy, attachment_storage_registry, Registry#{roots => Roots}),
            ?assertEqual({error, {<<"conflict">>, location_changed}}, confirm(Key));
        F when F =:= confirm_replay; F =:= snapshot_switch ->
            Garage = application:get_env(imboy, garage, #{}),
            case F of
                snapshot_switch ->
                    meck:new(attachment_storage, [passthrough, no_link]),
                    meck:expect(attachment_storage, frozen_location, fun(K, P) ->
                        Result = meck:passthrough([K, P]),
                        Changed = maps:map(
                            fun(_, R) ->
                                R#{
                                    endpoint =>
                                        <<"file:///private/tmp/must-not-follow-after-validation-B">>
                                }
                            end,
                            maps:get(roots, Registry)
                        ),
                        application:set_env(imboy, attachment_storage_registry, Registry#{
                            roots => Changed
                        }),
                        application:set_env(imboy, garage, Garage#{
                            endpoint => <<"http://127.0.0.1:1">>
                        }),
                        Result
                    end);
                _ ->
                    ok
            end,
            First =
                try
                    confirm(Key)
                after
                    try
                        case F of
                            snapshot_switch -> meck:unload(attachment_storage);
                            _ -> ok
                        end
                    after
                        application:set_env(imboy, attachment_storage_registry, Registry),
                        application:set_env(imboy, garage, Garage)
                    end
                end,
            {ok, A} = First,
            {ok, B} = confirm(Key),
            ?assertEqual(maps:get(<<"file_id">>, A), maps:get(<<"file_id">>, B)),
            {ok, [Metadata]} = elib_pg:query(
                <<"SELECT storage_driver,storage_namespace,storage_container,storage_locator_sha256,referer_time FROM attachment WHERE path=$1">>,
                [Key]
            ),
            ?assertEqual(atom_to_binary(D), maps:get(<<"storage_driver">>, Metadata)),
            ?assertEqual(Ns, maps:get(<<"storage_namespace">>, Metadata)),
            ?assertEqual(Container, maps:get(<<"storage_container">>, Metadata)),
            ?assertEqual(
                maps:get(locator_digest, I), maps:get(<<"storage_locator_sha256">>, Metadata)
            ),
            ?assertEqual(1, maps:get(<<"referer_time">>, Metadata))
    end,
    application:set_env(imboy, attachment_storage_registry, Registry),
    ?assertEqual(
        {ok, Bytes},
        attachment_storage_garage:read_snapshot(Snapshot)
    ),
    ok = attachment_storage:delete_snapshot(Snapshot).
confirm(Key) ->
    elib_pg:with_tx(fun(C) ->
        enterprise_asset_logic:confirm_tx(
            C,
            #{organization_id => 19101, application_id => 19102},
            #{object_key => Key}
        )
    end).

%% V3-20 退役负例：显式 disk registry root 被 storage 层拒绝（不再有可执行
%% disk 位置；driver_module 不再分派 disk）。
disk_root_rejected() ->
    Reg = #{
        version => 1,
        roots => #{
            <<"retired-disk">> => #{
                driver => <<"disk">>,
                namespace => <<"retired-disk">>,
                endpoint => <<"file:///tmp/retired-disk">>,
                container => <<"fixture">>
            }
        },
        upload_namespaces => #{}
    },
    {error, {unsupported_driver, <<"retired-disk">>}} =
        attachment_storage:validate_registry_with_reg(Reg),
    Prev = application:get_env(imboy, attachment_storage_registry),
    application:set_env(imboy, attachment_storage_registry, Reg),
    R =
        (catch attachment_storage:registration_location(
            <<"private">>, <<"imboy">>, <<"u1/x/probe.bin">>
        )),
    case Prev of
        undefined -> application:unset_env(imboy, attachment_storage_registry);
        {ok, V} -> application:set_env(imboy, attachment_storage_registry, V)
    end,
    case R of
        {error, upload_root_unavailable} -> ok;
        Other -> erlang:error({disk_root_expected_rejected, Other})
    end,
    ok.

%% No PG/S3 required: exercise the real fixture lifetime in a fresh peer, since
%% cleanup_resources/1 owns the peer's named pool and listener. Assertions before
%% and after each setup/case execute in the same VM, including cancelled cases.
environment_isolation_test_() ->
    {timeout, 30, fun() -> pure_mock_fixture:run(fun environment_isolation_checks/0) end}.

environment_isolation_checks() ->
    Root = filename:join("/tmp", "enterprise-env-" ++ binary_to_list(elib_uuid:gen_v7())),
    ok = file:make_dir(Root),
    ConfigPath = filename:join(Root, "sys.config"),
    ManifestPath = filename:join(Root, "manifest.json"),
    Env = [
        {pg_conf, malformed_pg_config},
        {garage, #{fixture => changed}},
        {attachment_storage_registry, #{version => 1, roots => #{}}},
        {jwt_key, <<"synthetic-fixture-key">>},
        {enterprise_driver_env_regression, changed}
    ],
    Keys = proplists:get_keys(Env),
    Before = saved_environment(Keys),
    OsBefore = [{K, os:getenv(K)} || K <- ["W403_CONFIG_PATH", "W403_RESOURCE_MANIFEST"]],
    try
        ok = file:write_file(ConfigPath, io_lib:format("~p.~n", [[{imboy, Env}]])),
        ok = file:write_file(ManifestPath, <<"{\"database\":\"attv5_chain_env_regression\"}">>),
        true = os:putenv("W403_CONFIG_PATH", ConfigPath),
        true = os:putenv("W403_RESOURCE_MANIFEST", ManifestPath),
        lists:foreach(fun(Mode) -> check_environment_lifetime(Mode, Keys) end, [
            configured, unset, wrapped
        ])
    after
        cache_fixture_helper:restore_env(Before),
        [
            case Value of
                false -> os:unsetenv(Key);
                _ -> os:putenv(Key, Value)
            end
         || {Key, Value} <- OsBefore
        ],
        ok = file:del_dir_r(Root)
    end.

check_environment_lifetime(Mode, Keys) ->
    [application:unset_env(imboy, K) || K <- Keys],
    case Mode of
        unset ->
            ok;
        _ ->
            Value =
                case Mode of
                    configured -> #{original => true};
                    wrapped -> {ok, #{original => true}}
                end,
            [application:set_env(imboy, K, Value) || K <- [garage, attachment_storage_registry]],
            application:set_env(imboy, jwt_key, <<"original-fixture-key">>),
            application:set_env(imboy, pg_conf, original_pg_config)
    end,
    Before = saved_environment(Keys),
    State = setup(fun(_Root, Env) ->
        install_regression_environment(Env),
        #{}
    end),
    ?assertNotEqual(Before, saved_environment(Keys)),
    ok = cleanup(State),
    ?assertEqual(Before, saved_environment(Keys)),
    lists:foreach(
        fun(Class) ->
            ?assertMatch(
                {Class, fixture_setup_failure, [_ | _]},
                capture_environment_failure(fun() ->
                    setup(fun(_Root, Env) ->
                        install_regression_environment(Env),
                        apply(erlang, Class, [fixture_setup_failure])
                    end)
                end)
            ),
            ?assertEqual(Before, saved_environment(Keys))
        end,
        [error, throw, exit]
    ),
    %% Real setup_owned/2 installs the entire config and fails before starting PG.
    ?assertError({badmap, malformed_pg_config}, setup()),
    ?assertEqual(Before, saved_environment(Keys)),
    check_cleanup_failure(Before, Keys),
    lists:foreach(
        fun(Failure) ->
            Owner = self(),
            Ref = make_ref(),
            Test =
                {timeout, 0.05, fun() ->
                    application:set_env(imboy, garage, #{case_changed => true}),
                    application:set_env(imboy, attachment_storage_registry, #{case_changed => true}),
                    Owner ! {environment_changed, Ref},
                    case Failure of
                        killed ->
                            exit(self(), kill);
                        timeout ->
                            receive
                                fixture_never_sent -> ok
                            end
                    end
                end},
            %% The cancellation is the injected fault; assert its result without
            %% emitting a false Nightly failure summary from this inner runner.
            ?assertEqual(error, eunit:test(environment_case(Test), [no_tty])),
            receive
                {environment_changed, Ref} -> ok
            after 1000 -> error(cancelled_case_did_not_run)
            end,
            ?assertEqual(Before, saved_environment(Keys))
        end,
        [killed, timeout]
    ).

check_cleanup_failure(Before, Keys) ->
    State = setup(fun(_Root, Env) ->
        install_regression_environment(Env),
        #{}
    end),
    Root = maps:get(root, State),
    try
        ?assertMatch(
            {error, {temporary_root_cleanup_failed, _}, [_ | _]},
            capture_environment_failure(fun() -> cleanup(State#{root => Root ++ [0]}) end)
        ),
        ?assertEqual(Before, saved_environment(Keys))
    after
        ok = cleanup(State)
    end,
    %% A failing cleanup must not replace the original setup failure.
    ?assertMatch(
        {error, original_setup_failure, [_ | _]},
        capture_environment_failure(fun() ->
            setup(fun(_Root, Env) ->
                install_regression_environment(Env),
                cache_fixture_helper:on_failure(fun() -> error(fixture_cleanup_failure) end),
                error(original_setup_failure)
            end)
        end)
    ),
    ?assertEqual(Before, saved_environment(Keys)).

install_regression_environment(Env) ->
    [ok = application:set_env(imboy, K, V) || {K, V} <- Env],
    ok.

capture_environment_failure(Test) ->
    try Test() of
        Result -> {returned, Result}
    catch
        Class:Reason:Stack -> {Class, Reason, Stack}
    end.
