%%% Real owned PG + Garage/Disk reads; governance side effects are isolated stubs.
-module(enterprise_asset_driver_pg_tests).
-include_lib("eunit/include/eunit.hrl").

internal_driver_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        [
            {atom_to_list(D) ++ "/" ++ atom_to_list(F), {timeout, 90, fun() -> run(S, D, F) end}}
         || D <- [disk, garage],
            F <- [
                confirm_replay,
                snapshot_switch,
                remap_rejected,
                no_uuid_registration,
                registration_unavailable
            ]
        ]
    end}.
setup() ->
    {ok, B} = file:read_file(os:getenv("W403_RESOURCE_MANIFEST")),
    #{<<"database">> := Db} = jsone:decode(B),
    <<"attv5_chain_", _/binary>> = Db,
    {ok, [Config]} = file:consult(os:getenv("W403_CONFIG_PATH")),
    [application:set_env(imboy, K, V) || {K, V} <- proplists:get_value(imboy, Config)],
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
    Root = <<"file:///private/tmp/attv5-internal-", (elib_uuid:gen_v7())/binary>>,
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
cleanup(_) ->
    cowboy:stop_listener(internal_driver_fixture),
    [
        meck:unload(M)
     || M <- [
            enterprise_asset_retention_logic,
            enterprise_audit_event_repo,
            enterprise_webhook_logic,
            enterprise_application_usage_repo
        ]
    ],
    pooler:rm_pool(pgsql).
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
    Expiry = erlang:system_time(second) + 60,
    {ok, Cap} = imboy_jwt:issue_upload_capability(Key, <<"text/plain">>, Expiry),
    Url = "http://127.0.0.1:" ++ integer_to_list(maps:get(port, S)) ++ "/gateway",
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {Url, [{"x-upload-capability", binary_to_list(Cap)}], "text/plain", Bytes},
        [{timeout, 30000}],
        [{body_format, binary}]
    ),
    {ok, Row} = attach_pending_repo:status_lookup(Key),
    {ok, Snapshot} = attachment_storage:frozen_location(Key, Row),
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
                    case F of
                        snapshot_switch -> meck:unload(attachment_storage);
                        _ -> ok
                    end,
                    application:set_env(imboy, attachment_storage_registry, Registry),
                    application:set_env(imboy, garage, Garage)
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
        case D of
            disk -> attachment_storage_disk:read_snapshot(Snapshot);
            garage -> attachment_storage_garage:read_snapshot(Snapshot)
        end
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
