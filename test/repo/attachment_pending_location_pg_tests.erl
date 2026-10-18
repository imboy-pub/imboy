-module(attachment_pending_location_pg_tests).
-include_lib("eunit/include/eunit.hrl").

registration_location_pg_test_() ->
    isolated_pg_fixture:test(fun registration_location_fixture/0).

registration_location_fixture() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [{timeout, 30, fun() -> registration(Driver) end} || Driver <- [<<"disk">>, <<"garage">>]]
    end}.

setup() ->
    {ok, Manifest} = file:read_file(required("ATTACHMENT_REVIEW_MANIFEST")),
    #{<<"database">> := Db} = jsone:decode(Manifest),
    <<"attv5_chain_", _/binary>> = Db,
    {ok, [Config]} = file:consult(required("ATTACHMENT_REVIEW_CONFIG")),
    Env = proplists:get_value(imboy, Config),
    Pg = proplists:get_value(pg_conf, Env),
    {epgsql, connect, [Connect0]} = maps:get(start_mfa, Pg),
    Host = maps:get(host, Connect0),
    true = lists:member(Host, ["127.0.0.1", <<"127.0.0.1">>, "localhost", <<"localhost">>]),
    Connect = Connect0#{database => binary_to_list(Db)},
    application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _} = pooler:new_pool(Pg#{name => pgsql, start_mfa => {epgsql, connect, [Connect]}}),
    {ok, [#{<<"db">> := Db}]} = elib_pg:query(<<"SELECT current_database() AS db">>, []),
    ok.

cleanup(_) -> pooler:rm_pool(pgsql).

required(Name) ->
    case os:getenv(Name) of
        false -> error({required_resource_missing, Name});
        "" -> error({required_resource_missing, Name});
        Value -> Value
    end.

registration(Driver) ->
    Ns = <<"registration-", Driver/binary>>,
    Endpoint =
        case Driver of
            <<"disk">> -> <<"file:///tmp/attv5-registration-owned">>;
            <<"garage">> -> <<"http://127.0.0.1:1">>
        end,
    application:set_env(imboy, garage, #{
        endpoint => <<"http://127.0.0.1:1">>, bucket => <<"fixture">>
    }),
    Rec = #{driver => Driver, namespace => Ns, container => <<"fixture">>, endpoint => Endpoint},
    Registry = #{version => 1, roots => #{Ns => Rec}, upload_namespaces => #{<<"private">> => Ns}},
    application:set_env(imboy, attachment_storage_registry, Registry),
    Cid = elib_uuid:gen_v7(),
    Key = <<"registration/", Cid/binary>>,
    Declaration = #{
        <<"filename">> => <<"fixture.txt">>,
        <<"mime">> => <<"text/plain">>,
        <<"scope">> => <<"private">>,
        <<"scope_ref">> => null
    },
    try registration(Driver, Registry, Cid, Key, Declaration)
    after
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM public.attach_pending WHERE object_key=ANY($1)">>,
            [[Key, <<Key/binary, "/new-random">>, <<Key/binary, "/old-client">>,
                <<Key/binary, "/internal">>]]
        )
    end.

registration(Driver, Registry, Cid, Key, Declaration) ->
    Ns = <<"registration-", Driver/binary>>,
    {ok, Original} = attach_pending_repo:register_v5(
        Key, <<"fixture">>, <<"private">>, 99191, Cid, Declaration
    ),
    {ok, [Row]} = elib_pg:query(
        <<
            "SELECT storage_driver,storage_namespace,storage_container,"
            "storage_locator_sha256 FROM attach_pending WHERE client_upload_id=$1::uuid"
        >>,
        [Cid]
    ),
    ?assertEqual(Driver, maps:get(<<"storage_driver">>, Row)),
    ?assertEqual(Ns, maps:get(<<"storage_namespace">>, Row)),
    ?assertEqual(<<"fixture">>, maps:get(<<"storage_container">>, Row)),
    ?assertEqual(64, byte_size(maps:get(<<"storage_locator_sha256">>, Row))),
    application:set_env(imboy, attachment_storage_registry, #{version => 1, roots => #{}}),
    {ok, Replay} = attach_pending_repo:register_v5(
        <<Key/binary, "/new-random">>,
        <<"different-current-bucket">>,
        <<"private">>,
        99191,
        Cid,
        Declaration
    ),
    ?assertEqual(maps:get(object_key, Original), maps:get(object_key, Replay)),
    ?assertEqual(maps:get(expires_at, Original), maps:get(expires_at, Replay)),
    application:set_env(imboy, attachment_storage_registry, Registry),
    NewKey = <<Key/binary, "/old-client">>,
    {ok, Legacy} = attach_pending_repo:register_v5(
        NewKey, <<"fixture">>, <<"private">>, 99191, undefined, Declaration
    ),
    ?assert(is_binary(maps:get(client_upload_id, Legacy))),
    ?assertEqual(NewKey, maps:get(object_key, Legacy)),
    AppCid = elib_uuid:gen_v7(),
    {ok, Internal} = elib_pg:with_tx(fun(Conn) ->
        attach_pending_repo:register_v5_tx(
            Conn,
            <<Key/binary, "/internal">>,
            <<"fixture">>,
            <<"private">>,
            0,
            AppCid,
            #{
                <<"subject">> => #{
                    <<"kind">> => <<"application">>, <<"principal">> => <<"fixture">>
                },
                <<"declaration">> => Declaration
            }
        )
    end),
    ?assertEqual(AppCid, maps:get(client_upload_id, Internal)).
