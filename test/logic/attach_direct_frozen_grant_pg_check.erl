-module(attach_direct_frozen_grant_pg_check).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() -> snapshot_switch().

snapshot_switch() ->
    Options = #{
        host => "127.0.0.1",
        port => list_to_integer(os:getenv("C04_FROZEN_PG_PORT")),
        username => os:getenv("POSTGRES_USER"),
        password => os:getenv("POSTGRES_PASSWORD"),
        database => os:getenv("C04_FROZEN_PG_DB"),
        timeout => 5000
    },
    {ok, Conn} = epgsql:connect(Options),
    meck:new([pooler, config_ds], [no_link, non_strict]),
    Before = [{K, application:get_env(imboy, K)} || K <- [garage, attachment_storage_registry]],
    try
        meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
        meck:expect(pooler, take_member, fun(pgsql, _) -> Conn end),
        meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
        meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
        meck:expect(pooler, return_member, fun(pgsql, _, fail) -> ok end),
        A = #{
            endpoint => <<"http://127.0.0.1:49510">>,
            bucket => <<"c04-test">>,
            access_key => binary:encode_hex(crypto:strong_rand_bytes(12)),
            secret_key => binary:encode_hex(crypto:strong_rand_bytes(24)),
            region => <<"garage">>
        },
        B = A#{
            endpoint => <<"http://127.0.0.1:49520">>,
            access_key => binary:encode_hex(crypto:strong_rand_bytes(12))
        },
        Root = #{
            driver => <<"garage">>,
            namespace => <<"c04-frozen">>,
            endpoint => maps:get(endpoint, A),
            container => maps:get(bucket, A)
        },
        ok = application:set_env(imboy, garage, A),
        ok = application:set_env(imboy, attachment_storage_registry, #{
            version => 1,
            roots => #{<<"c04-frozen">> => Root},
            upload_namespaces => #{<<"private">> => <<"c04-frozen">>}
        }),
        Uid = erlang:system_time(microsecond),
        {ok, #{object_key := Key}} = attach_direct_logic:register(
            Uid,
            <<"private">>,
            undefined,
            undefined,
            <<"frozen.txt">>
        ),
        Hash = binary:copy(<<"a">>, 64),
        ok = attach_direct_logic:declare(Key, Hash, 10, <<"text/plain">>),
        ok = application:set_env(imboy, garage, B),
        ?assertMatch(
            {error, {location_unverified, location_changed}}, attach_direct_logic:grant(Key, Hash)
        ),
        {ok, _, [{<<"DECLARED">>, 0}]} = epgsql:equery(
            Conn,
            "SELECT direct_state,(SELECT count(*) FROM attach_direct_attempt a WHERE a.object_key=p.object_key) "
            "FROM attach_pending p WHERE object_key=$1",
            [Key]
        ),
        ok = application:set_env(imboy, garage, maps:remove(secret_key, A)),
        ?assertEqual(
            {error, signing_credentials_unavailable}, attach_direct_logic:grant(Key, Hash)
        ),
        {ok, _, [{<<"DECLARED">>, 0}]} = epgsql:equery(
            Conn,
            "SELECT direct_state,(SELECT count(*) FROM attach_direct_attempt a WHERE a.object_key=p.object_key) "
            "FROM attach_pending p WHERE object_key=$1",
            [Key]
        ),
        ok = application:set_env(imboy, garage, A),
        {ok, #{attempt_id := 1, grant := #{url := Url}}} = attach_direct_logic:grant(Key, Hash),
        ?assertMatch({0, _}, binary:match(Url, <<"http://127.0.0.1:49510/">>)),
        ok = application:set_env(imboy, garage, B),
        ?assertMatch(
            {error, {location_unverified, location_changed}},
            attach_direct_logic:renew_grant(Key, Hash)
        ),
        {ok, _, [{<<"GRANTED">>, 1}]} = epgsql:equery(
            Conn,
            "SELECT direct_state,(SELECT count(*) FROM attach_direct_attempt a WHERE a.object_key=p.object_key) "
            "FROM attach_pending p WHERE object_key=$1",
            [Key]
        ),
        ok
    after
        [
            case V of
                undefined -> application:unset_env(imboy, K);
                {ok, X} -> application:set_env(imboy, K, X)
            end
         || {K, V} <- Before
        ],
        meck:unload([pooler, config_ds]),
        epgsql:close(Conn)
    end.
