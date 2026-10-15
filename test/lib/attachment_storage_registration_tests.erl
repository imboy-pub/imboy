-module(attachment_storage_registration_tests).
-include_lib("eunit/include/eunit.hrl").
registration_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun exact_unique_garage/0,
        fun ambiguous_garage_denied/0,
        fun disk_requires_explicit_mapping/0,
        fun explicit_unknown_denied/0
    ]}.
setup() ->
    P = {
        application:get_env(imboy, attachment_storage_registry), application:get_env(imboy, garage)
    },
    application:set_env(imboy, garage, #{endpoint => <<"http://127.0.0.1:3900">>}),
    P.
cleanup({R, G}) ->
    restore(attachment_storage_registry, R),
    restore(garage, G).
restore(K, {ok, V}) -> application:set_env(imboy, K, V);
restore(K, undefined) -> application:unset_env(imboy, K).
garage() ->
    #{
        driver => <<"garage">>,
        namespace => <<"g">>,
        endpoint => <<"http://127.0.0.1:3900">>,
        container => <<"b">>
    }.
exact_unique_garage() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1, roots => #{<<"g-root">> => garage()}
    }),
    ?assertMatch(
        {ok, #{
            driver := <<"garage">>, namespace := <<"g">>, container := <<"b">>, locator_digest := _
        }},
        attachment_storage:registration_location(<<"private">>, <<"b">>, <<"key">>)
    ),
    ?assertMatch(
        {error, _}, attachment_storage:registration_location(<<"private">>, <<"other">>, <<"key">>)
    ).
ambiguous_garage_denied() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1, roots => #{<<"a">> => garage(), <<"b">> => (garage())#{namespace => <<"g2">>}}
    }),
    ?assertEqual(
        {error, upload_root_unavailable},
        attachment_storage:registration_location(<<"private">>, <<"b">>, <<"key">>)
    ).
disk_requires_explicit_mapping() ->
    R = #{
        version => 1,
        roots => #{
            <<"d-root">> => #{
                driver => <<"disk">>,
                namespace => <<"d">>,
                endpoint => <<"file:///private/tmp/owned">>,
                container => <<"fixed">>
            }
        }
    },
    application:set_env(imboy, attachment_storage_registry, R),
    ?assertEqual(
        {error, upload_root_unavailable},
        attachment_storage:registration_location(<<"private">>, <<"b">>, <<"key">>)
    ),
    application:set_env(imboy, attachment_storage_registry, R#{
        upload_namespaces => #{<<"private">> => <<"d">>}
    }),
    ?assertMatch(
        {ok, #{driver := <<"disk">>, namespace := <<"d">>, container := <<"fixed">>}},
        attachment_storage:registration_location(<<"private">>, <<"b">>, <<"key">>)
    ).
explicit_unknown_denied() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{<<"g-root">> => garage()},
        upload_namespaces => #{<<"private">> => <<"missing">>}
    }),
    ?assertEqual(
        {error, upload_root_unavailable},
        attachment_storage:registration_location(<<"private">>, <<"b">>, <<"key">>)
    ).
