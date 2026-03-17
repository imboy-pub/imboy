-module(imboy_cluster_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

init_without_cluster_nodes_returns_ok_test_() ->
    ?TEST_WITH_APP(fun() ->
        application:set_env(imboy, cluster_nodes, []),
        try
            ?assertEqual(ok, imboy_cluster:init())
        after
            application:unset_env(imboy, cluster_nodes)
        end
    end).

init_with_current_node_config_returns_ok_test_() ->
    ?TEST_WITH_APP(fun() ->
        application:set_env(imboy, cluster_nodes, [node()]),
        try
            ?assertEqual(ok, imboy_cluster:init())
        after
            application:unset_env(imboy, cluster_nodes)
        end
    end).

join_cluster_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, empty_cluster_nodes}, imboy_cluster:join_cluster([]))
    end).

join_cluster_current_node_only_returns_no_nodes_connected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, no_nodes_connected}, imboy_cluster:join_cluster([node()]))
    end).

get_cluster_nodes_returns_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(is_list(imboy_cluster:get_cluster_nodes()))
    end).

ping_nodes_returns_node_status_pairs_test_() ->
    ?TEST_SIMPLE(fun() ->
        lists:foreach(
            fun({ClusterNode, Status}) ->
                ?assert(is_atom(ClusterNode)),
                ?assert(lists:member(Status, [pong, pang]))
            end,
            imboy_cluster:ping_nodes()
        )
    end).

handle_node_info_valid_updates_cache_test_() ->
    ?TEST_WITH_APP(fun() ->
        ?WITH_MECK(imboy_cache, [
            {'set', 3, fun(Key, Value, TTL) ->
                ?assertEqual({cluster_node_info, 'node1@host'}, Key),
                ?assertEqual(300, TTL),
                ?assertEqual('node1@host', maps:get(node, Value)),
                ok
            end}
        ], fun() ->
            NodeInfo = #{
                node => 'node1@host',
                timestamp => {1704, 0, 0},
                version => "1.0.0",
                status => online,
                capabilities => [cache, cluster]
            },
            ?assertEqual(ok, imboy_cluster:handle_node_info(NodeInfo))
        end)
    end).

handle_node_info_missing_fields_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, missing_required_fields},
            imboy_cluster:handle_node_info(#{
                node => 'node1@host',
                version => "1.0.0"
            })
        )
    end).

handle_node_info_invalid_node_name_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_node_name},
            imboy_cluster:handle_node_info(#{
                node => <<"not_an_atom">>,
                timestamp => {1704, 0, 0},
                version => "1.0.0",
                status => online
            })
        )
    end).

handle_node_info_accepts_extra_fields_test_() ->
    ?TEST_WITH_APP(fun() ->
        ?WITH_MECK(imboy_cache, [
            {'set', 3, fun(_Key, _Value, _TTL) -> ok end}
        ], fun() ->
            ?assertEqual(
                ok,
                imboy_cluster:handle_node_info(#{
                    node => test@host,
                    timestamp => {0, 0, 0},
                    version => "1.0",
                    status => online,
                    extra_field => <<"value">>
                })
            )
        end)
    end).

handle_node_info_empty_map_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, missing_required_fields}, imboy_cluster:handle_node_info(#{}))
    end).
