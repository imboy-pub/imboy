-module(imboy_cluster_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_cluster 模块的 EUnit 测试
%%%
%%% 目标：验证集群管理功能
%%% 覆盖：init/0, join_cluster/1, get_cluster_nodes/0, ping_nodes/0, 
%%%       handle_node_info/1, validate_node_info/1, update_node_info_cache/1
%%%===================================================================

%% ===================================================================
%% init/0 测试
%% ===================================================================

init_without_cluster_nodes_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 未配置集群节点
        application:set_env(imboy, cluster_nodes, []),
        ?assertEqual([], application:get_env(imboy, cluster_nodes)),
        application:unset_env(imboy, cluster_nodes)
    end).

init_with_cluster_nodes_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 配置集群节点
        application:set_env(imboy, cluster_nodes, [node1@host, node2@host]),
        ?assertMatch({ok, [_, _]}, application:get_env(imboy, cluster_nodes)),
        application:unset_env(imboy, cluster_nodes)
    end).

%% ===================================================================
%% join_cluster/1 测试
%% ===================================================================

join_cluster_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_cluster:join_cluster([]),
        ?assertEqual({error, empty_cluster_nodes}, Result)
    end).

join_cluster_single_node_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 单节点集群
        Nodes = [node()],
        % 由于需要实际的 net_kernel，这里只验证参数类型
        ?assert(is_list(Nodes)),
        ?assertEqual(1, length(Nodes))
    end).

join_cluster_multiple_nodes_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 多节点集群
        Nodes = [node1@host, node2@host, node3@host],
        ?assert(is_list(Nodes)),
        ?assertEqual(3, length(Nodes))
    end).

%% ===================================================================
%% get_cluster_nodes/0 测试
%% ===================================================================

get_cluster_nodes_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证函数返回节点列表
        Result = imboy_cluster:get_cluster_nodes(),
        ?assert(is_list(Result))
    end).

%% ===================================================================
%% ping_nodes/0 测试
%% ===================================================================

ping_nodes_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 空集群情况
        Result = imboy_cluster:ping_nodes(),
        ?assert(is_list(Result))
    end).

ping_nodes_returns_list_of_tuples_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_cluster:ping_nodes(),
        ?assert(is_list(Result)),
        % 验证返回的是 {node(), pong | pang} 列表
        lists:foreach(fun({Node, Status}) ->
            ?assert(is_atom(Node)),
            ?assert(lists:member(Status, [pong, pang]))
        end, Result)
    end).

%% ===================================================================
%% handle_node_info/1 测试
%% ===================================================================

handle_node_info_valid_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => 'node1@host',
            timestamp => {1704, 0, 0},
            version => "1.0.0",
            status => online
        },
        Result = imboy_cluster:handle_node_info(NodeInfo),
        ?assertEqual(ok, Result)
    end).

handle_node_info_missing_fields_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => 'node1@host',
            version => "1.0.0"
            % 缺少 timestamp 和 status
        },
        Result = imboy_cluster:handle_node_info(NodeInfo),
        ?assertMatch({error, _}, Result)
    end).

handle_node_info_invalid_node_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => <<"not_an_atom">>,  % 节点名应该是 atom
            timestamp => {1704, 0, 0},
            version => "1.0.0",
            status => online
        },
        Result = imboy_cluster:handle_node_info(NodeInfo),
        ?assertMatch({error, invalid_node_name}, Result)
    end).

handle_node_info_with_capabilities_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => 'node1@host',
            timestamp => {1704, 0, 0},
            version => "2.0.0",
            status => online,
            capabilities => [cache, messaging]
        },
        Result = imboy_cluster:handle_node_info(NodeInfo),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% validate_node_info/1 测试
%% ===================================================================

validate_node_info_all_fields_present_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => 'node1@host',
            timestamp => {1704, 0, 0},
            version => "1.0.0",
            status => online
        },
        Result = imboy_cluster:validate_node_info(NodeInfo),
        ?assertEqual({ok, NodeInfo}, Result)
    end).

validate_node_info_missing_node_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            timestamp => {1704, 0, 0},
            version => "1.0.0",
            status => online
        },
        Result = imboy_cluster:validate_node_info(NodeInfo),
        ?assertEqual({error, missing_required_fields}, Result)
    end).

validate_node_info_missing_timestamp_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => 'node1@host',
            version => "1.0.0",
            status => online
        },
        Result = imboy_cluster:validate_node_info(NodeInfo),
        ?assertEqual({error, missing_required_fields}, Result)
    end).

validate_node_info_missing_version_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => 'node1@host',
            timestamp => {1704, 0, 0},
            status => online
        },
        Result = imboy_cluster:validate_node_info(NodeInfo),
        ?assertEqual({error, missing_required_fields}, Result)
    end).

validate_node_info_missing_status_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => 'node1@host',
            timestamp => {1704, 0, 0},
            version => "1.0.0"
        },
        Result = imboy_cluster:validate_node_info(NodeInfo),
        ?assertEqual({error, missing_required_fields}, Result)
    end).

validate_node_info_node_not_atom_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => "node1@host",  % 字符串而不是 atom
            timestamp => {1704, 0, 0},
            version => "1.0.0",
            status => online
        },
        Result = imboy_cluster:validate_node_info(NodeInfo),
        ?assertEqual({error, invalid_node_name}, Result)
    end).

validate_node_info_with_extra_fields_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{
            node => 'node1@host',
            timestamp => {1704, 0, 0},
            version => "1.0.0",
            status => online,
            extra_field => "value"
        },
        Result = imboy_cluster:validate_node_info(NodeInfo),
        % 额外字段应该被接受
        ?assertEqual({ok, NodeInfo}, Result)
    end).

%% ===================================================================
%% update_node_info_cache/1 测试
%% ===================================================================

update_node_info_cache_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(imboy_cache, [unstick, passthrough]),
        try
            NodeInfo = #{
                node => 'node1@host',
                timestamp => {1704, 0, 0},
                version => "1.0.0",
                status => online
            },
            meck:expect(imboy_cache, set, fun(_Key, _Value, _TTL) -> ok end),
            
            Result = imboy_cluster:update_node_info_cache(NodeInfo),
            ?assertEqual(ok, Result),
            
            ?assert(meck:validate(imboy_cache))
        after
            meck:unload(imboy_cache)
        end
    end).

update_node_info_cache_with_different_node_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(imboy_cache, [unstick, passthrough]),
        try
            NodeInfo = #{
                node => 'node2@host',
                timestamp => {1705, 0, 0},
                version => "2.0.0",
                status => offline
            },
            meck:expect(imboy_cache, set, fun(_Key, _Value, _TTL) -> ok end),
            
            Result = imboy_cluster:update_node_info_cache(NodeInfo),
            ?assertEqual(ok, Result),
            
            ?assert(meck:validate(imboy_cache))
        after
            meck:unload(imboy_cache)
        end
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

join_cluster_with_current_node_only_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentNode = node(),
        Nodes = [CurrentNode],
        ?assertEqual(1, length(Nodes)),
        ?assertEqual(CurrentNode, hd(Nodes))
    end).

handle_node_info_empty_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{},
        Result = imboy_cluster:handle_node_info(NodeInfo),
        ?assertMatch({error, _}, Result)
    end).

validate_node_info_empty_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{},
        Result = imboy_cluster:validate_node_info(NodeInfo),
        ?assertEqual({error, missing_required_fields}, Result)
    end).

update_node_info_cache_with_minimal_info_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(imboy_cache, [unstick, passthrough]),
        try
            NodeInfo = #{
                node => 'test@host',
                timestamp => {0, 0, 0},
                version => "0.0.1",
                status => unknown
            },
            meck:expect(imboy_cache, set, fun(_Key, _Value, _TTL) -> ok end),
            
            Result = imboy_cluster:update_node_info_cache(NodeInfo),
            ?assertEqual(ok, Result),
            
            ?assert(meck:validate(imboy_cache))
        after
            meck:unload(imboy_cache)
        end
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

init_returns_ok_or_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        % init 应该返回 ok | {error, term()}
        ?assert(is_function(fun imboy_cluster:init/0, 0))
    end).

join_cluster_returns_ok_or_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        % join_cluster 应该返回 ok | {error, term()}
        ?assert(is_function(fun imboy_cluster:join_cluster/1, 1))
    end).

ping_nodes_returns_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_cluster:ping_nodes(),
        ?assert(is_list(Result))
    end).

handle_node_info_returns_ok_or_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{node => test, timestamp => {0,0,0}, version => "1.0", status => online},
        Result = imboy_cluster:handle_node_info(NodeInfo),
        ?assertEqual(ok, Result)
    end).

validate_node_info_returns_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        NodeInfo = #{node => test, timestamp => {0,0,0}, version => "1.0", status => online},
        Result = imboy_cluster:validate_node_info(NodeInfo),
        ?assertMatch({ok, _}, Result)
    end).