%% @doc 集群管理模块
%% 负责Erlang节点的集群加入和管理功能
-module(imboy_cluster).
-author("imboy").

-export([init/0, join_cluster/1, get_cluster_nodes/0, ping_nodes/0, handle_node_info/1]).

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 初始化集群管理
-spec init() -> ok | {error, term()}.
init() ->
    % 获取当前节点名称
    CurrentNode = node(),
    imboy_log:info("集群管理初始化，当前节点: ~p", [CurrentNode]),
    
    % 从配置中获取集群节点列表
    ClusterNodes = get_cluster_nodes_from_config(),
    
    % 如果有配置集群节点，则尝试加入集群
    case ClusterNodes of
        [] ->
            imboy_log:info("未配置集群节点，以单节点模式运行");
        _ ->
            imboy_log:info("发现集群节点配置: ~p", [ClusterNodes]),
            % 尝试加入集群
            case join_cluster(ClusterNodes) of
                ok ->
                    imboy_log:info("成功加入集群");
                {error, Reason} ->
                    imboy_log:error("加入集群失败: ~p", [Reason])
            end
    end,
    ok.

%% @doc 加入集群
-spec join_cluster([node()]) -> ok | {error, term()}.
join_cluster([]) ->
    {error, empty_cluster_nodes};
join_cluster(ClusterNodes) ->
    CurrentNode = node(),
    imboy_log:info("尝试加入集群，当前节点: ~p，目标节点: ~p", [CurrentNode, ClusterNodes]),
    
    % 检查是否已经在集群中
    case lists:member(CurrentNode, ClusterNodes) of
        true ->
            imboy_log:info("当前节点已在集群配置中");
        false ->
            ok
    end,
    
    % 尝试连接到集群中的每个节点
    Results = lists:map(
        fun(TargetNode) ->
            case TargetNode of
                CurrentNode ->
                    {CurrentNode, skip};
                _ ->
                    case net_kernel:connect_node(TargetNode) of
                        true ->
                            imboy_log:info("成功连接到节点: ~p", [TargetNode]),
                            {TargetNode, connected};
                        false ->
                            imboy_log:warning("无法连接到节点: ~p", [TargetNode]),
                            {TargetNode, failed}
                    end
            end
        end,
        ClusterNodes
    ),
    
    % 检查是否至少成功连接到一个节点
    ConnectedNodes = [Node || {Node, connected} <- Results],
    case ConnectedNodes of
        [] ->
            {error, no_nodes_connected};
        _ ->
            imboy_log:info("成功连接到 ~p 个集群节点", [length(ConnectedNodes)]),
            % 向集群广播当前节点信息
            broadcast_node_info(ConnectedNodes),
            ok
    end.

%% @doc 获取当前集群中的所有节点
-spec get_cluster_nodes() -> [node()].
get_cluster_nodes() ->
    nodes().

%% @doc ping所有集群节点
-spec ping_nodes() -> [{node(), pong | pang}].
ping_nodes() ->
    ClusterNodes = get_cluster_nodes(),
    lists:map(
        fun(Node) ->
            case net_adm:ping(Node) of
                pong ->
                    {Node, pong};
                pang ->
                    {Node, pang}
            end
        end,
        ClusterNodes
    ).

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 从配置中获取集群节点列表
-spec get_cluster_nodes_from_config() -> [node()].
get_cluster_nodes_from_config() ->
    case application:get_env(imboy, cluster_nodes, []) of
        [] ->
            % 如果没有配置，尝试从环境变量获取
            case os:getenv("CLUSTER_NODES") of
                false ->
                    [];
                NodesStr ->
                    parse_cluster_nodes_string(NodesStr)
            end;
        Nodes ->
            Nodes
    end.

%% @doc 解析集群节点字符串
-spec parse_cluster_nodes_string(string()) -> [node()].
parse_cluster_nodes_string(NodesStr) ->
    NodesList = string:split(NodesStr, ",", all),
    lists:map(
        fun(NodeStr) ->
            Trimmed = string:trim(NodeStr),
            list_to_atom(Trimmed)
        end,
        NodesList
    ).

%% @doc 向集群广播当前节点信息
-spec broadcast_node_info([node()]) -> ok.
broadcast_node_info([]) ->
    ok;
broadcast_node_info(Nodes) ->
    % 获取当前节点信息
    CurrentNode = node(),
    NodeInfo = #{
        node => CurrentNode,
        timestamp => erlang:timestamp(),
        version => "1.0.0",
        status => online,
        capabilities => [cache, cluster, messaging]
    },
    
    % 使用rpc向集群中的每个节点发送节点信息
    lists:foreach(
        fun(TargetNode) ->
            % 调用目标节点的节点信息处理函数
            case rpc:call(TargetNode, imboy_cluster, handle_node_info, [NodeInfo]) of
                ok ->
                    % 目标节点成功接收并处理节点信息
                    imboy_log:info("向节点 ~p 广播信息成功: ~p", [TargetNode, NodeInfo]);
                {badrpc, Reason} ->
                    imboy_log:warning("向节点 ~p 广播信息失败: ~p", [TargetNode, Reason]);
                {error, HandleReason} ->
                    imboy_log:warning("节点 ~p 处理信息失败: ~p", [TargetNode, HandleReason])
            end
        end,
        Nodes
    ),
    ok.

%% @doc 处理接收到的节点信息
-spec handle_node_info(map()) -> ok | {error, term()}.
handle_node_info(NodeInfo) ->
    % 验证节点信息格式
    case validate_node_info(NodeInfo) of
        {ok, ValidInfo} ->
            SourceNode = maps:get(node, ValidInfo),
            imboy_log:info("接收到节点 ~p 的信息: ~p", [SourceNode, ValidInfo]),
            
            % 更新本地节点信息缓存
            update_node_info_cache(ValidInfo),
            
            % 如果需要，可以向其他节点转发此信息
            % forward_node_info(ValidInfo),
            
            ok;
        {error, Reason} ->
            imboy_log:warning("接收到无效的节点信息: ~p, 错误: ~p", [NodeInfo, Reason]),
            {error, Reason}
    end.

%% @doc 验证节点信息格式
-spec validate_node_info(map()) -> {ok, map()} | {error, term()}.
validate_node_info(NodeInfo) ->
    RequiredFields = [node, timestamp, version, status],
    case lists:all(fun(Field) -> maps:is_key(Field, NodeInfo) end, RequiredFields) of
        true ->
            % 验证节点名是否为atom
            Node = maps:get(node, NodeInfo),
            case is_atom(Node) of
                true ->
                    {ok, NodeInfo};
                false ->
                    {error, invalid_node_name}
            end;
        false ->
            {error, missing_required_fields}
    end.

%% @doc 更新节点信息缓存
-spec update_node_info_cache(map()) -> ok.
update_node_info_cache(NodeInfo) ->
    SourceNode = maps:get(node, NodeInfo),
    CacheKey = {cluster_node_info, SourceNode},
    
    % 设置缓存，过期时间为5分钟
    CacheTTL = 300, % 5分钟
    imboy_cache:set(CacheKey, NodeInfo, CacheTTL),
    
    imboy_log:debug("已更新节点 ~p 的信息缓存", [SourceNode]),
    ok.
