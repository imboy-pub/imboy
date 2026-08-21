-module(channel_discovery_e2e_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% Channel Discovery 端到端契约测试
%%%
%%% 验证：
%%% - Channel Discovery 路由存在
%%% - 核心模块导出契约稳定
%%% - 迁移文件包含关键表定义
%%%===================================================================

%% ===================================================================
%% 路由契约
%% ===================================================================

channel_discovery_routes_exist_test() ->
    Source = read_file("src/imboy_router.erl"),
    lists:foreach(
        fun(Path) ->
            ?assert(binary:match(Source, Path) =/= nomatch)
        end,
        [
            <<"/api/v1/channels/search">>,
            <<"/api/v1/channels/discover">>,
            <<"/api/v1/channels/featured">>,
            <<"/api/v1/channels/trending">>,
            <<"/api/v1/channels/categories">>
        ]
    ).

%% ===================================================================
%% 模块导出契约
%% ===================================================================

channel_discovery_logic_exports_test() ->
    ensure_module_loaded(channel_discovery_logic),
    ?assert(erlang:function_exported(channel_discovery_logic, search, 4)),
    ?assert(erlang:function_exported(channel_discovery_logic, discover, 4)),
    ?assert(erlang:function_exported(channel_discovery_logic, featured, 1)),
    ?assert(erlang:function_exported(channel_discovery_logic, trending, 2)),
    ?assert(erlang:function_exported(channel_discovery_logic, categories, 0)).

%% ===================================================================
%% 迁移文件契约
%% ===================================================================

fts_channel_migration_contains_table_test() ->
    Migration = read_file(fts_channel_migration_path()),
    ?assert(binary:match(Migration, <<"fts_channel">>) =/= nomatch).

%% ===================================================================
%% Internal
%% ===================================================================

read_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

fts_channel_migration_path() ->
    case filelib:wildcard("priv/migrations/*00000069_channel_discovery*.up.sql") of
        [Path | _] ->
            Path;
        [] ->
            error({missing_migration, "00000069_channel_discovery"})
    end.

ensure_module_loaded(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    ok.
