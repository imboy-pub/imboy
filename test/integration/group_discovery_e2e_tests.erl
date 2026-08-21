-module(group_discovery_e2e_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% Group Discovery 端到端契约测试
%%%
%%% 验证：
%%% - Group Discovery 路由存在
%%% - 核心模块导出契约稳定
%%% - 迁移文件包含关键表定义
%%%===================================================================

%% ===================================================================
%% 路由契约
%% ===================================================================

group_discovery_routes_exist_test() ->
    Source = read_file("src/imboy_router.erl"),
    lists:foreach(
        fun(Path) ->
            ?assert(binary:match(Source, Path) =/= nomatch)
        end,
        [
            <<"/api/v1/group/search">>,
            <<"/api/v1/group/discover">>,
            <<"/api/v1/group/featured">>,
            <<"/api/v1/group/hot">>,
            <<"/api/v1/group/categories">>,
            <<"/api/v1/group/preview">>
        ]
    ).

%% ===================================================================
%% 模块导出契约
%% ===================================================================

group_discovery_logic_exports_test() ->
    ensure_module_loaded(group_discovery_logic),
    ?assert(erlang:function_exported(group_discovery_logic, search, 4)),
    ?assert(erlang:function_exported(group_discovery_logic, discover, 4)),
    ?assert(erlang:function_exported(group_discovery_logic, featured, 1)),
    ?assert(erlang:function_exported(group_discovery_logic, hot, 1)),
    ?assert(erlang:function_exported(group_discovery_logic, categories, 0)),
    ?assert(erlang:function_exported(group_discovery_logic, preview, 1)).

fts_group_repo_exports_test() ->
    ensure_module_loaded(fts_group_repo),
    ?assert(erlang:function_exported(fts_group_repo, tablename, 0)).

%% ===================================================================
%% 迁移文件契约
%% ===================================================================

fts_group_migration_contains_table_test() ->
    Migration = read_file(fts_group_migration_path()),
    ?assert(binary:match(Migration, <<"fts_group">>) =/= nomatch).

%% ===================================================================
%% Internal
%% ===================================================================

read_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

fts_group_migration_path() ->
    case filelib:wildcard("priv/migrations/*00000068_fts_group*.up.sql") of
        [Path | _] ->
            Path;
        [] ->
            error({missing_migration, "00000068_fts_group"})
    end.

ensure_module_loaded(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    ok.
