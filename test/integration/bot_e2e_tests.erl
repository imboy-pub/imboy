-module(bot_e2e_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% Bot 端到端契约测试
%%%
%%% 验证：
%%% - Bot 路由存在且正确
%%% - Bot 核心模块导出契约稳定
%%% - Bot 迁移文件包含关键表定义
%%%===================================================================

%% ===================================================================
%% 路由契约
%% ===================================================================

bot_routes_exist_test() ->
    Source = read_file("src/imboy_router.erl"),
    lists:foreach(
        fun(Path) ->
            ?assert(binary:match(Source, Path) =/= nomatch)
        end,
        [
            <<"/api/v1/bot/register">>,
            <<"/api/v1/bot/get">>,
            <<"/api/v1/bot/update">>,
            <<"/api/v1/bot/disable">>,
            <<"/api/v1/bot/enable">>,
            <<"/api/v1/bot/list_mine">>,
            <<"/api/v1/bot/search">>
        ]
    ).

%% ===================================================================
%% 模块导出契约
%% ===================================================================

bot_repo_exports_contract_test() ->
    ensure_module_loaded(bot_repo),
    ?assert(erlang:function_exported(bot_repo, tablename, 0)),
    ?assert(erlang:function_exported(bot_repo, create, 1)),
    ?assert(erlang:function_exported(bot_repo, find, 1)),
    ?assert(erlang:function_exported(bot_repo, find_by_username, 1)),
    ?assert(erlang:function_exported(bot_repo, find_by_token, 1)),
    ?assert(erlang:function_exported(bot_repo, update, 2)),
    ?assert(erlang:function_exported(bot_repo, set_status, 2)),
    ?assert(erlang:function_exported(bot_repo, page, 2)),
    ?assert(erlang:function_exported(bot_repo, page_by_owner, 3)),
    ?assert(erlang:function_exported(bot_repo, search, 3)).

bot_ds_exports_contract_test() ->
    ensure_module_loaded(bot_ds),
    ?assert(erlang:function_exported(bot_ds, create, 1)),
    ?assert(erlang:function_exported(bot_ds, is_bot, 1)),
    ?assert(erlang:function_exported(bot_ds, find_by_token, 1)).

bot_logic_exports_contract_test() ->
    ensure_module_loaded(bot_logic),
    ?assert(erlang:function_exported(bot_logic, register, 1)),
    ?assert(erlang:function_exported(bot_logic, get, 1)),
    ?assert(erlang:function_exported(bot_logic, update, 2)),
    ?assert(erlang:function_exported(bot_logic, set_status, 2)),
    ?assert(erlang:function_exported(bot_logic, list_mine, 2)),
    ?assert(erlang:function_exported(bot_logic, search, 3)),
    ?assert(erlang:function_exported(bot_logic, send_message, 3)).

bot_webhook_logic_exports_contract_test() ->
    ensure_module_loaded(bot_webhook_logic),
    ?assert(erlang:function_exported(bot_webhook_logic, push, 2)),
    ?assert(erlang:function_exported(bot_webhook_logic, push_message, 3)),
    ?assert(erlang:function_exported(bot_webhook_logic, sign_payload, 2)).

%% ===================================================================
%% Agent 发现契约
%% ===================================================================

agent_routes_exist_test() ->
    Source = read_file("src/imboy_router.erl"),
    lists:foreach(
        fun(Path) ->
            ?assert(binary:match(Source, Path) =/= nomatch)
        end,
        [
            <<"/api/v1/agent/list">>,
            <<"/api/v1/agent/discover">>,
            <<"/api/v1/agent/search">>,
            <<"/api/v1/agent/categories">>
        ]
    ).

agent_logic_exports_contract_test() ->
    ensure_module_loaded(ai_agent_logic),
    ?assert(erlang:function_exported(ai_agent_logic, list_assistants, 1)),
    ?assert(erlang:function_exported(ai_agent_logic, categories, 0)).

agent_handler_exports_contract_test() ->
    ensure_module_loaded(ai_agent_handler),
    ?assert(erlang:function_exported(ai_agent_handler, init, 2)).

%% ===================================================================
%% 迁移文件契约
%% ===================================================================

bot_migration_contains_table_test() ->
    Migration = read_file(bot_migration_path()),
    ?assert(binary:match(Migration, <<"CREATE TABLE IF NOT EXISTS public.bot">>) =/= nomatch),
    ?assert(
        binary:match(Migration, <<"CREATE TABLE IF NOT EXISTS public.bot_oauth_grant">>) =/= nomatch
    ),
    ?assert(binary:match(Migration, <<"3=bot(开发者服务)"/utf8>>) =/= nomatch).

bot_prefix_migration_contains_default_agents_test() ->
    Migration = read_file(bot_prefix_migration_path()),
    ?assert(binary:match(Migration, <<"agent_ark">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"agent_bailian">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"account_type,">>) =/= nomatch).

%% ===================================================================
%% Internal
%% ===================================================================

read_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

bot_migration_path() ->
    case filelib:wildcard("priv/migrations/*00000070_bot*.up.sql") of
        [Path | _] ->
            Path;
        [] ->
            error({missing_migration, "00000070_bot"})
    end.

bot_prefix_migration_path() ->
    case filelib:wildcard("priv/migrations/*00000071_bot_prefix*.up.sql") of
        [Path | _] ->
            Path;
        [] ->
            error({missing_migration, "00000071_bot_prefix_to_agent"})
    end.

ensure_module_loaded(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    ok.
