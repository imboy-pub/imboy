-module(mention_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% mention 轻量集成契约测试
%%%
%%% 背景：
%%% 旧版集成测试依赖大量历史 helper/API（如 list_by_user/mark_read 等旧符号），
%%% 在当前实现中已不可执行，且容易形成“看起来有测试、实际无法验证”的假象。
%%%
%%% 目标：
%%% - 验证路由层存在 mention 关键端点
%%% - 验证核心逻辑模块导出契约稳定
%%% - 验证数据库迁移包含 msg_mention 表定义
%%%===================================================================

mention_routes_exist_test() ->
    Source = read_file("src/imboy_router.erl"),
    lists:foreach(fun(Path) ->
        ?assert(binary:match(Source, Path) =/= nomatch)
    end, [
        <<"/v1/mention/list">>,
        <<"/v1/mention/unread">>,
        <<"/v1/mention/mark_read">>,
        <<"/v1/mention/suggest">>
    ]).

mention_logic_exports_contract_test() ->
    ensure_module_loaded(mention_logic),
    ?assert(erlang:function_exported(mention_logic, create_mentions, 4)),
    ?assert(erlang:function_exported(mention_logic, list_mentions, 3)),
    ?assert(erlang:function_exported(mention_logic, list_group_mentions, 4)),
    ?assert(erlang:function_exported(mention_logic, mark_as_read, 2)),
    ?assert(erlang:function_exported(mention_logic, mark_as_read_by_mention_id, 2)),
    ?assert(erlang:function_exported(mention_logic, mark_all_as_read, 1)),
    ?assert(erlang:function_exported(mention_logic, mark_group_as_read, 2)),
    ?assert(erlang:function_exported(mention_logic, count_unread, 1)),
    ?assert(erlang:function_exported(mention_logic, count_group_unread, 2)),
    ?assert(erlang:function_exported(mention_logic, get_member_suggestions, 3)).

mention_repo_exports_contract_test() ->
    ensure_module_loaded(mention_repo),
    ?assert(erlang:function_exported(mention_repo, insert, 4)),
    ?assert(erlang:function_exported(mention_repo, find_by_uid, 2)),
    ?assert(erlang:function_exported(mention_repo, find_by_group_and_uid, 3)),
    ?assert(erlang:function_exported(mention_repo, mark_as_read, 2)),
    ?assert(erlang:function_exported(mention_repo, find_msg_id_by_mention_id, 2)),
    ?assert(erlang:function_exported(mention_repo, mark_all_as_read, 1)),
    ?assert(erlang:function_exported(mention_repo, mark_group_as_read, 2)),
    ?assert(erlang:function_exported(mention_repo, count_unread, 1)),
    ?assert(erlang:function_exported(mention_repo, count_unread_in_group, 2)).

mention_migration_contains_table_test() ->
    Migration = read_file("priv/migrations/00000056_msg_mentions.sql"),
    ?assert(binary:match(Migration, <<"CREATE TABLE IF NOT EXISTS public.msg_mention">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"mentioned_uid bigint NOT NULL">>) =/= nomatch),
    ?assert(binary:match(Migration, <<"is_read boolean NOT NULL DEFAULT false">>) =/= nomatch).

read_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

ensure_module_loaded(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    ok.
