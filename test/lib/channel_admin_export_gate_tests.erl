-module(channel_admin_export_gate_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% export 回归门：channel_admin_repo 的 *_tx 事务函数曾被漏出 -export
%%% （483f5d0b 归档写守卫收口写了实现、DS 层也调用了，但 export 漏加），
%%% 全量 eunit 却全绿——meck mock 模块级替换会放行未导出函数的调用，
%%% 运行时（app 侧移除/改角色、admin 后台同名操作）才 undef 500。
%%% xref 抓出后补齐。本套件用 function_exported 断言钉死，防回归。
%%% @end
%%%-------------------------------------------------------------------

tx_exports_gate_test_() ->
    %% 显式加载，避免依赖其他测试先触发加载
    code:load_file(channel_admin_repo),
    [
        ?_assert(erlang:function_exported(channel_admin_repo, delete_tx, 3)),
        ?_assert(erlang:function_exported(channel_admin_repo, update_role_tx, 4)),
        %% 既有同步版入口一并钉住
        ?_assert(erlang:function_exported(channel_admin_repo, delete, 2)),
        ?_assert(erlang:function_exported(channel_admin_repo, update_role, 3))
    ].
