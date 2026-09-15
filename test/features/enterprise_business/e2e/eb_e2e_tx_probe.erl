%%% @doc EB-11 E2E 的**用例级事务端口探针**（test-only）：只为「提交失败 fail-closed」这一条
%%% 负例提供可注入的失败点，不做任何业务判定。
%%%
%%% 依据：plan §2.1 #18、EB-11-A06（「提交失败 fail-closed：不得返回 accepted、不得发
%%% realtime resource id、不得丢弃待重试输入；恢复后同一幂等键只产生一条消息和一条接受审计」）。
%%%
%%% 与 F1/F2/F6 的关系：本探针**不**补任何装配缺口；它只是把一个**端口实现**注入到
%%% `append_message` 的 `canonical_tx` 参数上（application 层显式支持的端口覆盖），
%%% 用于在真实 PG 上制造一次提交失败。`ok` 模式逐字委派给生产的 `eb_pg_tx`。
-module(eb_e2e_tx_probe).

-export([reset/0, set_mode/1, mode/0, calls/1, accept_message/3, append_conversation_audit/3]).

-define(TAB, eb_e2e_tx_probe_tab).

-spec reset() -> ok.
reset() ->
    ensure(),
    ets:delete_all_objects(?TAB),
    ets:insert(?TAB, [{mode, ok}, {accept_calls, 0}, {audit_calls, 0}]),
    ok.

-spec set_mode(ok | fail) -> ok.
set_mode(Mode) when Mode =:= ok; Mode =:= fail ->
    ensure(),
    ets:insert(?TAB, {mode, Mode}),
    ok.

-spec mode() -> ok | fail.
mode() ->
    ensure(),
    case ets:lookup(?TAB, mode) of
        [{mode, Mode}] -> Mode;
        [] -> ok
    end.

-spec calls(accept | audit) -> non_neg_integer().
calls(accept) ->
    ensure(),
    case ets:lookup(?TAB, accept_calls) of
        [{accept_calls, N}] -> N;
        [] -> 0
    end;
calls(audit) ->
    ensure(),
    case ets:lookup(?TAB, audit_calls) of
        [{audit_calls, N}] -> N;
        [] -> 0
    end.

%% @doc `eb_tx_port:accept_message/3` 的形状：`fail` 模式显式失败（不触库）。
-spec accept_message(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
accept_message(OrgId, WorkspaceId, Params) ->
    ensure(),
    ets:update_counter(?TAB, accept_calls, 1),
    case mode() of
        fail -> {error, canonical_tx_down};
        ok -> eb_pg_tx:accept_message(OrgId, WorkspaceId, Params)
    end.

-spec append_conversation_audit(integer(), integer(), map()) -> {ok, integer()} | {error, term()}.
append_conversation_audit(OrgId, WorkspaceId, Params) ->
    ensure(),
    ets:update_counter(?TAB, audit_calls, 1),
    case mode() of
        fail -> {error, audit_tx_down};
        ok -> eb_pg_tx:append_conversation_audit(OrgId, WorkspaceId, Params)
    end.

ensure() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, public, set, {write_concurrency, true}]);
        _ -> ?TAB
    end,
    ok.
