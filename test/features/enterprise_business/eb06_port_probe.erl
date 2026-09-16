%%% @doc EB-06 重开的**记账用假端口**（test-only，零 I/O、零 meck）。
%%%
%%% 用途：`eb_conversation_app:handover_identity/2` 的「交接与其审计互相蕴含」判定需要
%%% 可观测的**调用事实**——(a) 审计是否经**用例级事务端口**
%%%（`eb_tx_port:append_conversation_audit/3`）写入；(b) 它失败时交接是否被撤销。
%%%
%%% 本模块同时实现两个端口的形状：
%%%   * `eb_audit_port:append/2` —— 普通审计端口的独立写（交接**不得**走这条）；
%%%   * `eb_tx_port:append_conversation_audit/3` —— 用例级事务端口（交接**必须**走这条）。
%%% 两者都只记账：命中次数写进 ETS，返回一个合成 id；`tx_mode = fail` 时事务端口
%%% 显式返回 `{error, audit_down}`，用于「审计失败 ⇒ 交接不得生效」的负例。
%%%
%%% 边界：本模块**只**记录与返回，不触数据库、不读系统时间、不做任何业务判定。
-module(eb06_port_probe).

-export([
    reset/1,
    set_tx_mode/1,
    count/1,
    append/2,
    append_conversation_audit/3,
    %% store 端口探针（只读入口的「零写形状」行为判定）
    list_messages_after/3,
    fetch_message/3,
    ack_delivery/3,
    append_message/3
]).

-define(TABLE, eb06_port_probe).

%% @doc 复位计数器与 tx 模式。`Opts` 可含 `tx_mode => ok | fail`（缺省 `ok`）。
-spec reset(map()) -> ok.
reset(Opts) ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end,
    ets:insert(?TABLE, [
        {plain_audit_append, 0},
        {tx_conversation_audit, 0},
        {tx_mode, maps:get(tx_mode, Opts, ok)}
    ]),
    ok.

%% @doc 切换用例级事务端口的行为：`ok`（成功）| `fail`（`{error, audit_down}`）。
-spec set_tx_mode(ok | fail) -> ok.
set_tx_mode(Mode) when Mode =:= ok; Mode =:= fail ->
    ensure_table(),
    ets:insert(?TABLE, {tx_mode, Mode}),
    ok.

%% @doc 读计数器（表不存在时返回 0，便于断言「一次都没发生」）。
-spec count(atom()) -> non_neg_integer().
count(Key) ->
    case ets:info(?TABLE) of
        undefined ->
            0;
        _ ->
            case ets:lookup(?TABLE, Key) of
                [{Key, N}] when is_integer(N) -> N;
                _ -> 0
            end
    end.

%% @doc 普通审计端口的独立写：**只记账**（用于证明交接没有绕到这里）。
-spec append(integer(), map()) -> {ok, integer()}.
append(_OrgId, _Event) ->
    ensure_table(),
    ets:update_counter(?TABLE, plain_audit_append, 1),
    {ok, synthetic_audit_id()}.

%% @doc 用例级事务端口：`eb_tx_port:append_conversation_audit/3` 的形状。
-spec append_conversation_audit(integer(), integer(), map()) -> {ok, integer()} | {error, term()}.
append_conversation_audit(_OrgId, _WorkspaceId, _Params) ->
    ensure_table(),
    ets:update_counter(?TABLE, tx_conversation_audit, 1),
    case tx_mode() of
        fail -> {error, audit_down};
        ok -> {ok, synthetic_audit_id()}
    end.

ensure_table() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end.

tx_mode() ->
    case ets:lookup(?TABLE, tx_mode) of
        [{tx_mode, Mode}] -> Mode;
        [] -> ok
    end.

%% 合成审计 id（进程内唯一，不触数据库）。
synthetic_audit_id() ->
    erlang:unique_integer([positive, monotonic]) + 9000000000000000.

%% ===================================================================
%% store 端口探针：只读入口不得触发任何写形状
%% ===================================================================

%% @doc 读 callback：记账并返回合成结果（不触库）。
-spec list_messages_after(integer(), integer(), map()) -> {ok, [map()]}.
list_messages_after(_OrgId, _WorkspaceId, _Query) ->
    bump(store_read_list_messages_after),
    {ok, []}.

%% @doc 读 callback：记账并返回合成行（不触库）。
-spec fetch_message(integer(), integer(), integer()) -> {ok, map()}.
fetch_message(_OrgId, _WorkspaceId, MessageId) ->
    bump(store_read_fetch_message),
    {ok, #{id => MessageId}}.

%% @doc 写 callback：**一旦被调用即 raise** —— 只读入口碰到写形状必须立刻暴露。
-spec ack_delivery(integer(), integer(), map()) -> no_return().
ack_delivery(_OrgId, _WorkspaceId, _Ack) ->
    bump(store_write_attempted),
    erlang:error({write_shape_called_through_store, ack_delivery}).

%% @doc 写 callback：同 ack_delivery/3，一旦被调用即 raise。
-spec append_message(integer(), integer(), map()) -> no_return().
append_message(_OrgId, _WorkspaceId, _Message) ->
    bump(store_write_attempted),
    erlang:error({write_shape_called_through_store, append_message}).

bump(Key) ->
    ensure_table(),
    ets:update_counter(?TABLE, Key, 1, {Key, 0}).
