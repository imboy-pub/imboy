%%% @doc AI trace 受控清理通道（F 组 L3 的应用侧载体）。
%%%
%%% **必须**以独立非超级凭据连接（K1b：复用 imboy_user ⇒ F 组全部失效，
%%% 进入条件硬失败）。连接参数来自 imboy env `cs_ai_purge`：
%%%
%%% ```
%%% {cs_ai_purge, #{
%%%     host => "127.0.0.1", port => 4323,
%%%     username => "cs_ai_purge", password => "...",
%%%     database => "imboy", ssl => false, timeout => 4000
%%% }}
%%% ```
%%%
%%% 该角色只有 `cs_ai_purge_trace(bigint, timestamptz, text, text, bigint)`
%%% 的 EXECUTE 权限（L1：对 customer_service_ai_turn 无 DELETE/UPDATE）；
%%% 函数内校验 session_user 白名单（L3）+ purge GUC 二次确认（非权限边界，
%%% R13）。审计行由函数写入 customer_service_event（action='ai.trace_purged'）。
-module(cs_ai_purge_worker).

-moduledoc "AI trace 受控清理（SECURITY DEFINER 通道；独立非超级凭据）。".

-export([purge/4]).

%% @doc 受控清理：删除 OrgId 在 BeforeTs 之前的 customer_service_ai_turn，
%% 并落审计行。与迁移 135 后语义一致：审计事件 id 由应用侧 TSID 发号。
%% GUC（imboy.ai_trace_purge）由本函数在事务内 SET LOCAL——它是"两点
%% 确认"，**不是**权限边界（R13；真正的边界是 L1 权限 + L3 session_user）。
-spec purge(integer(), calendar:datetime() | integer(), binary(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
purge(OrgId, BeforeTs, Reason, RequestId) when
    is_integer(OrgId), OrgId > 0, is_binary(Reason), is_binary(RequestId)
->
    #{host := _Host} = Cfg = config_ds:env(cs_ai_purge),
    case epgsql:connect(connect_opts(Cfg)) of
        {ok, Conn} ->
            EventId = elib_tsid:generate(enterprise_audit_event),
            try
                purge_tx(Conn, OrgId, BeforeTs, Reason, RequestId, EventId)
            after
                epgsql:close(Conn)
            end;
        {error, ConnectReason} ->
            {error, {purge_connect_failed, ConnectReason}}
    end;
purge(_OrgId, _BeforeTs, _Reason, _RequestId) ->
    {error, {invalid_argument, purge}}.

purge_tx(Conn, OrgId, BeforeTs, Reason, RequestId, EventId) ->
    {ok, _, _} = epgsql:squery(Conn, <<"BEGIN">>),
    {ok, [], []} = epgsql:squery(Conn, <<"SET LOCAL imboy.ai_trace_purge = 'on'">>),
    Sql = <<"SELECT cs_ai_purge_trace($1, $2, $3, $4, $5)">>,
    case epgsql:equery(Conn, Sql, [OrgId, ts(BeforeTs), Reason, RequestId, EventId]) of
        {ok, _Cols, [{CountBin}]} when is_binary(CountBin); is_integer(CountBin) ->
            Count = to_int(CountBin),
            {ok, _, _} = epgsql:squery(Conn, <<"COMMIT">>),
            {ok, Count};
        {error, QueryReason} ->
            _ = epgsql:squery(Conn, <<"ROLLBACK">>),
            {error, QueryReason};
        Other ->
            _ = epgsql:squery(Conn, <<"ROLLBACK">>),
            {error, {unexpected_purge_result, Other}}
    end.

connect_opts(Cfg) ->
    Cfg.

ts(BeforeTs) when is_binary(BeforeTs) ->
    %% rfc3339 二进制（epgsql_codec_rfc3339_bin 编码面直通）
    BeforeTs;
ts(BeforeTs) when is_integer(BeforeTs) ->
    BeforeTs;
ts(DateTime) when is_tuple(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.

to_int(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin);
to_int(N) when is_integer(N) ->
    N.
