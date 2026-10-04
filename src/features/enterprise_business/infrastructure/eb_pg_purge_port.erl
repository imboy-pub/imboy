%%% @doc EB-03R T3：`eb_purge_port` 的实现（bounded purge 的**窄信封**）。
%%%
%%% 依据：用户裁决 §三 + EB-03R §2.3 T3——
%%% `(OrgId, WorkspaceId, NowMs, Limit) -> {ok, #{deleted := N}}`，**内部**才用
%%% `SKIP LOCKED` + limit；契约里不得出现 `exec/1`、`query/2`、`transaction/1`
%%% 这类把任意 SQL 交给应用层的形状（A04 的导出面白名单判定这一点）。
%%%
%%% ## 与 `eb_pg_purge` 的关系
%%%
%%% `eb_pg_purge`（EB-03 交付）已经实现了全部安全性：注入时钟筛选候选、
%%% `FOR UPDATE SKIP LOCKED` + 参数化 `LIMIT`、整批一个事务、DB 守卫拒绝即整批回滚、
%%% 只有真的删了行才写审计。本模块**不重写**它，只做两件事：
%%%   1. 把 `NowMs`（毫秒，Port 信封）换算成 `eb_pg_purge` 需要的 Unix 秒；
%%%   2. 把内部返回结构收敛为 Port 声明的 `{ok, #{deleted := N}}`。
%%%
%%% 因此 application 层拿到的能力**恰好**是「删一批到期行」，而不是「一个事务」。
-module(eb_pg_purge_port).

-moduledoc "eb_purge_port 实现（EB-03R T3）—— bounded purge 的窄信封。".
-behaviour(eb_purge_port).

-export([purge_batch/4]).

%% @doc 执行一批 bounded purge。
%%
%% * `NowMs` 必须是整数（注入时钟毫秒）；换算成秒后交给 `eb_pg_purge`，
%%   本模块**不读系统时间**。
%% * `Limit` 必须是 1..1000 的整数（越界即 `{error, {invalid_batch_limit, _}}`）。
%% * active hold 覆盖的行由**DB 层**保障不被删：`eb_pg_purge` 的候选查询用
%%   `FOR UPDATE SKIP LOCKED`，被 active hold 的行因 FK 的 KEY SHARE 行锁被**跳过**；
%%   即便进了候选，`trg_enterprise_message_purge_guard` / `fk_erh_active_message`
%%   也会在 DB 层拒绝（失败宁可多保留）。
-spec purge_batch(integer(), integer(), integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
purge_batch(OrgId, WorkspaceId, NowMs, Limit) when
    is_integer(NowMs), is_integer(Limit), Limit >= 1, Limit =< 1000
->
    case eb_pg_exec:tenant_error(OrgId, WorkspaceId) of
        ok ->
            eb_pg_purge:purge_batch(OrgId, WorkspaceId, #{
                now => NowMs div 1000,
                batch_limit => Limit
            });
        {error, _} = Err ->
            Err
    end;
purge_batch(_OrgId, _WorkspaceId, NowMs, Limit) when is_integer(NowMs) ->
    {error, {invalid_batch_limit, Limit}};
purge_batch(_OrgId, _WorkspaceId, NowMs, _Limit) ->
    {error, {invalid_clock, NowMs}}.

%% EB-06-A18：过渡信封 `/3`（`Opts :: #{now, batch_limit}`）与其键白名单校验
%% （`invalid_opt_keys/1`）已删除（E6-D3 / E5-D2 的递延项）。实现侧现在**只导出**
%% 上面的 `/4` 窄形状 ⇒ 调用方无法再把一个 map 直接转发给 purge worker。
