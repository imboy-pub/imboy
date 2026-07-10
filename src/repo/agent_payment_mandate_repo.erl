-module(agent_payment_mandate_repo).

%%%
% Agent 受控支付授权仓库 / Agent payment mandate repository
% 表 agent_payment_mandate（见 priv/migrations/00000029）。所有 SQL 经 elib_pg 参数化。
%
% 核心是 reserve/2：在单条 SQL 内「固定窗口重置 + 累计上限校验 + 原子自增 spent_fen」
% 一次完成（read+判+write 无竞态），命中 0 行即代表超周期累计上限。金融安全的额度闸门。
%%%

-export([tablename/0]).
-export([create/1]).
-export([find/1]).
-export([find_active/1]).
-export([reserve/2]).
-export([release/2]).
-export([set_status/2]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"agent_payment_mandate">>).

%% @doc 创建授权。Data 键（原子 key）：
%%   owner_uid, agent_uid, max_amount_fen, max_total_fen,
%%   period_secs(可选,默认 86400), expires_in_secs(必填,从现在起 N 秒后到期)
%% expires_at 用 NOW() + interval 计算，避免 timestamp 参数编码坑（见项目记忆 epgsql cast）。
-spec create(map()) -> {ok, integer()} | {error, term()}.
create(
    #{
        owner_uid := OwnerUid,
        agent_uid := AgentUid,
        max_amount_fen := MaxAmount,
        max_total_fen := MaxTotal,
        expires_in_secs := ExpiresInSecs
    } = Data
) ->
    Tb = tablename(),
    PeriodSecs = maps:get(period_secs, Data, 86400),
    Id = elib_tsid:generate(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, owner_uid, agent_uid, max_amount_fen, max_total_fen, period_secs,"
            "  spent_fen, window_start, expires_at, status, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,0,NOW(),"
            "         NOW() + ($7 * interval '1 second'),1,NOW(),NOW())"
            " RETURNING id">>,
    case
        elib_pg:query(Sql, [Id, OwnerUid, AgentUid, MaxAmount, MaxTotal, PeriodSecs, ExpiresInSecs])
    of
        {ok, _} ->
            {ok, Id};
        {error, Reason} ->
            ?ERROR_LOG("agent_payment_mandate_repo:create error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 按 id 查单个授权（不校验有效性）
-spec find(integer()) -> {ok, map()} | {error, notfound | term()}.
find(Id) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT id, owner_uid, agent_uid, max_amount_fen, max_total_fen,"
            " period_secs, spent_fen, status FROM ",
            Tb/binary,
            " WHERE id = $1"
        >>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查 agent 当前有效授权（status=1 且未过期），闸门①的数据源。
%% 过期/撤销的行天然被 SQL 过滤 → 调用方拿到 notfound 即视为「无有效授权」。
-spec find_active(integer()) -> {ok, map()} | {error, notfound | term()}.
find_active(AgentUid) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT id, owner_uid, agent_uid, max_amount_fen, max_total_fen,"
            " period_secs, spent_fen, status FROM ",
            Tb/binary,
            " WHERE agent_uid = $1 AND status = 1 AND expires_at > NOW()"
            " ORDER BY id DESC LIMIT 1"
        >>,
    case elib_pg:query(Sql, [AgentUid]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 原子预留额度（闸门②周期累计上限）。单条 SQL 内：
%%   - 若窗口已过期（NOW-window_start >= period_secs）→ 重置 spent=本笔、window_start=NOW
%%   - 否则 → spent += 本笔
%%   - WHERE 守卫：结果 spent <= max_total_fen 才更新
%% 命中 1 行 → {ok, NewSpent}；命中 0 行 → 超上限（或 id 不存在/已撤销）→ exceeds_total_limit。
-spec reserve(integer(), integer()) ->
    {ok, integer()} | {error, exceeds_total_limit | term()}.
reserve(MandateId, AmountFen) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET spent_fen = CASE"
            "        WHEN EXTRACT(EPOCH FROM (NOW() - window_start)) >= period_secs"
            "        THEN $2 ELSE spent_fen + $2 END,"
            "     window_start = CASE"
            "        WHEN EXTRACT(EPOCH FROM (NOW() - window_start)) >= period_secs"
            "        THEN NOW() ELSE window_start END,"
            "     updated_at = NOW()"
            " WHERE id = $1 AND status = 1"
            "   AND (CASE"
            "        WHEN EXTRACT(EPOCH FROM (NOW() - window_start)) >= period_secs"
            "        THEN $2 ELSE spent_fen + $2 END) <= max_total_fen"
            " RETURNING spent_fen">>,
    case elib_pg:query(Sql, [MandateId, AmountFen]) of
        {ok, [Row | _]} -> {ok, maps:get(<<"spent_fen">>, Row)};
        {ok, []} -> {error, exceeds_total_limit};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 释放预留（扣款失败补偿）。spent_fen -= AmountFen（下限 0），保证周期额度不被空耗。
-spec release(integer(), integer()) -> ok | {error, term()}.
release(MandateId, AmountFen) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET spent_fen = GREATEST(spent_fen - $2, 0), updated_at = NOW()"
            " WHERE id = $1">>,
    case elib_pg:query(Sql, [MandateId, AmountFen]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 撤销/启用授权（status 1=有效 0=撤销）
-spec set_status(integer(), 0 | 1) -> {ok, non_neg_integer()} | {error, term()}.
set_status(Id, Status) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => Status, updated_at => elib_dt:now()}, <<"id = $1">>, [Id]).
