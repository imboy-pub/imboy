-module(enterprise_application_usage_repo).

-moduledoc "企业 Application 用量聚合计量仓储层。".
%%%
% enterprise_application_usage_repo 是企业 Application 用量**聚合计量**仓储
% （FULL-02 / plan-full §5：`enterprise_application_usage` 只存聚合计量，
% 不存消息正文/PII）。
%
%% 形态（migration 00000140）：(organization_id, application_id, metric,
% period_start) 主键 + counter 计数；metric 由 DB CHECK 钉死在固定枚举，**没有**
%% 任何自由文本/PII 列（列集封闭，真库套件以 information_schema 断言）。
%% 计量行禁止物理删除（触发器 23514），计数只增不减（counter >= 0 CHECK）。
%% 周期为按月 date_trunc（同一自然月内累计）。
%%%

-export([tablename/0, metrics/0, bump_tx/4, bump_tx/5]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_application_usage">>).

%% @doc 固定计量名枚举（与 migration 00000140 的 ck_eau_metric 逐字一致）。
%% 新增计量名必须先改 DB CHECK（本函数是应用侧镜像，不是第二处真源）。
-spec metrics() -> [binary(), ...].
metrics() ->
    [
        <<"identity.bound">>,
        <<"identity.revoked">>,
        <<"directory.page">>,
        <<"file.confirmed">>,
        <<"message.accepted">>,
        <<"message.failed">>,
        <<"seat.read">>
    ].

%% @doc 计量 +1（当月桶）。Metric 必须是固定枚举成员（非成员 → invalid_metric，
%% 绝不静默丢弃或落自由文本）。
-spec bump_tx(any(), integer(), integer(), binary()) ->
    ok | {error, invalid_metric | term()}.
bump_tx(Conn, OrgId, AppId, Metric) when is_integer(OrgId), is_integer(AppId) ->
    case lists:member(Metric, metrics()) of
        true ->
            Sql = <<
                "INSERT INTO ",
                (tablename())/binary,
                " (organization_id, application_id, metric, period_start, counter, updated_at)"
                " VALUES ($1, $2, $3, date_trunc('month', CURRENT_TIMESTAMP)::date, $4, NOW())"
                " ON CONFLICT (organization_id, application_id, metric, period_start)"
                " DO UPDATE SET counter = ",
                (tablename())/binary,
                ".counter + EXCLUDED.counter, updated_at = NOW()"
            >>,
            case elib_pg:execute(Conn, Sql, [OrgId, AppId, Metric, 1]) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        false ->
            {error, invalid_metric}
    end;
bump_tx(_Conn, _OrgId, _AppId, _Metric) ->
    {error, invalid_metric}.

%% @doc 计量 +N（批量路径；N 必须为正）。
-spec bump_tx(any(), integer(), integer(), binary(), pos_integer()) ->
    ok | {error, invalid_metric | term()}.
bump_tx(Conn, OrgId, AppId, Metric, N) when is_integer(N), N > 0 ->
    case lists:member(Metric, metrics()) of
        true ->
            Sql = <<
                "INSERT INTO ",
                (tablename())/binary,
                " (organization_id, application_id, metric, period_start, counter, updated_at)"
                " VALUES ($1, $2, $3, date_trunc('month', CURRENT_TIMESTAMP)::date, $4, NOW())"
                " ON CONFLICT (organization_id, application_id, metric, period_start)"
                " DO UPDATE SET counter = ",
                (tablename())/binary,
                ".counter + EXCLUDED.counter, updated_at = NOW()"
            >>,
            case elib_pg:execute(Conn, Sql, [OrgId, AppId, Metric, N]) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        false ->
            {error, invalid_metric}
    end;
bump_tx(_Conn, _OrgId, _AppId, _Metric, _N) ->
    {error, invalid_metric}.
