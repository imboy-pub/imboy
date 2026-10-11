%%%-------------------------------------------------------------------
%%% @doc QR 登录会话 PostgreSQL 权威 Repo（G-QR 收口 C08）
%%%
%%% 依据 g-qr-atomic-consumption-proposal.md 第 3 节：
%%% - 单语句 UPDATE ... WHERE ... RETURNING 的 PG 行级原子性承担
%%%   原子 claim/consume 与状态机 CAS（替代 depcache get→改→set 竞态窗口）；
%%% - claim_login_token/1：一次性领取（login_token 取出即置 NULL，
%%%   consumed_at 打戳；0 行返回 = 已消费/未确认/过期）；
%%% - transition/5：scan/confirm/cancel 状态机 CAS（乱序与并发下
%%%   状态转移唯一）；
%%% - 全部语句内联 expires_at > now() 过期判断。
%%%
%%% 不做跨节点内存态：多节点对同一行的并发竞争由 PG 行锁仲裁，
%%% "两个后端节点同时领取只有一个成功"即本模块的跨节点语义。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_session_repo).
-moduledoc "QR 登录会话 PG 权威仓库：原子 claim/consume 与状态机 CAS。".

-export([
    create/1,
    find_by_session_token/1,
    find_by_qr_token/1,
    transition_scan/2,
    transition_confirm/3,
    transition_cancel/1,
    claim_login_token/1,
    delete_expired/0
]).

%% @doc 创建会话（waiting）。冲突（session/qr token 重复）返回 {error, conflict}。
-spec create(map()) -> ok | {error, term()}.
create(#{
    session_token := SessionToken,
    qr_token := QRToken,
    device_id := DeviceId,
    device_name := DeviceName,
    platform := Platform,
    purpose := Purpose,
    expires_at_ms := ExpiresAtMs
}) ->
    Sql = <<
        "INSERT INTO public.qr_login_session "
        "(session_token, qr_token, status, device_id, device_name, platform, purpose, expires_at) "
        "VALUES ($1, $2, 'waiting', $3, $4, $5, $6, "
        "  to_timestamp($7 / 1000.0)) "
        "ON CONFLICT DO NOTHING"
    >>,
    case
        elib_pg:query(Sql, [
            SessionToken,
            QRToken,
            DeviceId,
            DeviceName,
            Platform,
            Purpose,
            ExpiresAtMs
        ])
    of
        {ok, []} ->
            ok;
        {ok, _} ->
            ok;
        {error, #{<<"constraint">> := _}} ->
            {error, conflict};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 按会话 token 查询（未过期行）。
-spec find_by_session_token(binary()) -> map() | undefined | {error, term()}.
find_by_session_token(SessionToken) ->
    Sql = <<
        "SELECT session_token, qr_token, status, scanned_by, device_id, "
        "device_name, platform, purpose, "
        "(login_token IS NOT NULL AND consumed_at IS NULL) AS token_claimable, "
        "expires_at, created_at "
        "FROM public.qr_login_session "
        "WHERE session_token = $1 AND expires_at > now()"
    >>,
    case elib_pg:query(Sql, [SessionToken]) of
        {ok, [Row]} -> Row;
        {ok, []} -> undefined;
        {error, Reason} -> {error, Reason}
    end.

%% @doc QR token 反查会话（替代 {qr_login_qr, _} 缓存映射）。
-spec find_by_qr_token(binary()) -> map() | undefined | {error, term()}.
find_by_qr_token(QRToken) ->
    Sql = <<
        "SELECT session_token, status, scanned_by, expires_at "
        "FROM public.qr_login_session "
        "WHERE qr_token = $1 AND expires_at > now()"
    >>,
    case elib_pg:query(Sql, [QRToken]) of
        {ok, [Row]} -> Row;
        {ok, []} -> undefined;
        {error, Reason} -> {error, Reason}
    end.

%% @doc scan CAS：waiting → scanned（绑定扫码人）。0 行 = 已扫/已取消/
%% 已确认/过期/不存在（调用方映射既有错误码）。
-spec transition_scan(binary(), integer()) -> {ok, map()} | {error, not_transitioned}.
transition_scan(SessionToken, Uid) ->
    Sql = <<
        "UPDATE public.qr_login_session "
        "SET status = 'scanned', scanned_by = $2, scanned_at = now() "
        "WHERE session_token = $1 AND status = 'waiting' AND expires_at > now() "
        "RETURNING session_token, status, scanned_by, device_id, device_name, "
        "platform, purpose, expires_at"
    >>,
    one_or_none(Sql, [SessionToken, Uid]).

%% @doc confirm CAS：scanned → confirmed + 原子写入 login_token（单语句，
%% 消除"confirmed 但 token 未写"中间态窗口）。仅 scanned_by = Uid 行可转移。
-spec transition_confirm(binary(), integer(), binary()) ->
    {ok, map()} | {error, not_transitioned}.
transition_confirm(SessionToken, Uid, LoginToken) ->
    Sql = <<
        "UPDATE public.qr_login_session "
        "SET status = 'confirmed', login_token = $3, confirmed_at = now() "
        "WHERE session_token = $1 AND status = 'scanned' AND scanned_by = $2 "
        "AND login_token IS NULL AND consumed_at IS NULL AND expires_at > now() "
        "RETURNING session_token, status, scanned_by, device_id, device_name, "
        "platform, purpose, expires_at"
    >>,
    one_or_none(Sql, [SessionToken, Uid, LoginToken]).

%% @doc cancel CAS：waiting|scanned → cancelled（幂等：已 cancelled 返回 ok）。
-spec transition_cancel(binary()) -> ok | {error, not_transitioned}.
transition_cancel(SessionToken) ->
    Sql = <<
        "UPDATE public.qr_login_session "
        "SET status = 'cancelled' "
        "WHERE session_token = $1 AND status IN ('waiting', 'scanned') "
        "AND expires_at > now() "
        "RETURNING session_token"
    >>,
    case one_or_none(Sql, [SessionToken]) of
        {ok, _} ->
            ok;
        {error, not_transitioned} ->
            %% 幂等：已 cancelled / 已消费视为成功取消（与 cache 路径
            %% handle_cancel 的"不存在也返回 cancelled"语义对齐）
            case find_by_session_token(SessionToken) of
                #{<<"status">> := <<"cancelled">>} -> ok;
                _ -> {error, not_transitioned}
            end
    end.

-spec one_or_none(binary(), [term()]) -> {ok, map()} | {error, not_transitioned}.
one_or_none(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_transitioned};
        {error, _} -> {error, not_transitioned}
    end.

%% @doc 原子领取 login_token（G-QR 核心 oracle）：
%% UPDATE 单语句行锁保证并发下恰好一个消费者拿到 token；
%% 0 行返回 = 已消费 / 未确认 / 已过期。
-spec claim_login_token(binary()) ->
    {ok, #{login_token := binary(), scanned_by := integer()}} | {error, not_claimable}.
claim_login_token(SessionToken) ->
    Sql = <<
        "UPDATE public.qr_login_session "
        "SET login_token = NULL, consumed_at = now() "
        "WHERE session_token = $1 AND status = 'confirmed' "
        "AND login_token IS NOT NULL AND consumed_at IS NULL AND expires_at > now() "
        "RETURNING OLD.login_token, scanned_by"
    >>,
    case elib_pg:query(Sql, [SessionToken]) of
        {ok, [#{<<"login_token">> := Token, <<"scanned_by">> := ScannedBy}]} ->
            {ok, #{login_token => Token, scanned_by => num(ScannedBy)}};
        {ok, []} ->
            {error, not_claimable};
        {error, _} ->
            {error, not_claimable}
    end.

-spec num(integer() | binary() | null) -> integer().
num(N) when is_integer(N) -> N;
num(B) when is_binary(B) -> binary_to_integer(B);
num(null) -> 0.

%% @doc 过期清扫（定时兜底；领取/状态机已内联过期判断）。
-spec delete_expired() -> {ok, non_neg_integer()}.
delete_expired() ->
    Sql = <<
        "DELETE FROM public.qr_login_session WHERE expires_at <= now()"
    >>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} when is_list(Rows) ->
            {ok, length(Rows)};
        _ ->
            {ok, 0}
    end.
