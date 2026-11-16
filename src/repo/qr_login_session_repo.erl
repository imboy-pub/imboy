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
%%%
%%% LEE-5：claim 语句依赖 PostgreSQL 18 的 RETURNING OLD 能力（单语句取回
%%% pre-update token）；ensure_claim_compatible/0 以 server_version_num 做
%%% 节点级门禁，create/claim 入口强制过闸，不满足时调用方 fail closed
%%% 拒绝 QR 登录，绝不降级。版本查询失败不缓存，仅成功取得才记忆。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_session_repo).
-moduledoc "QR 登录会话 PG 权威仓库：原子 claim/consume 与状态机 CAS。".

-include("log.hrl").

%% claim 依赖 UPDATE ... RETURNING OLD.*（PostgreSQL 18 起）
-define(PG_MIN_VERSION_NUM, 180000).
-define(QR_PG_VERSION_KEY, {imboy, qr_login_pg_version_num}).
%% 过期清扫单批上限（对齐 msg_c2c_repo 批量删除惯例）
-define(DEFAULT_CLEANUP_BATCH, 500).

-export([
    create/1,
    find_by_session_token/1,
    find_by_qr_token/1,
    transition_scan/2,
    transition_confirm/3,
    transition_cancel/1,
    claim_login_token/1,
    ensure_claim_compatible/0,
    delete_expired/0,
    delete_expired/1
]).

%% @doc 创建会话（waiting）。冲突（session/qr token 重复）返回 {error, conflict}。
%% LEE-5 门禁：入口先过 ensure_claim_compatible（repo 层兜底，任何调用方
%% 无法绕过）；不满足时 fail closed，绝不在不兼容库上创建可确认会话。
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
    case ensure_claim_compatible() of
        ok ->
            do_create(SessionToken, QRToken, DeviceId, DeviceName, Platform, Purpose, ExpiresAtMs);
        {error, Reason} ->
            {error, Reason}
    end.

-spec do_create(binary(), binary(), binary(), binary(), binary(), binary(), integer()) ->
    ok | {error, term()}.
do_create(SessionToken, QRToken, DeviceId, DeviceName, Platform, Purpose, ExpiresAtMs) ->
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
-spec transition_scan(binary(), integer()) ->
    {ok, map()} | {error, not_transitioned} | {error, {pg_error, term()}}.
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
    {ok, map()} | {error, not_transitioned} | {error, {pg_error, term()}}.
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
-spec transition_cancel(binary()) ->
    ok | {error, not_transitioned} | {error, {pg_error, term()}}.
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
        {error, {pg_error, _}} = Error ->
            Error;
        {error, not_transitioned} ->
            %% 仅 cancelled 幂等成功；数据库故障不等于已取消或不存在。
            %% Only a confirmed cancelled state is an idempotent success.
            case find_by_session_token(SessionToken) of
                #{<<"status">> := <<"cancelled">>} -> ok;
                {error, Reason} -> {error, {pg_error, Reason}};
                _ -> {error, not_transitioned}
            end
    end.

-spec one_or_none(binary(), [term()]) ->
    {ok, map()} | {error, not_transitioned} | {error, {pg_error, term()}}.
one_or_none(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_transitioned};
        {error, Reason} ->
            %% SQL 故障与零行 CAS 分开；提交结果不确定时不重试、不补偿发 token。
            %% A storage error is not a rejected CAS; never retry an uncertain write here.
            ?WARN_LOG(
                "[lee-5] qr_login transition PG error (fail closed): ~p", [Reason]
            ),
            {error, {pg_error, Reason}}
    end.

%% @doc PG18 能力门禁：claim 依赖 UPDATE ... RETURNING OLD.*（PG 18 起）。
%% 首次调用查询 server_version_num 并以 persistent_term 记忆（节点级一次，
%% 后续过闸为纯内存比较，同一请求链零重复查询）；查询失败返回错误且
%% **不缓存**——仅成功取得版本信息才允许写入 persistent_term。
%% 注意：被拒绝的版本同样被记忆（fail-closed 方向）——库原地大版本升级
%% （如 17→18）后须滚动重启节点重开门禁，缓存不随库升级自动刷新。
%% 低于 PG_MIN_VERSION_NUM（如 PG17）→ {error, {pg_version_unsupported, N}}，
%% 调用方必须明确拒绝 QR 登录，绝不降级。
-spec ensure_claim_compatible() -> ok | {error, term()}.
ensure_claim_compatible() ->
    case persistent_term:get(?QR_PG_VERSION_KEY, undefined) of
        Num when is_integer(Num) ->
            version_verdict(Num);
        undefined ->
            Sql = <<"SELECT current_setting('server_version_num')::int AS version_num">>,
            case elib_pg:query(Sql, []) of
                {ok, [#{<<"version_num">> := Num}]} when is_integer(Num) ->
                    persistent_term:put(?QR_PG_VERSION_KEY, Num),
                    version_verdict(Num);
                {ok, _Other} ->
                    {error, {pg_version_unknown, _Other}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec version_verdict(integer()) -> ok | {error, {pg_version_unsupported, integer()}}.
version_verdict(Num) when Num >= ?PG_MIN_VERSION_NUM ->
    ok;
version_verdict(Num) ->
    {error, {pg_version_unsupported, Num}}.

%% @doc 原子领取 login_token（G-QR 核心 oracle）：
%% UPDATE 单语句行锁保证并发下恰好一个消费者拿到 token；
%% 0 行返回 = 已消费 / 未确认 / 已过期。
%% LEE-5 门禁：入口过 ensure_claim_compatible（repo 层兜底——Polling/SSE/
%% 任何未来入口都无法绕过）；门禁失败返回可区分的 {error,{claim_gate,_}}
%% 供调用方映射为明确拒绝（503 / service_unavailable 注释），绝无 token。
%% PG 执行错误返回 pg_error，不冒充已消费；不重试、不恢复已消费 token。
%% Storage errors are distinct from zero rows, including an uncertain commit outcome.
-spec claim_login_token(SessionToken :: binary()) ->
    {ok, #{login_token := binary(), scanned_by := integer()}}
    | {error, not_claimable}
    | {error, {claim_gate, term()}}
    | {error, {pg_error, term()}}.
claim_login_token(SessionToken) ->
    case ensure_claim_compatible() of
        ok ->
            do_claim_login_token(SessionToken);
        {error, Reason} ->
            {error, {claim_gate, Reason}}
    end.

-spec do_claim_login_token(binary()) ->
    {ok, #{login_token := binary(), scanned_by := integer()}}
    | {error, not_claimable}
    | {error, {pg_error, term()}}.
do_claim_login_token(SessionToken) ->
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
        {error, Reason} ->
            ?WARN_LOG(
                "[lee-5] qr_login claim PG error (fail closed): ~p", [Reason]
            ),
            {error, {pg_error, Reason}}
    end.

-spec num(integer() | binary() | null) -> integer().
num(N) when is_integer(N) -> N;
num(B) when is_binary(B) -> binary_to_integer(B);
num(null) -> 0.

%% @doc 过期清扫（qr_login_cleanup_worker 定时调用；领取/状态机已内联过期
%% 判断——过期安全从不依赖清扫）。LEE-5：cache 删除后 PG 为唯一存储，
%% 本清扫防表膨胀；单批限量（LIMIT）；PG 错误透传给调用方（worker 记 WARN
%% 不影响登录——fail open 的只是清扫，凭据安全不受影响）。
-spec delete_expired() -> {ok, non_neg_integer()} | {error, term()}.
delete_expired() ->
    delete_expired(?DEFAULT_CLEANUP_BATCH).

%% @doc 批量限量版：每批至多 Batch 行，仅删 expires_at <= now()（PG 侧时钟
%% 裁决，与 claim 的 expires_at > now() 严格分区——活跃行/未过期行永不触碰；
%% 并发清扫由行锁串行化）。计数经 elib_pg:execute 取影响行数（query 会把
%% 无 RETURNING 的 DELETE 收敛为空结果集，取不到计数）。PG 错误原样返回，
%% **不得**吞成 {ok, 0}——否则 worker 的失败 WARN 分支不可达，PG 故障期间
%% 清扫静默无痕。
-spec delete_expired(Batch :: pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_expired(Batch) when is_integer(Batch), Batch > 0 ->
    Sql = <<
        "DELETE FROM public.qr_login_session WHERE session_token IN ("
        "SELECT session_token FROM public.qr_login_session "
        "WHERE expires_at <= now() ORDER BY expires_at ASC LIMIT $1)"
    >>,
    case elib_pg:execute(Sql, [Batch]) of
        {ok, Count} when is_integer(Count) ->
            {ok, Count};
        {ok, _Other} ->
            %% 无影响行数的意外 ok 形态（如带 RETURNING 的形状）——按 0 行处理
            {ok, 0};
        {error, Reason} ->
            {error, Reason}
    end.
