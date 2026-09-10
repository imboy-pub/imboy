-module(auth_session_ds).

%%%
%%% Task 10 / LT-04：持久化会话吊销（session epoch）共享语义。
%%%
%%% HTTP 鉴权（auth_ds）、WebSocket 握手（websocket_ds）与既有 WS 连接的
%%% 心跳重校验（websocket_handler）都消费本模块的同一谓词，禁止各自实现。
%%%
%%% 模型：每用户单调递增 epoch（public.user_auth_epoch，DB 权威 = 重启后生效）。
%%% token 签发时携带 ep claim；token.ep < 当前 epoch ⇒ 已吊销。
%%% bump 触发点：改密（自助/忘记密码/管理端重置）、管理端禁用、全端登出。
%%% 单端登出走既有设备吊销管道（user_device_ds:delete），不经本模块。
%%%
%%% fail-closed：epoch 现势无法确认（DB 不可用）时一律判已吊销；
%%% 仅"行缺失"是已知态（= 1），不是未知态。
%%%
%%% legacy 豁免：无 ep claim 的存量 token（含空 did legacy）沿 E2EE-013 的
%%% did 豁免先例自然过期（tk 7200s），不做强制失效，避免发布即全端登出。
%%%

-export([current_epoch/1]).
-export([revoked/2]).
-export([bump/1]).
-export([bump_in_tx/2]).
-export([kick_all_sessions/1]).

-include("log.hrl").

-define(EPOCH_MEMO_SEC, 60).
-define(UPSERT_SQL, <<
    "INSERT INTO public.user_auth_epoch (user_id, epoch, updated_at) "
    "VALUES ($1, 2, now()) "
    "ON CONFLICT (user_id) DO UPDATE "
    "SET epoch = public.user_auth_epoch.epoch + 1, updated_at = now() "
    "RETURNING epoch"
>>).

%% @doc 当前 epoch；缺行 = 1；DB 不可用 = {error, unavailable}。
%% 正向 memo 60s；bump 时主动失效并跨节点广播，与设备吊销同款时序。
%% 缓存层自身不可用（如 depcache 未启动）不算未知态：降级为缓存未命中直查 DB，
%% DB 也不可用才 fail-closed——auth 路径不能因缓存故障而崩溃。
-spec current_epoch(integer()) -> {ok, non_neg_integer()} | {error, unavailable}.
current_epoch(Uid) when is_integer(Uid), Uid > 0 ->
    Key = {auth_session_epoch, Uid},
    Cached = safe_cache_get(Key),
    case Cached of
        {ok, {ok, N}} when is_integer(N), N >= 1 ->
            {ok, N};
        _ ->
            case safe_query_epoch(Uid) of
                {ok, E} when is_integer(E), E >= 1 ->
                    safe_cache_memo(Key, E),
                    {ok, E};
                _ ->
                    ok = ?WARN_LOG({auth_session_epoch_unavailable, Uid, unavailable}),
                    {error, unavailable}
            end
    end;
current_epoch(_) ->
    {error, unavailable}.

%% @private 缓存读：缓存层崩溃视为未命中（auth 路径不因缓存故障崩溃）
-spec safe_cache_get(term()) -> term().
safe_cache_get(Key) ->
    try imboy_cache:get(Key) of
        Other -> Other
    catch
        _:_ -> undefined
    end.

%% @private 缓存写：失败静默（仅损失 60s 命中收益）
-spec safe_cache_memo(term(), non_neg_integer()) -> ok.
safe_cache_memo(Key, E) ->
    try imboy_cache:memo(fun() -> {ok, E} end, Key, ?EPOCH_MEMO_SEC) of
        _ -> ok
    catch
        _:_ -> ok
    end.

%% @private 查库归一化：缺行=1（已知默认态，bump 首次即写 2）；畸形行/异常=fail-closed
-spec safe_query_epoch(integer()) ->
    {ok, non_neg_integer()} | {error, unavailable}.
safe_query_epoch(Uid) ->
    try
        case
            elib_pg:query(
                <<"SELECT epoch FROM public.user_auth_epoch WHERE user_id = $1">>,
                [Uid]
            )
        of
            {ok, []} ->
                {ok, 1};
            {ok, [#{<<"epoch">> := E}]} when is_integer(E), E >= 1 ->
                {ok, E};
            {ok, _} ->
                {error, unavailable};
            {error, _} ->
                {error, unavailable}
        end
    catch
        _:_ ->
            {error, unavailable}
    end.

%% @doc 共享吊销谓词：true = 拒绝该 token。
%% TokenEpoch = undefined（legacy 无 ep claim）→ 豁免（did 先例）。
%% epoch 现势不可确认 → true（fail-closed）。
-spec revoked(integer(), term()) -> boolean().
revoked(Uid, TokenEpoch) when is_integer(Uid), is_integer(TokenEpoch) ->
    case current_epoch(Uid) of
        {ok, Current} -> TokenEpoch < Current;
        {error, _} -> true
    end;
revoked(_Uid, undefined) ->
    false;
revoked(_Uid, _Malformed) ->
    %% ep claim 畸形 = 无法确认签发时态，fail-closed
    true.

%% @doc 独立事务 bump（调用方无外层事务时用），成功后失效缓存并广播。
-spec bump(integer()) -> ok | {error, term()}.
bump(Uid) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            {ok, _, _, [{_E}]} = epgsql:equery(Conn, ?UPSERT_SQL, [Uid]),
            ok
        end)
    of
        ok ->
            evict(Uid),
            ok;
        {rollback, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
    end.

%% @doc 事务内 bump：密码更新等已有事务的调用方在同一事务内原子 bump，
%% 保证"改密成功 ⇒ 吊销必已写入"；失败抛 {abort_tx, ...} 回滚整个事务。
-spec bump_in_tx(pid(), integer()) -> ok.
bump_in_tx(Conn, Uid) ->
    case epgsql:equery(Conn, ?UPSERT_SQL, [Uid]) of
        {ok, _, _, [_]} ->
            evict(Uid),
            ok;
        {error, Reason} ->
            erlang:throw({abort_tx, {session_epoch_bump_failed, Reason}})
    end.

%% @doc 踢掉该用户全部在线会话（imboy_syn 全局注册表，跨节点生效）。
%% 与 auth_logic:logout/2 的单端踢法同构，区别仅在不过滤 did。
-spec kick_all_sessions(integer()) -> ok.
kick_all_sessions(Uid) ->
    Devices = imboy_syn:list_by_uid(Uid),
    lists:foreach(
        fun({Pid, _Meta}) ->
            imboy_syn:leave(Uid, Pid)
        end,
        Devices
    ),
    ok.

%% @private bump 后失效本节点缓存并跨节点广播
-spec evict(integer()) -> ok.
evict(Uid) ->
    Key = {auth_session_epoch, Uid},
    imboy_cache:delete(Key),
    _ = imboy_cache:broadcast({flush, Key}),
    ok.
