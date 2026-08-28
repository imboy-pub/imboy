-module(workspace_guard).
-compile([nowarn_deprecated_catch]).
%%%
% workspace_guard 工作区归档统一写守卫（双体验 v2.5.2 WP4/T7）
%
% 职责（计划 §七 T7；v2.1 评估决策 2：Archive 服务端强制为原则）：
%   1. ensure_writable/1（自动提交版）：logic 层前置检查——经 workspace_resolver
%      解析 target→workspace；scope=personal 恒放行（回归红线：个人资源永不受
%      守卫影响）；scope=workspace 读 workspace.status，archived → 稳定错误码
%      ?ERR_WORKSPACE_ARCHIVED(980)。资源不存在恒放行（走既有 404 流程）。
%      ⚠️ 本版本存在"检查-写窗口"（读与写不在同一事务），仅用于无法进同事务
%      的写路径（R3 #9-13/#17 的最小可行接入），残留风险见 WP4 报告。
%   2. ensure_writable_tx/2（事务版）：供 elib_pg:with_tx 的 Fun(Conn) 内调用——
%      在业务写同一事务里对 workspace 行执行 SELECT ... FOR UPDATE（行锁），
%      与归档/恢复事务（UPDATE workspace SET status=...）线性化：
%        - 业务写先拿锁：归档 UPDATE 阻塞至业务写 COMMIT → 完成后归档成功；
%        - 归档先拿锁：业务写的 FOR UPDATE 阻塞 → 归档 COMMIT 后读到
%          status=archived → 业务写被拒。
%      全部业务写事务必须"先锁 workspace 行、再写业务行"，与只锁 workspace
%      行的归档事务之间不存在锁序环（无死锁）。
%   3. abort_on_error/1：with_tx Fun 内的零样板助手——{error, Reason} 时
%      throw({abort_tx, Reason})，由 elib_pg:with_tx 归一为 {error, Reason}。
%   4. write_tx/2（P0 收口样板）：守卫+业务写单事务封装，供 DS/Repo 写入口
%      消除"检查-写窗口"（R3 #9-#13/#17 前置检查版逐条改用本助手）。
%   5. write_tx_or_skip/2：派生读写（已读/浏览计数，R3 #12/#18 语义）——
%      archived 时跳过写入返回 skipped，读取不受影响。
%
% target→workspace 解析（scope/workspace_id 创建后不可变，§1.4.2 规则 9）：
%   解析读使用自动提交连接是安全的——归档只改 status，不改归属；行锁与状态
%   检查始终落在业务写的同一事务连接上。
%
% 无现有镜像源（imboy_policy_catalog 是静态元数据、msg_archive_ds 是消息历史
% 读取，均不得作镜像）；注入点模式参考 logic 层前置校验写法。
%%%

-export([archived_error/0]).
-export([archived_error_code/0]).
-export([ensure_writable/1]).
-export([ensure_writable_tx/2]).
-export([abort_on_error/1]).
-export([is_archived_error/1]).
-export([write_tx/2]).
-export([write_tx_or_skip/2]).

-include("error_code.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 归档拒绝的稳定错误（handler 直接映射 envelope code）
-spec archived_error() -> {error, {integer(), binary()}}.
archived_error() ->
    {error, {?ERR_WORKSPACE_ARCHIVED, <<"工作区已归档，写操作被拒绝"/utf8>>}}.

%% @doc 稳定错误码导出（UI/客户端据此提示，守卫失败唯一错误码）
-spec archived_error_code() -> integer().
archived_error_code() ->
    ?ERR_WORKSPACE_ARCHIVED.

%% @doc 判定一个 logic 返回值是否为归档拒绝（供 handler/测试识别）
-spec is_archived_error(term()) -> boolean().
is_archived_error({error, {Code, _Msg}}) when Code =:= ?ERR_WORKSPACE_ARCHIVED ->
    true;
is_archived_error(_) ->
    false.

%% @doc 归档写守卫（自动提交版，logic 层前置检查）
%% Target = {ResourceType, ResourceId}，ResourceType 见 workspace_resolver。
%% personal / 资源不存在 → ok；workspace 且 archived → {error, {980, Msg}}。
-spec ensure_writable({atom(), integer() | binary()}) ->
    ok | {error, {integer(), binary()}}.
ensure_writable(Target) ->
    case workspace_resolver:resolve_workspace(Target) of
        {ok, WsId} ->
            case workspace_status(WsId) of
                <<"archived">> -> archived_error();
                _ -> ok
            end;
        _ ->
            %% personal 直通；not_found 放行走既有 404（不吞既有语义）
            ok
    end.

%% @doc 归档写守卫（事务版，with_tx Fun(Conn) 内调用）
%% 与业务写同事务：SELECT ... FOR UPDATE 锁 workspace 行并读 status。
%% personal / 资源不存在 → ok；archived → {error, {980, Msg}}（不抛异常，
%% 调用方用 abort_on_error/1 或 case 决定回滚）。
-spec ensure_writable_tx(any(), {atom(), integer() | binary()}) ->
    ok | {error, {integer(), binary()}}.
ensure_writable_tx(Conn, Target) ->
    case workspace_resolver:resolve_workspace(Target) of
        {ok, WsId} ->
            Sql = <<"SELECT status FROM workspace WHERE id = $1 FOR UPDATE">>,
            case elib_pg:query(Conn, Sql, [WsId]) of
                {ok, [#{<<"status">> := <<"archived">>} | _]} ->
                    archived_error();
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    %% 行锁查询失败按 fail-closed 拒绝（不静默放行）
                    _ = ?ERROR_LOG([workspace_guard_lock_failed, WsId, Reason]),
                    {error, {503, <<"工作区状态检查失败，请稍后重试"/utf8>>}}
            end;
        _ ->
            ok
    end.

%% @doc with_tx 内零样板：ok 原样返回；{error, Reason} → throw({abort_tx, Reason})
%% （elib_pg:with_tx 会 ROLLBACK 并归一返回 {error, Reason}）
-spec abort_on_error(ok | {error, term()}) -> ok | no_return().
abort_on_error(ok) ->
    ok;
abort_on_error({error, Reason}) ->
    throw({abort_tx, Reason}).

%% @doc 同事务写守卫样板（P0 收口）：with_tx 内先 ensure_writable_tx（FOR UPDATE
%% 锁 workspace 行）再执行 WriteFun(Conn)，检查与业务写原子提交。
%% archived → {error, {980, Msg}}（事务回滚）；personal/资源不存在 → 直写。
%% 供 DS/Repo 层写入口收口（镜像 project_ds:create 模板），消除前置检查的
%% "检查-写窗口"。WriteFun 返回值原样透传（错误由调用方或 with_tx 归一）。
-spec write_tx({atom(), integer() | binary()}, fun((any()) -> Ret)) ->
    Ret | {error, {integer(), binary()}}.
write_tx(Target, WriteFun) ->
    elib_pg:with_tx(fun(Conn) ->
        ok = abort_on_error(ensure_writable_tx(Conn, Target)),
        WriteFun(Conn)
    end).

%% @doc 派生读写样板（已读/浏览计数类）：archived → skipped（跳过写入、不报错，
%% 读取路径不受归档影响——回归红线"归档后可读"）；active/personal → 执行
%% WriteFun 并返回 {written, Ret}；行锁查询失败 fail-closed 抛 abort_tx。
%% 仅用于派生计数（R3 #12/#18 语义），内容写路径必须用 write_tx/2。
-spec write_tx_or_skip({atom(), integer() | binary()}, fun((any()) -> Ret)) ->
    {written, Ret} | skipped | {error, term()}.
write_tx_or_skip(Target, WriteFun) ->
    elib_pg:with_tx(fun(Conn) ->
        case ensure_writable_tx(Conn, Target) of
            ok ->
                {written, WriteFun(Conn)};
            {error, {?ERR_WORKSPACE_ARCHIVED, _Msg}} ->
                skipped;
            {error, Reason} ->
                throw({abort_tx, Reason})
        end
    end).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec workspace_status(integer()) -> binary() | undefined.
workspace_status(WsId) ->
    case elib_pg:one(<<"SELECT status FROM workspace WHERE id = $1">>, [WsId]) of
        {ok, #{<<"status">> := Status}} -> Status;
        _ -> undefined
    end.
