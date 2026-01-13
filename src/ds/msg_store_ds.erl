-module(msg_store_ds).
-dialyzer({nowarn_function, [staging_pending/1]}).
%%%-------------------------------------------------------------------
%%% @doc  消息写入队列管理服务（gen_server）
%%%
%%% == 模块职责 ==
%%% 此模块是消息写入系统的"队列管理器"，负责：
%%% 1. 接收消息写入请求并备份到 staging 表（同步操作）
%%% 2. 触发 msg_store_worker_ds 进行批量处理（异步操作）
%%% 3. 管理备份表的生命周期（标记已处理、清理过期数据）
%%%
%%% == 工作流程 ==
%%% 1. **备份阶段（同步）**：
%%%    ```erlang
%%%    msg_store_ds:stage(<<"c2c">>, MsgId, <<"text">>, <<>>, <<>>, Payload, FromId, ToId, CreatedAt, ServerTs)
%%%    → 写入 msg_store_staging 表
%%%    → 确保消息零丢失
%%%    ```
%%%
%%% 2. **入队阶段（异步）**：
%%%    ```erlang
%%%    msg_store_ds:enqueue(<<"c2c">>, MsgId, Data)
%%%    → 发送 kick 消息给 msg_store_worker_ds
%%%    → 立即返回，不阻塞
%%%    ```
%%%
%%% 3. **处理阶段（Worker）**：
%%%    ```erlang
%%%    msg_store_worker_ds 收到 kick
%%%    → 从 staging 表抢占 100 条未处理记录（FOR UPDATE SKIP LOCKED）
%%%    → 批量写入正式表（msg_c2c、msg_c2g 等）
%%%    → 成功后调用 msg_store_ds:unstage(MsgId)
%%%    ```
%%%
%%% 4. **清理阶段（定时）**：
%%%    ```erlang
%%%    每小时执行一次 cleanup_staging
%%%    → 删除 processed_at < 1 小时的记录
%%%    → 释放磁盘空间
%%%    ```
%%%
%%% == 分布式支持 ==
%%% - 使用 PostgreSQL 的 FOR UPDATE SKIP LOCKED 实现多节点安全抢占
%%% - 每个节点独立运行 Worker，避免重复处理同一条记录
%%% - processed_at 标记确保幂等性
%%%
%%% == 调用示例 ==
%%% ```erlang
%%% % 1. 发送消息时（msg_c2c_logic）
%%% case msg_store_ds:stage(<<"c2c">>, MsgId, <<"text">>, <<>>, <<>>, PayloadJson, FromId, ToId, CreatedAt, ServerTs) of
%%%     ok ->
%%%         % 备份成功，触发异步处理
%%%         msg_store_ds:enqueue(<<"c2c">>, MsgId, Data),
%%%         % 立即响应客户端
%%%         self() ! {reply, #{<<"type">> => <<"C2C_SERVER_ACK">>}};
%%%     error ->
%%%         % 备份失败，返回错误
%%%         {reply, error_msg}
%%% end,
%%%
%%% % 2. Worker 处理完成后自动调用
%%% msg_store_ds:unstage(MsgId)  % 标记备份表记录为已处理
%%% ```
%%% @end
%%%-------------------------------------------------------------------

%% ==================== API ====================

-export([start_link/0]).

%% 备份与入队
-export([stage/10, enqueue/3, unstage/1]).

%% 状态查询
-export([len/0, status/0]).

%% ==================== Callbacks ====================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-include("log.hrl").

%% ==================== Macros & Records ====================

-define(SERVER, ?MODULE).
-define(CLEANUP_INTERVAL, 3600000).     % 清理间隔：1 小时

-record(state, {
    last_flush_time
}).

%% ==================== API Functions ====================

%% @doc 启动队列管理服务
%% @see init/1
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-------------------------------------------------------------------
%% @doc  备份消息到 staging 表（同步操作）
%%
%% 此函数是消息写入流程的第一步，确保消息零丢失。
%% 直接写入数据库 staging 表，作为消息的唯一事实源（Single Source of Truth）。
%%
%% <b>参数说明：</b>
%% - Type: 消息类别（<<"c2c">>、<<"c2g">>、<<"s2c">>、<<"c2s">>）
%% - MsgId: 消息唯一ID（全局唯一，由客户端或服务端生成）
%% - MsgType: 消息子类型（<<"text">>、<<"image">>、<<"video">> 等）
%% - Action: S2C 操作类型（<<"pull_offline_msg">> 等，仅 s2c 使用）
%% - E2EE: 端到端加密元数据（JSONB binary 或 <<>>）
%% - Payload: 消息内容（JSON binary，不含 msg_type/action/e2ee）
%% - FromId: 发送者用户ID（integer）
%% - ToId: 接收者用户ID（integer，单聊场景）
%% - ToIdList: 接收者用户ID列表（[integer]，群聊场景）
%% - CreatedAt: 消息创建时间（RFC3339 binary）
%% - ServerTs: 服务器时间戳（RFC3339 binary）
%%
%% <b>返回值：</b>
%% - ok: 备份成功
%% - error: 备份失败（原因会记录错误日志）
%%
%% <b>失败处理：</b>
%% - 备份失败时，应向客户端返回错误响应
%% - 客户端需要重新发送消息
%%
%% @see enqueue/3 入队触发异步处理
%% @see unstage/1 标记消息已处理
%% @end
%%-------------------------------------------------------------------
-spec stage(binary(), binary(), binary(), binary(), binary(), binary(), integer(), integer() | [integer()], binary(), binary()) -> ok | error.
stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs) ->
    case msg_store_repo:stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs) of
        {ok, _} ->
            ok = ?DEBUG_LOG([msg_store_ds, stage, Type, MsgId, ok]),
            ok;
        {error, {unique_violation, _MsgId}} ->
            %% 【幂等性修复】消息已存在（客户端重发），返回 ok
            ok = ?INFO_LOG([msg_store_ds, stage_duplicate, Type, MsgId]),
            ok;
        {error, Reason} ->
            ok = ?ERROR_LOG([msg_store_ds, stage_error, Type, MsgId, Reason]),
            error
    end.

%%-------------------------------------------------------------------
%% @doc  入队并触发 Worker 处理（异步操作）
%%
%% 此函数是消息写入流程的第二步，触发批量处理。
%% 发送 kick 消息给 msg_store_worker_ds，立即返回，不阻塞调用者。
%%
%% - 这是异步操作，立即返回 ok
%%
%% <b>参数说明：</b>
%% - Type: 消息类型（binary，如 <<"c2c">>、<<"c2g">>、<<"s2c">>、<<"c2s">>）
%% - MsgId: 消息唯一ID
%% - Data: 消息数据（map，包含 payload、from_id、to_id 等）
%%
%% <b>返回值：</b>
%% - ok: 总是返回 ok（异步操作，不保证处理成功）
%%
%% <b>错误处理：</b>
%% - 如果 Worker 处理失败，会在 staging 表中标记 error_msg
%% - Worker 会根据 retry_count 进行指数退避重试
%% - 重试间隔：1s → 2s → 4s → 8s → 16s → 32s → 60s（最大）
%%
%% @see stage/10 备份消息到 staging 表
%% @see msg_store_worker 批量处理器
%% @end
%%-------------------------------------------------------------------
-spec enqueue(binary(), binary(), map()) -> ok.
enqueue(Type, MsgId, Data) ->
    gen_server:cast(?SERVER, {enqueue, Type, MsgId, Data}).

%%-------------------------------------------------------------------
%% @doc  标记消息已处理，删除备份表记录（异步操作）
%%
%% 此函数由 msg_store_worker_ds 在成功写入正式表后调用。
%% 标记 staging 表中的记录为已处理，但不立即删除，保留一段时间用于故障恢复。
%%
%% <b>调用时机：</b>
%% - msg_store_worker_ds 成功将消息写入正式表（msg_c2c、msg_c2g 等）后
%% - 通常不需要手动调用，由 Worker 自动调用
%%
%% <b>异步特性：</b>
%% - 这是异步操作，使用 elib_async:async_retry 执行
%% - 带重试机制（默认 3 次，1 秒延迟）
%% - 不阻塞 Worker 处理流程
%%
%% <b>工作原理：</b>
%% 1. 尝试标记 staging 表记录的 processed_at 字段为当前时间
%% 2. 不会立即删除记录，而是标记为"已处理"
%% 3. 定时清理任务（cleanup_staging）会在 1 小时后删除这些记录
%%
%% <b>幂等性：</b>
%%% - 可以安全地重复调用（对同一条消息）
%%% - 如果记录已标记为 processed_at，重复调用不会产生副作用
%%%
%% <b>参数说明：</b>
%% - MsgId: 消息唯一ID
%%
%% <b>返回值：</b>
%% - ok: 立即返回（异步操作）
%%
%% <b>故障恢复：</b>
%% - 如果服务器重启，staging 表中 processed_at = NULL 的记录会被自动重新处理
%% - 已标记为 processed_at 的记录不会被重复处理
%%
%% @see stage/7 备份消息到 staging 表
%% @see msg_store_worker_ds:process_row/1 Worker 处理流程
%% @end
%%-------------------------------------------------------------------
-spec unstage(binary()) -> ok.
unstage(MsgId) ->
    gen_server:cast(?SERVER, {unstage, MsgId}).

%%-------------------------------------------------------------------
%% @doc  获取待处理队列长度
%%
%% 返回 staging 表中待处理的消息数量（processed_at IS NULL）。
%%
%% <b>用途：</b>
%% - 监控队列积压情况
%% - 告警阈值：> 1000 条可能表示数据库写入慢或 Worker 处理慢
%%
%% <b>返回值：</b>
%% - 非负整数：待处理消息数量
%%
%% @see status/0 获取详细队列状态
%% @end
%%-------------------------------------------------------------------
-spec len() -> non_neg_integer().
len() ->
    StagingStats = msg_store_repo:get_staging_stats(),
    staging_pending(StagingStats).

%%-------------------------------------------------------------------
%% @doc  获取队列详细状态
%%
%% 返回 staging 表的统计信息，包括待处理、已处理、总数。
%%
%% <b>返回值：</b>
%% ```erlang
%%% #{
%%%     queue_len => 100,              % 待处理数量
%%%     staging_stats => #{
%%%         pending => 100,             % 待处理（processed_at IS NULL）
%%%         processed => 5000,          % 已处理（processed_at IS NOT NULL）
%%%         total => 5100               % 总数
%%%     }
%%% }
%%% ```
%%
%% <b>用途：</b>
%% - 监控系统健康状态
%% - 诊断性能问题
%% - 评估清理任务的执行效果
%%
%% @see len/0 获取待处理队列长度
%% @end
%%-------------------------------------------------------------------
-spec status() -> map().
status() ->
    gen_server:call(?SERVER, status).

%% ==================== Callbacks ====================

%% @private
init([]) ->
    _ = msg_store_repo:ensure_table_exists(),
    ok = ?INFO_LOG("msg_store_ds started successfully"),
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_staging),
    {ok, #state{last_flush_time = erlang:monotonic_time(millisecond)}}.

%% @private
handle_call(status, _From, State) ->
    StagingStats = msg_store_repo:get_staging_stats(),
    {reply, #{
        queue_len => staging_pending(StagingStats),
        staging_stats => StagingStats
    }, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @private
handle_cast({enqueue, MsgType, MsgId, Data}, State) ->
    _ = Data,
    _ = gen_statem:cast(msg_store_worker, kick),
    ok = ?DEBUG_LOG([msg_store_ds, enqueue, MsgType, MsgId, ok]),
    {noreply, State};

handle_cast({unstage, MsgId}, State) ->
    % 从备份表标记为已处理（异步，不阻塞，带重试）
    elib_async:async_retry(fun() ->
        % 尝试删除各种类型的备份记录
        lists:foreach(fun(Type) ->
            msg_store_repo:mark_processed(Type, MsgId)
        end, [<<"c2c">>, <<"c2g">>, <<"s2c">>, <<"c2s">>])
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(cleanup_staging, State) ->
    % 清理已处理的备份消息（超过 1 小时）
    case msg_store_repo:delete_processed(3600) of
        {ok, Count} when Count > 0 ->
            ok = ?INFO_LOG("msg_store_ds cleanup: deleted ~p processed staging records", [Count]);
        {ok, 0} ->
            ok;
        {error, Reason} ->
            ok = ?ERROR_LOG("msg_store_ds cleanup failed: ~p", [Reason])
    end,
    % 重新启动定时器
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_staging),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok = ?INFO_LOG("msg_store_ds terminated"),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

staging_pending(Result) ->
    case Result of
        {ok, [Row | _]} when is_map(Row) ->
            case maps:get(<<"pending">>, Row, 0) of
                Pending when is_integer(Pending) -> Pending;
                _ -> maps:get(pending, Row, 0)
            end;
        {ok, []} ->
            0;
        _ ->
            0
    end.
