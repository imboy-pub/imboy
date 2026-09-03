-module(user_server).

-include("chat.hrl").
-include("log.hrl").

%%%
% 用户异步行为服务
%%%
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([stop/0]).
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
%%
-export([cast_notice_friend/2]).
-export([cast_online/4]).
-export([cast_offline/3]).
-export([cast_cancel/3]).

-define(WORKER_COUNT, 16).
-define(MAX_SHARD_QUEUE, 1000).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 启动用户服务器
%% 启动一个本地注册的gen_server进程来处理用户相关的异步操作。
%% @returns 成功返回{ok, Pid}，失败返回错误信息
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 停止用户服务器
%% 停止用户服务器进程，终止所有异步操作。
%% @returns 停止结果
-spec stop() -> stopped.
stop() ->
    gen_server:call(?MODULE, stop).

%% gen_server.

%% @doc 初始化用户服务器
%% 初始化gen_server的状态数据。
%% @returns 成功返回 {ok, Workers}
-spec init([]) -> {ok, any()}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, start_workers()}.

% gen_server:call是同步的，gen_server:cast是异步的
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(Request, From, State) ->
    ok = ?DEBUG_LOG([handle_call, Request, From, State]),
    {reply, ignored, State}.

% 异步处理请求

% 用户注册成功后的逻辑处理
handle_cast({signup_success, _Uid, _PostVals}, State) ->
    % ?DEBUG_LOG([Uid, PostVals]),
    % 生成account
    {noreply, State, hibernate};
% 用户登录成功后的逻辑处理
handle_cast({login_success, Uid, PostVals}, State) ->
    Uid2 = ec_cnv:to_integer(Uid),
    {noreply, dispatch(Uid2, fun() -> login_success(Uid2, PostVals) end, State), hibernate};
handle_cast({notice_friend, Uid, ToState}, State) ->
    {noreply, dispatch(Uid, fun() -> notice_friend(Uid, ToState) end, State), hibernate};
handle_cast({offline, Uid, _Pid, _DID}, State) ->
    {noreply, dispatch(Uid, fun() -> notice_friend(Uid, <<"offline">>) end, State), hibernate};
handle_cast({cancel, Uid, CreatedAt, Opt}, State) ->
    Fun = fun() -> cancel(Uid, CreatedAt, Opt) end,
    {noreply, dispatch(Uid, Fun, no_replay, State), hibernate};
handle_cast({online, Uid, _Pid, _DType, DID}, State) ->
    {noreply, dispatch(Uid, fun() -> online(Uid, DID) end, State), hibernate};
handle_cast(Msg, State) ->
    ok = ?DEBUG_LOG([Msg, State]),
    {noreply, State}.

login_success(Uid, PostVals) ->
    Now = elib_dt:now(),
    % 记录设备信息
    PostMap = PostVals,
    DID = maps:get(<<"did">>, PostMap, <<"">>),
    _ = user_device_ds:save(Now, Uid, DID, PostMap),
    _ = user_ds:update_friends_last_seen_at(Uid, Now),
    % 分别计算c2c c2g s2c 相关消息类型的表里面是否有离线消息（按设备维度）
    _ = message_ds:check_and_notify_offline_msgs(Uid, DID),
    ok.

online(Uid, DID) ->
    Now = elib_dt:now(),

    % 1. 更新设备活跃时间（合并原 ws_online 逻辑）
    Set = <<"last_active_at = $1::timestamptz">>,
    _ = user_device_ds:update_by_did(Uid, DID, Set, [Now]),

    % 2. 检查离线消息（合并原 ws_online 逻辑，按设备维度）
    _ = message_ds:check_and_notify_offline_msgs(Uid, DID),

    % 3. 在其他设备登录了（原 online 逻辑）
    DName = user_device_logic:device_name(Uid, DID),
    MsgId = elib_id:gen("logged_another_device"),
    Action = <<"logged_another_device">>,
    Payload =
        #{
            <<"did">> => DID,
            <<"dname">> => DName
        },
    Msg = message_ds:assemble_msg(<<"S2C">>, <<>>, Uid, Payload, MsgId, <<>>, Action, null),

    MsLi = elib_retry_config:intervals(<<"notice">>),
    Msg2 = jsone:encode(Msg, [native_utf8]),
    % 给自己的其他设备发送消息（黑名单排除当前设备）：IncludeDIDLi=false。
    % 早前误用 [DID]+true（白名单仅当前设备）——登录设备自己收到
    % "其他设备登录" 通知，而真正需要感知的其他设备永远收不到。
    _ = message_ds:send_next(Uid, MsgId, Msg2, MsLi, [DID], false),

    % 4. 检查上线通知好友（原 online 逻辑）
    case user_setting_ds:chat_state_hide(Uid) of
        false ->
            notice_friend(Uid, <<"online">>),
            ok;
        true ->
            ok
    end.

-spec handle_info(any(), any()) -> {noreply, any()}.
handle_info({worker_done, Pid, Ref}, Shards) ->
    {noreply, [complete_task(Pid, Ref, Shard) || Shard <- Shards]};
handle_info({'EXIT', Pid, Reason}, Shards) ->
    case lists:any(fun(#{pid := Worker}) -> Worker =:= Pid end, Shards) of
        true ->
            _ = ?ERROR_LOG([user_server_worker_restarted, Pid, Reason]),
            {noreply, [restart_worker(Pid, Shard) || Shard <- Shards]};
        false ->
            {noreply, Shards}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, State) ->
    stop_workers(State),
    ok.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, [], _Extra) ->
    process_flag(trap_exit, true),
    {ok, start_workers()};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_workers() ->
    [new_shard() || _ <- lists:seq(1, ?WORKER_COUNT)].

new_shard() ->
    #{pid => spawn_link(fun worker_loop/0), current => undefined, queue => queue:new()}.

stop_workers(Workers) ->
    Monitors =
        [
            begin
                unlink(Pid),
                Ref = erlang:monitor(process, Pid),
                exit(Pid, shutdown),
                {Pid, Ref}
            end
         || #{pid := Pid} <- Workers
        ],
    [
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        end
     || {Pid, Ref} <- Monitors
    ],
    ok.

%% ponytail: fixed shards can still queue under a single hot UID; replace with a
%% supervised per-user queue only if production telemetry proves that ceiling.
dispatch(_Uid, Fun, []) ->
    _ = Fun(),
    [];
dispatch(Uid, Fun, Shards) ->
    dispatch(Uid, Fun, replay, Shards).

dispatch(_Uid, Fun, _Policy, []) ->
    _ = Fun(),
    [];
dispatch(Uid, Fun, Policy, Shards) ->
    Index = erlang:phash2(Uid, length(Shards)) + 1,
    {Before, [Shard | After]} = lists:split(Index - 1, Shards),
    Before ++ [enqueue({Fun, Policy}, Shard) | After].

enqueue(Task, #{current := undefined} = Shard) ->
    run_task(Task, Shard);
enqueue(Task, #{queue := Queue} = Shard) ->
    case queue:len(Queue) < ?MAX_SHARD_QUEUE of
        true ->
            Shard#{queue := queue:in(Task, Queue)};
        false ->
            _ = ?ERROR_LOG([user_server_shard_overloaded, ?MAX_SHARD_QUEUE]),
            Shard
    end.

run_task({Fun, Policy}, #{pid := Pid} = Shard) ->
    Ref = make_ref(),
    Pid ! {run, self(), Ref, Fun},
    Shard#{current := {Ref, Fun, Policy}}.

complete_task(Pid, Ref, #{pid := Pid, current := {Ref, _, _}} = Shard) ->
    next_task(Shard);
complete_task(_Pid, _Ref, Shard) ->
    Shard.

next_task(#{queue := Queue} = Shard) ->
    case queue:out(Queue) of
        {{value, Task}, Rest} -> run_task(Task, Shard#{current := undefined, queue := Rest});
        {empty, _} -> Shard#{current := undefined}
    end.

restart_worker(Pid, #{pid := Pid, current := Current} = Shard) ->
    NewShard = Shard#{pid := spawn_link(fun worker_loop/0)},
    case Current of
        undefined -> NewShard;
        {_Ref, Fun, replay} -> run_task({Fun, replay}, NewShard#{current := undefined});
        {_Ref, _Fun, no_replay} -> next_task(NewShard#{current := undefined})
    end;
restart_worker(_Pid, Shard) ->
    Shard.

worker_loop() ->
    receive
        {run, Owner, Ref, Fun} ->
            try Fun() of
                _ -> ok
            catch
                Class:Reason:Stacktrace ->
                    _ = ?ERROR_LOG([user_server_worker_failed, Class, Reason, Stacktrace])
            end,
            Owner ! {worker_done, self(), Ref},
            worker_loop();
        stop ->
            ok
    end.

%% @doc 异步通知好友状态变更
%% 异步发送通知给用户的所有好友，告知用户状态变更。
%% @param CurrentUid 当前用户ID
%% @param ChatState 聊天状态（如online、offline、hide等）
%% @returns ok
-spec cast_notice_friend(pos_integer(), binary()) -> ok.
cast_notice_friend(CurrentUid, ChatState) ->
    gen_server:cast(?MODULE, {notice_friend, CurrentUid, ChatState}),
    ok.

%% 检查消息 用异步队列实现

%% @doc WebSocket上线异步处理
%% 处理用户WebSocket连接上线后的异步操作，如检查离线消息、
%% 更新设备信息、通知好友等。
%% @param Uid 用户ID
%% @param Pid WebSocket连接进程PID
%% @param DID 设备ID
%% @param DType 设备类型
%% @returns ok
-spec cast_online(pos_integer(), pid(), binary(), binary()) -> ok.
cast_online(Uid, Pid, DID, DType) ->
    gen_server:cast(?MODULE, {online, Uid, Pid, DType, DID}),
    ok.

%% @doc 用户下线异步处理
%% 处理用户下线后的异步操作，如通知好友等。
%% @param Uid 用户ID
%% @param Pid 下线的进程PID
%% @param DID 设备ID
%% @returns ok
-spec cast_offline(pos_integer(), pid(), binary()) -> ok.
cast_offline(Uid, Pid, DID) ->
    gen_server:cast(?MODULE, {offline, Uid, Pid, DID}),
    ok.

%% @doc 异步注销用户
%% 异步处理用户注销，删除用户相关数据并通知好友。
%% 会清理用户的所有关联数据，包括好友关系、设备信息等。
%% @param Uid 用户ID
%% @param CreatedAt 注销时间戳
%% @param Opt 客户端选项信息
%% @returns ok
-spec cast_cancel(pos_integer(), integer(), map()) -> ok.
cast_cancel(Uid, CreatedAt, Opt) ->
    gen_server:cast(?MODULE, {cancel, Uid, CreatedAt, Opt}),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec cancel(any(), any(), any()) -> ok.
cancel(Uid, CreatedAt, Opt) ->
    User = user_ds:find_by_id(
        Uid,
        <<"id,account,nickname,avatar,sign,gender,region,birthday,profession,school,interests,account_type,status,created_at,updated_at">>
    ),
    Setting = user_setting_ds:find_by_uid(Uid),
    CreatedAt2 = elib_dt:to_rfc3339(CreatedAt),
    % 记录用户注销日志
    Body = jsone:encode(#{
        <<"user">> => User,
        <<"setting">> => Setting,
        <<"client_opt">> => Opt
    }),
    _ = user_log_ds:add_internal(undefined, 100, Uid, Body, CreatedAt2),
    % 删除用户所有相关数据（user_ds 内部处理事务）
    _ = user_ds:delete_all_related_data(Uid),
    % 通知好友
    ToUidLi = friend_ds:list_by_uid(Uid),
    Action = <<"user_cancel">>,
    _ = msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, null, #{}, save),
    ok.

-spec notice_friend(integer(), binary()) -> ok.
notice_friend(Uid, Action) ->
    % 用户在线状态变更
    % Action: <<"online">> | <<"offline">> | <<"hide">>.
    ToUidLi = friend_ds:list_by_uid(Uid),
    _ = msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, null, #{}, no_save),
    ok.
