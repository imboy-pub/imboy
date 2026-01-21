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
%% @returns 成功返回{ok, State}，State为空列表
-spec init([]) -> {ok, any()}.
init([]) ->
    {ok, []}.

% gen_server:call是同步的，gen_server:cast是异步的
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(Request, From, State) ->
    ok = ?DEBUG_LOG([handle_call, Request, From, State]),
    {reply, ignored, State}.

% 异步处理请求

% 用户注册成功后的逻辑处理
handle_cast({signup_success, _Uid, _PostVals}, State) ->
    % ?DEBUG_LOG([Uid, elib_hashids:decode(Uid), PostVals]),
    % 生成account
    {noreply, State, hibernate};
% 用户登录成功后的逻辑处理
handle_cast({login_success, Uid, PostVals}, State) ->
    % 用户登录成功之后的业务逻辑处理
    Uid2 = elib_hashids:decode(Uid),
    Now = elib_dt:now(),
    % 记录设备信息
    PostMap = PostVals,
    DID = maps:get(<<"did">>, PostMap, <<"">>),
    _ = user_device_ds:save(Now, Uid2, DID, PostMap),
    _ = user_ds:update_friends_last_seen_at(Uid2, Now),
    % 分别计算c2c c2g s2c 相关消息类型的表里面是否有离线消息
    _ = message_ds:check_and_notify_offline_msgs(Uid2),

    % 记录设备信息 END
    {noreply, State, hibernate};
handle_cast({notice_friend, Uid, ToState}, State) ->
    % ?DEBUG_LOG([notice_friend, Uid, ToState]),
    _ = notice_friend(Uid, ToState),
    {noreply, State, hibernate};
handle_cast({offline, Uid, _Pid, _DID}, State) ->
    % ?DEBUG_LOG([offline, Uid, State, DID]),
    _ = notice_friend(Uid, <<"offline">>),
    {noreply, State, hibernate};
handle_cast({cancel, Uid, CreatedAt, Opt}, State) ->
    _ = cancel(Uid, CreatedAt, Opt),
    {noreply, State, hibernate};
handle_cast({online, Uid, _Pid, _DType, DID}, State) ->
    Now = elib_dt:now(),

    % 1. 更新设备活跃时间（合并原 ws_online 逻辑）
    Set = <<"last_active_at = $1::timestamptz">>,
    _ = user_device_ds:update_by_did(Uid, DID, Set, [Now]),

    % 2. 检查离线消息（合并原 ws_online 逻辑）
    _ = message_ds:check_and_notify_offline_msgs(Uid),

    % 3. 在其他设备登录了（原 online 逻辑）
    DName = user_device_logic:device_name(Uid, DID),
    MsgId = elib_id:gen("logged_another_device"),
    Action = <<"logged_another_device">>,
    Payload =
        #{<<"did">> => DID,
          <<"dname">> => DName},
    Msg = message_ds:assemble_msg(<<"S2C">>, <<>>, Uid, Payload, MsgId, <<>>, Action, <<>>),

    MsLi = elib_retry_config:intervals(<<"notice">>),
    Msg2 = jsone:encode(Msg, [native_utf8]),
    % 给自己的其他设备发生消息
    _ = message_ds:send_next(Uid, MsgId, Msg2, MsLi, [DID], true),

    % 4. 检查上线通知好友（原 online 逻辑）
    case user_setting_ds:chat_state_hide(Uid) of
        false ->
            notice_friend(Uid, <<"online">>),
            ok;
        true ->
            ok
    end,
    {noreply, State, hibernate};
handle_cast(Msg, State) ->
    ok = ?DEBUG_LOG([Msg, State]),
    {noreply, State}.

-spec handle_info(any(), any()) -> {noreply, any()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    User = user_ds:find_by_id(Uid, <<"*">>),
    Setting = user_setting_ds:find_by_uid(Uid),
    CreatedAt2 = elib_dt:to_rfc3339(CreatedAt),
    % 记录用户注销日志
    Body = jsone:encode(#{<<"user">> => User,
                          <<"setting">> => Setting,
                          <<"client_opt">> => Opt}),
    _ = user_log_ds:add_internal(undefined, 100, Uid, Body, CreatedAt2),
    % 删除用户所有相关数据（user_ds 内部处理事务）
    _ = user_ds:delete_all_related_data(Uid),
    % 通知好友
    ToUidLi = friend_ds:list_by_uid(Uid),
    Action = <<"user_cancel">>,
    _ = msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, <<>>, #{}, save),
    ok.

-spec notice_friend(integer(), binary()) -> ok.
notice_friend(Uid, Action) ->
    % 用户在线状态变更
    % Action: <<"online">> | <<"offline">> | <<"hide">>.
    ToUidLi = friend_ds:list_by_uid(Uid),
    _ = msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, <<>>, #{}, no_save),
    ok.
