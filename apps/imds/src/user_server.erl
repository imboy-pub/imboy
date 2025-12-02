-module(user_server).

-include_lib("imlib/include/chat.hrl").

-include_lib("imlib/include/log.hrl").
%%%
% 用户异步行为服务
%%%
-behaviour(gen_server).

%% Types
-type user_id() :: integer() | binary().
-type device_id() :: binary().
-type device_type() :: binary().
-type chat_state() :: binary().
-type timestamp() :: integer().
-type gen_server_state() :: any().

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
-spec init([]) -> {ok, gen_server_state()}.
init([]) ->
    {ok, []}.


% gen_server:call是同步的，gen_server:cast是异步的
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(Request, From, State) ->
    ?DEBUG_LOG([handle_call, Request, From, State]),
    {reply, ignored, State}.


% 异步处理请求

% 用户注册成功后的逻辑处理
handle_cast({signup_success, _Uid, _PostVals}, State) ->
    % ?DEBUG_LOG([Uid, imboy_hashids:decode(Uid), PostVals]),
    % 生成account
    {noreply, State, hibernate};
% 用户登录成功后的逻辑处理
handle_cast({login_success, Uid, PostVals}, State) ->
    % 用户登录成功之后的业务逻辑处理
    % 更新 user 表
    % 更新 user_client 表
    Uid2 = imboy_hashids:decode(Uid),
    Now = imboy_dt:now(),
    % ?DEBUG_LOG([Uid, Uid2, PostVals]),
    % 记录设备信息
    DID = proplists:get_value(<<"did">>, PostVals, <<"">>),
    user_device_repo:save(Now, Uid2, DID, PostVals),
    user_repo:update_friends_last_seen_at(Uid2, Now),
    % 分别计算c2c c2g s2c 相关消息类型的表里面是否有离线消息
    message_ds:check_and_notify_offline_msgs(Uid2),

    % 记录设备信息 END
    {noreply, State, hibernate};
% 用户登录成功后的逻辑处理
handle_cast({ws_online, Uid, _Pid, _DType, DID}, State) ->
    % ?DEBUG_LOG([handle_cast, ws_online, Uid, Pid, DType, DID, State]),
    % 更新 最近活跃时间
    Set = <<"last_active_at = $1::timestamptz">>,
    SetArgs = [imboy_dt:now()],
    user_device_repo:update_by_did(Uid, DID, Set, SetArgs),
    % 分别计算c2c c2g s2c 相关消息类型的表里面是否有离线消息
    message_ds:check_and_notify_offline_msgs(imboy_hashids:decode(Uid)),
    {noreply, State, hibernate};

handle_cast({notice_friend, Uid, ToState}, State) ->
    ?DEBUG_LOG([notice_friend, Uid, ToState]),
    notice_friend(Uid, ToState),
    {noreply, State, hibernate};
handle_cast({offline, Uid, _Pid, DID}, State) ->
    ?DEBUG_LOG([offline, Uid, State, DID]),
    notice_friend(Uid, <<"offline">>),
    {noreply, State, hibernate};
handle_cast({cancel, Uid, CreatedAt, Opt}, State) ->
    cancel(Uid, CreatedAt, Opt),
    {noreply, State, hibernate};
handle_cast({online, Uid, _Pid, DID}, State) ->
    % ?DEBUG_LOG([online, Uid, Pid, State, DID]),
    DName = user_device_logic:device_name(Uid, DID),
    % 在其他设备登录了
    MsgId = <<"logged_another_device">>,
    Payload = [{<<"msg_type">>, MsgId}, {<<"did">>, DID}, {<<"dname">>, DName}],
    Msg = message_ds:assemble_msg(<<"S2C">>, <<>>, Uid, Payload, MsgId),

    MsLi = [0, 5000, 10000],
    Msg2 = jsone:encode(Msg, [native_utf8]),
    % 给自己的其他设备发生消息
    message_ds:send_next(Uid, MsgId, Msg2, MsLi, [DID], false),
    % end

    % 检查上线通知好友
    case user_setting_ds:chat_state_hide(Uid) of
        false ->
            % 上线通知好友
            notice_friend(Uid, <<"online">>),
            ok;
        true ->
            ok
    end,
    {noreply, State, hibernate};

handle_cast(Msg, State) ->
    ?DEBUG_LOG([Msg, State]),
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
-spec cast_notice_friend(user_id(), chat_state()) -> ok.
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
-spec cast_online(user_id(), pid(), device_id(), device_type()) -> ok.
cast_online(Uid, Pid, DID, DType) ->
    gen_server:cast(?MODULE, {ws_online, Uid, Pid, DType, DID}),
    gen_server:cast(?MODULE, {online, Uid, Pid, DID}),
    ok.

%% @doc 用户下线异步处理
%% 处理用户下线后的异步操作，如通知好友等。
%% @param Uid 用户ID
%% @param Pid 下线的进程PID
%% @param DID 设备ID
%% @returns ok
-spec cast_offline(user_id(), pid(), device_id()) -> ok.
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
-spec cast_cancel(user_id(), timestamp(), map()) -> ok.
cast_cancel(Uid, CreatedAt, Opt) ->
    gen_server:cast(?MODULE, {cancel, Uid, CreatedAt, Opt}),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec cancel(any(), any(), any()) -> ok.
cancel(Uid, CreatedAt, Opt) ->
    User = user_repo:find_by_id(Uid, <<"*">>),
    Setting = user_setting_ds:find_by_uid(Uid),
    imboy_db:with_transaction(fun(Conn) ->
        Body = jsone:encode(#{
            <<"user">> => User,
            <<"setting">> => Setting,
            <<"client_opt">> => Opt
        }),
        user_log_repo:add(Conn, #{
            % 日志类型: 100 用户注销备份
            type => 100,
            uid => Uid,
            body => Body,
            created_at => CreatedAt
            }),
        % 使用参数化查询避免SQL注入和类型转换问题
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_repo:tablename())/binary, " WHERE id = $1">>
            , [Uid]),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_collect_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_denylist_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_device_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_friend_repo:tablename())/binary, " WHERE from_user_id = $1">>
            , [Uid]),
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_friend_repo:tablename())/binary, " WHERE to_user_id = $1">>
            , [Uid]),
        imboy_db:execute(Conn
            , <<"DELETE FROM user_friend_category WHERE owner_user_id = $1">>
            , [Uid]),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_device_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_setting_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_tag_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_tag_relation_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),
        imboy_db:execute(Conn
            , <<"DELETE FROM fts_user WHERE user_id = $1">>
            , [Uid]),
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (geo_people_nearby_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),

        %
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (group_repo:tablename())/binary, " WHERE owner_uid = $1">>
            , [Uid]),
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (group_member_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (group_random_code_repo:tablename())/binary, " WHERE user_id = $1">>
            , [Uid]),
        ok
    end),

    ToUidLi = friend_ds:list_by_uid(Uid),
    MsgType = <<"user_cancel">>,
    msg_s2c_ds:send(Uid, MsgType, ToUidLi, save),
    ok.

-spec notice_friend(integer(), binary()) -> ok.
notice_friend(Uid, State) ->
    % 用户在线状态变更
    % State: <<"online">> | <<"offline">> | <<"hide">>.
    ToUidLi = friend_ds:list_by_uid(Uid),
    msg_s2c_ds:send(Uid, State, ToUidLi, no_save),
    ok.

