-module(user_server).

-include_lib("imlib/include/chat.hrl").

-include_lib("imlib/include/log.hrl").
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


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec stop() -> stopped.
stop() ->
    gen_server:call(?MODULE, stop).


%% gen_server.


-spec init([]) -> {ok, []}.
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
    % 记录设备信息 END
    {noreply, State, hibernate};
% 用户登录成功后的逻辑处理
handle_cast({ws_online, Uid, _DType, DID}, State) ->
    % ?DEBUG_LOG([handle_cast, ws_online, Uid, DType, DID, State]),
    % 更新 最近活跃时间
    Set = <<"last_active_at = $1">>,
    SetArgs = [imboy_dt:now()],
    user_device_repo:update_by_did(Uid, DID, Set, SetArgs),
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
handle_cast({online, Uid, Pid, DID}, State) ->
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
    % ?DEBUG_LOG(["before check_msg/2",Uid, Pid, State]),
    % 检查 S2C C2C 离线消息
    msg_s2c_logic:check_msg(Uid, Pid, DID),
    msg_c2c_logic:check_msg(Uid, Pid, DID),
    % 检查群聊离线消息
    msg_c2g_logic:check_msg(Uid, Pid, DID),
    {noreply, State, hibernate};

handle_cast(Msg, State) ->
    ?DEBUG_LOG([Msg, State]),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 切换在线状态 异步通知好友
cast_notice_friend(CurrentUid, ChatState) ->
    gen_server:cast(?MODULE, {notice_friend, CurrentUid, ChatState}),
    ok.


%% 检查消息 用异步队列实现

%% ws 上线后的异步操作 例如检查离线消息等
-spec cast_online(binary(), pid(), binary(), binary()) -> ok.
cast_online(Uid, Pid, DID, DType) ->
    gen_server:cast(?MODULE, {ws_online, Uid, DType, DID}),
    gen_server:cast(?MODULE, {online, Uid, Pid, DID}),
    ok.


%% 检查离线消息 用异步队列实现
cast_offline(Uid, Pid, DID) ->
    gen_server:cast(?MODULE, {offline, Uid, Pid, DID}),
    ok.

%% 异步注销用户
cast_cancel(Uid, CreatedAt, Opt) ->
    gen_server:cast(?MODULE, {cancel, Uid, CreatedAt, Opt}),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

cancel(Uid, CreatedAt, Opt) ->
    User = user_repo:find_by_id(Uid, <<"*">>),
    Setting = user_setting_ds:find_by_uid(Uid),
    imboy_db:with_transaction(fun(Conn) ->
        {ok, Body} = jsone_encode:encode(#{
            <<"user">> => User,
            <<"setting">> => Setting,
            <<"client_opt">> => Opt
        }, [native_utf8]),
        user_log_repo:add(Conn, #{
            % 日志类型: 100 用户注销备份
            type => 100,
            uid => Uid,
            body => Body,
            created_at => CreatedAt
            }),
        UidBin = ec_cnv:to_binary(Uid),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_repo:tablename())/binary, " WHERE id = ", UidBin/binary>>
            , []),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_collect_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_denylist_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_device_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_friend_repo:tablename())/binary, " WHERE from_user_id = ", UidBin/binary>>
            , []),
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_friend_repo:tablename())/binary, " WHERE to_user_id = ", UidBin/binary>>
            , []),
        imboy_db:execute(Conn
            , <<"DELETE FROM user_friend_category WHERE owner_user_id = ", UidBin/binary>>
            , []),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_device_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_setting_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_tag_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),

        imboy_db:execute(Conn
            , <<"DELETE FROM ", (user_tag_relation_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),
        imboy_db:execute(Conn
            , <<"DELETE FROM fts_user WHERE user_id = ", UidBin/binary>>
            , []),
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (geo_people_nearby_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),

        %
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (group_repo:tablename())/binary, " WHERE owner_uid = ", UidBin/binary>>
            , []),
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (group_member_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),
        imboy_db:execute(Conn
            , <<"DELETE FROM ", (group_random_code_repo:tablename())/binary, " WHERE user_id = ", UidBin/binary>>
            , []),
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
