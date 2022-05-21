-module(user_server).

-include_lib("imboy/include/chat.hrl").

-include_lib("imboy/include/common.hrl").
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
-export([cast_online/3]).
-export([cast_offline/3]).


%% API.

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


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(Request, From, State) ->
    ?LOG([handle_call, Request, From, State]),
    {reply, ignored, State}.


% 异步处理请求

% 用户注册成功后的逻辑处理
handle_cast({signup_success, UID, PostVals}, State) ->
    ?LOG([UID, imboy_hashids:uid_decode(UID), PostVals]),
    % 生成account
    {noreply, State, hibernate};
% 用户登录成功后的逻辑处理
handle_cast({login_success, UID, PostVals}, State) ->
    % 用户登录成功之后的业务逻辑处理
    % 更新 user 表
    % 更新 user_client 表
    UID2 = imboy_hashids:uid_decode(UID),
    Now = imboy_dt:milliseconds(),
    ?LOG([UID, UID2, PostVals]),
    % 记录设备信息
    DID = proplists:get_value(<<"did">>, PostVals, <<"">>),
    user_device_repo:save(Now, UID2, DID, PostVals),
    % 记录设备信息 END
    {noreply, State, hibernate};

handle_cast({notice_friend, UID, ToState}, State) ->
    ?LOG([notice_friend, UID, ToState]),
    notice_friend(UID, ToState),
    {noreply, State, hibernate};
handle_cast({offline, UID, _Pid, DID}, State) ->
    ?LOG([offline, UID, State, DID]),
    notice_friend(UID, offline),
    {noreply, State, hibernate};

handle_cast({online, UID, Pid, DID}, State) ->
    ?LOG([online, UID, Pid, State, DID]),
    % 检查上线通知好友
    case user_setting_ds:chat_state_hide(UID) of
        false ->
            % 上线通知好友
            notice_friend(UID, online),
            ok;
        true ->
            ok
    end,
    % ?LOG(["before check_msg/2",UID, Pid, State]),
    % 检查离线消息
    msg_c2c_logic:check_msg(UID, Pid, DID),
    % 检查群聊离线消息
    msg_c2g_logic:check_msg(UID, Pid, DID),
    {noreply, State, hibernate};

handle_cast(Msg, State) ->
    ?LOG([Msg, State]),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 切换在线状态 异步通知好友
cast_notice_friend(CurrentUID, ChatState) ->
    gen_server:cast(?MODULE, {notice_friend, CurrentUID, ChatState}),
    ok.


%% 检查消息 用异步队列实现

-spec cast_online(UID :: binary(), Pid :: pid(), DID :: binary()) -> ok.
cast_online(UID, Pid, DID) ->
    gen_server:cast(?MODULE, {online, UID, Pid, DID}),
    ok.


%% 检查离线消息 用异步队列实现
cast_offline(UID, Pid, DID) ->
    gen_server:cast(?MODULE, {offline, UID, Pid, DID}),
    ok.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec notice_friend(any(), user_chat_state()) -> ok.
notice_friend(UID, State) ->
    Column = <<"`to_user_id`">>,
    case friend_ds:find_by_uid(UID, Column) of
        [] ->
            ok;
        Friends ->
            % ?LOG([State, Friends]),
            send_state_msg(UID, State, Friends),
            ok
    end.


-spec send_state_msg(any(), user_chat_state(), list()) -> ok.
send_state_msg(_FromId, _State, []) ->
    ok;
% 给在线好友发送上线消息
send_state_msg(FromId, State, [[{<<"to_user_id">>, ToUID}] | Tail]) ->
    case chat_online:lookup(ToUID) of
        [] ->
            ok;
        List ->
            [send_msg_code1019(FromId, ToUID2, ToPid2, State) ||
                {_, ToPid2, ToUID2, _DType, _DID} <- List]
    end,
    send_state_msg(FromId, State, Tail).


send_msg_code1019(From, To, ToPid, _State) ->
    ?LOG([From, To, ToPid]),
    % 用户在线状态变更
    Msg = message_ds:assemble_s2c(<<"1019">>, <<"">>, From, To),
    erlang:start_timer(1, ToPid, jsone:encode(Msg, [native_utf8])),
    ok.
