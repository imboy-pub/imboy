-module (user_server).
%%%
% 用户异步行为服务
%%%
-behaviour(gen_server).
-include("chat.hrl").
-include("common.hrl").

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
-export([cast_online/2]).
-export([cast_offline/2]).

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
handle_cast({signup_success, Uid, PostVals}, State) ->
    ?LOG([Uid, hashids_translator:uid_decode(Uid), PostVals]),
    % 生成account
    {noreply, State, hibernate};
% 用户登录成功后的逻辑处理
handle_cast({login_success, Uid, PostVals}, State) ->
    % 用户登录成功之后的业务逻辑处理
    % 更新 user 表
    % 更新 user_client 表
    ?LOG([Uid, hashids_translator:uid_decode(Uid), PostVals]),
    {noreply, State, hibernate};

handle_cast({notice_friend, Uid, ToState}, State) ->
    ?LOG([notice_friend, Uid, ToState]),
    notice_friend(Uid, ToState),
    {noreply, State, hibernate};
handle_cast({offline, Uid, _Pid}, State) ->
    ?LOG([offline, Uid, State]),
    notice_friend(Uid, offline),
    {noreply, State, hibernate};
handle_cast({online, Uid, Pid}, State) ->
    ?LOG([online, Uid, Pid, State]),
    % 检查上线通知好友
    case user_setting_ds:chat_state_hide(Uid) of
        false ->
            % 上线通知好友
            notice_friend(Uid, online),
            ok;
        true ->
            ok
    end,
    % ?LOG(["before check_msg/2",Uid, Pid, State]),
    % 检查离线消息
    msg_c2c_logic:check_msg(Uid, Pid),
    % 检查群聊离线消息
    msg_c2g_logic:check_msg(Uid, Pid),
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
cast_notice_friend(CurrentUid, ChatState) ->
    gen_server:cast(?MODULE, {notice_friend, CurrentUid, ChatState}),
    ok.
%% 检查消息 用异步队列实现
cast_online(Uid, Pid) ->
    gen_server:cast(?MODULE, {online, Uid, Pid}),
    ok.
%% 检查离线消息 用异步队列实现
cast_offline(Uid, Pid) ->
    gen_server:cast(?MODULE, {offline, Uid, Pid}),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


-spec notice_friend(any(), user_chat_state()) -> ok.
notice_friend(Uid, State) ->
    Column = <<"`to_user_id`">>,
    case friend_ds:find_by_uid(Uid, Column) of
        [] ->
            ok;
        Friends ->
            % ?LOG([State, Friends]),
            send_state_msg(Uid, State, Friends),
            ok
    end.

-spec send_state_msg(any(), user_chat_state(), list()) -> ok.
send_state_msg(_FromId, _State, []) -> ok;
% 给在线好友发送上线消息
send_state_msg(FromId, State, [[{<<"to_user_id">>, ToUid}]| Tail]) ->
    case chat_store_repo:lookup(ToUid) of
        [] ->
            ok;
        List ->
            [send_msg_1019(FromId, ToUid2, ToPid2, State) || {_, ToPid2, ToUid2, _Type} <- List]
    end,
    send_state_msg(FromId, State, Tail).

send_msg_1019(From, To, ToPid, _State) ->
    ?LOG([From, To, ToPid]),
    % 用户在线状态变更
    Msg = message_ds:s2c(<<"1019">>, <<"">>, From, To),
    erlang:start_timer(1, ToPid, jsone:encode(Msg)),
    ok.
