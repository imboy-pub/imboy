-module (offline_server).
%%%
% 离线消息发送服务
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

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

% 异步处理请求
handle_cast({notice_friend, Uid, ToState}, State) ->
    % ?LOG([Uid, ToState]),
    notice_friend(Uid, ToState),
    {noreply, State, hibernate};
handle_cast({offline, Uid, _Pid}, State) ->
    notice_friend(Uid, offline),
    {noreply, State, hibernate};
handle_cast({online, Uid, Pid}, State) ->
    % ?LOG([Uid, Pid, State]),
    % 检查上线通知好友
    case user_setting_ds:chat_state_hide(Uid) of
        false ->
            % 上线通知好友
            notice_friend(Uid, online),
            ok;
        true ->
            ok
    end,

    % 检查离线消息
    dialog_msg_as:check_msg(Uid, Pid),
    % 检查群聊离线消息
    group_msg_as:check_msg(Uid, Pid),
    {noreply, State, hibernate};
handle_cast(_Msg, State) ->
    % ?LOG([Msg, State]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal.

-spec notice_friend(any(), user_chat_state()) -> ok.
notice_friend(Uid, State) ->
    Column = <<"`to_user_id`">>,
    case friend_ds:find_by_uid(Uid, Column) of
        [] ->
            ok;
        Friends ->
            send_state_msg(Uid, State, Friends),
            ok
    end.

-spec send_state_msg(any(), user_chat_state(), list()) -> ok.
send_state_msg(_FromId, _State, []) ->
    ok;
%% 给在线好友发送上线消息
send_state_msg(FromId, State, [[{<<"to_user_id">>, ToUid}]| Tail]) ->
    case user_ds:is_offline(ToUid) of
        {Pid, _ToUid, _Type} ->
            Msg = [
                {<<"type">>, <<"user_state">>},
                {<<"from_id">>, FromId},
                {<<"to_id">>, ToUid},
                {<<"status">>, State},
                {<<"timestamp">>, dt_util:milliseconds()}
            ],
            % ?LOG(Msg),
            erlang:start_timer(1, Pid, jsx:encode(Msg));
        _ ->
            ok
    end,
    send_state_msg(FromId, State, Tail).
