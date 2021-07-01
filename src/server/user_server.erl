-module (user_server).
%%%
% 用户异步行为服务
%%%
-behaviour(gen_server).
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

handle_cast({login_success, UserId, DeviceNumber}, State) ->
    % 用户登录成功之后的业务逻辑处理
    % 更新 user 表
    % 更新 user_client 表
    ?LOG([UserId, DeviceNumber]),
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


%% Internal.
