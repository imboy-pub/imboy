-module(moderation_sweep_logic).

%% R-02：处置动作到期 sweep。把 end_at 已过期的 executed 禁言/限制
%% 审计行周期性翻转为 expired（业务失效由原语 until 保证，此处只闭环
%% 审计状态）。默认启用、5 分钟一轮；数据零删除、可随时禁用。

-behaviour(gen_server).

-include("log.hrl").

-export([start_link/0]).
-export([sweep_now/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(DEFAULT_INTERVAL, 300000).

-record(state, {interval :: integer(), total_expired :: integer()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 运维/测试手动触发一轮 sweep，返回翻转行数。
sweep_now() ->
    gen_server:call(?MODULE, sweep_now).

init([]) ->
    Enabled = application:get_env(imboy, moderation_sweep_enabled, true),
    case Enabled of
        true ->
            Interval = application:get_env(imboy, moderation_sweep_interval, ?DEFAULT_INTERVAL),
            erlang:send_after(Interval, self(), do_sweep),
            ok = ?INFO_LOG([moderation_sweep_logic, started, #{interval => Interval}]),
            {ok, #state{interval = Interval, total_expired = 0}};
        false ->
            ok = ?INFO_LOG([moderation_sweep_logic, disabled]),
            {ok, #state{interval = 0, total_expired = 0}}
    end.

handle_call(sweep_now, _From, State0) ->
    {Count, State} = do_sweep(State0),
    {reply, {ok, Count}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(do_sweep, #state{interval = Interval} = State) when Interval > 0 ->
    _ = do_sweep(State),
    erlang:send_after(Interval, self(), do_sweep),
    {noreply, State};
handle_info(do_sweep, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec do_sweep(#state{}) -> {integer(), #state{}}.
do_sweep(State0) ->
    case moderation_action_logic:expire_due() of
        {ok, Count} ->
            NewState = State0#state{total_expired = State0#state.total_expired + Count},
            case Count > 0 of
                true -> ok = ?INFO_LOG([moderation_sweep, expired, #{count => Count}]);
                false -> ok
            end,
            {Count, NewState};
        {error, Reason} ->
            ok = ?ERROR_LOG([moderation_sweep, expire_due_failed, Reason]),
            {0, State0}
    end.
