-module(license_notice_worker).
-behaviour(gen_server).
%%%===================================================================
%%% @doc License 到期提醒定时 worker。
%%%
%%% 每日检查授权到期；命中提醒档（30/7/1 天或已过期）且「已启用 + 配置了
%%% 收件人」时，经 elib_email 发送提醒。同一档位只发一次（防重，进程内状态）。
%%%
%%% ⚠️ 默认禁用 + 收件人空列表 = 不发任何邮件：避免擅自对外发信。上线需运维
%%% 在 sys.config 显式配置（收件人邮箱由运维提供，本模块不内置任何地址）：
%%%   {license_notice_enabled, true}         %% 默认 false
%%%   {license_notice_to, [<<"ops@example.com">>]}  %% 默认 []，示例非真实地址
%%%   {license_notice_interval_ms, 86400000} %% 默认每日
%%% @end
%%%===================================================================

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% 供测试：防重决策纯函数
-export([should_send/2]).

-include("log.hrl").

%% 每日检查；启动后延迟首检，等 license 加载与时钟稳定
-define(CHECK_INTERVAL_MS, 86400000).
-define(INITIAL_DELAY_MS, 60000).

-record(state, {last_notified = none}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(?INITIAL_DELAY_MS, self(), check),
    {ok, #state{}}.

handle_info(check, State) ->
    NewState = run_check(State),
    erlang:send_after(interval_ms(), self(), check),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 启用且有收件人才检查；否则原样返回（不发信）
-spec run_check(#state{}) -> #state{}.
run_check(#state{last_notified = Last} = State) ->
    case enabled() andalso recipients() =/= [] of
        false ->
            State;
        true ->
            ExpiresAt = imboy_license:expires_at(),
            case ExpiresAt > 0 of
                false ->
                    %% 社区版/无到期：重置防重，便于将来获授权后重新提醒
                    State#state{last_notified = none};
                true ->
                    Due = license_expiry_notice:due_threshold(
                        ExpiresAt, erlang:system_time(millisecond)
                    ),
                    case should_send(Due, Last) of
                        true ->
                            send_notices(Due),
                            State#state{last_notified = Due};
                        false when Due =:= none ->
                            State#state{last_notified = none};
                        false ->
                            State
                    end
            end
    end.

%% @doc 防重决策（纯函数）：无命中档或与上次已发档相同则不发。
-spec should_send(none | expired | 1 | 7 | 30, term()) -> boolean().
should_send(none, _Last) -> false;
should_send(Due, Due) -> false;
should_send(_Due, _Last) -> true.

-spec send_notices(expired | 1 | 7 | 30) -> ok.
send_notices(Due) ->
    {Subject, Body} = license_expiry_notice:render(Due, imboy_license:licensee()),
    Tos = recipients(),
    lists:foreach(
        fun(To) ->
            case elib_email:send(To, Subject, Body) of
                {ok, _} -> ok;
                {error, R} -> ?WARN_LOG("[license_notice] send to ~ts failed: ~p", [To, R])
            end
        end,
        Tos
    ),
    ?INFO_LOG("[license_notice] threshold=~p notified ~p recipient(s)", [Due, length(Tos)]),
    ok.

-spec enabled() -> boolean().
enabled() ->
    application:get_env(imboy, license_notice_enabled, false).

-spec recipients() -> [binary()].
recipients() ->
    application:get_env(imboy, license_notice_to, []).

-spec interval_ms() -> pos_integer().
interval_ms() ->
    application:get_env(imboy, license_notice_interval_ms, ?CHECK_INTERVAL_MS).
