%% IMBOY_ERLANG_QUALITY：控制器 SIGTERM 下的资源回收（EUNIT_120S §3.2 孤儿/外部服务清理）。
%%
%% 默认 erl_signal_handler 对 SIGTERM 直接 init:stop()，escript main 的 try/after 不会
%% 执行——2026-10-29 实证：被 TERM 的 group 控制器留下 16 个克隆库、10 份配置副本和一个
%% Garage 守护进程。本处理器替换默认行为：先跑登记的回收闭包，再以 143 退出。
-module(imboy_quality_signal).
-behaviour(gen_event).
-export([install/1, uninstall/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

%% Cleanup 为零元闭包，必须幂等（正常路径已回收时再次调用应无副作用）。
install(Cleanup) when is_function(Cleanup, 0) ->
    ok = os:set_signal(sigterm, handle),
    _ = gen_event:delete_handler(erl_signal_server, erl_signal_handler, []),
    ok = gen_event:add_handler(erl_signal_server, ?MODULE, Cleanup),
    ok.

uninstall() ->
    _ = gen_event:delete_handler(erl_signal_server, ?MODULE, []),
    _ = gen_event:add_handler(erl_signal_server, erl_signal_handler, []),
    ok.

init(Cleanup) -> {ok, Cleanup}.

handle_event(sigterm, Cleanup) ->
    io:format(
        standard_error,
        "quality.escript: SIGTERM received, releasing task resources before exit~n",
        []
    ),
    try
        Cleanup()
    catch
        Class:Reason ->
            io:format(standard_error, "quality.escript: cleanup on SIGTERM failed ~p:~p~n", [
                Class, Reason
            ])
    end,
    erlang:halt(143);
handle_event(_Other, State) ->
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
terminate(_Reason, _State) -> ok.
