%%% @doc 领域事件总线测试用订阅者 handler。
%%% 把收到的事件转发给注册时传入的测试进程 pid，便于 eunit 断言。
-module(de_test_handler).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

init(Pid) when is_pid(Pid) ->
    {ok, Pid}.

handle_event(Event, Pid) ->
    Pid ! {domain_event, Event},
    {ok, Pid}.

handle_call(_Req, Pid) ->
    {ok, ok, Pid}.

handle_info(_Info, Pid) ->
    {ok, Pid}.

terminate(_Reason, _Pid) ->
    ok.

code_change(_OldVsn, Pid, _Extra) ->
    {ok, Pid}.
