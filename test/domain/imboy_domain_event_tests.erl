%%% @doc imboy_domain_event 领域事件总线 eunit 测试（零 mock）。
-module(imboy_domain_event_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Pid} = imboy_domain_event:start_link(),
    Pid.

cleanup(Pid) ->
    gen_event:stop(Pid).

bus_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun publish_delivers_to_subscriber/1,
        fun publish2_builds_tuple/1,
        fun publish_empty_is_ok/1
    ]}.

%% 发布的事件应被订阅者收到（原样不变）。
publish_delivers_to_subscriber(_Pid) ->
    fun() ->
        ok = imboy_domain_event:subscribe(de_test_handler, self()),
        ok = imboy_domain_event:publish([{member_added, <<"g1">>, <<"u2">>}]),
        Got =
            receive
                {domain_event, E} -> E
            after 1000 -> timeout
            end,
        ?assertEqual({member_added, <<"g1">>, <<"u2">>}, Got)
    end.

%% publish/2 语法糖应正确组装 tuple。
publish2_builds_tuple(_Pid) ->
    fun() ->
        ok = imboy_domain_event:subscribe(de_test_handler, self()),
        ok = imboy_domain_event:publish(
            owner_transferred, [<<"g1">>, <<"u1">>, <<"u2">>]
        ),
        Got =
            receive
                {domain_event, E} -> E
            after 1000 -> timeout
            end,
        ?assertEqual({owner_transferred, <<"g1">>, <<"u1">>, <<"u2">>}, Got)
    end.

%% 空事件列表不应报错。
publish_empty_is_ok(_Pid) ->
    fun() ->
        ?assertEqual(ok, imboy_domain_event:publish([]))
    end.
