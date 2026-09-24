%%% @doc imboy_domain_event 领域事件总线 eunit 测试（零 mock）。
-module(imboy_domain_event_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    %% imboy_domain_event 是 imboy_sup 子进程：app 常驻时复用其总线，
    %% 只在 app 起不来时自建；cleanup 绝不停 app 总线，只摘自身 handler。
    case eunit_runner:ensure_named_server(imboy_domain_event) of
        {ok, Pid} ->
            {reused, Pid};
        {error, {not_started, _}} ->
            {ok, Pid} = imboy_domain_event:start_link(),
            {own, Pid}
    end.

cleanup({own, Pid}) ->
    catch gen_event:stop(Pid);
cleanup({reused, _Pid}) ->
    catch imboy_domain_event:unsubscribe(de_test_handler, ok),
    ok.

bus_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun publish_delivers_to_subscriber/1,
        fun publish2_builds_tuple/1,
        fun publish_empty_is_ok/1
    ]}.

%% 发布的事件应被订阅者收到（原样不变）。
%%
%% 事件名刻意用本套件专属的 de_probe_event：生产总线在 app 常驻时被复用，
%% 而生产订阅者 group_event_handler 对 {member_added, Gid, Uid} 会执行真实
%% 通知链（group_member_join → group_ds:member_uids(Gid) 等真库操作）。本套件
%% 夹具 id 是字符串（<<"g1">>/<<"u2">>），喂进该链会以
%% {integer_overflow,int8,<<"g1">>} 打死连接（实测 dead_connection），进而
%% 拖垮共享 pgsql 池、污染后续套件（ent-org-v21-closure gate7/8 实证
%% cs_pg_widget a01 与 agent_task_logic 的 no_connection 风暴）。
%% 本用例的被测对象是**总线投递语义**（原样送达订阅者），不是生产订阅者的
%% 处理逻辑，故用不触发任何生产 handler 的事件名隔离；断言强度不变。
publish_delivers_to_subscriber(_Pid) ->
    fun() ->
        ok = imboy_domain_event:subscribe(de_test_handler, self()),
        ok = imboy_domain_event:publish([{de_probe_event, <<"g1">>, <<"u2">>}]),
        Got =
            receive
                {domain_event, E} -> E
            after 1000 -> timeout
            end,
        ?assertEqual({de_probe_event, <<"g1">>, <<"u2">>}, Got)
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
