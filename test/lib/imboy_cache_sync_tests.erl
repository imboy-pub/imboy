-module(imboy_cache_sync_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("cache.hrl").
-include("chat.hrl").

-record(state, {}).

-define(TEST_KEY, <<"test_key">>).
-define(TEST_DATA, #{id => 123, name => <<"Test">>}).
-define(TEST_MAX_AGE, 3600).
-define(TEST_DEPEND, [<<"user:123">>]).

%% ⚠️ eunit 不解释 {Desc, fun} 返回的 {setup,...} spec（探针实证），
%% ?WITH_MECKS 包在 {Desc, fun} 体内 = 静默空转。此 helper 立即执行等价语义：
%% setup → 执行断言 → cleanup，使断言真实生效（simple fun 与 generator 同进程，
%% Self 哨兵可用，无需改进程字典）。
run_with_mocks(MockConfigs, TestFun) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({mock_setup_failed, Module, Reason})
            end
        end,
        MockConfigs
    ),
    try
        TestFun()
    after
        lists:foreach(
            fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
            MockConfigs
        )
    end.

broadcast_delegates_to_syn_publish_test_() ->
    ?TEST_WITH_APP(fun() ->
        run_with_mocks(
            [
                {syn, [
                    {'publish', 3, fun(Scope, Group, Message) ->
                        ?assertEqual(?CACHE_SCOPE, Scope),
                        ?assertEqual(dsync_handler, Group),
                        ?assertEqual({cache_sync, #{action => set, key => ?TEST_KEY}}, Message),
                        {ok, 2}
                    end}
                ]}
            ],
            fun() ->
                ?assertEqual(
                    {ok, 2}, imboy_cache_sync:broadcast(#{action => set, key => ?TEST_KEY})
                )
            end
        )
    end).

init_registers_with_syn_test_() ->
    ?TEST_WITH_APP(fun() ->
        run_with_mocks(
            [
                {syn, [
                    {'join', 4, fun(Scope, Group, Pid, Meta) ->
                        ?assertEqual(?CACHE_SCOPE, Scope),
                        ?assertEqual(dsync_handler, Group),
                        ?assert(is_pid(Pid)),
                        ?assertEqual(#{}, Meta),
                        ok
                    end}
                ]}
            ],
            fun() ->
                ?assertMatch({ok, #state{}}, imboy_cache_sync:init([]))
            end
        )
    end).

init_returns_badmatch_when_syn_join_fails_test_() ->
    ?TEST_WITH_APP(fun() ->
        run_with_mocks(
            [
                {syn, [
                    {'join', 4, fun(_Scope, _Group, _Pid, _Meta) ->
                        {error, register_failed}
                    end}
                ]}
            ],
            fun() ->
                ?assertError({badmatch, {error, register_failed}}, imboy_cache_sync:init([]))
            end
        )
    end).

handle_info_set_message_delegates_to_depcache_set_test_() ->
    ?TEST_WITH_APP(fun() ->
        run_with_mocks(
            [
                {depcache, [
                    {'set', 5, fun(Key, Data, MaxAge, Depend, Server) ->
                        ?assertEqual(?TEST_KEY, Key),
                        ?assertEqual(?TEST_DATA, Data),
                        ?assertEqual(?TEST_MAX_AGE, MaxAge),
                        ?assertEqual(?TEST_DEPEND, Depend),
                        ?assertEqual(?DEPCACHE_SERVER, Server),
                        ok
                    end}
                ]}
            ],
            fun() ->
                ?assertEqual(
                    {noreply, #state{}},
                    imboy_cache_sync:handle_info(
                        {cache_sync, {set, ?TEST_KEY, ?TEST_DATA, ?TEST_MAX_AGE, ?TEST_DEPEND}},
                        #state{}
                    )
                )
            end
        )
    end).

handle_info_flush_message_delegates_to_depcache_flush_test_() ->
    ?TEST_WITH_APP(fun() ->
        run_with_mocks(
            [
                {depcache, [
                    {'flush', 2, fun(Key, Server) ->
                        ?assertEqual(?TEST_KEY, Key),
                        ?assertEqual(?DEPCACHE_SERVER, Server),
                        ok
                    end}
                ]}
            ],
            fun() ->
                ?assertEqual(
                    {noreply, #state{}},
                    imboy_cache_sync:handle_info({cache_sync, {flush, ?TEST_KEY}}, #state{})
                )
            end
        )
    end).

handle_info_flush_all_message_delegates_to_depcache_flush_test_() ->
    ?TEST_WITH_APP(fun() ->
        run_with_mocks(
            [
                {depcache, [
                    {'flush', 1, fun(Server) ->
                        ?assertEqual(?DEPCACHE_SERVER, Server),
                        ok
                    end}
                ]}
            ],
            fun() ->
                ?assertEqual(
                    {noreply, #state{}},
                    imboy_cache_sync:handle_info({cache_sync, flush}, #state{})
                )
            end
        )
    end).

handle_info_unknown_message_is_ignored_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({noreply, #state{}}, imboy_cache_sync:handle_info(unknown, #state{}))
    end).

terminate_leaves_syn_group_test_() ->
    ?TEST_WITH_APP(fun() ->
        run_with_mocks(
            [
                {syn, [
                    {'leave', 3, fun(Scope, Group, Pid) ->
                        ?assertEqual(?CACHE_SCOPE, Scope),
                        ?assertEqual(dsync_handler, Group),
                        ?assert(is_pid(Pid)),
                        ok
                    end}
                ]}
            ],
            fun() ->
                ?assertEqual(ok, imboy_cache_sync:terminate(normal, #state{}))
            end
        )
    end).

handle_call_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {reply, ok, #state{}}, imboy_cache_sync:handle_call(request, {self(), ref}, #state{})
        )
    end).

handle_cast_returns_noreply_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({noreply, #state{}}, imboy_cache_sync:handle_cast(message, #state{}))
    end).

code_change_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, #state{}}, imboy_cache_sync:code_change(old_vsn, #state{}, #{}))
    end).
