-module(llm_stream_fanout_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% Phase 4 T4.2：llm_stream fan-out 多观察者 单测。
% 验证 observer_uids 扇出给每个 uid；缺省时回退单 target_uid（Phase 2 向后兼容）。
% 每个测试在独立进程跑（llm_stream 用进程字典存状态）。
%%%

%% 向后兼容：无 observer_uids → 仅推给 target_uid（Phase 2 原行为）
single_target_backward_compat_test_() ->
    ?WITH_MECK(
        imboy_syn,
        [
            {'publish', 2, fun(_Uid, _Json) -> {ok, 1} end}
        ],
        fun() ->
            run_isolated(fun() ->
                Ctx = #{
                    stream_id => <<"s1">>,
                    target_uid => 7,
                    from => <<"42">>,
                    to => <<"7">>,
                    type => <<"C2C">>
                },
                ok = llm_stream:flush_tail(Ctx),
                %% 只推给 target_uid=7 一次
                ?assertEqual(1, meck:num_calls(imboy_syn, publish, [7, '_']))
            end)
        end
    ).

%% fan-out：有 observer_uids → 推给每个观察者，不用 target_uid
fanout_to_all_observers_test_() ->
    ?WITH_MECK(
        imboy_syn,
        [
            {'publish', 2, fun(_Uid, _Json) -> {ok, 1} end}
        ],
        fun() ->
            run_isolated(fun() ->
                Ctx = #{
                    stream_id => <<"s2">>,
                    observer_uids => [11, 22, 33],
                    from => <<"agent1">>,
                    to => <<"group9">>,
                    type => <<"C2G">>
                },
                ok = llm_stream:flush_tail(Ctx),
                ?assertEqual(1, meck:num_calls(imboy_syn, publish, [11, '_'])),
                ?assertEqual(1, meck:num_calls(imboy_syn, publish, [22, '_'])),
                ?assertEqual(1, meck:num_calls(imboy_syn, publish, [33, '_'])),
                %% 一帧扇出 3 次
                ?assertEqual(3, meck:num_calls(imboy_syn, publish, '_'))
            end)
        end
    ).

%% 空观察者列表 → 不推（群内无人在线，不崩）
fanout_empty_observers_no_publish_test_() ->
    ?WITH_MECK(
        imboy_syn,
        [
            {'publish', 2, fun(_Uid, _Json) -> {ok, 1} end}
        ],
        fun() ->
            run_isolated(fun() ->
                Ctx = #{
                    stream_id => <<"s3">>,
                    observer_uids => [],
                    from => <<"a">>,
                    to => <<"g">>,
                    type => <<"C2G">>
                },
                ok = llm_stream:flush_tail(Ctx),
                ?assertEqual(0, meck:num_calls(imboy_syn, publish, '_'))
            end)
        end
    ).

%% 在独立进程跑闭包并等待完成（隔离 llm_stream 的进程字典状态）
run_isolated(Fun) ->
    Self = self(),
    Pid = spawn(fun() ->
        Fun(),
        Self ! {done, self()}
    end),
    receive
        {done, Pid} -> ok
    after 5000 ->
        error(timeout)
    end.
