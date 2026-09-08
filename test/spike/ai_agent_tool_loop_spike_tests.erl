-module(ai_agent_tool_loop_spike_tests).
-include_lib("eunit/include/eunit.hrl").

%%% AGT-01：tool-loop spike 负例矩阵（fake provider，无真实 LLM/网络）。
%%% 全部负例有界终止；正例走一轮 tool call + 定稿。

%% fake provider：脚本队列——依次返回 tool_calls 轮，最后返回定稿
fake_provider(CallsPerRound, FinalContent) ->
    Counter = erlang:unique_integer([positive]),
    ets:new(Counter, [named_table, public, set]),
    ets:insert(Counter, {round, 0}),
    fun(_Uid, _Messages, _Opts) ->
        [{round, N}] = ets:lookup(Counter, round),
        ets:insert(Counter, {round, N + 1}),
        case N < CallsPerRound of
            true ->
                Calls = [
                    #{
                        <<"id">> => iolist_to_binary(["c", integer_to_binary(N), "-", I]),
                        <<"function">> => #{
                            <<"name">> => Name,
                            <<"arguments">> => ArgsJson
                        }
                    }
                 || {I, Name, ArgsJson} <- elements(N)
                ],
                {ok, tool_resp(Calls)};
            false ->
                {ok, final_resp(<<"final answer">>)}
        end
    end.

elements(N) ->
    %% 每轮的 tool calls（负例矩阵按需覆盖）
    [{1, <<"echo">>, <<"{\"text\":\"x\"}">>}].

tool_resp(Calls) ->
    #{
        <<"choices">> => [
            #{
                <<"message">> => #{
                    <<"role">> => <<"assistant">>, <<"tool_calls">> => Calls
                }
            }
        ]
    }.

final_resp(Content) ->
    #{
        <<"choices">> => [
            #{
                <<"message">> => #{
                    <<"role">> => <<"assistant">>, <<"content">> => Content
                }
            }
        ]
    }.

ok_sender(_N, _A, _C) -> {ok, <<"[spike] ok">>}.
err_sender(_N, _A, _C) -> {error, boom}.
counting_sender(Tab) ->
    fun(_N, _A, _C) ->
        ets:update_counter(Tab, calls, 1, {calls, 0}),
        {ok, <<"[spike] awaiting_approval">>}
    end.

%% 负例：坏 JSON 参数 → 错误消息回填，循环有界终止
bad_json_terminated_test() ->
    Provider = fun(_U, M, _O) ->
        case length(M) of
            0 ->
                {ok,
                    tool_resp([
                        #{
                            <<"id">> => <<"c1">>,
                            <<"function">> => #{
                                <<"name">> => <<"any">>,
                                <<"arguments">> => <<"{bad">>
                            }
                        }
                    ])};
            _ ->
                {ok, final_resp(<<"done">>)}
        end
    end,
    R = ai_agent_tool_loop_spike:run(1, [], #{}, #{
        provider => Provider,
        tool_fun => fun ok_sender/3
    }),
    ?assertMatch({ok, _}, R).

%% 负例：未知 tool → 错误回填（tool_fun 返回 error），有界终止
unknown_tool_terminated_test() ->
    Provider = fun(_U, M, _O) ->
        case length(M) of
            0 ->
                {ok,
                    tool_resp([
                        #{
                            <<"id">> => <<"c2">>,
                            <<"function">> => #{
                                <<"name">> => <<"no_such_tool">>,
                                <<"arguments">> => <<"{}">>
                            }
                        }
                    ])};
            _ ->
                {ok, final_resp(<<"done">>)}
        end
    end,
    R = ai_agent_tool_loop_spike:run(1, [], #{}, #{
        provider => Provider,
        tool_fun => fun err_sender/3
    }),
    ?assertMatch({ok, _}, R).

%% 负例：provider 恒 tool_calls（递归）→ 轮数上限终止
recursive_tool_terminated_test() ->
    Infinite = fun(_U, _M, _O) ->
        {ok,
            tool_resp([
                #{
                    <<"id">> => <<"cx">>,
                    <<"function">> => #{
                        <<"name">> => <<"echo">>,
                        <<"arguments">> => <<"{}">>
                    }
                }
            ])}
    end,
    R = ai_agent_tool_loop_spike:run(
        1,
        [],
        #{},
        #{provider => Infinite, tool_fun => fun ok_sender/3}
    ),
    ?assertMatch({error, {max_rounds_exceeded, 3}}, R).

%% 负例：provider 坏响应形态 / provider error
bad_provider_shapes_test() ->
    ?assertMatch(
        {error, {bad_provider_response, _}},
        ai_agent_tool_loop_spike:run(
            1,
            [],
            #{},
            #{
                provider => fun(_U, _M, _O) -> {ok, #{<<"junk">> => 1}} end,
                tool_fun => fun ok_sender/3
            }
        )
    ),
    ?assertMatch(
        {error, {provider_error, _}},
        ai_agent_tool_loop_spike:run(
            1,
            [],
            #{},
            #{
                provider => fun(_U, _M, _O) -> {error, backend_down} end,
                tool_fun => fun ok_sender/3
            }
        )
    ).

%% 正例：一轮 tool call + 定稿（rounds=2）
positive_loop_test() ->
    Provider = fun(_U, M, _O) ->
        case length(M) of
            0 ->
                {ok,
                    tool_resp([
                        #{
                            <<"id">> => <<"c1">>,
                            <<"function">> => #{
                                <<"name">> => <<"echo">>,
                                <<"arguments">> => <<"{\"a\":1}">>
                            }
                        }
                    ])};
            _ ->
                {ok, final_resp(<<"final answer">>)}
        end
    end,
    R = ai_agent_tool_loop_spike:run(1, [], #{}, #{
        provider => Provider,
        tool_fun => fun ok_sender/3
    }),
    ?assertMatch({ok, #{<<"rounds">> := 2, <<"content">> := <<"final answer">>}}, R).

%% 正例：超大结果截断（>8192 字节回填被截断，不挂死）
oversized_result_truncated_test() ->
    Big = binary:copy(<<"x">>, 20000),
    Provider = fun(_U, M, _O) ->
        case length(M) of
            0 ->
                {ok,
                    tool_resp([
                        #{
                            <<"id">> => <<"c1">>,
                            <<"function">> => #{
                                <<"name">> => <<"big">>,
                                <<"arguments">> => <<"{}">>
                            }
                        }
                    ])};
            _ ->
                {ok, final_resp(<<"done">>)}
        end
    end,
    ToolFun = fun(_N, _A, _C) -> {ok, Big} end,
    R = ai_agent_tool_loop_spike:run(1, [], #{}, #{
        provider => Provider,
        tool_fun => ToolFun
    }),
    ?assertMatch({ok, _}, R).
