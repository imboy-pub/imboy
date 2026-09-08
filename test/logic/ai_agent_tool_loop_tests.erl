-module(ai_agent_tool_loop_tests).
-include_lib("eunit/include/eunit.hrl").

%% chat/3 经 ProviderMod:chat 动态调用，须显式导出（否则 unused-function 警告在
%% warnings-as-errors 下变为编译失败）
-export([chat/3]).

%%% AGT-02：生产 tool-loop 单元测试（fake provider = 本模块 chat/3，无网络）。
%%% 覆盖：无 tools 降级单轮 / tool_calls 执行与错误回填 / 轮数上限有界终止。

%% fake provider：imboy_llm chat/3 形态；脚本队列存本模块 ETS 表
chat(_Uid, _Messages, Opts) ->
    Tab = maps:get(script_tab, Opts),
    case ets:lookup(Tab, queue) of
        [{queue, [{tool_calls, Calls} | Rest]}] ->
            ets:insert(Tab, {queue, Rest}),
            Msg = #{
                <<"role">> => <<"assistant">>,
                <<"tool_calls">> => Calls,
                <<"content">> => <<>>
            },
            {ok,
                maps:merge(
                    #{<<"result">> => <<>>},
                    #{<<"tool_calls">> => Calls, <<"message">> => Msg}
                )};
        [{queue, [{final, Content} | Rest]}] ->
            ets:insert(Tab, {queue, Rest}),
            {ok, #{<<"result">> => Content}};
        [{queue, []}] ->
            {ok, #{<<"result">> => <<"fallback">>}};
        _ ->
            {ok, #{<<"result">> => <<"fallback">>}}
    end.

run_no_tools_single_round_test() ->
    Tab = ets:new(fps, [public, set]),
    ets:insert(Tab, {queue, [{final, <<"plain">>}]}),
    R = ai_agent_tool_loop:run(1, [], #{script_tab => Tab}, ?MODULE, #{}),
    ?assertMatch({ok, #{<<"content">> := <<"plain">>, <<"rounds">> := 1}}, R),
    ets:delete(Tab).

run_unknown_tool_bounded_test() ->
    Tab = ets:new(fps, [public, set]),
    ets:insert(
        Tab,
        {queue, [
            {tool_calls, [
                #{
                    <<"id">> => <<"c1">>,
                    <<"function">> => #{
                        <<"name">> => <<"no_such_tool_xyz">>,
                        <<"arguments">> => <<"{}">>
                    }
                }
            ]},
            {final, <<"done">>}
        ]}
    ),
    Opts = #{script_tab => Tab, tools => [<<"no_such_tool_xyz">>]},
    R = ai_agent_tool_loop:run(1, [], Opts, ?MODULE, #{}),
    ?assertMatch({ok, #{<<"rounds">> := 2, <<"content">> := <<"done">>}}, R),
    ets:delete(Tab).

run_recursive_max_rounds_test() ->
    Tab = ets:new(fps, [public, set]),
    TC = fun(I) ->
        #{
            <<"id">> => iolist_to_binary(["cx", integer_to_binary(I)]),
            <<"function">> => #{
                <<"name">> => <<"no_such_tool_xyz">>,
                <<"arguments">> => <<"{}">>
            }
        }
    end,
    ets:insert(Tab, {queue, [{tool_calls, [TC(I)]} || I <- lists:seq(1, 10)]}),
    Opts = #{script_tab => Tab, tools => [<<"no_such_tool_xyz">>]},
    R = ai_agent_tool_loop:run(1, [], Opts, ?MODULE, #{}),
    ?assertMatch({error, {max_rounds_exceeded, 3}}, R),
    ets:delete(Tab).
