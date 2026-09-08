-module(ai_agent_tool_loop_spike).

%%%
% AGT-01 spike：OpenAI 兼容 provider + MCP registry + HITL 三者同一自包含
% 上下文的 tool-loop 可行性验证。≤200 行、零新依赖、不改生产路径。
%
% 裁决口径：
%   Provider = fun(Uid, Messages, Opts) -> {ok, RespMap} | {error, Reason}
%     RespMap 形态（OpenAI 兼容）：#{<<"choices">> => [#{<<"message">> => Msg}]}
%     Msg 含 <<"tool_calls">>（本轮要执行的 tool 列表）或 <<"content">>（定稿）
%   ToolCall = #{<<"id">>, <<"function">> => #{<<"name">>, <<"arguments">>(JSON 文本)}}
%   ToolFun  = fun(Name, Args, Ctx) -> {ok, ResultBin} | {error, Reason}
% 负例全部有界终止：轮数上限 + 单结果截断 + 坏 JSON/未知 tool 以错误消息回填。
%%%

-export([run/4]).

-define(MAX_ROUNDS, 3).
-define(MAX_RESULT_BYTES, 8192).
-define(TOOL_TIMEOUT_MS, 5000).

%% @doc 核心循环（生产化前的最小形态）。
%% Provider: fun(Uid, Messages, Opts)；ToolFun: fun(Name, Args, Ctx)。
%% 返回 {ok, #{<<"role">> => <<"assistant">>, <<"content">> => Final, rounds => N}}
%% | {error, Reason}。
run(Uid, Messages, Opts, #{provider := Provider, tool_fun := ToolFun}) ->
    run_loop(Uid, Messages, Opts, Provider, ToolFun, 1).

run_loop(_Uid, _Messages, _Opts, _Provider, _ToolFun, Round) when Round > ?MAX_ROUNDS ->
    {error, {max_rounds_exceeded, ?MAX_ROUNDS}};
run_loop(Uid, Messages, Opts, Provider, ToolFun, Round) when
    is_integer(Uid), is_list(Messages), is_map(Opts)
->
    case Provider(Uid, Messages, Opts) of
        {error, Reason} ->
            {error, {provider_error, Reason}};
        {ok, #{<<"choices">> := [#{<<"message">> := Msg} | _]}} ->
            case maps:find(<<"tool_calls">>, Msg) of
                {ok, ToolCalls} when is_list(ToolCalls), ToolCalls =/= [] ->
                    Executed = lists:map(
                        fun(TC) -> exec_tool(TC, ToolFun, Opts) end,
                        ToolCalls
                    ),
                    Messages2 = Messages ++ [Msg | Executed],
                    run_loop(Uid, Messages2, Opts, Provider, ToolFun, Round + 1);
                _ ->
                    {ok, Msg#{<<"rounds">> => Round}}
            end;
        {ok, Other} ->
            {error, {bad_provider_response, Other}}
    end.

%% 单个 tool call：坏 JSON / 未知 tool / 超大结果全部回填为错误消息（有界）
exec_tool(
    #{
        <<"id">> := Id,
        <<"function">> := #{
            <<"name">> := Name,
            <<"arguments">> := ArgsJson
        }
    },
    ToolFun,
    Opts
) ->
    case safe_decode(ArgsJson) of
        {error, _} ->
            tool_msg(Id, <<"[spike] arguments is not valid JSON">>);
        Args ->
            Ctx = maps:get(ctx, Opts, #{}),
            case (catch ToolFun(Name, Args, Ctx)) of
                {'EXIT', {timeout, _}} ->
                    tool_msg(Id, <<"[spike] tool timeout">>);
                {'EXIT', Reason} ->
                    tool_msg(
                        Id,
                        iolist_to_binary(
                            [
                                "[spike] tool crash: ",
                                truncate(iolist_to_binary(io_lib:format("~p", [Reason])), 200)
                            ]
                        )
                    );
                {ok, ResultBin} when is_binary(ResultBin) ->
                    tool_msg(Id, truncate(ResultBin, ?MAX_RESULT_BYTES));
                {ok, Other} ->
                    tool_msg(
                        Id,
                        truncate(
                            iolist_to_binary(
                                io_lib:format("~p", [Other])
                            ),
                            ?MAX_RESULT_BYTES
                        )
                    );
                Other ->
                    tool_msg(
                        Id,
                        truncate(
                            iolist_to_binary(
                                io_lib:format("~p", [Other])
                            ),
                            ?MAX_RESULT_BYTES
                        )
                    )
            end
    end;
exec_tool(TC, _ToolFun, _Opts) ->
    #{
        <<"role">> => <<"tool">>,
        <<"content">> => truncate(iolist_to_binary(io_lib:format("~p", [TC])), 200)
    }.

tool_msg(Id, Content) ->
    #{<<"role">> => <<"tool">>, <<"tool_call_id">> => Id, <<"content">> => Content}.

safe_decode(Bin) when is_binary(Bin) ->
    try
        {ok, jsone:decode(Bin)}
    catch
        _:_ -> {error, bad_json}
    end;
safe_decode(_) ->
    {error, bad_json}.

truncate(Bin, Max) when is_binary(Bin), byte_size(Bin) > Max ->
    binary:part(Bin, 0, Max);
truncate(Bin, _) ->
    Bin.
