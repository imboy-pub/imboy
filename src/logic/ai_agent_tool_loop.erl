-module(ai_agent_tool_loop).

%%%
% AGT-02：内建 Agent 受控 tool 执行循环（生产化，源自 AGT-01 spike）。
%
% Provider 模块（imboy_llm behaviour）发起对话；响应含 tool_calls 时经
% barrel_mcp_registry（authz gate + grant 白名单）执行并把结果回填，
% 直到定稿或轮数上限。负例全部有界终止（spike 矩阵语义）。
%
% 工具执行记录结构化日志（tool 名 + 耗时；不记参数正文与 secret，
% TRACE-00 §7）。
%%%

-export([run/5, applicable/2]).

-include("log.hrl").

-define(MAX_ROUNDS, 3).
-define(MAX_RESULT_BYTES, 8192).
-define(TOOL_TIMEOUT_MS, 5000).

%% @doc 执行 tool-loop。
%% ProviderMod：imboy_llm behaviour 模块；ToolCtx：注入工具执行上下文
%% （auth_info Principal 等，来自 MCP-01 认证链）；Opts.tools 非空时启用循环。
%% 返回 {ok, #{<<"content">> => Final, <<"rounds">> => N}} | {error, term()}。
-spec run(integer(), [map()], map(), module(), map()) ->
    {ok, map()} | {error, term()}.
%% @doc 是否走 tool-loop：Agent 配置了 tools 且 provider 支持 tools。
applicable(Agent, ProviderMod) ->
    Tools = maps:get(tools, Agent, maps:get(<<"tools">>, Agent, [])),

    case Tools of
        [] ->
            false;
        L when is_list(L), L =/= [] ->
            try ProviderMod:capabilities() of
                #{tools := true} -> true;
                _ -> false
            catch
                _:_ -> false
            end;
        _ ->
            false
    end.

run(Uid, Messages, Opts, ProviderMod, ToolCtx) ->
    Tools = maps:get(tools, Opts, []),
    case Tools of
        [] ->
            %% 无注册 tool：单轮直答（provider 不支持/未配置 tools 的降级路径）
            provider_round(ProviderMod, Uid, Messages, Opts);
        _ ->
            Opts2 = Opts#{tools => Tools},
            run_loop(Uid, Messages, Opts2, ProviderMod, ToolCtx, 1)
    end.

run_loop(_Uid, _Messages, _Opts, _ProviderMod, _ToolCtx, Round) when Round > ?MAX_ROUNDS ->
    {error, {max_rounds_exceeded, ?MAX_ROUNDS}};
run_loop(Uid, Messages, Opts, ProviderMod, ToolCtx, Round) ->
    case ProviderMod:chat(Uid, Messages, Opts) of
        {error, Reason} ->
            {error, {provider_error, Reason}};
        {ok, Resp} when is_map(Resp) ->
            case maps:find(<<"tool_calls">>, Resp) of
                {ok, ToolCalls} when is_list(ToolCalls), ToolCalls =/= [] ->
                    Msg = maps:get(<<"message">>, Resp, #{<<"role">> => <<"assistant">>}),
                    Executed = [exec_tool(TC, ToolCtx) || TC <- ToolCalls],
                    Messages2 = Messages ++ [Msg | Executed],
                    run_loop(Uid, Messages2, Opts, ProviderMod, ToolCtx, Round + 1);
                _ ->
                    Content = maps:get(<<"result">>, Resp, <<>>),
                    {ok, #{<<"content">> => Content, <<"rounds">> => Round}}
            end;
        {ok, Other} ->
            {error, {bad_provider_response, Other}};
        Other ->
            {error, {bad_provider_response, Other}}
    end.

provider_round(ProviderMod, Uid, Messages, Opts) ->
    case ProviderMod:chat(Uid, Messages, Opts) of
        {ok, #{<<"result">> := Content}} ->
            {ok, #{<<"content">> => Content, <<"rounds">> => 1}};
        {error, Reason} ->
            {error, {provider_error, Reason}};
        {ok, Other} ->
            {error, {bad_provider_response, Other}}
    end.

%% 单个 tool call：坏 JSON / 未知 tool / 超时 / 崩溃 → 错误消息回填（有界）
exec_tool(
    #{
        <<"id">> := Id,
        <<"function">> := #{
            <<"name">> := Name,
            <<"arguments">> := ArgsJson
        }
    },
    ToolCtx
) ->
    case safe_decode(ArgsJson) of
        {error, _} ->
            tool_msg(Id, <<"[agent] arguments is not valid JSON">>);
        Args ->
            RequestId = erlang:unique_integer([positive]),
            Ctx = ToolCtx#{reply_to => self(), request_id => RequestId},
            T0 = erlang:monotonic_time(millisecond),
            Res =
                case barrel_mcp_registry:run_tool(Name, Args, Ctx) of
                    {ok, _} ->
                        %% 异步注册成功：等 worker 回投结果
                        receive
                            {tool_result, RequestId, Result} ->
                                {ok, truncate(result_text(Result), ?MAX_RESULT_BYTES)};
                            {tool_failed, RequestId, internal_error} ->
                                {error, tool_internal_error}
                        after ?TOOL_TIMEOUT_MS ->
                            {error, tool_timeout}
                        end;
                    {error, Reason} ->
                        %% 未知 tool / 校验失败等同步错误：回填错误消息（不等待）
                        {error, Reason}
                end,
            Lat = erlang:monotonic_time(millisecond) - T0,
            ?DEBUG_LOG(
                "[AGENT_TOOL_LOOP] tool=~ts latency=~pms ok=~p~n",
                [Name, Lat, element(1, Res) =:= ok]
            ),
            finish_tool(Id, Res)
    end;
exec_tool(TC, _ToolCtx) ->
    #{
        <<"role">> => <<"tool">>,
        <<"content">> => truncate(iolist_to_binary(io_lib:format("~p", [TC])), 200)
    }.

finish_tool(Id, {ok, Text}) ->
    tool_msg(Id, Text);
finish_tool(Id, {error, Reason}) ->
    tool_msg(
        Id,
        iolist_to_binary(
            [
                "[agent] tool error: ",
                truncate(iolist_to_binary(io_lib:format("~p", [Reason])), 200)
            ]
        )
    ).

tool_msg(Id, Content) ->
    #{<<"role">> => <<"tool">>, <<"tool_call_id">> => Id, <<"content">> => Content}.

result_text(Result) when is_binary(Result) -> Result;
result_text(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).

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
