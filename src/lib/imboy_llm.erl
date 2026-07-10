-module(imboy_llm).

%%% @doc LLM 提供商适配层 behaviour（BYO-LLM）
%%% 统一以 OpenAI 兼容的 Messages 格式对接不同大模型提供商，
%%% 具体实现见 imboy_llm_qianfan 等适配器模块。
%%% Messages 格式：[#{<<"role">> => binary(), <<"content">> => binary()}]
%%% @since 2026-07-09

-callback chat(Uid :: integer(), Messages :: [map()], Opts :: map()) ->
    {ok, #{binary() => term()}} | {error, term()}.

%% @doc 流式对话（可选）：每收到一个增量文本片段调一次 StreamFun，
%% 结束时返回定稿全文 #{<<"result">> => Full}。
%% 仅 capabilities().stream =:= true 的 provider 需实现（Phase 2）。
-callback chat_stream(
    Uid :: integer(),
    Messages :: [map()],
    Opts :: map(),
    StreamFun :: fun((binary()) -> ok)
) ->
    {ok, #{binary() => term()}} | {error, term()}.

-callback capabilities() ->
    #{stream := boolean(), vision := boolean(), tools := boolean()}.

-optional_callbacks([chat_stream/4]).
