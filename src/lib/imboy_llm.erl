-module(imboy_llm).

%%% @doc LLM 提供商适配层 behaviour（BYO-LLM）
%%% 统一以 OpenAI 兼容的 Messages 格式对接不同大模型提供商，
%%% 具体实现见 imboy_llm_qianfan 等适配器模块。
%%% Messages 格式：[#{<<"role">> => binary(), <<"content">> => binary()}]
%%% @since 2026-07-09

-callback chat(Uid :: integer(), Messages :: [map()], Opts :: map()) ->
    {ok, #{binary() => term()}} | {error, term()}.

-callback capabilities() ->
    #{stream := boolean(), vision := boolean(), tools := boolean()}.
