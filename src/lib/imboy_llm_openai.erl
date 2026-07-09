-module(imboy_llm_openai).

%%% @doc imboy_llm behaviour 的 OpenAI 兼容适配器
%%% 覆盖所有 OpenAI Chat Completions 兼容服务：DeepSeek/Qwen/vLLM/Ollama 等。
%%% base_url/api_key/model 由 Opts 传入（见 imboy_llm_registry 的 llm_providers 配置）。
%%% @since 2026-07-09

-behaviour(imboy_llm).

-export([chat/3]).
-export([capabilities/0]).

%% @doc 发起对话：POST {base_url}/chat/completions
%% @param Uid 用户ID（OpenAI 兼容通路暂未使用）
%% @param Messages OpenAI 兼容消息列表
%% @param Opts 必含 base_url、api_key、model
-spec chat(integer(), [map()], map()) ->
    {ok, #{binary() => term()}} | {error, term()}.
chat(_Uid, [], _Opts) ->
    {error, empty_messages};
chat(Uid, Messages, Opts) ->
    try
        do_chat(Uid, Messages, Opts)
    catch
        % 缺 base_url/api_key/model 等配置错误（badkey）在此兜住
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc OpenAI 兼容通路当前能力
-spec capabilities() -> #{stream := boolean(), vision := boolean(), tools := boolean()}.
capabilities() ->
    #{stream => false, vision => false, tools => false}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

do_chat(_Uid, Messages, Opts) ->
    BaseUrl = maps:get(base_url, Opts),
    ApiKey = maps:get(api_key, Opts),
    Model = maps:get(model, Opts),
    Headers = [
        {"Content-Type", "application/json"},
        {"Authorization", "Bearer " ++ ec_cnv:to_list(ApiKey)}
    ],
    Data = #{<<"model">> => Model, <<"messages">> => Messages},
    case elib_req:post(url(BaseUrl), Data, Headers) of
        {ok, #{<<"choices">> := [#{<<"message">> := #{<<"content">> := Content}} | _]}} ->
            {ok, #{<<"result">> => Content}};
        {ok, RespMap} ->
            % 无 choices 的错误响应（如 #{<<"error">> => ...}）
            {error, RespMap};
        {error, Code, RespMap} ->
            {error, {Code, RespMap}};
        {error, Reason} ->
            {error, Reason}
    end.

url(BaseUrl) ->
    case binary:last(BaseUrl) of
        $/ -> <<BaseUrl/binary, "chat/completions">>;
        _ -> <<BaseUrl/binary, "/chat/completions">>
    end.
