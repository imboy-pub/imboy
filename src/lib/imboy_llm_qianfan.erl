-module(imboy_llm_qianfan).

%%% @doc imboy_llm behaviour 的百度千帆适配器
%%% 委托现有 qianfan_api:create_chat/3，把 OpenAI 兼容 Messages
%%% 映射为 (Content, History)：末条消息 content → Content，其余 → History。
%%% @since 2026-07-09

-behaviour(imboy_llm).

-export([chat/3]).
-export([capabilities/0]).

%% @doc 发起对话
%% @param Uid 用户ID
%% @param Messages OpenAI 兼容消息列表，末条应为 user 消息
%% @param Opts 预留选项（千帆通路暂未使用）
-spec chat(integer(), [map()], map()) ->
    {ok, #{binary() => term()}} | {error, term()}.
chat(_Uid, [], _Opts) ->
    {error, empty_messages};
chat(Uid, Messages, _Opts) ->
    [Last | RevHistory] = lists:reverse(Messages),
    Content = maps:get(<<"content">>, Last, <<>>),
    History = lists:reverse(RevHistory),
    try qianfan_api:create_chat(Uid, Content, History) of
        #{<<"result">> := _} = RespMap ->
            {ok, RespMap};
        RespMap ->
            % 千帆错误响应无 result 字段（如 error_code/error_msg）
            {error, RespMap}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc 千帆 completions_pro 通路当前能力
-spec capabilities() -> #{stream := boolean(), vision := boolean(), tools := boolean()}.
capabilities() ->
    #{stream => false, vision => false, tools => false}.
