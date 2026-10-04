-module(imboy_llm_openai).

-moduledoc "imboy_llm behaviour 的 OpenAI 兼容适配器 —— 覆盖 DeepSeek/Qwen/vLLM/Ollama 等 Chat Completions 兼容服务。".
%%% @doc imboy_llm behaviour 的 OpenAI 兼容适配器
%%% 覆盖所有 OpenAI Chat Completions 兼容服务：DeepSeek/Qwen/vLLM/Ollama 等。
%%% base_url/api_key/model 由 Opts 传入（见 imboy_llm_registry 的 llm_providers 配置）。
%%% @since 2026-07-09

-behaviour(imboy_llm).

-include("log.hrl").

-export([chat/3]).
-export([chat_stream/4]).
-export([capabilities/0]).
%% 导出 SSE 解析纯函数供 EUnit 直接测试（httpc 流集成走真机验证）
-export([parse_sse/1]).
%% 导出 stream_loop 供 EUnit 进程消息注入测试
-export([stream_loop/5]).

%% 相邻片段间隔超时（两次 stream 消息之间的最长等待）
-define(STREAM_TIMEOUT, 60000).
%% 整个流的总时长上限（防被小流量持续喂养绕过间隔超时）
-define(STREAM_TOTAL_TIMEOUT, 120000).
%% SSE 缓冲 / 累积全文的字节上限（防恶意/故障 provider 无界内存）
-define(STREAM_MAX_BYTES, 262144).

%% 非流式 chat/3 的单次请求超时缺省值（毫秒），与 elib_req 的全局默认一致。
%% provider 可用 Opts.timeout 覆盖（见 llm_providers 配置的 timeout 键）。
-define(DEFAULT_TIMEOUT_MS, 30000).

%% @doc 发起对话：POST {base_url}/chat/completions
%% @param Uid 用户ID（OpenAI 兼容通路暂未使用）
%% @param Messages OpenAI 兼容消息列表
%% @param Opts 必含 base_url、api_key、model；可选 timeout（毫秒）
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

%% @doc 流式对话：POST {base_url}/chat/completions with stream=true
%% 用 stdlib httpc streaming（{sync,false},{stream,self}），逐片解析 SSE，
%% 每个 delta.content 调 StreamFun/1，结束返回定稿全文 #{<<"result">> => Full}。
-spec chat_stream(integer(), [map()], map(), fun((binary()) -> ok)) ->
    {ok, #{binary() => term()}} | {error, term()}.
chat_stream(_Uid, [], _Opts, _StreamFun) ->
    {error, empty_messages};
chat_stream(Uid, Messages, Opts, StreamFun) when is_function(StreamFun, 1) ->
    try
        do_chat_stream(Uid, Messages, Opts, StreamFun)
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc OpenAI 兼容通路当前能力（stream=true：支持流式）
-spec capabilities() -> #{stream := boolean(), vision := boolean(), tools := boolean()}.
capabilities() ->
    #{stream => true, vision => false, tools => false}.

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
    Data0 = #{<<"model">> => Model, <<"messages">> => Messages},
    %% extra_body：provider 级额外请求参数（如智谱 thinking 开关），顶层合并。
    Data =
        case maps:get(extra_body, Opts, undefined) of
            EB when is_map(EB) -> maps:merge(Data0, EB);
            _ -> Data0
        end,
    case elib_req:post(url(BaseUrl), Data, Headers, timeout_ms(Opts)) of
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

%% @doc 单次请求超时（毫秒）：provider 配置的 Opts.timeout 优先，回落 30s。
%%
%% 为什么必须可配：elib_req 的 30s 是**对所有 HTTP 调用**都成立的保守默认，
%% 对纯文本接口够用，但对「视频理解 + 始终思考」的多模态模型会在长尾上偶发
%% 截断（-> dispatch_failure(timeout) -> 前端 error_code=timeout）。实测
%% glm-5.3-flash 跑完一段回课视频耗时 21.7s，30s 只余不到 1.4x 余量。
%%
%% ⚠️ 调大本值时务必同步改前端轮询预算，两侧是**同一个最坏路径**的两半：
%%     服务端最坏 = Opts.timeout × moya_ai_worker 的最大尝试次数
%%     前端预算（moya/src/packages/teacher/workbench/workbench.ts）
%% 只看一侧会重现「前端先放弃、后端还在跑」——老师看到的是一屏不变的
%% 「AI 正在看这份作业」，而任务其实成了。
-spec timeout_ms(map()) -> pos_integer().
timeout_ms(Opts) when is_map(Opts) ->
    case maps:get(timeout, Opts, undefined) of
        T when is_integer(T), T > 0 -> T;
        _ -> ?DEFAULT_TIMEOUT_MS
    end.

url(BaseUrl) ->
    case binary:last(BaseUrl) of
        $/ -> <<BaseUrl/binary, "chat/completions">>;
        _ -> <<BaseUrl/binary, "/chat/completions">>
    end.

%% ------------------- 流式实现 -------------------

do_chat_stream(_Uid, Messages, Opts, StreamFun) ->
    _ = application:ensure_started(ssl),
    _ = application:ensure_started(inets),
    BaseUrl = maps:get(base_url, Opts),
    ApiKey = maps:get(api_key, Opts),
    Model = maps:get(model, Opts),
    Headers = [
        {"Authorization", "Bearer " ++ ec_cnv:to_list(ApiKey)}
    ],
    Data0 = #{<<"model">> => Model, <<"messages">> => Messages, <<"stream">> => true},
    Data = maybe_max_tokens(Data0, Opts),
    Body = jsone:encode(Data, [native_utf8]),
    Url = ec_cnv:to_list(url(BaseUrl)),
    Request = {Url, Headers, "application/json", Body},
    case
        httpc:request(
            post,
            Request,
            [{timeout, ?STREAM_TIMEOUT}],
            [{sync, false}, {stream, self}, {body_format, binary}]
        )
    of
        {ok, ReqId} ->
            Deadline = erlang:monotonic_time(millisecond) + ?STREAM_TOTAL_TIMEOUT,
            stream_loop(ReqId, <<>>, <<>>, StreamFun, Deadline);
        {error, Reason} ->
            {error, Reason}
    end.

%% max_tokens 可选：Opts 提供整数则加入请求体，给 provider 一个生成长度软上限
maybe_max_tokens(Data, Opts) ->
    case maps:get(max_tokens, Opts, undefined) of
        N when is_integer(N), N > 0 -> Data#{<<"max_tokens">> => N};
        _ -> Data
    end.

%% @doc 接收 httpc streaming 消息，累积 SSE buffer，逐 delta 调 StreamFun，累加定稿全文。
%% Deadline 为流总时长上限（monotonic ms）；缓冲/累积超字节上限即中止，
%% 防被配置的 provider 异常（小流量持续喂养 / 超大不闭合单行）耗尽内存。
%% 导出供 EUnit 进程消息注入测试。
stream_loop(ReqId, Buf, Acc, StreamFun, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= Deadline of
        true ->
            _ = httpc:cancel_request(ReqId),
            {error, stream_total_timeout};
        false ->
            Wait = min(?STREAM_TIMEOUT, Deadline - Now),
            receive
                {http, {ReqId, stream_start, _RespHeaders}} ->
                    stream_loop(ReqId, Buf, Acc, StreamFun, Deadline);
                {http, {ReqId, stream, BinPart}} ->
                    {Deltas, Buf2} = parse_sse(<<Buf/binary, BinPart/binary>>),
                    Acc2 = emit_deltas(Deltas, Acc, StreamFun),
                    case over_limit(Buf2, Acc2) of
                        true ->
                            _ = httpc:cancel_request(ReqId),
                            ok = ?WARN_LOG(
                                {llm_stream_too_large, byte_size(Buf2), byte_size(Acc2)}
                            ),
                            {error, response_too_large};
                        false ->
                            stream_loop(ReqId, Buf2, Acc2, StreamFun, Deadline)
                    end;
                {http, {ReqId, stream_end, _RespHeaders}} ->
                    %% flush 残留 buffer（末尾可能有未随 \n\n 结束的完整 event）
                    {Deltas, Rest} = parse_sse(Buf),
                    Rest =/= <<>> andalso
                        ?WARN_LOG({llm_stream_trailing_discarded, byte_size(Rest)}),
                    Acc2 = emit_deltas(Deltas, Acc, StreamFun),
                    {ok, #{<<"result">> => Acc2}};
                {http, {ReqId, {error, Reason}}} ->
                    {error, Reason};
                {http, {ReqId, {{_, StatusCode, _}, _RespHeaders, RespBody}}} ->
                    %% 非 200：httpc 未进入流式，返回整体响应
                    {error, {StatusCode, RespBody}}
            after Wait ->
                _ = httpc:cancel_request(ReqId),
                {error, stream_timeout}
            end
    end.

over_limit(Buf, Acc) ->
    byte_size(Buf) > ?STREAM_MAX_BYTES orelse byte_size(Acc) > ?STREAM_MAX_BYTES.

emit_deltas(Deltas, Acc, StreamFun) ->
    lists:foldl(
        fun(D, A) ->
            _ = StreamFun(D),
            <<A/binary, D/binary>>
        end,
        Acc,
        Deltas
    ).

%% @doc 从 SSE buffer 抽出所有完整 event 的 delta 文本，返回 {Deltas, RestBuffer}
%% OpenAI SSE：event 以 "\n\n" 分隔，每行 "data: {json}" 或 "data: [DONE]"。
%% 不完整的尾部（无 "\n\n"）留在 RestBuffer 等待下一片。
-spec parse_sse(binary()) -> {[binary()], binary()}.
parse_sse(Buffer) ->
    parse_sse(Buffer, []).

parse_sse(Buffer, Acc) ->
    case binary:split(Buffer, <<"\n\n">>) of
        [_Incomplete] ->
            {lists:reverse(Acc), Buffer};
        [Event, Rest] ->
            case parse_event(Event) of
                skip -> parse_sse(Rest, Acc);
                done -> {lists:reverse(Acc), <<>>};
                {delta, D} -> parse_sse(Rest, [D | Acc])
            end
    end.

%% 解析单个 event block（可能多行），取首个含 content 的 data 行
parse_event(Event) ->
    Lines = binary:split(Event, <<"\n">>, [global]),
    scan_data_lines(Lines).

scan_data_lines([]) ->
    skip;
scan_data_lines([Line | Rest]) ->
    case data_payload(Line) of
        none ->
            scan_data_lines(Rest);
        <<"[DONE]">> ->
            done;
        Json ->
            case decode_delta(Json) of
                <<>> -> scan_data_lines(Rest);
                Content -> {delta, Content}
            end
    end.

%% 取 "data:" 行的 payload（去前后空白）；非 data 行返回 none
data_payload(Line) ->
    case string:trim(Line) of
        <<"data:", Rest/binary>> -> string:trim(Rest);
        _ -> none
    end.

decode_delta(Json) ->
    try jsone:decode(Json) of
        #{<<"choices">> := [#{<<"delta">> := #{<<"content">> := C}} | _]} when is_binary(C) ->
            C;
        _ ->
            <<>>
    catch
        _:_ -> <<>>
    end.
