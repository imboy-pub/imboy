-module(ai_agent_reply).

%%%
% Agent C2C 回复分派 / Agent C2C reply dispatch（Phase 1 T1.4）
%
% 复用 msg_c2s_logic:c2s_to_external 的「异步调 LLM → 投递响应」骨架思路，
% 但作为 msg_c2c_logic:c2c/3 的旁路：人给 agent 发私聊 → 触发 LLM → 回投 agent→human。
% 核心通道只在 allow 分支尾部 fire-and-forget 调用 maybe_dispatch/3，不改变原返回值。
%
% E2EE 红线：服务端绝不解密 ciphertext 做 AI。E2EE 消息一律跳过（不触发）。
%%%

-export([maybe_dispatch/3]).
%% 供内部 async 闭包与测试调用
-export([run_and_reply/5]).

-include("log.hrl").

%% @doc C2C 旁路：若 ToId 是启用中的 agent 且消息为非 E2EE 文本，异步触发 LLM 回复。
%% 对普通 C2C（非 agent / E2EE / 无文本）无副作用，恒返回 ok（fire-and-forget）。
-spec maybe_dispatch(integer(), integer(), map()) -> ok.
maybe_dispatch(FromUid, ToId, Data) ->
    %% 故障隔离：agent 回复旁路的任何异常绝不能拖垮正常 C2C 投递。
    try
        do_maybe_dispatch(FromUid, ToId, Data)
    catch
        Class:Reason ->
            ok = ?ERROR_LOG("[AGENT_REPLY_GUARD] to=~p ~p:~p~n", [ToId, Class, Reason]),
            ok
    end.

-spec do_maybe_dispatch(integer(), integer(), map()) -> ok.
do_maybe_dispatch(FromUid, ToId, Data) ->
    case ai_agent_prompt:is_e2ee(Data) of
        true ->
            % E2EE 红线：不解密、不做 AI
            ok;
        false ->
            case ai_agent_prompt:extract_text(Data) of
                <<>> ->
                    % 非文本（图片/文件/位置等）不触发
                    ok;
                Text ->
                    case ai_agent_ds:is_agent(ToId) of
                        {true, Agent} ->
                            dispatch(FromUid, ToId, Agent, Text);
                        false ->
                            ok
                    end
            end
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec dispatch(integer(), integer(), map(), binary()) -> ok.
dispatch(FromUid, ToId, Agent, Text) ->
    %% 金钱 DoS 闸门：LLM 调用前限流（唯一 choke point，C2G 接线后走同一闸门）。
    %% deny 时静默丢弃——不回复、不花钱、不给攻击者信号。
    case agent_rate_limiter:allow(ToId, FromUid) of
        allow ->
            do_dispatch(FromUid, ToId, Agent, Text);
        {deny, Reason} ->
            ok = ?WARN_LOG("[AGENT_RATE_LIMITED] agent=~p from=~p reason=~p~n", [
                ToId, FromUid, Reason
            ]),
            ok
    end.

-spec do_dispatch(integer(), integer(), map(), binary()) -> ok.
do_dispatch(FromUid, ToId, Agent, Text) ->
    Provider = maps:get(<<"provider">>, Agent, <<>>),
    case imboy_llm_registry:lookup(Provider) of
        {ok, #{module := Mod, opts := Opts0}} ->
            Opts = ai_agent_prompt:merge_model(Opts0, Agent),
            Messages = ai_agent_prompt:build_messages(Agent, Text),
            % 异步：不阻塞发送者的 C2C 处理
            elib_async:async(fun() ->
                ?MODULE:run_and_reply(Mod, Opts, FromUid, ToId, Messages)
            end),
            ok;
        undefined ->
            ok = ?ERROR_LOG("[AGENT_REPLY] provider not found: ~p~n", [Provider]),
            ok
    end.

%% @doc 调 LLM 并把结果作为 C2C 从 agent 回投给 human（async 闭包体，独立导出便于测试）
%% 流式开关（llm_stream_enabled）+ provider 支持流式时走逐字流式，否则一次性回复。
-spec run_and_reply(module(), map(), integer(), integer(), [map()]) -> ok.
run_and_reply(Mod, Opts, FromUid, ToId, Messages) ->
    case llm_stream:stream_capable(Mod) of
        true ->
            run_and_reply_stream(Mod, Opts, FromUid, ToId, Messages);
        false ->
            run_and_reply_sync(Mod, Opts, FromUid, ToId, Messages)
    end.

%% 一次性回复（原路径）
-spec run_and_reply_sync(module(), map(), integer(), integer(), [map()]) -> ok.
run_and_reply_sync(Mod, Opts, FromUid, ToId, Messages) ->
    case Mod:chat(ToId, Messages, Opts) of
        {ok, RespMap} when is_map(RespMap) ->
            Result = maps:get(<<"result">>, RespMap, <<>>),
            deliver_reply(FromUid, ToId, Result);
        {error, Reason} ->
            ok = ?ERROR_LOG("[AGENT_REPLY_FAILED] to=~p reason=~p~n", [ToId, Reason]),
            ok
    end.

%% 流式回复：逐个 delta 节流后 imboy_syn:publish 直推 ephemeral stream_delta 帧
%% （不落库/不 ACK/不重试），流结束用同一 id 走 deliver_reply 定稿落库。
-spec run_and_reply_stream(module(), map(), integer(), integer(), [map()]) -> ok.
run_and_reply_stream(Mod, Opts, FromUid, ToId, Messages) ->
    StreamId = ec_cnv:to_binary(elib_tsid:generate()),
    Ctx = #{
        stream_id => StreamId,
        target_uid => FromUid,
        from => ec_cnv:to_binary(ToId),
        to => ec_cnv:to_binary(FromUid),
        type => <<"C2C">>
    },
    StreamFun = fun(Delta) -> llm_stream:push(Ctx, Delta) end,
    case Mod:chat_stream(ToId, Messages, Opts, StreamFun) of
        {ok, RespMap} when is_map(RespMap) ->
            llm_stream:flush_tail(Ctx),
            Result = maps:get(<<"result">>, RespMap, <<>>),
            %% 定稿复用 StreamId：前端主键去重 → update 替换流式气泡
            deliver_reply(FromUid, ToId, StreamId, Result);
        {error, Reason} ->
            ok = ?ERROR_LOG("[AGENT_REPLY_STREAM_FAILED] to=~p reason=~p~n", [ToId, Reason]),
            %% 兜底：flush 残留 + 把已累积部分作为定稿发出（同 id），避免气泡永不定稿
            llm_stream:flush_tail(Ctx),
            deliver_reply(FromUid, ToId, StreamId, llm_stream:full_text(Ctx))
    end.

%% 构建 C2C 回复消息（agent→human）并经既有投递骨架发送（一次性路径：新生成 id）
-spec deliver_reply(integer(), integer(), binary()) -> ok.
deliver_reply(FromUid, ToId, Result) ->
    ReplyMsgId = ec_cnv:to_binary(elib_tsid:generate()),
    deliver_reply(FromUid, ToId, ReplyMsgId, Result).

%% 用指定 MsgId 定稿：流式路径复用 StreamId，令 stream_delta 与定稿共享 id
-spec deliver_reply(integer(), integer(), binary(), binary()) -> ok.
deliver_reply(_FromUid, _ToId, _MsgId, <<>>) ->
    ok;
deliver_reply(FromUid, ToId, MsgId, Result) ->
    Content = elib_str:replace_single_quote(Result),
    Msg = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2C">>,
        <<"from">> => ec_cnv:to_binary(ToId),
        <<"to">> => ec_cnv:to_binary(FromUid),
        <<"msg_type">> => <<"text">>,
        %% text/content 双键：兼容不同渲染消费者
        <<"payload">> => #{<<"text">> => Content, <<"content">> => Content},
        <<"created_at">> => elib_dt:millisecond()
    },
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = elib_retry_config:intervals(<<"c2c">>),
    message_ds:send_next(FromUid, MsgId, MsgJson, MsLi),
    ok.
