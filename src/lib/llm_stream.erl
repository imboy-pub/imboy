-module(llm_stream).

%%% @doc LLM 流式增量的节流直推（ephemeral stream_delta 帧）
%%%
%%% 中间 token 累积到字节阈值才 imboy_syn:publish 一帧，不落库/不 ACK/不重试；
%%% 定稿由调用方另走可靠投递（message_ds:send_next），复用同一 stream_id，
%%% 令前端主键去重把流式气泡替换为定稿。
%%%
%%% 状态存进程字典（键按 stream_id 隔离），调用方须在单个 async 进程内使用，
%%% 进程退出后进程字典自动清理。被 ai_agent_reply（agent C2C）与
%%% msg_c2s_logic（bot_* C2S）两条通路共用（Phase 2 T2.2）。
%%%
%%% Ctx = #{stream_id := binary(),   %% 流会话 id，与定稿共享
%%%         target_uid := integer(), %% imboy_syn:publish 目标（在线接收方 human uid）
%%%         from := binary(),        %% 帧 from（agent uid / bot 标识）
%%%         to := binary(),          %% 帧 to（human uid）
%%%         type := binary()}        %% 帧 type（C2C / C2S）
%%% @since 2026-07-10

-export([push/2, flush_tail/1, full_text/1, reset/1]).
-export([stream_capable/1]).

%% 节流阈值（字节）：累积到此长度才推一帧，避免每个 token 一帧。
%% ~12 字节 ≈ 4 个中文字或 12 个 ASCII。
%% ponytail: 只做字节阈值节流；时间节流（M ms）等真有体感问题再加。
-define(FLUSH_BYTES, 12).

%% @doc 累积一个 delta，达字节阈值则 flush 一帧 stream_delta（中间帧 is_end=false）
-spec push(map(), binary()) -> ok.
push(Ctx, Delta) ->
    Sid = maps:get(stream_id, Ctx),
    put({llm_stream_full, Sid}, <<(pd({llm_stream_full, Sid}, <<>>))/binary, Delta/binary>>),
    Buf = <<(pd({llm_stream_buf, Sid}, <<>>))/binary, Delta/binary>>,
    case byte_size(Buf) >= ?FLUSH_BYTES of
        true ->
            publish(Ctx, Buf, false),
            put({llm_stream_buf, Sid}, <<>>),
            ok;
        false ->
            put({llm_stream_buf, Sid}, Buf),
            ok
    end.

%% @doc 流结束收尾：把残留缓冲连同 is_end=true 信号推最后一帧
%% （即使缓冲为空也发一帧带 is_end=true，给前端明确的流结束信号）
-spec flush_tail(map()) -> ok.
flush_tail(Ctx) ->
    Sid = maps:get(stream_id, Ctx),
    Buf = pd({llm_stream_buf, Sid}, <<>>),
    publish(Ctx, Buf, true),
    put({llm_stream_buf, Sid}, <<>>),
    ok.

%% @doc 已累积的全文（流式失败兜底定稿用）
-spec full_text(map()) -> binary().
full_text(Ctx) ->
    pd({llm_stream_full, maps:get(stream_id, Ctx)}, <<>>).

%% @doc 清空某 stream 的进程字典状态。
%% c2s_to_external 的 async_retry 在同一进程内递归重试（不 respawn），
%% 每次重试前须调此清空 buf/full/seq，否则新一轮 delta 会拼在上轮残留后
%% 产生乱码、seq 也不归零。调用方在每次真正发起 chat_stream 前调用。
-spec reset(map()) -> ok.
reset(Ctx) ->
    Sid = maps:get(stream_id, Ctx),
    _ = erase({llm_stream_buf, Sid}),
    _ = erase({llm_stream_full, Sid}),
    _ = erase({llm_stream_seq, Sid}),
    ok.

%% @doc 是否走流式：全局开关 + provider 导出 chat_stream/4 + capabilities.stream=true
-spec stream_capable(module()) -> boolean().
stream_capable(Mod) ->
    config_ds:env(llm_stream_enabled, false) =:= true andalso
        chat_stream_exported(Mod) andalso
        maps:get(stream, safe_capabilities(Mod), false) =:= true.

%% 先 ensure_loaded 再判 exported：dev/test 惰性加载下模块未载时
%% function_exported 会假 false（同 mcp_authz_gate 坑）。ensure_loaded 幂等便宜。
chat_stream_exported(Mod) ->
    _ = code:ensure_loaded(Mod),
    erlang:function_exported(Mod, chat_stream, 4).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 直推一帧 stream_delta（JSON 预编码，imboy_syn:publish 直达在线设备）
%% IsEnd=true 标记流结束（收尾帧），前端据此停止"打字中"动画、等定稿 text 替换
publish(Ctx, Chunk, IsEnd) ->
    Sid = maps:get(stream_id, Ctx),
    Seq = pd({llm_stream_seq, Sid}, 0),
    put({llm_stream_seq, Sid}, Seq + 1),
    Frame = #{
        <<"id">> => Sid,
        <<"type">> => maps:get(type, Ctx),
        <<"from">> => maps:get(from, Ctx),
        <<"to">> => maps:get(to, Ctx),
        <<"msg_type">> => <<"stream_delta">>,
        <<"payload">> => #{
            <<"stream_id">> => Sid,
            <<"index">> => Seq,
            <<"delta">> => Chunk,
            <<"is_end">> => IsEnd
        },
        <<"created_at">> => elib_dt:millisecond()
    },
    _ = imboy_syn:publish(maps:get(target_uid, Ctx), jsone:encode(Frame, [native_utf8])),
    ok.

pd(Key, Default) ->
    case get(Key) of
        undefined -> Default;
        Value -> Value
    end.

safe_capabilities(Mod) ->
    try Mod:capabilities() of
        Caps when is_map(Caps) -> Caps;
        _ -> #{}
    catch
        _:_ -> #{}
    end.
