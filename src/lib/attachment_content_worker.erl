-module(attachment_content_worker).
-moduledoc "direct-v1 最小内容 Worker（step-12，协议 §5.4）：受控流式读 + 规则 + 认证 receipt。".
%%%
% 职责（最小集）：
%   * 读取稳定最终 candidate（读函数注入：真实 Garage 流式读在 step-5a 后
%     接线；本模块只依赖 `ReadChunkFun(Offset, Len) -> {ok, binary()} | eof`）；
%   * 同次受控读中计算完整 SHA-256 与 size（流式，非分块拼接）；
%   * 执行现行内容规则：魔数头（探测块）+ 文本全量洁净（流式累积，与
%     eb_asset_content:sniff/2 全量语义逐项等价——测试以 sniff/2 为 oracle）；
%   * 预算：扫描 deadline（毫秒）超时 fail-closed（worker 失败停待验证，
%     不视通过）；内存有界（探测 64KiB、流块 256KiB）。
% 不做：图片变换（本轮无图片派生管线）、E2EE 明文（E2EE 附件不交 Worker）。
%%%

-export([
    scan/6,
    scan_stream/4
]).

-define(PROBE_BYTES, 65536).
-define(STREAM_CHUNK, 262144).

%% @doc 扫描 + 出 receipt。
%% ReadChunkFun(Offset, Len)：受控读注入；SniffFun(Mime, Head) 为魔数
%% 判定注入（core 不反向依赖 feature 的规则模块——铁律5，由 EB 域调用方
%% 传 fun eb_asset_content:sniff/2）；Mime 为声明类型；
%% AttemptKey/ObjectKey 绑定 receipt；Secret 为 receipt HMAC 密钥；
%% Budget 为扫描预算 map（必含 deadline_ms）。
%% 返回 {ok, #{sha256, size, receipt}} | {error, {mime_content_mismatch, _}} |
%%       {error, timeout}。
scan(
    ReadChunkFun,
    SniffFun,
    Mime,
    AttemptKey,
    ObjectKey,
    #{secret := Secret, deadline_ms := Budget}
) ->
    Deadline = erlang:monotonic_time(millisecond) + Budget,
    case scan_stream(ReadChunkFun, SniffFun, Mime, Deadline) of
        {ok, #{sha256 := Sha, size := Size}} ->
            Receipt = attachment_content_receipt:issue(
                #{
                    attempt_key => AttemptKey,
                    object_key => ObjectKey,
                    sha256 => Sha,
                    size => Size,
                    mime => Mime,
                    verdict => <<"pass">>
                },
                Secret
            ),
            {ok, #{sha256 => Sha, size => Size, receipt => Receipt}};
        {error, _} = E ->
            E
    end.

%% @doc 受控流式扫描：返回 {ok, #{sha256, size}} 或规则/超时错误。
%% 头 ?PROBE_BYTES 做魔数探测；全量流做 size/hash 与文本洁净累积。
%% 空对象（size=0，declare 合法）：ReadChunkFun(0, ·) 返回 eof → 头按
%% <<>> 处理（非 text 家族魔数必然 mismatch 拒绝；text 家族空集洁净），
%% 流读立即 eof 得空摘要 e3b0…（语义与全量判定等价）。
scan_stream(ReadChunkFun, SniffFun, Mime, Deadline) ->
    Head =
        case ReadChunkFun(0, ?PROBE_BYTES) of
            eof -> <<>>;
            {ok, B} -> B
        end,
    %% 1. 魔数头判定。text 家族的强判定在全量流做（探测块只做非 text 魔数）；
    %%    非 text 家族头规则全部落在探测块范围内（最长前缀 8B + ftyp 偏移）。
    HeadVerdict = SniffFun(Mime, Head),
    case {HeadVerdict, is_text_family(Mime)} of
        {ok, _} ->
            ok;
        {{error, {mime_content_mismatch, _}}, true} ->
            %% text 家族：头不判定，洁净在全量流累积
            ok;
        {{error, _}, false} ->
            erlang:throw({mime_content_mismatch, Mime})
    end,
    stream_rest(
        ReadChunkFun,
        0,
        crypto:hash_init(sha256),
        Deadline,
        is_text_family(Mime),
        0
    ).

stream_rest(ReadChunkFun, Offset, Ctx, Deadline, TextFamily, TextClean) ->
    case now_left(Deadline) of
        timeout ->
            erlang:throw(timeout);
        _ ->
            case ReadChunkFun(Offset, ?STREAM_CHUNK) of
                eof ->
                    Digest = crypto:hash_final(Ctx),
                    case TextFamily andalso TextClean =:= dirty of
                        true ->
                            {error, {mime_content_mismatch, <<"text">>}};
                        false ->
                            {ok, #{sha256 => binary:encode_hex(Digest, lowercase), size => Offset}}
                    end;
                {ok, Bin} ->
                    Clean =
                        case TextFamily of
                            true ->
                                case text_chunk_clean(Bin) of
                                    true -> TextClean;
                                    false -> dirty
                                end;
                            false ->
                                clean
                        end,
                    stream_rest(
                        ReadChunkFun,
                        Offset + byte_size(Bin),
                        crypto:hash_update(Ctx, Bin),
                        Deadline,
                        TextFamily,
                        Clean
                    );
                {error, _} = E ->
                    E
            end
    end.

now_left(Deadline) ->
    case Deadline - erlang:monotonic_time(millisecond) of
        Left when Left > 0 -> Left;
        _ -> timeout
    end.

%% 文本族洁净（与 eb_asset_content:text_plain_ok 等价的流式形式）：
%% 无 NUL、无非 \t\n\r 控制噪声；逐块判定 AND 累积 ≡ 全量判定。
%% 噪声字集逐字节 binary:match（BIF 子串搜索，非字符集匹配——连续
%% 序列模式只会漏检）：256KiB 块 ~29 次 BIF 扫描，仍为微秒级，替代
%% 逐字节 lists:any 的 ~25ms 级（100MB 文本不再因预算内超时被误拒）。
text_chunk_clean(Bin) ->
    binary:match(Bin, <<0>>) =:= nomatch andalso
        not lists:any(
            fun(B) -> binary:match(Bin, <<B>>) =/= nomatch end,
            ctrl_noise_bytes()
        ).

ctrl_noise_bytes() ->
    lists:seq(1, 8) ++ [11, 12] ++ lists:seq(14, 31).

is_text_family(<<"text/", _/binary>>) -> true;
is_text_family(<<"image/svg+xml">>) -> true;
is_text_family(_) -> false.
