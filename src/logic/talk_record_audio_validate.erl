-module(talk_record_audio_validate).
-moduledoc "no-backend-ffmpeg V1：成品有界校验（服务器读字节，不信客户端声明）。\n\n"
"在已确认成品对象的 spool 源上单遍完成：\n"
"<ul><li>总字节 == 冻结 final_audio.byte_size == Σ 段 byte_size == spool size"
"（严格拒绝尾随字节）；</li>"
"<li>按 manifest 段 byte_size 顺序分界，逐段 SHA-256 比对已确认分段"
"（篡改任一字节、重排等长段均拒绝）；</li>"
"<li>ADTS 全帧扫描（AAC-LC、单 raw data block、全帧采样率/声道一致、"
"段边界帧对齐；截断帧拒绝——与客户端 talk_record_adts.dart 同一结构规则）；</li>"
"<li>真实样本总数（帧数×1024）、统一整数舍入时长与冻结输入逐项比对；</li>"
"<li>限额取 capabilities 与通用附件上限的有效较小值，fail-closed。</li></ul>".
-export([validate/3, validate/4, limits/0]).

-define(READ_CHUNK, 65536).
-define(MAX_PARTS, 2000).
%% attachment_artifact_reader / with_spool 的通用成品上限（128MB）。
-define(MAX_TOTAL_BYTES, 134217728).

%% @doc 校验成品 spool 源。Parts 为 manifest 段（sha256/byte_size，稳定序）；
%% Final 为 job 冻结 processor_input.final_audio（八字段）。
%% 返回 {ok, #{frames => _, sample_rate => _, channels => _, samples => _,
%% duration_ms => _}}；任何不符在单次下载内全部拒绝，不产出派生副本。
-spec validate(term(), [map()], map()) ->
    {ok, map()} | {error, term()}.
validate(Source, Parts, Final) ->
    validate(Source, Parts, Final, limits()).

validate(Source, Parts, Final, Limits) ->
    SegSizes = [seg_size(P) || P <- Parts],
    Total = lists:sum(SegSizes),
    case
        Parts =/= [] andalso
            Total > 0 andalso
            Total =< maps:get(max_bytes, Limits) andalso
            length(Parts) =< maps:get(max_parts, Limits) andalso
            Total =:= attachment_upload_spool:size(Source)
    of
        false ->
            {error, audio_size_limit};
        true ->
            case final_shape(Final) of
                ok ->
                    State = #{
                        global => crypto:hash_init(sha256),
                        seg_idx => 0,
                        seg_ctx => crypto:hash_init(sha256),
                        seg_left => hd(SegSizes),
                        seg_sizes => SegSizes,
                        seg_shas => [maps:get(<<"sha256">>, P, <<>>) || P <- Parts],
                        digests => [],
                        adts => adts_init()
                    },
                    case loop(Source, 0, Total, State) of
                        {ok, #{global := GlobalCtx, adts := Adts} = End} ->
                            finish(Final, End, GlobalCtx, Adts);
                        {error, _} = E ->
                            E
                    end;
                {error, _} = E ->
                    E
            end
    end.

seg_size(P) ->
    case maps:get(<<"byte_size">>, P, 0) of
        N when is_integer(N), N > 0 -> N;
        _ -> 0
    end.

%% 冻结输入形态校验（输入已在 complete 事务准入，这里只防内部漂移）。
final_shape(Final) ->
    Rate = maps:get(<<"sample_rate">>, Final, 0),
    Channels = maps:get(<<"channels">>, Final, 0),
    Samples = maps:get(<<"sample_count">>, Final, 0),
    Ms = maps:get(<<"duration_ms">>, Final, 0),
    case
        is_integer(Rate) andalso Rate > 0 andalso
            Channels =:= 1 andalso
            is_integer(Samples) andalso Samples > 0 andalso Samples rem 1024 =:= 0 andalso
            is_integer(Ms) andalso Ms =:= ms_of_samples(Samples, Rate)
    of
        true -> ok;
        false -> {error, final_input_invalid}
    end.

loop(_Source, Offset, Total, State) when Offset >= Total ->
    {ok, State};
loop(Source, Offset, Total, State) ->
    Count = min(?READ_CHUNK, Total - Offset),
    case attachment_upload_spool:read_at(Source, Offset, Count) of
        {ok, Bytes} ->
            case feed(Bytes, State) of
                {ok, Next} ->
                    loop(Source, Offset + byte_size(Bytes), Total, Next);
                {error, _} = E ->
                    E
            end;
        {error, Reason} ->
            {error, {spool_read_failed, Reason}}
    end.

feed(<<>>, State) ->
    {ok, State};
feed(Bytes, State) ->
    #{seg_left := Left} = State,
    Take = min(Left, byte_size(Bytes)),
    <<SegBytes:Take/binary, Rest/binary>> = Bytes,
    case adts_feed(SegBytes, maps:get(adts, State)) of
        {error, Reason} ->
            {error, Reason};
        Adts ->
            State1 = State#{
                global => crypto:hash_update(maps:get(global, State), SegBytes),
                seg_ctx => crypto:hash_update(maps:get(seg_ctx, State), SegBytes),
                seg_left => Left - Take,
                adts => Adts
            },
            continue_segment(Rest, State1)
    end.

continue_segment(Rest, #{seg_left := Left} = State) when Left > 0 ->
    feed(Rest, State);
continue_segment(Rest, #{seg_idx := Idx, adts := {Frames, _, _, Buf}} = State) ->
    %% 段边界：逐段 SHA 比对 + 帧对齐（客户端分段合同：段以
    %% 完整帧结尾，跨段残帧即拒绝）。
    Digest = binary:encode_hex(crypto:hash_final(maps:get(seg_ctx, State)), lowercase),
    Expected = lists:nth(Idx + 1, maps:get(seg_shas, State)),
    case Digest =:= Expected andalso Buf =:= <<>> andalso Frames > 0 of
        false when Buf =/= <<>> ->
            {error, audio_part_boundary_invalid};
        false ->
            {error, audio_part_integrity_mismatch};
        true ->
            next_segment(Rest, State#{
                seg_idx => Idx + 1,
                digests => [Digest | maps:get(digests, State)]
            })
    end.

next_segment(Rest, #{seg_idx := Idx, seg_sizes := Sizes} = State) ->
    case Idx =:= length(Sizes) of
        true ->
            %% 最后一段：feed 自然结束。
            {ok, State};
        false ->
            %% 帧计数/参数跨段累计；已确认段边界缓冲排空。
            feed(Rest, State#{
                seg_ctx => crypto:hash_init(sha256),
                seg_left => lists:nth(Idx + 1, Sizes)
            })
    end.

finish(Final, End, GlobalCtx, Adts) ->
    #{seg_idx := SegIdx, seg_sizes := SegSizes, digests := Digests} = End,
    GlobalSha = binary:encode_hex(crypto:hash_final(GlobalCtx), lowercase),
    case
        SegIdx =:= length(SegSizes) andalso
            length(Digests) =:= SegIdx andalso
            GlobalSha =:= maps:get(<<"sha256">>, Final, <<>>)
    of
        false ->
            {error, final_integrity_mismatch};
        true ->
            case Adts of
                {Frames, RateIdx, Channels, <<>>} when is_integer(Frames), Frames > 0 ->
                    Rate = rate_of(RateIdx),
                    Samples = Frames * 1024,
                    case
                        Rate =:= maps:get(<<"sample_rate">>, Final) andalso
                            Channels =:= maps:get(<<"channels">>, Final) andalso
                            Samples =:= maps:get(<<"sample_count">>, Final) andalso
                            ms_of_samples(Samples, Rate) =:= maps:get(<<"duration_ms">>, Final)
                    of
                        true ->
                            {ok, #{
                                frames => Frames,
                                sample_rate => Rate,
                                channels => Channels,
                                samples => Samples,
                                duration_ms => ms_of_samples(Samples, Rate)
                            }};
                        false ->
                            {error, audio_identity_mismatch}
                    end;
                _ ->
                    %% 截断帧（残缓冲）或零帧。
                    {error, audio_frame_invalid}
            end
    end.

%% ===================================================================
%% ADTS 流式扫描（与客户端 talk_record_adts.dart 同一结构规则）：
%% syncword 0xFFF、MPEG-4、layer=00、profile=01（AAC-LC）、采样率索引
%% 0..12、声道 1..2、单 raw data block（B6 低 2 位=00）、帧长覆盖完整帧；
%% 全部帧采样率/声道必须一致。状态 {Frames, RateIdx, Channels, Buf}。
%% ===================================================================
adts_init() ->
    {0, undefined, undefined, <<>>}.

adts_feed(Bytes, {Frames, RateIdx, Channels, Buf}) ->
    adts_loop(<<Buf/binary, Bytes/binary>>, Frames, RateIdx, Channels, <<>>).

adts_loop(Buf, Frames, RateIdx, Channels, _) when byte_size(Buf) < 7 ->
    {Frames, RateIdx, Channels, Buf};
adts_loop(<<16#FF, B1, B2, B3, B4, B5, B6, _Rest/binary>> = Buf, Frames, RateIdx, Channels, _Whole) ->
    Header =
        case B1 band 1 of
            1 -> 7;
            _ -> 9
        end,
    Length = ((B3 band 3) bsl 11) + (B4 bsl 3) + (B5 bsr 5),
    SyncOk = (B1 band 16#F6) =:= 16#F0,
    ProfileOk = (B2 bsr 6) =:= 1,
    RateIdx0 = (B2 bsr 2) band 15,
    RateOk = RateIdx0 =< 12,
    Channels0 = ((B2 band 1) bsl 2) + (B3 bsr 6),
    ChannelsOk = Channels0 >= 1 andalso Channels0 =< 2,
    BlocksOk = B6 band 3 =:= 0,
    case
        SyncOk andalso ProfileOk andalso RateOk andalso ChannelsOk andalso
            BlocksOk andalso Length > Header
    of
        false ->
            {error, audio_frame_invalid};
        true ->
            Rate1 = consistent(RateIdx, RateIdx0),
            Channels1 = consistent(Channels, Channels0),
            case {Rate1, Channels1} of
                {error, _} ->
                    {error, audio_frame_invalid};
                {RI, CH} ->
                    case byte_size(Buf) < Length of
                        true ->
                            %% 帧未完整：保留整段缓冲等下一块（不提前计数，
                            %% 防止下一块续解析时重复计数）。
                            {Frames, RI, CH, Buf};
                        false ->
                            <<Frame:Length/binary, Rest2/binary>> = Buf,
                            <<_:Header/binary, _/binary>> = Frame,
                            adts_loop(Rest2, Frames + 1, RI, CH, <<>>)
                    end
            end
    end;
adts_loop(Buf, Frames, RateIdx, Channels, _) ->
    %% 非 syncword 开头：结构违规。
    case Buf of
        <<>> -> {Frames, RateIdx, Channels, Buf};
        _ -> {error, audio_frame_invalid}
    end.

consistent(undefined, V) -> V;
consistent(V, V) -> V;
consistent(_A, _B) -> error.

rate_of(Idx) ->
    Rates = [
        96000,
        88200,
        64000,
        48000,
        44100,
        32000,
        24000,
        22050,
        16000,
        12000,
        11025,
        8000,
        7350
    ],
    lists:nth(Idx + 1, Rates).

%% 统一整数舍入：ms(S,R) = (S×1000 + R div 2) div R（与客户端一致）。
ms_of_samples(Samples, Rate) ->
    (Samples * 1000 + Rate div 2) div Rate.

%% ===================================================================
%% 限额：capabilities 与通用附件上限的有效较小值；未配置 fail-closed。
%% ===================================================================
limits() ->
    Cap = talk_record_ds:capabilities(),
    MaxBytes0 = maps:get(<<"max_recording_bytes">>, Cap, 0),
    #{
        max_parts => ?MAX_PARTS,
        max_bytes => min(MaxBytes0, ?MAX_TOTAL_BYTES),
        max_duration_ms => maps:get(<<"max_duration_ms">>, Cap, 0)
    }.
