-module(talk_record_audio_validate_tests).
-moduledoc "no-backend-ffmpeg V1：成品有界校验定向用例（真字节构造）。".
-include_lib("eunit/include/eunit.hrl").

%% 16kHz 单声道 AAC-LC 合法帧（7 字节头 + 32 字节载荷 = 39 字节）。
frame() ->
    Length = 39,
    B3 = 16#40 bor (Length bsr 11),
    B4 = (Length bsr 3) band 16#FF,
    B5 = (Length band 7) bsl 5,
    <<16#FF, 16#F1, 16#60, B3, B4, B5, 16#00, 0:256>>.

%% 44.1kHz 变体（freqIdx=4）用于参数漂移/重排负例。
frame_44k() ->
    Length = 39,
    B3 = 16#40 bor (Length bsr 11),
    B4 = (Length bsr 3) band 16#FF,
    B5 = (Length band 7) bsl 5,
    <<16#FF, 16#F1, 16#50, B3, B4, B5, 16#00, 0:256>>.

stream(Frames) when Frames > 0 -> iolist_to_binary(lists:duplicate(Frames, frame())).

stream_44k(Frames) when Frames > 0 ->
    iolist_to_binary(lists:duplicate(Frames, frame_44k())).

make_source(Bytes) ->
    {ok, W} = attachment_upload_spool:new(),
    {ok, _} = attachment_upload_spool:append(W, Bytes),
    {ok, S} = attachment_upload_spool:seal(W),
    S.

%% 按 SegSizes 顺序分界产 manifest parts（真实 sha256/byte_size）。
parts_of(Bytes, SegSizes) ->
    parts_of(Bytes, SegSizes, 0, 0, []).
parts_of(_Bytes, [], _Off, _Seq, Acc) ->
    lists:reverse(Acc);
parts_of(Bytes, [N | Rest], Off, Seq, Acc) ->
    <<_:Off/binary, Seg:N/binary, _/binary>> = Bytes,
    parts_of(Bytes, Rest, Off + N, Seq + 1, [
        #{
            <<"seq">> => Seq,
            <<"sha256">> => binary:encode_hex(crypto:hash(sha256, Seg), lowercase),
            <<"byte_size">> => N
        }
        | Acc
    ]).

final_of(Bytes, Frames, Rate) ->
    #{
        <<"sha256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
        <<"byte_size">> => byte_size(Bytes),
        <<"sample_rate">> => Rate,
        <<"channels">> => 1,
        <<"sample_count">> => Frames * 1024,
        <<"duration_ms">> => ms(Frames * 1024, Rate)
    }.

ms(Samples, Rate) -> (Samples * 1000 + Rate div 2) div Rate.

%% 两段各 4 帧：段明文 SHA/byte_size 分界正确，冻结身份一致 → 通过。
two_part_ok_test() ->
    Bytes = stream(8),
    Parts = parts_of(Bytes, [4 * 39, 4 * 39]),
    Final = final_of(Bytes, 8, 16000),
    {ok, #{frames := 8, sample_rate := 16000, channels := 1}} =
        talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).

%% 单段 12 帧。
single_part_ok_test() ->
    Bytes = stream(12),
    Parts = parts_of(Bytes, [12 * 39]),
    Final = final_of(Bytes, 12, 16000),
    {ok, #{frames := 12, samples := 12288, duration_ms := 768}} =
        talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).

%% 跨块边界（>64KB）与段边界帧对齐仍通过。
cross_chunk_ok_test() ->
    N = 1800,
    Bytes = stream(N),
    Seg = N * 39 div 2,
    Parts = parts_of(Bytes, [Seg, N * 39 - Seg]),
    Final = final_of(Bytes, N, 16000),
    {ok, #{frames := N}} = talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).

%% 篡改任一字节：段 SHA 不符拒绝。
tampered_byte_test() ->
    Bytes = stream(4),
    Parts = parts_of(Bytes, [4 * 39]),
    Final = final_of(Bytes, 4, 16000),
    Tampered = binary:replace(Bytes, <<0:32>>, <<1:32>>, [{scope, {20, 4}}]),
    {error, audio_part_integrity_mismatch} =
        talk_record_audio_validate:validate(make_source(Tampered), Parts, Final).

%% 重排等长段：段序 SHA 不符拒绝（顺序分界逐段比对）。
reordered_equal_len_test() ->
    A = stream(3),
    B = stream_44k(3),
    Parts = parts_of(<<A/binary, B/binary>>, [3 * 39, 3 * 39]),
    Final = final_of(<<A/binary, B/binary>>, 6, 16000),
    {error, audio_part_integrity_mismatch} =
        talk_record_audio_validate:validate(make_source(<<B/binary, A/binary>>), Parts, Final).

%% 尾随垃圾：声明内含尾随 → ADTS 扫描坏帧拒绝。
trailing_garbage_test() ->
    Bytes = <<(stream(4))/binary, "garbage">>,
    Parts = parts_of(Bytes, [byte_size(Bytes)]),
    Final = (final_of(Bytes, 4, 16000))#{<<"sample_count">> => 4 * 1024},
    {error, audio_frame_invalid} =
        talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).

%% spool 尺寸 > Σ 段 byte_size：总账不符拒绝。
spool_size_mismatch_test() ->
    Bytes = stream(4),
    Parts = parts_of(Bytes, [4 * 39]),
    Final = final_of(Bytes, 4, 16000),
    {error, audio_size_limit} =
        talk_record_audio_validate:validate(make_source(<<Bytes/binary, "x">>), Parts, Final).

%% 截断帧：段不以完整帧结尾 → 段边界残缓冲拒绝。
truncated_frame_test() ->
    Full = stream(2),
    Bytes = binary:part(Full, 0, byte_size(Full) - 10),
    Parts = parts_of(Bytes, [byte_size(Bytes)]),
    Final = (final_of(Bytes, 2, 16000))#{<<"sample_count">> => 2 * 1024},
    {error, audio_part_boundary_invalid} =
        talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).

%% 段边界不帧对齐：跨段残帧拒绝（客户端分段合同）。
part_boundary_unaligned_test() ->
    Bytes = stream(4),
    Parts = parts_of(Bytes, [3 * 39 + 10, 39 - 10]),
    Final = final_of(Bytes, 4, 16000),
    {error, audio_part_boundary_invalid} =
        talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).

%% 换采样率：全帧参数一致性拒绝。
sample_rate_drift_test() ->
    Bytes = <<(stream(2))/binary, (stream_44k(2))/binary>>,
    Parts = parts_of(Bytes, [byte_size(Bytes)]),
    Final = final_of(Bytes, 4, 16000),
    {error, audio_frame_invalid} =
        talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).

%% 冻结身份漂移：样本数与真实帧数不符拒绝。
identity_mismatch_test() ->
    Bytes = stream(4),
    Parts = parts_of(Bytes, [4 * 39]),
    %% 同字节同 size，但身份自洽地声称 8 帧（时长同步改）→ 身份比对拒绝。
    Final = #{
        <<"sha256">> => maps:get(<<"sha256">>, final_of(Bytes, 4, 16000)),
        <<"byte_size">> => byte_size(Bytes),
        <<"sample_rate">> => 16000,
        <<"channels">> => 1,
        <<"sample_count">> => 8 * 1024,
        <<"duration_ms">> => ms(8 * 1024, 16000)
    },
    {error, audio_identity_mismatch} =
        talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).

%% final 形态漂移：channels 非 1 拒绝。
final_shape_drift_test() ->
    Bytes = stream(4),
    Parts = parts_of(Bytes, [4 * 39]),
    Final = (final_of(Bytes, 4, 16000))#{<<"channels">> => 2},
    {error, final_input_invalid} =
        talk_record_audio_validate:validate(make_source(Bytes), Parts, Final).
