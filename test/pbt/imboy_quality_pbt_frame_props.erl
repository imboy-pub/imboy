%% IMBOY_ERLANG_QUALITY P2-03：F01 帧往返/wire 布局 + F02 任意有界分片恢复。
%% 独立 wire oracle：按协议合同手拼字节（<<73,66,2,Flg,Typ,Len:32,P>>，
%% MAGIC=16#4942=73,66），不复用 imboy_frame:encode——防编解码同错抵消。
-module(imboy_quality_pbt_frame_props).

-export([
    prop_f01_roundtrip_wire_layout/0,
    prop_f02_fragment_recovery/0,
    witnesses_ok/0
]).

-include_lib("proper/include/proper.hrl").

%% ---- 独立 wire oracle（合同：Magic 0x4942 | Ver 2 | Flg u8 | Typ u8 | Len u32 BE） ----

oracle_wire(Ver, Flags, Type, Payload) ->
    Len = byte_size(Payload),
    <<73, 66, Ver:8, Flags:8, Type:8, Len:32/big-unsigned, Payload/binary>>.

oracle_frame(Type, Flags, Payload) ->
    oracle_wire(2, Flags, Type, Payload).

%% ---- F01：编码 wire 布局 + 解码往返 ----
%% 域：Type/Flags 全域 u8；Payload 任意二进制（天然含非 UTF8/空/0-255 字节）。
prop_f01_roundtrip_wire_layout() ->
    ?FORALL(
        {Type, Flags, Payload},
        {integer(0, 255), integer(0, 255), binary()},
        begin
            Encoded = imboy_frame:encode(Type, Flags, Payload),
            Wire = oracle_frame(Type, Flags, Payload),
            case imboy_frame:decode(Encoded) of
                {ok, Frame, <<>>} ->
                    imboy_frame:version(Frame) =:= 2 andalso
                        imboy_frame:flags(Frame) =:= Flags andalso
                        imboy_frame:type(Frame) =:= Type andalso
                        imboy_frame:payload(Frame) =:= Payload andalso
                        %% 独立布局参考：字节级一致
                        Encoded =:= Wire;
                _ ->
                    false
            end
        end
    ).

%% ---- F02：合法帧序列的任意有界分片流式恢复 ----
%% 生成：非空帧列表 -> 拼接字节流；切割点集合（去重升序，域 1..Size-1；
%% 最小帧 9 字节故 Size 恒 >1）。分片依次喂 decode_stream：所有完整帧按原序
%% 恢复，未消费尾部保留。嵌套 ?FORALL（内层域依赖外层具体值）。
prop_f02_fragment_recovery() ->
    ?FORALL(
        Frames,
        non_empty(list({integer(0, 255), integer(0, 255), binary()})),
        begin
            Stream = frame_bytes(Frames),
            Size = byte_size(Stream),
            ?FORALL(
                Cuts0,
                list(integer(1, Size - 1)),
                begin
                    Fragments = fragments(Stream, [0 | lists:usort(Cuts0)] ++ [Size]),
                    case feed_all(Fragments, [], <<>>) of
                        {ok, Got, Remaining} ->
                            Got =:= [mk_expect(X) || X <- Frames] andalso
                                Remaining =:= unconsumed_tail(Stream, Frames);
                        {error, _} ->
                            false
                    end
                end
            )
        end
    ).

frame_bytes(Frames) ->
    iolist_to_binary([imboy_frame:encode(T, F, P) || {T, F, P} <- Frames]).

mk_expect({T, F, P}) ->
    #{v => 2, flags => F, type => T, payload => P}.

unconsumed_tail(Stream, Frames) ->
    unconsumed(Stream, Frames).

unconsumed(Stream, [{_, _, P} | Rest]) ->
    Skip = 9 + byte_size(P),
    case byte_size(Stream) >= Skip of
        true -> unconsumed(binary:part(Stream, Skip, byte_size(Stream) - Skip), Rest);
        false -> Stream
    end;
unconsumed(Remaining, []) ->
    Remaining.

fragments(Bin, Points) ->
    frag(Bin, Points).

frag(Bin, [A, B | Rest]) ->
    [binary:part(Bin, A, B - A) | frag(Bin, [B | Rest])];
frag(_, [_]) ->
    [].

feed_all([], FramesAcc, RestAcc) ->
    %% Acc 以 ++ 尾部追加保持正序（与 imboy_frame:decode_stream 的头插+reverse
    %% 不同），此处不可再 reverse（P2-03 反例修复：反序导致假失败）
    {ok, FramesAcc, RestAcc};
feed_all([Frag | Rest], FramesAcc, RestAcc) ->
    case imboy_frame:decode_stream(<<RestAcc/binary, Frag/binary>>, []) of
        {error, _} ->
            {error, unexpected_decode_error};
        {Frames, Remaining} ->
            Mapped = [
                mk_expect({imboy_frame:type(X), imboy_frame:flags(X), imboy_frame:payload(X)})
             || X <- Frames
            ],
            feed_all(Rest, FramesAcc ++ Mapped, Remaining)
    end.

%% ---- 强制见证（TEST_PLAN F01/F02 mandatory witnesses） ----
%% 全部为确定性断言；任一失败 -> 非 ok 返回 -> 框架拒跑随机（random_started=0）。

witnesses_ok() ->
    W = [
        {"w1_field_bounds_empty", fun w1/0},
        {"w2_non_utf8_payload", fun w2/0},
        {"w3_golden_wire", fun w3/0},
        {"w4_bad_magic", fun w4/0},
        {"w5_version_guard", fun w5/0},
        {"w6_frame_too_large", fun w6/0},
        {"w7_partial_tail_more", fun w7/0},
        {"w8_multi_frame_order", fun w8/0},
        {"w9_one_byte_more", fun w9/0}
    ],
    case [Name || {Name, F} <- W, F() =/= ok] of
        [] -> ok;
        Failed -> {error, {witness_failed, Failed}}
    end.

w1() ->
    roundtrip_ok(0, 0, <<>>) andalso roundtrip_ok(255, 255, <<>>) andalso ok.

w2() ->
    roundtrip_ok(7, 16, <<0, 255, 128, 16#FF, 16#00, "abc">>) andalso ok.

w3() ->
    expect(
        imboy_frame:encode(9, 16#05, <<"im">>) =:= <<73, 66, 2, 16#05, 9, 2:32, "im">>
    ).

w4() ->
    expect_err(imboy_frame:decode(<<16#49, 16#43, 2, 0, 0, 0:32>>), bad_magic).

w5() ->
    expect_err(imboy_frame:decode(oracle_wire(3, 0, 0, <<>>)), unsupported_version).

w6() ->
    expect_err(
        imboy_frame:decode(<<73, 66, 2, 0, 0, 16#1000001:32>>), frame_too_large
    ).

w7() ->
    Full = imboy_frame:encode(1, 0, <<"abcdefgh">>),
    Trunc = binary:part(Full, 0, 12),
    case imboy_frame:decode(Trunc) of
        {more, _} -> ok;
        _ -> error
    end.

w8() ->
    S2 = <<
        (imboy_frame:encode(1, 0, <<"a">>))/binary, (imboy_frame:encode(2, 0, <<"bc">>))/binary
    >>,
    case imboy_frame:decode_stream(S2, []) of
        {Fs, <<>>} ->
            expect(
                [imboy_frame:type(X) || X <- Fs] =:= [1, 2] andalso
                    [imboy_frame:payload(X) || X <- Fs] =:= [<<"a">>, <<"bc">>]
            );
        _ ->
            error
    end.

w9() ->
    case imboy_frame:decode(<<73>>) of
        {more, _} -> ok;
        _ -> error
    end.

roundtrip_ok(T, F, P) ->
    Enc = imboy_frame:encode(T, F, P),
    case
        Enc =:= oracle_frame(T, F, P) andalso
            (catch imboy_frame:decode(Enc))
    of
        {ok, Fr, <<>>} ->
            imboy_frame:type(Fr) =:= T andalso imboy_frame:flags(Fr) =:= F andalso
                imboy_frame:payload(Fr) =:= P;
        _ ->
            false
    end.

expect(true) -> ok;
expect(false) -> error.

expect_err({error, R}, R) -> ok;
expect_err(_, _) -> error.
