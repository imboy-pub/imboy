%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_frame 分层二进制协议帧编解码测试
%%%
%%% 设计规范参见 .claude/plans/imboy-frame-protocol.md
%%%
%%% 帧格式:
%%%   [Magic:16 = 0xIB01] [Ver:8] [Flags:8] [Type:8] [PayloadLen:32] [Payload:N]
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(imboy_frame_tests).

-include_lib("eunit/include/eunit.hrl").

%% Type 常量 (与 imboy_frame.hrl 对齐)
-define(TYPE_HEARTBEAT_PING, 16#01).
-define(TYPE_HEARTBEAT_PONG, 16#02).
-define(TYPE_ACK,            16#03).
-define(TYPE_MSG_C2C,        16#20).

%% Flags
-define(FLAG_NONE, 0).
-define(FLAG_ACK,  16#20).
-define(FLAG_CMP,  16#80).
-define(FLAG_PING, 16#E0).  %% CMP=0 ENC=0 ACK=1 PRI=7

-define(MAGIC, 16#4942).  %% "IB"
-define(VERSION, 2).

%%%===================================================================
%%% encode/3 测试
%%%===================================================================

encode_empty_payload_test() ->
    Bin = imboy_frame:encode(?TYPE_HEARTBEAT_PING, ?FLAG_PING, <<>>),
    ?assertEqual(9, byte_size(Bin)),
    <<Magic:16, Ver:8, Flags:8, Type:8, Len:32>> = Bin,
    ?assertEqual(?MAGIC, Magic),
    ?assertEqual(?VERSION, Ver),
    ?assertEqual(?FLAG_PING, Flags),
    ?assertEqual(?TYPE_HEARTBEAT_PING, Type),
    ?assertEqual(0, Len).

encode_ping_with_seq_test() ->
    %% 心跳帧 = 9 字节头 + 2 字节 seq = 11 字节
    Seq = 42,
    Bin = imboy_frame:encode(?TYPE_HEARTBEAT_PING, ?FLAG_PING, <<Seq:16>>),
    ?assertEqual(11, byte_size(Bin)),
    <<_:9/binary, PayloadSeq:16>> = Bin,
    ?assertEqual(Seq, PayloadSeq).

encode_ack_with_msg_id_test() ->
    MsgId = 16#DEADBEEFCAFEBABE,
    Bin = imboy_frame:encode(?TYPE_ACK, ?FLAG_NONE, <<MsgId:64>>),
    ?assertEqual(17, byte_size(Bin)),
    <<_:9/binary, PayloadId:64>> = Bin,
    ?assertEqual(MsgId, PayloadId).

encode_large_payload_test() ->
    Payload = binary:copy(<<"x">>, 5000),
    Bin = imboy_frame:encode(?TYPE_MSG_C2C, ?FLAG_NONE, Payload),
    ?assertEqual(9 + 5000, byte_size(Bin)),
    %% 头部布局: [Magic:2][Ver:1][Flags:1][Type:1][Len:4] = 9 bytes
    <<_:5/binary, Len:32, _/binary>> = Bin,
    ?assertEqual(5000, Len).

encode_payload_length_bigendian_test() ->
    %% 确认 PayloadLen 使用 big-endian
    Payload = binary:copy(<<0>>, 256),
    Bin = imboy_frame:encode(?TYPE_MSG_C2C, ?FLAG_NONE, Payload),
    <<_Magic:16, _Ver:8, _Flags:8, _Type:8, B1:8, B2:8, B3:8, B4:8, _/binary>> = Bin,
    ?assertEqual(0, B1),
    ?assertEqual(0, B2),
    ?assertEqual(1, B3),
    ?assertEqual(0, B4).

%%%===================================================================
%%% decode/1 测试
%%%===================================================================

decode_single_frame_test() ->
    Payload = <<"hello">>,
    Bin = imboy_frame:encode(?TYPE_MSG_C2C, ?FLAG_ACK, Payload),
    {ok, Frame, Rest} = imboy_frame:decode(Bin),
    ?assertEqual(<<>>, Rest),
    ?assertEqual(?VERSION, imboy_frame:version(Frame)),
    ?assertEqual(?FLAG_ACK, imboy_frame:flags(Frame)),
    ?assertEqual(?TYPE_MSG_C2C, imboy_frame:type(Frame)),
    ?assertEqual(Payload, imboy_frame:payload(Frame)).

decode_empty_payload_test() ->
    Bin = imboy_frame:encode(?TYPE_HEARTBEAT_PONG, ?FLAG_NONE, <<>>),
    {ok, Frame, <<>>} = imboy_frame:decode(Bin),
    ?assertEqual(<<>>, imboy_frame:payload(Frame)),
    ?assertEqual(?TYPE_HEARTBEAT_PONG, imboy_frame:type(Frame)).

decode_roundtrip_test() ->
    %% 往返测试：所有关键字段
    Cases = [
        {?TYPE_HEARTBEAT_PING, ?FLAG_PING, <<42:16>>},
        {?TYPE_HEARTBEAT_PONG, ?FLAG_NONE, <<42:16>>},
        {?TYPE_ACK, ?FLAG_NONE, <<16#DEADBEEF:64>>},
        {?TYPE_MSG_C2C, ?FLAG_ACK, <<"短消息">>},
        {?TYPE_MSG_C2C, ?FLAG_CMP bor ?FLAG_ACK, binary:copy(<<0>>, 1000)}
    ],
    lists:foreach(fun({T, F, P}) ->
        Bin = imboy_frame:encode(T, F, P),
        {ok, Frame, <<>>} = imboy_frame:decode(Bin),
        ?assertEqual(T, imboy_frame:type(Frame)),
        ?assertEqual(F, imboy_frame:flags(Frame)),
        ?assertEqual(P, imboy_frame:payload(Frame))
    end, Cases).

%%%===================================================================
%%% 不完整帧处理
%%%===================================================================

decode_incomplete_header_test() ->
    %% 少于 9 字节头 → {more, Buf}
    ?assertMatch({more, <<?MAGIC:16>>},
                 imboy_frame:decode(<<?MAGIC:16>>)),
    ?assertMatch({more, _},
                 imboy_frame:decode(<<?MAGIC:16, ?VERSION:8, 0:8, 1:8>>)).

decode_incomplete_payload_test() ->
    %% 头部 OK 但 payload 不完整 → {more, Buf}
    Partial = <<?MAGIC:16, ?VERSION:8, 0:8, ?TYPE_MSG_C2C:8, 100:32, "xxx">>,
    ?assertMatch({more, _}, imboy_frame:decode(Partial)).

decode_empty_test() ->
    ?assertMatch({more, <<>>}, imboy_frame:decode(<<>>)).

%%%===================================================================
%%% 错误处理
%%%===================================================================

decode_bad_magic_test() ->
    Bad = <<16#FFFF:16, ?VERSION:8, 0:8, 1:8, 0:32>>,
    ?assertEqual({error, bad_magic}, imboy_frame:decode(Bad)).

decode_first_byte_matches_magic_high_test() ->
    %% 恰好 1 字节等于魔数高字节 0x49 → {more, Buf}
    ?assertMatch({more, <<16#49>>}, imboy_frame:decode(<<16#49>>)).

decode_partial_magic_high_wrong_low_test() ->
    %% 2 字节: 0x49, 0x00 (不是合法魔数 0x4942) → {error, bad_magic}
    ?assertEqual({error, bad_magic}, imboy_frame:decode(<<16#49, 16#00>>)).

decode_single_non_magic_byte_test() ->
    %% 单字节且不是魔数高字节 → 直接 bad_magic
    ?assertEqual({error, bad_magic}, imboy_frame:decode(<<16#FF>>)).

decode_version_tolerance_test() ->
    %% 解码器对 version 字段不做校验，向前兼容
    Buf = <<?MAGIC:16, 99:8, 0:8, ?TYPE_ACK:8, 0:32>>,
    {ok, Frame, <<>>} = imboy_frame:decode(Buf),
    ?assertEqual(99, imboy_frame:version(Frame)).

decode_frame_too_large_test() ->
    %% PayloadLen > 16MB 应直接报错
    Big = 16#1000001,
    Bad = <<?MAGIC:16, ?VERSION:8, 0:8, 1:8, Big:32, 0:8>>,
    ?assertEqual({error, frame_too_large}, imboy_frame:decode(Bad)).

encode_oversized_payload_test() ->
    %% 编码时超出 MAX_FRAME_SIZE 应 crash（badarg 或类似）
    %% 使用较小的 payload 验证正常路径；超限路径用 catch 包裹
    TooBig = binary:copy(<<0>>, 16#1000001),
    ?assertError(frame_too_large,
                 imboy_frame:encode(?TYPE_MSG_C2C, 0, TooBig)).

%%%===================================================================
%%% 流式解码 (粘包拆分)
%%%===================================================================

decode_stream_multiple_frames_test() ->
    F1 = imboy_frame:encode(?TYPE_HEARTBEAT_PING, ?FLAG_PING, <<1:16>>),
    F2 = imboy_frame:encode(?TYPE_ACK, ?FLAG_NONE, <<100:64>>),
    F3 = imboy_frame:encode(?TYPE_MSG_C2C, ?FLAG_ACK, <<"abc">>),
    Buf = <<F1/binary, F2/binary, F3/binary>>,

    {Frames, Remaining} = imboy_frame:decode_stream(Buf, []),
    ?assertEqual(<<>>, Remaining),
    ?assertEqual(3, length(Frames)),
    [Fm1, Fm2, Fm3] = Frames,
    ?assertEqual(?TYPE_HEARTBEAT_PING, imboy_frame:type(Fm1)),
    ?assertEqual(?TYPE_ACK, imboy_frame:type(Fm2)),
    ?assertEqual(?TYPE_MSG_C2C, imboy_frame:type(Fm3)),
    ?assertEqual(<<"abc">>, imboy_frame:payload(Fm3)).

decode_stream_trailing_incomplete_test() ->
    %% 两个完整帧 + 一个不完整帧 → 返回 2 帧 + 剩余 buffer
    F1 = imboy_frame:encode(?TYPE_ACK, 0, <<1:64>>),
    F2 = imboy_frame:encode(?TYPE_ACK, 0, <<2:64>>),
    Partial = <<?MAGIC:16, ?VERSION:8, 0:8, ?TYPE_MSG_C2C:8, 100:32, "xx">>,
    Buf = <<F1/binary, F2/binary, Partial/binary>>,

    {Frames, Rest} = imboy_frame:decode_stream(Buf, []),
    ?assertEqual(2, length(Frames)),
    ?assertEqual(Partial, Rest).

decode_stream_empty_test() ->
    ?assertEqual({[], <<>>}, imboy_frame:decode_stream(<<>>, [])).

%%%===================================================================
%%% Flags 辅助函数
%%%===================================================================

flags_helpers_test() ->
    Frame = make_frame(?TYPE_MSG_C2C, ?FLAG_CMP bor ?FLAG_ACK bor 16#02, <<>>),
    ?assert(imboy_frame:is_compressed(Frame)),
    ?assert(imboy_frame:needs_ack(Frame)),
    ?assertNot(imboy_frame:is_encrypted(Frame)),
    ?assertEqual(2, imboy_frame:priority(Frame)).

flags_none_test() ->
    Frame = make_frame(?TYPE_ACK, 0, <<>>),
    ?assertNot(imboy_frame:is_compressed(Frame)),
    ?assertNot(imboy_frame:needs_ack(Frame)),
    ?assertNot(imboy_frame:is_encrypted(Frame)),
    ?assertEqual(0, imboy_frame:priority(Frame)).

%%%===================================================================
%%% 构造器快捷方式
%%%===================================================================

heartbeat_ping_test() ->
    Bin = imboy_frame:heartbeat_ping(7),
    {ok, Frame, <<>>} = imboy_frame:decode(Bin),
    ?assertEqual(?TYPE_HEARTBEAT_PING, imboy_frame:type(Frame)),
    ?assertEqual(<<7:16>>, imboy_frame:payload(Frame)).

heartbeat_pong_test() ->
    Bin = imboy_frame:heartbeat_pong(7),
    {ok, Frame, <<>>} = imboy_frame:decode(Bin),
    ?assertEqual(?TYPE_HEARTBEAT_PONG, imboy_frame:type(Frame)),
    ?assertEqual(<<7:16>>, imboy_frame:payload(Frame)).

ack_helper_test() ->
    MsgId = 16#1234567890ABCDEF,
    Bin = imboy_frame:ack(MsgId),
    {ok, Frame, <<>>} = imboy_frame:decode(Bin),
    ?assertEqual(?TYPE_ACK, imboy_frame:type(Frame)),
    ?assertEqual(<<MsgId:64>>, imboy_frame:payload(Frame)).

%%%===================================================================
%%% Helpers
%%%===================================================================

make_frame(Type, Flags, Payload) ->
    Bin = imboy_frame:encode(Type, Flags, Payload),
    {ok, Frame, <<>>} = imboy_frame:decode(Bin),
    Frame.
