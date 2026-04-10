%%%-------------------------------------------------------------------
%%% @doc
%%% IMBoy 分层二进制协议帧编解码 (v2)
%%%
%%% 职责：
%%%   - 将业务 payload 包裹为紧凑二进制帧
%%%   - 从 TCP/WebSocket 二进制流解析出帧（支持粘包/拆包）
%%%   - 提供控制帧（心跳、ACK）的快捷构造器
%%%
%%% 帧格式:
%%%   +----------+------+------+------+-----------+---------+
%%%   | Magic(2) |Ver(1)|Flg(1)|Typ(1)|  Len(4)   | Payload |
%%%   +----------+------+------+------+-----------+---------+
%%%     0xIB01    0x02   bits  u8       uint32 BE   variable
%%%
%%% 设计原则:
%%%   - 无外部依赖（不依赖 ezstd/jsone/gpb）
%%%   - 零拷贝：利用 Erlang 二进制模式匹配
%%%   - 压缩解耦：CMP flag 保留给调用方按需处理
%%%   - 加密解耦：ENC flag 保留给未来 E2EE 扩展
%%%
%%% 规范: .claude/plans/imboy-frame-protocol.md
%%% @end
%%%-------------------------------------------------------------------
-module(imboy_frame).

-include("imboy_frame.hrl").

%% 编解码 API
-export([
    encode/3,
    decode/1,
    decode_stream/2
]).

%% 帧访问器
-export([
    version/1,
    flags/1,
    type/1,
    payload/1
]).

%% Flags 判定
-export([
    is_compressed/1,
    is_encrypted/1,
    needs_ack/1,
    priority/1
]).

%% 快捷构造器
-export([
    heartbeat_ping/1,
    heartbeat_pong/1,
    ack/1,
    nack/1
]).

-export_type([frame/0]).

-type frame() :: #imboy_frame{}.

%%%===================================================================
%%% 编解码 API
%%%===================================================================

%% @doc 编码一个帧为二进制
%%
%% 调用者负责（如需要）对 Payload 进行压缩并设置 CMP flag。
%% 本函数只做 framing，不做任何内容变换。
%%
%% @param Type   帧类型 (0..255)
%% @param Flags  帧标志位 (0..255)
%% @param Payload 载荷二进制
%% @returns 完整帧二进制
-spec encode(0..255, 0..255, binary()) -> binary().
encode(Type, Flags, Payload)
  when is_integer(Type), Type >= 0, Type =< 255,
       is_integer(Flags), Flags >= 0, Flags =< 255,
       is_binary(Payload) ->
    Len = byte_size(Payload),
    case Len > ?IMBOY_FRAME_MAX_PAYLOAD of
        true  -> error(frame_too_large);
        false -> ok
    end,
    <<?IMBOY_FRAME_MAGIC:16,
      ?IMBOY_FRAME_VERSION:8,
      Flags:8,
      Type:8,
      Len:32/big-unsigned,
      Payload/binary>>.

%% @doc 解码一个帧
%%
%% 返回：
%%   {ok, Frame, Rest}   —— 解析成功，Rest 为剩余未消费字节
%%   {more, Buffer}      —— 数据不足，调用者需等待更多字节再次调用
%%   {error, Reason}     —— 协议错误（bad_magic / frame_too_large）
%%
%% 注意：本函数不处理 CMP/ENC，payload 原样返回。
-spec decode(binary()) ->
    {ok, frame(), binary()} | {more, binary()} | {error, atom()}.
decode(<<?IMBOY_FRAME_MAGIC:16, Ver:8, Flags:8, Type:8, Len:32/big-unsigned, Rest/binary>> = Buf)
  when Len =< ?IMBOY_FRAME_MAX_PAYLOAD ->
    case Rest of
        <<Payload:Len/binary, Tail/binary>> ->
            Frame = #imboy_frame{
                version = Ver,
                flags   = Flags,
                type    = Type,
                payload = Payload
            },
            {ok, Frame, Tail};
        _ ->
            {more, Buf}
    end;
decode(<<?IMBOY_FRAME_MAGIC:16, _Ver:8, _Flags:8, _Type:8, Len:32/big-unsigned, _/binary>>)
  when Len > ?IMBOY_FRAME_MAX_PAYLOAD ->
    {error, frame_too_large};
decode(<<?IMBOY_FRAME_MAGIC:16, _/binary>> = Buf) ->
    %% 魔数匹配但头部/载荷不完整
    {more, Buf};
decode(<<>> = Buf) ->
    {more, Buf};
decode(<<B1:8>> = Buf) when B1 =:= (?IMBOY_FRAME_MAGIC bsr 8) ->
    %% 恰好 1 字节且等于魔数高字节，无法判断是否合法，需等下一字节
    {more, Buf};
decode(_) ->
    {error, bad_magic}.

%% @doc 流式解码：从 buffer 中尽可能多地解析完整帧
%%
%% 返回 {[Frame], RemainingBuf}，Frames 按顺序排列。
%%
%% 错误语义（重要）：
%%   一旦遇到 {error, Reason}（如 bad_magic 或 frame_too_large），
%%   本函数直接返回该错误，已经解析的 Frame 会被丢弃。
%%
%%   这是故意的设计：在 WebSocket binary frame 边界之上，每个 WS
%%   frame 已经是一个完整的协议帧，出现 bad_magic 意味着对端协议
%%   错误，调用方应关闭连接而不是尝试重同步。
%%
%%   如果将来要在 TCP 原始字节流上使用，需要额外提供 resync 语义。
-spec decode_stream(binary(), [frame()]) ->
    {[frame()], binary()} | {error, atom()}.
decode_stream(Buf, Acc) ->
    case decode(Buf) of
        {ok, Frame, Rest} ->
            decode_stream(Rest, [Frame | Acc]);
        {more, Remaining} ->
            {lists:reverse(Acc), Remaining};
        {error, _} = E ->
            E
    end.

%%%===================================================================
%%% 帧访问器
%%%===================================================================

-spec version(frame()) -> 0..255.
version(#imboy_frame{version = V}) -> V.

-spec flags(frame()) -> 0..255.
flags(#imboy_frame{flags = F}) -> F.

-spec type(frame()) -> 0..255.
type(#imboy_frame{type = T}) -> T.

-spec payload(frame()) -> binary().
payload(#imboy_frame{payload = P}) -> P.

%%%===================================================================
%%% Flags 判定
%%%===================================================================

-spec is_compressed(frame()) -> boolean().
is_compressed(#imboy_frame{flags = F}) ->
    (F band ?FRAME_FLAG_CMP) =/= 0.

-spec is_encrypted(frame()) -> boolean().
is_encrypted(#imboy_frame{flags = F}) ->
    (F band ?FRAME_FLAG_ENC) =/= 0.

-spec needs_ack(frame()) -> boolean().
needs_ack(#imboy_frame{flags = F}) ->
    (F band ?FRAME_FLAG_ACK) =/= 0.

-spec priority(frame()) -> 0..7.
priority(#imboy_frame{flags = F}) ->
    F band ?FRAME_FLAG_PRI_MASK.

%%%===================================================================
%%% 快捷构造器
%%%===================================================================

%% @doc 心跳 Ping 帧（最高优先级 + 请求 ACK）
-spec heartbeat_ping(0..65535) -> binary().
heartbeat_ping(Seq) when is_integer(Seq), Seq >= 0, Seq =< 65535 ->
    %% Flags = ACK (bit5) | PRI 7 (bits0-2) = 0xE0... wait
    %% PRI 7 = 0b111 = 0x07, ACK = 0x20 → 0x27
    %% 规范文档写的是 0xE0 但那是把 PRI 放在高 3 位，这里按 hrl 定义 PRI 为低 3 位
    Flags = ?FRAME_FLAG_ACK bor 7,
    encode(?FRAME_TYPE_HEARTBEAT_PING, Flags, <<Seq:16/big-unsigned>>).

%% @doc 心跳 Pong 帧（回显 seq）
-spec heartbeat_pong(0..65535) -> binary().
heartbeat_pong(Seq) when is_integer(Seq), Seq >= 0, Seq =< 65535 ->
    Flags = 7,  %% PRI=7 无 ACK
    encode(?FRAME_TYPE_HEARTBEAT_PONG, Flags, <<Seq:16/big-unsigned>>).

%% @doc ACK 帧
-spec ack(non_neg_integer()) -> binary().
ack(MsgId) when is_integer(MsgId), MsgId >= 0 ->
    encode(?FRAME_TYPE_ACK, 0, <<MsgId:64/big-unsigned>>).

%% @doc NACK 帧
-spec nack(non_neg_integer()) -> binary().
nack(MsgId) when is_integer(MsgId), MsgId >= 0 ->
    encode(?FRAME_TYPE_NACK, 0, <<MsgId:64/big-unsigned>>).
