%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_frame 分层二进制协议帧格式常量
%%%
%%% 设计规范: .claude/plans/imboy-frame-protocol.md
%%%
%%% 帧结构:
%%%   [Magic:16 = 0xIB01] [Ver:8] [Flags:8] [Type:8] [PayloadLen:32] [Payload:N]
%%%
%%% 头部固定 9 字节，载荷变长。
%%% @end
%%%-------------------------------------------------------------------

-ifndef(IMBOY_FRAME_HRL).
-define(IMBOY_FRAME_HRL, true).

%% 协议魔数 ("IB" ASCII = 0x4942) 与版本
-define(IMBOY_FRAME_MAGIC,   16#4942).
-define(IMBOY_FRAME_VERSION, 2).

%% 帧头字节数
-define(IMBOY_FRAME_HEADER_SIZE, 9).

%% 最大帧载荷 (16 MiB)
-define(IMBOY_FRAME_MAX_PAYLOAD, 16#1000000).

%%%===================================================================
%%% Flags 位定义
%%%===================================================================
%% bit7 CMP   : 载荷是否 zstd 压缩
%% bit6 ENC   : 载荷是否端到端加密 (预留)
%% bit5 ACK   : 是否需要 ACK
%% bit4-3 DIR : ACK 帧方向 (0=C2C 1=C2G 2=S2C 3=C2S)，仅 ACK 帧使用，其余帧保留 0
%% bit2-0 PRI : 优先级 0-7

-define(FRAME_FLAG_CMP,       16#80).
-define(FRAME_FLAG_ENC,       16#40).
-define(FRAME_FLAG_ACK,       16#20).
-define(FRAME_FLAG_DIR_MASK,  16#18).
-define(FRAME_FLAG_DIR_SHIFT, 3).
-define(FRAME_FLAG_PRI_MASK,  16#07).

%% ACK 方向编码值（写入 flags bit4-3）
-define(FRAME_DIR_C2C, 0).
-define(FRAME_DIR_C2G, 1).
-define(FRAME_DIR_S2C, 2).
-define(FRAME_DIR_C2S, 3).

%%%===================================================================
%%% Type 枚举
%%%===================================================================

%% 0x00-0x0F : 控制帧
-define(FRAME_TYPE_HEARTBEAT_PING, 16#01).
-define(FRAME_TYPE_HEARTBEAT_PONG, 16#02).
-define(FRAME_TYPE_ACK,            16#03).
-define(FRAME_TYPE_NACK,           16#04).
-define(FRAME_TYPE_CLOSE,          16#05).
-define(FRAME_TYPE_ERROR,          16#06).

%% 0x10-0x1F : 握手/认证
-define(FRAME_TYPE_HANDSHAKE_HELLO, 16#10).
-define(FRAME_TYPE_HANDSHAKE_AUTH,  16#11).
-define(FRAME_TYPE_HANDSHAKE_OK,    16#12).

%% 0x20-0x7F : 业务消息
-define(FRAME_TYPE_MSG_C2C,    16#20).
-define(FRAME_TYPE_MSG_C2G,    16#21).
-define(FRAME_TYPE_MSG_C2S,    16#22).
-define(FRAME_TYPE_MSG_S2C,    16#23).
-define(FRAME_TYPE_MSG_SYNC,   16#24).
-define(FRAME_TYPE_MSG_TYPING, 16#25).
-define(FRAME_TYPE_MSG_READ,   16#26).
-define(FRAME_TYPE_MSG_RECALL, 16#27).

%% 0x80-0xEF : RPC/扩展
-define(FRAME_TYPE_RPC_REQ, 16#80).
-define(FRAME_TYPE_RPC_RSP, 16#81).

%%%===================================================================
%%% 帧记录
%%%===================================================================

-record(imboy_frame, {
    version = ?IMBOY_FRAME_VERSION :: 0..255,
    flags   = 0                    :: 0..255,
    type                           :: 0..255,
    payload = <<>>                 :: binary()
}).

-endif.
