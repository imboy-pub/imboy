-module(msg_c2g_reply_repo).
%%%
%%% msg_c2g_reply_repo —— C2G 引用回复簇（自 msg_c2g_repo 拆出，质量门 ≤800 行）
%%%
%%% write_msg_with_reply 家族（当前表无 reply 列：reply 字段按既有语义忽略，
%%% 直写复用 msg_c2g_repo:write_msg_impl）与 find_by_reply_to_msg_id 占位。
%%% 行为逐字未动。
%%%

-export([write_msg_with_reply/11, write_msg_with_reply/12, write_msg_with_reply/13]).
-export([write_msg_with_reply/14]).
-export([find_by_reply_to_msg_id/1]).

%% @doc 写入带引用回复信息的C2G离线消息
%% 注意：当前数据库表不支持 reply_to_msg_id 等字段，此函数忽略回复信息
%% @param CreatedAt 消息创建时间（RFC3339 binary）
%% @param Id 消息唯一ID
%% @param Payload 消息载荷（JSON binary）
%% @param FromId 发送者用户ID（integer，对应 bigint 列）
%% @param ToUids 接收者用户ID列表
%% @param Gid 群组ID
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON map，可选）
%% @param _ReplyToMsgId 被引用回复的消息ID（暂不支持）
%% @param _ReplyToFromId 被引用消息的发送者ID（暂不支持）
%% @param _ReplySnippet 被引用消息的摘要（暂不支持）
%% @return ok | {error, Reason}
-spec write_msg_with_reply(
    %% CreatedAt：impl 内部 to_rfc3339 归一（同 write_msg 的 CreatedAtRaw 契约）
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    [integer()],
    integer(),
    binary(),
    %% E2EE：对象或客户端直传的 JSON 字符串
    map() | binary() | null,
    binary(),
    integer(),
    binary()
) -> ok | {error, term()}.
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    FromId,
    ToUids,
    Gid,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet
) ->
    write_msg_with_reply(
        CreatedAt,
        Id,
        Payload,
        FromId,
        ToUids,
        Gid,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet,
        null
    ).

-spec write_msg_with_reply(
    %% CreatedAt：impl 内部 to_rfc3339 归一（同 write_msg 的 CreatedAtRaw 契约）
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    [integer()],
    integer(),
    binary(),
    map() | binary() | null,
    binary(),
    integer(),
    binary(),
    pos_integer() | null
) -> ok | {error, term()}.
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    FromId,
    ToUids,
    Gid,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet,
    ConvSeq
) ->
    %% R03/D04：兼容旧入口，不带接受时间。
    write_msg_with_reply(
        CreatedAt,
        Id,
        Payload,
        FromId,
        ToUids,
        Gid,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet,
        ConvSeq,
        undefined
    ).

%% @doc R03/D04 + R05-A 兼容旧入口（/13）：无自毁期限。
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    FromId,
    ToUids,
    Gid,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet,
    ConvSeq,
    AcceptedAt
) ->
    write_msg_with_reply(
        CreatedAt,
        Id,
        Payload,
        FromId,
        ToUids,
        Gid,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet,
        ConvSeq,
        AcceptedAt,
        null
    ).

%% @doc R03/D04 + R05-A（IMBOY-TIMEZONE-REPAIR-V1）：带接受时间与自毁期限的
%% 引用回复直写。AcceptedAt/ExpireAt 与 staging 行同一值；ON CONFLICT 补洞
%% 子句只回填 reply 三列，不触碰 accepted_at/server_ts/expire_at。
-spec write_msg_with_reply(
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    [integer()],
    integer(),
    binary(),
    map() | binary() | null,
    binary(),
    integer(),
    binary(),
    pos_integer() | null,
    binary() | null | undefined,
    binary() | null
) -> ok | {error, term()}.
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    FromId,
    ToUids,
    Gid,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet,
    ConvSeq,
    AcceptedAt,
    ExpireAt
) ->
    %% RT-P3-03（2026-08-27）：表自建库迁移 00000006 起即有 reply 三列，
    %% 原"不支持"注释系误记导致引用元数据被静默丢弃；改走真实现。
    msg_c2g_repo:write_msg_impl(
        CreatedAt,
        Id,
        Payload,
        FromId,
        ToUids,
        Gid,
        MsgType,
        E2EE,
        ExpireAt,
        ConvSeq,
        {ReplyToMsgId, ReplyToFromId, ReplySnippet},
        null,
        current_state,
        AcceptedAt
    ).

%% @doc 根据被引用消息ID查找所有回复消息
%% 注意：当前数据库表不支持 reply_to_msg_id 字段，此函数返回空列表
%% @param _ReplyToMsgId 被引用的消息ID
%% @return {ok, []}
-spec find_by_reply_to_msg_id(binary()) -> {ok, list(map())} | {error, term()}.
find_by_reply_to_msg_id(_ReplyToMsgId) ->
    % 当前数据库表不支持 reply_to_msg_id 字段
    {ok, []}.
