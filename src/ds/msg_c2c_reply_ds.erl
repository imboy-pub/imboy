-module(msg_c2c_reply_ds).
%%%
%%% msg_c2c_reply_ds —— C2C 引用回复簇（自 msg_c2c_ds 拆出，质量门 ≤800 行）
%%%
%%% write_msg_with_reply 家族（R03/D04、R05-A、IMBOY-TIMEZONE-REPAIR-V1）与
%%% 回复摘录/脱敏（visible_reply_snippets / public_reply_source /
%%% redact_reply_payload / redact_reply_fields）。存取原语仍在 msg_c2c_ds，
%%% 本模块直写走 msg_c2c_repo，行为逐字未动。
%%%

-export([write_msg_with_reply/11, write_msg_with_reply/12]).
-export([write_msg_with_reply/13, write_msg_with_reply/14]).
-export([visible_reply_snippets/1, redact_reply_payload/2, public_reply_source/2]).

%% ===================================================================
%% 引用回复功能
%% ===================================================================

%% @doc 存储带引用回复信息的点对点消息
%%
%% 将消息及其引用信息存储到数据库中
%%
%% @param CreatedAt 消息创建时间戳（integer 毫秒或 binary RFC3339）
%% @param Id 消息ID
%% @param Payload 消息内容（JSON binary）
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param ServerTS 服务器时间戳（integer 毫秒或 binary RFC3339）
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON map，可选）
%% @param ReplyToMsgId 被引用回复的消息ID
%% @param ReplyToFromId 被引用消息的发送者ID
%% @param ReplySnippet 被引用消息的摘要
%% @returns ok | {error, Reason} 数据库操作结果
-spec write_msg_with_reply(
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    integer(),
    binary() | integer(),
    binary(),
    map() | null,
    binary(),
    integer(),
    binary()
) ->
    ok | {error, term()}.
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    From,
    To,
    ServerTS,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet
) ->
    % R03/D04：兼容旧入口，不带接受时间（accepted_at 列保持 NULL）。
    write_msg_with_reply(
        CreatedAt,
        Id,
        Payload,
        From,
        To,
        ServerTS,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet,
        null
    ).

%% @doc R03/D04 + R05-A 兼容旧入口（/12）：无自毁期限。
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    From,
    To,
    ServerTS,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet,
    AcceptedAt
) ->
    write_msg_with_reply(
        CreatedAt,
        Id,
        Payload,
        From,
        To,
        ServerTS,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet,
        AcceptedAt,
        null
    ).

%% @doc R03/D04 + R05-A（IMBOY-TIMEZONE-REPAIR-V1）：带接受时间与自毁期限的
%% 引用回复直写。AcceptedAt/ExpireAt 为接受事务内固定值（RFC3339 binary），
%% 与 staging 行同一值；ON CONFLICT 补洞子句不触碰 accepted_at/expire_at。
-spec write_msg_with_reply(
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    integer(),
    binary() | integer(),
    binary(),
    map() | null,
    binary(),
    integer(),
    binary(),
    binary() | null,
    binary() | null
) ->
    ok | {error, term()}.
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    From,
    To,
    ServerTS,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet,
    AcceptedAt,
    ExpireAt
) ->
    write_msg_with_reply(
        CreatedAt,
        Id,
        Payload,
        From,
        To,
        ServerTS,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet,
        AcceptedAt,
        ExpireAt,
        null
    ).

write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    From,
    To,
    ServerTS,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet,
    AcceptedAt,
    ExpireAt,
    SenderDid
) ->
    % 统一转换时间戳为 RFC3339 binary 格式（timestamptz 列需要）
    CreatedAt2 = elib_dt:to_rfc3339(CreatedAt),
    ServerTS2 = elib_dt:to_rfc3339(ServerTS),
    % 检查并清理溢出消息
    ok = msg_c2c_ds:check_and_delete_overflow(To),
    msg_c2c_repo:write_msg_with_reply(
        CreatedAt2,
        Id,
        Payload,
        From,
        To,
        ServerTS2,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet,
        AcceptedAt,
        ExpireAt,
        SenderDid
    ).

visible_reply_snippets([]) ->
    #{};
visible_reply_snippets(Ids) ->
    case msg_c2c_repo:visible_reply_snippets(Ids) of
        {ok, Rows} ->
            maps:from_list([
                {{Id, From, To}, {SourceId, Author, Snippet}}
             || #{
                    <<"msg_id">> := Id,
                    <<"from_id">> := From,
                    <<"to_id">> := To,
                    <<"reply_to_msg_id">> := SourceId,
                    <<"reply_to_from_id">> := Author,
                    <<"reply_snippet">> := Snippet
                } <- Rows
            ]);
        {error, _} ->
            #{}
    end.

public_reply_source(<<"quote">>, {Id, _Author, Snippet}) when
    is_binary(Id), Id =/= <<>>, is_binary(Snippet)
->
    #{<<"reply_source_id">> => Id};
public_reply_source(<<"quote">>, _Snapshot) ->
    #{<<"reply_source_id">> => null};
public_reply_source(_MsgType, _Snapshot) ->
    #{}.

redact_reply_payload(Payload, Snippet) when is_map(Payload) ->
    Redacted = redact_reply_fields(Payload, Snippet),
    case maps:get(<<"metadata">>, Redacted, null) of
        Metadata when is_map(Metadata) ->
            Redacted#{<<"metadata">> := redact_reply_fields(Metadata, Snippet)};
        _ ->
            Redacted
    end;
redact_reply_payload(Payload, Snippet) when is_binary(Payload) ->
    case jsone:try_decode(Payload, [{object_format, map}]) of
        {ok, Map, <<>>} when is_map(Map) ->
            jsone:encode(redact_reply_payload(Map, Snippet), [native_utf8]);
        _ ->
            Payload
    end;
redact_reply_payload(Payload, _Snippet) ->
    Payload.

redact_reply_fields(
    #{<<"quote_msg">> := Quote} = Payload, {Id, Author, Snippet}
) when
    is_map(Quote),
    is_binary(Id),
    Id =/= <<>>,
    is_integer(Author),
    is_binary(Snippet),
    Snippet =/= <<>>
->
    (maps:without([<<"quote_msg_author_name">>], Payload))#{
        <<"quote_msg">> := #{
            <<"id">> => Id,
            <<"authorId">> => message_ds:envelope_id_to_binary(Author),
            <<"type">> => <<"text">>,
            <<"text">> => Snippet
        }
    };
redact_reply_fields(Payload, _Snippet) ->
    maps:without([<<"quote_msg">>, <<"quote_msg_author_name">>], Payload).
