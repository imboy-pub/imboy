-module(enterprise_message_origin_repo).

%%%
% enterprise_message_origin_repo 是企业托管消息的**真实 Application origin**
% 账本仓储（FULL-02 / plan-full §3.1「存储必须同时保留真实 Application origin
% （不得只留 Human 痕迹）」；表由 migration 00000140 建立）。
%
%% 一行 = 一条企业托管消息的 origin 事实：
%   * application_id 恒非空（真实调用方 Application，审计 actor 不是 Human）；
%   * sender_kind=human 时 sender_user_id 必填（Human 痕迹与 Application 痕迹
%%    **同时**存在；缺一即 DB CHECK 23514）；
%   * non_e2ee 恒真（企业托管固定非 E2EE）；
%   * 行禁止物理删除（只增不减的 origin 证据）。
%% 与 msg_c2c/msg_c2g 同事务写入（logic 层 message accept 路径），使 origin 与
%% 消息主体原子：要么都在、要么都不在。
%%%

-export([tablename/0, next_id/0, insert_tx/8, count_for_msg_tx/3]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "id, conversation_kind, organization_id, application_id, sender_kind,"
    " sender_user_id, non_e2ee, msg_row_id, msg_id, created_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_message_origin">>).

%% @doc origin 账本命名空间 TSID（惰性注册，镜像 enterprise_external_identity_repo）。
-spec next_id() -> pos_integer().
next_id() ->
    case lists:member(enterprise_message_origin, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(enterprise_message_origin)
    end,
    elib_tsid:generate(enterprise_message_origin).

%% @doc 事务内写入 origin 行。ConversationKind ∈ direct|group；SenderKind ∈
%% application|human（human 必须给 sender_user_id）。形态非法由 DB CHECK 拒绝
%% （归一 {error, invalid_origin}），调用方按内部错误处理（程序错误）。
-spec insert_tx(
    any(),
    direct | group,
    integer(),
    integer(),
    application | human,
    undefined | integer(),
    pos_integer(),
    binary()
) ->
    {ok, map()} | {error, invalid_origin | term()}.
insert_tx(Conn, ConversationKind, OrgId, AppId, SenderKind, SenderUserId, MsgRowId, MsgId) when
    is_integer(OrgId), is_integer(AppId), is_integer(MsgRowId), MsgRowId > 0, is_binary(MsgId)
->
    Sql =
        <<"INSERT INTO ", (tablename())/binary,
            " (id, conversation_kind, organization_id, application_id, sender_kind,"
            "  sender_user_id, non_e2ee, msg_row_id, msg_id, created_at)",
            " VALUES ($1, $2, $3, $4, $5, $6, true, $7, $8, NOW())", " RETURNING ",
            ?COLUMNS/binary>>,
    Params = [
        next_id(),
        atom_to_binary(ConversationKind, utf8),
        OrgId,
        AppId,
        atom_to_binary(SenderKind, utf8),
        SenderUserId,
        MsgRowId,
        MsgId
    ],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, insert_empty_result};
        {error, #error{code = <<"23514">>}} ->
            {error, invalid_origin};
        {error, Reason} ->
            {error, Reason}
    end;
insert_tx(_Conn, _Kind, _OrgId, _AppId, _SenderKind, _SenderUserId, _MsgRowId, _MsgId) ->
    {error, invalid_origin}.

%% @doc 计数（诊断/负例用）：本 (org, app) 针对某消息行的 origin 行数。
-spec count_for_msg_tx(any(), integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_for_msg_tx(Conn, MsgRowId, MsgId) when is_integer(MsgRowId), is_binary(MsgId) ->
    Sql =
        <<"SELECT COUNT(*) AS n FROM ", (tablename())/binary,
            " WHERE msg_row_id = $1 AND msg_id = $2">>,
    case elib_pg:query(Conn, Sql, [MsgRowId, MsgId]) of
        {ok, [#{<<"n">> := N} | _]} -> {ok, N};
        {ok, []} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.
