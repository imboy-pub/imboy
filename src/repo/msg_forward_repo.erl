-module(msg_forward_repo).
%%%
% msg_forward_repo 是 msg_forward repository 缩写
% 消息转发记录数据仓库层，提供消息转发记录的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([insert/1, insert_tx/2, source_group_visible/3, source_group_visible_tx/4]).
-export([find_by_original_msg_id/1]).
-export([find_by_forward_from_id/2]).
-export([count_by_original_msg_id/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取消息转发表的表名
%% @return 返回消息转发表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_forward">>).

%% Current membership alone does not grant access to messages from an older generation.
source_group_visible(MsgId, Gid, Uid) ->
    elib_pg:with_conn(fun(C) -> source_group_visible_tx(C, MsgId, Gid, Uid) end).

source_group_visible_tx(Conn, MsgId, Gid, Uid) ->
    Entitlement = micro_community_access_repo:predicate_sql(<<"grp.id">>, <<"$3">>),
    Scope = organization_resource_access_repo:group_predicate_sql(<<"grp.id">>, <<"$3">>),
    Sql = <<
        "SELECT EXISTS (SELECT 1 FROM public.msg_c2g_recipient_snapshot s"
        " JOIN public.msg_c2g m ON m.msg_id=s.msg_id AND m.to_id=s.to_gid AND m.from_id=s.from_id"
        " AND (m.expire_at IS NULL OR m.expire_at>now())"
        " AND m.payload->>'action' IS DISTINCT FROM 'message_revoke_ack'"
        " AND NOT EXISTS (SELECT 1 FROM public.c2g_revoke_fact r"
        " WHERE r.group_id=s.to_gid AND r.original_msg_id=s.msg_id)"
        " JOIN public.\"group\" grp ON grp.id=s.to_gid AND grp.status=1"
        " JOIN public.group_member gm ON gm.group_id=grp.id AND gm.user_id=$3 AND gm.status=1"
        " JOIN public.group_member_generation gmg ON gmg.group_id=grp.id AND gmg.user_id=$3"
        " AND gmg.end_seq IS NULL AND gmg.start_seq<=s.conv_seq"
        " WHERE s.msg_id=$1 AND s.to_gid=$2 AND $3=ANY(s.recipient_uids) AND ",
        Entitlement/binary,
        " AND ",
        Scope/binary,
        ") AS allowed"
    >>,
    case elib_pg:query(Conn, Sql, [MsgId, Gid, Uid]) of
        {ok, [#{<<"allowed">> := true}]} -> true;
        _ -> false
    end.

%% @doc 插入转发记录
%% @param ForwardRecord 转发记录映射
%% @return {ok, Count} | {error, Reason}
-spec insert(map()) -> {ok, non_neg_integer()} | {error, term()}.
insert(ForwardRecord) when is_map(ForwardRecord) ->
    insert_with(fun(Sql, Params) -> elib_pg:query(Sql, Params) end, ForwardRecord).

insert_tx(Conn, ForwardRecord) when is_map(ForwardRecord) ->
    insert_with(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, ForwardRecord).

insert_with(Query, ForwardRecord) ->
    Tb = tablename(),
    OriginalMsgId = maps:get(<<"original_msg_id">>, ForwardRecord),
    OriginalFromId = maps:get(<<"original_from_id">>, ForwardRecord),
    OriginalToId = maps:get(<<"original_to_id">>, ForwardRecord),
    OriginalType = maps:get(<<"original_type">>, ForwardRecord),
    ForwardMsgId = maps:get(<<"forward_msg_id">>, ForwardRecord),
    ForwardFromId = maps:get(<<"forward_from_id">>, ForwardRecord),
    ForwardToId = maps:get(<<"forward_to_id">>, ForwardRecord),
    ForwardType = maps:get(<<"forward_type">>, ForwardRecord),

    Id = elib_tsid:generate(msg_forward),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, original_msg_id, original_from_id, original_to_id, original_type, ",
            "forward_msg_id, forward_from_id, forward_to_id, forward_type) "
            "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)">>,

    case
        Query(Sql, [
            Id,
            OriginalMsgId,
            OriginalFromId,
            OriginalToId,
            OriginalType,
            ForwardMsgId,
            ForwardFromId,
            ForwardToId,
            ForwardType
        ])
    of
        {ok, _} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据原始消息ID查找转发记录
%% @param MsgId 原始消息ID
%% @return {ok, Rows} | {error, Reason}
-spec find_by_original_msg_id(binary()) -> {ok, list(map())} | {error, term()}.
find_by_original_msg_id(MsgId) when is_binary(MsgId) ->
    Tb = tablename(),
    Sql =
        <<"SELECT original_msg_id, original_from_id, original_to_id, original_type, ",
            "forward_msg_id, forward_from_id, forward_to_id, forward_type, created_at ", "FROM ",
            Tb/binary, " WHERE original_msg_id = $1 ORDER BY created_at DESC">>,

    case elib_pg:query(Sql, [MsgId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据转发者ID查找转发记录
%% @param FromUid 转发者用户ID
%% @param Limit 查询结果数量限制，0 表示不限制
%% @return {ok, Rows} | {error, Reason}
-spec find_by_forward_from_id(integer(), non_neg_integer()) -> {ok, list(map())} | {error, term()}.
find_by_forward_from_id(FromUid, Limit) when is_integer(FromUid), is_integer(Limit) ->
    Tb = tablename(),
    case Limit > 0 of
        true ->
            Sql =
                <<"SELECT original_msg_id, original_from_id, original_to_id, original_type, ",
                    "forward_msg_id, forward_from_id, forward_to_id, forward_type, created_at ",
                    "FROM ", Tb/binary, " WHERE forward_from_id = $1 ",
                    "ORDER BY created_at DESC LIMIT $2">>,
            elib_pg:query(Sql, [FromUid, Limit]);
        false ->
            Sql =
                <<"SELECT original_msg_id, original_from_id, original_to_id, original_type, ",
                    "forward_msg_id, forward_from_id, forward_to_id, forward_type, created_at ",
                    "FROM ", Tb/binary, " WHERE forward_from_id = $1 ",
                    "ORDER BY created_at DESC">>,
            elib_pg:query(Sql, [FromUid])
    end.

%% @doc 统计原始消息的转发次数
%% @param MsgId 原始消息ID
%% @return {ok, Count} | {error, Reason}
-spec count_by_original_msg_id(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_by_original_msg_id(MsgId) when is_binary(MsgId) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary, " WHERE original_msg_id = $1">>,

    case elib_pg:query(Sql, [MsgId]) of
        {ok, [#{<<"count">> := Count}]} -> {ok, Count};
        {ok, []} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
