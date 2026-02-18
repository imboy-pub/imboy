-module(msg_forward_repo).
%%%
% msg_forward_repo 是 msg_forward repository 缩写
% 消息转发记录数据仓库层，提供消息转发记录的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([insert/1]).
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


%% @doc 插入转发记录
%% @param ForwardRecord 转发记录映射
%% @return {ok, Count} | {error, Reason}
-spec insert(map()) -> {ok, non_neg_integer()} | {error, term()}.
insert(ForwardRecord) when is_map(ForwardRecord) ->
    Tb = tablename(),
    OriginalMsgId = maps:get(<<"original_msg_id">>, ForwardRecord),
    OriginalFromId = maps:get(<<"original_from_id">>, ForwardRecord),
    OriginalToId = maps:get(<<"original_to_id">>, ForwardRecord),
    OriginalType = maps:get(<<"original_type">>, ForwardRecord),
    ForwardMsgId = maps:get(<<"forward_msg_id">>, ForwardRecord),
    ForwardFromId = maps:get(<<"forward_from_id">>, ForwardRecord),
    ForwardToId = maps:get(<<"forward_to_id">>, ForwardRecord),
    ForwardType = maps:get(<<"forward_type">>, ForwardRecord),

    Sql = <<"INSERT INTO ", Tb/binary,
            " (original_msg_id, original_from_id, original_to_id, original_type, ",
            "forward_msg_id, forward_from_id, forward_to_id, forward_type) "
            "VALUES ($1, $2, $3, $4, $5, $6, $7, $8)">>,

    case elib_pg:query(Sql, [OriginalMsgId, OriginalFromId, OriginalToId, OriginalType,
                              ForwardMsgId, ForwardFromId, ForwardToId, ForwardType]) of
        {ok, _} -> {ok, 1};
        {error, Reason} -> {error, Reason}
    end.


%% @doc 根据原始消息ID查找转发记录
%% @param MsgId 原始消息ID
%% @return {ok, Rows} | {error, Reason}
-spec find_by_original_msg_id(binary()) -> {ok, list(map())} | {error, term()}.
find_by_original_msg_id(MsgId) when is_binary(MsgId) ->
    Tb = tablename(),
    Sql = <<"SELECT original_msg_id, original_from_id, original_to_id, original_type, ",
            "forward_msg_id, forward_from_id, forward_to_id, forward_type, created_at ",
            "FROM ", Tb/binary, " WHERE original_msg_id = $1 ORDER BY created_at DESC">>,

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
            Sql = <<"SELECT original_msg_id, original_from_id, original_to_id, original_type, ",
                    "forward_msg_id, forward_from_id, forward_to_id, forward_type, created_at ",
                    "FROM ", Tb/binary, " WHERE forward_from_id = $1 ",
                    "ORDER BY created_at DESC LIMIT $2">>,
            elib_pg:query(Sql, [FromUid, Limit]);
        false ->
            Sql = <<"SELECT original_msg_id, original_from_id, original_to_id, original_type, ",
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
