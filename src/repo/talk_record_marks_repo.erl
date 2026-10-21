-module(talk_record_marks_repo).
-moduledoc "面聊重点标记仓储（T07：talk_record_mark；迁移 00000211+00000213）。".
%%%
% talk_record_mark 数据仓库层。
%
% 00000213 裁决落实（§11.1 + Q-T04 张力）：
%   * 真源：capture_id（可空，复合 FK 同记录）+ capture_offset_ms（有效音频
%     时间轴位置）；
%   * position_ms 保留为 V1 单 capture 投影——写路径同时维护两者
%     （V1 position_ms := capture_offset_ms），读路径以 offset 为准投影；
%   * client_mark_id 幂等：UNIQUE(record_id, author_id, client_mark_id)；
%   * revision：mark 自身版本域（PATCH/DELETE If-Match，§11.2）。
%%%

-export([tablename/0]).
-export([insert_tx/2]).
-export([find_tx/3, find_by_client_id_tx/4, page_by_record_tx/3]).
-export([update_tx/4, delete_tx/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MARK_COLUMNS,
    <<"m.id,m.record_id,m.author_id,m.client_mark_id,m.capture_id,",
        "m.capture_offset_ms,m.position_ms,m.text,m.revision,m.created_at,m.updated_at">>
).

%% ===================================================================
%% API
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"talk_record_mark">>).

%% @doc 事务内插入 mark。唯一键 (record_id, author_id, client_mark_id) 冲突 →
%% {error, unique_violation}（上层转幂等重放/冲突判定）。
-spec insert_tx(any(), map()) -> {ok, integer()} | {error, unique_violation | term()}.
insert_tx(Conn, Data) ->
    Id = maps:get(<<"id">>, Data),
    {Sql0, Params} = elib_pg_sql:insert(tablename(), Data),
    Sql = <<Sql0/binary, " ON CONFLICT DO NOTHING">>,
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, 1} ->
            {ok, Id};
        {ok, 0} ->
            {error, unique_violation};
        {error, Reason} ->
            case sqlstate(Reason) of
                <<"23505">> -> {error, unique_violation};
                _ -> {error, Reason}
            end
    end.

%% @doc 按主键取 mark（record_id + mark_id 复合匹配，§8）。
-spec find_tx(any(), integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, RecordId, MarkId) ->
    Sql =
        <<"SELECT ", ?MARK_COLUMNS/binary, " FROM talk_record_mark m",
            " WHERE m.record_id = $1 AND m.id = $2">>,
    case elib_pg:query(Conn, Sql, [RecordId, MarkId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 幂等键查找（record+author+client_mark_id，§2 离线去重）。
-spec find_by_client_id_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_by_client_id_tx(Conn, RecordId, AuthorId, ClientMarkId) ->
    Sql =
        <<"SELECT ", ?MARK_COLUMNS/binary, " FROM talk_record_mark m",
            " WHERE m.record_id = $1 AND m.author_id = $2 AND m.client_mark_id = $3">>,
    case elib_pg:query(Conn, Sql, [RecordId, AuthorId, ClientMarkId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 记录 marks 清单（position_ms/id 正序，§4；含全部作者，跟随记录 ACL）。
-spec page_by_record_tx(any(), integer(), pos_integer()) -> {ok, [map()]} | {error, term()}.
page_by_record_tx(Conn, RecordId, Limit) ->
    Sql =
        <<"SELECT ", ?MARK_COLUMNS/binary, " FROM talk_record_mark m",
            " WHERE m.record_id = $1 ORDER BY m.position_ms ASC, m.id ASC LIMIT $2">>,
    elib_pg:query(Conn, Sql, [RecordId, Limit]).

%% @doc mark 编辑 CAS（text/capture_offset_ms/position_ms；revision+1）。
%% 命中 0 行由上层区分 not_found / precondition_failed。
-spec update_tx(any(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, not_found | term()}.
update_tx(Conn, MarkId, ExpectedRevision, Sets) ->
    Ordered = [
        K
     || K <- [<<"text">>, <<"capture_id">>, <<"capture_offset_ms">>, <<"position_ms">>],
        maps:is_key(K, Sets)
    ],
    Sets1 = [{K, maps:get(K, Sets)} || K <- Ordered] ++ [{<<"updated_at">>, elib_dt:now()}],
    SetSql = join_set_clause(Sets1, 1),
    Base = length(Sets1) + 1,
    Sql =
        <<"UPDATE talk_record_mark m SET ", SetSql/binary, ", revision = revision + 1",
            " WHERE m.id = $", (integer_to_binary(Base))/binary, " AND m.revision = $",
            (integer_to_binary(Base + 1))/binary, " RETURNING ", ?MARK_COLUMNS/binary>>,
    Params = [V || {_, V} <- Sets1] ++ [MarkId, ExpectedRevision],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc mark 删除 CAS（If-Match revision 命中才删）。
-spec delete_tx(any(), integer(), pos_integer()) -> {ok, deleted} | {error, not_found | term()}.
delete_tx(Conn, MarkId, ExpectedRevision) ->
    Sql = <<"DELETE FROM talk_record_mark m", " WHERE m.id = $1 AND m.revision = $2">>,
    case elib_pg:execute_count(Conn, Sql, [MarkId, ExpectedRevision]) of
        {ok, 1} -> {ok, deleted};
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec join_set_clause([{binary(), term()}], pos_integer()) -> binary().
join_set_clause(Sets, Start) ->
    {Parts, _N} =
        lists:mapfoldl(
            fun({Col, _V}, N) ->
                {<<Col/binary, " = $", (integer_to_binary(N))/binary>>, N + 1}
            end,
            Start,
            Sets
        ),
    iolist_to_binary(join_binary(Parts, <<", ">>)).

join_binary([], _Sep) ->
    [];
join_binary([P], _Sep) ->
    [P];
join_binary([P | Rest], Sep) ->
    [P, Sep | join_binary(Rest, Sep)].

-spec sqlstate(term()) -> binary() | undefined.
sqlstate({error, Inner}) when
    is_tuple(Inner), tuple_size(Inner) >= 3, element(1, Inner) =:= error
->
    case element(3, Inner) of
        Code when is_binary(Code), byte_size(Code) =:= 5 -> Code;
        _ -> undefined
    end;
sqlstate(_) ->
    undefined.
