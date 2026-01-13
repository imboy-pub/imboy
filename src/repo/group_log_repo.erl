-module (group_log_repo).
%%%
% group_log 相关操作都放到该模块，存储库模块
% group_log related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([add/2]).
-export ([batch_add/2]).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取群日志表的表名
%% @return 返回群日志表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_log">>).

%% @doc 添加群日志
%% @param Conn 数据库连接
%% @param Data 日志数据map，包含 type, option_uid, group_id, body, created_at 等字段
%% @return {ok, Result} | {ok, Count, Result} | {error, Reason}
-spec add(epgsql:connection() | pid(), map()) -> {ok, term()} | {ok, term(), term()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:insert(Tb, Data, <<>>),
    elib_pg:execute(Conn, Sql, Params).

%% @doc 批量添加群日志
%% 使用 PostgreSQL 的 VALUES 语法批量插入，避免 N+1 查询问题
%% @param Conn 数据库连接
%% @param DataList 日志数据列表，每个元素为包含 type, option_uid, group_id, body, created_at 的map
%% @returns {ok, Affected} 添加成功返回影响行数 | {error, Reason} 添加失败
%% @details 当 DataList 为空列表时返回 {ok, 0}
-spec batch_add(epgsql:connection() | pid(), [map()]) -> {ok, non_neg_integer()} | {error, term()}.
batch_add(Conn, DataList) when is_list(DataList), length(DataList) > 0 ->
    Tb = tablename(),
    % 构建 VALUES 子句和参数列表
    {ValuesClause, AllParams} = build_values_clause(DataList, 1, <<>>, []),
    Sql = <<"INSERT INTO ", Tb/binary,
            " (type, option_uid, group_id, body, created_at) ",
            ValuesClause/binary>>,
    elib_pg:execute(Conn, Sql, AllParams);
batch_add(_Conn, []) ->
    {ok, 0}.

%% @private
%% @doc 构建 VALUES 子句（内部函数）
%% 使用安全的二进制拼接方式构建 SQL，避免使用 io_lib:format
%% @param DataList 日志数据列表
%% @param Index 起始参数索引
%% @param ValuesAcc 累积的 VALUES 子句
%% @param ParamsAcc 累积的参数列表
%% @return {ValuesClause, AllParams} VALUES 子句和完整参数列表
-spec build_values_clause([map()], integer(), binary(), [term()]) -> {binary(), [term()]}.
build_values_clause([], _Index, ValuesAcc, ParamsAcc) ->
    {ValuesAcc, lists:reverse(ParamsAcc)};
build_values_clause([Data | Rest], Index, ValuesAcc, ParamsAcc) ->
    Type = maps:get(type, Data),
    OptionUid = maps:get(option_uid, Data),
    GroupId = maps:get(group_id, Data),
    Body = maps:get(body, Data),
    CreatedAt = maps:get(created_at, Data),

    % 为当前索引构建参数占位符（使用安全的二进制拼接）
    BaseIndex = (Index - 1) * 5,
    N1 = integer_to_binary(BaseIndex + 1),
    N2 = integer_to_binary(BaseIndex + 2),
    N3 = integer_to_binary(BaseIndex + 3),
    N4 = integer_to_binary(BaseIndex + 4),
    N5 = integer_to_binary(BaseIndex + 5),
    ValueClause = <<"($", N1/binary, ", $", N2/binary, ", $", N3/binary,
                     ", $", N4/binary, ", $", N5/binary, ")">>,

    NewValuesAcc = case Index of
        1 -> <<ValuesAcc/binary, ValueClause/binary>>;
        _ -> <<ValuesAcc/binary, ", ", ValueClause/binary>>
    end,

    NewParamsAcc = [Type, OptionUid, GroupId, Body, CreatedAt | ParamsAcc],
    build_values_clause(Rest, Index + 1, NewValuesAcc, NewParamsAcc).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
