-module(group_repo).
%%%
% group_repo 是 group repository 缩写
% 群组数据仓库层，提供群组数据的基础数据库操作
%%%

-export([tablename/0]).
-export([add/2]).
-export([create/1, find_by_gid/1]).
-export([find_by_id/2]).
-export([list_by_ids/2]).
-export([list_by_uid/2, list_by_uid/3]).
-export([page/2, page/4]).
-export([update/1]).
-export([update_owner_tx/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取群组表的表名
%% @return 返回群组表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group">>).

%% @doc 添加新群组
%% @doc 添加群组（使用连接）
%% @param Conn 数据库连接
%% @param Data 包含群组信息的map
%% @return {ok, GroupId} | {error, Reason} (返回插入的群组ID)
-spec add(any(), map()) -> {ok, integer()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(group_info),
    %% 同 user_repo:save/1 的修复：normalize_legacy_create_data/1 用 atom
    %% `id` key 承载调用方传入的 id/gid，这里若只无条件覆盖 binary
    %% <<"id">>，两个 key 类型不同会同时存在，elib_pg_sql:insert/2 拼出
    %% 的 INSERT 语句里 "id" 列重复两次，PG 报 42701（真库集成测试实测
    %% 复现：conversation_pin_delete_integration_tests 建群即崩）。
    Data1 = maps:remove(id, maps:remove(<<"id">>, Data)),
    Data2 = Data1#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 兼容旧接口：创建群组
-spec create(map()) -> ok | {error, term()}.
create(Data0) ->
    Data = normalize_legacy_create_data(Data0),
    case elib_pg:with_tx(fun(Conn) -> add(Conn, Data) end) of
        {ok, _Gid} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 兼容旧接口：按 gid 查询群组
-spec find_by_gid(integer() | binary()) -> {ok, map()} | {error, term()}.
find_by_gid(Gid) ->
    case find_by_id(Gid, <<"*">>) of
        #{} = Row when map_size(Row) > 0 ->
            {ok, Row};
        #{} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 根据群组ID查找群组信息
%% @param Gid 群组ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return Row 查询成功返回行数据（map） | {error, Reason} 查询失败
%% @example group_repo:find_by_id(1, <<"*">>).
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, any()}.
find_by_id(Gid, Column) when is_list(Gid); is_binary(Gid) ->
    find_by_id(ec_cnv:to_integer(Gid), Column);
find_by_id(Gid, Column) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => Gid}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据群组ID列表批量查询群组信息
%% @param Ids 群组ID列表
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Rows} 查询成功返回 map 列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_ids([1,2], <<"*">>).
-spec list_by_ids(list(integer() | binary()), binary()) -> {ok, list(map())} | {error, any()}.
list_by_ids(Ids, Column) when length(Ids) > 0 ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => {in, Ids}}, #{}),
    case elib_pg:query(Sql, Params) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            {error, Reason}
    end;
list_by_ids([], _Column) ->
    {ok, []}.

%% @doc 查询用户创建的群组列表（使用默认限制10000）
%% @param Uid 用户ID（群组所有者）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_uid(1, <<"*">>).
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).

%% @doc 查询用户创建的群组列表（指定限制数量）
%% @param Uid 用户ID（群组所有者）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example group_repo:list_by_uid(1, <<"*">>).
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{owner_uid => Uid, status => 1}, #{
        limit => Limit
    }),
    elib_pg:query(Sql, Params).

%% @doc 分页查询群组
-spec page(integer(), integer()) -> {ok, map()} | {error, any()}.
page(Page, Size) ->
    page(Page, Size, #{}, <<"created_at DESC">>).

%% @doc 分页查询群组（带条件）
-spec page(integer(), integer(), map(), binary()) -> {ok, map()} | {error, any()}.
page(Page, Size, Where, OrderBy) ->
    Tb = tablename(),
    Column =
        <<"id,title,avatar,owner_uid,creator_uid,type,join_limit,member_count,introduction,status,created_at">>,
    elib_pg:page_with_total(Tb, Column, Where, OrderBy, Page, Size).

%% @doc 更新群组信息
-spec update(map()) -> {ok, non_neg_integer()} | {error, any()}.
update(Data) ->
    Tb = tablename(),
    Id = maps:get(<<"id">>, Data),
    UpdateData = maps:without([<<"id">>], Data),
    elib_pg:update(Tb, UpdateData, <<"id = $1">>, [Id]).

%% @doc 在事务中更新群主
%% 更新指定群组的群主ID
%% @param Conn 数据库连接
%% @param Gid 群组ID
%% @param NewOwnerUid 新群主ID
%% @return ok | {error, Reason}
-spec update_owner_tx(any(), integer(), integer()) -> ok | {error, any()}.
update_owner_tx(Conn, Gid, NewOwnerUid) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql = <<"UPDATE ", Tb/binary, " SET owner_uid = $1, updated_at = $2 WHERE id = $3">>,
    case elib_pg:execute(Conn, Sql, [NewOwnerUid, Now, Gid]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, "群组不存在"};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec normalize_legacy_create_data(map()) -> map().
normalize_legacy_create_data(Data0) ->
    Gid = pick_value(Data0, [id, <<"id">>, gid, <<"gid">>], 0),
    OwnerUid = pick_value(Data0, [owner_uid, <<"owner_uid">>], 0),
    Name = pick_value(Data0, [title, <<"title">>, name, <<"name">>], <<"">>),
    #{
        id => ec_cnv:to_integer(Gid),
        owner_uid => ec_cnv:to_integer(OwnerUid),
        creator_uid => ec_cnv:to_integer(OwnerUid),
        title => ec_cnv:to_binary(Name),
        status => 1,
        created_at => elib_dt:now(),
        updated_at => elib_dt:now()
    }.

-spec pick_value(map(), [atom() | binary()], term()) -> term().
pick_value(_Map, [], Default) ->
    Default;
pick_value(Map, [Key | Rest], Default) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            pick_value(Map, Rest, Default)
    end.
