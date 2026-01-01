-module(group_member_repo).
%%%
% group_member_repo 是 group_member repository 缩写
%%%
-export([tablename/0]).
-export ([add/1]).
-export ([add/2]).
-export ([find/3]).
-export ([list_same_group/2]).
% -export ([list_same_group/2]).
-export([list_by_gid/2, list_by_gid/3]).
-export([list_by_uid/2, list_by_uid/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_pg_sql:public_tablename(<<"group_member">>).


add(Data) ->
    Tb = tablename(),
    imboy_pg:insert(Tb, Data).

%% @doc 添加群组成员（使用连接）
%% @param Conn 数据库连接
%% @param Data 包含群组成员信息的map
%% @return {ok, Count, Result} | {error, Reason} (三元组，包含RETURNING结果)
-spec add(any(), map()) -> {ok, non_neg_integer(), any()} | {error, any()}.
add(Conn, Data) ->
    Tb = tablename(),
    {Sql, Params} = imboy_pg_sql:insert(Tb, Data, <<"RETURNING id">>),
    imboy_pg:execute(Conn, Sql, Params).

% group_member_repo:find(6, 1, <<"*">>).
-spec find(integer(), integer(), binary()) -> map().
find(Gid, Uid, Column) ->
    Tb = tablename(),
    % use index uk_Gid_Uid
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE group_id = $1 AND user_id = $2">>,
    % ?DEBUG_LOG([Sql]),
    case imboy_pg:one(Sql, [Gid, Uid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

list_by_gid(Gid, Column) ->
    list_by_gid(Gid, Column, 10000).


list_by_gid(Gid, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE group_id = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_pg:query(Sql, [Gid, Limit]).


list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000).


-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    % use index i_Uid_Status
    {Sql, Params} = imboy_pg_sql:build_select(Tb, Column, #{user_id => Uid, status => 1}, #{limit => Limit}),
    imboy_pg:query(Sql, Params).

% group_member_repo:list_same_group(108, 25).
-spec list_same_group(integer(), integer()) -> list().
list_same_group(0, _) ->
    [];
list_same_group(_, 0) ->
    [];
list_same_group(Uid1, Uid2) ->
    % use index i_Uid_Status
    % T1 = imboy_dt:microsecond(),
    Sql = <<"SELECT group_id
        FROM (
            SELECT group_id
            FROM public.group_member
            WHERE user_id = $1 AND status = 1
        ) AS subquery
        WHERE EXISTS (
            SELECT 1
              FROM public.group_member gm2
             WHERE gm2.group_id = subquery.group_id
               AND gm2.user_id = $2 AND gm2.status = 1
        );">>,
    case imboy_pg:query(Sql, [Uid1, Uid2]) of
        {ok, []} ->
            [];
        {ok, Rows} ->
            [Gid || #{<<"group_id">> := Gid} <- Rows]
    end.
    % T2 = imboy_dt:microsecond(),
    % {T2-T1, Res}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
