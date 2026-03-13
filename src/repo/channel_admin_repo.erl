-module(channel_admin_repo).
%%%
% channel_admin_repo 是 channel_admin repository 缩写
% 频道管理员数据仓库层
%%%

-export([tablename/0]).
-export([add/1]).
-export([add/2]).
-export([find/2]).
-export([list_by_channel/1]).
-export([list_by_uid/1]).
-export([delete/2]).
-export([is_admin/2]).
-export([get_role/2]).
-export([update_role/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取频道管理员表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"channel_admin">>).

%% @doc 添加管理员
-spec add(map()) -> {ok, term()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    elib_pg:insert(Tb, Data).

%% @doc 添加管理员（使用连接）
-spec add(any(), map()) -> {ok, term(), term()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:insert(Tb, Data, <<"RETURNING id">>),
    elib_pg:execute(Conn, Sql, Params).

%% @doc 查找管理员记录
-spec find(integer(), integer()) -> map() | #{}.
find(ChannelId, Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 AND user_id = $2 LIMIT 1">>,
    case elib_pg:one(Sql, [ChannelId, Uid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 查询频道的管理员列表
-spec list_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 ORDER BY role DESC">>,
    elib_pg:query(Sql, [ChannelId]).

%% @doc 查询用户管理的频道
-spec list_by_uid(integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE user_id = $1">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 删除管理员
-spec delete(integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(ChannelId, Uid) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE channel_id = $1 AND user_id = $2">>,
    elib_pg:execute(Sql, [ChannelId, Uid]).

%% @doc 检查用户是否为频道管理员
-spec is_admin(integer(), integer()) -> boolean().
is_admin(ChannelId, Uid) ->
    Admin = find(ChannelId, Uid),
    map_size(Admin) > 0.

%% @doc 获取用户在频道的角色
%% @return 0: 非管理员 1: 编辑 2: 管理员 3: 创建者
-spec get_role(integer(), integer()) -> integer().
get_role(ChannelId, Uid) ->
    Admin = find(ChannelId, Uid),
    maps:get(<<"role">>, Admin, 0).

%% @doc 更新管理员角色
-spec update_role(integer(), integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
update_role(ChannelId, Uid, Role) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{role => Role}, <<"channel_id = $1 AND user_id = $2">>, [ChannelId, Uid]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
