-module(live_room_repo).
%%%
% live_room_repo 是 live_room repository 缩写
% 直播间数据仓库层，提供直播间数据的基础数据库操作
%%%

-export([tablename/0]).
-export([find_by_id/1]).
-export([create/1]).
-export([update/2]).
-export([page_active/2]).
-export([page_by_uid/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取直播间表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"live_room">>).

%% @doc 根据ID查找直播间
%% @param Id 直播间ID
%% @return map() | {error, any()}
-spec find_by_id(integer() | binary()) -> map() | {error, any()}.
find_by_id(Id) when is_binary(Id); is_list(Id) ->
    find_by_id(ec_cnv:to_integer(Id));
find_by_id(Id) when is_integer(Id), Id > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:one(Sql, [Id]) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end;
find_by_id(_) ->
    #{}.

%% @doc 创建直播间
%% @param Data 直播间数据 map
%% @return {ok, Id, Row} | {error, Reason}
-spec create(map()) -> {ok, integer(), map()} | {error, term()}.
create(Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)).

%% @doc 更新直播间指定字段
%% @param Id 直播间ID
%% @param Data 要更新的字段 map
%% @return {ok, non_neg_integer()} | {error, any()}
-spec update(integer(), map()) -> {ok, non_neg_integer()} | {error, any()}.
update(Id, Data) ->
    Tb = tablename(),
    elib_pg:update(Tb, Data, <<"id = $1">>, [Id]).

%% @doc 分页查询直播中的房间（status=1）
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, any()}
-spec page_active(integer(), integer()) -> {ok, map()} | {error, any()}.
page_active(Page, Size) ->
    Tb = tablename(),
    Column = <<"id,user_id,title,cover,status,viewer_count,tag_id,scene,created_at,updated_at">>,
    Where = #{status => 1},
    OrderBy = <<"viewer_count DESC, id DESC">>,
    elib_pg:page_with_total(Tb, Column, Where, OrderBy, Page, Size).

%% @doc 按用户分页查询直播间
%% @param Uid 用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, map()} | {error, any()}
-spec page_by_uid(integer(), integer(), integer()) -> {ok, map()} | {error, any()}.
page_by_uid(Uid, Page, Size) ->
    Tb = tablename(),
    Column = <<"id,user_id,title,cover,status,viewer_count,tag_id,scene,created_at,updated_at">>,
    Where = #{user_id => Uid},
    OrderBy = <<"id DESC">>,
    elib_pg:page_with_total(Tb, Column, Where, OrderBy, Page, Size).
