-module(user_collect_repo).
%%%
% user_collect 相关操作都放到该模块，存储库模块
% collect related operations are put in this module, repository module
% 用户收藏数据仓库层，提供用户收藏信息的基础数据库操作
%%%

-export([tablename/0]).
-export([count_by_uid_kind_id/2]).
-export([delete/2]).
-export([update/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% @doc 获取用户收藏表的表名
%% @return 返回用户收藏表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_collect">>).


%% @doc 统计用户指定类型的收藏数量
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @return Count 收藏数量
%% @example user_collect_repo:count_by_uid_kind_id(2, <<"cqi6od1pa9gjnlt3a5a0">>).
-spec count_by_uid_kind_id(integer(), binary()) -> non_neg_integer().
count_by_uid_kind_id(Uid, KindId) ->
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"SELECT count(*) as count FROM ", (tablename())/binary, " WHERE user_id = $1 AND status = 1 AND kind_id = $2">>,
    case elib_pg:query(Sql, [Uid, KindId]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

%% @doc 删除用户收藏
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
%% @example user_collect_repo:delete(2, <<"cqi6od1pa9gjnlt3a5a0">>).
-spec delete(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete(Uid, KindId) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND kind_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    case elib_pg:execute(Sql, [Uid, KindId]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.


%% @doc 更新用户收藏
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @param Data 要更新的数据map
%% @return {ok, Count} 更新成功返回影响行数 | {error, Reason} 更新失败
%% @example user_collect_repo:update(2, <<"cqi6od1pa9gjnlt3a5a0">>, #{<<"updated_at">> => elib_dt:now()}).
-spec update(integer(), binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Uid, KindId, Data) ->
    Table = tablename(),
    Where = <<"user_id = $1 AND kind_id = $2">>,
    elib_pg:update(Table, Data, Where, [Uid, KindId]).
%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

