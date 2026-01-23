-module (group_notice_repo).
%%%
% group_notice 相关操作都放到该模块，存储库模块
% group_notice related operations are put in this module, repository module
% 群公告数据仓库层，提供群公告信息的基础数据库操作
%%%

-export ([tablename/0]).
-export ([demo/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取群公告表的表名
%% @return 返回群公告表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_notice">>).

%% @doc 示例方法（演示用途）
%% @param Uid 用户ID
%% @param Val1 参数1（预留）
%% @param Val2 参数2（预留）
%% @return {ok, Rows} 查询成功返回列表 | {error, Reason} 查询失败
%% @details 此方法仅作为示例，实际使用时应根据具体需求修改
-spec demo(integer(), binary(), binary()) ->
    {ok, list(map())} | {error, any()}.
demo(Uid, _Val1, _Val2) ->
    Sql = <<"SELECT id FROM group_notice WHERE id = $1">>,
    elib_pg:query(Sql, [Uid]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

