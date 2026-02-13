-module (group_notice_repo).
%%%
% group_notice 相关操作都放到该模块，存储库模块
% group_notice related operations are put in this module, repository module
% 群公告数据仓库层，提供群公告信息的基础数据库操作
%%%

-export ([tablename/0]).

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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

