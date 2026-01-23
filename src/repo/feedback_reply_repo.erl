-module (feedback_reply_repo).
%%%
% feedback_reply 相关操作都放到该模块，存储库模块
% feedback_reply related operations are put in this module, repository module
%%%

%% @doc 获取反馈回复表名
%% @returns binary() 表名
-export ([tablename/0]).


-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"feedback_reply">>).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

