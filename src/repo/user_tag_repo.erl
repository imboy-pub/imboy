-module(user_tag_repo).
%%%
% tag 相关操作都放到该模块，存储库模块
% tag related operations are put in this module, repository module
% 用户标签数据仓库层，提供用户标签信息的基础数据库操作
%%%
%% @doc 此模块仅提供表名定义，实际标签操作在 user_tag_relation_repo 中实现
%%%

-export([tablename/0]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取用户标签表的表名
%% @return 返回用户标签表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_tag">>).


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
