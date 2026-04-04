-module(announcement_repo).

%%%
% announcement 数据仓库模块
% announcement repository module
%%%

-export([tablename/0]).

-include("common.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"announcement">>).
