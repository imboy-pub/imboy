-module(fts_user_repo).
%%%
% fts 相关操作都放到该模块，存储库模块
% fts related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([allow_search/1]).
-export([count_for_user_search_page/1,
         user_search_page/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% @doc 获取全文搜索用户表的表名
%% @return 返回全文搜索用户表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"fts_user">>).

%% @doc 检查用户是否允许被搜索
%% @param Uid 用户ID
%% @return true 允许搜索 | false 不允许搜索
%% @example fts_user_repo:allow_search(108).
-spec allow_search(integer()) -> boolean().
allow_search(Uid) ->
    % allow_search 用户允许被搜索 1 是 2 否
    case elib_pg:pluck(tablename(), <<"allow_search">>, #{user_id => Uid}, #{}, 2) of
        {ok, Allow} when Allow == 1 ->
            true;
        _ ->
            false
    end.

%% @doc 分页搜索用户（全文搜索）
%% 使用 PostgreSQL 的 pg_jieba 分词插件进行中文全文搜索
%% @param Keyword 搜索关键词
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @return {ok, Rows} 查询成功返回用户列表（按相关性排序） | {error, Reason} 查询失败
%% @example fts_user_repo:user_search_page(<<"东区"/utf8>>, 10, 0).
-spec user_search_page(binary(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
user_search_page(Keyword, Limit, Offset) ->
    % 使用安全的参数化查询，防止 SQL 注入
    % 先准备关键词
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            Sql = <<"select ", ?DEF_USER_COLUMN/binary,
                    ",u.created_at,ts_rank_cd(fts.token, to_tsquery('jiebacfg', $1)) as rank from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.allow_search = 1 AND fts.token @@ to_tsquery('jiebacfg', $2) order by rank desc LIMIT $3 OFFSET $4">>,
            elib_pg:query(Sql, [Keyword2, Keyword2, Limit, Offset]);
        _ ->
            {ok, []}
    end.



%% @doc 统计用户搜索结果数量
%% @param Keyword 搜索关键词（空字符串返回0）
%% @return Count 匹配的用户数量
%% @example fts_user_repo:count_for_user_search_page(<<"leeyi"/utf8>>).
%% @example fts_user_repo:count_for_user_search_page(<<"东区"/utf8>>).
-spec count_for_user_search_page(binary()) -> non_neg_integer().
count_for_user_search_page(<<>>) ->
    0;
count_for_user_search_page(Keyword) ->
    % 使用安全的参数化查询
    % 先准备关键词
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            % count(*) 只返回一行，无需 LIMIT
            Sql = <<"SELECT count(*) as count FROM ", (tablename())/binary,
                    " WHERE allow_search = 1 AND token @@ to_tsquery('jiebacfg', $1)">>,
            case elib_pg:one(Sql, [Keyword2]) of
                {ok, #{<<"count">> := Count}} ->
                    Count;
                _ ->
                    0
            end;
        _ ->
            0
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

