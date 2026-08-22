-module(fts_group_repo).
%%%
% fts_group_repo 是群组全文搜索存储库模块
% 封装群组全文搜索的 PostgreSQL 查询
%%%

-export([tablename/0]).
-export([count_for_group_search/1]).
-export([group_search_page/3]).
-export([group_search_page/4]).

-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取全文搜索群组表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"fts_group">>).

%% @doc 统计群组搜索结果数量
%% @param Keyword 搜索关键词（空字符串返回0）
%% @return Count 匹配的群组数量
-spec count_for_group_search(binary()) -> non_neg_integer().
count_for_group_search(<<>>) ->
    0;
count_for_group_search(Keyword) ->
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            Sql =
                <<"SELECT count(*) as count FROM ", (tablename())/binary,
                    " fts LEFT JOIN public.\"group\" g ON g.id = fts.group_id "
                    "WHERE g.status = 1 AND g.type = 1 AND fts.token @@ to_tsquery('jiebacfg', $1)">>,
            case elib_pg:one(Sql, [Keyword2]) of
                {ok, #{<<"count">> := Count}} ->
                    Count;
                _ ->
                    0
            end;
        _ ->
            0
    end.

%% @doc 分页搜索群组（全文搜索）
%% 使用 PostgreSQL 的 pg_jieba 分词插件进行中文全文搜索
%% 只返回 status=1（活跃）且 type=1（公开）的群组
%% @param Keyword 搜索关键词
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @return {ok, Rows} | {error, Reason}
-spec group_search_page(binary(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
group_search_page(Keyword, Limit, Offset) ->
    group_search_page(Keyword, Limit, Offset, undefined).

%% @doc 分页搜索群组（全文搜索，支持分类筛选）
%% @param Keyword 搜索关键词
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param CategoryId 分类ID（undefined 表示不筛选）
%% @return {ok, Rows} | {error, Reason}
-spec group_search_page(binary(), integer(), integer(), integer() | undefined) ->
    {ok, list(map())} | {error, any()}.
group_search_page(<<>>, Limit, Offset, CategoryId) ->
    % 空关键词：按成员数排序返回公开群
    Sql = build_discover_sql(CategoryId, <<"member_count DESC">>),
    elib_pg:query(Sql, [Limit, Offset]);
group_search_page(Keyword, Limit, Offset, CategoryId) ->
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            WhereClause =
                case CategoryId of
                    undefined ->
                        <<"g.status = 1 AND g.type = 1 AND fts.token @@ to_tsquery('jiebacfg', $1)">>;
                    _ ->
                        <<"g.status = 1 AND g.type = 1 AND g.category_id = $2 AND fts.token @@ to_tsquery('jiebacfg', $1)">>
                end,
            OrderClause = <<"ts_rank_cd(fts.token, to_tsquery('jiebacfg', $1)) DESC">>,
            Sql = build_search_sql(WhereClause, OrderClause),
            Params =
                case CategoryId of
                    undefined -> [Keyword2, Limit, Offset];
                    _ -> [Keyword2, CategoryId, Limit, Offset]
                end,
            elib_pg:query(Sql, Params);
        _ ->
            {ok, []}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 构建搜索 SQL（带全文搜索排序）
%% 参数位次：keyword 恒为 $1；带 category_id 筛选时 category 为 $2 →
%% LIMIT $3 OFFSET $4，否则 LIMIT $2 OFFSET $3（if_then_else 的 Then 分支
%% 对应「含 category_id」情形）。
-spec build_search_sql(binary(), binary()) -> binary().
build_search_sql(WhereClause, OrderClause) ->
    <<
        "SELECT g.id, g.title, g.avatar, g.introduction, g.member_count, "
        "g.type, g.join_limit, g.category_id, g.is_featured, g.created_at, "
        "ts_rank_cd(fts.token, to_tsquery('jiebacfg', $1)) as rank "
        "FROM ",
        (tablename())/binary,
        " fts "
        "LEFT JOIN public.\"group\" g ON g.id = fts.group_id "
        "WHERE ",
        WhereClause/binary,
        " "
        "ORDER BY ",
        OrderClause/binary,
        " "
        "LIMIT $",
        (build_param_index(if_then_else(WhereClause, 3, 2)))/binary,
        " "
        "OFFSET $",
        (build_param_index(if_then_else(WhereClause, 4, 3)))/binary
    >>.

%% @doc 构建发现页 SQL（无关键词，按指定排序）
-spec build_discover_sql(integer() | undefined, binary()) -> binary().
build_discover_sql(CategoryId, OrderClause) ->
    WhereClause =
        case CategoryId of
            undefined ->
                <<"g.status = 1 AND g.type = 1">>;
            _ ->
                <<"g.status = 1 AND g.type = 1 AND g.category_id = $3">>
        end,
    <<
        "SELECT g.id, g.title, g.avatar, g.introduction, g.member_count, "
        "g.type, g.join_limit, g.category_id, g.is_featured, g.created_at "
        "FROM public.\"group\" g "
        "WHERE ",
        WhereClause/binary,
        " "
        "ORDER BY g.",
        OrderClause/binary,
        " "
        "LIMIT $1 OFFSET $2"
    >>.

%% @doc 判断 WHERE 子句中是否包含 category_id 参数
-spec if_then_else(binary(), integer(), integer()) -> integer().
if_then_else(WhereClause, Then, Else) ->
    case binary:match(WhereClause, <<"category_id">>) of
        nomatch -> Else;
        _ -> Then
    end.

%% @doc 构建参数索引
-spec build_param_index(integer()) -> binary().
build_param_index(N) ->
    integer_to_binary(N).
