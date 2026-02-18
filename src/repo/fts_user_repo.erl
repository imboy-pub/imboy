-module(fts_user_repo).
%%%
% fts 相关操作都放到该模块，存储库模块
% fts related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([allow_search/1]).
-export([count_for_user_search_page/1,
         user_search_page/3]).
-export([search_c2c_msg/4,
         search_c2g_msg/4,
         search_c2c_msg_count/1,
         search_c2g_msg_count/1,
         search_c2c_msg_with_options/4,
         search_c2g_msg_with_options/4,
         search_c2c_msg_count_with_options/2,
         search_c2g_msg_count_with_options/2]).

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


%% @doc 搜索私聊消息（全文搜索）
%% 搜索私聊消息中包含关键词的消息
%% @param Keyword 搜索关键词
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param Uid 当前用户ID（用于权限检查，只返回相关消息）
%% @return {ok, Rows} 查询成功返回消息列表 | {error, Reason} 查询失败
%% @example fts_user_repo:search_c2c_msg(<<"你好"/utf8>>, 10, 0, 100).
-spec search_c2c_msg(binary(), integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
search_c2c_msg(Keyword, Limit, Offset, Uid) ->
    % 使用安全的参数化查询
    % 先准备关键词
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            % 搜索私聊消息，只返回当前用户参与的消息
            Sql = <<"select m.*, f.nickname as from_nickname, t.nickname as to_nickname from msg_c2c m left join public.user f on m.from_id = f.id left join public.user t on m.to_id = t.id where m.payload @@ to_tsquery('jiebacfg', $1) and (m.from_id = $2 or m.to_id = $2) order by m.created_at desc LIMIT $3 OFFSET $4">>,
            elib_pg:query(Sql, [Keyword2, Uid, Limit, Offset]);
        _ ->
            {ok, []}
    end.


%% @doc 搜索群聊消息（全文搜索）
%% 搜索群聊消息中包含关键词的消息
%% @param Keyword 搜索关键词
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param Uid 当前用户ID（用于权限检查，只返回有权限查看的群消息）
%% @return {ok, Rows} 查询成功返回消息列表 | {error, Reason} 查询失败
%% @example fts_user_repo:search_c2g_msg(<<"开会"/utf8>>, 10, 0, 100).
-spec search_c2g_msg(binary(), integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
search_c2g_msg(Keyword, Limit, Offset, Uid) ->
    % 使用安全的参数化查询
    % 先准备关键词
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            % 搜索群聊消息，只返回当前用户所在的群的消息
            Sql = <<"select m.*, f.nickname as from_nickname, g.group_name from msg_c2g m left join public.user f on m.from_id = f.id left join public.group g on m.group_id = g.id where m.payload @@ to_tsquery('jiebacfg', $1) and exists (select 1 from public.group_member gm where gm.group_id = m.group_id and gm.user_id = $2) order by m.created_at desc LIMIT $3 OFFSET $4">>,
            elib_pg:query(Sql, [Keyword2, Uid, Limit, Offset]);
        _ ->
            {ok, []}
    end.


%% @doc 统计私聊搜索结果数量
%% @param Keyword 搜索关键词
%% @return Count 匹配的私聊消息数量
%% @example fts_user_repo:search_c2c_msg_count(<<"你好"/utf8>>).
-spec search_c2c_msg_count(binary()) -> non_neg_integer().
search_c2c_msg_count(Keyword) ->
    % 使用安全的参数化查询
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            % 统计私聊消息数量
            Sql = <<"SELECT count(*) as count FROM msg_c2c WHERE payload @@ to_tsquery('jiebacfg', $1)">>,
            case elib_pg:one(Sql, [Keyword2]) of
                {ok, #{<<"count">> := Count}} ->
                    Count;
                _ ->
                    0
            end;
        _ ->
            0
    end.


%% @doc 统计群聊搜索结果数量
%% @param Keyword 搜索关键词
%% @return Count 匹配的群聊消息数量
%% @example fts_user_repo:search_c2g_msg_count(<<"开会"/utf8>>).
-spec search_c2g_msg_count(binary()) -> non_neg_integer().
search_c2g_msg_count(Keyword) ->
    % 使用安全的参数化查询
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            % 统计群聊消息数量
            Sql = <<"SELECT count(*) as count FROM msg_c2g WHERE payload @@ to_tsquery('jiebacfg', $1)">>,
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
%% 增强搜索功能 - 支持多种筛选条件
%% ===================================================================

%% @doc 高级搜索私聊消息（全文搜索）
%% 支持按日期范围、消息类型、发送者、会话ID、排序方式筛选
%% @param Keyword 搜索关键词
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param Options 筛选选项 map
%%   - start_date: 开始日期 (binary: <<"YYYY-MM-DD">>)
%%   - end_date: 结束日期 (binary: <<"YYYY-MM-DD">>)
%%   - msg_type: 消息类型 (binary: <<"text">>, <<"image">>, <<"video">>, etc.)
%%   - from_uid: 发送者ID (integer)
%%   - conversation_id: 会话ID (integer)
%%   - sort_by: 排序方式 (binary: <<"relevance">> | <<"time">>)
%% @return {ok, Rows} 查询成功返回消息列表 | {error, Reason} 查询失败
-spec search_c2c_msg_with_options(binary(), integer(), integer(), map()) -> {ok, list(map())} | {error, any()}.
search_c2c_msg_with_options(Keyword, Limit, Offset, Options) ->
    % 先准备关键词
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            % 构建查询
            {SelectSql, WhereSql, Params} = build_advanced_query(Options, Keyword2, <<"c2c">>),
            FinalSql = <<"select m.*, f.nickname as from_nickname, t.nickname as to_nickname, ",
                          "ts_headline('jiebacfg', m.payload, websearch_to_tsquery('jiebacfg', $1)) as highlight ",
                          "from msg_c2c m ",
                          "left join public.user f on m.from_id = f.id ",
                          "left join public.user t on m.to_id = t.id ",
                          "where ", WhereSql/binary, " ",
                          SelectSql/binary,
                          " LIMIT $", (build_param_index(length(Params) + 2))/binary,
                          " OFFSET $", (build_param_index(length(Params) + 3))/binary>>,
            elib_pg:query(FinalSql, lists:flatten([Keyword2 | Params]) ++ [Limit, Offset]);
        _ ->
            {ok, []}
    end.

%% @doc 高级搜索群聊消息（全文搜索）
%% 支持按日期范围、消息类型、发送者、会话ID、排序方式筛选
%% @param Keyword 搜索关键词
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @param Options 筛选选项 map
%% @return {ok, Rows} 查询成功返回消息列表 | {error, Reason} 查询失败
-spec search_c2g_msg_with_options(binary(), integer(), integer(), map()) -> {ok, list(map())} | {error, any()}.
search_c2g_msg_with_options(Keyword, Limit, Offset, Options) ->
    % 先准备关键词
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            % 构建查询
            {SelectSql, WhereSql, Params} = build_advanced_query(Options, Keyword2, <<"c2g">>),
            FinalSql = <<"select m.*, f.nickname as from_nickname, g.group_name, ",
                          "ts_headline('jiebacfg', m.payload, websearch_to_tsquery('jiebacfg', $1)) as highlight ",
                          "from msg_c2g m ",
                          "left join public.user f on m.from_id = f.id ",
                          "left join public.group g on m.group_id = g.id ",
                          "where ", WhereSql/binary, " ",
                          SelectSql/binary,
                          " LIMIT $", (build_param_index(length(Params) + 2))/binary,
                          " OFFSET $", (build_param_index(length(Params) + 3))/binary>>,
            elib_pg:query(FinalSql, lists:flatten([Keyword2 | Params]) ++ [Limit, Offset]);
        _ ->
            {ok, []}
    end.

%% @doc 统计私聊高级搜索结果数量
%% @param Keyword 搜索关键词
%% @param Options 筛选选项 map
%% @return Count 匹配的私聊消息数量
-spec search_c2c_msg_count_with_options(binary(), map()) -> non_neg_integer().
search_c2c_msg_count_with_options(Keyword, Options) ->
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            {_SelectSql, WhereSql, Params} = build_advanced_query(Options, Keyword2, <<"c2c">>),
            Sql = <<"SELECT count(*) as count FROM msg_c2c WHERE ", WhereSql/binary>>,
            case elib_pg:one(Sql, [Keyword2 | Params]) of
                {ok, #{<<"count">> := Count}} ->
                    Count;
                _ ->
                    0
            end;
        _ ->
            0
    end.

%% @doc 统计群聊高级搜索结果数量
%% @param Keyword 搜索关键词
%% @param Options 筛选选项 map
%% @return Count 匹配的群聊消息数量
-spec search_c2g_msg_count_with_options(binary(), map()) -> non_neg_integer().
search_c2g_msg_count_with_options(Keyword, Options) ->
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            {_SelectSql, WhereSql, Params} = build_advanced_query(Options, Keyword2, <<"c2g">>),
            Sql = <<"SELECT count(*) as count FROM msg_c2g WHERE ", WhereSql/binary>>,
            case elib_pg:one(Sql, [Keyword2 | Params]) of
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

%% @doc 构建高级搜索查询
%% 返回 {SelectClause, WhereClause, Params}
-spec build_advanced_query(map(), binary(), binary()) -> {binary(), binary(), list()}.
build_advanced_query(Options, _Keyword2, MsgType) ->
    % 构建WHERE子句和参数列表
    Conditions = build_conditions(Options, MsgType),
    {WhereParts, Params} = lists:unzip(Conditions),
    WhereClause = iolist_to_binary(lists:join(<<" AND ">>, WhereParts)),

    % 构建排序子句
    SelectClause = build_select_clause(Options),

    {SelectClause, WhereClause, Params}.

%% @doc 构建排序子句
-spec build_select_clause(map()) -> binary().
build_select_clause(Options) ->
    case maps:get(<<"sort_by">>, Options, <<"relevance">>) of
        <<"relevance">> ->
            <<"order by ts_rank(to_tsvector('jiebacfg', m.payload), to_tsquery('jiebacfg', $1)) DESC, m.created_at DESC">>;
        <<"time">> ->
            <<"order by m.created_at DESC">>;
        _ ->
            <<"order by m.created_at DESC">>
    end.

%% @doc 构建条件列表
%% 返回 [{WherePart, Param}] 列表
-spec build_conditions(map(), binary()) -> list({binary(), any()}).
build_conditions(Options, MsgType) ->
    % 基础条件：全文搜索
    BaseConditions = [{<<"m.payload @@ to_tsquery('jiebacfg', $1)">>, undefined}],

    % 添加日期范围条件
    StartDateCond = case maps:get(<<"start_date">>, Options, undefined) of
        undefined -> [];
        StartDate -> [{<<"m.created_at >= $2">>, StartDate}]
    end,

    EndDateCond = case maps:get(<<"end_date">>, Options, undefined) of
        undefined -> [];
        EndDate ->
            ParamIndex = case maps:is_key(<<"start_date">>, Options) of
                true -> 3;
                false -> 2
            end,
            ParamBin = integer_to_binary(ParamIndex),
            [{<<"m.created_at <= $", ParamBin/binary>>, EndDate}]
    end,

    % 计算当前参数索引
    ParamIndex2 = 2 + length(StartDateCond) + length(EndDateCond),
    ParamBin2 = integer_to_binary(ParamIndex2),

    % 添加消息类型条件
    MsgTypeCond = case maps:get(<<"msg_type">>, Options, undefined) of
        undefined -> [];
        MsgTypeVal -> [{<<"m.msg_type = $", ParamBin2/binary>>, MsgTypeVal}]
    end,

    % 计算下一个参数索引
    ParamIndex3 = ParamIndex2 + length(MsgTypeCond),
    ParamBin3 = integer_to_binary(ParamIndex3),

    % 添加发送者条件
    FromUidCond = case maps:get(<<"from_uid">>, Options, undefined) of
        undefined -> [];
        FromUid -> [{<<"m.from_id = $", ParamBin3/binary>>, FromUid}]
    end,

    % 计算下一个参数索引
    ParamIndex4 = ParamIndex3 + length(FromUidCond),
    ParamBin4 = integer_to_binary(ParamIndex4),

    % 添加会话ID条件
    ConversationCond = case {MsgType, maps:get(<<"conversation_id">>, Options, undefined)} of
        {<<"c2c">>, ConversationId} when is_integer(ConversationId) ->
            [{<<"(m.from_id = $", ParamBin4/binary, " OR m.to_id = $", ParamBin4/binary, ")">>, ConversationId}];
        {<<"c2g">>, ConversationId} when is_integer(ConversationId) ->
            [{<<"m.group_id = $", ParamBin4/binary>>, ConversationId}];
        _ ->
            []
    end,

    % 合并所有条件
    BaseConditions ++ StartDateCond ++ EndDateCond ++ MsgTypeCond ++ FromUidCond ++ ConversationCond.

%% @doc 构建参数索引
-spec build_param_index(integer()) -> binary().
build_param_index(N) ->
    integer_to_binary(N).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================
