-module(fts_user_repo).
%%%
% fts 相关操作都放到该模块，存储库模块
% fts related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([allow_search/1]).
-export([
    count_for_user_search_page/1,
    user_search_page/3
]).
-export([
    search_c2c_msg/4,
    search_c2g_msg/4,
    search_c2c_msg_count/1,
    search_c2g_msg_count/1,
    search_c2c_msg_with_options/4,
    search_c2g_msg_with_options/4,
    search_c2c_msg_count_with_options/2,
    search_c2g_msg_count_with_options/2
]).

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
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            Sql =
                <<"select ", ?DEF_USER_COLUMN/binary,
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
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            % count(*) 只返回一行，无需 LIMIT
            Sql =
                <<"SELECT count(*) as count FROM ", (tablename())/binary,
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
-spec search_c2c_msg(binary(), integer(), integer(), integer()) ->
    {ok, list(map())} | {error, any()}.
search_c2c_msg(Keyword, Limit, Offset, Uid) ->
    % 使用安全的参数化查询
    % 先准备关键词
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            % 搜索私聊消息，只返回当前用户参与的消息
            % m.e2ee IS NULL: E2EE 密文不可搜索（与部分索引谓词一致）
            % 文档侧必须显式 to_tsvector('jiebacfg', payload)：text 直连
            % `payload @@ tsquery` 的文档侧不按 jieba 分词，中文关键词
            % 恒不命中（此前仅完整 latin token 可搜）；显式形式也与
            % idx_msg_c2c_payload_fts 的索引表达式一致，可走 GIN 索引。
            Sql =
                <<"select m.*, f.nickname as from_nickname, t.nickname as to_nickname from msg_c2c m left join public.user f on m.from_id = f.id left join public.user t on m.to_id = t.id where to_tsvector('jiebacfg', m.payload) @@ to_tsquery('jiebacfg', $1) and m.e2ee is null and (m.from_id = $2 or m.to_id = $2) order by m.created_at desc LIMIT $3 OFFSET $4">>,
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
-spec search_c2g_msg(binary(), integer(), integer(), integer()) ->
    {ok, list(map())} | {error, any()}.
search_c2g_msg(Keyword, Limit, Offset, Uid) ->
    % 使用安全的参数化查询
    % 先准备关键词
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            % 搜索群聊消息，只返回当前用户所在的群的消息
            % m.e2ee IS NULL: E2EE 密文不可搜索（与部分索引谓词一致）
            % msg_c2g.payload 是 jsonb：没有 jsonb @@ tsquery 操作符，必须先
            % 取 payload->>'text' 再 to_tsvector；群 id 列是 to_id（表里没有
            % group_id 列），群名列是 title（没有 group_name 列）——修正前
            % 这三处列名/类型错使 SQL 恒报错，被上层吞成恒空结果。
            Sql =
                <<"select m.*, f.nickname as from_nickname, g.title as group_name, m.to_id as group_id from msg_c2g m left join public.user f on m.from_id = f.id left join public.group g on m.to_id = g.id where to_tsvector('jiebacfg', m.payload->>'text') @@ to_tsquery('jiebacfg', $1) and m.e2ee is null and exists (select 1 from public.group_member gm where gm.group_id = m.to_id and gm.user_id = $2) order by m.created_at desc LIMIT $3 OFFSET $4">>,
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
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            % 统计私聊消息数量（文档侧显式 jieba 分词，理由同 search_c2c_msg/4）
            Sql =
                <<"SELECT count(*) as count FROM msg_c2c WHERE to_tsvector('jiebacfg', payload) @@ to_tsquery('jiebacfg', $1) AND e2ee IS NULL">>,
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
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            % 统计群聊消息数量
            % payload 是 jsonb：无 jsonb @@ tsquery 操作符，须先取 text 再 to_tsvector
            Sql =
                <<"SELECT count(*) as count FROM msg_c2g WHERE to_tsvector('jiebacfg', payload->>'text') @@ to_tsquery('jiebacfg', $1) AND e2ee IS NULL">>,
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
-spec search_c2c_msg_with_options(binary(), integer(), integer(), map()) ->
    {ok, list(map())} | {error, any()}.
search_c2c_msg_with_options(Keyword, Limit, Offset, Options) ->
    % 先准备关键词
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            % 构建查询
            {SelectSql, WhereSql, Params} = build_advanced_query(Options, Keyword2, <<"c2c">>),
            FinalSql =
                <<"select m.*, f.nickname as from_nickname, t.nickname as to_nickname, ",
                    "ts_headline('jiebacfg', m.payload, websearch_to_tsquery('jiebacfg', $1)) as highlight ",
                    "from msg_c2c m ", "left join public.user f on m.from_id = f.id ",
                    "left join public.user t on m.to_id = t.id ", "where ", WhereSql/binary, " ",
                    SelectSql/binary, " LIMIT $", (build_param_index(length(Params) + 1))/binary,
                    " OFFSET $", (build_param_index(length(Params) + 2))/binary>>,
            elib_pg:query(FinalSql, lists:flatten(Params) ++ [Limit, Offset]);
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
-spec search_c2g_msg_with_options(binary(), integer(), integer(), map()) ->
    {ok, list(map())} | {error, any()}.
search_c2g_msg_with_options(Keyword, Limit, Offset, Options) ->
    % 先准备关键词
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            % 构建查询
            {SelectSql, WhereSql, Params} = build_advanced_query(Options, Keyword2, <<"c2g">>),
            % msg_c2g.payload 为 jsonb：ts_headline/匹配均须取 ->>'text'；
            % 群 id 列是 to_id（无 group_id 列），群名列是 title。
            FinalSql =
                <<"select m.*, f.nickname as from_nickname, g.title as group_name, m.to_id as group_id, ",
                    "ts_headline('jiebacfg', m.payload->>'text', websearch_to_tsquery('jiebacfg', $1)) as highlight ",
                    "from msg_c2g m ", "left join public.user f on m.from_id = f.id ",
                    "left join public.group g on m.to_id = g.id ", "where ", WhereSql/binary, " ",
                    SelectSql/binary, " LIMIT $", (build_param_index(length(Params) + 1))/binary,
                    " OFFSET $", (build_param_index(length(Params) + 2))/binary>>,
            elib_pg:query(FinalSql, lists:flatten(Params) ++ [Limit, Offset]);
        _ ->
            {ok, []}
    end.

%% @doc 统计私聊高级搜索结果数量
%% @param Keyword 搜索关键词
%% @param Options 筛选选项 map
%% @return Count 匹配的私聊消息数量
-spec search_c2c_msg_count_with_options(binary(), map()) -> non_neg_integer().
search_c2c_msg_count_with_options(Keyword, Options) ->
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            {_SelectSql, WhereSql, Params} = build_advanced_query(Options, Keyword2, <<"c2c">>),
            % 别名 m 必须存在：WhereSql 内条件均以 m. 前缀引用
            Sql = <<"SELECT count(*) as count FROM msg_c2c m WHERE ", WhereSql/binary>>,
            case elib_pg:one(Sql, lists:flatten(Params)) of
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
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            {_SelectSql, WhereSql, Params} = build_advanced_query(Options, Keyword2, <<"c2g">>),
            % 别名 m 必须存在：WhereSql 内条件均以 m. 前缀引用
            Sql = <<"SELECT count(*) as count FROM msg_c2g m WHERE ", WhereSql/binary>>,
            case elib_pg:one(Sql, lists:flatten(Params)) of
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
%% 返回 {SelectClause, WhereClause, Params}。
%% Params 的首元素是关键词（$1），与 SQL 内 $N 逐位对应——
%% 调用方不得再前置 Keyword，否则参数个数与占位不符（epgsql 拒绝，
%% 此前的双重记账使全部 with_options 查询恒失败并被吞成 0 结果）。
-spec build_advanced_query(map(), binary(), binary()) -> {binary(), binary(), list()}.
build_advanced_query(Options, Keyword2, MsgType) ->
    % 构建WHERE子句和参数列表
    Conditions = build_conditions(Options, Keyword2, MsgType),
    {WhereParts, Params} = lists:unzip(Conditions),
    % m.e2ee IS NULL: E2EE 密文不可搜索（无参数条件，直接拼接不影响参数索引）
    WhereClause = iolist_to_binary([lists:join(<<" AND ">>, WhereParts), <<" AND m.e2ee IS NULL">>]),

    % 构建排序子句
    SelectClause = build_select_clause(Options, MsgType),

    {SelectClause, WhereClause, Params}.

%% @doc 消息正文的全文本表达式。
%% msg_c2c.payload 是 text；msg_c2g.payload 是 jsonb（无 jsonb @@ tsquery
%% 操作符），统一取 ->>'text' 再 to_tsvector。
-spec payload_text_expr(binary()) -> binary().
payload_text_expr(<<"c2g">>) ->
    <<"to_tsvector('jiebacfg', m.payload->>'text')">>;
payload_text_expr(_) ->
    <<"to_tsvector('jiebacfg', m.payload)">>.

%% @doc 构建排序子句
-spec build_select_clause(map(), binary()) -> binary().
build_select_clause(Options, MsgType) ->
    PayloadExpr = payload_text_expr(MsgType),
    case maps:get(<<"sort_by">>, Options, <<"relevance">>) of
        <<"relevance">> ->
            <<"order by ts_rank(", PayloadExpr/binary,
                ", to_tsquery('jiebacfg', $1)) DESC, m.created_at DESC">>;
        <<"time">> ->
            <<"order by m.created_at DESC">>;
        _ ->
            <<"order by m.created_at DESC">>
    end.

%% @doc 构建条件列表
%% 返回 [{WherePart, Param}] 列表；Param 顺序即 $N 顺序（首条=$1 关键词）
-spec build_conditions(map(), binary(), binary()) -> list({binary(), any()}).
build_conditions(Options, Keyword2, MsgType) ->
    % 基础条件：全文搜索（C2G 的 jsonb payload 须先取 text，见 payload_text_expr/1）
    PayloadExpr = payload_text_expr(MsgType),
    BaseConditions = [
        {<<PayloadExpr/binary, " @@ to_tsquery('jiebacfg', $1)">>, Keyword2}
    ],

    % 添加日期范围条件
    StartDateCond =
        case maps:get(<<"start_date">>, Options, undefined) of
            undefined -> [];
            StartDate -> [{<<"m.created_at >= $2">>, StartDate}]
        end,

    EndDateCond =
        case maps:get(<<"end_date">>, Options, undefined) of
            undefined ->
                [];
            EndDate ->
                ParamIndex =
                    case maps:is_key(<<"start_date">>, Options) of
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
    MsgTypeCond =
        case maps:get(<<"msg_type">>, Options, undefined) of
            undefined -> [];
            MsgTypeVal -> [{<<"m.msg_type = $", ParamBin2/binary>>, MsgTypeVal}]
        end,

    % 计算下一个参数索引
    ParamIndex3 = ParamIndex2 + length(MsgTypeCond),
    ParamBin3 = integer_to_binary(ParamIndex3),

    % 添加发送者条件
    FromUidCond =
        case maps:get(<<"from_uid">>, Options, undefined) of
            undefined -> [];
            FromUid -> [{<<"m.from_id = $", ParamBin3/binary>>, FromUid}]
        end,

    % 计算下一个参数索引
    ParamIndex4 = ParamIndex3 + length(FromUidCond),
    ParamBin4 = integer_to_binary(ParamIndex4),

    % 添加会话ID条件
    ConversationCond =
        case {MsgType, maps:get(<<"conversation_id">>, Options, undefined)} of
            {<<"c2c">>, ConversationId} when is_integer(ConversationId) ->
                [
                    {
                        <<"(m.from_id = $", ParamBin4/binary, " OR m.to_id = $", ParamBin4/binary,
                            ")">>,
                        ConversationId
                    }
                ];
            {<<"c2g">>, ConversationId} when is_integer(ConversationId) ->
                [{<<"m.to_id = $", ParamBin4/binary>>, ConversationId}];
            _ ->
                []
        end,

    % 合并所有条件
    BaseConditions ++ StartDateCond ++ EndDateCond ++ MsgTypeCond ++ FromUidCond ++
        ConversationCond.

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
