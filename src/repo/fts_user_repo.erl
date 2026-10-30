-module(fts_user_repo).
-moduledoc "用户全文搜索存储库模块 / repository。".
%%%
% fts 相关操作都放到该模块，存储库模块
% fts related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([allow_search/1]).
-export([search_authorized_page/6]).
-export([
    count_for_user_search_page/1,
    user_search_page/3
]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% A single transaction binds list/count to the authenticated user and group
%% generation. Missing ordering evidence is an error, never a partial page.
search_authorized_page(Uid, Keyword, Limit, Offset, Type, Options) when
    is_integer(Uid),
    Uid > 0,
    is_binary(Keyword),
    is_integer(Limit),
    Limit > 0,
    is_integer(Offset),
    Offset >= 0,
    is_map(Options),
    (Type =:= <<"c2c">> orelse Type =:= <<"c2g">>)
->
    case valid_conversation_option(Options) of
        true -> search_authorized_page_valid(Uid, Keyword, Limit, Offset, Type, Options);
        false -> {error, search_unavailable}
    end;
search_authorized_page(_, _, _, _, _, _) ->
    {error, search_unavailable}.

valid_conversation_option(Options) ->
    case maps:find(<<"conversation_id">>, Options) of
        error -> true;
        {ok, Id} -> is_integer(Id) andalso Id > 0 andalso Id =< 9223372036854775807
    end.

search_authorized_page_valid(Uid, Keyword, Limit, Offset, Type, Options) ->
    try
        case
            elib_pg:with_tx(
                fun(Conn) ->
                    authorized_page_tx(Conn, Uid, Keyword, Limit, Offset, Type, Options)
                end,
                [{reraise, false}, {begin_opts, <<"ISOLATION LEVEL READ COMMITTED">>}]
            )
        of
            {ok, _, _} = Ok -> Ok;
            _ -> {error, search_unavailable}
        end
    catch
        _:_ -> {error, search_unavailable}
    end.

authorized_page_tx(Conn, Uid, Keyword, Limit, Offset, Type, Options) ->
    KeywordSql = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword">>,
    case search_query(Conn, KeywordSql, [Keyword]) of
        [#{<<"keyword">> := Parsed}] ->
            SortOptions =
                case map_size(Options) of
                    0 -> #{<<"sort_by">> => <<"time">>};
                    _ -> Options
                end,
            {Order, Base, Params} = build_advanced_query(SortOptions, Parsed, Type),
            ParamList = lists:flatten(Params),
            UidParam = <<"$", (integer_to_binary(length(ParamList) + 1))/binary>>,
            Args = ParamList ++ [Uid],
            ScopedBase = pair_scope(Base, Type, Options, UidParam, length(ParamList)),
            authorized_page_query(
                Conn, Type, ScopedBase, Order, UidParam, Args, Limit, Offset, map_size(Options) > 0
            );
        _ ->
            throw({rollback, search_unavailable})
    end.

pair_scope(Base, <<"c2c">>, #{<<"conversation_id">> := _}, UidParam, PeerIndex) ->
    Peer = <<"$", (integer_to_binary(PeerIndex))/binary>>,
    <<Base/binary, " AND ((m.from_id=", UidParam/binary, " AND m.to_id=", Peer/binary,
        ") OR (m.from_id=", Peer/binary, " AND m.to_id=", UidParam/binary, "))">>;
pair_scope(Base, _, _, _, _) ->
    Base.

authorized_page_query(Conn, <<"c2c">>, Base, Order, UidParam, Args, Limit, Offset, Advanced) ->
    Guard = <<"(m.from_id=", UidParam/binary, " OR m.to_id=", UidParam/binary, ")">>,
    Where = <<Base/binary, " AND ", Guard/binary>>,
    page_rows(Conn, <<"c2c">>, Where, Order, Args, Limit, Offset, Advanced, <<"false">>);
authorized_page_query(Conn, <<"c2g">>, Base, Order, UidParam, Args, Limit, Offset, Advanced) ->
    Groups = authorized_groups(Conn, Base, UidParam, Args),
    Guard = group_guard(UidParam),
    Identity = snapshot_identity(),
    Consistency = snapshot_consistency(),
    MissingSql =
        <<"EXISTS (SELECT 1 FROM msg_c2g m WHERE ", Base/binary, " AND ", Guard/binary,
            " AND NOT EXISTS (SELECT 1 FROM public.msg_c2g_recipient_snapshot snap "
            "JOIN public.msg_store_seq mss ON mss.conv_key='c2g:' || m.to_id::text "
            "AND mss.seq >= snap.conv_seq WHERE ", Identity/binary, " AND ", Consistency/binary,
            "))">>,
    Seq = <<
        "EXISTS (SELECT 1 FROM public.msg_c2g_recipient_snapshot snap "
        "JOIN public.group_member_generation seqgen ON seqgen.group_id=m.to_id "
        "AND seqgen.user_id=",
        UidParam/binary,
        " AND seqgen.end_seq IS NULL "
        "JOIN public.msg_store_seq mss ON mss.conv_key='c2g:' || m.to_id::text "
        "AND mss.seq >= snap.conv_seq WHERE ",
        Identity/binary,
        " AND ",
        Consistency/binary,
        " AND snap.conv_seq >= seqgen.start_seq)"
    >>,
    Where = <<Base/binary, " AND ", Guard/binary, " AND ", Seq/binary>>,
    Result = page_rows(Conn, <<"c2g">>, Where, Order, Args, Limit, Offset, Advanced, MissingSql),
    case authorized_groups(Conn, Base, UidParam, Args) of
        Groups -> Result;
        _ -> throw({rollback, search_unavailable})
    end.

snapshot_identity() ->
    <<"snap.msg_id=m.msg_id AND snap.to_gid=m.to_id AND snap.from_id=m.from_id">>.

snapshot_consistency() ->
    <<
        "NOT EXISTS (SELECT 1 FROM public.msg_store_staging st WHERE st.type='c2g' "
        "AND st.msg_id=m.msg_id AND (st.from_id IS DISTINCT FROM m.from_id "
        "OR st.to_id IS DISTINCT FROM m.to_id OR (st.conv_seq IS NOT NULL "
        "AND st.conv_seq IS DISTINCT FROM snap.conv_seq))) AND "
        "NOT EXISTS (SELECT 1 FROM public.msg_store stored WHERE stored.chat_type='c2g' "
        "AND stored.msg_id=m.msg_id AND (stored.from_id IS DISTINCT FROM m.from_id "
        "OR stored.group_id IS DISTINCT FROM m.to_id OR stored.conv_seq IS DISTINCT FROM snap.conv_seq))"
    >>.

group_guard(UidParam) ->
    Entitlement = micro_community_access_repo:predicate_sql(<<"grp.id">>, UidParam),
    <<
        "EXISTS (SELECT 1 FROM public.group_member_generation gmg "
        "JOIN public.group_member gm ON gm.group_id=gmg.group_id AND gm.user_id=gmg.user_id "
        "AND gm.status=1 JOIN public.\"group\" grp ON grp.id=gmg.group_id AND grp.status=1 "
        "WHERE gmg.group_id=m.to_id AND gmg.user_id=",
        UidParam/binary,
        " AND gmg.end_seq IS NULL AND ",
        Entitlement/binary,
        ")"
    >>.

authorized_groups(Conn, Base, UidParam, Args) ->
    Entitlement = micro_community_access_repo:predicate_sql(<<"grp.id">>, UidParam),
    Sql = <<
        "SELECT gmg.group_id,gmg.generation_no,gmg.start_seq "
        "FROM public.group_member_generation gmg "
        "JOIN public.group_member gm ON gm.group_id=gmg.group_id AND gm.user_id=gmg.user_id "
        "AND gm.status=1 JOIN public.\"group\" grp ON grp.id=gmg.group_id AND grp.status=1 "
        "WHERE gmg.user_id=",
        UidParam/binary,
        " AND gmg.end_seq IS NULL AND ",
        Entitlement/binary,
        " AND EXISTS (SELECT 1 FROM msg_c2g m WHERE m.to_id=gmg.group_id AND ",
        Base/binary,
        ") ORDER BY gmg.group_id FOR SHARE OF gmg,gm,grp"
    >>,
    search_query(Conn, Sql, Args).

page_rows(Conn, Type, Where, Order, Args, Limit, Offset, Advanced, MissingSql) ->
    {Table, Joins} =
        case Type of
            <<"c2c">> ->
                {<<"msg_c2c">>, <<
                    "LEFT JOIN public.user f ON m.from_id=f.id "
                    "LEFT JOIN public.user t ON m.to_id=t.id"
                >>};
            <<"c2g">> ->
                {<<"msg_c2g">>, <<
                    "LEFT JOIN public.user f ON m.from_id=f.id "
                    "LEFT JOIN public.\"group\" g ON m.to_id=g.id"
                >>}
        end,
    Columns =
        case Type of
            <<"c2c">> ->
                <<"m.*, f.nickname AS from_nickname, t.nickname AS to_nickname">>;
            <<"c2g">> ->
                <<"m.*, f.nickname AS from_nickname, g.title AS group_name, m.to_id AS group_id">>
        end,
    Highlight =
        case {Advanced, Type} of
            {true, <<"c2c">>} ->
                <<", ts_headline('jiebacfg', m.payload, websearch_to_tsquery('jiebacfg', $1)) AS highlight">>;
            {true, <<"c2g">>} ->
                <<", ts_headline('jiebacfg', coalesce(m.payload #>> '{payload,text}', m.payload->>'text'), websearch_to_tsquery('jiebacfg', $1)) AS highlight">>;
            _ ->
                <<>>
        end,
    L = integer_to_binary(length(Args) + 1),
    O = integer_to_binary(length(Args) + 2),
    FinalOrder = binary:replace(Order, <<"m.">>, <<"p.">>, [global]),
    Sql =
        <<"SELECT meta.search_total,meta.search_missing,p.* FROM (SELECT count(*) AS search_total, ",
            MissingSql/binary, " AS search_missing FROM ", Table/binary, " m WHERE ", Where/binary,
            ") meta LEFT JOIN LATERAL (SELECT ", Columns/binary, Highlight/binary, " FROM ",
            Table/binary, " m ", Joins/binary, " WHERE ", Where/binary, " ", Order/binary,
            " LIMIT $", L/binary, " OFFSET $", O/binary, ") p ON true ", FinalOrder/binary>>,
    case search_query(Conn, Sql, Args ++ [Limit, Offset]) of
        [#{<<"search_missing">> := false, <<"search_total">> := Count} | _] = Rows ->
            Items = [
                maps:without([<<"search_total">>, <<"search_missing">>], R)
             || R <- Rows,
                maps:get(<<"msg_id">>, R, null) =/= null
            ],
            {ok, Count, Items};
        _ ->
            throw({rollback, search_unavailable})
    end.

search_query(Conn, Sql, Args) ->
    case elib_pg:query(Conn, Sql, Args) of
        {ok, Rows} -> Rows;
        {error, _} -> throw({rollback, search_unavailable})
    end.

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
                    ",u.created_at,ts_rank_cd(fts.token, to_tsquery('jiebacfg',"
                    " $1)) as rank from public.fts_user fts left join public.user u on u.id = fts.user_id where "
                    "fts.allow_search = 1 AND fts.token @@ to_tsquery('jiebacfg',"
                    " $2) order by rank desc LIMIT $3 OFFSET $4">>,
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
    WhereClause = iolist_to_binary([
        lists:join(<<" AND ">>, WhereParts),
        <<" AND m.e2ee IS NULL AND (m.expire_at IS NULL OR m.expire_at > NOW())">>
    ]),

    % 构建排序子句
    SelectClause = build_select_clause(Options, MsgType),

    {SelectClause, WhereClause, Params}.

%% @doc 消息正文的全文本表达式。
%% msg_c2c.payload 是 text；msg_c2g.payload 是 jsonb（无 jsonb @@ tsquery
%% 操作符），统一取 ->>'text' 再 to_tsvector。
-spec payload_text_expr(binary()) -> binary().
payload_text_expr(<<"c2g">>) ->
    <<"to_tsvector('jiebacfg', coalesce(m.payload #>> '{payload,text}', m.payload->>'text'))">>;
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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================
