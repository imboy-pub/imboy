-module(project_channel_agg_repo).
%%%
% project_channel_agg_repo 是项目频道聚合查询 repository 缩写
% Channel-firstclass W2 ZC-04：Pinned / Related Posts / Activity 三类聚合的
% 只读数据源（Resources = project.links 单列读取，经 project_repo 承载）。
%
% 设计约束（W2 计划验收）：
%   * 有界：Pinned / Activity 走 page/size（size 上限由 DS 层钳制 ≤50）；
%     Related Posts 用窗口函数单条 SQL 实现"每关联频道最近 N 条 + 总量上限"。
%   * 无正文：列白名单只含元数据（id/channel/author/type/time），
%     channel_message.content 与 payload 列不进 SELECT。
%   * 无 N+1：每个聚合请求固定 1-2 条 SQL（count + data 或单条窗口查询），
%     与关联频道数/消息量无关（mock 计数断言见 project_channel_logic_tests）。
%   * Pinned 排除公告形态消息：group_notice/announcement 独立表存储结构上
%     不可能混入；此处再加 msg_type NOT IN 防线，对"公告消息落频道流"的
%     未来演化兜底（W2 计划 TDD 用例 6）。
%%%

-export([pinned_page/3]).
-export([related_posts/3]).
-export([activity_page/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 公告形态消息类型（代码内字面量黑名单，非用户输入，内联 SQL；
%% group_notice/announcement 存独立表结构上不混入，此为对
%% "公告消息落频道流"未来演化的防线）
-define(NOTICE_MSG_TYPES_SQL,
    "(cm.msg_type IS NULL OR cm.msg_type NOT IN"
    " ('notice','announcement','group_notice'))"
).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 项目 Pinned 聚合分页：关联频道的置顶消息（元数据列白名单）
%% 过滤：status=1 正常、未撤回、非公告形态；排序 created_at DESC, id DESC。
%% 固定 2 条 SQL（count + data）。
-spec pinned_page(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
pinned_page(ProjectId, Page, Size) ->
    RelTb = project_channel_rel_repo:tablename(),
    Offset = (Page - 1) * Size,
    Where =
        <<"FROM channel_message cm JOIN ", RelTb/binary, " r ON r.channel_id = cm.channel_id",
            " WHERE r.project_id = $1",
            "   AND cm.status = 1 AND cm.is_pinned = true AND cm.revoked = false"
            "   AND ", (?NOTICE_MSG_TYPES_SQL)>>,
    CountSql = <<"SELECT COUNT(*) AS count ", Where/binary>>,
    case elib_pg:one(CountSql, [ProjectId]) of
        {ok, #{<<"count">> := Total}} ->
            DataSql =
                <<"SELECT cm.id, cm.channel_id, cm.author_id, cm.author_name,",
                    " cm.msg_type, cm.created_at ", Where/binary,
                    " ORDER BY cm.created_at DESC, cm.id DESC", " LIMIT $2 OFFSET $3">>,
            page_query(DataSql, [ProjectId, Size, Offset], Page, Size, Total);
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, {unexpected_count_result, Other}}
    end.

%% @doc 项目 Related Posts 聚合：关联频道最近帖子的有界摘要（不含正文）
%% 单条窗口函数 SQL：每频道最近 PerChannel 条 + 总量 TotalCap 上限，
%% 排序 created_at DESC, id DESC（跨频道混排，跨页稳定序键）。
-spec related_posts(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
related_posts(ProjectId, PerChannel, TotalCap) ->
    RelTb = project_channel_rel_repo:tablename(),
    Sql =
        <<"SELECT t.id, t.channel_id, t.author_id, t.msg_type, t.created_at FROM (",
            " SELECT cm.id, cm.channel_id, cm.author_id, cm.msg_type, cm.created_at,",
            "   row_number() OVER (PARTITION BY cm.channel_id",
            "     ORDER BY cm.created_at DESC, cm.id DESC) AS rn", " FROM channel_message cm JOIN ",
            RelTb/binary, " r ON r.channel_id = cm.channel_id",
            " WHERE r.project_id = $1 AND cm.status = 1 AND cm.revoked = false",
            ") t WHERE t.rn <= $2", " ORDER BY t.created_at DESC, t.id DESC LIMIT $3">>,
    case elib_pg:query(Sql, [ProjectId, PerChannel, TotalCap]) of
        {ok, Items} -> {ok, Items};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 项目 Activity 聚合分页：project_event 元数据流
%% payload 以 jsonb 原样返回（binary），正文键清洗由 logic 层展示前执行。
%% 固定 2 条 SQL（count + data）。
-spec activity_page(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
activity_page(ProjectId, Page, Size) ->
    Tb = elib_pg_sql:public_tablename(<<"project_event">>),
    Offset = (Page - 1) * Size,
    CountSql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE project_id = $1">>,
    case elib_pg:one(CountSql, [ProjectId]) of
        {ok, #{<<"count">> := Total}} ->
            DataSql =
                <<"SELECT id, event_type, actor_id, target_id, payload, created_at FROM ",
                    Tb/binary, " WHERE project_id = $1",
                    " ORDER BY created_at DESC, id DESC LIMIT $2 OFFSET $3">>,
            page_query(DataSql, [ProjectId, Size, Offset], Page, Size, Total);
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, {unexpected_count_result, Other}}
    end.

page_query(DataSql, Params, Page, Size, Total) ->
    case elib_pg:query(DataSql, Params) of
        {ok, Items} ->
            TotalPage =
                case Total > 0 of
                    true -> ((Total - 1) div Size) + 1;
                    false -> 0
                end,
            {ok, #{
                list => Items,
                page => Page,
                size => Size,
                total => Total,
                total_page => TotalPage
            }};
        {error, Reason} ->
            {error, Reason}
    end.
