-module(fts_logic).

%%%
% fts 业务逻辑模块
% fts business logic module
%%%

-export([user_search_page/4]).
-export([recently_user_page/4]).
-export([search_msg/5]).
-export([search_msg/6]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("common.hrl").
-include_lib("kernel/include/logger.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% @doc 用户搜索（全文检索）
%% 使用 PostgreSQL 全文索引搜索用户
%% @param Uid 当前用户ID
%% @param Page 页码
%% @param Size 每页大小
%% @param Keyword 搜索关键词
%% @return map() 包含 total、page、size、list 的搜索结果
%%% user_search_page 好有搜索全文索引
-spec user_search_page(integer(), integer(), integer(), binary()) -> map().
user_search_page(_, Page, Size, <<>>) ->
    #{total => 0, page => Page, size => Size, list => []};
user_search_page(Uid, Page, Size, Keyword) ->
    Offset = (Page - 1) * Size,
    Total = fts_user_ds:count_for_user_search_page(Keyword),
    case fts_user_ds:user_search_page(Keyword, Size, Offset) of
        {ok, []} ->
            #{total => Total, page => Page, size => Size, list => []};
        {ok, Items0} ->
            ColumnLi = [<<"uid">>, <<"nickname">>, <<"avatar">>, <<"gender">>, <<"signature">>, <<"created_at">>],
            Items2 = [ lists:zipwith(fun(X, Y) -> {X, Y} end,
                                     [<<"is_friend">>, <<"remark">>] ++ ColumnLi,
                                     case friend_ds:is_friend(Uid, Uid2, <<"remark">>) of
                                         {B1, Remark} ->
                                             [B1, Remark]
                                     end ++ [elib_hashids:encode(Uid2), maps:get(<<"nickname">>, Row, <<>>), maps:get(<<"avatar">>, Row, <<>>), maps:get(<<"gender">>, Row, 0), maps:get(<<"signature">>, Row, <<>>), maps:get(<<"created_at">>, Row, <<>>)])
                       || #{<<"uid">> := Uid2} = Row <- Items0, Uid2 /= Uid ],
            #{total => Total, page => Page, size => Size, list => Items2};
        _ ->
            #{total => Total, page => Page, size => Size, list => []}
    end.


%% @doc 最近用户搜索（全文检索）
%% 搜索允许被搜索的用户，支持关键词过滤
%% @param Uid 当前用户ID
%% @param Page 页码
%% @param Size 每页大小
%% @param Keyword 搜索关键词（可选）
%% @return map() 包含 total、page、size、list 的搜索结果
-spec recently_user_page(integer(), integer(), integer(), binary()) -> map().
recently_user_page(Uid, Page, Size, Keyword) ->
    Column = <<?DEF_USER_COLUMN/binary, ",created_at">>,
    Offset = (Page - 1) * Size,
    case Keyword of
        <<>> ->
            Tb = <<"public.user u LEFT JOIN public.fts_user fts ON fts.user_id = u.id">>,
            WhereMap = #{<<"fts.allow_search">> => 1},
            OrderBy = <<"u.created_at desc">>,
            case elib_pg:page_with_total(Tb, Column, WhereMap, OrderBy, Page, Size) of
                {ok, #{total := Total, list := Rows}} ->
                    ColumnLi = [re:replace(B, <<"^\\s+|\\s+$">>, <<>>, [global, {return, binary}]) || B <- binary:split(Column, <<",">>, [global])],
                    Items0 = [ list_to_tuple([maps:get(Name, Row) || Name <- ColumnLi]) || Row <- Rows ],
                    Items1 = [ tuple_to_list(Item) || Item <- Items0 ],
                    Items2 = [ lists:zipwith(fun(X, Y) -> {X, Y} end,
                                             [<<"is_friend">>, <<"remark">>] ++ ColumnLi,
                                             case friend_ds:is_friend(Uid, Uid2, <<"remark">>) of
                                                 {B1, Remark} ->
                                                     [B1, Remark]
                                             end ++ [elib_hashids:encode(Uid2) | Row])
                               || [Uid2 | Row] <- Items1, Uid2 /= Uid ],
                    #{total => Total, page => Page, size => Size, list => Items2};
                {error, _} ->
                    #{total => 0, page => Page, size => Size, list => []}
            end;
        _ ->
            Total = fts_user_ds:count_for_user_search_page(Keyword),
            Rows =
                case fts_user_ds:user_search_page(Keyword, Size, Offset) of
                    {ok, Items} -> Items;
                    _ -> []
                end,
            case Rows of
                [] ->
                    #{total => Total, page => Page, size => Size, list => []};
                _ ->
                    ColumnLi = [re:replace(B, <<"^\\s+|\\s+$">>, <<>>, [global, {return, binary}]) || B <- binary:split(Column, <<",">>, [global])],
                    Items0 = [ list_to_tuple([maps:get(Name, Row) || Name <- ColumnLi]) || Row <- Rows ],
                    Items1 = [ tuple_to_list(Item) || Item <- Items0 ],
                    Items2 = [ lists:zipwith(fun(X, Y) -> {X, Y} end,
                                             [<<"is_friend">>, <<"remark">>] ++ ColumnLi,
                                             case friend_ds:is_friend(Uid, Uid2, <<"remark">>) of
                                                 {B1, Remark} ->
                                                     [B1, Remark]
                                             end ++ [elib_hashids:encode(Uid2) | Row])
                               || [Uid2 | Row] <- Items1, Uid2 /= Uid ],
                    #{total => Total, page => Page, size => Size, list => Items2}
            end
    end.


%% @doc 消息全文搜索
%% 搜索私聊或群聊消息内容
%% @param Uid 当前用户ID（用于权限检查）
%% @param Page 页码
%% @param Size 每页大小
%% @param Keyword 搜索关键词
%% @param Type 消息类型：C2C（私聊）或 C2G（群聊）
%% @return map() 包含 total、page、size、list 的搜索结果
%% @end
-spec search_msg(integer(), integer(), integer(), binary(), binary()) -> map().
search_msg(Uid, Page, Size, Keyword, Type) ->
    search_msg(Uid, Page, Size, Keyword, Type, #{}).

%% @doc 消息全文搜索（增强版）
%% 搜索私聊或群聊消息内容，支持多种筛选条件
%% @param Uid 当前用户ID（用于权限检查）
%% @param Page 页码
%% @param Size 每页大小
%% @param Keyword 搜索关键词
%% @param Type 消息类型：C2C（私聊）或 C2G（群聊）
%% @param Options 筛选选项 map：
%%   - start_date: 开始日期 (binary: <<"YYYY-MM-DD">>)
%%   - end_date: 结束日期 (binary: <<"YYYY-MM-DD">>)
%%   - msg_type: 消息类型 (binary: <<"text">>, <<"image">>, <<"video">>, etc.)
%%   - from_uid: 发送者ID (integer)
%%   - conversation_id: 会话ID (integer)
%%   - sort_by: 排序方式 (binary: <<"relevance">> | <<"time">>)
%% @return map() 包含 total、page、size、list、filters 的搜索结果
%% @end
-spec search_msg(integer(), integer(), integer(), binary(), binary(), map()) -> map().
search_msg(Uid, Page, Size, Keyword, Type, Options) ->
    Offset = (Page - 1) * Size,
    TypeBin = binary:upper_case(Type),

    % 判断是否使用增强搜索
    HasOptions = map_size(Options) > 0,

    case TypeBin of
        <<"C2C">> when HasOptions ->
            Total = fts_user_repo:search_c2c_msg_count_with_options(Keyword, Options),
            case fts_user_repo:search_c2c_msg_with_options(Keyword, Size, Offset, Options) of
                {ok, []} ->
                    #{total => Total, page => Page, size => Size, list => [], filters => Options};
                {ok, Items} ->
                    Items2 = [format_c2c_msg_item(Msg) || Msg <- Items],
                    #{total => Total, page => Page, size => Size, list => Items2, filters => Options};
                {error, _} ->
                    #{total => Total, page => Page, size => Size, list => [], filters => Options}
            end;
        <<"C2C">> ->
            Total = fts_user_repo:search_c2c_msg_count(Keyword),
            case fts_user_repo:search_c2c_msg(Keyword, Size, Offset, Uid) of
                {ok, []} ->
                    #{total => Total, page => Page, size => Size, list => []};
                {ok, Items} ->
                    Items2 = [format_c2c_msg_item(Msg) || Msg <- Items],
                    #{total => Total, page => Page, size => Size, list => Items2};
                {error, _} ->
                    #{total => Total, page => Page, size => Size, list => []}
            end;
        <<"C2G">> when HasOptions ->
            Total = fts_user_repo:search_c2g_msg_count_with_options(Keyword, Options),
            case fts_user_repo:search_c2g_msg_with_options(Keyword, Size, Offset, Options) of
                {ok, []} ->
                    #{total => Total, page => Page, size => Size, list => [], filters => Options};
                {ok, Items} ->
                    Items2 = [format_c2g_msg_item(Msg) || Msg <- Items],
                    #{total => Total, page => Page, size => Size, list => Items2, filters => Options};
                {error, _} ->
                    #{total => Total, page => Page, size => Size, list => [], filters => Options}
            end;
        <<"C2G">> ->
            Total = fts_user_repo:search_c2g_msg_count(Keyword),
            case fts_user_repo:search_c2g_msg(Keyword, Size, Offset, Uid) of
                {ok, []} ->
                    #{total => Total, page => Page, size => Size, list => []};
                {ok, Items} ->
                    Items2 = [format_c2g_msg_item(Msg) || Msg <- Items],
                    #{total => Total, page => Page, size => Size, list => Items2};
                {error, _} ->
                    #{total => Total, page => Page, size => Size, list => []}
            end;
        _ ->
            #{total => 0, page => Page, size => Size, list => []}
    end.


%% @doc 格式化私聊消息结果
%% @param Msg 消息映射
%% @return 格式化后的消息结果
%% @end
-spec format_c2c_msg_item(map()) -> map().
format_c2c_msg_item(Msg) ->
    BaseMap = #{
        <<"message_id">> => elib_hashids:encode(maps:get(<<"id">>, Msg)),
        <<"msg_type">> => maps:get(<<"msg_type">>, Msg),
        <<"payload">> => maps:get(<<"payload">>, Msg),
        <<"created_at">> => maps:get(<<"created_at">>, Msg),
        <<"from">> => elib_hashids:encode(maps:get(<<"from_id">>, Msg)),
        <<"to">> => elib_hashids:encode(maps:get(<<"to_id">>, Msg)),
        <<"status">> => maps:get(<<"status">>, Msg, 0)
    },
    % 添加高亮字段（如果存在）
    BaseMap2 = case maps:get(<<"highlight">>, Msg, undefined) of
        undefined -> BaseMap;
        Highlight -> maps:put(<<"highlight">>, Highlight, BaseMap)
    end,
    % 添加会话类型
    maps:put(<<"conversation_type">>, <<"c2c">>, BaseMap2).


%% @doc 格式化群聊消息结果
%% @param Msg 消息映射
%% @return 格式化后的消息结果
%% @end
-spec format_c2g_msg_item(map()) -> map().
format_c2g_msg_item(Msg) ->
    BaseMap = #{
        <<"message_id">> => elib_hashids:encode(maps:get(<<"id">>, Msg)),
        <<"msg_type">> => maps:get(<<"msg_type">>, Msg),
        <<"payload">> => maps:get(<<"payload">>, Msg),
        <<"created_at">> => maps:get(<<"created_at">>, Msg),
        <<"from">> => elib_hashids:encode(maps:get(<<"from_id">>, Msg)),
        <<"group_id">> => elib_hashids:encode(maps:get(<<"group_id">>, Msg)),
        <<"status">> => maps:get(<<"status">>, Msg, 0)
    },
    % 添加高亮字段（如果存在）
    BaseMap2 = case maps:get(<<"highlight">>, Msg, undefined) of
        undefined -> BaseMap;
        Highlight -> maps:put(<<"highlight">>, Highlight, BaseMap)
    end,
    % 添加会话类型
    maps:put(<<"conversation_type">>, <<"c2g">>, BaseMap2).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

