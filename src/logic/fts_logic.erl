-module(fts_logic).

%%%
% fts 业务逻辑模块
% fts business logic module
%%%

-export([user_search_page/4]).
-export([recently_user_page/4]).

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


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

