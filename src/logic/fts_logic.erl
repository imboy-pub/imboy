-module(fts_logic).

%%%
% fts 业务逻辑模块
% fts business logic module
%%%

-export([user_search_page/4]).
-export([recently_user_page/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include("def_column.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%%% user_search_page 好有搜索全文索引
-spec user_search_page(integer(), integer(), integer(), binary()) -> map().
user_search_page(_, Page, Size, <<>>) ->
    #{total => 0, page => Page, size => Size, list => []};
user_search_page(Uid, Page, Size, Keyword) ->
    Offset = (Page - 1) * Size,
    Total = fts_user_repo:count_for_user_search_page(Keyword),
    case fts_user_repo:user_search_page(Keyword, Size, Offset) of
        {ok, []} ->
            #{total => Total, page => Page, size => Size, list => []};
        {ok, Items0} ->
            ColumnLi = [<<"uid">>, <<"nickname">>, <<"avatar">>, <<"gender">>, <<"signature">>, <<"created_at">>],
            Items2 = [ lists:zipwith(fun(X, Y) -> {X, Y} end,
                                     [<<"is_friend">>, <<"remark">>] ++ ColumnLi,
                                     case friend_ds:is_friend(Uid, Uid2, <<"remark">>) of
                                         {B1, Remark} ->
                                             [B1, Remark]
                                     end ++ [imboy_hashids:encode(Uid2), maps:get(<<"nickname">>, Row, <<>>), maps:get(<<"avatar">>, Row, <<>>), maps:get(<<"gender">>, Row, 0), maps:get(<<"signature">>, Row, <<>>), maps:get(<<"created_at">>, Row, <<>>)])
                       || #{<<"uid">> := Uid2} = Row <- Items0, Uid2 /= Uid ],
            #{total => Total, page => Page, size => Size, list => Items2};
        _ ->
            #{total => Total, page => Page, size => Size, list => []}
    end.


-spec recently_user_page(integer(), integer(), integer(), binary()) -> map().
recently_user_page(Uid, Page, Size, Keyword) ->
    Column = <<?DEF_USER_COLUMN/binary, ",created_at">>,
    % 使用参数化查询，如果Keyword为空则只查询allow_search条件
    {Tb, WhereMap, _SqlWhere} =
        case Keyword of
            <<>> ->
                {<<"public.user u LEFT JOIN public.fts_user fts ON fts.user_id = u.id">>,
                 #{<<"fts.allow_search">> => 1},
                 <<"fts.allow_search = 1">>};
            _Kwd2 ->
                {
                <<"public.user u LEFT JOIN public.fts_user fts ON fts.user_id = u.id">>,
                 #{<<"fts.allow_search">> => 1,
                   <<"fts.token">> => {raw, <<"fts.token @@ to_tsquery('jiebacfg', $1)">>}
                },
                 <<"fts.allow_search = 1 AND fts.token @@ to_tsquery('jiebacfg', $1)">>}
        end,
    OrderBy = <<"u.created_at desc">>,
    case imboy_pg:page_with_total(Tb, Column, WhereMap, OrderBy, Page, Size) of
        {ok, #{total := Total, list := Rows}} ->
            ColumnLi = [re:replace(B, <<"^\\s+|\\s+$">>, <<>>, [global, {return, binary}]) || B <- binary:split(Column, <<",">>, [global])],
            Items0 = [ list_to_tuple([maps:get(Name, Row) || Name <- ColumnLi]) || Row <- Rows ],
            Items1 = [ tuple_to_list(Item) || Item <- Items0 ],
            Items2 = [ lists:zipwith(fun(X, Y) -> {X, Y} end,
                                     [<<"is_friend">>, <<"remark">>] ++ ColumnLi,
                                     case friend_ds:is_friend(Uid, Uid2, <<"remark">>) of
                                         {B1, Remark} ->
                                             [B1, Remark]
                                     end ++ [imboy_hashids:encode(Uid2) | Row])
                       || [Uid2 | Row] <- Items1, Uid2 /= Uid ],
            #{total => Total, page => Page, size => Size, list => Items2};
        {error, _} ->
            #{total => 0, page => Page, size => Size, list => []}
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

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
