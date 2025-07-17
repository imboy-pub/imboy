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
-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/def_column.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%%% user_search_page 好有搜索全文索引
-spec user_search_page(integer(), integer(), integer(), binary()) -> ok.
user_search_page(_, Page, Size, <<>>) ->
    imboy_response:page_payload(0, Page, Size, []);
user_search_page(Uid, Page, Size, Keyword) ->
    Offset = (Page - 1) * Size,
    Total = fts_user_repo:count_for_user_search_page(Keyword),
    case fts_user_repo:user_search_page(Keyword, Size, Offset) of
        {ok, _, []} ->
            imboy_response:page_payload(Total, Page, Size, []);
        {ok, ColumnLi, Items0} ->
            Items1 = [ tuple_to_list(Item) || Item <- Items0 ],
            Items2 = [ lists:zipwith(fun(X, Y) -> {X, Y} end,
                                     [<<"is_friend">>, <<"remark">>] ++ ColumnLi,
                                     case friend_ds:is_friend(Uid, Uid2) of
                                         {B1, Remark} ->
                                             [B1, Remark];
                                         _ ->
                                             [false, <<>>]
                                     end ++ [imboy_hashids:encode(Uid2) | Row])
                       || [Uid2 | Row] <- Items1, Uid2 /= Uid ],
            imboy_response:page_payload(Total, Page, Size, Items2);
        _ ->
            imboy_response:page_payload(Total, Page, Size, [])
    end.


-spec recently_user_page(integer(), integer(), integer(), binary()) -> ok.
recently_user_page(Uid, Page, Size, Keyword) ->
    Offset = (Page - 1) * Size,

    WhereLi =
        case Keyword of
            <<>> ->
                ["fts.allow_search = 1"];
            Kwd ->
                ["fts.allow_search = 1", <<" AND fts.token @@ to_tsquery('jiebacfg', '", Kwd, "'">>]
        end,
    Column = <<?DEF_USER_COLUMN/binary, ",created_at">>,
    Where = imboy_cnv:implode(" ", WhereLi),
    Total = imboy_db:pluck(<<"user">>, Where, <<"count(*) as count">>, 0),
    OrderBy = <<"u.created_at desc">>,
    case user_repo:select_by_where(Column, Where, Size, Offset, OrderBy) of
        {ok, _, []} ->
            imboy_response:page_payload(Total, Page, Size, []);
        {ok, ColumnLi, Items0} ->
            Items1 = [ tuple_to_list(Item) || Item <- Items0 ],
            Items2 = [ lists:zipwith(fun(X, Y) -> {X, Y} end,
                                     [<<"is_friend">>, <<"remark">>] ++ ColumnLi,
                                     case friend_ds:is_friend(Uid, Uid2) of
                                         {B1, Remark} ->
                                             [B1, Remark];
                                         _ ->
                                             [false, <<>>]
                                     end ++ [imboy_hashids:encode(Uid2) | Row])
                       || [Uid2 | Row] <- Items1, Uid2 /= Uid ],
            imboy_response:page_payload(Total, Page, Size, Items2);
        _ ->
            imboy_response:page_payload(Total, Page, Size, [])
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
