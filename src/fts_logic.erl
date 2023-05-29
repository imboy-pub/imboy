-module(fts_logic).
%%%
% fts 业务逻辑模块
% fts business logic module
%%%

-export ([user_search_page/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% user_search_page 好有搜索全文索引
-spec user_search_page(integer(), integer(), binary()) -> ok.
user_search_page(Page, Size, <<"">>) ->
    imboy_response:page_payload(0, Page, Size, []);
user_search_page(Page, Size, Keywrod) ->
    Offset = (Page - 1) * Size,
    Total = fts_repo:count_for_user_search_page(Keywrod),
    case fts_repo:user_search_page(Keywrod, Size, Offset) of
        {ok, _, []} ->
            imboy_response:page_payload(Total, Page, Size, []);
        {ok, ColumnLi, Items0} ->
            Items1 = [tuple_to_list(Item) || Item <- Items0],
            Items2 = [lists:zipwith(fun(X, Y) -> {X, Y} end,
                ColumnLi,
                [
                    imboy_hashids:uid_encode(DeniedUserId)] ++ Row) ||
                    [DeniedUserId | Row] <- Items1
                ],
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
