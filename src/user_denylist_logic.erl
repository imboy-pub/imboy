-module(user_denylist_logic).
%%%
% user_denylist 业务逻辑模块
% user_denylist business logic module
%%%

-export ([add/3, remove/2]).
-export ([page/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------
%%% 查找非好友

-spec page(Uid::integer(), Page::integer(), Size::integer()) -> list().
page(Uid, Page,  Size) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = user_denylist_repo:count_by_uid(Uid),
    case user_denylist_repo:page(Uid, Size, Offset) of
        {ok, _, []} ->
            imboy_response:page_payload(Total, Page, Size, []);
        {ok, ColumnLi, Items} ->
            Items2 = [lists:zipwith(fun(X, Y) -> {X, Y} end,
                ColumnLi,
                [imboy_hashids:uid_encode(DeniedUserId)] ++ Row) || [DeniedUserId | Row] <- Items],
            imboy_response:page_payload(Total, Page, Size, Items2);
        _ ->
            imboy_response:page_payload(Total, Page, Size, [])
    end.

-spec add(Uid::integer(), DeniedUserId::integer(), Remark::binary()) -> integer().
add(Uid, DeniedUserId, Remark) ->
    Now = imboy_dt:millisecond(),
    user_denylist_repo:add(Uid, DeniedUserId, Remark, Now),
    Now.

-spec remove(Uid::integer(), DeniedUserId::integer()) -> ok.
remove(Uid, DeniedUserId) ->
    user_denylist_repo:remove(Uid, DeniedUserId),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------------



%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
