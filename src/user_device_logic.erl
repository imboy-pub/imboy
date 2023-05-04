-module(user_device_logic).
%%%
% user_device 业务逻辑模块
% user_device business logic module
%%%

-export([device_name/2, change_name/3, delete/2]).
-export([page/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec device_name(integer(), binary()) -> binary().
% Uid = 1.
% DID = <<"3f039a2b4724a5b7">>.
% Key = {user_device_name, Uid, DID}.
% user_device_logic:device_name(1, <<"3f039a2b4724a5b7">>).
%  imboy_cache:get(Key).
device_name(Uid, DID) ->
    Key = {user_device_name, Uid, DID},
    Fun = fun() -> user_device_repo:device_name(Uid, DID) end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).

-spec change_name(integer(), binary(), binary()) -> ok.
change_name(Uid, DID, Name) ->
    Set = <<"device_name = $1">>,
    SetArgs = [Name],
    user_device_repo:update_by_did(Uid, DID, Set, SetArgs),

    Key = {user_device_name, Uid, DID},
    imboy_cache:flush(Key),
    ok.

-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    user_device_repo:delete(Uid, DID),
    Key = {user_device_name, Uid, DID},
    imboy_cache:flush(Key),
    ok.

-spec page(Uid::integer(), Page::integer(), Size::integer()) -> list().
page(Uid, Page,  Size) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = user_device_repo:count_by_uid(Uid),
    case user_device_repo:page(Uid, Size, Offset) of
        {ok, _, []} ->
            imboy_response:page_payload(Total, Page, Size, []);
        {ok, ColumnLi, Items0} ->
            Items1 = [tuple_to_list(Item) || Item <- Items0],
            OnlineDids =  imboy_session:online_dids(Uid),
            Items2 = [
                lists:zipwith(fun(X, Y) -> {X, Y} end,
                [<<"online">> | ColumnLi],
                [lists:member(DID, OnlineDids), DID] ++ Row) ||
                    [DID | Row]  <- Items1
            ],
            imboy_response:page_payload(Total, Page, Size, Items2);
        _ ->
            imboy_response:page_payload(Total, Page, Size, [])
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
