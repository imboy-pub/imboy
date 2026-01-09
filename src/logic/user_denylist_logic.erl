-module(user_denylist_logic).

%%%
% user_denylist 业务逻辑模块
% user_denylist business logic module
%%%

-export([add/2,
         remove/2]).
-export([page/3]).
-export([in_denylist/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%%% 黑名单分页列表
-spec page(integer(), integer(), integer()) -> map().
page(Uid, Page, Size) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = user_denylist_repo:count_for_uid(Uid),
    case user_denylist_repo:page_for_uid(Uid, Size, Offset) of
        {ok, []} ->
            #{total => Total, page => Page, size => Size, list => []};
        {ok, Items0} ->
            Items2 = [#{<<"denied_user_id">> => imboy_hashids:encode(maps:get(<<"denied_user_id">>, Row)),
                        <<"created_at">> => maps:get(<<"created_at">>, Row, <<>>),
                        <<"nickname">> => maps:get(<<"nickname">>, Row, <<>>),
                        <<"avatar">> => maps:get(<<"avatar">>, Row, <<>>),
                        <<"account">> => maps:get(<<"account">>, Row, <<>>),
                        <<"sign">> => maps:get(<<"sign">>, Row, <<>>),
                        <<"remark">> => maps:get(<<"remark">>, Row, <<>>),
                        <<"tag">> => maps:get(<<"tag">>, Row, <<>>),
                        <<"gender">> => maps:get(<<"gender">>, Row, 0),
                        <<"region">> => maps:get(<<"region">>, Row, <<>>),
                        <<"source">> => maps:get(<<"source">>, Row, <<>>)} || Row <- Items0],
            #{total => Total, page => Page, size => Size, list => Items2};
        _ ->
            #{total => Total, page => Page, size => Size, list => []}
    end.


-spec add(integer(), integer()) -> binary().
add(Uid, DeniedUserId) ->
    Now = imboy_dt:now(),
    _ = user_denylist_repo:add(Uid, DeniedUserId, Now),
    Key = {in_denylist, Uid, DeniedUserId},
    imboy_cache:flush(Key),
    Now.


-spec remove(integer(), integer()) -> ok.
remove(Uid, DeniedUserId) ->
    _ = user_denylist_repo:remove(Uid, DeniedUserId),
    Key = {in_denylist, Uid, DeniedUserId},
    imboy_cache:flush(Key),
    ok.


% user_denylist_logic:in_denylist(107, 62913).
-spec in_denylist(integer(), integer()) -> integer().
in_denylist(Uid, DeniedUserId) ->
    Key = {in_denylist, Uid, DeniedUserId},
    Fun = fun() -> user_denylist_repo:in_denylist(Uid, DeniedUserId) end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
