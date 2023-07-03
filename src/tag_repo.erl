-module (tag_repo).
%%%
% tag 相关操作都放到该模块，存储库模块
% tag related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export([count_for_where/1, page_for_where/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"tag">>).

% tag_repo:count_for_where(107).
count_for_where(Where) ->
    Tb = tablename(),
    % use index i_user_collect_UserId_Status_Hashid
    imboy_db:pluck(
        <<Tb/binary>>
        , Where
        , <<"count(*) as count">>
        , 0
    ).

% tag_repo:page_for_where(1, 10, 0, <<"id desc">>).
-spec page_for_where(integer(), integer(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
page_for_where(Limit, Offset, Where, OrderBy) ->

    Column = <<"id, name, referer_time, updated_at, created_at">>,
    Where2 = <<" WHERE ", Where/binary," ORDER BY ", OrderBy/binary," LIMIT $1 OFFSET $2">>,

    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where2/binary>>,
    % lager:info(io_lib:format("user_tag_repo:page_for_where/4 sql:~p;~n", [Sql])),
    imboy_db:query(Sql, [Limit, Offset]).

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
