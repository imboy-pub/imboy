-module(user_collect_repo).
%%%
% user_collect 相关操作都放到该模块，存储库模块
% collect related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([count_by_uid_kind_id/2]).
-export([delete/2]).
-export([update/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"user_collect">>).


% user_collect_repo:count_by_uid_kind_id(107, <<"">>).
count_by_uid_kind_id(Uid, KindId) ->
    Uid2 = integer_to_binary(Uid),
    % use index uk_user_collect_UserId_Status_kindId
    imboy_db:pluck(tablename(),
                   <<"user_id = ", Uid2/binary, " and status = 1 and kind_id = '", KindId/binary, "'">>,
                   <<"count(*) as count">>,
                   0).

% {ok, 1} | {ok, 1, {ReturningField}}
-spec delete(integer(), binary()) -> {ok, integer()} | {ok, integer(), tuple()}.
delete(Uid, KindId) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND kind_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    imboy_db:execute(Sql, [Uid, KindId]).


-spec update(integer(), binary(), list()) -> {ok, integer()} | {ok, integer(), tuple()}.
update(Uid, KindId, Data) ->
    Table = tablename(),
    Set = imboy_db:get_set(Data),
    Where = <<" WHERE user_id = $1 AND kind_id = $2">>,
    Sql = <<"UPDATE ", Table/binary, " SET ", Set/binary, Where/binary>>,
    imboy_db:execute(Sql, [Uid, KindId]).
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
