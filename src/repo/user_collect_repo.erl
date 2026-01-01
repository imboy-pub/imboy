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
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_pg_sql:public_tablename(<<"user_collect">>).


% user_collect_repo:count_by_uid_kind_id(2, <<"cqi6od1pa9gjnlt3a5a0">>).
count_by_uid_kind_id(Uid, KindId) ->
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"SELECT count(*) as count FROM ", (tablename())/binary, " WHERE user_id = $1 AND status = 1 AND kind_id = $2">>,
    case imboy_pg:query(Sql, [Uid, KindId]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

% {ok, 1}
-spec delete(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete(Uid, KindId) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND kind_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    case imboy_pg:execute(Sql, [Uid, KindId]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.


% user_collect_repo:update(2, <<"cqi6od1pa9gjnlt3a5a0">>, #{<<"updated_at">> => imboy_dt:now()}).
-spec update(integer(), binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Uid, KindId, Data) ->
    Table = tablename(),
    Where = <<"user_id = $1 AND kind_id = $2">>,
    imboy_pg:update(Table, Data, Where, [Uid, KindId]).
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
