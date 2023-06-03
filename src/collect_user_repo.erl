-module (collect_user_repo).
%%%
% collect_user 相关操作都放到该模块，存储库模块
% collect related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export([count_for_where/1, page_for_where/3]).
-export([count_by_uid_kind_id/2]).

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
    imboy_db:public_tablename(<<"collect_user">>).


% collect_user_repo:count_by_uid_kind_id(107, <<"">>).
count_by_uid_kind_id(Uid, KindId) ->
    Uid2 = integer_to_binary(Uid),
    % use index uk_collect_user_UserId_Status_kindId
    imboy_db:pluck(
        tablename()
        , <<"user_id = ", Uid2/binary, " and status = 1 and kind_id = '", KindId/binary, "'">>
        , <<"count(*) as count">>
        , 0
    ).


% collect_user_repo:count_for_where(107).
count_for_where(Where) ->
    Tb = tablename(),
    % use index i_collect_user_UserId_Status_Hashid
    imboy_db:pluck(
        <<Tb/binary, " cu">>
        , Where
        , <<"count(*) as count">>
        , 0
    ).

%%% 用户的收藏分页列表
% collect_user_repo:page_for_where(1, 10, 0).
-spec page_for_where(integer(), integer(), binary()) ->
    {ok, list(), list()} | {error, any()}.
page_for_where(Limit, Offset, Where) ->
    Column = <<"cu.kind, cu.kind_id, cu.source, cu.created_at, r.info">>,
    Resource = imboy_db:public_tablename(<<"collect_resource">>),
    Join1 = <<"left join ", Resource/binary, " as r on r.kind_id = cu.kind_id ">>,
    Where2 = <<" WHERE ", Where/binary," ORDER BY cu.id desc LIMIT $1 OFFSET $2">>,

    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " as cu ",
        Join1/binary,
        Where2/binary>>,
    % ?LOG(['Sql', Sql]),
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
