-module (feedback_reply_repo).
%%%
% feedback_reply 相关操作都放到该模块，存储库模块
% feedback_reply related operations are put in this module, repository module
%%%

-export ([tablename/0]).

-export([count_for_where/1,
         page_for_where/4]).

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
    imboy_db:public_tablename(<<"feedback_reply">>).

% feedback_repo:count_for_where(<<"user_id=1">>).
count_for_where(Where) ->
    Tb = tablename(),
    % use index i_user_collect_UserId_Status_Hashid
    imboy_db:pluck(<<Tb/binary>>, Where, <<"count(*) as count">>, 0).


%%% 用户的收藏分页列表
% feedback_repo:page_for_where(1, 10, 0, <<"id desc">>).
-spec page_for_where(integer(), integer(), binary(), binary()) -> {ok, list(), list()} | {error, any()}.
page_for_where(Limit, Offset, Where, OrderBy) ->
    Column = <<"id as feedback_reply_id, feedback_id, feedback_reply_pid, replier_user_id, replier_name, body, status, updated_at, created_at">>,
    Where2 = <<" WHERE ", Where/binary, " ORDER BY ", OrderBy/binary, " LIMIT $1 OFFSET $2">>,

    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where2/binary>>,
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
