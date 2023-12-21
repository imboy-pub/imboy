-module (feedback_repo).
%%%
% feedback 相关操作都放到该模块，存储库模块
% feedback related operations are put in this module, repository module
%%%

-export ([tablename/0]).

-export([count_for_where/1, page_for_where/5]).

-export ([add/11]).
-export([save/2]).
-export([delete/2]).

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
    imboy_db:public_tablename(<<"feedback">>).


% feedback_repo:count_for_where(<<"user_id=1">>).
count_for_where(Where) ->
    Tb = tablename(),
    % use index i_user_collect_UserId_Status_Hashid
    imboy_db:pluck(<<Tb/binary>>, Where, <<"count(*) as count">>, 0).


%%% 用户的收藏分页列表
% feedback_repo:page_for_where(1, 10, 0, <<"id desc">>, <<"*">>).
-spec page_for_where(integer(), integer(), binary(), binary(), binary()) -> {ok, list(), list()} | {error, any()}.
page_for_where(Limit, Offset, Where, OrderBy, Column) ->
    Where2 = <<" WHERE ", Where/binary, " ORDER BY ", OrderBy/binary, " LIMIT $1 OFFSET $2">>,

    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where2/binary>>,
    % ?LOG(['Sql', Sql]),
    imboy_db:query(Sql, [Limit, Offset]).

%%% 新增用户反馈
-spec add(integer(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
% feedback_repo:add(Uid, Did, COS, COSV, AppVsn, Title, Body, Attach, FeedbackMd5)
add(Uid, Did, COS, COSV, AppVsn, Type, Rating, Title, Body, Attach, FeedbackMd5) ->
    Tb = tablename(),
    Column = <<"(user_id, device_id, client_operating_system, client_operating_system_vsn, app_vsn, type, rating, title, body, attach, feedback_md5, status, created_at)">>,
    Value = [
        Uid
        , <<"'", Did/binary, "'">>
        , <<"'", COS/binary, "'">>
        , <<"'", COSV/binary, "'">>
        , <<"'", AppVsn/binary, "'">>
        , <<"'", Type/binary, "'">>
        , <<"'", Rating/binary, "'">>
        , <<"'", Title/binary, "'">>
        , <<"'", Body/binary, "'">>
        , <<"'", Attach/binary, "'">>
        , <<"'", FeedbackMd5/binary, "'">>
        , 1
        , imboy_dt:millisecond()
    ],
    imboy_db:insert_into(Tb, Column, Value),
    ok.

-spec save(integer(), binary()) -> ok.
save(Uid, FeedbackId) ->
    Tb = tablename(),
    % 状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE status = 1 AND user_id = $1 AND id = $2">>,
    imboy_db:execute(Sql, [Uid, FeedbackId]),
    ok.

-spec delete(integer(), binary()) -> ok.
delete(Uid, FeedbackId) ->
    Tb = tablename(),
    % 状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE status = 1 AND user_id = $1 AND id = $2">>,
    imboy_db:execute(Sql, [Uid, FeedbackId]),
    ok.

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
