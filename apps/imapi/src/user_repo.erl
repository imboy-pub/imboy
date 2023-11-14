-module(user_repo).
%%%
% user_repo 是 user repository 缩写
%%%

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/def_column.hrl").

-export([tablename/0]).
-export([find_by_email/2,
         find_by_mobile/2,
         find_by_account/2]).
-export([find_by_id/1, find_by_id/2]).
-export([find_by_ids/2]).
-export([select_by_where/4]).


%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"user">>).


select_by_where(Where, Limit, Offset, OrderBy) ->
    Tb = tablename(),
    FtsTb = fts_repo:tablename(),
    Limit2 = integer_to_binary(Limit),
    Offset2 = integer_to_binary(Offset),
    Sql = <<"SELECT ", ?DEF_USER_COLUMN/binary, " FROM ", Tb/binary, " u LEFT JOIN ", FtsTb/binary,
            " fts ON fts.user_id = u.id
     WHERE ", Where/binary, " order by ", OrderBy/binary, " LIMIT ", Limit2/binary, " OFFSET ", Offset2/binary>>,
    imboy_log:info(io_lib:format("user_repo/select_by_where/4: Sql ~p ~n", [Sql])),
    imboy_db:query(Sql, []).


% user_repo:find_by_email("10008@imboy.pub", <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
find_by_email(Email, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE email = $1">>,
    imboy_db:query(Sql, [Email]).


% user_repo:find_by_mobile(<<"13692177080">>, <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
% user_repo:find_by_mobile("13692177080", <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
find_by_mobile(Mobile, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE mobile = $1">>,
    imboy_db:query(Sql, [Mobile]).


% user_repo:find_by_account("550138", <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
find_by_account(Username, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE account = $1">>,
    imboy_db:query(Sql, [Username]).


find_by_id(Uid) ->
    Column = <<"id,account,avatar,sign">>,
    find_by_id(Uid, Column).


find_by_id(Uid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    % ?LOG([Sql]),
    imboy_db:query(Sql, [Uid]).


find_by_ids(Uids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Uid) -> [Uid, ","] end, Uids),
    [_ | L2] = lists:reverse(L1),
    Ids = list_to_binary(lists:concat(L2)),
    Where = <<" WHERE id IN (", Ids/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
