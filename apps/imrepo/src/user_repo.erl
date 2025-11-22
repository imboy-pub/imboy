-module(user_repo).
%%%
% user_repo 是 user repository 缩写
%%%

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/def_column.hrl").

-export([tablename/0]).
-export ([save/1, update/2, delete/1]).

-export([find_by_email/2,
         find_by_mobile/2,
         find_by_account/2]).
-export([find_by_id/1, find_by_id/2]).
-export([list_by_ids/2]).
-export([select_by_where/4]).
-export([select_by_where/5]).

-export([update_friends_last_seen_at/2]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"user">>).


select_by_where(Where, Limit, Offset, OrderBy) ->
    select_by_where(?DEF_USER_COLUMN, Where, Limit, Offset, OrderBy).

select_by_where(Column, Where, Limit, Offset, OrderBy) ->
    Tb = tablename(),
    FtsTb = fts_user_repo:tablename(),
    Limit2 = integer_to_binary(Limit),
    Offset2 = integer_to_binary(Offset),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " u LEFT JOIN ", FtsTb/binary,
            " fts ON fts.user_id = u.id
     WHERE ", Where/binary, " order by ", OrderBy/binary, " LIMIT ", Limit2/binary, " OFFSET ", Offset2/binary>>,
    imboy_log:info(io_lib:format("user_repo/select_by_where/5: Sql ~p ~n", [Sql])),
    imboy_db:query(Sql, []).


% user_repo:find_by_email("100000@imboy.pub", <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
find_by_email(Email, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE email = $1">>,
    % ?DEBUG_LOG(["sql ", Sql]),
    imboy_db:find(Sql, [Email]).


% user_repo:find_by_mobile(<<"13692177080">>, <<"*">>).
% user_repo:find_by_mobile("13692177080", <<"*">>).
find_by_mobile(Mobile, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE mobile = $1">>,
    ?DEBUG_LOG(["sql ", Sql]),
    imboy_db:find(Sql, [Mobile]).


% user_repo:find_by_account("550138", <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
find_by_account(Account, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE account = $1">>,
    imboy_db:find(Sql, [Account]).


find_by_id(Uid) ->
    Column = <<"id,account,avatar,sign">>,
    find_by_id(Uid, Column).


find_by_id(Uid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    % ?DEBUG_LOG([Sql]),
    imboy_db:find(Sql, [Uid]).


list_by_ids(Uids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Uid) -> [Uid, ","] end, Uids),
    [_ | L2] = lists:reverse(L1),
    Ids = list_to_binary(lists:concat(L2)),
    Where = <<" WHERE id IN (", Ids/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql).


%%% 更新好友关系中的 last_seen_at
update_friends_last_seen_at(Uid, Timestamp) ->
    % 更新我是from_user_id的记录
    update_last_seen_at_by_from_uid(Uid, Timestamp),
    % 更新我是to_user_id的记录
    update_last_seen_at_by_to_uid(Uid, Timestamp).



% user_repo:save(#{mobile => <<"13692177080">>, password => imboy_password:generate(imboy_hasher:md5("admin888")), account => "13692177080A", "status" => 1, "role_id" => {1,3}, "nickname" => <<"大大大"/utf8>>, created_at => imboy_dt:now()}).
save(Data) ->
    Tb = tablename(),
    imboy_db:insert_into(Tb, Data).

% user_repo:update(1, #{role_name => <<"修改后的角色名称"/utf8>>}).
update(Id, Data) ->
    Tb = tablename(),
    Where = <<"id = ", (integer_to_binary(Id))/binary>>,
    imboy_db:update(Tb, Where, Data).

% user_repo:delete(1).
delete(Id) ->
    Tb = tablename(),
    Where = <<"id = ", (integer_to_binary(Id))/binary>>,
    % 软删除，更新状态为 -1
    imboy_db:update_by_id(Tb, Where, #{<<"status">> => -1}).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%%% 更新from_user_id为指定用户的记录
update_last_seen_at_by_from_uid(Uid, Timestamp) ->
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET last_seen_at = $1::timestamptz, updated_at = $2::timestamptz ",
            "WHERE from_user_id = $3 AND status = 1">>,
    imboy_db:execute(Sql, [Timestamp, imboy_dt:now(), Uid]).


%%% 更新to_user_id为指定用户的记录
update_last_seen_at_by_to_uid(Uid, Timestamp) ->
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET last_seen_at = $1::timestamptz, updated_at = $2::timestamptz ",
            "WHERE to_user_id = $3 AND status = 1">>,
    imboy_db:execute(Sql, [Timestamp, imboy_dt:now(), Uid]).
