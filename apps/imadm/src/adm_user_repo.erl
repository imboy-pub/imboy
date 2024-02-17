-module (adm_user_repo).
%%%
% adm_user 相关操作都放到该模块，存储库模块
% adm_user related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([save/1]).

-export([find_by_email/2,
         find_by_mobile/2,
         find_by_account/2]).
-export([find_by_id/2]).
-export([find_by_ids/2]).

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
    imboy_db:public_tablename(<<"adm_user">>).



% adm_user_repo:find_by_email("10008@imboy.pub", <<"*">>).
find_by_email(Email, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE email = $1">>,
    % ?LOG(Sql),
    imboy_db:find(Sql, [Email]).


% adm_user_repo:find_by_mobile(<<"13692177080">>, <<"*">>).
% adm_user_repo:find_by_mobile("13692177080", <<"*">>).
find_by_mobile(Mobile, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE mobile = $1">>,
    % ?LOG(["sql ", Sql]),
    imboy_db:find(Sql, [Mobile]).


% adm_user_repo:find_by_account("550138", <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
find_by_account(Username, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE account = $1">>,
    imboy_db:find(Sql, [Username]).


find_by_id(Uid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    % ?LOG([Sql]),
    imboy_db:find(Sql, [Uid]).


find_by_ids(Uids, Column) ->
    Tb = tablename(),
    L1 = lists:flatmap(fun(Uid) -> [Uid, ","] end, Uids),
    [_ | L2] = lists:reverse(L1),
    Ids = list_to_binary(lists:concat(L2)),
    Where = <<" WHERE id IN (", Ids/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql).

% adm_user_repo:save(#{mobile => <<"13692177080">>, password => imboy_password:generate(imboy_hasher:md5("admin888")), account => "13692177080A", "status" => 1, "role_id" => {1,3}, "nickname" => <<"大大大"/utf8>>, created_at => imboy_dt:utc(millisecond)}).
save(Data) ->
    Tb = tablename(),
    imboy_db:insert_into(Tb, Data).

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
