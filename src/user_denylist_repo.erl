-module(user_denylist_repo).
%%%
% user_denylist 相关操作都放到该模块，存储库模块
% user_denylist related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([add/3,
         remove/2]).
-export([in_denylist/2]).
-export([count_for_uid/1,
         page_for_uid/3]).

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
    imboy_db:public_tablename(<<"user_denylist">>).


% user_denylist_repo:count_for_uid(107).
count_for_uid(Uid) ->
    Uid2 = integer_to_binary(Uid),
    % use index uk_UserId_DeniedUserId
    imboy_db:pluck(tablename(), <<"user_id = ", Uid2/binary>>, <<"count(*) as count">>, 0).


% user_denylist_repo:page_for_uid(1, 10, 0).
-spec page_for_uid(integer(), integer(), integer()) -> {ok, list(), list()} | {error, any()}.
page_for_uid(Uid, Limit, Offset) ->
    % Source = <<"JSON_UNQUOTE(json_extract(f.setting, '$.source')) AS source">>,
    Source = <<"f.setting::jsonb->>'source' AS source">>,
    Column =
        <<"d.denied_user_id, d.created_at, u.nickname, u.avatar, u.account, u.sign, f.remark,f.tag, u.gender, u.region,",
          Source/binary>>,

    UserTable = imboy_db:public_tablename(<<"user">>),
    UserFTable = imboy_db:public_tablename(<<"user_friend">>),
    Join1 = <<"inner join ", UserTable/binary, " as u on u.id = d.denied_user_id ">>,
    Join2 = <<"inner join ", UserFTable/binary, " as f on d.denied_user_id = f.to_user_id ">>,
    Where = <<" WHERE d.user_id = $1 and f.from_user_id = $2 LIMIT $3 OFFSET $4">>,

    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " as d ", Join1/binary, Join2/binary, Where/binary>>,
    % ?LOG([Sql, Uid, Limit, Offset]),
    imboy_db:query(Sql, [Uid, Uid, Limit, Offset]).


-spec add(integer(), integer(), integer()) -> {ok, integer()}.
add(Uid, DeniedUserId, Now) ->
    Tb = tablename(),
    Column = <<"(user_id,denied_user_id,created_at)">>,

    Uid2 = integer_to_binary(Uid),
    DeniedUserId2 = integer_to_binary(DeniedUserId),
    Now2 = integer_to_binary(Now),

    Value = <<"('", Uid2/binary, "', '", DeniedUserId2/binary, "', '", Now2/binary, "')">>,
    imboy_db:insert_into(Tb, Column, Value).


-spec remove(Uid :: integer(), DeniedUid :: integer()) -> ok.
remove(Uid, DeniedUid) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE user_id = $1 AND denied_user_id = $2">>,
    imboy_db:execute(Sql, [Uid, DeniedUid]),
    ok.


% user_denylist_repo:in_denylist(107, 62913).
-spec in_denylist(integer(), integer()) -> integer().
in_denylist(Uid, DeniedUid) ->
    Uid2 = integer_to_binary(Uid),
    DeniedUid2 = integer_to_binary(DeniedUid),
    % use index uk_UserId_DeniedUserId
    imboy_db:pluck(tablename(),
                   <<"user_id = ", Uid2/binary, " AND denied_user_id = ", DeniedUid2/binary>>,
                   <<"count(*) as count">>,
                   0).

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
