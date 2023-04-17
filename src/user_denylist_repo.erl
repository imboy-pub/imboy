-module (user_denylist_repo).
%%%
% user_denylist 相关操作都放到该模块，存储库模块
% user_denylist related operations are put in this module, repository module
%%%

-export ([add/4, remove/2]).
-export ([count_by_uid/1, in_denylist/2]).
-export([page/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------
-spec page(Uid::integer(), Limit::integer(), Offset::integer()) -> mysql:query_result().
page(Uid, Limit,  Offset) ->
    Column = <<"d.denied_user_id, d.remark as reason, d.created_at, u.nickname, u.avatar, u.account, u.sign, f.remark">>,
    Join1 = <<"inner join user as u on u.id = d.denied_user_id ">>,
    Join2 = <<"inner join user_friend as f on d.denied_user_id = f.to_user_id ">>,
    Where = <<"WHERE d.`user_id` = ? and f.from_user_id = ? LIMIT ? OFFSET ?">>,

    Sql = <<"SELECT ", Column/binary, " FROM `user_denylist` as d ",
        Join1/binary,
        Join2/binary,
        Where/binary>>,
    % ?LOG([Sql, Uid, Limit, Offset]),
    mysql_pool:query(Sql, [Uid, Uid, Limit, Offset]).

-spec add(Uid::integer(), DeniedUserId::integer(), Remark::binary(), Now::integer()) -> mysql:query_result().
add(Uid, DeniedUserId, Remark, Now) ->
    Table = <<"`user_denylist`">>,
    Column = <<"(`user_id`,`denied_user_id`,`remark`,`created_at`)">>,

    Uid2 = integer_to_binary(Uid),
    DeniedUserId2 = integer_to_binary(DeniedUserId),
    Now2 = integer_to_binary(Now),

    Value = <<"('", Uid2/binary, "', '", DeniedUserId2/binary, "', '",
              Remark/binary, "', '", Now2/binary, "')">>,
    mysql_pool:replace_into(Table, Column, Value).


-spec remove(Uid::integer(), DeniedUid::integer()) -> ok.
remove(Uid, DeniedUid) ->
    Sql = <<"DELETE FROM `user_denylist`
        WHERE `user_id` = ? AND `denied_user_id` = ?">>,
    mysql_pool:execute(Sql, [Uid, DeniedUid]),
    ok.

count_by_uid(Uid) ->
    % use index uk_UserId_DeniedUserId
    Sql = <<"SELECT count(*) as count FROM `user_denylist`
        WHERE `user_id` = ?">>,
    {ok,[<<"count">>],[[Count]]} = mysql_pool:query(Sql, [Uid]),
    Count.

% user_denylist_repo:in_denylist(107, 62913).
in_denylist(Uid, DeniedUid)->
    % use index uk_UserId_DeniedUserId
    Sql = <<"SELECT count(*) as count FROM `user_denylist`
        WHERE `user_id` = ? AND `denied_user_id` = ?">>,
     % mysql_pool:query(Sql, [Uid, DeniedUid]).
    {ok,[<<"count">>],[[Count]]} = mysql_pool:query(Sql, [Uid, DeniedUid]),
    Count.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%

%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
