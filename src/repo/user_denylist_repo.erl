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

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_pg_sql:public_tablename(<<"user_denylist">>).


% user_denylist_repo:count_for_uid(107).
count_for_uid(Uid) ->
    % user_id 是 bigint 类型，需要传入 integer
    imboy_pg:pluck_value(tablename(), <<"count(*) as count">>, #{user_id => Uid}, #{}, 0).


% user_denylist_repo:page_for_uid(1, 10, 0).
-spec page_for_uid(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
page_for_uid(Uid, Limit, Offset) ->
    % Source = <<"JSON_UNQUOTE(json_extract(f.setting, '$.source')) AS source">>,
    Source = <<"f.setting::jsonb->>'source' AS source">>,
    Column =
        <<"d.denied_user_id, d.created_at, u.nickname, u.avatar, u.account, u.sign, f.remark,f.tag, u.gender, u.region,",
          Source/binary>>,

    UserTable = imboy_pg_sql:public_tablename(<<"user">>),
    UserFTable = imboy_pg_sql:public_tablename(<<"user_friend">>),
    Join1 = <<"inner join ", UserTable/binary, " as u on u.id = d.denied_user_id ">>,
    Join2 = <<"inner join ", UserFTable/binary, " as f on d.denied_user_id = f.to_user_id ">>,
    Where = <<" WHERE d.user_id = $1 and f.from_user_id = $2 LIMIT $3 OFFSET $4">>,

    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " as d ", Join1/binary, Join2/binary, Where/binary>>,
    % ?DEBUG_LOG([Sql, Uid, Limit, Offset]),
    imboy_pg:query(Sql, [Uid, Uid, Limit, Offset]).


-spec add(integer(), integer(), binary()) -> {ok, integer()}.
add(Uid, DeniedUserId, Now) ->
    {Sql, Params} = imboy_pg_sql:insert(tablename(), #{
        user_id => Uid,
        denied_user_id => DeniedUserId,
        created_at => Now
    }, <<>>),
    imboy_pg:execute(Sql, Params).

-spec remove(Uid :: integer(), DeniedUid :: integer()) -> ok | {error, any()}.
remove(Uid, DeniedUid) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE user_id = $1 AND denied_user_id = $2">>,
    case imboy_pg:execute(Sql, [Uid, DeniedUid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.


% user_denylist_repo:in_denylist(107, 62913).
% user_denylist_repo:in_denylist(4, 1).
-spec in_denylist(integer(), integer()) -> integer().
in_denylist(Uid, DeniedUid) ->
    % user_id 和 denied_user_id 是 bigint 类型，需要传入 integer
    % use index uk_UserId_DeniedUserId
    imboy_pg:pluck_value(tablename(),
                        <<"count(*) as count">>,
                        #{user_id => Uid, denied_user_id => DeniedUid},
                        #{},
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
