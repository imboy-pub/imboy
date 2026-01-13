-module(user_denylist_repo).
%%%
% user_denylist 相关操作都放到该模块，存储库模块
% user_denylist related operations are put in this module, repository module
% 用户黑名单数据仓库层，提供用户黑名单信息的基础数据库操作
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


%% @doc 获取用户黑名单表的表名
%% @return 返回用户黑名单表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_denylist">>).


%% @doc 统计用户黑名单数量
%% @param Uid 用户ID
%% @return Count 黑名单数量
%% @example user_denylist_repo:count_for_uid(107).
-spec count_for_uid(integer()) -> non_neg_integer().
count_for_uid(Uid) ->
    % user_id 是 bigint 类型，需要传入 integer
    elib_pg:pluck_value(tablename(), <<"count(*) as count">>, #{user_id => Uid}, #{}, 0).


%% @doc 分页查询用户黑名单
%% @param Uid 用户ID
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @return {ok, Rows} 查询成功返回黑名单用户列表 | {error, Reason} 查询失败
%% @example user_denylist_repo:page_for_uid(1, 10, 0).
-spec page_for_uid(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
page_for_uid(Uid, Limit, Offset) ->
    % Source = <<"JSON_UNQUOTE(json_extract(f.setting, '$.source')) AS source">>,
    Source = <<"f.setting::jsonb->>'source' AS source">>,
    Column =
        <<"d.denied_user_id, d.created_at, u.nickname, u.avatar, u.account, u.sign, f.remark,f.tag, u.gender, u.region,",
          Source/binary>>,

    UserTable = elib_pg_sql:public_tablename(<<"user">>),
    UserFTable = elib_pg_sql:public_tablename(<<"user_friend">>),
    Join1 = <<"inner join ", UserTable/binary, " as u on u.id = d.denied_user_id ">>,
    Join2 = <<"inner join ", UserFTable/binary, " as f on d.denied_user_id = f.to_user_id ">>,
    Where = <<" WHERE d.user_id = $1 and f.from_user_id = $2 LIMIT $3 OFFSET $4">>,

    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " as d ", Join1/binary, Join2/binary, Where/binary>>,
    % ?DEBUG_LOG([Sql, Uid, Limit, Offset]),
    elib_pg:query(Sql, [Uid, Uid, Limit, Offset]).


%% @doc 添加用户到黑名单
%% @param Uid 用户ID
%% @param DeniedUserId 被加入黑名单的用户ID
%% @param Now 创建时间
%% @return {ok, Id} 添加成功返回记录ID
-spec add(integer(), integer(), binary()) -> {ok, integer()}.
add(Uid, DeniedUserId, Now) ->
    {Sql, Params} = elib_pg_sql:insert(tablename(), #{
        user_id => Uid,
        denied_user_id => DeniedUserId,
        created_at => Now
    }, <<>>),
    elib_pg:execute(Sql, Params).

%% @doc 从黑名单移除用户
%% @param Uid 用户ID
%% @param DeniedUid 被移除黑名单的用户ID
%% @return ok 移除成功 | {error, Reason} 移除失败
-spec remove(Uid :: integer(), DeniedUid :: integer()) -> ok | {error, any()}.
remove(Uid, DeniedUid) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE user_id = $1 AND denied_user_id = $2">>,
    case elib_pg:execute(Sql, [Uid, DeniedUid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%% @doc 检查用户是否在黑名单中
%% @param Uid 用户ID
%% @param DeniedUid 被检查的用户ID
%% @return Count 在黑名单中返回1，否则返回0
%% @example user_denylist_repo:in_denylist(107, 62913).
%% @example user_denylist_repo:in_denylist(4, 1).
-spec in_denylist(integer(), integer()) -> integer().
in_denylist(Uid, DeniedUid) ->
    % user_id 和 denied_user_id 是 bigint 类型，需要传入 integer
    % use index uk_UserId_DeniedUserId
    elib_pg:pluck_value(tablename(),
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
