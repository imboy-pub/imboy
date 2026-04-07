-module(user_repo).
%%%
% user_repo 是 user repository 缩写
% 用户数据仓库层，提供用户数据的基础数据库操作
%%%

-include("log.hrl").
-include("common.hrl").

-export([tablename/0]).
-export ([save/1, update/2, delete/1]).
-export([create/1, find_by_uid/1]).
-export([page/2, page/4]).

-export([find_by_email/2,
         find_by_mobile/2,
         find_by_account/2]).
-export([find_by_id/1, find_by_id/2]).
-export([list_by_ids/2]).

-export([update_friends_last_seen_at/2]).
-export([may_exist/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取用户表的表名
%% @return 返回用户表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user">>).

%% @doc 分页查询用户
-spec page(integer(), integer()) -> {ok, map()} | {error, any()}.
page(Page, Size) ->
    page(Page, Size, #{}, <<"created_at DESC">>).

%% @doc 分页查询用户（带条件）
-spec page(integer(), integer(), map(), binary()) -> {ok, map()} | {error, any()}.
page(Page, Size, Where, OrderBy) ->
    Tb = tablename(),
    Column = <<"id,account,nickname,COALESCE(NULLIF(mobile, ''), account) AS mobile,"
               "email,avatar,gender,region,sign,status,created_at">>,
    elib_pg:page_with_total(Tb, Column, Where, OrderBy, Page, Size).

%% @doc 兼容旧接口：创建用户
-spec create(map()) -> ok | {error, term()}.
create(Data0) ->
    Data = normalize_legacy_create_data(Data0),
    case save(Data) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 兼容旧接口：按 uid 查询用户
-spec find_by_uid(integer() | binary()) -> {ok, map()} | {error, term()}.
find_by_uid(Uid) ->
    case find_by_id(ec_cnv:to_integer(Uid), <<"*">>) of
        #{} = Row when map_size(Row) > 0 ->
            {ok, Row};
        #{} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.




%% @doc 根据邮箱查找用户
%% @param Email 用户邮箱地址
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return map() 用户信息，未找到时返回空map
%% @example user_repo:find_by_email(<<"100000@imboy.pub">>, <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
-spec find_by_email(binary(), binary()) -> map().
find_by_email(Email, Column) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{email => Email}, #{limit => 1}),
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, Params)).


%% @doc 根据手机号查找用户
%% @param Mobile 用户手机号码，支持binary或string类型
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return map() 用户信息，未找到时返回空map
%% @example user_repo:find_by_mobile(<<"13692177080">>, <<"*">>).
%% @example user_repo:find_by_mobile("13692177080", <<"*">>).
-spec find_by_mobile(binary() | string(), binary()) -> map().
find_by_mobile(Mobile, Column) when is_binary(Mobile); is_list(Mobile) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{mobile => Mobile}, #{limit => 1}),
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, Params)).


%% @doc 根据用户账号查找用户
%% @param Account 用户账号（字符串或binary类型）
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return map() 用户信息，未找到时返回空map
%% @example user_repo:find_by_account("550138", <<"id,account,mobile,password,nickname,avatar,gender,region,sign">>).
-spec find_by_account(binary() | string(), binary()) -> map().
find_by_account(Account, Column) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{account => Account}, #{limit => 1}),
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, Params)).


%% @doc 根据用户ID查找用户基本信息（使用默认列）
%% @param Uid 用户ID
%% @return map() 用户信息，未找到时返回空map
-spec find_by_id(pos_integer()) -> map().
find_by_id(Uid) ->
    Column = <<"id,account,avatar,sign">>,
    find_by_id(Uid, Column).


%% @doc 根据用户ID查找用户（指定列）
%% @param Uid 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return map() 用户信息，未找到时返回空map
-spec find_by_id(pos_integer(), binary()) -> map().
find_by_id(Uid, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, [Uid])).


%% @doc 根据用户ID列表批量查询用户信息
%% @param Uids 用户ID列表，元素类型为integer或binary，不能为空列表
%% @param Column 要查询的列名，支持多个列用逗号分隔，或使用 "*" 查询所有列
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @throws function_clause 当 Uids 为空列表时
-spec list_by_ids(list(pos_integer() | binary()), binary()) -> {ok, list(map())} | {error, term()}.
list_by_ids(Uids, Column) when length(Uids) > 0 ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => {in, Uids}}, #{}),
    elib_pg:query(Sql, Params).


%% @doc 更新指定用户的所有好友关系中的最后在线时间
%% @param Uid 用户ID
%% @param Timestamp 要更新的时间戳（timestamptz格式）
%% @return ok
%% @details 该函数会更新用户作为from_user_id和to_user_id的所有好友关系记录
-spec update_friends_last_seen_at(pos_integer(), binary()) -> ok.
update_friends_last_seen_at(Uid, Timestamp) ->
    % 更新我是from_user_id的记录
    _ = update_last_seen_at(<<"from_user_id">>, Uid, Timestamp),
    % 更新我是to_user_id的记录
    _ = update_last_seen_at(<<"to_user_id">>, Uid, Timestamp),
    ok.

%% @doc 检查用户是否存在（轻量级检查）
%% @param Uid 用户ID
%% @return true | false
%% @details 只查询 id 字段，避免加载不必要的字段
-spec may_exist(pos_integer()) -> boolean().
may_exist(Uid) when is_integer(Uid), Uid > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT id FROM ", Tb/binary, " WHERE id = $1 AND status >= 0 LIMIT 1">>,
    case elib_pg:query(Sql, [Uid]) of
        {ok, _, [_]} -> true;
        _ -> false
    end;
may_exist(_) ->
    false.



%% @doc 保存新用户记录
%% @param Data 包含用户信息的map，必须包含mobile、password、account等必要字段
%% @return {ok, Count} 保存成功 | {error, Reason} 保存失败
%% @example user_repo:save(#{mobile => <<"13692177080">>, password => elib_password:generate(elib_hasher:md5("admin888")), account => "13692177080A", status => 1, role_id => {1,3}, nickname => <<"大大大"/utf8>>, created_at => elib_dt:now()}).
-spec save(map()) -> {ok, integer()} | {error, term()}.
save(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(user),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 更新用户信息
%% @param Id 用户ID
%% @param Data 包含要更新字段的map
%% @return {ok, Count} 更新成功 | {error, Reason} 更新失败
%% @example user_repo:update(1, #{role_name => <<"修改后的角色名称"/utf8>>}).
-spec update(pos_integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Id, Data) ->
    Tb = tablename(),
    elib_pg:update(Tb, Data, <<"id = $1">>, [Id]).

%% @doc 删除用户（软删除）
%% @param Id 用户ID
%% @return {ok, Count} 删除成功 | {error, Reason} 删除失败
%% @details 实际上是软删除，将用户状态更新为 -1
%% @example user_repo:delete(1).
-spec delete(pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete(Id) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => -1}, <<"id = $1">>, [Id]).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 更新指定字段为指定用户的好友关系记录的最后在线时间
%% @param Field 字段名 (from_user_id 或 to_user_id)
%% @param Uid 用户ID
%% @param Timestamp 要更新的时间戳
%% @return {ok, Count} | {error, Reason}
-spec update_last_seen_at(binary(), pos_integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
update_last_seen_at(Field, Uid, Timestamp) ->
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET last_seen_at = $1::timestamptz, updated_at = $2::timestamptz ",
            "WHERE ", Field/binary, " = $3 AND status = 1">>,
    elib_pg:execute(Sql, [Timestamp, elib_dt:now(), Uid]).

%% @doc 兼容旧测试数据结构（uid/name 等）并补齐非空字段默认值
-spec normalize_legacy_create_data(map()) -> map().
normalize_legacy_create_data(Data0) ->
    Id = pick_value(Data0, [id, <<"id">>, uid, <<"uid">>], 0),
    Nickname = pick_value(Data0, [nickname, <<"nickname">>], <<"">>),
    Account = pick_value(Data0, [account, <<"account">>], Nickname),
    Password = pick_value(Data0, [password, <<"password">>], <<"password123">>),
    Mobile = pick_value(Data0, [mobile, <<"mobile">>], <<>>),
    Email = pick_value(Data0, [email, <<"email">>], <<>>),
    Region = pick_value(Data0, [region, <<"region">>], <<>>),
    Avatar = pick_value(Data0, [avatar, <<"avatar">>], <<>>),
    Sign = pick_value(Data0, [sign, <<"sign">>], <<>>),
    #{
        id => ec_cnv:to_integer(Id),
        nickname => ec_cnv:to_binary(Nickname),
        account => ec_cnv:to_binary(Account),
        password => ec_cnv:to_binary(Password),
        mobile => ec_cnv:to_binary(Mobile),
        email => ec_cnv:to_binary(Email),
        region => ec_cnv:to_binary(Region),
        avatar => ec_cnv:to_binary(Avatar),
        sign => ec_cnv:to_binary(Sign),
        status => 1,
        created_at => elib_dt:now(),
        reg_ip => <<"127.0.0.1">>,
        reg_cosv => <<"perf-test">>
    }.

-spec pick_value(map(), [atom() | binary()], term()) -> term().
pick_value(_Map, [], Default) ->
    Default;
pick_value(Map, [Key | Rest], Default) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            pick_value(Map, Rest, Default)
    end.
