-module(friend_ds).
%%%
% friend_ds 是 friend domain service 缩写
%%%
-export([is_friend/2]).
-export([is_friend/3]).
-export([is_friend_fields/3]).
-export([check_relationship/2]).  %% 新增：联合查询函数
-export([list_by_uid/1]).
-export([page_by_uid/1, page_by_uid/3]).
-export([page_by_cid/4]).
-export([page_by_tag/5]).
-export([change_remark/3]).
-export([set_category_id/3]).
-export([confirm_friend/7]).
-export([delete/2]).
-export([move_to_category/3]).
-export([invalidate_cache/2]).

-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 根据用户ID获取好友列表
%%
%% 获取指定用户的所有好友ID列表
%%
%% @param Uid 用户ID
%% @returns list() 好友用户ID列表
% friend_ds:list_by_uid(1).
-spec list_by_uid(integer()) -> [map()].
list_by_uid(Uid) ->
    Column = <<"to_user_id">>,
    case friend_repo:list_by_uid(Uid, Column) of
        {ok, []} ->
            [];
        {ok, Rows} ->
            [ToUid || #{<<"to_user_id">> := ToUid} <- Rows]
    end.

%% @doc 检查好友关系是否存在
%%
%% 检查目标用户是否为源用户的好友，返回布尔值（ToUid 是 FromUid 的好友）
%%
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @returns boolean() 好友关系状态
% friend_ds:is_friend(1, 3)
-spec is_friend(integer(), integer()) -> boolean().
is_friend(FromUid, ToUid) ->
    {IsF, _} = friend_ds:is_friend(FromUid, ToUid, <<"remark">>),
    IsF.

%% @doc 检查好友关系并获取指定字段值
%%
%% 检查好友关系是否存在，并返回指定字段的信息，使用缓存提高性能
%%
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @param Field 要查询的字段名
%% @returns {boolean(), binary()} 好友关系状态和字段值
% friend_ds:is_friend(1, 3, <<"remark">>).
-spec is_friend(integer(), integer(), binary()) -> {boolean(), binary()}.
is_friend(FromUid, ToUid, Field) ->
    Key = {is_friend2, FromUid, ToUid},
    Fun = fun() ->
                  case friend_repo:friend_field(FromUid, ToUid, Field) of
                      {ok, [#{Field := Val} |_]} ->
                          {true, Val};
                      {ok, _} ->
                          % 没有好友记录
                          {false, <<>>};
                      {error, _Reason} ->
                          {false, <<>>}
                  end
          end,
    %% 【缓存一致性修复】缓存时间从 1 天缩短到 5 分钟（300 秒）
    imboy_cache:memo(Fun, Key, 300).

%% @doc 检查好友关系并获取多个字段值
%%
%% 检查好友关系是否存在，并返回多个字段的信息，使用缓存提高性能
%%
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @param Fields 要查询的字段列表，如 [<<"remark">>, <<"created_at">>]
%% @returns {boolean(), map()} 好友关系状态和字段值map
% friend_ds:is_friend_fields(1, 3, [<<"remark">>, <<"created_at">>]).
-spec is_friend_fields(integer(), integer(), [binary()]) -> {boolean(), map()}.
is_friend_fields(FromUid, ToUid, Fields) ->
    Key = {is_friend_fields2, FromUid, ToUid, Fields},
    Fun = fun() ->
                  case friend_repo:friend_fields(FromUid, ToUid, Fields) of
                      {ok, [Row |_]} when is_map(Row) ->
                          {true, Row};
                      {ok, _} ->
                          % 没有好友记录
                          {false, #{}};
                      {error, _Reason} ->
                          {false, #{}}
                  end
          end,
    %% 【缓存一致性修复】缓存时间从 1 天缩短到 5 分钟（300 秒）
    imboy_cache:memo(Fun, Key, 300).

%% @doc 分页获取用户好友列表
%%
%% 使用默认参数分页获取用户的好友列表
%%
%% @param Uid 用户ID
%% @returns list() 好友信息列表
% friend_ds:page_by_uid(1).
-spec page_by_uid(integer()) -> [map()].
page_by_uid(Uid) ->
    page_by_uid(Uid, 1000, 0).

%% @doc 分页获取用户好友列表
%%
%% 根据指定的限制和偏移量分页获取用户的好友列表
%%
%% @param Uid 用户ID
%% @param Limit 每页数量限制
%% @param Offset 偏移量
%% @returns list() 好友信息列表
-spec page_by_uid(integer(), integer(), integer()) -> [map()].
page_by_uid(Uid, Limit, Offset) ->
    Where = <<"WHERE f.status = 1 AND f.from_user_id = $1 LIMIT $2 OFFSET $3">>,
    WhereArgs = [Uid, Limit, Offset],
    page(Where, WhereArgs, fields(Uid)).

%% @doc 按分类ID分页获取好友列表
%%
%% 根据指定的分类ID分页获取用户的好友列表
%%
%% @param Cid 分类ID
%% @param Uid 用户ID
%% @param Limit 每页数量限制
%% @param Offset 偏移量
%% @returns list() 好友信息列表
% friend_ds:page_by_cid(1, 1, 10, 0).
-spec page_by_cid(integer(), integer(), integer(), integer()) -> [map()].
page_by_cid(Cid, Uid, Limit, Offset) ->
    Where = <<"WHERE f.status = 1 AND f.from_user_id = $1 AND f.category_id = $2 LIMIT $3 OFFSET $4">>,
    WhereArgs = [Uid, Cid, Limit, Offset],
    page(Where, WhereArgs, fields(Uid)).


%% @doc 按标签分页获取好友列表
%%
%% 根据指定标签和关键词分页获取用户的好友列表，支持关键词搜索
%%
%% @param Uid 用户ID
%% @param Page 页码（从1开始）
%% @param Size 每页大小
%% @param TagId 标签ID
%% @param Kwd 搜索关键词
%% @returns map() 分页结果 #{total => Total, page => Page, size => Size, list => Items}
% friend_ds:page_by_tag(31, 1, 10, 15, <<>>).
-spec page_by_tag(integer(), integer(), integer(), integer(), binary()) -> map().
page_by_tag(Uid, Page, Size, TagId, Kwd) when Page > 0 ->
    TagName = elib_pg:pluck_value(<<"public.user_tag">>, <<"name">>, #{id => TagId}, #{}, <<>>),
    case TagName of
        <<>> ->
            #{total => 0, page => Page, size => Size, list => []};
        _ ->
            UserTable = elib_pg_sql:public_tablename(<<"user">>),
            UserDTable = elib_pg_sql:public_tablename(<<"user_denylist">>),
            Join1 = <<"left join ", UserDTable/binary, " as d on d.denied_user_id = f.to_user_id ">>,
            Join2 = <<"inner join ", UserTable/binary, " as u on u.id = f.to_user_id ">>,
            BaseFrom = <<(friend_repo:tablename())/binary, " as f ", Join1/binary, Join2/binary>>,
            TagNamePattern = <<TagName/binary, ",%">>,
            OrderBy = <<"u.id desc">>,
            WhereMap =
                case bit_size(Kwd) > 0 of
                    true ->
                        KwdPattern = <<"%", Kwd/binary, ",%">>,
                        #{<<"__and">> => [
                            #{<<"f.status">> => 1,
                              <<"f.from_user_id">> => Uid,
                              <<"f.tag">> => {op, <<"LIKE">>, TagNamePattern}},
                            #{<<"__or">> => [
                                #{<<"f.tag">> => {op, <<"LIKE">>, KwdPattern}},
                                #{<<"f.remark">> => {op, <<"LIKE">>, KwdPattern}},
                                #{<<"u.nickname">> => {op, <<"LIKE">>, KwdPattern}},
                                #{<<"u.sign">> => {op, <<"LIKE">>, KwdPattern}}
                            ]}
                        ]};
                    false ->
                        #{<<"f.status">> => 1,
                          <<"f.from_user_id">> => Uid,
                          <<"f.tag">> => {op, <<"LIKE">>, TagNamePattern}}
                end,
            case elib_pg:page_with_total(BaseFrom, fields(Uid), WhereMap, OrderBy, Page, Size) of
                {ok, #{total := Total, list := Rows}} ->
                    % 【优化】使用批量在线状态查询，避免 N+1 查询问题
                    Items = [ elib_hashids:replace_id(User) || User <- user_logic:batch_online_state(Rows) ],
                    #{total => Total, page => Page, size => Size, list => Items};
                {error, Reason} ->
                    _ = elib_log:error(Reason),
                    #{total => 0, page => Page, size => Size, list => []}
            end
    end.



%% @doc 执行分页查询
%%
%% 根据查询条件执行分页查询，关联用户表和用户黑名单表
%%
%% @param Where 查询条件
%% @param WhereArgs 查询参数
%% @param Fields 查询字段
%% @returns list() 查询结果列表
-spec page(binary(), list(), binary()) -> [map()].
page(Where, WhereArgs, Fields) ->
    UserTable = elib_pg_sql:public_tablename(<<"user">>),
    UserDTable = elib_pg_sql:public_tablename(<<"user_denylist">>),
    Join1 = <<"left join ", UserDTable/binary, " as d on d.denied_user_id = f.to_user_id ">>,
    Join2 = <<"inner join ", UserTable/binary, " as u on u.id = f.to_user_id ">>,
    Tb = friend_repo:tablename(),
    Sql = <<"SELECT ", Fields/binary, " FROM ", Tb/binary, " as f ", Join1/binary, Join2/binary, Where/binary>>,
    % ?DEBUG_LOG([Sql, WhereArgs]),
    case elib_pg:query(Sql, WhereArgs) of
        {ok, Rows} when Fields == <<"count(*) count">> ->
            Rows;
        {ok, []} ->
            [];
        {ok, Rows} when is_list(Rows) ->
            % 【优化】使用批量在线状态查询，避免 N+1 查询问题
            [ elib_hashids:replace_id(User) || User <- user_logic:batch_online_state(Rows) ];
        {error, _Reason} ->
            []
    end.


%% @doc 修改好友备注
%%
%% 修改指定好友的备注信息
%%
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @param Remark 备注信息
%% @returns {ok, integer()} | {error, any()} 数据库操作结果
% friend_ds:change_remark(1, 2, <<" 1 to 2 f">>).
-spec change_remark(integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
change_remark(FromUid, ToUid, Remark) ->
    friend_repo:change_remark(FromUid, ToUid, Remark).

%% @doc 设置好友分类ID
%%
%% 修改指定好友的分类ID
%%
%% @param Uid 用户ID
%% @param CategoryId 当前分类ID
%% @param NewCid 新分类ID
%% @returns {ok, integer()} | {error, any()} 数据库操作结果
% friend_ds:set_category_id(1, 1, 0).
-spec set_category_id(integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
set_category_id(Uid, CategoryId, NewCid) ->
    friend_repo:set_category_by_cid(Uid, CategoryId, NewCid).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 内部函数：生成查询字段列表
%%
%% 根据用户ID生成好友查询所需的字段列表，包括用户信息和好友关系信息
%%
%% @param Uid 用户ID，可以是整数或二进制格式
%% @returns binary() 查询字段字符串
-spec fields(integer() | binary()) -> binary().
fields(Uid) when is_integer(Uid) ->
    fields(integer_to_binary(Uid));
fields(Uid) ->
    C_IsFriend = <<" case when d.user_id = ", Uid/binary,
                   " and d.denied_user_id = u.id then 0 else 1 end as is_friend,">>,
    C_IsFrom = <<"f.setting::jsonb->>'is_from' AS is_from,">>,
    C_Source = <<"f.setting::jsonb->>'source' AS source,">>,
    C2 = <<C_IsFrom/binary, C_Source/binary, C_IsFriend/binary, "f.remark, f.tag, f.category_id,f.created_at">>,
    <<"id,", F2/binary>> = ?DEF_USER_COLUMN,
    <<"u.id,", F2/binary, ",", C2/binary>>.


%% ===================================================================
%% 优化函数：联合查询
%% ===================================================================

%% @doc 联合查询好友关系和黑名单状态
%%
%% 一次性查询两个用户之间的关系状态（是否好友、是否在黑名单）
%% 相比分别调用 is_friend/2 和 user_denylist_logic:in_denylist/2，这个函数只进行一次数据库查询
%%
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @returns {IsFriend :: boolean(), InDenylist :: boolean()}
%%          IsFriend: 是否是好友
%%          InDenylist: 黑名单状态（0 表示不在黑名单，其他值表示在黑名单）
%%
%% @doc 朋友关系和黑名单联合查询
-spec check_relationship(integer(), integer()) -> {boolean(), boolean()}.
check_relationship(FromUid, ToUid) ->
    Key = {check_relationship3, FromUid, ToUid},
    Fun = fun() ->
        % 使用 LEFT JOIN 同时查询好友关系和黑名单状态
        UserDTable = elib_pg_sql:public_tablename(<<"user_denylist">>),
        Sql = <<
            "SELECT "
            "EXISTS(SELECT 1 FROM ", (friend_repo:tablename())/binary, " "
            "WHERE ((from_user_id = $1 AND to_user_id = $2 AND status = 1) OR "
            "(from_user_id = $2 AND to_user_id = $1 AND status = 1))) as is_friend, "
            "EXISTS(SELECT 1 FROM ", UserDTable/binary, " "
            "WHERE user_id = $1 AND denied_user_id = $2) as in_denylist"
        >>,
        % elib_log:info([<<"check_relationship">>, Sql]),
        case elib_pg:query(Sql, [FromUid, ToUid]) of
            {ok, [#{<<"is_friend">> := IsFriend, <<"in_denylist">> := InDenylist}]} ->
                {IsFriend, InDenylist};
            {ok, []} ->
                {false, false};
            {error, _Reason} ->
                {false, false}
        end
    end,
    % 【缓存一致性修复】统一缓存时间为 5 分钟（300 秒），与 is_friend/3 保持一致
    imboy_cache:memo(Fun, Key, 300).

%% @doc 确认好友关系
%% @param IsFriend 是否已经是好友
%% @param FromID 源用户ID
%% @param ToID 目标用户ID
%% @param Remark 备注
%% @param Setting 设置信息
%% @param Tag 标签
%% @param NowTs 时间戳
%% @return ok
-spec confirm_friend(boolean(), integer(), integer(), binary(), map() | binary() | undefined, binary(), binary()) -> ok.
confirm_friend(IsFriend, FromID, ToID, Remark, Setting, Tag, NowTs) ->
    friend_repo:confirm_friend(IsFriend, FromID, ToID, Remark, Setting, Tag, NowTs).

%% @doc 删除好友
%% @param FromID 源用户ID
%% @param ToID 目标用户ID
%% @return ok | {error, Reason}
-spec delete(integer(), integer()) -> ok | {error, any()}.
delete(FromID, ToID) ->
    Result = friend_repo:delete(FromID, ToID),
    %% 【缓存一致性修复】删除后主动失效缓存
    case Result of
        ok -> invalidate_cache(FromID, ToID);
        {error, _} -> Result
    end.

%% @doc 移动好友到分组
%% @param FromUID 源用户ID
%% @param ToUID 目标用户ID
%% @param CategoryId 分类ID
%% @return ok | {error, Reason}
-spec move_to_category(integer(), integer(), integer()) -> ok | {error, any()}.
move_to_category(FromUID, ToUID, CategoryId) ->
    friend_repo:move_to_category(FromUID, ToUID, CategoryId).

%% @doc 主动失效好友关系缓存
%%
%% 当好友关系变更时（添加、删除），主动清除相关缓存
%% 避免长时间缓存导致的数据不一致问题
%%
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @return ok
-spec invalidate_cache(integer(), integer()) -> ok.
invalidate_cache(FromUid, ToUid) ->
    %% 清除 is_friend2 缓存
    imboy_cache:flush({is_friend2, FromUid, ToUid}),
    imboy_cache:flush({is_friend2, ToUid, FromUid}),
    %% 清除 is_friend_fields 缓存（使用通配符模式）
    lists:foreach(fun(Key) ->
        case imboy_cache:get(Key) of
            {ok, _} -> imboy_cache:flush(Key);
            _ -> ok
        end
    end, [
        {is_friend_fields, FromUid, ToUid, '_'},
        {is_friend_fields, ToUid, FromUid, '_'}
    ]),
    %% 清除 check_relationship 缓存
    imboy_cache:flush({check_relationship, FromUid, ToUid}),
    imboy_cache:flush({check_relationship, ToUid, FromUid}),
    ok.
