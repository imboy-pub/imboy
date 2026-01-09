-module(friend_ds).
%%%
% friend_ds 是 friend domain service 缩写
%%%
-export([is_friend/2]).
-export([is_friend/3]).
-export([is_friend_fields/3]).
-export([list_by_uid/1]).
-export([page_by_uid/1, page_by_uid/3]).
-export([page_by_cid/4]).
-export([page_by_tag/5]).
-export([change_remark/3]).
-export([set_category_id/3]).

-include("log.hrl").
-include("def_column.hrl").

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
                      {error, _Reason} ->
                          {false, <<>>}
                  end
          end,
    %  缓存key挺多，是针对用户ID的，缓存时间不宜过长
    % 缓存1天，
    imboy_cache:memo(Fun, Key, 86400).

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
    Key = {is_friend_fields, FromUid, ToUid, Fields},
    Fun = fun() ->
                  case friend_repo:friend_fields(FromUid, ToUid, Fields) of
                      {ok, [Row |_]} when is_map(Row) ->
                          {true, Row};
                      {error, _Reason} ->
                          {false, #{}}
                  end
          end,
    %  缓存key挺多，是针对用户ID的，缓存时间不宜过长
    % 缓存1天，
    imboy_cache:memo(Fun, Key, 86400).

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
    TagName = imboy_pg:pluck_value(<<"public.user_tag">>, <<"name">>, #{id => TagId}, #{}, <<>>),
    case TagName of
        <<>> ->
            #{total => 0, page => Page, size => Size, list => []};
        _ ->
            UserTable = imboy_pg_sql:public_tablename(<<"user">>),
            UserDTable = imboy_pg_sql:public_tablename(<<"user_denylist">>),
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
            case imboy_pg:page_with_total(BaseFrom, fields(Uid), WhereMap, OrderBy, Page, Size) of
                {ok, #{total := Total, list := Rows}} ->
                    Items = [ imboy_hashids:replace_id(user_logic:online_state(User)) || User <- Rows ],
                    #{total => Total, page => Page, size => Size, list => Items};
                {error, Reason} ->
                    _ = imboy_log:error(Reason),
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
-spec page(binary(), [term()], binary()) -> [map()] | [].
page(Where, WhereArgs, Fields) ->
    UserTable = imboy_pg_sql:public_tablename(<<"user">>),
    UserDTable = imboy_pg_sql:public_tablename(<<"user_denylist">>),
    Join1 = <<"left join ", UserDTable/binary, " as d on d.denied_user_id = f.to_user_id ">>,
    Join2 = <<"inner join ", UserTable/binary, " as u on u.id = f.to_user_id ">>,
    Tb = friend_repo:tablename(),
    Sql = <<"SELECT ", Fields/binary, " FROM ", Tb/binary, " as f ", Join1/binary, Join2/binary, Where/binary>>,
    % ?DEBUG_LOG([Sql, WhereArgs]),
    case imboy_pg:query(Sql, WhereArgs) of
        {ok, Rows} when Fields == <<"count(*) count">> ->
            Rows;
        {ok, []} ->
            [];
        {ok, Rows} when is_list(Rows) ->
            [ imboy_hashids:replace_id(user_logic:online_state(User)) || User <- Rows ];
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
