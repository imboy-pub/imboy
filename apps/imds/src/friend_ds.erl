-module(friend_ds).
%%%
% friend_ds 是 friend domain service 缩写
%%%
-export([is_friend/2]).
-export([is_friend/3]).
-export([list_by_uid/1]).
-export([page_by_uid/1, page_by_uid/3]).
-export([page_by_cid/4]).
-export([page_by_tag/5]).
-export([change_remark/3]).
-export([set_category_id/3]).

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/def_column.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 根据用户ID获取好友列表
%%
%% 获取指定用户的所有好友ID列表
%%
%% @param Uid 用户ID
%% @returns list() 好友用户ID列表
-spec list_by_uid(integer()) -> list().
list_by_uid(Uid) ->
    Column = <<"to_user_id">>,
    case friend_repo:list_by_uid(Uid, Column) of
        {ok, _, []} ->
            [];
        {ok, _ColumnList, Rows} ->
            [ToUid || {ToUid} <- Rows]
    end.

%% @doc 检查好友关系是否存在
%%
%% 检查目标用户是否为源用户的好友，返回布尔值（ToUid 是 FromUid 的好友）
%%
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @returns {boolean(), any()} 好友关系状态和相关信息
% friend_ds:is_friend(1, 3)
-spec is_friend(integer(), integer()) -> {boolean(), any()}.
is_friend(FromUid, ToUid) ->
    {Res, _} = friend_ds:is_friend(FromUid, ToUid, <<"remark">>),
    Res.

%% @doc 检查好友关系并获取指定字段值
%%
%% 检查好友关系是否存在，并返回指定字段的信息，使用缓存提高性能
%%
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @param Field 要查询的字段名
%% @returns boolean() 好友关系状态
% friend_ds:is_friend(1, 3, <<"remark">>).
-spec is_friend(integer(), integer(), binary()) -> boolean().
is_friend(FromUid, ToUid, Field) ->
    Key = {is_friend, FromUid, ToUid},
    Fun = fun() ->
                  case friend_repo:friend_field(FromUid, ToUid, Field) of
                      {ok, _ColumnLi, [{Val}]} ->
                          {true, Val};
                      _ ->
                          {false, <<>>}
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
-spec page_by_uid(integer()) -> list().
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
-spec page_by_uid(integer(), integer(), integer()) -> list().
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
-spec page_by_cid(integer(), integer(), integer(), integer()) -> list().
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
%% @returns list() 分页结果
-spec page_by_tag(integer(), integer(), integer(), integer(), binary()) -> list().
page_by_tag(Uid, Page, Size, TagId, Kwd) when Page > 0 ->
    Offset = (Page - 1) * Size,
    TagId2 = integer_to_binary(TagId),
    % TagName = <<"aaa">>,
    TagName = imboy_db:pluck(<<"user_tag">>, <<"id = ", TagId2/binary>>, <<"name">>, <<>>),
    {Total0, Items0} =
        if
            TagName == <<>> ->
                {0, []};
            bit_size(Kwd) > 0 ->
                Where0 = imboy_cnv:implode(" AND ",
                                            ["f.status = 1",
                                             "f.from_user_id = $1",
                                             <<"f.tag like '%", TagName/binary, ",%'">>,
                                             <<"(f.tag like '%", Kwd/binary, ",%' OR f.remark like '%", Kwd/binary,
                                               "%' OR u.nickname like '%", Kwd/binary, "%' OR u.sign like '%",
                                               Kwd/binary, "%')">>]),
                Where = <<"WHERE ", Where0/binary, " LIMIT $2 OFFSET $3">>,

                WhereArgs = [Uid, Size, Offset],

                Items = page(Where, WhereArgs, fields(Uid)),
                Total = count(Where, WhereArgs),
                {Total, Items};
            true ->
                Where = <<"WHERE f.status = 1 AND f.from_user_id = $1 AND f.tag like '%", TagName/binary,
                          ",%' LIMIT $2 OFFSET $3">>,

                WhereArgs = [Uid, Size, Offset],

                Items = page(Where, WhereArgs, fields(Uid)),
                Total = count(Where, WhereArgs),
                {Total, Items}
        end,
    imboy_response:page_payload(Total0, Page, Size, Items0).


%% @doc 计算好友数量
%%
%% 根据查询条件统计好友数量
%%
%% @param Where 查询条件
%% @param WhereArgs 查询参数
%% @returns list() 统计结果列表
-spec count(binary(), list()) -> list().
count(Where, WhereArgs) ->
    case page(Where, WhereArgs, <<"count(*) count">>) of
        [{Count}] ->
            Count;
        _ ->
            0
    end.

%% @doc 执行分页查询
%%
%% 根据查询条件执行分页查询，关联用户表和用户黑名单表
%%
%% @param Where 查询条件
%% @param WhereArgs 查询参数
%% @param Fields 查询字段
%% @returns list() 查询结果列表
-spec page(binary(), list(), binary()) -> list().
page(Where, WhereArgs, Fields) ->
    UserTable = imboy_db:public_tablename(<<"user">>),
    UserDTable = imboy_db:public_tablename(<<"user_denylist">>),
    Join1 = <<"left join ", UserDTable/binary, " as d on d.denied_user_id = f.to_user_id ">>,
    Join2 = <<"inner join ", UserTable/binary, " as u on u.id = f.to_user_id ">>,

    Tb = friend_repo:tablename(),
    Sql = <<"SELECT ", Fields/binary, " FROM ", Tb/binary, " as f ", Join1/binary, Join2/binary, Where/binary>>,
    % Res = imboy_db:query(Sql, WhereArgs),
    % ?DEBUG_LOG([Res]),
    % ok.
    % ?DEBUG_LOG([Sql, WhereArgs]),
    case imboy_db:query(Sql, WhereArgs) of
        {ok, _ColumnList, Rows} when Fields == <<"count(*) count">> ->
            Rows;
        {ok, _, []} ->
            [];
        {ok, ColumnList, Rows} ->
            Friends = [ lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnList, tuple_to_list(Row)) || Row <- Rows ],
            [ user_logic:online_state(imboy_hashids:replace_id(User)) || User <- Friends ];
        _ ->
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
    Tb = friend_repo:tablename(),
    Dt = imboy_dt:now(),
    ?DEBUG_LOG([Dt]),
    Sql = <<"UPDATE ", Tb/binary, " SET remark = $1, updated_at = $2
        WHERE status = $3 AND from_user_id = $4 AND to_user_id = $5">>,
    imboy_db:execute(Sql, [Remark, Dt, 1, FromUid, ToUid]).

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
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET category_id = $1, updated_at = $2
        WHERE status = $3 AND from_user_id = $4 AND category_id = $5">>,
    imboy_db:execute(Sql, [NewCid, imboy_dt:now(), 1, Uid, CategoryId]).


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
