-module(group_category_logic).

%%%
% group_category 业务逻辑模块
% 提供群组分类的业务逻辑处理
%%%

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% 查询操作
-export([list/1]).

%% 增删改操作
-export([create/2]).
-export([rename/3]).
-export([delete/2]).
-export([move_group/3]).
-export([update_sort_order/2]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 创建群组分类
%% @param Uid 用户ID
%% @param CategoryName 分类名称
%% @return {ok, CategoryId} 创建成功返回分类ID | {error, Reason} 创建失败
-spec create(integer(), binary()) -> {ok, integer()} | {error, binary()}.
create(_Uid, <<>>) ->
    {error, <<"分类名称不能为空"/utf8>>};
create(_Uid, CategoryName) when byte_size(CategoryName) > 50 ->
    {error, <<"分类名称不能超过50个字符"/utf8>>};
create(Uid, CategoryName) ->
    case group_category_ds:add(Uid, CategoryName) of
        {ok, CategoryId} ->
            {ok, CategoryId};
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, _Reason} ->
            {error, <<"创建分类失败"/utf8>>}
    end.

%% @doc 查询用户的群组分类列表
%% @param Uid 用户ID
%% @return {ok, Categories} 查询成功返回分类列表 | {error, Reason} 查询失败
-spec list(integer()) -> {ok, list(map())} | {error, binary()}.
list(Uid) ->
    try
        Categories = group_category_ds:find_by_uid(Uid),
        {ok, Categories}
    catch
        _Error:_Reason ->
            {error, <<"查询分类列表失败"/utf8>>}
    end.

%% @doc 重命名群组分类
%% @param Uid 用户ID
%% @param CategoryId 分类ID
%% @param NewName 新的分类名称
%% @return ok 重命名成功 | {error, Reason} 重命名失败
-spec rename(integer(), integer(), binary()) -> ok | {error, binary()}.
rename(_Uid, _CategoryId, <<>>) ->
    {error, <<"分类名称不能为空"/utf8>>};
rename(_Uid, _CategoryId, NewName) when byte_size(NewName) > 50 ->
    {error, <<"分类名称不能超过50个字符"/utf8>>};
rename(Uid, CategoryId, NewName) ->
    case group_category_ds:rename(Uid, CategoryId, NewName) of
        {ok, 1} ->
            ok;
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, _Reason} ->
            {error, <<"重命名分类失败"/utf8>>}
    end.

%% @doc 删除群组分类
%% 删除分类时会将该分类下的群组移到默认分类（category_id=0）
%% @param Uid 用户ID
%% @param CategoryId 分类ID
%% @return ok 删除成功 | {error, Reason} 删除失败
-spec delete(integer(), integer()) -> ok | {error, binary()}.
delete(_Uid, 0) ->
    {error, <<"不能删除默认分类"/utf8>>};
delete(Uid, CategoryId) ->
    case group_category_ds:delete(Uid, CategoryId) of
        {ok, 1} ->
            ok;
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, _Reason} ->
            {error, <<"删除分类失败"/utf8>>}
    end.

%% @doc 移动群组到指定分类
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @param CategoryId 分类ID（0表示未分类）
%% @return ok 移动成功 | {error, Reason} 移动失败
-spec move_group(integer(), integer(), integer()) -> ok | {error, binary()}.
move_group(Uid, Gid, CategoryId) ->
    %% 验证群组成员是否存在
    case group_member_repo:find(Gid, Uid, <<"id">>) of
        #{<<"id">> := _} ->
            %% 验证分类是否存在（如果是0则跳过验证）
            ValidateResult = case CategoryId of
                0 ->
                    ok;
                _ ->
                    case group_category_repo:list_by_uid(Uid, <<"id">>) of
                        {ok, Categories} ->
                            CategoryIds = [Id || #{<<"id">> := Id} <- Categories],
                            case lists:member(CategoryId, CategoryIds) of
                                true ->
                                    ok;
                                false ->
                                    {error, <<"分类不存在或无权限"/utf8>>}
                            end;
                        _ ->
                            {error, <<"分类不存在或无权限"/utf8>>}
                    end
            end,
            %% 根据验证结果决定是否执行移动操作
            case ValidateResult of
                ok ->
                    case group_category_ds:move_group_to_category(Uid, Gid, CategoryId) of
                        {ok, _Count} ->
                            ok;
                        {error, _Reason} ->
                            {error, <<"移动群组失败"/utf8>>}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, <<"你不是该群组成员"/utf8>>}
    end.

%% @doc 更新分类排序
%% @param Uid 用户ID
%% @param SortOrders 排序列表，格式为 [{CategoryId, SortOrder}, ...]
%% @return ok 更新成功 | {error, Reason} 更新失败
-spec update_sort_order(integer(), list({integer(), integer()})) -> ok | {error, binary()}.
update_sort_order(Uid, SortOrders) when is_list(SortOrders) ->
    %% 验证所有分类是否属于该用户
    case group_category_repo:list_by_uid(Uid, <<"id">>) of
        {ok, Categories} ->
            UserCategoryIds = lists:usort([Id || #{<<"id">> := Id} <- Categories]),
            UserCategoryIdsSet = sets:from_list(UserCategoryIds),

            %% 验证并更新每个分类的排序
            Results = lists:map(fun({CategoryId, SortOrder}) ->
                case sets:is_element(CategoryId, UserCategoryIdsSet) of
                    true ->
                        group_category_ds:update_sort_order(Uid, CategoryId, SortOrder);
                    false ->
                        {error, <<"分类不存在或无权限"/utf8>>}
                end
            end, SortOrders),

            %% 检查是否所有更新都成功
            ErrorResults = [R || R <- Results, element(1, R) =:= error],
            case ErrorResults of
                [] ->
                    ok;
                _ ->
                    {error, hd([Reason || {error, Reason} <- ErrorResults])}
            end;
        _ ->
            {error, <<"查询分类列表失败"/utf8>>}
    end;
update_sort_order(_Uid, _Invalid) ->
    {error, <<"无效的排序列表"/utf8>>}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
