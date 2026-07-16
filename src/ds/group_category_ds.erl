-module(group_category_ds).

%%%
% group_category_ds 是 group_category domain service 缩写
% 群组分类数据服务层，提供群组分类的数据操作和业务逻辑
%%%

-include("log.hrl").

%% 查询操作
-export([find_by_uid/1]).
-export([list_by_uid/2]).

%% 增删改操作
-export([add/2]).
-export([rename/3]).
-export([delete/2]).
-export([update_sort_order/3]).
-export([move_group_to_category/3]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 添加群组分类
%% 如果同名分类已存在，直接返回已存在的分类ID
%% @param Uid 用户ID
%% @param Name 分类名称
%% @return {ok, CategoryId} 操作成功返回分类ID | {error, Reason} 操作失败
-spec add(integer(), binary()) -> {ok, integer()} | {error, term()}.
add(Uid, Name) ->
    %% 检查同名分类是否已存在
    case group_category_repo:find_by_name(Uid, Name) of
        {ok, #{<<"id">> := CategoryId}} ->
            %% 同名分类已存在，直接返回
            {ok, CategoryId};
        {ok, #{}} ->
            %% 未找到同名分类，执行插入
            case group_category_repo:add(Uid, Name) of
                {error, ErrorMsg} ->
                    {error, ErrorMsg};
                {ok, CategoryId} ->
                    {ok, CategoryId}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 根据用户ID查找群组分类列表
%% 返回包含默认分类的完整列表
%% @param Uid 用户ID
%% @return list() 群组分类列表，包含默认分类（id=0）
-spec find_by_uid(integer()) -> list(map()).
find_by_uid(Uid) ->
    Field = <<"id, category_name, sort_order">>,
    %% 一次聚合各分类群数量（NULL 归入未分类 0），供每个分类补 group_count
    CountMap =
        case group_category_repo:count_groups_grouped_by_category(Uid) of
            {ok, CountRows} when is_list(CountRows) ->
                maps:from_list([
                    {Cid, N}
                 || #{<<"category_id">> := Cid, <<"count">> := N} <- CountRows
                ]);
            _ ->
                #{}
        end,
    Default = #{
        <<"id">> => 0,
        <<"category_name">> => <<"未分类"/utf8>>,
        <<"sort_order">> => 0,
        <<"group_count">> => maps:get(0, CountMap, 0)
    },
    case group_category_repo:list_by_uid(Uid, Field) of
        {ok, Rows} when is_list(Rows) ->
            [
                Default
                | [
                    #{
                        <<"id">> => Id,
                        <<"category_name">> => Name,
                        <<"sort_order">> => SortOrder,
                        <<"group_count">> => maps:get(Id, CountMap, 0)
                    }
                 || #{
                        <<"id">> := Id,
                        <<"category_name">> := Name,
                        <<"sort_order">> := SortOrder
                    } <- Rows
                ]
            ];
        _ ->
            [Default]
    end.

%% @doc 重命名群组分类
%% @param Uid 用户ID
%% @param CategoryId 分类ID
%% @param NewName 新的分类名称
%% @return {ok, 1} 操作成功 | {error, Reason} 操作失败
-spec rename(integer(), integer() | binary(), binary()) -> {ok, 1} | {error, term()}.
rename(_Uid, CategoryId, _NewName) when
    CategoryId =:= undefined;
    CategoryId =:= <<"">>;
    CategoryId =:= ""
->
    {error, <<"invalid_id: 分类ID必须"/utf8>>};
rename(_Uid, _CategoryId, NewName) when
    NewName =:= undefined;
    NewName =:= <<"">>;
    NewName =:= ""
->
    {error, <<"invalid_name: 分类名称必须"/utf8>>};
rename(Uid, CategoryId, NewName) ->
    case group_category_repo:update_name(Uid, ec_cnv:to_integer(CategoryId), NewName) of
        {ok, 1} ->
            {ok, 1};
        {ok, 0} ->
            {error, <<"not_found: 分类不存在或无权限"/utf8>>};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除群组分类
%% 删除分类前需要将该分类下的群组移到默认分类（category_id=0）
%% @param Uid 用户ID
%% @param CategoryId 分类ID
%% @return {ok, 1} 操作成功 | {error, Reason} 操作失败
-spec delete(integer(), integer() | binary()) -> {ok, 1} | {error, term()}.
delete(_Uid, CategoryId) when
    CategoryId =:= undefined;
    CategoryId =:= <<"">>;
    CategoryId =:= ""
->
    {error, <<"invalid_id: 分类ID必须"/utf8>>};
delete(Uid, CategoryId) ->
    CategoryIdInt = ec_cnv:to_integer(CategoryId),

    %% 将该分类下的群组移到默认分类
    case group_category_repo:list_groups_by_category(Uid, CategoryIdInt, <<"gm.group_id">>) of
        {ok, Groups} ->
            %% 批量更新群组的分类ID为0
            lists:foreach(
                fun(#{<<"group_id">> := Gid}) ->
                    group_category_repo:update_group_category(Uid, Gid, 0)
                end,
                Groups
            );
        _ ->
            ok
    end,

    %% 删除分类
    case group_category_repo:delete(Uid, CategoryIdInt) of
        {ok, 1} ->
            {ok, 1};
        {ok, 0} ->
            {error, <<"not_found: 分类不存在或无权限"/utf8>>};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 更新群组分类的排序
%% @param Uid 用户ID
%% @param CategoryId 分类ID
%% @param SortOrder 新的排序值
%% @return {ok, 1} 操作成功 | {error, Reason} 操作失败
-spec update_sort_order(integer(), integer(), integer()) -> {ok, 1} | {error, term()}.
update_sort_order(Uid, CategoryId, SortOrder) ->
    case group_category_repo:update_sort_order(Uid, ec_cnv:to_integer(CategoryId), SortOrder) of
        {ok, 1} ->
            {ok, 1};
        {ok, 0} ->
            {error, <<"not_found: 分类不存在或无权限"/utf8>>};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 移动群组到指定分类
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @param CategoryId 分类ID（0表示未分类）
%% @return {ok, 1} 操作成功 | {error, Reason} 操作失败
-spec move_group_to_category(integer(), integer(), integer()) -> {ok, integer()} | {error, term()}.
move_group_to_category(Uid, Gid, CategoryId) ->
    case group_category_repo:update_group_category(Uid, Gid, CategoryId) of
        {ok, Count} when Count >= 0 ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end.

%% G3: group_category_logic 不应直调 group_category_repo
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, Column) -> group_category_repo:list_by_uid(Uid, Column).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
