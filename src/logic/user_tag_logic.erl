-module(user_tag_logic).
%%%
% user_tag 业务逻辑模块
% user_tag business logic module
%%%

-export([page/5]).
-export([change_name/5]).
-export([add/2]).
-export([add/3]).
-export([save/3]).
-export([delete/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 标签分页列表
%% 获取用户标签的分页数据
%% @param Scene 场景（1 收藏 2 好友）
%% @param Page 页码
%% @param Size 每页大小
%% @param Where 查询条件
%% @param OrderBy 排序
%% @return map() 包含 total、page、size、list 的分页结果
-spec page(integer(), integer(), integer(), map(), binary()) -> map().
page(Scene, Page, Size, Where, OrderBy) when Page > 0 ->
    % 使用 DS 层接口
    user_tag_ds:page(Scene, Page, Size, Where, OrderBy).

%% @doc 删除标签
%% 删除标签，标签中的联系人不会被删除，使用此标签设置了分组的朋友圈，可见范围也将更新
%% 会先删除 user_tag_relation 中该标签的所有关联记录，再删除标签本身，避免孤立数据
%% @param Uid 用户ID
%% @param Scene 场景（1 收藏 2 好友）
%% @param Tag 标签名称
%% @return ok
-spec delete(integer(), integer(), binary()) -> ok.
delete(Uid, Scene, Tag) ->
    %% 先通过 DS 层查找 TagId，用于级联清理
    _ =
        case user_tag_ds:find_tag_id(Uid, Scene, Tag) of
            {ok, TagId} when TagId > 0 ->
                %% 先删除 user_tag_relation 中该标签的所有关联记录，防止孤立数据
                user_tag_relation_ds:delete_by_tag_id(TagId);
            _ ->
                ok
        end,
    %% 调用 DS 层完成标签删除及相关表 tag 字段更新
    user_tag_ds:delete(Uid, Scene, Tag).

%% @doc 修改标签名称
%% 修改标签的名称
%% @param Count 同名标签数量
%% @param Uid 用户ID
%% @param Scene 场景
%% @param TagId 标签ID
%% @param TagName 新标签名称
%% @return ok | binary()
-spec change_name(integer(), integer(), integer(), integer(), binary()) -> ok | binary().
change_name(Count, _Uid, _Scene, _TagId, TagName) when Count > 0 ->
    <<TagName/binary, " 已存在"/utf8>>;
change_name(0, Uid, Scene, TagId, TagName) ->
    % 使用 DS 层接口
    user_tag_ds:change_name(0, Uid, Scene, TagId, TagName).

%% @doc 添加标签
%% 添加新的用户标签
%% 兼容旧入口：默认使用好友场景（scene = 2）
%% @param Uid 用户ID
%% @param Tag 标签名称
%% @return {ok, map()} | {error, binary()}
-spec add(integer(), binary()) -> {ok, map()} | {error, binary()}.
add(Uid, Tag) ->
    case add(Uid, 2, Tag) of
        {ok, TagId} ->
            {ok, #{
                <<"id">> => TagId,
                <<"name">> => Tag,
                <<"tag_name">> => Tag
            }};
        Error ->
            Error
    end.

%% @doc 兼容旧入口：保留 save 名称但复用当前 add/3
%% @param Uid 用户ID
%% @param Scene 场景
%% @param Tag 标签名称
%% @return {ok, integer()} | {error, binary()}
-spec save(integer(), integer(), binary()) -> {ok, integer()} | {error, binary()}.
save(Uid, Scene, Tag) ->
    add(Uid, Scene, Tag).

%% @doc 添加标签
%% 添加新的用户标签
%% @param Uid 用户ID
%% @param Scene 场景
%% @param Tag 标签名称
%% @return {ok, TagId} | {error, binary()}
-spec add(integer(), integer(), binary()) -> {ok, integer()} | {error, binary()}.
add(Uid, Scene, Tag) ->
    % 使用 DS 层接口
    user_tag_ds:add(Uid, Scene, Tag).

%% ===================================================================
%% EUnit tests.
%% ===================================================================
