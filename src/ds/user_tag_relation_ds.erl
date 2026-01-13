-module(user_tag_relation_ds).
%%%
% user_tag_relation_ds 是用户标签关系数据服务层
% 封装用户标签关系的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([remove/4]).
-export([set/5]).
-export([save_user_tag_relation/6]).
-export([select_tag/3]).
-export([remove_user_tag_relation/5]).
-export([replace_object_tag/6]).
-export([save_tag/5]).
-export([update_tag/5]).
-export([delete_object_tag/4]).
-export([tablename/0]).
-export([flush_subtitle/1]).
-export([tag_subtitle/3]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 获取表名
-spec tablename() -> binary().
tablename() ->
    user_tag_relation_repo:tablename().

%% @doc 移除用户标签关系
-spec remove(binary() | integer(), binary(), integer(), integer()) -> {ok, any()} | {error, any()}.
remove(Uid, Scene, ObjectId, TagId) when is_binary(TagId) ->
    remove(Uid, Scene, ObjectId, binary_to_integer(TagId));
remove(_Uid, Scene, ObjectId, _TagId) ->
    user_tag_relation_repo:delete(Scene, _Uid, ObjectId).

%% @doc 设置用户标签关系
%% 注意：此函数暂时由 Logic 层实现，因为包含复杂的事务逻辑
-spec set(integer(), integer(), list(), integer(), binary()) -> ok | binary().
set(Uid, Scene, ObjectIds, TagId, TagName) ->
    user_tag_relation_logic:set(Uid, Scene, ObjectIds, TagId, TagName).

%% @doc 保存用户标签关系
-spec save_user_tag_relation(pid(), integer(), integer(), integer(), binary(), binary()) -> integer().
save_user_tag_relation(Conn, Scene, Uid, TagId, ObjectId, CreatedAt) ->
    user_tag_relation_repo:save_user_tag_relation(Conn, Scene, Uid, TagId, ObjectId, CreatedAt).

%% @doc 查询标签
-spec select_tag(binary(), list(), binary()) -> {ok, non_neg_integer(), list(map())} | {error, term()}.
select_tag(Where, WhereArgs, Column) ->
    user_tag_relation_repo:select_tag(Where, WhereArgs, Column).

%% @doc 移除用户标签关系
-spec remove_user_tag_relation(pid(), binary(), binary() | integer(), integer(), binary() | integer()) -> ok.
remove_user_tag_relation(Conn, Scene, Uid, TagId, ObjectId) ->
    user_tag_relation_repo:remove_user_tag_relation(Conn, Scene, Uid, TagId, ObjectId).

%% @doc 替换对象标签
-spec replace_object_tag(pid(), binary(), binary() | integer(), binary() | integer(), [binary()], [binary()]) -> ok.
replace_object_tag(Conn, Scene, Uid, ObjectId, FromName, ToName) ->
    user_tag_relation_repo:replace_object_tag(Conn, Scene, Uid, ObjectId, FromName, ToName).

%% @doc 保存标签
-spec save_tag(pid(), integer(), integer(), binary(), binary()) -> {integer(), binary()} | {error, term()}.
save_tag(Conn, Uid, Scene, CreatedAt, Tag) ->
    user_tag_relation_repo:save_tag(Conn, Uid, Scene, CreatedAt, Tag).

%% @doc 更新标签
-spec update_tag(pid(), integer(), binary(), integer(), binary()) -> {integer(), binary()} | {error, term()}.
update_tag(Conn, TagId, TagName, Uid, CreatedAt) ->
    user_tag_relation_repo:update_tag(Conn, TagId, TagName, Uid, CreatedAt).

%% @doc 删除对象标签
%% 注意：此函数暂时由 Logic 层实现，因为包含复杂的事务逻辑
-spec delete_object_tag(pid(), integer(), integer(), binary()) -> ok.
delete_object_tag(Conn, Scene, Uid, ObjectId) ->
    user_tag_relation_logic:delete_object_tag(Conn, Scene, Uid, ObjectId).

%% @doc 清理缓存
-spec flush_subtitle(integer()) -> ok.
flush_subtitle(TagId) ->
    user_tag_relation_repo:flush_subtitle(TagId).

%% @doc 获取标签副标题
-spec tag_subtitle(integer(), integer(), any()) -> binary().
tag_subtitle(Scene, TagId, Count) ->
    user_tag_relation_repo:tag_subtitle(Scene, TagId, Count).

%% ===================================================================
%% Internal Functions
%% ===================================================================
