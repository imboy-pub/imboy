-module(user_tag_logic).
%%%
% user_tag 业务逻辑模块
% user_tag business logic module
%%%

-export([page/5]).
-export([change_name/5]).
-export([add/3]).
-export([delete/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
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
%% @param Uid 用户ID
%% @param Scene 场景（1 收藏 2 好友）
%% @param Tag 标签名称
%% @return ok
-spec delete(integer(), integer(), binary()) -> ok.
delete(Uid, Scene, Tag) ->
    % 使用 DS 层接口
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

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
