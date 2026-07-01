-module(group_tag_ds).
-dialyzer({nowarn_function, [count/1]}).
%%%
% group_tag_ds 是群组标签数据服务层
% 封装群组标签的数据操作和业务逻辑
%%%

-include("log.hrl").
-include("common.hrl").

%% 导出函数
-export([add/3]).
-export([remove/3]).
-export([list/1]).
-export([search/1]).
-export([hot_tags/1]).
-export([count/1]).
%% G3: adm_group_handler 不应直调 group_tag_repo
-export([list_by_group/2]).
-export([count_by_group/1]).
-export([delete/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 添加群组标签
%% @param GroupId 群组ID
%% @param Uid 操作者用户ID
%% @param TagName 标签名称
%% @return {ok, TagId} | {error, Reason}
-spec add(integer(), integer(), binary()) -> {ok, integer()} | {error, binary()}.
add(GroupId, _Uid, _TagName) when GroupId =< 0 ->
    {error, <<"无效的群组ID"/utf8>>};
add(_GroupId, _Uid, TagName) when TagName =:= <<>> ->
    {error, <<"标签名不能为空"/utf8>>};
add(_GroupId, _Uid, TagName) when byte_size(TagName) > 50 ->
    {error, <<"标签名过长，最多50个字符"/utf8>>};
add(GroupId, Uid, TagName) ->
    % 检查标签是否已存在
    case group_tag_repo:exists(GroupId, TagName) of
        true ->
            {error, <<"标签已存在"/utf8>>};
        false ->
            Data = #{
                group_id => GroupId,
                tag_name => TagName,
                created_by => Uid,
                created_at => elib_dt:now()
            },
            case group_tag_repo:add(undefined, Data) of
                {ok, TagId} -> {ok, TagId};
                {error, Reason} -> {error, ec_cnv:to_binary(Reason)}
            end
    end.

%% @doc 删除群组标签
%% @param GroupId 群组ID
%% @param Uid 操作者用户ID（未使用，保留用于权限验证）
%% @param TagName 标签名称
%% @return ok | {error, Reason}
-spec remove(integer(), integer(), binary()) -> ok | {error, binary()}.
remove(_GroupId, _Uid, TagName) when TagName =:= <<>> ->
    {error, <<"标签名不能为空"/utf8>>};
remove(GroupId, _Uid, TagName) ->
    case group_tag_repo:delete(GroupId, TagName) of
        {ok, _} -> ok;
        {error, Reason} -> {error, ec_cnv:to_binary(Reason)}
    end.

%% @doc 查询群组的标签列表
%% @param GroupId 群组ID
%% @return {ok, [TagMap]} | {error, Reason}
-spec list(integer()) -> {ok, list(map())} | {error, binary()}.
list(GroupId) when GroupId > 0 ->
    case group_tag_repo:list_by_group(GroupId, <<"id, tag_name, created_by, created_at">>) of
        {ok, Rows} -> {ok, Rows};
        {error, _Reason} -> {ok, []}
    end;
list(_) ->
    {ok, []}.

%% @doc 按标签名搜索使用该标签的群组
%% @param TagName 标签名称
%% @return {ok, [GroupTagMap]} | {error, Reason}
-spec search(binary()) -> {ok, list(map())} | {error, binary()}.
search(TagName) when TagName =:= <<>> ->
    {error, <<"标签名不能为空"/utf8>>};
search(TagName) ->
    case
        group_tag_repo:list_by_tag_name(TagName, <<"group_id, tag_name, created_by, created_at">>)
    of
        {ok, Rows} -> {ok, Rows};
        {error, _Reason} -> {ok, []}
    end.

%% @doc 获取热门标签（按使用频率排序）
%% @param Limit 返回数量限制
%% @return {ok, [{tag_name, count}]} | {error, Reason}
-spec hot_tags(integer()) -> {ok, list(map())} | {error, binary()}.
hot_tags(Limit) when Limit > 0 ->
    case group_tag_repo:hot_tags(Limit) of
        {ok, Rows} -> {ok, Rows};
        {error, _Reason} -> {ok, []}
    end;
hot_tags(_) ->
    {ok, []}.

%% @doc 统计群组的标签数量
%% @param GroupId 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count(integer()) -> {ok, non_neg_integer()} | {error, binary()}.
count(GroupId) ->
    case group_tag_repo:count_by_group(GroupId) of
        {ok, Count} -> {ok, Count};
        {error, _Reason} -> {ok, 0}
    end.

%% G3 thin wrappers for adm_group_handler
-spec list_by_group(integer(), binary()) -> {ok, list(map())} | {error, term()}.
list_by_group(Gid, Column) -> group_tag_repo:list_by_group(Gid, Column).

-spec count_by_group(integer()) -> {ok, non_neg_integer()} | {error, term()}.
count_by_group(Gid) -> group_tag_repo:count_by_group(Gid).

-spec delete(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete(GroupId, TagName) -> group_tag_repo:delete(GroupId, TagName).
