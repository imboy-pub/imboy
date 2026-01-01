-module(friend_repo).
%%%
% 好友关系数据仓库层，提供好友关系数据的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([list_by_uid/2]).
-export([friend_field/3]).
-export([confirm_friend/7]).
-export([delete/2]).
-export([move_to_category/3]).
-export([change_remark/3]).
-export([set_category_by_cid/3]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取好友关系表的表名
%% @return 返回好友关系表的完整表名
%% @example friend_repo:tablename().
-spec tablename() -> binary().
tablename() ->
    imboy_pg_sql:public_tablename(<<"user_friend">>).


%% @doc 确认好友关系
%% @param IsConfirmed 是否已确认，true表示已存在好友关系，false需要创建新关系
%% @param FromID 发起好友请求的用户ID
%% @param ToID 接收好友请求的用户ID
%% @param Remark 好友备注
%% @param Setting 好友设置列表
%% @param Tag 好友标签
%% @param NowTs 当前时间戳
%% @return ok
-spec confirm_friend(boolean(), integer(), integer(), binary(), binary(), binary(), binary()) -> ok.
confirm_friend(true, _, _, _, _, _, _) ->
    ok;
confirm_friend(false, FromID, ToID, Remark, Setting, Tag, NowTs) ->
    Tb = tablename(),
    imboy_pg:insert(Tb, #{
        from_user_id => FromID,
        to_user_id => ToID,
        status => 1,
        category_id => 0,
        remark => Remark,
        created_at => NowTs,
        setting => jsone:encode(filter_friend_setting(Setting), [native_utf8]),
        tag => Tag
        }),
    ok.


%% @doc 查询好友关系中的特定字段值
%% @param FromID 发起好友关系的用户ID
%% @param ToID 接收好友关系的用户ID
%% @param Field 要查询的字段名
%% @return {ok, Rows} 查询成功返回列和行数据（map） | {error, Reason} 查询失败
-spec friend_field(integer(), integer(), binary()) -> {ok, list(map())} | {error, any()}.
friend_field(FromID, ToID, Field) ->
    Tb = tablename(),
    {Sql, Params} = imboy_pg_sql:build_select(Tb, Field, #{from_user_id => FromID, to_user_id => ToID, status => 1}, #{}),
    imboy_pg:query(Sql, Params).


%% @doc 查询指定用户的好友列表（使用默认限制10000）
%% @param UID 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Rows} 查询成功返回列和行数据（map） | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_by_uid(UID, Column) ->
    list_by_uid(UID, Column, 10000).

%% @doc 查询指定用户的好友列表（指定限制数量）
%% @param UID 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(UID, Column, Limit) ->
    Tb = tablename(),
    {Sql, Params} = imboy_pg_sql:build_select(Tb, Column, #{from_user_id => UID, status => 1}, #{limit => Limit}),
    imboy_pg:query(Sql, Params).


%% @doc 删除好友关系
%% @param FromID 发起好友关系的用户ID
%% @param ToID 接收好友关系的用户ID
%% @return ok | {error, any()}
-spec delete(integer(), integer()) -> ok | {error, any()}.
delete(FromID, ToID) ->
    Tb = tablename(),
    Where = <<"from_user_id = $1 AND to_user_id = $2">>,
    % ?DEBUG_LOG(io:format("~s  ~p ~p\n", [Sql, FromID, ToID])),
    case imboy_pg:execute(<<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>, [FromID, ToID]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 移动好友到指定分类
%% @param FromUID 当前用户ID
%% @param ToUID 好友用户ID
%% @param CategoryID 好友分类ID
%% @return ok | {error, any()}
-spec move_to_category(integer(), integer(), integer()) -> ok | {error, any()}.
move_to_category(FromUID, ToUID, CategoryID) ->
    Tb = tablename(),
    Where = <<" WHERE status = 1 AND from_user_id = $2 AND to_user_id = $3">>,
    Sql = <<"UPDATE ", Tb/binary, " SET category_id = $1", Where/binary>>,
    % ?DEBUG_LOG([Sql, CategoryID, FromUID, ToUID]),
    case imboy_pg:execute(Sql, [CategoryID, FromUID, ToUID]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 修改好友备注
%% @param FromUid 源用户ID
%% @param ToUid 目标用户ID
%% @param Remark 备注内容
%% @return {ok, integer()} | {error, any()}
-spec change_remark(integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
change_remark(FromUid, ToUid, Remark) ->
    Tb = tablename(),
    Dt = imboy_dt:now(),
    Sql = <<"UPDATE ", Tb/binary, " SET remark = $1, updated_at = $2
        WHERE status = $3 AND from_user_id = $4 AND to_user_id = $5">>,
    imboy_pg:execute(Sql, [Remark, Dt, 1, FromUid, ToUid]).

%% @doc 批量按分类变更好友分类ID
%% @param Uid 用户ID
%% @param CategoryId 当前分类ID
%% @param NewCid 新分类ID
%% @return {ok, integer()} | {error, any()}
-spec set_category_by_cid(integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
set_category_by_cid(Uid, CategoryId, NewCid) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET category_id = $1, updated_at = $2
        WHERE status = $3 AND from_user_id = $4 AND category_id = $5">>,
    imboy_pg:execute(Sql, [NewCid, imboy_dt:now(), 1, Uid, CategoryId]).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 过滤和标准化好友设置配置
%% @param Setting 原始好友设置列表
%% @return 过滤后的好友设置列表
%% @details 对好友的一些权限控制配置进行标准化处理
%% @private
-spec filter_friend_setting(list()) -> list().
filter_friend_setting(Setting) ->
    [
     % 好友关系发起人 1 是 0 否
     {<<"isfrom">>, proplists:get_value(<<"isfrom">>, Setting, 0)},
     {<<"source">>, proplists:get_value(<<"source">>, Setting, "")},
     % 客户端约定
     % role 可能的值 all just_chat
     {<<"role">>, proplists:get_value(<<"role">>, Setting, "all")},
     %  不让他（她）看
     {<<"donotlethimlook">>, proplists:get_value(<<"donotlethimlook">>, Setting, false)},
     % 不看他（她）
     {<<"donotlookhim">>, proplists:get_value(<<"donotlookhim">>, Setting, false)}].
