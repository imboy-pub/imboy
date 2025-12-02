-module(friend_repo).
%%%
% 好友关系数据仓库层，提供好友关系数据的基础数据库操作
%%%

-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([list_by_uid/2]).
-export([friend_field/3]).
-export([confirm_friend/7]).
-export([delete/2]).
-export([move_to_category/3]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取好友关系表的表名
%% @return 返回好友关系表的完整表名
%% @example friend_repo:tablename().
-spec tablename() -> binary().
tablename() ->
    imboy_db:public_tablename(<<"user_friend">>).


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
    imboy_db:insert_into(Tb, #{
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
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec friend_field(integer(), integer(), binary()) -> {ok, list(), list()} | {error, any()}.
friend_field(FromID, ToID, Field) ->
    Tb = tablename(),
    Where = <<" WHERE from_user_id = $1 AND to_user_id = $2 AND status = 1">>,
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [FromID, ToID]).


%% @doc 查询指定用户的好友列表（使用默认限制10000）
%% @param UID 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary()) -> {ok, list(), list()} | {error, any()}.
list_by_uid(UID, Column) ->
    list_by_uid(UID, Column, 10000).

%% @doc 查询指定用户的好友列表（指定限制数量）
%% @param UID 用户ID
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @param Limit 查询结果数量限制
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(), list()} | {error, any()}.
list_by_uid(UID, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE from_user_id = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [UID, Limit]).


%% @doc 删除好友关系
%% @param FromID 发起好友关系的用户ID
%% @param ToID 接收好友关系的用户ID
%% @return ok
-spec delete(integer(), integer()) -> ok.
delete(FromID, ToID) ->
    Tb = tablename(),
    Where = <<" WHERE from_user_id = $1 AND to_user_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    % ?DEBUG_LOG(io:format("~s  ~p ~p\n", [Sql, FromID, ToID])),
    imboy_db:execute(Sql, [FromID, ToID]),
    ok.

%% @doc 移动好友到指定分类
%% @param FromUID 当前用户ID
%% @param ToUID 好友用户ID
%% @param CategoryID 好友分类ID
%% @return ok
-spec move_to_category(integer(), integer(), integer()) -> ok.
move_to_category(FromUID, ToUID, CategoryID) ->
    Tb = tablename(),
    Where = <<" WHERE status = 1 AND from_user_id = $2 AND to_user_id = $3">>,
    Sql = <<"UPDATE ", Tb/binary, " SET category_id = $1", Where/binary>>,
    % ?DEBUG_LOG([Sql, CategoryID, FromUID, ToUID]),
    imboy_db:execute(Sql, [CategoryID, FromUID, ToUID]),
    ok.


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
