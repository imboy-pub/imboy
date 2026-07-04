-module(friend_repo).
%%%
% 好友关系数据仓库层，提供好友关系数据的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([list_by_uid/2, list_by_uid/3]).
-export([count_by_uid/1]).
-export([friend_field/3]).
-export([friend_fields/3]).
-export([friend_fields_batch/3]).
-export([confirm_friend/7]).
-export([insert_pending/4, find_pending/2, delete_pending/2]).
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
    elib_pg_sql:public_tablename(<<"user_friend">>).

%% @doc 确认好友关系
%% @param IsConfirmed 是否已确认，true表示已存在好友关系，false需要创建新关系
%% @param FromID 发起好友请求的用户ID
%% @param ToID 接收好友请求的用户ID
%% @param Remark 好友备注
%% @param Setting 好友设置列表
%% @param Tag 好友标签
%% @param NowTs 当前时间戳（timestamptz格式）
%% @return ok
-spec confirm_friend(
    boolean(), integer(), integer(), binary(), map() | binary() | undefined, binary(), binary()
) -> ok.
confirm_friend(true, _, _, _, _, _, _) ->
    ok;
confirm_friend(false, FromID, ToID, Remark, Setting, Tag, NowTs) ->
    Tb = tablename(),
    Id = elib_tsid:generate(friend),
    SettingJson = jsone:encode(filter_friend_setting(Setting), [native_utf8]),
    %% T3.4 余项：upsert 替代纯 INSERT。无 (from,to) 行→插 status=1；
    %% 存在 pending(status=0) 行→promote 0→1。兼容 uk_fromuid_touid 唯一约束，
    %% 避免引入 pending 行后纯 INSERT 因唯一冲突静默失败。
    Sql = <<
        "INSERT INTO ",
        Tb/binary,
        " (id, from_user_id, to_user_id, status, category_id, remark, setting, tag, created_at)"
        " VALUES ($1, $2, $3, 1, 0, $4, $5, $6, $7)"
        " ON CONFLICT (from_user_id, to_user_id) DO UPDATE SET"
        " status = 1, remark = EXCLUDED.remark, setting = EXCLUDED.setting,"
        " tag = EXCLUDED.tag, updated_at = $7"
    >>,
    Params = [Id, FromID, ToID, Remark, SettingJson, Tag, NowTs],
    case elib_pg:query(Sql, Params) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG({friend_upsert_failed, FromID, ToID, Reason}),
            ok
    end.

%% @doc 插入 pending 好友申请行（status=0，单向 from→to）。
%% 已存在 (from,to) 行（任意 status）则 DO NOTHING（幂等，由 uk_fromuid_touid 保证）。
-spec insert_pending(integer(), integer(), map() | binary() | undefined, binary()) -> ok.
insert_pending(FromID, ToID, Setting, NowTs) ->
    Tb = tablename(),
    Id = elib_tsid:generate(friend),
    SettingJson = jsone:encode(filter_friend_setting(Setting), [native_utf8]),
    Sql = <<
        "INSERT INTO ",
        Tb/binary,
        " (id, from_user_id, to_user_id, status, category_id, setting, created_at)"
        " VALUES ($1, $2, $3, 0, 0, $4, $5)"
        " ON CONFLICT (from_user_id, to_user_id) DO NOTHING"
    >>,
    case elib_pg:query(Sql, [Id, FromID, ToID, SettingJson, NowTs]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG({pending_insert_failed, FromID, ToID, Reason}),
            ok
    end.

%% @doc 是否存在 from→to 的 pending(status=0) 申请行。
-spec find_pending(integer(), integer()) -> boolean().
find_pending(FromID, ToID) ->
    Tb = tablename(),
    Sql = <<
        "SELECT id FROM ",
        Tb/binary,
        " WHERE from_user_id = $1 AND to_user_id = $2 AND status = 0 LIMIT 1"
    >>,
    case elib_pg:query(Sql, [FromID, ToID]) of
        {ok, [_ | _]} -> true;
        _ -> false
    end.

%% @doc 删除 from→to 的 pending(status=0) 申请行（reject 用，不影响 status=1 好友行）。
-spec delete_pending(integer(), integer()) -> ok.
delete_pending(FromID, ToID) ->
    Tb = tablename(),
    Sql = <<
        "DELETE FROM ",
        Tb/binary,
        " WHERE from_user_id = $1 AND to_user_id = $2 AND status = 0"
    >>,
    _ = elib_pg:query(Sql, [FromID, ToID]),
    ok.

%% @doc 查询好友关系的字段值（单字段）
%% @param FromID 发起好友关系的用户ID
%% @param ToID 接收好友关系的用户ID
%% @param Field 字段名，如 <<"remark">>
%% @return {ok, Rows} | {error, Reason}
-spec friend_field(integer(), integer(), binary()) -> {ok, list(map())} | {error, any()}.
friend_field(FromID, ToID, Field) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(
        Tb, Field, #{from_user_id => FromID, to_user_id => ToID, status => 1}, #{}
    ),
    elib_pg:query(Sql, Params).

%% @doc 查询好友关系的多个字段
%% @param FromID 发起好友关系的用户ID
%% @param ToID 接收好友关系的用户ID
%% @param Fields 要查询的字段列表，如 [<<"remark">>, <<"created_at">>]
%% @return {ok, [map()]} 查询成功返回字段数据 | {error, any()} 查询失败
% friend_repo:friend_fields(4, 6, [<<"remark">>, <<"created_at">>]).
-spec friend_fields(integer(), integer(), [binary()]) -> {ok, [map()]} | {error, any()}.
friend_fields(FromID, ToID, Fields) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(
        Tb, Fields, #{from_user_id => FromID, to_user_id => ToID, status => 1}, #{}
    ),
    ?DEBUG_LOG([Sql, Params]),
    elib_pg:query(Sql, Params).

%% @doc 批量查询 FromID 对多个 ToID 的好友关系(一次查询替代 N 次)
%% Fields 为内部常量列名(非用户输入)，返回行含 to_user_id + 请求字段
%% @example friend_repo:friend_fields_batch(4, [6,7], [<<"remark">>, <<"created_at">>]).
-spec friend_fields_batch(integer(), [integer()], [binary()]) -> {ok, [map()]} | {error, any()}.
friend_fields_batch(_FromID, [], _Fields) ->
    {ok, []};
friend_fields_batch(FromID, ToIDs, Fields) ->
    Tb = tablename(),
    Cols = elib_cnv:implode(<<",">>, [<<"to_user_id">> | Fields]),
    Sql =
        <<"SELECT ", Cols/binary, " FROM ", Tb/binary,
            " WHERE from_user_id = $1 AND status = 1 AND to_user_id = ANY($2)">>,
    elib_pg:query(Sql, [FromID, ToIDs]).

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
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{from_user_id => UID, status => 1}, #{
        limit => Limit
    }),
    elib_pg:query(Sql, Params).

%% @doc 统计用户的好友数量
%% @param UID 用户ID
%% @return non_neg_integer() 好友数量
-spec count_by_uid(integer()) -> non_neg_integer().
count_by_uid(UID) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COUNT(*) as count FROM ", Tb/binary, " WHERE from_user_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [UID]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 删除好友关系（双向，单条 SQL 原子操作）
%% @param FromID 发起好友关系的用户ID
%% @param ToID 接收好友关系的用户ID
%% @return ok | {error, any()}
-spec delete(integer(), integer()) -> ok | {error, any()}.
delete(FromID, ToID) ->
    Tb = tablename(),
    Sql = <<
        "DELETE FROM ",
        Tb/binary,
        " WHERE (from_user_id = $1 AND to_user_id = $2)"
        "    OR (from_user_id = $2 AND to_user_id = $1)"
    >>,
    case elib_pg:execute(Sql, [FromID, ToID]) of
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
    case elib_pg:execute(Sql, [CategoryID, FromUID, ToUID]) of
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
    Dt = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET remark = $1, updated_at = $2\n"
            "        WHERE status = $3 AND from_user_id = $4 AND to_user_id = $5">>,
    elib_pg:execute(Sql, [Remark, Dt, 1, FromUid, ToUid]).

%% @doc 批量按分类变更好友分类ID
%% @param Uid 用户ID
%% @param CategoryId 当前分类ID
%% @param NewCid 新分类ID
%% @return {ok, integer()} | {error, any()}
-spec set_category_by_cid(integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
set_category_by_cid(Uid, CategoryId, NewCid) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET category_id = $1, updated_at = $2\n"
            "        WHERE status = $3 AND from_user_id = $4 AND category_id = $5">>,
    elib_pg:execute(Sql, [NewCid, elib_dt:now(), 1, Uid, CategoryId]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 过滤和标准化好友设置配置
%% @param Setting 原始好友设置列表
%% @return 过滤后的好友设置列表
%% @details 对好友的一些权限控制配置进行标准化处理
%% @private
-spec filter_friend_setting(map() | binary() | undefined) -> map().
filter_friend_setting(undefined) ->
    filter_friend_setting(#{});
filter_friend_setting(Setting) when is_binary(Setting) ->
    case Setting of
        <<>> ->
            filter_friend_setting(#{});
        _ ->
            try jsone:decode(Setting, [{object_format, map}]) of
                Map when is_map(Map) ->
                    filter_friend_setting(Map);
                [] ->
                    filter_friend_setting(#{});
                _ ->
                    filter_friend_setting(#{})
            catch
                _:_ ->
                    filter_friend_setting(#{})
            end
    end;
filter_friend_setting(Setting) when is_map(Setting) ->
    IsFrom = maps:get(<<"is_from">>, Setting, 0),
    #{
        <<"is_from">> => IsFrom,
        <<"source">> => maps:get(<<"source">>, Setting, <<>>),
        <<"role">> => maps:get(<<"role">>, Setting, <<"all">>),
        <<"donotlethimlook">> => maps:get(<<"donotlethimlook">>, Setting, false),
        <<"donotlookhim">> => maps:get(<<"donotlookhim">>, Setting, false)
    }.
