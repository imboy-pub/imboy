-module(user_setting_repo).
%%%
% user_setting_repo 是 user_setting repository 缩写
% 用户设置数据仓库层，提供用户设置信息的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([find_by_uid/2]).
-export([update/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取用户设置表的表名
%% @return 返回用户设置表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_setting">>).


%% @doc 根据用户ID查找用户设置
%% @param Uid 用户ID（支持binary或integer类型）
%% @param Column 要查询的列名，支持多个列用逗号分隔
%% @return Map 查询成功返回设置信息map，未找到返回空map
-spec find_by_uid(binary() | integer(), binary()) -> map().
find_by_uid(Uid, Column) when is_binary(Uid) ->
    find_by_uid(elib_hashids:decode(Uid), Column);
find_by_uid(Uid, Column) when is_integer(Uid) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.


%% @doc 更新用户设置
%% @param Uid 用户ID（支持binary或integer类型）
%% @param Setting 设置数据map
%% @return ok 更新成功
%% @details 使用 ON CONFLICT 语法实现 upsert（存在则更新，不存在则插入）
%% @example user_setting_repo:update(3, #{people_nearby_visible => true}).
-spec update(binary() | integer(), map()) -> ok.
update(Uid, Setting) when is_binary(Uid) ->
    update(elib_hashids:decode(Uid), Setting);
update(Uid, Setting) when is_integer(Uid) ->
    Data = #{
        <<"user_id">> => Uid,  % 用户ID
        <<"setting">> => jsone:encode(Setting, [
            native_utf8,         % 保持UTF8编码
            {float_format, [{decimals, 4}, compact]}  % 优化浮点数格式
        ]),
        <<"updated_at">> => elib_dt:now()  % 自动格式化为数据库时间
    },
    %% ON CONFLICT 子句
    %% 使用EXCLUDED引用新插入的值
    OnConflict = <<
        "ON CONFLICT (user_id) DO UPDATE SET\n"
        "  setting = EXCLUDED.setting,\n"
        "  updated_at = EXCLUDED.updated_at"
    >>,

    %% 构建带ON CONFLICT的INSERT SQL
    {Sql, Params} = elib_pg_sql:insert(tablename(), Data, <<>>),
    FullSql = [Sql, <<" ">>, OnConflict],
    _ = elib_pg:execute(FullSql, Params),
    ok.
