-module(user_dnd_rule_repo).
%%%
% user_dnd_rule_repo 是 user_dnd_rule repository 缩写
% 用户免打扰(DND)规则数据仓库层，提供免打扰规则的基础数据库操作
% 对应迁移：priv/migrations/000070_user_dnd.up.sql
%
% 注意：本 repo 仅负责 user_dnd_rule 表（按 user_id 1:1）。
%   user.dnd_enabled 全局开关属 user 表字段，由 user_repo 读写，
%   接入时用 user_repo:find_by_uid(Uid, <<"dnd_enabled">>) 读取，无需改本模块。
%%%

-include("log.hrl").

-export([tablename/0]).
-export([find_by_uid/1]).
-export([upsert/1]).
-export([delete_by_uid/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取免打扰规则表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_dnd_rule">>).

%% @doc 按用户ID查询免打扰规则
%% @param Uid 用户ID（支持 binary 或 integer）
%% @return map() 命中返回规则 map，未找到返回空 map
-spec find_by_uid(binary() | integer()) -> map().
find_by_uid(Uid) when is_binary(Uid) ->
    find_by_uid(ec_cnv:to_integer(Uid));
find_by_uid(Uid) when is_integer(Uid) ->
    Tb = tablename(),
    Sql = <<
        "SELECT id, user_id, start_min, end_min, status, updated_at, created_at "
        "FROM ",
        Tb/binary,
        " WHERE user_id = $1"
    >>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 插入或更新免打扰规则（user_id 唯一，upsert）
%% @param Data 含 <<"user_id">>/<<"start_min">>/<<"end_min">>/<<"status">> 的 map
%% @return ok
%% @example user_dnd_rule_repo:upsert(#{<<"user_id">>=>1,<<"start_min">>=>1320,<<"end_min">>=>480,<<"status">>=>1}).
-spec upsert(map()) -> ok.
upsert(#{<<"user_id">> := _Uid} = Data0) ->
    %% id 为 bigserial 自增，不注入；updated_at 由服务端统一写入
    Data = Data0#{<<"updated_at">> => elib_dt:now()},
    OnConflict = <<
        "ON CONFLICT (user_id) DO UPDATE SET\n"
        "  start_min = EXCLUDED.start_min,\n"
        "  end_min = EXCLUDED.end_min,\n"
        "  status = EXCLUDED.status,\n"
        "  updated_at = EXCLUDED.updated_at"
    >>,
    {Sql, Params} = elib_pg_sql:insert(tablename(), Data),
    FullSql = [Sql, <<" ">>, OnConflict],
    _ = elib_pg:execute(FullSql, Params),
    ok.

%% @doc 删除用户免打扰规则
%% @param Uid 用户ID（支持 binary 或 integer）
%% @return ok
-spec delete_by_uid(binary() | integer()) -> ok.
delete_by_uid(Uid) when is_binary(Uid) ->
    delete_by_uid(ec_cnv:to_integer(Uid));
delete_by_uid(Uid) when is_integer(Uid) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE user_id = $1">>,
    _ = elib_pg:execute(Sql, [Uid]),
    ok.
