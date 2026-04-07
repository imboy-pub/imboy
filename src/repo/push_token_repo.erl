-module(push_token_repo).
%%%
% push_token_repo 是 push_token repository 缩写
% 推送 Token 数据仓库层，提供推送 token 的基础数据库操作
%%%

-export([tablename/0]).
-export([upsert/5]).
-export([deactivate/2]).
-export([deactivate_by_token/1]).
-export([deactivate_inactive/1]).
-export([list_by_uid/1]).
-export([list_by_uids/1]).
-export([list_page/2]).
-export([delete_by_uid/1]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"push_token">>).

%% @doc 注册或更新推送 token（upsert）
%% 同一用户同一设备只保留一个活跃 token
-spec upsert(integer(), binary(), binary(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
upsert(Uid, DeviceId, DeviceType, Platform, Token) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    %% 先将该设备旧 token 置为无效
    DeactivateSql = <<"UPDATE ", Tb/binary,
                      " SET status = 0, updated_at = $1"
                      " WHERE user_id = $2 AND device_id = $3 AND status = 1">>,
    _ = elib_pg:execute(DeactivateSql, [Now, Uid, DeviceId]),
    %% 插入新 token
    Id = elib_tsid:generate(push_token),
    Data = #{
        <<"id">> => Id,
        <<"user_id">> => Uid,
        <<"device_id">> => DeviceId,
        <<"device_type">> => DeviceType,
        <<"platform">> => Platform,
        <<"token">> => Token,
        <<"status">> => 1,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },
    {Sql, Params} = elib_pg_sql:insert(Tb, Data),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 使指定用户设备的 token 失效
-spec deactivate(integer(), binary()) -> {ok, integer()} | {error, term()}.
deactivate(Uid, DeviceId) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET status = 0, updated_at = $1"
            " WHERE user_id = $2 AND device_id = $3 AND status = 1">>,
    elib_pg:execute(Sql, [Now, Uid, DeviceId]).

%% @doc 按 token 值使 token 失效（token 过期/无效时调用）
-spec deactivate_by_token(binary()) -> {ok, integer()} | {error, term()}.
deactivate_by_token(Token) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET status = 0, updated_at = $1"
            " WHERE token = $2 AND status = 1">>,
    elib_pg:execute(Sql, [Now, Token]).

%% @doc 将超过指定天数未更新的活跃 Token 置为失效
%% 用于定期清理不活跃的推送 Token，避免向失效设备发送推送
-spec deactivate_inactive(pos_integer()) -> {ok, integer()} | {error, term()}.
deactivate_inactive(InactiveDays) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    %% 计算截止时间：当前时间减去 InactiveDays 天（毫秒）
    CutoffMs = Now - InactiveDays * 86400000,
    Sql = <<"UPDATE ", Tb/binary,
            " SET status = 0, updated_at = $1"
            " WHERE status = 1 AND updated_at < $2">>,
    elib_pg:execute(Sql, [Now, CutoffMs]).

%% @doc 查询用户所有活跃推送 token
-spec list_by_uid(integer()) -> {ok, list()} | {error, term()}.
list_by_uid(Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT device_id, device_type, platform, token"
            " FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 批量查询多个用户的活跃推送 token
-spec list_by_uids([integer()]) -> {ok, list()} | {error, term()}.
list_by_uids([]) ->
    {ok, []};
list_by_uids(Uids) when is_list(Uids) ->
    Tb = tablename(),
    Sql = <<"SELECT user_id, device_id, device_type, platform, token"
            " FROM ", Tb/binary,
            " WHERE user_id = ANY($1) AND status = 1">>,
    elib_pg:query(Sql, [Uids]).

%% @doc 分页查询推送 token（Admin 管理用）
-spec list_page(pos_integer(), pos_integer()) ->
    {ok, #{list := list(), total := integer()}} | {error, term()}.
list_page(Page, Size) ->
    Tb = tablename(),
    CountSql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE status = 1">>,
    case elib_pg:one(CountSql, []) of
        {ok, #{<<"count">> := Total}} ->
            Offset = (Page - 1) * Size,
            DataSql = <<"SELECT user_id, device_id, device_type, platform, token, created_at, updated_at"
                        " FROM ", Tb/binary,
                        " WHERE status = 1"
                        " ORDER BY updated_at DESC"
                        " LIMIT $1 OFFSET $2">>,
            case elib_pg:query(DataSql, [Size, Offset]) of
                {ok, Rows} ->
                    {ok, #{list => Rows, total => Total}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, _} ->
            {ok, #{list => [], total => 0}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除用户所有推送 token（账号注销时使用）
-spec delete_by_uid(integer()) -> {ok, integer()} | {error, term()}.
delete_by_uid(Uid) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE user_id = $1">>,
    elib_pg:execute(Sql, [Uid]).
